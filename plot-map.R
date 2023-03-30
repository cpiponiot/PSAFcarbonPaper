### make map of plots for publication ------------

## 1. Prepare R environment ####

# R packages - installation

# required packages
req_packages <- c("BIOMASS", "readxl", "data.table", "ggmap", "ggplot2")

# install missing packages
inst_packages <- req_packages[!( req_packages %in% rownames(installed.packages()))]

if (length(inst_packages) > 0) install.packages(inst_packages)

library(ggmap)
library(ggplot2)

# Set working directory to source file location
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


## 2. Load data from excel file ####

data_coord <- read.csv("data/coords-timor.csv")
data.table::setDT(data_coord)

data_coord[, N_PLOT := as.numeric(data.table::tstrsplit(N_Q_ID, "_")[[1]])]

data_coord <- data_coord[, .(long = mean(LONG), lat = mean(LAT)), .(N_PLOT)]

data_plot <- readxl::read_excel("data/DATA_BIOMASS_TL_SHORT.xlsx", "PLOT_GENERAL_INFO")

data_plot <- merge(data_coord, data_plot[, c("N_PLOT", "ID_VILLAGE", "ID_AF")])

data_plot[, village := substr(ID_VILLAGE, 1, 1)]
data_plot[village == "S", village := "Samalari"]
data_plot[village == "G", village := "Gariuai"]

data_box = data_plot[, .(xc = mean(long), yc = mean(lat)), .(village)]
data_box[village == "Gariuai", `:=`(w = 0.1, xc = xc + 0.001)]
data_box[village == "Samalari", w := 0.05]
data_box[, `:=`(x1 = xc - w/2, x2 = xc + w/2, y1 = yc - w/2, y2 = yc + w/2)]

## 3. create Timor map ####

bbox <- c(124.5, -9.2, 127.5,  -8.3)
basemap <- ggmap::get_stamenmap(bbox, maptype = "terrain", zoom = 10)

# geodata::elevation_3s(lon= c(data_box[village == "Samalari"]$x1), 
#                       lat= c(data_box[village == "Samalari"]$y1), 
#                       path = "data")
elev = as.data.frame(terra::rast("data/srtm_62_14.tif"), xy = TRUE)
elevS <- subset(elev, x > data_box[village == "Samalari"]$x1 & 
                  x < data_box[village == "Samalari"]$x2 & 
                  y > data_box[village == "Samalari"]$y1 & 
                  y < data_box[village == "Samalari"]$y2)
elevG <- subset(elev, x > data_box[village == "Gariuai"]$x1 & 
                  x < data_box[village == "Gariuai"]$x2 & 
                  y > data_box[village == "Gariuai"]$y1 & 
                  y < data_box[village == "Gariuai"]$y2)


g1 = ggmap::ggmap(basemap) +
  geom_tile(data = data_box, aes(x = xc, y = yc, width = w, height = w), fill = NA, color = 1, linewidth = 0.5) +
  ggrepel::geom_text_repel(data = data_box, aes(x = xc, y = yc, label = village)) + 
  labs(x = "", y = "") +
  theme(legend.position = "top", axis.title = element_blank(), 
        axis.ticks = element_blank(), axis.text = element_blank())

g2 = ggplot(data = elevG) +
  geom_raster(aes(x = x, y = y, fill=srtm_62_14)) +
  geom_point(data = subset(data_plot, village == "Gariuai"), 
             aes(x = long, y = lat, color = ID_AF)) + 
  coord_equal() +
  viridis::scale_fill_viridis(option = "D",
                              limits = range(c(elevS$srtm_62_14, elevG$srtm_62_14))
                              ) +
  labs(x = "", y = "", title = "Gariuai", color = "", fill="Elevation (m)") +
  guides(color = FALSE, fill=guide_colorbar(title.position = "top")) + 
  theme(legend.position = "top", axis.title = element_blank(), 
        axis.ticks = element_blank(), axis.text = element_blank()) #+
  # ggsn::scalebar(data = subset(data_plot, village == "Gariuai"), dist = 1, dist_unit = "km", model = 'WGS84', 
  #                st.dist = 0.05, st.size = 4,
  #                transform = TRUE, location = "bottomright")

l1 = ggpubr::get_legend(g2)

g2 = g2 + theme(legend.position = "none")

g3 = ggplot(data = elevS) +
  geom_raster(aes(x = x, y = y, fill=srtm_62_14)) +
  geom_point(data = subset(data_plot, village == "Samalari"),
             aes(x = long, y = lat, color = ID_AF)) + 
  coord_equal() +
  labs(x = "", y = "", title = "Samalari", color = "AF\nsystems", fill="Elevation (m)") +
  viridis::scale_fill_viridis(option = "D",
                              limits = range(c(elevS$srtm_62_14, elevG$srtm_62_14)))+
  guides(color=guide_legend(nrow=2,byrow=TRUE), fill=FALSE) +
  theme(legend.position = "bottom", axis.title = element_blank(), 
        axis.ticks = element_blank(), axis.text = element_blank())+ 
  ggsn::scalebar(data = subset(data_plot, village == "Samalari"),
                 dist = 0.5, dist_unit = "km", model = 'WGS84', 
                 st.dist = 0.1, st.size = 3,
                 transform = TRUE, location = "bottomright")

l2 = ggpubr::get_legend(g3)
g3 = g3 + theme(legend.position = "none")


ggpubr::ggarrange(g1, 
                  ggpubr::ggarrange(g2, 
                                    ggpubr::ggarrange(g3, l1, l2, ncol=1, heights = c(4,1,1)), 
                                    widths = c(4,3), labels = c("b", "c")), 
                  heights = c(2,5), nrow = 2, labels = c("a", ""))


ggsave("figures/mapPlotsSRTM.png", height =6, width = 7)

