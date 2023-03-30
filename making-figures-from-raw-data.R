
# set working directory to match the source file
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# install and load packages

packages_needed = c("ggplot2", "dataverse", "readxl", "data.table")

packages_to_install = packages_needed[!( packages_needed %in% rownames(installed.packages()))]

if (length(packages_to_install) > 0)
  install.packages(packages_to_install)

lapply(packages_needed, require, character.only = TRUE)

## download data from dataverse - once published
dataverse_url <- "https://dataverse.cirad.fr/privateurl.xhtml?token=f8eabb2a-3635-499b-aee5-80fd2b7495a0"
dataverse_doi <- "10.18167/DVN1/QCXWIY"


# for now : download manually from temporary link
# utils::unzip("dataverse_files.zip")

# get agb data
source("script-agb.R")


# Prepare the soil carbon data ####

# open organic carbon data
# bulk density in Gariuai according to SoilGrids = 125 cg/cm3
data_soil <- readxl::read_excel("data/RESUME_DATA_SOIL_SHORT_TL.xls", sheet = "RESUME-LAB ")

# add bulk density and coars fraction
data_soil2 <- readxl::read_excel("data/RAW_DATA_SOIL_LARGE_TL.xls", sheet = "SAMPLING_MEASURE")

# bulk density from direct measurements
data_soil2$BD <- data_soil2$Sample_DRY/data_soil2$V_cylinder

# coarse fraction
data_soil2$CF <- data_soil2$V_rocks_tot/data_soil2$V_cylinder

# change format of depth to correspond to other soil table
data_soil2$SAMPLE_DEPTH <- factor(data_soil2$DEPTH_SAMPLE_C, levels = c("2_7", "12_17", "22_27"))
levels(data_soil2$SAMPLE_DEPTH) <- c("0-10", "10-20", "20-30")

data_soil <- merge(data_soil, data_soil2, by = c("N_PLOT", "SAMPLE_DEPTH"), all=TRUE)

# bulk density from pedotransfer function: Minasny and Hartemink, 2011

data_soil$depth_mean <- unlist(lapply(strsplit(data_soil$SAMPLE_DEPTH, "-"), function(x) mean(as.numeric(x))))
data_soil$depth_max <- unlist(lapply(strsplit(data_soil$SAMPLE_DEPTH, "-"), function(x) as.numeric(x[2])))

data_soil$sand <- as.numeric(data_soil$`FINE SAND`) + as.numeric(data_soil$`COARSE SAND`)/100

data_soil$BDmin <- 
  0.935 + 
  0.049 * log(data_soil$depth_mean) + 
  0.0055 * data_soil$sand + 
  0.000065 * (data_soil$sand - 38.96)^2

data_soil$BDptf <- 
  100/(
    data_soil$OM/0.224 + 
      (100 - data_soil$OM)/data_soil$BDmin
  )

g1 = ggplot(data_soil, aes(x = BD, y = BDptf, color = ID_TYPE)) + 
  geom_abline(slope = 1, intercept = 0, lty=2) + 
  geom_point() + 
  lims(x = c(0.8, 1.6), y = c(0.8, 1.6)) +
  facet_wrap(~ID_VILLAGE)
#+
  # theme(legend.position = "none")
g2 = ggplot(data_soil, aes(x = BD, y = BDmin, color = SAMPLE_DEPTH)) +
  geom_abline(slope = 1, intercept = 0, lty=2) + 
  geom_point() + 
  lims(x = c(0.8, 1.6), y = c(0.8, 1.6)) +
  theme(legend.position = c(0.2, 0.8))

ggpubr::ggarrange(g1, g2) 
ggsave("figures/pedotransfer-test.png", height = 5, width = 10)

ggplot(data_soil, aes(x = SAMPLE_DEPTH, y = BDmin, group = N_PLOT)) + 
  geom_line() + 
  geom_point(aes(y = BDptf), col=2) +
  facet_wrap( ~ N_PLOT) + 
  coord_flip() +
  scale_x_discrete(limits=rev)

ggplot(data_soil, aes(x = SAMPLE_DEPTH, y = `Corg\n`, group = N_PLOT)) + 
  geom_point() + 
  facet_wrap( ~ N_PLOT) + 
  coord_flip() +
  scale_x_discrete(limits=rev)

data.table::setDT(data_soil)

## add intermediate soil carbon in 10-20 cm depth (for now: no value)
data_soil[, Corg := mean(`Corg\n`, na.rm = TRUE), .(N_PLOT)]
data_soil[!is.na(`Corg\n`), Corg := `Corg\n`]

# replace one missing value of coarse fraction with plot level mean
data_soil[, CF_site := mean(CF, na.rm = TRUE), .(N_PLOT)]
data_soil[is.na(CF), CF := CF_site]

# calculate total soil carbon stock per depth
data_soil[, c("h0", "h1") := lapply(data.table::tstrsplit(SAMPLE_DEPTH, "-"), as.numeric)]
data_soil[, Cstock :=          ## in Mg/ha
            (h1 - h0) * 1e8 *  ## equivalent volume of a 1 ha soil sample, in cm3/ha
            BD *               ## bulk density 
            Corg/100 *         ## carbon organic content in soil
            (1-CF) *           ## remove coarse fraction
            1e-6               ## convert from g/ha to Mg/ha
]

data_soil_C <- data_soil[, .(meanSC = sum(Cstock), clay = mean(as.numeric(CLAY), na.rm = TRUE)), .(N_PLOT)]

## error propagation using a Monte Carlo method -------------
data_soil_unc = data_soil[, .(BD = rnorm(1000, BD, BD*0.1), 
                              Corg = rnorm(1000, Corg, Corg*0.12), 
                              CF = rnorm(1000, CF, CF*0.15), 
                              iter = 1:1000), 
                          .(N_PLOT, SAMPLE_DEPTH, h1, h0)]

data_soil_unc = data_soil_unc[, .(Cstock = sum(
  (h1 - h0) * 1e8 *  
    BD * Corg/100 * (1-CF) * 1e-6  
)),
.(N_PLOT, iter)
]

data_soil_unc = data_soil_unc[, .(
  lwrSC = quantile(Cstock, 0.025), 
  uprSC = quantile(Cstock, 0.975)), 
  .(N_PLOT)]


# combine AGB and soil C
dataC <- merge(data_agb, data_soil_C)

# add soil carbon uncertainty
dataC <- merge(dataC, data_soil_unc)

# add belowground biomass estimate
dataC$meanBGC <- 0.25*dataC$meanAGC

dataC[, village := substr(ID_VILLAGE, 1, 1)]
dataC$ID_AF <- factor(dataC$ID_AF, levels = c("CF", "SP", "YA", "HG", "FG"))

# average values per village and SAF type
dataC_mean <- dataC[, .(AGC_mean = mean(meanAGC), SC_mean = mean(meanSC), 
                        AGC_se = sd(meanAGC)/sqrt(length(meanAGC)), 
                        SC_se = sd(meanSC)/sqrt(length(meanSC))), .(ID_AF)]

# stat tests
summary(lme4::lmer(meanSC ~ meanAGC + clay + (meanAGC | village), data = dataC))

summary(aov(meanAGC ~ ID_AF + village, data = dataC))
summary(aov(meanSC ~ ID_AF + clay + village, data = dataC))

# regression coefficients
dataC_reg <- dataC[, .(intercept = summary(lm(meanSC ~ meanAGC))$coefficients[1,1], 
                       slope = summary(lm(meanSC ~ meanAGC))$coefficients[2,1], 
                       pvalue = summary(lm(meanSC ~ meanAGC))$coefficients[2,4]), .(village)]

## figure 
ggplot(dataC, aes(color = ID_AF)) +
  geom_point(aes(x = meanAGC, y = meanSC, shape = village), size = 2) + 
  geom_linerange(aes(x = meanAGC, y = meanSC, ymin = lwrSC, ymax = uprSC)) +
  geom_errorbarh(aes(y = meanSC, xmin = lwrAGC, xmax = uprAGC)) +
  geom_abline(data = dataC_reg, aes(intercept = intercept, slope = slope, lty = village)) +
  labs(color = "AF system", x = "Aboveground C (Mg C/ha)", y = "Soil C (Mg C/ha)") + 
  expand_limits(x = 0, y = 0) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  scale_linetype_manual(values=c("twodash", "dotted"))+
  theme_classic() 
ggsave("figures/fig-scatterplot-carbon.png", height = 5, width = 7)

## melt data.table
dataC <- melt(dataC, id.vars = c("N_PLOT", "ID_VILLAGE", "ID_AF", "village"), 
              measure.vars = c("meanAGC", "meanBGC", "meanSC"))

# ideally, use each iteration for uncertainty estimation + estimate spatial uncertainty

dataC_all <- dataC[, .(value = mean(value)), .(village, ID_AF, variable)]


## reorder factors 

dataC_all$ID_AF <- factor(dataC_all$ID_AF, levels = c("CF", "SP", "YA", "HG", "FG"))

# uncertainty fig 2: bootstrap by MC iteration?
dataC_all[, variable := factor(variable, levels = c("meanAGC", "meanBGC", "meanSC"))]
levels(dataC_all$variable) <- c("AGC", "BGC", "SOC")

## figure 
ggplot(dataC_all, aes(x = village, y = value, fill = variable)) +
  geom_bar(position="stack", stat="identity") + 
  facet_wrap(~ID_AF, nrow = 1, strip.position = "bottom") +
  scale_fill_manual(values = c("forestgreen", "darkgoldenrod1", "coral4")) +
  theme_classic() +
  theme(strip.placement = "outside") +
  labs(x = "", y = "Carbon stocks (Mg C/ha)", fill = "Carbon\npool") +
  scale_y_continuous(expand = c(0, 0))
ggsave("figures/fig-carbon-stocks-stack.png", height = 5, width = 8)




## diagnostic données sol pour marguerite
g1 <- ggplot(data_soil, aes(x = CF*100, col = SAMPLE_DEPTH)) + 
  geom_histogram(bins = 30) +
  geom_text(data = subset(data_soil, CF > 0.3), angle = -60, 
            aes(x = CF*100-5, y = 15, 
                label = paste("Village:", ID_VILLAGE, "/ SAF:", ID_TYPE, "/ Plot:", N_PLOT))) +
  labs(x = "Elements grossiers (% du volume)", 
       y = "Nombre d'échantillons (site x profondeur)", 
       col = "Profondeur (cm)") + 
  scale_y_continuous(expand = expand_scale(0, 0)) +
  theme_classic()  +
  theme(legend.position = c(0.8, 0.9))

g2 <- ggplot(data_soil, aes(x = Corg, col = SAMPLE_DEPTH)) + 
  geom_histogram(bins = 20) +
  labs(x = "Teneur en carbone (%)", 
       y = "Nombre d'échantillons (site x profondeur)", 
       col = "Profondeur (cm)") + 
  scale_y_continuous(expand = expand_scale(0, 0)) +
  theme_classic() +
  theme(legend.position = c(0.8, 0.9))

g3 <- ggplot(data_soil, aes(x = BD, col = SAMPLE_DEPTH)) + 
  geom_histogram(bins = 20) +
  labs(x = "Densité apparente", 
       y = "Nombre d'échantillons (site x profondeur)", 
       col = "Profondeur (cm)") + 
  scale_y_continuous(expand = expand_scale(0, 0)) +
  theme_classic()+
  theme(legend.position = c(0.8, 0.9))

ggpubr::ggarrange(g1, g2, g3, labels = "auto", nrow = 1)
ggsave("figures/fig-distr-sol.pdf", height = 5, width = 15)


ggplot(data_soil, aes(x = as.numeric(CLAY), y = Corg, col = SAMPLE_DEPTH)) +
  geom_point() +
  labs(col = "Profondeur (cm)", x = "Argile (%)", y = "Teneur en carbone (%)") + 
  geom_smooth(method = "lm") +
  theme_classic()
ggsave("figures/fig-clay-corg.pdf", height = 4, width = 6)
