### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###  ### 
### Estimation of aboveground biomass in agroforestry systems in Timor Leste ###
### Author: Camille Piponiot                                                 ###
### Correspondance:           camille.piponiot-laroche@cirad.fr              ###
### Date: 2022-06-21                                                         ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###  ###


## 1. Prepare R environment ####

# R packages - installation 

# required packages
req_packages <- c("BIOMASS", "readxl", "data.table", "ggplot2")

# install missing packages
inst_packages <- req_packages[!( req_packages %in% rownames(installed.packages()))]

if (length(inst_packages) > 0)
  install.packages(inst_packages)

# Set working directory to source file location 
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


## 2. Load data from excel file ####

# Open large tree data (> 100 cm circumference)
data_large <- readxl::read_excel("data/DATA_BIOMASS_TL_SHORT.xlsx", "TREE INV >100CM")

# Open large tree data (30-100 cm circumference)
data_small <- readxl::read_excel("data/DATA_BIOMASS_TL_SHORT.xlsx", "TREE INV 30-100CM")

# Open plot information
data_plot <- readxl::read_excel("data/DATA_BIOMASS_TL_SHORT.xlsx", "PLOT_GENERAL_INFO")


## 3. combine tables ####

# create a "N_Q_ID" column in data_large, with corresponding quadrat number =
# "TOT" (entire plot)
data_large$N_Q_ID <- paste(data_large$N_PLOT, "TOT", sep = "_")

# common columns to be kept
col_keep <- c("N_PLOT", "N_Q_ID", "SCIENTIFIC_NAME", "DIAM_130_CM", "HEIGHT")

# stack tree tables
data_tot <- rbind(data_large[, col_keep], data_small[, col_keep])

# merge with plot information
data_tot <- merge(data_tot, data_plot, by = "N_PLOT")

# convert data_tot into data.table 
data.table::setDT(data_tot)


## 4. clean tables ####

# remove NA or <30 cm circumference values 
data_tot <- subset(data_tot, !is.na(DIAM_130_CM) | DIAM_130_CM*pi < 30)

# split scientific names into genus and species
names <- data.table::tstrsplit(data_tot$SCIENTIFIC_NAME, " ")

# get genera
data_tot$GENUS <- names[[1]]

# get species
data_tot$SPECIES <- names[[2]]

# Timoneus should be Timonius
data_tot$GENUS[!is.na(data_tot$GENUS) & data_tot$GENUS == "Timoneus"] <- "Timonius"

# remove one tree with DBH = 0
data_tot <- subset(data_tot, DIAM_130_CM > 0)

## 5. estimate above ground biomass with the Chave et al (2014) equation ####

## get wood density
data_wd <- BIOMASS::getWoodDensity(genus = data_tot$GENUS, species = data_tot$SPECIES) 

# 15% coefficient of variation on height
cvH <- 0.15

### estimate AGB
data_sim <- data.table(
  data_tot,
  BIOMASS::AGBmonteCarlo(
    D = data_tot$DIAM_130_CM, 
    WD = data_wd$meanWD, 
    errWD = data_wd$sdWD, 
    H = data_tot$HEIGHT, 
    errH = data_tot$HEIGHT*cvH, 
    Dpropag = "chave2004", 
    n = 1000,
    Carbon = TRUE
  )$AGC_simu
)

data_sim = melt(
  data_sim,
  measure.vars = paste0("V", 1:1000),
  variable.name = "iter",
  value.name =  "AGC"
)

## 6. apply a different allometry for palms ####

# the volume of the trunk is estimated as the volume of a cylinder with a 11%
# volumetric shrinkage (source: www.wood-database.com/red-palm/). The height of
# palms was measured above the leaves, and was thus multiplied by a 0.9
# conversion factor to get the height below the leaves. We multiply the
# estimated volume by the estimated wood density of the palms (here the
# plot-level mean wood density). Optionally, we could use the wood densities of
# Cocos and Borassus palms at 12% moisture content (source
# www.wood-database.com), but they are not representative of the density of the
# entire stem.

meanWD <- unique(data_wd[data_wd$levelWD == "dataset", "meanWD"])
sdWD <- unique(data_wd[data_wd$levelWD == "dataset", "sdWD"])

palm_genera <- c("Cocos", "Areca", "Arenga", "Corypha", "Borassus")

data_palm <- data_sim[GENUS %in% palm_genera]

# wood density (g/cm^3)
data_palm[, WD := rnorm(nrow(data_palm), meanWD, sdWD)]

## height in cm, with a conversion factor between 0.8 and 1
data_palm[, H := rnorm(nrow(data_palm), HEIGHT, HEIGHT*cvH)*
            runif(nrow(data_palm), .8, 1)
          * 100]

# diameter: error from Chave et al 2004?
data_palm[, DBH := DIAM_130_CM]

data_palm[, AGC := WD * H * (DBH/2)^2 * pi * 
            0.89 * # volumetric shrinkage
            1e-6  # convert from g to tons
]

# group palm AGC estimation with others
data_tot <- rbind(data_sim[!GENUS %in% palm_genera], 
                  data_palm[, colnames(data_sim), with=FALSE])


## 7. add belowground biomass estimation

# root-to-shoot ratio allometry from Ledo et al., 2017
# no associated uncertainty? 

# get cwd from CHELSA 

if (!file.exists("data/CWD_CHELSA.tif")) {
  
  urls <- paste0(
    "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/",
    rep(c("pet/CHELSA_pet_penman_", "pr/CHELSA_pr_"), each = 12),
    rep(paste0(c(rep(0,9), rep("", 3)), 1:12), 2),
    "_1981-2010_V.2.1.tif"
  )
  
  if (!dir.exists("CHELSA")) dir.create("CHELSA")
  
  destfiles <- paste0("CHELSA/", data.table::tstrsplit(urls, "\\/")[[11]])
  
  for (i in seq_len(length(urls))) {
    if (!file.exists(destfiles[i]))
      utils::download.file(urls[i], destfiles[i], method = "curl")
  }
  
  # create CWD raster of Timor Lests
  pr <- terra::crop(terra::rast(grep("_pr_", destfiles, value = TRUE)), raster::extent(c(120, 130, -11, -6)))
  pet <- terra::crop(terra::rast(grep("_pet_", destfiles, value = TRUE)), raster::extent(c(120, 130, -11, -6)))
  
  cwd = sum(pr,  - pet)
  cwd[cwd > 0] = 0
  cwd = sum(cwd)
  
  terra::writeRaster(cwd, file = "data/CWD_CHELSA.tif", overwrite = TRUE)
  
}

# download cwd from Chave et al 2014
if (!file.exists("data/CWD.tif")) {
  utils::download.file("http://chave.ups-tlse.fr/pantropical_allometry/CWD.tif.zip", "data/CWD.tif.zip", method = "curl")
  utils::unzip("data/CWD.tif.zip", exdir = "data", overwrite = TRUE)
}

cwdMap = terra::rast("data/CWD.tif")
data_coord <- read.csv("data/coords-timor.csv")
data.table::setDT(data_coord)

## change this
data_coord$CWD <- terra::extract(cwdMap, data_coord[, c("LONG", "LAT")])$CWD
data_coord[, N_PLOT := as.numeric(data.table::tstrsplit(N_Q_ID, "_")[[1]])]

data_cwd <- data_coord[, .(CWD = mean(CWD)), .(N_PLOT)]

data_tot <- merge(data_tot, data_cwd, by = "N_PLOT", all.x = TRUE)

data_tot$RS = exp(- 1.2312 - 0.0215 * data_tot$DIAM_130_CM + 0.0002 *  data_tot$DIAM_130_CM^2 - 0.0007 * data_tot$CWD)
data_tot$BGC  = data_tot$RS * data_tot$AGC


## 8. estimate plot AGB and BGB ####

# get plot area
data_tot[, AREA := AREA_TOTAL_PARCEL]

# type of plot (total or quadrat)
data_tot[, TYPE := ifelse(grepl("Q", data_tot$N_Q_ID), "QUADRAT", "TOTAL")]

# replace with area = 0.05 for quadrats (5 quadrats of 0.01 ha each)
data_tot[TYPE == "QUADRAT", AREA := 0.05]

# sum AGB of trees in each plot and quadrat, and standardize by area (to Mg/ha)
data_agb_unc <- data_tot[, .(AGC = sum(AGC/AREA), BGC = sum(BGC/AREA)), .(N_PLOT, ID_VILLAGE, ID_AF, iter)]

data_agb <- data_agb_unc[, .(meanAGC = mean(AGC), 
                             lwrAGC = quantile(AGC, 0.025), 
                             medAGC = quantile(AGC, 0.5), 
                             uprAGC = quantile(AGC, 0.975), 
                             meanBGC = mean(BGC), 
                             lwrBGC = quantile(BGC, 0.025), 
                             medBGC = quantile(BGC, 0.5), 
                             uprBGC = quantile(BGC, 0.975)), 
                         .(N_PLOT, ID_VILLAGE, ID_AF)]


## optional: graphs ####

graphs = FALSE

if (graphs) {
  
  # install ggplot2
  if (!("ggplot2" %in% rownames(installed.packages())))
    install.packages("ggplot2")
  
  library(ggplot2)
  
  data_agb[, VILLAGE := substr(ID_VILLAGE, 1, 1)]
  
  ggplot(data_agb, aes(x = ID_AF, y = meanAGC, color = VILLAGE)) + 
    geom_boxplot() +
    geom_point()
  
}
