### INTRODUCTION #####

# Welcome. For the description of the project please visit: https://github.com/diekei/2023_ENS_diekei_host_shift
# Article is available at: https://www.biorxiv.org/content/10.1101/2023.07.12.548653v1


### LIBRARY #####

library(StatMatch)
library(gplots)
library(viridis)
library(reshape2)
library(tidyverse)
library(ggplot2)


### LOAD DATA #####

load('raster_D_FR.rda')
load('raster_D_FL.rda')
load('raster_D_HR.rda')
load('raster_D_HL.rda')
load('raster_V_FR.rda')
load('raster_V_FL.rda')
load('raster_V_HR.rda')
load('raster_V_HL.rda')


### ANALYSIS - CALCULATING PATTERN AREA DIFFERENCES #####

# compute gower's dissimilarity matrix
gwd_D_FR <- gower.dist(raster_D_FR)
gwd_D_FL <- gower.dist(raster_D_FL)
gwd_D_HR <- gower.dist(raster_D_HR)
gwd_D_HL <- gower.dist(raster_D_HL)
gwd_V_FR <- gower.dist(raster_V_FR)
gwd_V_FL <- gower.dist(raster_V_FL)
gwd_V_HR <- gower.dist(raster_V_HR)
gwd_V_HL <- gower.dist(raster_V_HL)

set_names <- row.names(raster_D_FR)

rownames(gwd_D_FR) <- set_names
colnames(gwd_D_FR) <- set_names
rownames(gwd_D_FL) <- set_names
colnames(gwd_D_FL) <- set_names
rownames(gwd_D_HR) <- set_names
colnames(gwd_D_HR) <- set_names
rownames(gwd_D_HL) <- set_names
colnames(gwd_D_HL) <- set_names
rownames(gwd_V_FR) <- set_names
colnames(gwd_V_FR) <- set_names
rownames(gwd_V_FL) <- set_names
colnames(gwd_V_FL) <- set_names
rownames(gwd_V_HR) <- set_names
colnames(gwd_V_HR) <- set_names
rownames(gwd_V_HL) <- set_names
colnames(gwd_V_HL) <- set_names

save(gwd_D_FR, file = 'data/gwd_D_FR.rda', row.names = T)
save(gwd_D_FL, file = 'data/gwd_D_FL.rda', row.names = T)
save(gwd_D_HR, file = 'data/gwd_D_HR.rda', row.names = T)
save(gwd_D_HL, file = 'data/gwd_D_HL.rda', row.names = T)
save(gwd_V_FR, file = 'data/gwd_V_FR.rda', row.names = T)
save(gwd_V_FL, file = 'data/gwd_V_FL.rda', row.names = T)
save(gwd_V_HR, file = 'data/gwd_V_HR.rda', row.names = T)
save(gwd_V_HL, file = 'data/gwd_V_HL.rda', row.names = T)

load('data/gwd_D_FR.rda')
load('data/gwd_D_FL.rda')
load('data/gwd_D_HR.rda')
load('data/gwd_D_HL.rda')
load('data/gwd_V_FR.rda')
load('data/gwd_V_FL.rda')
load('data/gwd_V_HR.rda')
load('data/gwd_V_HL.rda')

# quick visualisation example
heatmap.2(gwd_D_FL, scale = "none", col = viridis(100), 
          trace = "none", density.info = "none")

# matrix manipulation for visualisation

for(k in 1:ncol(gwd_D_FR)){
  for(j in 1:nrow(gwd_D_FR)){
    if(k > j){
      gwd_D_FR[j,k] <- 0
    }
  }
}

for(k in 1:ncol(gwd_D_FL)){
  for(j in 1:nrow(gwd_D_FL)){
    if(k > j){
      gwd_D_FL[j,k] <- 0
    }
  }
}

for(k in 1:ncol(gwd_D_HR)){
  for(j in 1:nrow(gwd_D_HR)){
    if(k > j){
      gwd_D_HR[j,k] <- 0
    }
  }
}

for(k in 1:ncol(gwd_D_HL)){
  for(j in 1:nrow(gwd_D_HL)){
    if(k > j){
      gwd_D_HL[j,k] <- 0
    }
  }
}

for(k in 1:ncol(gwd_V_FR)){
  for(j in 1:nrow(gwd_V_FR)){
    if(k > j){
      gwd_V_FR[j,k] <- 0
    }
  }
}

for(k in 1:ncol(gwd_V_FL)){
  for(j in 1:nrow(gwd_V_FL)){
    if(k > j){
      gwd_V_FL[j,k] <- 0
    }
  }
}

for(k in 1:ncol(gwd_V_HR)){
  for(j in 1:nrow(gwd_V_HR)){
    if(k > j){
      gwd_V_HR[j,k] <- 0
    }
  }
}

for(k in 1:ncol(gwd_V_HL)){
  for(j in 1:nrow(gwd_V_HL)){
    if(k > j){
      gwd_V_HL[j,k] <- 0
    }
  }
}


gwd_D_FR_KOF_KOF <- gwd_D_FR[1:26,1:26]
gwd_D_FR_KOM_KOM <- gwd_D_FR[27:46, 27:46]
gwd_D_FR_KOM_KOF <- gwd_D_FR[27:46, 1:26]
gwd_D_FR_NDF_KOF <- gwd_D_FR[47:69, 1:26]
gwd_D_FR_NDM_KOM <- gwd_D_FR[70:88, 27:46]
gwd_D_FR_NDF_NDF <- gwd_D_FR[47:69, 47:69]
gwd_D_FR_NDM_NDM <- gwd_D_FR[70:88, 70:88]
gwd_D_FR_NDM_NDF <- gwd_D_FR[70:88, 47:69]

gwd_D_FL_KOF_KOF <- gwd_D_FL[1:26,1:26]
gwd_D_FL_KOM_KOM <- gwd_D_FL[27:46, 27:46]
gwd_D_FL_KOM_KOF <- gwd_D_FL[27:46, 1:26]
gwd_D_FL_NDF_KOF <- gwd_D_FL[47:69, 1:26]
gwd_D_FL_NDM_KOM <- gwd_D_FL[70:88, 27:46]
gwd_D_FL_NDF_NDF <- gwd_D_FL[47:69, 47:69]
gwd_D_FL_NDM_NDM <- gwd_D_FL[70:88, 70:88]
gwd_D_FL_NDM_NDF <- gwd_D_FL[70:88, 47:69]

gwd_D_HR_KOF_KOF <- gwd_D_HR[1:26,1:26]
gwd_D_HR_KOM_KOM <- gwd_D_HR[27:46, 27:46]
gwd_D_HR_KOM_KOF <- gwd_D_HR[27:46, 1:26]
gwd_D_HR_NDF_KOF <- gwd_D_HR[47:69, 1:26]
gwd_D_HR_NDM_KOM <- gwd_D_HR[70:88, 27:46]
gwd_D_HR_NDF_NDF <- gwd_D_HR[47:69, 47:69]
gwd_D_HR_NDM_NDM <- gwd_D_HR[70:88, 70:88]
gwd_D_HR_NDM_NDF <- gwd_D_HR[70:88, 47:69]

gwd_D_HL_KOF_KOF <- gwd_D_HL[1:26,1:26]
gwd_D_HL_KOM_KOM <- gwd_D_HL[27:46, 27:46]
gwd_D_HL_KOM_KOF <- gwd_D_HL[27:46, 1:26]
gwd_D_HL_NDF_KOF <- gwd_D_HL[47:69, 1:26]
gwd_D_HL_NDM_KOM <- gwd_D_HL[70:88, 27:46]
gwd_D_HL_NDF_NDF <- gwd_D_HL[47:69, 47:69]
gwd_D_HL_NDM_NDM <- gwd_D_HL[70:88, 70:88]
gwd_D_HL_NDM_NDF <- gwd_D_HL[70:88, 47:69]

gwd_V_FR_KOF_KOF <- gwd_V_FR[1:26,1:26]
gwd_V_FR_KOM_KOM <- gwd_V_FR[27:46, 27:46]
gwd_V_FR_KOM_KOF <- gwd_V_FR[27:46, 1:26]
gwd_V_FR_NDF_KOF <- gwd_V_FR[47:69, 1:26]
gwd_V_FR_NDM_KOM <- gwd_V_FR[70:88, 27:46]
gwd_V_FR_NDF_NDF <- gwd_V_FR[47:69, 47:69]
gwd_V_FR_NDM_NDM <- gwd_V_FR[70:88, 70:88]
gwd_V_FR_NDM_NDF <- gwd_V_FR[70:88, 47:69]

gwd_V_FL_KOF_KOF <- gwd_V_FL[1:26,1:26]
gwd_V_FL_KOM_KOM <- gwd_V_FL[27:46, 27:46]
gwd_V_FL_KOM_KOF <- gwd_V_FL[27:46, 1:26]
gwd_V_FL_NDF_KOF <- gwd_V_FL[47:69, 1:26]
gwd_V_FL_NDM_KOM <- gwd_V_FL[70:88, 27:46]
gwd_V_FL_NDF_NDF <- gwd_V_FL[47:69, 47:69]
gwd_V_FL_NDM_NDM <- gwd_V_FL[70:88, 70:88]
gwd_V_FL_NDM_NDF <- gwd_V_FL[70:88, 47:69]

gwd_V_HR_KOF_KOF <- gwd_V_HR[1:26,1:26]
gwd_V_HR_KOM_KOM <- gwd_V_HR[27:46, 27:46]
gwd_V_HR_KOM_KOF <- gwd_V_HR[27:46, 1:26]
gwd_V_HR_NDF_KOF <- gwd_V_HR[47:69, 1:26]
gwd_V_HR_NDM_KOM <- gwd_V_HR[70:88, 27:46]
gwd_V_HR_NDF_NDF <- gwd_V_HR[47:69, 47:69]
gwd_V_HR_NDM_NDM <- gwd_V_HR[70:88, 70:88]
gwd_V_HR_NDM_NDF <- gwd_V_HR[70:88, 47:69]

gwd_V_HL_KOF_KOF <- gwd_V_HL[1:26,1:26]
gwd_V_HL_KOM_KOM <- gwd_V_HL[27:46, 27:46]
gwd_V_HL_KOM_KOF <- gwd_V_HL[27:46, 1:26]
gwd_V_HL_NDF_KOF <- gwd_V_HL[47:69, 1:26]
gwd_V_HL_NDM_KOM <- gwd_V_HL[70:88, 27:46]
gwd_V_HL_NDF_NDF <- gwd_V_HL[47:69, 47:69]
gwd_V_HL_NDM_NDM <- gwd_V_HL[70:88, 70:88]
gwd_V_HL_NDM_NDF <- gwd_V_HL[70:88, 47:69]

gwd_D_FR_KOF_KOF <- melt(gwd_D_FR_KOF_KOF, id.vars=1)
gwd_D_FR_KOM_KOM <- melt(gwd_D_FR_KOM_KOM, id.vars=1)
gwd_D_FR_KOM_KOF <- melt(gwd_D_FR_KOM_KOF, id.vars=1)
gwd_D_FR_NDF_KOF <- melt(gwd_D_FR_NDF_KOF, id.vars=1)
gwd_D_FR_NDM_KOM <- melt(gwd_D_FR_NDM_KOM, id.vars=1)
gwd_D_FR_NDF_NDF <- melt(gwd_D_FR_NDF_NDF, id.vars=1)
gwd_D_FR_NDM_NDM <- melt(gwd_D_FR_NDM_NDM, id.vars=1)
gwd_D_FR_NDM_NDF <- melt(gwd_D_FR_NDM_NDF, id.vars=1)

gwd_D_FL_KOF_KOF <- melt(gwd_D_FL_KOF_KOF, id.vars=1)
gwd_D_FL_KOM_KOM <- melt(gwd_D_FL_KOM_KOM, id.vars=1)
gwd_D_FL_KOM_KOF <- melt(gwd_D_FL_KOM_KOF, id.vars=1)
gwd_D_FL_NDF_KOF <- melt(gwd_D_FL_NDF_KOF, id.vars=1)
gwd_D_FL_NDM_KOM <- melt(gwd_D_FL_NDM_KOM, id.vars=1)
gwd_D_FL_NDF_NDF <- melt(gwd_D_FL_NDF_NDF, id.vars=1)
gwd_D_FL_NDM_NDM <- melt(gwd_D_FL_NDM_NDM, id.vars=1)
gwd_D_FL_NDM_NDF <- melt(gwd_D_FL_NDM_NDF, id.vars=1)

gwd_D_HR_KOF_KOF <- melt(gwd_D_HR_KOF_KOF, id.vars=1)
gwd_D_HR_KOM_KOM <- melt(gwd_D_HR_KOM_KOM, id.vars=1)
gwd_D_HR_KOM_KOF <- melt(gwd_D_HR_KOM_KOF, id.vars=1)
gwd_D_HR_NDF_KOF <- melt(gwd_D_HR_NDF_KOF, id.vars=1)
gwd_D_HR_NDM_KOM <- melt(gwd_D_HR_NDM_KOM, id.vars=1)
gwd_D_HR_NDF_NDF <- melt(gwd_D_HR_NDF_NDF, id.vars=1)
gwd_D_HR_NDM_NDM <- melt(gwd_D_HR_NDM_NDM, id.vars=1)
gwd_D_HR_NDM_NDF <- melt(gwd_D_HR_NDM_NDF, id.vars=1)

gwd_D_HL_KOF_KOF <- melt(gwd_D_HL_KOF_KOF, id.vars=1)
gwd_D_HL_KOM_KOM <- melt(gwd_D_HL_KOM_KOM, id.vars=1)
gwd_D_HL_KOM_KOF <- melt(gwd_D_HL_KOM_KOF, id.vars=1)
gwd_D_HL_NDF_KOF <- melt(gwd_D_HL_NDF_KOF, id.vars=1)
gwd_D_HL_NDM_KOM <- melt(gwd_D_HL_NDM_KOM, id.vars=1)
gwd_D_HL_NDF_NDF <- melt(gwd_D_HL_NDF_NDF, id.vars=1)
gwd_D_HL_NDM_NDM <- melt(gwd_D_HL_NDM_NDM, id.vars=1)
gwd_D_HL_NDM_NDF <- melt(gwd_D_HL_NDM_NDF, id.vars=1)

gwd_V_FR_KOF_KOF <- melt(gwd_V_FR_KOF_KOF, id.vars=1)
gwd_V_FR_KOM_KOM <- melt(gwd_V_FR_KOM_KOM, id.vars=1)
gwd_V_FR_KOM_KOF <- melt(gwd_V_FR_KOM_KOF, id.vars=1)
gwd_V_FR_NDF_KOF <- melt(gwd_V_FR_NDF_KOF, id.vars=1)
gwd_V_FR_NDM_KOM <- melt(gwd_V_FR_NDM_KOM, id.vars=1)
gwd_V_FR_NDF_NDF <- melt(gwd_V_FR_NDF_NDF, id.vars=1)
gwd_V_FR_NDM_NDM <- melt(gwd_V_FR_NDM_NDM, id.vars=1)
gwd_V_FR_NDM_NDF <- melt(gwd_V_FR_NDM_NDF, id.vars=1)

gwd_V_FL_KOF_KOF <- melt(gwd_V_FL_KOF_KOF, id.vars=1)
gwd_V_FL_KOM_KOM <- melt(gwd_V_FL_KOM_KOM, id.vars=1)
gwd_V_FL_KOM_KOF <- melt(gwd_V_FL_KOM_KOF, id.vars=1)
gwd_V_FL_NDF_KOF <- melt(gwd_V_FL_NDF_KOF, id.vars=1)
gwd_V_FL_NDM_KOM <- melt(gwd_V_FL_NDM_KOM, id.vars=1)
gwd_V_FL_NDF_NDF <- melt(gwd_V_FL_NDF_NDF, id.vars=1)
gwd_V_FL_NDM_NDM <- melt(gwd_V_FL_NDM_NDM, id.vars=1)
gwd_V_FL_NDM_NDF <- melt(gwd_V_FL_NDM_NDF, id.vars=1)

gwd_V_HR_KOF_KOF <- melt(gwd_V_HR_KOF_KOF, id.vars=1)
gwd_V_HR_KOM_KOM <- melt(gwd_V_HR_KOM_KOM, id.vars=1)
gwd_V_HR_KOM_KOF <- melt(gwd_V_HR_KOM_KOF, id.vars=1)
gwd_V_HR_NDF_KOF <- melt(gwd_V_HR_NDF_KOF, id.vars=1)
gwd_V_HR_NDM_KOM <- melt(gwd_V_HR_NDM_KOM, id.vars=1)
gwd_V_HR_NDF_NDF <- melt(gwd_V_HR_NDF_NDF, id.vars=1)
gwd_V_HR_NDM_NDM <- melt(gwd_V_HR_NDM_NDM, id.vars=1)
gwd_V_HR_NDM_NDF <- melt(gwd_V_HR_NDM_NDF, id.vars=1)

gwd_V_HL_KOF_KOF <- melt(gwd_V_HL_KOF_KOF, id.vars=1)
gwd_V_HL_KOM_KOM <- melt(gwd_V_HL_KOM_KOM, id.vars=1)
gwd_V_HL_KOM_KOF <- melt(gwd_V_HL_KOM_KOF, id.vars=1)
gwd_V_HL_NDF_KOF <- melt(gwd_V_HL_NDF_KOF, id.vars=1)
gwd_V_HL_NDM_KOM <- melt(gwd_V_HL_NDM_KOM, id.vars=1)
gwd_V_HL_NDF_NDF <- melt(gwd_V_HL_NDF_NDF, id.vars=1)
gwd_V_HL_NDM_NDM <- melt(gwd_V_HL_NDM_NDM, id.vars=1)
gwd_V_HL_NDM_NDF <- melt(gwd_V_HL_NDM_NDF, id.vars=1)

gwd_D_FR_KOF_KOF$pair <- "female mKO"
gwd_D_FR_KOM_KOM$pair <- "male mKO"
gwd_D_FR_KOM_KOF$pair <- "mKO"
gwd_D_FR_NDF_KOF$pair <- "female ND - female mKO"
gwd_D_FR_NDM_KOM$pair <- "male ND - male mKO"
gwd_D_FR_NDF_NDF$pair <- "female ND"
gwd_D_FR_NDM_NDM$pair <- "male ND"
gwd_D_FR_NDM_NDF$pair <- "ND"

gwd_D_FL_KOF_KOF$pair <- "female mKO"
gwd_D_FL_KOM_KOM$pair <- "male mKO"
gwd_D_FL_KOM_KOF$pair <- "mKO"
gwd_D_FL_NDF_KOF$pair <- "female ND - female mKO"
gwd_D_FL_NDM_KOM$pair <- "male ND - male mKO"
gwd_D_FL_NDF_NDF$pair <- "female ND"
gwd_D_FL_NDM_NDM$pair <- "male ND"
gwd_D_FL_NDM_NDF$pair <- "ND"

gwd_D_HR_KOF_KOF$pair <- "female mKO"
gwd_D_HR_KOM_KOM$pair <- "male mKO"
gwd_D_HR_KOM_KOF$pair <- "mKO"
gwd_D_HR_NDF_KOF$pair <- "female ND - female mKO"
gwd_D_HR_NDM_KOM$pair <- "male ND - male mKO"
gwd_D_HR_NDF_NDF$pair <- "female ND"
gwd_D_HR_NDM_NDM$pair <- "male ND"
gwd_D_HR_NDM_NDF$pair <- "ND"

gwd_D_HL_KOF_KOF$pair <- "female mKO"
gwd_D_HL_KOM_KOM$pair <- "male mKO"
gwd_D_HL_KOM_KOF$pair <- "mKO"
gwd_D_HL_NDF_KOF$pair <- "female ND - female mKO"
gwd_D_HL_NDM_KOM$pair <- "male ND - male mKO"
gwd_D_HL_NDF_NDF$pair <- "female ND"
gwd_D_HL_NDM_NDM$pair <- "male ND"
gwd_D_HL_NDM_NDF$pair <- "ND"

gwd_V_FR_KOF_KOF$pair <- "female mKO"
gwd_V_FR_KOM_KOM$pair <- "male mKO"
gwd_V_FR_KOM_KOF$pair <- "mKO"
gwd_V_FR_NDF_KOF$pair <- "female ND - female mKO"
gwd_V_FR_NDM_KOM$pair <- "male ND - male mKO"
gwd_V_FR_NDF_NDF$pair <- "female ND"
gwd_V_FR_NDM_NDM$pair <- "male ND"
gwd_V_FR_NDM_NDF$pair <- "ND"

gwd_V_FL_KOF_KOF$pair <- "female mKO"
gwd_V_FL_KOM_KOM$pair <- "male mKO"
gwd_V_FL_KOM_KOF$pair <- "mKO"
gwd_V_FL_NDF_KOF$pair <- "female ND - female mKO"
gwd_V_FL_NDM_KOM$pair <- "male ND - male mKO"
gwd_V_FL_NDF_NDF$pair <- "female ND"
gwd_V_FL_NDM_NDM$pair <- "male ND"
gwd_V_FL_NDM_NDF$pair <- "ND"

gwd_V_HR_KOF_KOF$pair <- "female mKO"
gwd_V_HR_KOM_KOM$pair <- "male mKO"
gwd_V_HR_KOM_KOF$pair <- "mKO"
gwd_V_HR_NDF_KOF$pair <- "female ND - female mKO"
gwd_V_HR_NDM_KOM$pair <- "male ND - male mKO"
gwd_V_HR_NDF_NDF$pair <- "female ND"
gwd_V_HR_NDM_NDM$pair <- "male ND"
gwd_V_HR_NDM_NDF$pair <- "ND"

gwd_V_HL_KOF_KOF$pair <- "female mKO"
gwd_V_HL_KOM_KOM$pair <- "male mKO"
gwd_V_HL_KOM_KOF$pair <- "mKO"
gwd_V_HL_NDF_KOF$pair <- "female ND - female mKO"
gwd_V_HL_NDM_KOM$pair <- "male ND - male mKO"
gwd_V_HL_NDF_NDF$pair <- "female ND"
gwd_V_HL_NDM_NDM$pair <- "male ND"
gwd_V_HL_NDM_NDF$pair <- "ND"

gwd_D_FR_KOF_KOF$comparison <- "(within)"
gwd_D_FR_KOM_KOM$comparison <- "(within)"
gwd_D_FR_KOM_KOF$comparison <- "(sex)"
gwd_D_FR_NDF_KOF$comparison <- "(between)"
gwd_D_FR_NDM_KOM$comparison <- "(between)"
gwd_D_FR_NDF_NDF$comparison <- "(within)"
gwd_D_FR_NDM_NDM$comparison <- "(within)"
gwd_D_FR_NDM_NDF$comparison <- "(sex)"

gwd_D_FL_KOF_KOF$comparison <- "(within)"
gwd_D_FL_KOM_KOM$comparison <- "(within)"
gwd_D_FL_KOM_KOF$comparison <- "(sex)"
gwd_D_FL_NDF_KOF$comparison <- "(between)"
gwd_D_FL_NDM_KOM$comparison <- "(between)"
gwd_D_FL_NDF_NDF$comparison <- "(within)"
gwd_D_FL_NDM_NDM$comparison <- "(within)"
gwd_D_FL_NDM_NDF$comparison <- "(sex)"

gwd_D_HR_KOF_KOF$comparison <- "(within)"
gwd_D_HR_KOM_KOM$comparison <- "(within)"
gwd_D_HR_KOM_KOF$comparison <- "(sex)"
gwd_D_HR_NDF_KOF$comparison <- "(between)"
gwd_D_HR_NDM_KOM$comparison <- "(between)"
gwd_D_HR_NDF_NDF$comparison <- "(within)"
gwd_D_HR_NDM_NDM$comparison <- "(within)"
gwd_D_HR_NDM_NDF$comparison <- "(sex)"

gwd_D_HL_KOF_KOF$comparison <- "(within)"
gwd_D_HL_KOM_KOM$comparison <- "(within)"
gwd_D_HL_KOM_KOF$comparison <- "(sex)"
gwd_D_HL_NDF_KOF$comparison <- "(between)"
gwd_D_HL_NDM_KOM$comparison <- "(between)"
gwd_D_HL_NDF_NDF$comparison <- "(within)"
gwd_D_HL_NDM_NDM$comparison <- "(within)"
gwd_D_HL_NDM_NDF$comparison <- "(sex)"

gwd_V_FR_KOF_KOF$comparison <- "(within)"
gwd_V_FR_KOM_KOM$comparison <- "(within)"
gwd_V_FR_KOM_KOF$comparison <- "(sex)"
gwd_V_FR_NDF_KOF$comparison <- "(between)"
gwd_V_FR_NDM_KOM$comparison <- "(between)"
gwd_V_FR_NDF_NDF$comparison <- "(within)"
gwd_V_FR_NDM_NDM$comparison <- "(within)"
gwd_V_FR_NDM_NDF$comparison <- "(sex)"

gwd_V_FL_KOF_KOF$comparison <- "(within)"
gwd_V_FL_KOM_KOM$comparison <- "(within)"
gwd_V_FL_KOM_KOF$comparison <- "(sex)"
gwd_V_FL_NDF_KOF$comparison <- "(between)"
gwd_V_FL_NDM_KOM$comparison <- "(between)"
gwd_V_FL_NDF_NDF$comparison <- "(within)"
gwd_V_FL_NDM_NDM$comparison <- "(within)"
gwd_V_FL_NDM_NDF$comparison <- "(sex)"

gwd_V_HR_KOF_KOF$comparison <- "(within)"
gwd_V_HR_KOM_KOM$comparison <- "(within)"
gwd_V_HR_KOM_KOF$comparison <- "(sex)"
gwd_V_HR_NDF_KOF$comparison <- "(between)"
gwd_V_HR_NDM_KOM$comparison <- "(between)"
gwd_V_HR_NDF_NDF$comparison <- "(within)"
gwd_V_HR_NDM_NDM$comparison <- "(within)"
gwd_V_HR_NDM_NDF$comparison <- "(sex)"

gwd_V_HL_KOF_KOF$comparison <- "(within)"
gwd_V_HL_KOM_KOM$comparison <- "(within)"
gwd_V_HL_KOM_KOF$comparison <- "(sex)"
gwd_V_HL_NDF_KOF$comparison <- "(between)"
gwd_V_HL_NDM_KOM$comparison <- "(between)"
gwd_V_HL_NDF_NDF$comparison <- "(within)"
gwd_V_HL_NDM_NDM$comparison <- "(within)"
gwd_V_HL_NDM_NDF$comparison <- "(sex)"

gwd_D_FR <- rbind(gwd_D_FR_KOF_KOF, gwd_D_FR_KOM_KOM, gwd_D_FR_KOM_KOF, gwd_D_FR_NDF_KOF, 
                  gwd_D_FR_NDM_KOM, gwd_D_FR_NDF_NDF, gwd_D_FR_NDM_NDM, gwd_D_FR_NDM_NDF)
gwd_D_FL <- rbind(gwd_D_FL_KOF_KOF, gwd_D_FL_KOM_KOM, gwd_D_FL_KOM_KOF, gwd_D_FL_NDF_KOF, 
                  gwd_D_FL_NDM_KOM, gwd_D_FL_NDF_NDF, gwd_D_FL_NDM_NDM, gwd_D_FL_NDM_NDF)
gwd_D_HR <- rbind(gwd_D_HR_KOF_KOF, gwd_D_HR_KOM_KOM, gwd_D_HR_KOM_KOF, gwd_D_HR_NDF_KOF, 
                  gwd_D_HR_NDM_KOM, gwd_D_HR_NDF_NDF, gwd_D_HR_NDM_NDM, gwd_D_HR_NDM_NDF)
gwd_D_HL <- rbind(gwd_D_HL_KOF_KOF, gwd_D_HL_KOM_KOM, gwd_D_HL_KOM_KOF, gwd_D_HL_NDF_KOF, 
                  gwd_D_HL_NDM_KOM, gwd_D_HL_NDF_NDF, gwd_D_HL_NDM_NDM, gwd_D_HL_NDM_NDF)
gwd_V_FR <- rbind(gwd_V_FR_KOF_KOF, gwd_V_FR_KOM_KOM, gwd_V_FR_KOM_KOF, gwd_V_FR_NDF_KOF, 
                  gwd_V_FR_NDM_KOM, gwd_V_FR_NDF_NDF, gwd_V_FR_NDM_NDM, gwd_V_FR_NDM_NDF)
gwd_V_FL <- rbind(gwd_V_FL_KOF_KOF, gwd_V_FL_KOM_KOM, gwd_V_FL_KOM_KOF, gwd_V_FL_NDF_KOF, 
                  gwd_V_FL_NDM_KOM, gwd_V_FL_NDF_NDF, gwd_V_FL_NDM_NDM, gwd_V_FL_NDM_NDF)
gwd_V_HR <- rbind(gwd_V_HR_KOF_KOF, gwd_V_HR_KOM_KOM, gwd_V_HR_KOM_KOF, gwd_V_HR_NDF_KOF, 
                  gwd_V_HR_NDM_KOM, gwd_V_HR_NDF_NDF, gwd_V_HR_NDM_NDM, gwd_V_HR_NDM_NDF)
gwd_V_HL <- rbind(gwd_V_HL_KOF_KOF, gwd_V_HL_KOM_KOM, gwd_V_HL_KOM_KOF, gwd_V_HL_NDF_KOF, 
                  gwd_V_HL_NDM_KOM, gwd_V_HL_NDF_NDF, gwd_V_HL_NDM_NDM, gwd_V_HL_NDM_NDF)

gwd_D_FR$wing <- "Right forewing"
gwd_D_FL$wing <- "Left forewing"
gwd_D_HR$wing <- "Right hindwing"
gwd_D_HL$wing <- "Left hindwing"
gwd_V_FR$wing <- "Right forewing"
gwd_V_FL$wing <- "Left forewing"
gwd_V_HR$wing <- "Right hindwing"
gwd_V_HL$wing <- "Left hindwing"

gwd_D <- rbind(gwd_D_FR, gwd_D_FL, gwd_D_HR, gwd_D_HL)
gwd_V <- rbind(gwd_V_FR, gwd_V_FL, gwd_V_HR, gwd_V_HL)

gwd_D$side <- "Dorsal"
gwd_V$side <- "Ventral"

gwd_all <- rbind(gwd_D, gwd_V)
gwd_all <- gwd_all[apply(gwd_all!=0, 1, all),]

save(gwd_all, file = 'data/gwd_all.rda', row.names = F)
write.csv(gwd_all, file = 'data/data_gwd.csv', row.names = F)
load('data/gwd_all.rda')

### VISUALISATION #####
gwd_all$wing <- as.factor(gwd_all$wing)
gwd_all$wing <- factor(gwd_all$wing, levels = c("Left forewing", "Right forewing", 
                                                "Left hindwing", "Right hindwing"))

gwd_all$comparison <- as.factor(gwd_all$comparison)
gwd_all$comparison <- factor(gwd_all$comparison, levels = c("(between)", "(within)", "(sex)"))

gwd_all$side <- as.factor(gwd_all$side)
gwd_all$side <- factor(gwd_all$side, levels = c("Dorsal", "Ventral"))

gwd_all$pair <- as.factor(gwd_all$pair)
gwd_all$pair <- factor(gwd_all$pair, levels = c("male ND - male mKO", "female ND - female mKO", 
                                                "male mKO", "female mKO", "male ND", "female ND",  
                                                "mKO", "ND"))

pair.labs <- c("female ND - female mKO" = "\u2640 ND - \u2640 mKO", 
               "male ND - male mKO" = "\u2642 ND - \u2642 mKO", 
               "female ND" = "\u2640 ND", "male ND" = "\u2642 ND", 
               "female mKO" = "\u2640 mKO", "male mKO" = "\u2642 mKO", 
               "ND" = "ND", "mKO" = "mKO")

plot_gwd <- ggplot(gwd_all, aes(x = value, y = pair)) + 
  facet_grid(side + comparison ~ wing, scales = "free") + 
  stat_summary(aes(color = comparison, size = 2), 
               fun.min = function(z) {quantile(z,0.25)},
               fun.max = function(z) {quantile(z,0.75)},
               fun = median,
               geom = "pointrange", size = 0.6, position = position_dodge(0.5)) + 
  scale_color_manual(values = c("red", "grey30", "grey30")) + 
  scale_y_discrete(labels = (text = pair.labs)) + 
  xlab("\nGower's dissimilarity measure (mean \U00B1 CI)") + 
  ylab("") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        strip.background = element_blank(), 
        legend.position = "none")

plot_gwd

ggsave(filename = "Plots/plot_gwd1.png",
       width = 8, height = 6, device='png', dpi=1200)


plot_gwd <- ggplot(gwd_all, aes(x = value, y = pair)) + 
  facet_grid(side + comparison ~ wing, scales = "free") + 
  stat_summary(aes(color = comparison, size = 2), 
               fun.min = function(z) {quantile(z,0.25)},
               fun.max = function(z) {quantile(z,0.75)},
               fun = median,
               geom = "pointrange", size = 0.6, position = position_dodge(0.5)) + 
  scale_color_manual(values = c("red", "grey30", "grey30")) + 
  scale_y_discrete(labels = (text = pair.labs)) + 
  xlab("\nGower's dissimilarity measure (mean \U00B1 CI)") + 
  ylab("") + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none")

plot_gwd

ggsave(filename = "Plots/plot_gwd2.png",
       width = 8, height = 6, device='png', dpi=1200)


#library(extrafont)
#font_import()
#loadfonts(device = "win")

plot_gwd <- ggplot(gwd_all, aes(x = pair, y = value)) + 
  facet_grid(wing ~ side + comparison, scales = "free") + 
  stat_summary(aes(color = comparison, size = 2), 
               fun.min = function(z) {quantile(z,0.25)},
               fun.max = function(z) {quantile(z,0.75)},
               fun = median,
               geom = "pointrange", size = 0.6, position = position_dodge(0.5)) + 
  scale_color_manual(values = c("red", "gray39", "gray39")) + 
  scale_x_discrete(labels = (text = pair.labs)) + 
  ylab("Gower's dissimilarity measure (mean \U00B1 CI)\n") + 
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5), 
        legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "gray39"))

plot_gwd

ggsave(filename = "Plots/plot_gwd3.png",
       width = 6, height = 8, device='png', dpi=1200)
