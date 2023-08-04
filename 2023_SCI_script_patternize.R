### INTRODUCTION #####

# Welcome. For the description of the project please visit: https://github.com/diekei/2023_ENS_diekei_host_shift
# Article is available at: https://www.biorxiv.org/content/10.1101/2023.07.12.548653v1


### LIBRARY #####

library(patternize)
library(viridis)
library(factoextra)
library(adegenet)
library(plyr)
library(ggplot2)
# attention! please run the code one by one, step by step


### GENERATING DATA - COMMON CODES #####

# for setting the common 'target' sample
target_D_FR <- as.matrix(read.table('landmarks/KO-M/Dorsal/Forewing/KO-M-D-6H-FR.txt', h = F))
target_D_FL <- as.matrix(read.table('landmarks/KO-M/Dorsal/Forewing/KO-M-D-6H-FL.txt', h = F))
target_D_HR <- as.matrix(read.table('landmarks/KO-M/Dorsal/Hindwing/KO-M-D-6H-HR.txt', h = F))
target_D_HL <- as.matrix(read.table('landmarks/KO-M/Dorsal/Hindwing/KO-M-D-6H-HL.txt', h = F))
target_V_FR <- as.matrix(read.table('landmarks/KO-M/Ventral/Forewing/KO-M-V-6H-FR.txt', h = F))
target_V_FL <- as.matrix(read.table('landmarks/KO-M/Ventral/Forewing/KO-M-V-6H-FL.txt', h = F))
target_V_HR <- as.matrix(read.table('landmarks/KO-M/Ventral/Hindwing/KO-M-V-6H-HR.txt', h = F))
target_V_HL <- as.matrix(read.table('landmarks/KO-M/Ventral/Hindwing/KO-M-V-6H-HL.txt', h = F))

# for setting the common wing outline
outline_D_FR <- read.table('landmarks/KO-M-D-6H-FR-outline.txt', h = F)
outline_D_FL <- read.table('landmarks/KO-M-D-6H-FL-outline.txt', h = F)
outline_D_HR <- read.table('landmarks/KO-M-D-6H-HR-outline.txt', h = F)
outline_D_HL <- read.table('landmarks/KO-M-D-6H-HL-outline.txt', h = F)
outline_V_FR <- read.table('landmarks/KO-M-V-6H-FR-outline.txt', h = F)
outline_V_FL <- read.table('landmarks/KO-M-V-6H-FL-outline.txt', h = F)
outline_V_HR <- read.table('landmarks/KO-M-V-6H-HR-outline.txt', h = F)
outline_V_HL <- read.table('landmarks/KO-M-V-6H-HL-outline.txt', h = F)

# for setting the common wing veins
lines_D_FR <- list.files(path = 'landmarks/', pattern = 'KO-M-D-6H-FR-lines', full.names = T)
lines_D_FL <- list.files(path = 'landmarks/', pattern = 'KO-M-D-6H-FL-lines', full.names = T)
lines_D_HR <- list.files(path = 'landmarks/', pattern = 'KO-M-D-6H-HR-lines', full.names = T)
lines_D_HL <- list.files(path = 'landmarks/', pattern = 'KO-M-D-6H-HL-lines', full.names = T)
lines_V_FR <- list.files(path = 'landmarks/', pattern = 'KO-M-V-6H-FR-lines', full.names = T)
lines_V_FL <- list.files(path = 'landmarks/', pattern = 'KO-M-V-6H-FL-lines', full.names = T)
lines_V_HR <- list.files(path = 'landmarks/', pattern = 'KO-M-V-6H-HR-lines', full.names = T)
lines_V_HL <- list.files(path = 'landmarks/', pattern = 'KO-M-V-6H-HL-lines', full.names = T)

# for setting the common mask
#mask_D_FR <- no extra mask needed
#mask_D_HL <- no extra mask needed
mask_D_HR <- read.table('landmarks/KO-M-D-6H-HR-mask.txt', h = F)
mask_D_HL <- read.table('landmarks/KO-M-D-6H-HL-mask.txt', h = F)
mask_V_FR <- read.table('landmarks/KO-M-V-6H-FR-mask.txt', h = F)
mask_V_FL <- read.table('landmarks/KO-M-V-6H-FL-mask.txt', h = F)
mask_V_HR <- read.table('landmarks/KO-M-V-6H-HR-mask.txt', h = F)
mask_V_HL <- read.table('landmarks/KO-M-V-6H-HL-mask.txt', h = F)

# for sampling RGB color target (example)
ID_target <- c('KO-M-V-6H')
prepath <- 'images/KO-F/Ventral/Forewing/'
extension <- '-FR.jpg'
RGB_target <- makeList(ID_target, 'image', prepath, extension)
RGB_target <- RGB_target[['KO-M-V-6H']]
RGB <- sampleRGB(RGB_target, resampleFactor = NULL, crop = c(0,0,0,0))


### GENERATING DATA - KO F #####

# indicating sample id
ID_KO_F_D <- c('KO-F-D-10N', 
               'KO-F-D-10R', 
               'KO-F-D-11P', 
               'KO-F-D-13N', 
               'KO-F-D-14J', 
               'KO-F-D-14N', 
               'KO-F-D-15K', 
               'KO-F-D-17N', 
               'KO-F-D-18P', 
               'KO-F-D-20213G', 
               'KO-F-D-20214E', 
               'KO-F-D-20219E', 
               'KO-F-D-2J', 
               'KO-F-D-2L', 
               'KO-F-D-2N', 
               'KO-F-D-31J', 
               'KO-F-D-4H', 
               'KO-F-D-4J', 
               'KO-F-D-6C', 
               'KO-F-D-6N', 
               'KO-F-D-7A', 
               'KO-F-D-7J', 
               'KO-F-D-7R', 
               'KO-F-D-8N', 
               'KO-F-D-9N', 
               'KO-F-D-9P')

ID_KO_F_V <- c('KO-F-V-10N', 
               'KO-F-V-10R', 
               'KO-F-V-11P', 
               'KO-F-V-13N', 
               'KO-F-V-14J', 
               'KO-F-V-14N', 
               'KO-F-V-15K', 
               'KO-F-V-17N', 
               'KO-F-V-18P', 
               'KO-F-V-20213G', 
               'KO-F-V-20214E', 
               'KO-F-V-20219E', 
               'KO-F-V-2J', 
               'KO-F-V-2L', 
               'KO-F-V-2N', 
               'KO-F-V-31J', 
               'KO-F-V-4H', 
               'KO-F-V-4J', 
               'KO-F-V-6C', 
               'KO-F-V-6N', 
               'KO-F-V-7A', 
               'KO-F-V-7J', 
               'KO-F-V-7R', 
               'KO-F-V-8N', 
               'KO-F-V-9N', 
               'KO-F-V-9P')

# indicating sample images and landmarks
prepath <- 'images/KO-F/Dorsal/Forewing/'
extension <- '-FR.jpg'
imageList_KO_F_D_FR <- makeList(ID_KO_F_D, 'image', prepath, extension)

prepath <- 'images/KO-F/Dorsal/Forewing/'
extension <- '-FL.jpg'
imageList_KO_F_D_FL <- makeList(ID_KO_F_D, 'image', prepath, extension)

prepath <- 'images/KO-F/Dorsal/Hindwing/'
extension <- '-HR.jpg'
imageList_KO_F_D_HR <- makeList(ID_KO_F_D, 'image', prepath, extension)

prepath <- 'images/KO-F/Dorsal/Hindwing/'
extension <- '-HL.jpg'
imageList_KO_F_D_HL <- makeList(ID_KO_F_D, 'image', prepath, extension)

prepath <- 'images/KO-F/Ventral/Forewing/'
extension <- '-FR.jpg'
imageList_KO_F_V_FR <- makeList(ID_KO_F_V, 'image', prepath, extension)

prepath <- 'images/KO-F/Ventral/Forewing/'
extension <- '-FL.jpg'
imageList_KO_F_V_FL <- makeList(ID_KO_F_V, 'image', prepath, extension)

prepath <- 'images/KO-F/Ventral/Hindwing/'
extension <- '-HR.jpg'
imageList_KO_F_V_HR <- makeList(ID_KO_F_V, 'image', prepath, extension)

prepath <- 'images/KO-F/Ventral/Hindwing/'
extension <- '-HL.jpg'
imageList_KO_F_V_HL <- makeList(ID_KO_F_V, 'image', prepath, extension)


prepath <- 'landmarks/KO-F/Dorsal/Forewing/'
extension <- '-FR.txt'
lanList_KO_F_D_FR <- makeList(ID_KO_F_D, 'landmark', prepath, extension)

prepath <- 'landmarks/KO-F/Dorsal/Forewing/'
extension <- '-FL.txt'
lanList_KO_F_D_FL <- makeList(ID_KO_F_D, 'landmark', prepath, extension)

prepath <- 'landmarks/KO-F/Dorsal/Hindwing/'
extension <- '-HR.txt'
lanList_KO_F_D_HR <- makeList(ID_KO_F_D, 'landmark', prepath, extension)

prepath <- 'landmarks/KO-F/Dorsal/Hindwing/'
extension <- '-HL.txt'
lanList_KO_F_D_HL <- makeList(ID_KO_F_D, 'landmark', prepath, extension)

prepath <- 'landmarks/KO-F/Ventral/Forewing/'
extension <- '-FR.txt'
lanList_KO_F_V_FR <- makeList(ID_KO_F_V, 'landmark', prepath, extension)

prepath <- 'landmarks/KO-F/Ventral/Forewing/'
extension <- '-FL.txt'
lanList_KO_F_V_FL <- makeList(ID_KO_F_V, 'landmark', prepath, extension)

prepath <- 'landmarks/KO-F/Ventral/Hindwing/'
extension <- '-HR.txt'
lanList_KO_F_V_HR <- makeList(ID_KO_F_V, 'landmark', prepath, extension)

prepath <- 'landmarks/KO-F/Ventral/Hindwing/'
extension <- '-HL.txt'
lanList_KO_F_V_HL <- makeList(ID_KO_F_V, 'landmark', prepath, extension)


# extracting patterns
RGB_FWD <- c(237,51,39)
raster_KO_F_D_FR <- patLanRGB(imageList_KO_F_D_FR, lanList_KO_F_D_FR, RGB_FWD,
                              transformRef = target_D_FR, resampleFactor = 3,
                              colOffset= 0.3, res = 300, 
                              adjustCoords = TRUE, plot = 'stack')

RGB_FWD <- c(237,51,39)
raster_KO_F_D_FL <- patLanRGB(imageList_KO_F_D_FL, lanList_KO_F_D_FL, RGB_FWD,
                              transformRef = target_D_FL, resampleFactor = 3,
                              colOffset= 0.3, res = 300, 
                              adjustCoords = TRUE, plot = 'stack')

RGB_HWD <- c(254,253,127)
raster_KO_F_D_HR <- patLanRGB(imageList_KO_F_D_HR, lanList_KO_F_D_HR, RGB_HWD,
                              transformRef = target_D_HR, resampleFactor = 3,
                              colOffset= 0.3, res = 300, 
                              adjustCoords = TRUE, plot = 'stack')

RGB_HWD <- c(254,253,127)
raster_KO_F_D_HL <- patLanRGB(imageList_KO_F_D_HL, lanList_KO_F_D_HL, RGB_HWD,
                              transformRef = target_D_HL, resampleFactor = 3,
                              colOffset= 0.3, res = 300, 
                              adjustCoords = TRUE, plot = 'stack')

#RGB_FWV <- c(252,114,112)
RGB_FWV <- c(252,163,110)
raster_KO_F_V_FR <- patLanRGB(imageList_KO_F_V_FR, lanList_KO_F_V_FR, RGB_FWV,
                              transformRef = target_V_FR, resampleFactor = 3,
                              colOffset= 0.25, res = 300, 
                              adjustCoords = TRUE, plot = 'stack')

RGB_FWV <- c(252,163,110)
raster_KO_F_V_FL <- patLanRGB(imageList_KO_F_V_FL, lanList_KO_F_V_FL, RGB_FWV,
                              transformRef = target_V_FL, resampleFactor = 3,
                              colOffset= 0.25, res = 300, 
                              adjustCoords = TRUE, plot = 'stack')

RGB_HWV <- c(254,253,127)
raster_KO_F_V_HR <- patLanRGB(imageList_KO_F_V_HR, lanList_KO_F_V_HR, RGB_HWV,
                              transformRef = target_V_HR, resampleFactor = 3,
                              colOffset= 0.3, res = 300, 
                              adjustCoords = TRUE, plot = 'stack')

RGB_HWV <- c(254,253,127)
raster_KO_F_V_HL <- patLanRGB(imageList_KO_F_V_HL, lanList_KO_F_V_HL, RGB_HWV,
                              transformRef = target_V_HL, resampleFactor = 3,
                              colOffset= 0.3, res = 300, 
                              adjustCoords = TRUE, plot = 'stack')


save(raster_KO_F_D_FR, file = 'data/raster_KO_F_D_FR.rda')
save(raster_KO_F_D_FL, file = 'data/raster_KO_F_D_FL.rda')
save(raster_KO_F_D_HR, file = 'data/raster_KO_F_D_HR.rda')
save(raster_KO_F_D_HL, file = 'data/raster_KO_F_D_HL.rda')
save(raster_KO_F_V_FR, file = 'data/raster_KO_F_V_FR.rda')
save(raster_KO_F_V_FL, file = 'data/raster_KO_F_V_FL.rda')
save(raster_KO_F_V_HR, file = 'data/raster_KO_F_V_HR.rda')
save(raster_KO_F_V_HL, file = 'data/raster_KO_F_V_HL.rda')

load('data/raster_KO_F_D_FR.rda')
load('data/raster_KO_F_D_FL.rda')
load('data/raster_KO_F_D_HR.rda')
load('data/raster_KO_F_D_HL.rda')
load('data/raster_KO_F_V_FR.rda')
load('data/raster_KO_F_V_FL.rda')
load('data/raster_KO_F_V_HR.rda')
load('data/raster_KO_F_V_HL.rda')


# masking/filtering extracted patterns part 1 (with outline)
for(e in 1:length(raster_KO_F_D_FR)){
  ID <- names(raster_KO_F_D_FR)[[e]]
  raster_KO_F_D_FR[[ID]] <- maskOutline(raster_KO_F_D_FR[[ID]], IDlist = ID_KO_F_D, outline_D_FR, 
                                       refShape = 'target', imageList = imageList_KO_F_D_FR)
}

for(e in 1:length(raster_KO_F_D_FL)){
  ID <- names(raster_KO_F_D_FL)[[e]]
  raster_KO_F_D_FL[[ID]] <- maskOutline(raster_KO_F_D_FL[[ID]], IDlist = ID_KO_F_D, outline_D_FL, 
                                        refShape = 'target', imageList = imageList_KO_F_D_FL)
}

for(e in 1:length(raster_KO_F_D_HR)){
  ID <- names(raster_KO_F_D_HR)[[e]]
  raster_KO_F_D_HR[[ID]] <- maskOutline(raster_KO_F_D_HR[[ID]], IDlist = ID_KO_F_D, outline_D_HR, 
                                        refShape = 'target', imageList = imageList_KO_F_D_HR)
}

for(e in 1:length(raster_KO_F_D_HL)){
  ID <- names(raster_KO_F_D_HL)[[e]]
  raster_KO_F_D_HL[[ID]] <- maskOutline(raster_KO_F_D_HL[[ID]], IDlist = ID_KO_F_D, outline_D_HL, 
                                        refShape = 'target', imageList = imageList_KO_F_D_HL)
}

for(e in 1:length(raster_KO_F_V_FR)){
  ID <- names(raster_KO_F_V_FR)[[e]]
  raster_KO_F_V_FR[[ID]] <- maskOutline(raster_KO_F_V_FR[[ID]], IDlist = ID_KO_F_V, outline_V_FR, 
                                        refShape = 'target', imageList = imageList_KO_F_V_FR)
}

for(e in 1:length(raster_KO_F_V_FL)){
  ID <- names(raster_KO_F_V_FL)[[e]]
  raster_KO_F_V_FL[[ID]] <- maskOutline(raster_KO_F_V_FL[[ID]], IDlist = ID_KO_F_V, outline_V_FL, 
                                        refShape = 'target', imageList = imageList_KO_F_V_FL)
}

for(e in 1:length(raster_KO_F_V_HR)){
  ID <- names(raster_KO_F_V_HR)[[e]]
  raster_KO_F_V_HR[[ID]] <- maskOutline(raster_KO_F_V_HR[[ID]], IDlist = ID_KO_F_V, outline_V_HR, 
                                        refShape = 'target', imageList = imageList_KO_F_V_HR)
}

for(e in 1:length(raster_KO_F_V_HL)){
  ID <- names(raster_KO_F_V_HL)[[e]]
  raster_KO_F_V_HL[[ID]] <- maskOutline(raster_KO_F_V_HL[[ID]], IDlist = ID_KO_F_V, outline_V_HL, 
                                        refShape = 'target', imageList = imageList_KO_F_V_HL)
}

# masking/filtering extracted patterns part 2 (with mask)
for(e in 1:length(raster_KO_F_D_HR)){
  ID <- names(raster_KO_F_D_HR)[[e]]
  raster_KO_F_D_HR[[ID]] <- maskOutline(raster_KO_F_D_HR[[ID]], IDlist = ID_KO_F_D, mask_D_HR, 
                                        refShape = 'target', imageList = imageList_KO_F_D_HR)
}

for(e in 1:length(raster_KO_F_D_HL)){
  ID <- names(raster_KO_F_D_HL)[[e]]
  raster_KO_F_D_HL[[ID]] <- maskOutline(raster_KO_F_D_HL[[ID]], IDlist = ID_KO_F_D, mask_D_HL, 
                                        refShape = 'target', imageList = imageList_KO_F_D_HL)
}

for(e in 1:length(raster_KO_F_V_FR)){
  ID <- names(raster_KO_F_V_FR)[[e]]
  raster_KO_F_V_FR[[ID]] <- maskOutline(raster_KO_F_V_FR[[ID]], IDlist = ID_KO_F_V, mask_V_FR, 
                                        refShape = 'target', imageList = imageList_KO_F_V_FR)
}

for(e in 1:length(raster_KO_F_V_FL)){
  ID <- names(raster_KO_F_V_FL)[[e]]
  raster_KO_F_V_FL[[ID]] <- maskOutline(raster_KO_F_V_FL[[ID]], IDlist = ID_KO_F_V, mask_V_FL, 
                                        refShape = 'target', imageList = imageList_KO_F_V_FL)
}

for(e in 1:length(raster_KO_F_V_HR)){
  ID <- names(raster_KO_F_V_HR)[[e]]
  raster_KO_F_V_HR[[ID]] <- maskOutline(raster_KO_F_V_HR[[ID]], IDlist = ID_KO_F_V, mask_V_HR, 
                                        refShape = 'target', imageList = imageList_KO_F_V_HR)
}

for(e in 1:length(raster_KO_F_V_HL)){
  ID <- names(raster_KO_F_V_HL)[[e]]
  raster_KO_F_V_HL[[ID]] <- maskOutline(raster_KO_F_V_HL[[ID]], IDlist = ID_KO_F_V, mask_V_HL, 
                                        refShape = 'target', imageList = imageList_KO_F_V_HL)
}


# plotting patterns
sumraster_KO_F_D_FR <- sumRaster(raster_KO_F_D_FR, ID_KO_F_D, type = 'RGB')
colfunc <- hsv(1, 1, seq(0,1,length.out = 100))
png(file = "Plots/plot_heatmap_KO_F_D_FR.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sumraster_KO_F_D_FR, ID_KO_F_D, 
         imageList = imageList_KO_F_D_FR, landList = lanList_KO_F_D_FR, 
         refShape = 'target', crop = c(0,0,0,0),
         flipRaster = 'y', flipOutline = 'y',
         outline = outline_D_FR, colpalette = colfunc, 
         plotCartoon = TRUE,  cartoonID = 'KO-M-D-6H', refImage = imageList_KO_M_D_FR[['KO-M-D-6H']],
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "proportion of individuals with colour", lines = lines_D_FR)
#dev.copy(png, file = "Plots/plot_heatmap_KO_F_D_FR.png",
#         width = 10, height = 6, units = "in", res = 1200)
dev.off()

sumraster_KO_F_D_FL <- sumRaster(raster_KO_F_D_FL, ID_KO_F_D, type = 'RGB')
colfunc <- hsv(1, 1, seq(0,1,length.out = 100))
png(file = "Plots/plot_heatmap_KO_F_D_FL.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sumraster_KO_F_D_FL, ID_KO_F_D, 
         imageList = imageList_KO_F_D_FL, landList = lanList_KO_F_D_FL, 
         refShape = 'target', crop = c(0,0,0,0),
         flipRaster = 'y', flipOutline = 'y',
         outline = outline_D_FL, colpalette = colfunc, 
         plotCartoon = TRUE,  cartoonID = 'KO-M-D-6H', refImage = imageList_KO_M_D_FL[['KO-M-D-6H']],
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "proportion of individuals with colour", lines = lines_D_FL)
#dev.copy(png, file = "Plots/plot_heatmap_KO_F_D_FL.png",
#         width = 10, height = 6, units = "in", res = 1200)
dev.off()

sumraster_KO_F_D_HR <- sumRaster(raster_KO_F_D_HR, ID_KO_F_D, type = 'RGB')
colfunc <- c('black', 'grey', 'khaki1')
png(file = "Plots/plot_heatmap_KO_F_D_HR.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sumraster_KO_F_D_HR, ID_KO_F_D, 
         imageList = imageList_KO_F_D_HR, landList = lanList_KO_F_D_HR, 
         refShape = 'target', crop = c(0,0,0,0),
         flipRaster = 'y', flipOutline = 'y',
         outline = outline_D_HR, colpalette = colfunc, 
         plotCartoon = TRUE,  cartoonID = 'KO-M-D-6H', refImage = imageList_KO_M_D_HR[['KO-M-D-6H']],
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "proportion of individuals with colour", lines = lines_D_HR)
#dev.copy(png, file = "Plots/plot_heatmap_KO_F_D_HR.png",
#         width = 10, height = 6, units = "in", res = 1200)
dev.off()

sumraster_KO_F_D_HL <- sumRaster(raster_KO_F_D_HL, ID_KO_F_D, type = 'RGB')
colfunc <- c('black', 'grey', 'khaki1')
png(file = "Plots/plot_heatmap_KO_F_D_HL.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sumraster_KO_F_D_HL, ID_KO_F_D, 
         imageList = imageList_KO_F_D_HL, landList = lanList_KO_F_D_HL, 
         refShape = 'target', crop = c(0,0,0,0),
         flipRaster = 'y', flipOutline = 'y',
         outline = outline_D_HL, colpalette = colfunc, 
         plotCartoon = TRUE,  cartoonID = 'KO-M-D-6H', refImage = imageList_KO_M_D_HL[['KO-M-D-6H']],
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "proportion of individuals with colour", lines = lines_D_HL)
#dev.copy(png, file = "Plots/plot_heatmap_KO_F_D_HL.png",
#         width = 10, height = 6, units = "in", res = 1200)
dev.off()

sumraster_KO_F_V_FR <- sumRaster(raster_KO_F_V_FR, ID_KO_F_V, type = 'RGB')
colfunc <- hsv(1, 1, seq(0,1,length.out = 100))
png(file = "Plots/plot_heatmap_KO_F_V_FR.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sumraster_KO_F_V_FR, ID_KO_F_V, 
         imageList = imageList_KO_F_V_FR, landList = lanList_KO_F_V_FR, 
         refShape = 'target', crop = c(0,0,0,0),
         flipRaster = 'y', flipOutline = 'y',
         outline = outline_V_FR, colpalette = colfunc, 
         plotCartoon = TRUE,  cartoonID = 'KO-M-V-6H', refImage = imageList_KO_M_V_FR[['KO-M-V-6H']],
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "proportion of individuals with colour", lines = lines_V_FR)
#dev.copy(png, file = "Plots/plot_heatmap_KO_F_V_FR.png",
#         width = 10, height = 6, units = "in", res = 1200)
dev.off()

sumraster_KO_F_V_FL <- sumRaster(raster_KO_F_V_FL, ID_KO_F_V, type = 'RGB')
colfunc <- hsv(1, 1, seq(0,1,length.out = 100))
png(file = "Plots/plot_heatmap_KO_F_V_FL.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sumraster_KO_F_V_FL, ID_KO_F_V, 
         imageList = imageList_KO_F_V_FL, landList = lanList_KO_F_V_FL, 
         refShape = 'target', crop = c(0,0,0,0),
         flipRaster = 'y', flipOutline = 'y',
         outline = outline_V_FL, colpalette = colfunc, 
         plotCartoon = TRUE,  cartoonID = 'KO-M-V-6H', refImage = imageList_KO_M_V_FL[['KO-M-V-6H']],
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "proportion of individuals with colour", lines = lines_V_FL)
#dev.copy(png, file = "Plots/plot_heatmap_KO_F_V_FL.png",
#         width = 10, height = 6, units = "in", res = 1200)
dev.off()

sumraster_KO_F_V_HR <- sumRaster(raster_KO_F_V_HR, ID_KO_F_V, type = 'RGB')
colfunc <- c('black', 'grey', 'khaki1')
png(file = "Plots/plot_heatmap_KO_F_V_HR.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sumraster_KO_F_V_HR, ID_KO_F_V, 
         imageList = imageList_KO_F_V_HR, landList = lanList_KO_F_V_HR, 
         refShape = 'target', crop = c(0,0,0,0),
         flipRaster = 'y', flipOutline = 'y',
         outline = outline_V_HR, colpalette = colfunc, 
         plotCartoon = TRUE,  cartoonID = 'KO-M-V-6H', refImage = imageList_KO_M_V_HR[['KO-M-V-6H']],
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "proportion of individuals with colour", lines = lines_V_HR)
#dev.copy(png, file = "Plots/plot_heatmap_KO_F_V_HR.png",
#         width = 10, height = 6, units = "in", res = 1200)
dev.off()

sumraster_KO_F_V_HL <- sumRaster(raster_KO_F_V_HL, ID_KO_F_V, type = 'RGB')
colfunc <- c('black', 'grey', 'khaki1')
png(file = "Plots/plot_heatmap_KO_F_V_HL.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sumraster_KO_F_V_HL, ID_KO_F_V, 
         imageList = imageList_KO_F_V_HL, landList = lanList_KO_F_V_HL, 
         refShape = 'target', crop = c(0,0,0,0),
         flipRaster = 'y', flipOutline = 'y',
         outline = outline_V_HL, colpalette = colfunc, 
         plotCartoon = TRUE,  cartoonID = 'KO-M-V-6H', refImage = imageList_KO_M_V_HL[['KO-M-V-6H']],
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "proportion of individuals with colour", lines = lines_V_HL)
#dev.copy(png, file = "Plots/plot_heatmap_KO_F_V_HL.png",
#         width = 10, height = 6, units = "in", res = 1200)
dev.off()


### GENERATING DATA - KO M #####

# indicating sample id
ID_KO_M_D <- c('KO-M-D-10M', 
               'KO-M-D-11A', 
               'KO-M-D-13F', 
               'KO-M-D-13K', 
               'KO-M-D-16J', 
               'KO-M-D-18N', 
               'KO-M-D-20219F', 
               'KO-M-D-21P', 
               'KO-M-D-2H', 
               'KO-M-D-2V', 
               'KO-M-D-3A', 
               'KO-M-D-4A', 
               'KO-M-D-5H', 
               'KO-M-D-5M', 
               'KO-M-D-6H', 
               'KO-M-D-6Q', 
               'KO-M-D-7H', 
               'KO-M-D-8J', 
               'KO-M-D-8P', 
               'KO-M-D-8R')

ID_KO_M_V <- c('KO-M-V-10M', 
               'KO-M-V-11A', 
               'KO-M-V-13F', 
               'KO-M-V-13K', 
               'KO-M-V-16J', 
               'KO-M-V-18N', 
               'KO-M-V-20219F', 
               'KO-M-V-21P', 
               'KO-M-V-2H', 
               'KO-M-V-2V', 
               'KO-M-V-3A', 
               'KO-M-V-4A', 
               'KO-M-V-5H', 
               'KO-M-V-5M', 
               'KO-M-V-6H', 
               'KO-M-V-6Q', 
               'KO-M-V-7H', 
               'KO-M-V-8J', 
               'KO-M-V-8P', 
               'KO-M-V-8R')

# indicating sample images and landmarks
prepath <- 'images/KO-M/Dorsal/Forewing/'
extension <- '-FR.jpg'
imageList_KO_M_D_FR <- makeList(ID_KO_M_D, 'image', prepath, extension)

prepath <- 'images/KO-M/Dorsal/Forewing/'
extension <- '-FL.jpg'
imageList_KO_M_D_FL <- makeList(ID_KO_M_D, 'image', prepath, extension)

prepath <- 'images/KO-M/Dorsal/Hindwing/'
extension <- '-HR.jpg'
imageList_KO_M_D_HR <- makeList(ID_KO_M_D, 'image', prepath, extension)

prepath <- 'images/KO-M/Dorsal/Hindwing/'
extension <- '-HL.jpg'
imageList_KO_M_D_HL <- makeList(ID_KO_M_D, 'image', prepath, extension)

prepath <- 'images/KO-M/Ventral/Forewing/'
extension <- '-FR.jpg'
imageList_KO_M_V_FR <- makeList(ID_KO_M_V, 'image', prepath, extension)

prepath <- 'images/KO-M/Ventral/Forewing/'
extension <- '-FL.jpg'
imageList_KO_M_V_FL <- makeList(ID_KO_M_V, 'image', prepath, extension)

prepath <- 'images/KO-M/Ventral/Hindwing/'
extension <- '-HR.jpg'
imageList_KO_M_V_HR <- makeList(ID_KO_M_V, 'image', prepath, extension)

prepath <- 'images/KO-M/Ventral/Hindwing/'
extension <- '-HL.jpg'
imageList_KO_M_V_HL <- makeList(ID_KO_M_V, 'image', prepath, extension)


prepath <- 'landmarks/KO-M/Dorsal/Forewing/'
extension <- '-FR.txt'
lanList_KO_M_D_FR <- makeList(ID_KO_M_D, 'landmark', prepath, extension)

prepath <- 'landmarks/KO-M/Dorsal/Forewing/'
extension <- '-FL.txt'
lanList_KO_M_D_FL <- makeList(ID_KO_M_D, 'landmark', prepath, extension)

prepath <- 'landmarks/KO-M/Dorsal/Hindwing/'
extension <- '-HR.txt'
lanList_KO_M_D_HR <- makeList(ID_KO_M_D, 'landmark', prepath, extension)

prepath <- 'landmarks/KO-M/Dorsal/Hindwing/'
extension <- '-HL.txt'
lanList_KO_M_D_HL <- makeList(ID_KO_M_D, 'landmark', prepath, extension)

prepath <- 'landmarks/KO-M/Ventral/Forewing/'
extension <- '-FR.txt'
lanList_KO_M_V_FR <- makeList(ID_KO_M_V, 'landmark', prepath, extension)

prepath <- 'landmarks/KO-M/Ventral/Forewing/'
extension <- '-FL.txt'
lanList_KO_M_V_FL <- makeList(ID_KO_M_V, 'landmark', prepath, extension)

prepath <- 'landmarks/KO-M/Ventral/Hindwing/'
extension <- '-HR.txt'
lanList_KO_M_V_HR <- makeList(ID_KO_M_V, 'landmark', prepath, extension)

prepath <- 'landmarks/KO-M/Ventral/Hindwing/'
extension <- '-HL.txt'
lanList_KO_M_V_HL <- makeList(ID_KO_M_V, 'landmark', prepath, extension)


# extracting patterns
RGB_FWD <- c(237,51,39)
raster_KO_M_D_FR <- patLanRGB(imageList_KO_M_D_FR, lanList_KO_M_D_FR, RGB_FWD,
                              transformRef = target_D_FR, resampleFactor = 3,
                              colOffset= 0.3, res = 300, 
                              adjustCoords = TRUE, plot = 'stack')

RGB_FWD <- c(237,51,39)
raster_KO_M_D_FL <- patLanRGB(imageList_KO_M_D_FL, lanList_KO_M_D_FL, RGB_FWD,
                              transformRef = target_D_FL, resampleFactor = 3,
                              colOffset= 0.3, res = 300, 
                              adjustCoords = TRUE, plot = 'stack')

RGB_HWD <- c(254,253,127)
raster_KO_M_D_HR <- patLanRGB(imageList_KO_M_D_HR, lanList_KO_M_D_HR, RGB_HWD,
                              transformRef = target_D_HR, resampleFactor = 3,
                              colOffset= 0.3, res = 300, 
                              adjustCoords = TRUE, plot = 'stack')

RGB_HWD <- c(254,253,127)
raster_KO_M_D_HL <- patLanRGB(imageList_KO_M_D_HL, lanList_KO_M_D_HL, RGB_HWD,
                              transformRef = target_D_HL, resampleFactor = 3,
                              colOffset= 0.3, res = 300, 
                              adjustCoords = TRUE, plot = 'stack')

#RGB_FWV <- c(252,114,112)
RGB_FWV <- c(252,163,110)
raster_KO_M_V_FR <- patLanRGB(imageList_KO_M_V_FR, lanList_KO_M_V_FR, RGB_FWV,
                              transformRef = target_V_FR, resampleFactor = 3,
                              colOffset= 0.25, res = 300, 
                              adjustCoords = TRUE, plot = 'stack')

RGB_FWV <- c(252,163,110)
raster_KO_M_V_FL <- patLanRGB(imageList_KO_M_V_FL, lanList_KO_M_V_FL, RGB_FWV,
                              transformRef = target_V_FL, resampleFactor = 3,
                              colOffset= 0.25, res = 300, 
                              adjustCoords = TRUE, plot = 'stack')

RGB_HWV <- c(254,253,127)
raster_KO_M_V_HR <- patLanRGB(imageList_KO_M_V_HR, lanList_KO_M_V_HR, RGB_HWV,
                              transformRef = target_V_HR, resampleFactor = 3,
                              colOffset= 0.3, res = 300, 
                              adjustCoords = TRUE, plot = 'stack')

RGB_HWV <- c(254,253,127)
raster_KO_M_V_HL <- patLanRGB(imageList_KO_M_V_HL, lanList_KO_M_V_HL, RGB_HWV,
                              transformRef = target_V_HL, resampleFactor = 3,
                              colOffset= 0.3, res = 300, 
                              adjustCoords = TRUE, plot = 'stack')


save(raster_KO_M_D_FR, file = 'data/raster_KO_M_D_FR.rda')
save(raster_KO_M_D_FL, file = 'data/raster_KO_M_D_FL.rda')
save(raster_KO_M_D_HR, file = 'data/raster_KO_M_D_HR.rda')
save(raster_KO_M_D_HL, file = 'data/raster_KO_M_D_HL.rda')
save(raster_KO_M_V_FR, file = 'data/raster_KO_M_V_FR.rda')
save(raster_KO_M_V_FL, file = 'data/raster_KO_M_V_FL.rda')
save(raster_KO_M_V_HR, file = 'data/raster_KO_M_V_HR.rda')
save(raster_KO_M_V_HL, file = 'data/raster_KO_M_V_HL.rda')

load('data/raster_KO_M_D_FR.rda')
load('data/raster_KO_M_D_FL.rda')
load('data/raster_KO_M_D_HR.rda')
load('data/raster_KO_M_D_HL.rda')
load('data/raster_KO_M_V_FR.rda')
load('data/raster_KO_M_V_FL.rda')
load('data/raster_KO_M_V_HR.rda')
load('data/raster_KO_M_V_HL.rda')


# masking/filtering extracted patterns part 1 (with outline)
for(e in 1:length(raster_KO_M_D_FR)){
  ID <- names(raster_KO_M_D_FR)[[e]]
  raster_KO_M_D_FR[[ID]] <- maskOutline(raster_KO_M_D_FR[[ID]], IDlist = ID_KO_M_D, outline_D_FR, 
                                        refShape = 'target', imageList = imageList_KO_M_D_FR)
}

for(e in 1:length(raster_KO_M_D_FL)){
  ID <- names(raster_KO_M_D_FL)[[e]]
  raster_KO_M_D_FL[[ID]] <- maskOutline(raster_KO_M_D_FL[[ID]], IDlist = ID_KO_M_D, outline_D_FL, 
                                        refShape = 'target', imageList = imageList_KO_M_D_FL)
}

for(e in 1:length(raster_KO_M_D_HR)){
  ID <- names(raster_KO_M_D_HR)[[e]]
  raster_KO_M_D_HR[[ID]] <- maskOutline(raster_KO_M_D_HR[[ID]], IDlist = ID_KO_M_D, outline_D_HR, 
                                        refShape = 'target', imageList = imageList_KO_M_D_HR)
}

for(e in 1:length(raster_KO_M_D_HL)){
  ID <- names(raster_KO_M_D_HL)[[e]]
  raster_KO_M_D_HL[[ID]] <- maskOutline(raster_KO_M_D_HL[[ID]], IDlist = ID_KO_M_D, outline_D_HL, 
                                        refShape = 'target', imageList = imageList_KO_M_D_HL)
}

for(e in 1:length(raster_KO_M_V_FR)){
  ID <- names(raster_KO_M_V_FR)[[e]]
  raster_KO_M_V_FR[[ID]] <- maskOutline(raster_KO_M_V_FR[[ID]], IDlist = ID_KO_M_V, outline_V_FR, 
                                        refShape = 'target', imageList = imageList_KO_M_V_FR)
}

for(e in 1:length(raster_KO_M_V_FL)){
  ID <- names(raster_KO_M_V_FL)[[e]]
  raster_KO_M_V_FL[[ID]] <- maskOutline(raster_KO_M_V_FL[[ID]], IDlist = ID_KO_M_V, outline_V_FL, 
                                        refShape = 'target', imageList = imageList_KO_M_V_FL)
}

for(e in 1:length(raster_KO_M_V_HR)){
  ID <- names(raster_KO_M_V_HR)[[e]]
  raster_KO_M_V_HR[[ID]] <- maskOutline(raster_KO_M_V_HR[[ID]], IDlist = ID_KO_M_V, outline_V_HR, 
                                        refShape = 'target', imageList = imageList_KO_M_V_HR)
}

for(e in 1:length(raster_KO_M_V_HL)){
  ID <- names(raster_KO_M_V_HL)[[e]]
  raster_KO_M_V_HL[[ID]] <- maskOutline(raster_KO_M_V_HL[[ID]], IDlist = ID_KO_M_V, outline_V_HL, 
                                        refShape = 'target', imageList = imageList_KO_M_V_HL)
}

# masking/filtering extracted patterns part 2 (with mask)
for(e in 1:length(raster_KO_M_D_HR)){
  ID <- names(raster_KO_M_D_HR)[[e]]
  raster_KO_M_D_HR[[ID]] <- maskOutline(raster_KO_M_D_HR[[ID]], IDlist = ID_KO_M_D, mask_D_HR, 
                                        refShape = 'target', imageList = imageList_KO_M_D_HR)
}

for(e in 1:length(raster_KO_M_D_HL)){
  ID <- names(raster_KO_M_D_HL)[[e]]
  raster_KO_M_D_HL[[ID]] <- maskOutline(raster_KO_M_D_HL[[ID]], IDlist = ID_KO_M_D, mask_D_HL, 
                                        refShape = 'target', imageList = imageList_KO_M_D_HL)
}

for(e in 1:length(raster_KO_M_V_FR)){
  ID <- names(raster_KO_M_V_FR)[[e]]
  raster_KO_M_V_FR[[ID]] <- maskOutline(raster_KO_M_V_FR[[ID]], IDlist = ID_KO_M_V, mask_V_FR, 
                                        refShape = 'target', imageList = imageList_KO_M_V_FR)
}

for(e in 1:length(raster_KO_M_V_FL)){
  ID <- names(raster_KO_M_V_FL)[[e]]
  raster_KO_M_V_FL[[ID]] <- maskOutline(raster_KO_M_V_FL[[ID]], IDlist = ID_KO_M_V, mask_V_FL, 
                                        refShape = 'target', imageList = imageList_KO_M_V_FL)
}

for(e in 1:length(raster_KO_M_V_HR)){
  ID <- names(raster_KO_M_V_HR)[[e]]
  raster_KO_M_V_HR[[ID]] <- maskOutline(raster_KO_M_V_HR[[ID]], IDlist = ID_KO_M_V, mask_V_HR, 
                                        refShape = 'target', imageList = imageList_KO_M_V_HR)
}

for(e in 1:length(raster_KO_M_V_HL)){
  ID <- names(raster_KO_M_V_HL)[[e]]
  raster_KO_M_V_HL[[ID]] <- maskOutline(raster_KO_M_V_HL[[ID]], IDlist = ID_KO_M_V, mask_V_HL, 
                                        refShape = 'target', imageList = imageList_KO_M_V_HL)
}


# plotting patterns
sumraster_KO_M_D_FR <- sumRaster(raster_KO_M_D_FR, ID_KO_M_D, type = 'RGB')
colfunc <- hsv(1, 1, seq(0,1,length.out = 100))
png(file = "Plots/plot_heatmap_KO_M_D_FR.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sumraster_KO_M_D_FR, ID_KO_M_D, 
         imageList = imageList_KO_M_D_FR, landList = lanList_KO_M_D_FR, 
         refShape = 'target', crop = c(0,0,0,0),
         flipRaster = 'y', flipOutline = 'y',
         outline = outline_D_FR, colpalette = colfunc, 
         plotCartoon = TRUE,  cartoonID = 'KO-M-D-6H', refImage = imageList_KO_M_D_FR[['KO-M-D-6H']],
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "proportion of individuals with colour", lines = lines_D_FR)
#dev.copy(png, file = "Plots/plot_heatmap_KO_M_D_FR.png",
#         width = 10, height = 6, units = "in", res = 1200)
dev.off()

sumraster_KO_M_D_FL <- sumRaster(raster_KO_M_D_FL, ID_KO_M_D, type = 'RGB')
colfunc <- hsv(1, 1, seq(0,1,length.out = 100))
png(file = "Plots/plot_heatmap_KO_M_D_FL.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sumraster_KO_M_D_FL, ID_KO_M_D, 
         imageList = imageList_KO_M_D_FL, landList = lanList_KO_M_D_FL, 
         refShape = 'target', crop = c(0,0,0,0),
         flipRaster = 'y', flipOutline = 'y',
         outline = outline_D_FL, colpalette = colfunc, 
         plotCartoon = TRUE,  cartoonID = 'KO-M-D-6H', refImage = imageList_KO_M_D_FL[['KO-M-D-6H']],
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "proportion of individuals with colour", lines = lines_D_FL)
#dev.copy(png, file = "Plots/plot_heatmap_KO_M_D_FL.png",
#         width = 10, height = 6, units = "in", res = 1200)
dev.off()

sumraster_KO_M_D_HR <- sumRaster(raster_KO_M_D_HR, ID_KO_M_D, type = 'RGB')
colfunc <- c('black', 'grey', 'khaki1')
png(file = "Plots/plot_heatmap_KO_M_D_HR.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sumraster_KO_M_D_HR, ID_KO_M_D, 
         imageList = imageList_KO_M_D_HR, landList = lanList_KO_M_D_HR, 
         refShape = 'target', crop = c(0,0,0,0),
         flipRaster = 'y', flipOutline = 'y',
         outline = outline_D_HR, colpalette = colfunc, 
         plotCartoon = TRUE,  cartoonID = 'KO-M-D-6H', refImage = imageList_KO_M_D_HR[['KO-M-D-6H']],
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "proportion of individuals with colour", lines = lines_D_HR)
#dev.copy(png, file = "Plots/plot_heatmap_KO_M_D_HR.png",
#         width = 10, height = 6, units = "in", res = 1200)
dev.off()

sumraster_KO_M_D_HL <- sumRaster(raster_KO_M_D_HL, ID_KO_M_D, type = 'RGB')
colfunc <- c('black', 'grey', 'khaki1')
png(file = "Plots/plot_heatmap_KO_M_D_HL.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sumraster_KO_M_D_HL, ID_KO_M_D, 
         imageList = imageList_KO_M_D_HL, landList = lanList_KO_M_D_HL, 
         refShape = 'target', crop = c(0,0,0,0),
         flipRaster = 'y', flipOutline = 'y',
         outline = outline_D_HL, colpalette = colfunc, 
         plotCartoon = TRUE,  cartoonID = 'KO-M-D-6H', refImage = imageList_KO_M_D_HL[['KO-M-D-6H']],
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "proportion of individuals with colour", lines = lines_D_HL)
#dev.copy(png, file = "Plots/plot_heatmap_KO_M_D_HL.png",
#         width = 10, height = 6, units = "in", res = 1200)
dev.off()

sumraster_KO_M_V_FR <- sumRaster(raster_KO_M_V_FR, ID_KO_M_V, type = 'RGB')
colfunc <- hsv(1, 1, seq(0,1,length.out = 100))
png(file = "Plots/plot_heatmap_KO_M_V_FR.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sumraster_KO_M_V_FR, ID_KO_M_V, 
         imageList = imageList_KO_M_V_FR, landList = lanList_KO_M_V_FR, 
         refShape = 'target', crop = c(0,0,0,0),
         flipRaster = 'y', flipOutline = 'y',
         outline = outline_V_FR, colpalette = colfunc, 
         plotCartoon = TRUE,  cartoonID = 'KO-M-V-6H', refImage = imageList_KO_M_V_FR[['KO-M-V-6H']],
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "proportion of individuals with colour", lines = lines_V_FR)
#dev.copy(png, file = "Plots/plot_heatmap_KO_M_V_FR.png",
#         width = 10, height = 6, units = "in", res = 1200)
dev.off()

sumraster_KO_M_V_FL <- sumRaster(raster_KO_M_V_FL, ID_KO_M_V, type = 'RGB')
colfunc <- hsv(1, 1, seq(0,1,length.out = 100))
png(file = "Plots/plot_heatmap_KO_M_V_FL.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sumraster_KO_M_V_FL, ID_KO_M_V, 
         imageList = imageList_KO_M_V_FL, landList = lanList_KO_M_V_FL, 
         refShape = 'target', crop = c(0,0,0,0),
         flipRaster = 'y', flipOutline = 'y',
         outline = outline_V_FL, colpalette = colfunc, 
         plotCartoon = TRUE,  cartoonID = 'KO-M-V-6H', refImage = imageList_KO_M_V_FL[['KO-M-V-6H']],
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "proportion of individuals with colour", lines = lines_V_FL)
#dev.copy(png, file = "Plots/plot_heatmap_KO_M_V_FL.png",
#         width = 10, height = 6, units = "in", res = 1200)
dev.off()

sumraster_KO_M_V_HR <- sumRaster(raster_KO_M_V_HR, ID_KO_M_V, type = 'RGB')
colfunc <- c('black', 'grey', 'khaki1')
png(file = "Plots/plot_heatmap_KO_M_V_HR.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sumraster_KO_M_V_HR, ID_KO_M_V, 
         imageList = imageList_KO_M_V_HR, landList = lanList_KO_M_V_HR, 
         refShape = 'target', crop = c(0,0,0,0),
         flipRaster = 'y', flipOutline = 'y',
         outline = outline_V_HR, colpalette = colfunc, 
         plotCartoon = TRUE,  cartoonID = 'KO-M-V-6H', refImage = imageList_KO_M_V_HR[['KO-M-V-6H']],
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "proportion of individuals with colour", lines = lines_V_HR)
#dev.copy(png, file = "Plots/plot_heatmap_KO_M_V_HR.png",
#         width = 10, height = 6, units = "in", res = 1200)
dev.off()

sumraster_KO_M_V_HL <- sumRaster(raster_KO_M_V_HL, ID_KO_M_V, type = 'RGB')
colfunc <- c('black', 'grey', 'khaki1')
png(file = "Plots/plot_heatmap_KO_M_V_HL.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sumraster_KO_M_V_HL, ID_KO_M_V, 
         imageList = imageList_KO_M_V_HL, landList = lanList_KO_M_V_HL, 
         refShape = 'target', crop = c(0,0,0,0),
         flipRaster = 'y', flipOutline = 'y',
         outline = outline_V_HL, colpalette = colfunc, 
         plotCartoon = TRUE,  cartoonID = 'KO-M-V-6H', refImage = imageList_KO_M_V_HL[['KO-M-V-6H']],
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "proportion of individuals with colour", lines = lines_V_HL)
#dev.copy(png, file = "Plots/plot_heatmap_KO_M_V_HL.png",
#         width = 10, height = 6, units = "in", res = 1200)
dev.off()



### GENERATING DATA - ND F #####

# indicating sample id
ID_ND_F_D <- c('ND-F-D-10G', 
               'ND-F-D-12R', 
               'ND-F-D-16O', 
               'ND-F-D-17R', 
               'ND-F-D-1Q', 
               'ND-F-D-20213E', 
               'ND-F-D-20A', 
               'ND-F-D-22J', 
               'ND-F-D-23A', 
               'ND-F-D-28J', 
               'ND-F-D-29A', 
               'ND-F-D-2O', 
               'ND-F-D-30A', 
               'ND-F-D-3Q', 
               'ND-F-D-4P', 
               'ND-F-D-4Q', 
               'ND-F-D-5A', 
               'ND-F-D-5Q', 
               'ND-F-D-6G', 
               'ND-F-D-6M', 
               'ND-F-D-6O', 
               'ND-F-D-7G', 
               'ND-F-D-8G')

ID_ND_F_V <- c('ND-F-V-10G', 
               'ND-F-V-12R', 
               'ND-F-V-16O', 
               'ND-F-V-17R', 
               'ND-F-V-1Q', 
               'ND-F-V-20213E', 
               'ND-F-V-20A', 
               'ND-F-V-22J', 
               'ND-F-V-23A', 
               'ND-F-V-28J', 
               'ND-F-V-29A', 
               'ND-F-V-2O', 
               'ND-F-V-30A', 
               'ND-F-V-3Q', 
               'ND-F-V-4P', 
               'ND-F-V-4Q', 
               'ND-F-V-5A', 
               'ND-F-V-5Q', 
               'ND-F-V-6G', 
               'ND-F-V-6M', 
               'ND-F-V-6O', 
               'ND-F-V-7G', 
               'ND-F-V-8G')

# indicating sample images and landmarks
prepath <- 'images/ND-F/Dorsal/Forewing/'
extension <- '-FR.jpg'
imageList_ND_F_D_FR <- makeList(ID_ND_F_D, 'image', prepath, extension)

prepath <- 'images/ND-F/Dorsal/Forewing/'
extension <- '-FL.jpg'
imageList_ND_F_D_FL <- makeList(ID_ND_F_D, 'image', prepath, extension)

prepath <- 'images/ND-F/Dorsal/Hindwing/'
extension <- '-HR.jpg'
imageList_ND_F_D_HR <- makeList(ID_ND_F_D, 'image', prepath, extension)

prepath <- 'images/ND-F/Dorsal/Hindwing/'
extension <- '-HL.jpg'
imageList_ND_F_D_HL <- makeList(ID_ND_F_D, 'image', prepath, extension)

prepath <- 'images/ND-F/Ventral/Forewing/'
extension <- '-FR.jpg'
imageList_ND_F_V_FR <- makeList(ID_ND_F_V, 'image', prepath, extension)

prepath <- 'images/ND-F/Ventral/Forewing/'
extension <- '-FL.jpg'
imageList_ND_F_V_FL <- makeList(ID_ND_F_V, 'image', prepath, extension)

prepath <- 'images/ND-F/Ventral/Hindwing/'
extension <- '-HR.jpg'
imageList_ND_F_V_HR <- makeList(ID_ND_F_V, 'image', prepath, extension)

prepath <- 'images/ND-F/Ventral/Hindwing/'
extension <- '-HL.jpg'
imageList_ND_F_V_HL <- makeList(ID_ND_F_V, 'image', prepath, extension)


prepath <- 'landmarks/ND-F/Dorsal/Forewing/'
extension <- '-FR.txt'
lanList_ND_F_D_FR <- makeList(ID_ND_F_D, 'landmark', prepath, extension)

prepath <- 'landmarks/ND-F/Dorsal/Forewing/'
extension <- '-FL.txt'
lanList_ND_F_D_FL <- makeList(ID_ND_F_D, 'landmark', prepath, extension)

prepath <- 'landmarks/ND-F/Dorsal/Hindwing/'
extension <- '-HR.txt'
lanList_ND_F_D_HR <- makeList(ID_ND_F_D, 'landmark', prepath, extension)

prepath <- 'landmarks/ND-F/Dorsal/Hindwing/'
extension <- '-HL.txt'
lanList_ND_F_D_HL <- makeList(ID_ND_F_D, 'landmark', prepath, extension)

prepath <- 'landmarks/ND-F/Ventral/Forewing/'
extension <- '-FR.txt'
lanList_ND_F_V_FR <- makeList(ID_ND_F_V, 'landmark', prepath, extension)

prepath <- 'landmarks/ND-F/Ventral/Forewing/'
extension <- '-FL.txt'
lanList_ND_F_V_FL <- makeList(ID_ND_F_V, 'landmark', prepath, extension)

prepath <- 'landmarks/ND-F/Ventral/Hindwing/'
extension <- '-HR.txt'
lanList_ND_F_V_HR <- makeList(ID_ND_F_V, 'landmark', prepath, extension)

prepath <- 'landmarks/ND-F/Ventral/Hindwing/'
extension <- '-HL.txt'
lanList_ND_F_V_HL <- makeList(ID_ND_F_V, 'landmark', prepath, extension)


# extracting patterns
RGB_FWD <- c(237,51,39)
raster_ND_F_D_FR <- patLanRGB(imageList_ND_F_D_FR, lanList_ND_F_D_FR, RGB_FWD,
                              transformRef = target_D_FR, resampleFactor = 3,
                              colOffset= 0.3, res = 300, 
                              adjustCoords = TRUE, plot = 'stack')

RGB_FWD <- c(237,51,39)
raster_ND_F_D_FL <- patLanRGB(imageList_ND_F_D_FL, lanList_ND_F_D_FL, RGB_FWD,
                              transformRef = target_D_FL, resampleFactor = 3,
                              colOffset= 0.3, res = 300, 
                              adjustCoords = TRUE, plot = 'stack')

RGB_HWD <- c(254,253,127)
raster_ND_F_D_HR <- patLanRGB(imageList_ND_F_D_HR, lanList_ND_F_D_HR, RGB_HWD,
                              transformRef = target_D_HR, resampleFactor = 3,
                              colOffset= 0.3, res = 300, 
                              adjustCoords = TRUE, plot = 'stack')

RGB_HWD <- c(254,253,127)
raster_ND_F_D_HL <- patLanRGB(imageList_ND_F_D_HL, lanList_ND_F_D_HL, RGB_HWD,
                              transformRef = target_D_HL, resampleFactor = 3,
                              colOffset= 0.3, res = 300, 
                              adjustCoords = TRUE, plot = 'stack')

#RGB_FWV <- c(252,114,112)
RGB_FWV <- c(252,163,110)
raster_ND_F_V_FR <- patLanRGB(imageList_ND_F_V_FR, lanList_ND_F_V_FR, RGB_FWV,
                              transformRef = target_V_FR, resampleFactor = 3,
                              colOffset= 0.25, res = 300, 
                              adjustCoords = TRUE, plot = 'stack')

RGB_FWV <- c(252,163,110)
raster_ND_F_V_FL <- patLanRGB(imageList_ND_F_V_FL, lanList_ND_F_V_FL, RGB_FWV,
                              transformRef = target_V_FL, resampleFactor = 3,
                              colOffset= 0.25, res = 300, 
                              adjustCoords = TRUE, plot = 'stack')

RGB_HWV <- c(254,253,127)
raster_ND_F_V_HR <- patLanRGB(imageList_ND_F_V_HR, lanList_ND_F_V_HR, RGB_HWV,
                              transformRef = target_V_HR, resampleFactor = 3,
                              colOffset= 0.3, res = 300, 
                              adjustCoords = TRUE, plot = 'stack')

RGB_HWV <- c(254,253,127)
raster_ND_F_V_HL <- patLanRGB(imageList_ND_F_V_HL, lanList_ND_F_V_HL, RGB_HWV,
                              transformRef = target_V_HL, resampleFactor = 3,
                              colOffset= 0.3, res = 300, 
                              adjustCoords = TRUE, plot = 'stack')


save(raster_ND_F_D_FR, file = 'data/raster_ND_F_D_FR.rda')
save(raster_ND_F_D_FL, file = 'data/raster_ND_F_D_FL.rda')
save(raster_ND_F_D_HR, file = 'data/raster_ND_F_D_HR.rda')
save(raster_ND_F_D_HL, file = 'data/raster_ND_F_D_HL.rda')
save(raster_ND_F_V_FR, file = 'data/raster_ND_F_V_FR.rda')
save(raster_ND_F_V_FL, file = 'data/raster_ND_F_V_FL.rda')
save(raster_ND_F_V_HR, file = 'data/raster_ND_F_V_HR.rda')
save(raster_ND_F_V_HL, file = 'data/raster_ND_F_V_HL.rda')

load('data/raster_ND_F_D_FR.rda')
load('data/raster_ND_F_D_FL.rda')
load('data/raster_ND_F_D_HR.rda')
load('data/raster_ND_F_D_HL.rda')
load('data/raster_ND_F_V_FR.rda')
load('data/raster_ND_F_V_FL.rda')
load('data/raster_ND_F_V_HR.rda')
load('data/raster_ND_F_V_HL.rda')


# masking/filtering extracted patterns part 1 (with outline)
for(e in 1:length(raster_ND_F_D_FR)){
  ID <- names(raster_ND_F_D_FR)[[e]]
  raster_ND_F_D_FR[[ID]] <- maskOutline(raster_ND_F_D_FR[[ID]], IDlist = ID_ND_F_D, outline_D_FR, 
                                        refShape = 'target', imageList = imageList_ND_F_D_FR)
}

for(e in 1:length(raster_ND_F_D_FL)){
  ID <- names(raster_ND_F_D_FL)[[e]]
  raster_ND_F_D_FL[[ID]] <- maskOutline(raster_ND_F_D_FL[[ID]], IDlist = ID_ND_F_D, outline_D_FL, 
                                        refShape = 'target', imageList = imageList_ND_F_D_FL)
}

for(e in 1:length(raster_ND_F_D_HR)){
  ID <- names(raster_ND_F_D_HR)[[e]]
  raster_ND_F_D_HR[[ID]] <- maskOutline(raster_ND_F_D_HR[[ID]], IDlist = ID_ND_F_D, outline_D_HR, 
                                        refShape = 'target', imageList = imageList_ND_F_D_HR)
}

for(e in 1:length(raster_ND_F_D_HL)){
  ID <- names(raster_ND_F_D_HL)[[e]]
  raster_ND_F_D_HL[[ID]] <- maskOutline(raster_ND_F_D_HL[[ID]], IDlist = ID_ND_F_D, outline_D_HL, 
                                        refShape = 'target', imageList = imageList_ND_F_D_HL)
}

for(e in 1:length(raster_ND_F_V_FR)){
  ID <- names(raster_ND_F_V_FR)[[e]]
  raster_ND_F_V_FR[[ID]] <- maskOutline(raster_ND_F_V_FR[[ID]], IDlist = ID_ND_F_V, outline_V_FR, 
                                        refShape = 'target', imageList = imageList_ND_F_V_FR)
}

for(e in 1:length(raster_ND_F_V_FL)){
  ID <- names(raster_ND_F_V_FL)[[e]]
  raster_ND_F_V_FL[[ID]] <- maskOutline(raster_ND_F_V_FL[[ID]], IDlist = ID_ND_F_V, outline_V_FL, 
                                        refShape = 'target', imageList = imageList_ND_F_V_FL)
}

for(e in 1:length(raster_ND_F_V_HR)){
  ID <- names(raster_ND_F_V_HR)[[e]]
  raster_ND_F_V_HR[[ID]] <- maskOutline(raster_ND_F_V_HR[[ID]], IDlist = ID_ND_F_V, outline_V_HR, 
                                        refShape = 'target', imageList = imageList_ND_F_V_HR)
}

for(e in 1:length(raster_ND_F_V_HL)){
  ID <- names(raster_ND_F_V_HL)[[e]]
  raster_ND_F_V_HL[[ID]] <- maskOutline(raster_ND_F_V_HL[[ID]], IDlist = ID_ND_F_V, outline_V_HL, 
                                        refShape = 'target', imageList = imageList_ND_F_V_HL)
}

# masking/filtering extracted patterns part 2 (with mask)
for(e in 1:length(raster_ND_F_D_HR)){
  ID <- names(raster_ND_F_D_HR)[[e]]
  raster_ND_F_D_HR[[ID]] <- maskOutline(raster_ND_F_D_HR[[ID]], IDlist = ID_ND_F_D, mask_D_HR, 
                                        refShape = 'target', imageList = imageList_ND_F_D_HR)
}

for(e in 1:length(raster_ND_F_D_HL)){
  ID <- names(raster_ND_F_D_HL)[[e]]
  raster_ND_F_D_HL[[ID]] <- maskOutline(raster_ND_F_D_HL[[ID]], IDlist = ID_ND_F_D, mask_D_HL, 
                                        refShape = 'target', imageList = imageList_ND_F_D_HL)
}

for(e in 1:length(raster_ND_F_V_FR)){
  ID <- names(raster_ND_F_V_FR)[[e]]
  raster_ND_F_V_FR[[ID]] <- maskOutline(raster_ND_F_V_FR[[ID]], IDlist = ID_ND_F_V, mask_V_FR, 
                                        refShape = 'target', imageList = imageList_ND_F_V_FR)
}

for(e in 1:length(raster_ND_F_V_FL)){
  ID <- names(raster_ND_F_V_FL)[[e]]
  raster_ND_F_V_FL[[ID]] <- maskOutline(raster_ND_F_V_FL[[ID]], IDlist = ID_ND_F_V, mask_V_FL, 
                                        refShape = 'target', imageList = imageList_ND_F_V_FL)
}

for(e in 1:length(raster_ND_F_V_HR)){
  ID <- names(raster_ND_F_V_HR)[[e]]
  raster_ND_F_V_HR[[ID]] <- maskOutline(raster_ND_F_V_HR[[ID]], IDlist = ID_ND_F_V, mask_V_HR, 
                                        refShape = 'target', imageList = imageList_ND_F_V_HR)
}

for(e in 1:length(raster_ND_F_V_HL)){
  ID <- names(raster_ND_F_V_HL)[[e]]
  raster_ND_F_V_HL[[ID]] <- maskOutline(raster_ND_F_V_HL[[ID]], IDlist = ID_ND_F_V, mask_V_HL, 
                                        refShape = 'target', imageList = imageList_ND_F_V_HL)
}


# plotting patterns
sumraster_ND_F_D_FR <- sumRaster(raster_ND_F_D_FR, ID_ND_F_D, type = 'RGB')
colfunc <- hsv(1, 1, seq(0,1,length.out = 100))
png(file = "Plots/plot_heatmap_ND_F_D_FR.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sumraster_ND_F_D_FR, ID_ND_F_D, 
         imageList = imageList_ND_F_D_FR, landList = lanList_ND_F_D_FR, 
         refShape = 'target', crop = c(0,0,0,0),
         flipRaster = 'y', flipOutline = 'y',
         outline = outline_D_FR, colpalette = colfunc, 
         plotCartoon = TRUE,  cartoonID = 'KO-M-D-6H', refImage = imageList_KO_M_D_FR[['KO-M-D-6H']],
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "proportion of individuals with colour", lines = lines_D_FR)
#dev.copy(png, file = "Plots/plot_heatmap_ND_F_D_FR.png",
#         width = 10, height = 6, units = "in", res = 1200)
dev.off()

sumraster_ND_F_D_FL <- sumRaster(raster_ND_F_D_FL, ID_ND_F_D, type = 'RGB')
colfunc <- hsv(1, 1, seq(0,1,length.out = 100))
png(file = "Plots/plot_heatmap_ND_F_D_FL.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sumraster_ND_F_D_FL, ID_ND_F_D, 
         imageList = imageList_ND_F_D_FL, landList = lanList_ND_F_D_FL, 
         refShape = 'target', crop = c(0,0,0,0),
         flipRaster = 'y', flipOutline = 'y',
         outline = outline_D_FL, colpalette = colfunc, 
         plotCartoon = TRUE,  cartoonID = 'KO-M-D-6H', refImage = imageList_KO_M_D_FL[['KO-M-D-6H']],
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "proportion of individuals with colour", lines = lines_D_FL)
#dev.copy(png, file = "Plots/plot_heatmap_ND_F_D_FL.png",
#         width = 10, height = 6, units = "in", res = 1200)
dev.off()

sumraster_ND_F_D_HR <- sumRaster(raster_ND_F_D_HR, ID_ND_F_D, type = 'RGB')
colfunc <- c('black', 'grey', 'khaki1')
png(file = "Plots/plot_heatmap_ND_F_D_HR.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sumraster_ND_F_D_HR, ID_ND_F_D, 
         imageList = imageList_ND_F_D_HR, landList = lanList_ND_F_D_HR, 
         refShape = 'target', crop = c(0,0,0,0),
         flipRaster = 'y', flipOutline = 'y',
         outline = outline_D_HR, colpalette = colfunc, 
         plotCartoon = TRUE,  cartoonID = 'KO-M-D-6H', refImage = imageList_KO_M_D_HR[['KO-M-D-6H']],
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "proportion of individuals with colour", lines = lines_D_HR)
#dev.copy(png, file = "Plots/plot_heatmap_ND_F_D_HR.png",
#         width = 10, height = 6, units = "in", res = 1200)
dev.off()

sumraster_ND_F_D_HL <- sumRaster(raster_ND_F_D_HL, ID_ND_F_D, type = 'RGB')
colfunc <- c('black', 'grey', 'khaki1')
png(file = "Plots/plot_heatmap_ND_F_D_HL.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sumraster_ND_F_D_HL, ID_ND_F_D, 
         imageList = imageList_ND_F_D_HL, landList = lanList_ND_F_D_HL, 
         refShape = 'target', crop = c(0,0,0,0),
         flipRaster = 'y', flipOutline = 'y',
         outline = outline_D_HL, colpalette = colfunc, 
         plotCartoon = TRUE,  cartoonID = 'KO-M-D-6H', refImage = imageList_KO_M_D_HL[['KO-M-D-6H']],
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "proportion of individuals with colour", lines = lines_D_HL)
#dev.copy(png, file = "Plots/plot_heatmap_ND_F_D_HL.png",
#         width = 10, height = 6, units = "in", res = 1200)
dev.off()

sumraster_ND_F_V_FR <- sumRaster(raster_ND_F_V_FR, ID_ND_F_V, type = 'RGB')
colfunc <- hsv(1, 1, seq(0,1,length.out = 100))
png(file = "Plots/plot_heatmap_ND_F_V_FR.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sumraster_ND_F_V_FR, ID_ND_F_V, 
         imageList = imageList_ND_F_V_FR, landList = lanList_ND_F_V_FR, 
         refShape = 'target', crop = c(0,0,0,0),
         flipRaster = 'y', flipOutline = 'y',
         outline = outline_V_FR, colpalette = colfunc, 
         plotCartoon = TRUE,  cartoonID = 'KO-M-V-6H', refImage = imageList_KO_M_V_FR[['KO-M-V-6H']],
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "proportion of individuals with colour", lines = lines_V_FR)
#dev.copy(png, file = "Plots/plot_heatmap_ND_F_V_FR.png",
#         width = 10, height = 6, units = "in", res = 1200)
dev.off()

sumraster_ND_F_V_FL <- sumRaster(raster_ND_F_V_FL, ID_ND_F_V, type = 'RGB')
colfunc <- hsv(1, 1, seq(0,1,length.out = 100))
png(file = "Plots/plot_heatmap_ND_F_V_FL.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sumraster_ND_F_V_FL, ID_ND_F_V, 
         imageList = imageList_ND_F_V_FL, landList = lanList_ND_F_V_FL, 
         refShape = 'target', crop = c(0,0,0,0),
         flipRaster = 'y', flipOutline = 'y',
         outline = outline_V_FL, colpalette = colfunc, 
         plotCartoon = TRUE,  cartoonID = 'KO-M-V-6H', refImage = imageList_KO_M_V_FL[['KO-M-V-6H']],
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "proportion of individuals with colour", lines = lines_V_FL)
#dev.copy(png, file = "Plots/plot_heatmap_ND_F_V_FL.png",
#         width = 10, height = 6, units = "in", res = 1200)
dev.off()

sumraster_ND_F_V_HR <- sumRaster(raster_ND_F_V_HR, ID_ND_F_V, type = 'RGB')
colfunc <- c('black', 'grey', 'khaki1')
png(file = "Plots/plot_heatmap_ND_F_V_HR.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sumraster_ND_F_V_HR, ID_ND_F_V, 
         imageList = imageList_ND_F_V_HR, landList = lanList_ND_F_V_HR, 
         refShape = 'target', crop = c(0,0,0,0),
         flipRaster = 'y', flipOutline = 'y',
         outline = outline_V_HR, colpalette = colfunc, 
         plotCartoon = TRUE,  cartoonID = 'KO-M-V-6H', refImage = imageList_KO_M_V_HR[['KO-M-V-6H']],
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "proportion of individuals with colour", lines = lines_V_HR)
#dev.copy(png, file = "Plots/plot_heatmap_ND_F_V_HR.png",
#         width = 10, height = 6, units = "in", res = 1200)
dev.off()

sumraster_ND_F_V_HL <- sumRaster(raster_ND_F_V_HL, ID_ND_F_V, type = 'RGB')
colfunc <- c('black', 'grey', 'khaki1')
png(file = "Plots/plot_heatmap_ND_F_V_HL.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sumraster_ND_F_V_HL, ID_ND_F_V, 
         imageList = imageList_ND_F_V_HL, landList = lanList_ND_F_V_HL, 
         refShape = 'target', crop = c(0,0,0,0),
         flipRaster = 'y', flipOutline = 'y',
         outline = outline_V_HL, colpalette = colfunc, 
         plotCartoon = TRUE,  cartoonID = 'KO-M-V-6H', refImage = imageList_KO_M_V_HL[['KO-M-V-6H']],
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "proportion of individuals with colour", lines = lines_V_HL)
#dev.copy(png, file = "Plots/plot_heatmap_ND_F_V_HL.png",
#         width = 10, height = 6, units = "in", res = 1200)
dev.off()



### GENERATING DATA - ND M #####

# indicating sample id
ID_ND_M_D <- c('ND-M-D-10K', 
               'ND-M-D-12N', 
               'ND-M-D-14K', 
               'ND-M-D-16F', 
               'ND-M-D-17O', 
               'ND-M-D-19J', 
               'ND-M-D-19P', 
               'ND-M-D-1A', 
               'ND-M-D-1J', 
               'ND-M-D-1P', 
               'ND-M-D-20211F', 
               'ND-M-D-23J', 
               'ND-M-D-26J', 
               'ND-M-D-2A', 
               'ND-M-D-4G', 
               'ND-M-D-5V', 
               'ND-M-D-6T', 
               'ND-M-D-8O', 
               'ND-M-D-9O')

ID_ND_M_V <- c('ND-M-V-10K', 
               'ND-M-V-12N', 
               'ND-M-V-14K', 
               'ND-M-V-16F', 
               'ND-M-V-17O', 
               'ND-M-V-19J', 
               'ND-M-V-19P', 
               'ND-M-V-1A', 
               'ND-M-V-1J', 
               'ND-M-V-1P', 
               'ND-M-V-20211F', 
               'ND-M-V-23J', 
               'ND-M-V-26J', 
               'ND-M-V-2A', 
               'ND-M-V-4G', 
               'ND-M-V-5V', 
               'ND-M-V-6T', 
               'ND-M-V-8O', 
               'ND-M-V-9O')

# indicating sample images and landmarks
prepath <- 'images/ND-M/Dorsal/Forewing/'
extension <- '-FR.jpg'
imageList_ND_M_D_FR <- makeList(ID_ND_M_D, 'image', prepath, extension)

prepath <- 'images/ND-M/Dorsal/Forewing/'
extension <- '-FL.jpg'
imageList_ND_M_D_FL <- makeList(ID_ND_M_D, 'image', prepath, extension)

prepath <- 'images/ND-M/Dorsal/Hindwing/'
extension <- '-HR.jpg'
imageList_ND_M_D_HR <- makeList(ID_ND_M_D, 'image', prepath, extension)

prepath <- 'images/ND-M/Dorsal/Hindwing/'
extension <- '-HL.jpg'
imageList_ND_M_D_HL <- makeList(ID_ND_M_D, 'image', prepath, extension)

prepath <- 'images/ND-M/Ventral/Forewing/'
extension <- '-FR.jpg'
imageList_ND_M_V_FR <- makeList(ID_ND_M_V, 'image', prepath, extension)

prepath <- 'images/ND-M/Ventral/Forewing/'
extension <- '-FL.jpg'
imageList_ND_M_V_FL <- makeList(ID_ND_M_V, 'image', prepath, extension)

prepath <- 'images/ND-M/Ventral/Hindwing/'
extension <- '-HR.jpg'
imageList_ND_M_V_HR <- makeList(ID_ND_M_V, 'image', prepath, extension)

prepath <- 'images/ND-M/Ventral/Hindwing/'
extension <- '-HL.jpg'
imageList_ND_M_V_HL <- makeList(ID_ND_M_V, 'image', prepath, extension)


prepath <- 'landmarks/ND-M/Dorsal/Forewing/'
extension <- '-FR.txt'
lanList_ND_M_D_FR <- makeList(ID_ND_M_D, 'landmark', prepath, extension)

prepath <- 'landmarks/ND-M/Dorsal/Forewing/'
extension <- '-FL.txt'
lanList_ND_M_D_FL <- makeList(ID_ND_M_D, 'landmark', prepath, extension)

prepath <- 'landmarks/ND-M/Dorsal/Hindwing/'
extension <- '-HR.txt'
lanList_ND_M_D_HR <- makeList(ID_ND_M_D, 'landmark', prepath, extension)

prepath <- 'landmarks/ND-M/Dorsal/Hindwing/'
extension <- '-HL.txt'
lanList_ND_M_D_HL <- makeList(ID_ND_M_D, 'landmark', prepath, extension)

prepath <- 'landmarks/ND-M/Ventral/Forewing/'
extension <- '-FR.txt'
lanList_ND_M_V_FR <- makeList(ID_ND_M_V, 'landmark', prepath, extension)

prepath <- 'landmarks/ND-M/Ventral/Forewing/'
extension <- '-FL.txt'
lanList_ND_M_V_FL <- makeList(ID_ND_M_V, 'landmark', prepath, extension)

prepath <- 'landmarks/ND-M/Ventral/Hindwing/'
extension <- '-HR.txt'
lanList_ND_M_V_HR <- makeList(ID_ND_M_V, 'landmark', prepath, extension)

prepath <- 'landmarks/ND-M/Ventral/Hindwing/'
extension <- '-HL.txt'
lanList_ND_M_V_HL <- makeList(ID_ND_M_V, 'landmark', prepath, extension)


# extracting patterns
RGB_FWD <- c(237,51,39)
raster_ND_M_D_FR <- patLanRGB(imageList_ND_M_D_FR, lanList_ND_M_D_FR, RGB_FWD,
                              transformRef = target_D_FR, resampleFactor = 3,
                              colOffset= 0.3, res = 300, 
                              adjustCoords = TRUE, plot = 'stack')

RGB_FWD <- c(237,51,39)
raster_ND_M_D_FL <- patLanRGB(imageList_ND_M_D_FL, lanList_ND_M_D_FL, RGB_FWD,
                              transformRef = target_D_FL, resampleFactor = 3,
                              colOffset= 0.3, res = 300, 
                              adjustCoords = TRUE, plot = 'stack')

RGB_HWD <- c(254,253,127)
raster_ND_M_D_HR <- patLanRGB(imageList_ND_M_D_HR, lanList_ND_M_D_HR, RGB_HWD,
                              transformRef = target_D_HR, resampleFactor = 3,
                              colOffset= 0.3, res = 300, 
                              adjustCoords = TRUE, plot = 'stack')

RGB_HWD <- c(254,253,127)
raster_ND_M_D_HL <- patLanRGB(imageList_ND_M_D_HL, lanList_ND_M_D_HL, RGB_HWD,
                              transformRef = target_D_HL, resampleFactor = 3,
                              colOffset= 0.3, res = 300, 
                              adjustCoords = TRUE, plot = 'stack')

#RGB_FWV <- c(252,114,112)
RGB_FWV <- c(252,163,110)
raster_ND_M_V_FR <- patLanRGB(imageList_ND_M_V_FR, lanList_ND_M_V_FR, RGB_FWV,
                              transformRef = target_V_FR, resampleFactor = 3,
                              colOffset= 0.25, res = 300, 
                              adjustCoords = TRUE, plot = 'stack')

RGB_FWV <- c(252,163,110)
raster_ND_M_V_FL <- patLanRGB(imageList_ND_M_V_FL, lanList_ND_M_V_FL, RGB_FWV,
                              transformRef = target_V_FL, resampleFactor = 3,
                              colOffset= 0.25, res = 300, 
                              adjustCoords = TRUE, plot = 'stack')

RGB_HWV <- c(254,253,127)
raster_ND_M_V_HR <- patLanRGB(imageList_ND_M_V_HR, lanList_ND_M_V_HR, RGB_HWV,
                              transformRef = target_V_HR, resampleFactor = 3,
                              colOffset= 0.3, res = 300, 
                              adjustCoords = TRUE, plot = 'stack')

RGB_HWV <- c(254,253,127)
raster_ND_M_V_HL <- patLanRGB(imageList_ND_M_V_HL, lanList_ND_M_V_HL, RGB_HWV,
                              transformRef = target_V_HL, resampleFactor = 3,
                              colOffset= 0.3, res = 300, 
                              adjustCoords = TRUE, plot = 'stack')


save(raster_ND_M_D_FR, file = 'data/raster_ND_M_D_FR.rda')
save(raster_ND_M_D_FL, file = 'data/raster_ND_M_D_FL.rda')
save(raster_ND_M_D_HR, file = 'data/raster_ND_M_D_HR.rda')
save(raster_ND_M_D_HL, file = 'data/raster_ND_M_D_HL.rda')
save(raster_ND_M_V_FR, file = 'data/raster_ND_M_V_FR.rda')
save(raster_ND_M_V_FL, file = 'data/raster_ND_M_V_FL.rda')
save(raster_ND_M_V_HR, file = 'data/raster_ND_M_V_HR.rda')
save(raster_ND_M_V_HL, file = 'data/raster_ND_M_V_HL.rda')

load('data/raster_ND_M_D_FR.rda')
load('data/raster_ND_M_D_FL.rda')
load('data/raster_ND_M_D_HR.rda')
load('data/raster_ND_M_D_HL.rda')
load('data/raster_ND_M_V_FR.rda')
load('data/raster_ND_M_V_FL.rda')
load('data/raster_ND_M_V_HR.rda')
load('data/raster_ND_M_V_HL.rda')


# masking/filtering extracted patterns part 1 (with outline)
for(e in 1:length(raster_ND_M_D_FR)){
  ID <- names(raster_ND_M_D_FR)[[e]]
  raster_ND_M_D_FR[[ID]] <- maskOutline(raster_ND_M_D_FR[[ID]], IDlist = ID_ND_M_D, outline_D_FR, 
                                        refShape = 'target', imageList = imageList_ND_M_D_FR)
}

for(e in 1:length(raster_ND_M_D_FL)){
  ID <- names(raster_ND_M_D_FL)[[e]]
  raster_ND_M_D_FL[[ID]] <- maskOutline(raster_ND_M_D_FL[[ID]], IDlist = ID_ND_M_D, outline_D_FL, 
                                        refShape = 'target', imageList = imageList_ND_M_D_FL)
}

for(e in 1:length(raster_ND_M_D_HR)){
  ID <- names(raster_ND_M_D_HR)[[e]]
  raster_ND_M_D_HR[[ID]] <- maskOutline(raster_ND_M_D_HR[[ID]], IDlist = ID_ND_M_D, outline_D_HR, 
                                        refShape = 'target', imageList = imageList_ND_M_D_HR)
}

for(e in 1:length(raster_ND_M_D_HL)){
  ID <- names(raster_ND_M_D_HL)[[e]]
  raster_ND_M_D_HL[[ID]] <- maskOutline(raster_ND_M_D_HL[[ID]], IDlist = ID_ND_M_D, outline_D_HL, 
                                        refShape = 'target', imageList = imageList_ND_M_D_HL)
}

for(e in 1:length(raster_ND_M_V_FR)){
  ID <- names(raster_ND_M_V_FR)[[e]]
  raster_ND_M_V_FR[[ID]] <- maskOutline(raster_ND_M_V_FR[[ID]], IDlist = ID_ND_M_V, outline_V_FR, 
                                        refShape = 'target', imageList = imageList_ND_M_V_FR)
}

for(e in 1:length(raster_ND_M_V_FL)){
  ID <- names(raster_ND_M_V_FL)[[e]]
  raster_ND_M_V_FL[[ID]] <- maskOutline(raster_ND_M_V_FL[[ID]], IDlist = ID_ND_M_V, outline_V_FL, 
                                        refShape = 'target', imageList = imageList_ND_M_V_FL)
}

for(e in 1:length(raster_ND_M_V_HR)){
  ID <- names(raster_ND_M_V_HR)[[e]]
  raster_ND_M_V_HR[[ID]] <- maskOutline(raster_ND_M_V_HR[[ID]], IDlist = ID_ND_M_V, outline_V_HR, 
                                        refShape = 'target', imageList = imageList_ND_M_V_HR)
}

for(e in 1:length(raster_ND_M_V_HL)){
  ID <- names(raster_ND_M_V_HL)[[e]]
  raster_ND_M_V_HL[[ID]] <- maskOutline(raster_ND_M_V_HL[[ID]], IDlist = ID_ND_M_V, outline_V_HL, 
                                        refShape = 'target', imageList = imageList_ND_M_V_HL)
}

# masking/filtering extracted patterns part 2 (with mask)
for(e in 1:length(raster_ND_M_D_HR)){
  ID <- names(raster_ND_M_D_HR)[[e]]
  raster_ND_M_D_HR[[ID]] <- maskOutline(raster_ND_M_D_HR[[ID]], IDlist = ID_ND_M_D, mask_D_HR, 
                                        refShape = 'target', imageList = imageList_ND_M_D_HR)
}

for(e in 1:length(raster_ND_M_D_HL)){
  ID <- names(raster_ND_M_D_HL)[[e]]
  raster_ND_M_D_HL[[ID]] <- maskOutline(raster_ND_M_D_HL[[ID]], IDlist = ID_ND_M_D, mask_D_HL, 
                                        refShape = 'target', imageList = imageList_ND_M_D_HL)
}

for(e in 1:length(raster_ND_M_V_FR)){
  ID <- names(raster_ND_M_V_FR)[[e]]
  raster_ND_M_V_FR[[ID]] <- maskOutline(raster_ND_M_V_FR[[ID]], IDlist = ID_ND_M_V, mask_V_FR, 
                                        refShape = 'target', imageList = imageList_ND_M_V_FR)
}

for(e in 1:length(raster_ND_M_V_FL)){
  ID <- names(raster_ND_M_V_FL)[[e]]
  raster_ND_M_V_FL[[ID]] <- maskOutline(raster_ND_M_V_FL[[ID]], IDlist = ID_ND_M_V, mask_V_FL, 
                                        refShape = 'target', imageList = imageList_ND_M_V_FL)
}

for(e in 1:length(raster_ND_M_V_HR)){
  ID <- names(raster_ND_M_V_HR)[[e]]
  raster_ND_M_V_HR[[ID]] <- maskOutline(raster_ND_M_V_HR[[ID]], IDlist = ID_ND_M_V, mask_V_HR, 
                                        refShape = 'target', imageList = imageList_ND_M_V_HR)
}

for(e in 1:length(raster_ND_M_V_HL)){
  ID <- names(raster_ND_M_V_HL)[[e]]
  raster_ND_M_V_HL[[ID]] <- maskOutline(raster_ND_M_V_HL[[ID]], IDlist = ID_ND_M_V, mask_V_HL, 
                                        refShape = 'target', imageList = imageList_ND_M_V_HL)
}


# plotting patterns
sumraster_ND_M_D_FR <- sumRaster(raster_ND_M_D_FR, ID_ND_M_D, type = 'RGB')
colfunc <- hsv(1, 1, seq(0,1,length.out = 100))
png(file = "Plots/plot_heatmap_ND_M_D_FR.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sumraster_ND_M_D_FR, ID_ND_M_D, 
         imageList = imageList_ND_M_D_FR, landList = lanList_ND_M_D_FR, 
         refShape = 'target', crop = c(0,0,0,0),
         flipRaster = 'y', flipOutline = 'y',
         outline = outline_D_FR, colpalette = colfunc, 
         plotCartoon = TRUE,  cartoonID = 'KO-M-D-6H', refImage = imageList_KO_M_D_FR[['KO-M-D-6H']],
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "proportion of individuals with colour", lines = lines_D_FR)
#dev.copy(png, file = "Plots/plot_heatmap_ND_M_D_FR.png",
#         width = 10, height = 6, units = "in", res = 1200)
dev.off()

sumraster_ND_M_D_FL <- sumRaster(raster_ND_M_D_FL, ID_ND_M_D, type = 'RGB')
colfunc <- hsv(1, 1, seq(0,1,length.out = 100))
png(file = "Plots/plot_heatmap_ND_M_D_FL.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sumraster_ND_M_D_FL, ID_ND_M_D, 
         imageList = imageList_ND_M_D_FL, landList = lanList_ND_M_D_FL, 
         refShape = 'target', crop = c(0,0,0,0),
         flipRaster = 'y', flipOutline = 'y',
         outline = outline_D_FL, colpalette = colfunc, 
         plotCartoon = TRUE,  cartoonID = 'KO-M-D-6H', refImage = imageList_KO_M_D_FL[['KO-M-D-6H']],
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "proportion of individuals with colour", lines = lines_D_FL)
#dev.copy(png, file = "Plots/plot_heatmap_ND_M_D_FL.png",
#         width = 10, height = 6, units = "in", res = 1200)
dev.off()

sumraster_ND_M_D_HR <- sumRaster(raster_ND_M_D_HR, ID_ND_M_D, type = 'RGB')
colfunc <- c('black', 'grey', 'khaki1')
png(file = "Plots/plot_heatmap_ND_M_D_HR.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sumraster_ND_M_D_HR, ID_ND_M_D, 
         imageList = imageList_ND_M_D_HR, landList = lanList_ND_M_D_HR, 
         refShape = 'target', crop = c(0,0,0,0),
         flipRaster = 'y', flipOutline = 'y',
         outline = outline_D_HR, colpalette = colfunc, 
         plotCartoon = TRUE,  cartoonID = 'KO-M-D-6H', refImage = imageList_KO_M_D_HR[['KO-M-D-6H']],
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "proportion of individuals with colour", lines = lines_D_HR)
#dev.copy(png, file = "Plots/plot_heatmap_ND_M_D_HR.png",
#         width = 10, height = 6, units = "in", res = 1200)
dev.off()

sumraster_ND_M_D_HL <- sumRaster(raster_ND_M_D_HL, ID_ND_M_D, type = 'RGB')
colfunc <- c('black', 'grey', 'khaki1')
png(file = "Plots/plot_heatmap_ND_M_D_HL.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sumraster_ND_M_D_HL, ID_ND_M_D, 
         imageList = imageList_ND_M_D_HL, landList = lanList_ND_M_D_HL, 
         refShape = 'target', crop = c(0,0,0,0),
         flipRaster = 'y', flipOutline = 'y',
         outline = outline_D_HL, colpalette = colfunc, 
         plotCartoon = TRUE,  cartoonID = 'KO-M-D-6H', refImage = imageList_KO_M_D_HL[['KO-M-D-6H']],
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "proportion of individuals with colour", lines = lines_D_HL)
#dev.copy(png, file = "Plots/plot_heatmap_ND_M_D_HL.png",
#         width = 10, height = 6, units = "in", res = 1200)
dev.off()

sumraster_ND_M_V_FR <- sumRaster(raster_ND_M_V_FR, ID_ND_M_V, type = 'RGB')
colfunc <- hsv(1, 1, seq(0,1,length.out = 100))
png(file = "Plots/plot_heatmap_ND_M_V_FR.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sumraster_ND_M_V_FR, ID_ND_M_V, 
         imageList = imageList_ND_M_V_FR, landList = lanList_ND_M_V_FR, 
         refShape = 'target', crop = c(0,0,0,0),
         flipRaster = 'y', flipOutline = 'y',
         outline = outline_V_FR, colpalette = colfunc, 
         plotCartoon = TRUE,  cartoonID = 'KO-M-V-6H', refImage = imageList_KO_M_V_FR[['KO-M-V-6H']],
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "proportion of individuals with colour", lines = lines_V_FR)
#dev.copy(png, file = "Plots/plot_heatmap_ND_M_V_FR.png",
#         width = 10, height = 6, units = "in", res = 1200)
dev.off()

sumraster_ND_M_V_FL <- sumRaster(raster_ND_M_V_FL, ID_ND_M_V, type = 'RGB')
colfunc <- hsv(1, 1, seq(0,1,length.out = 100))
png(file = "Plots/plot_heatmap_ND_M_V_FL.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sumraster_ND_M_V_FL, ID_ND_M_V, 
         imageList = imageList_ND_M_V_FL, landList = lanList_ND_M_V_FL, 
         refShape = 'target', crop = c(0,0,0,0),
         flipRaster = 'y', flipOutline = 'y',
         outline = outline_V_FL, colpalette = colfunc, 
         plotCartoon = TRUE,  cartoonID = 'KO-M-V-6H', refImage = imageList_KO_M_V_FL[['KO-M-V-6H']],
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "proportion of individuals with colour", lines = lines_V_FL)
#dev.copy(png, file = "Plots/plot_heatmap_ND_M_V_FL.png",
#         width = 10, height = 6, units = "in", res = 1200)
dev.off()

sumraster_ND_M_V_HR <- sumRaster(raster_ND_M_V_HR, ID_ND_M_V, type = 'RGB')
colfunc <- c('black', 'grey', 'khaki1')
png(file = "Plots/plot_heatmap_ND_M_V_HR.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sumraster_ND_M_V_HR, ID_ND_M_V, 
         imageList = imageList_ND_M_V_HR, landList = lanList_ND_M_V_HR, 
         refShape = 'target', crop = c(0,0,0,0),
         flipRaster = 'y', flipOutline = 'y',
         outline = outline_V_HR, colpalette = colfunc, 
         plotCartoon = TRUE,  cartoonID = 'KO-M-V-6H', refImage = imageList_KO_M_V_HR[['KO-M-V-6H']],
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "proportion of individuals with colour", lines = lines_V_HR)
#dev.copy(png, file = "Plots/plot_heatmap_ND_M_V_HR.png",
#         width = 10, height = 6, units = "in", res = 1200)
dev.off()

sumraster_ND_M_V_HL <- sumRaster(raster_ND_M_V_HL, ID_ND_M_V, type = 'RGB')
colfunc <- c('black', 'grey', 'khaki1')
png(file = "Plots/plot_heatmap_ND_M_V_HL.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sumraster_ND_M_V_HL, ID_ND_M_V, 
         imageList = imageList_ND_M_V_HL, landList = lanList_ND_M_V_HL, 
         refShape = 'target', crop = c(0,0,0,0),
         flipRaster = 'y', flipOutline = 'y',
         outline = outline_V_HL, colpalette = colfunc, 
         plotCartoon = TRUE,  cartoonID = 'KO-M-V-6H', refImage = imageList_KO_M_V_HL[['KO-M-V-6H']],
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "proportion of individuals with colour", lines = lines_V_HL)
#dev.copy(png, file = "Plots/plot_heatmap_ND_M_V_HL.png",
#         width = 10, height = 6, units = "in", res = 1200)
dev.off()



### ANALYSIS - SUBSTRACTING #####

sub_F_D_FR <- sumraster_KO_F_D_FR/length(ID_KO_F_D) - sumraster_ND_F_D_FR/length(ID_ND_F_D)
sub_F_D_FL <- sumraster_KO_F_D_FL/length(ID_KO_F_D) - sumraster_ND_F_D_FL/length(ID_ND_F_D)
sub_F_D_HR <- sumraster_KO_F_D_HR/length(ID_KO_F_D) - sumraster_ND_F_D_HR/length(ID_ND_F_D)
sub_F_D_HL <- sumraster_KO_F_D_HL/length(ID_KO_F_D) - sumraster_ND_F_D_HL/length(ID_ND_F_D)

sub_F_V_FR <- sumraster_KO_F_V_FR/length(ID_KO_F_V) - sumraster_ND_F_V_FR/length(ID_ND_F_V)
sub_F_V_FL <- sumraster_KO_F_V_FL/length(ID_KO_F_V) - sumraster_ND_F_V_FL/length(ID_ND_F_V)
sub_F_V_HR <- sumraster_KO_F_V_HR/length(ID_KO_F_V) - sumraster_ND_F_V_HR/length(ID_ND_F_V)
sub_F_V_HL <- sumraster_KO_F_V_HL/length(ID_KO_F_V) - sumraster_ND_F_V_HL/length(ID_ND_F_V)

sub_M_D_FR <- sumraster_KO_M_D_FR/length(ID_KO_M_D) - sumraster_ND_M_D_FR/length(ID_ND_M_D)
sub_M_D_FL <- sumraster_KO_M_D_FL/length(ID_KO_M_D) - sumraster_ND_M_D_FL/length(ID_ND_M_D)
sub_M_D_HR <- sumraster_KO_M_D_HR/length(ID_KO_M_D) - sumraster_ND_M_D_HR/length(ID_ND_M_D)
sub_M_D_HL <- sumraster_KO_M_D_HL/length(ID_KO_M_D) - sumraster_ND_M_D_HL/length(ID_ND_M_D)

sub_M_V_FR <- sumraster_KO_M_V_FR/length(ID_KO_M_V) - sumraster_ND_M_V_FR/length(ID_ND_M_V)
sub_M_V_FL <- sumraster_KO_M_V_FL/length(ID_KO_M_V) - sumraster_ND_M_V_FL/length(ID_ND_M_V)
sub_M_V_HR <- sumraster_KO_M_V_HR/length(ID_KO_M_V) - sumraster_ND_M_V_HR/length(ID_ND_M_V)
sub_M_V_HL <- sumraster_KO_M_V_HL/length(ID_KO_M_V) - sumraster_ND_M_V_HL/length(ID_ND_M_V)


colfunc <- c("blue", "#7AA6DC99", "black", 
             "#EFC00099", "yellow")

png(file = "Plots/plot_heatmap_substract_F_D_FR.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sub_F_D_FR, ID_KO_F_D, 
         imageList = imageList_KO_F_D_FR,
         refShape = 'target', crop = c(0,0,0,0), zlim=c(-1,1),
         flipRaster = 'y', flipOutline = 'y', adjustCoords = TRUE,
         outline = outline_D_FR, colpalette = colfunc, normalized = TRUE,
         plotCartoon = TRUE,  cartoonID = 'KO-M-D-6H', refImage = raster::stack(raster_KO_M_D_FR[['KO-M-D-6H']]),
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "ND <   > mKO", lines = lines_D_FR)
dev.off()

png(file = "Plots/plot_heatmap_substract_F_D_FL.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sub_F_D_FL, ID_KO_F_D, 
         imageList = imageList_KO_F_D_FL,
         refShape = 'target', crop = c(0,0,0,0), zlim=c(-1,1),
         flipRaster = 'y', flipOutline = 'y', adjustCoords = TRUE,
         outline = outline_D_FL, colpalette = colfunc, normalized = TRUE,
         plotCartoon = TRUE,  cartoonID = 'KO-M-D-6H', refImage = raster::stack(raster_KO_M_D_FL[['KO-M-D-6H']]),
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "ND <   > mKO", lines = lines_D_FL)
dev.off()

png(file = "Plots/plot_heatmap_substract_F_D_HR.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sub_F_D_HR, ID_KO_F_D, 
         imageList = imageList_KO_F_D_HR,
         refShape = 'target', crop = c(0,0,0,0), zlim=c(-1,1),
         flipRaster = 'y', flipOutline = 'y', adjustCoords = TRUE,
         outline = outline_D_HR, colpalette = colfunc, normalized = TRUE,
         plotCartoon = TRUE,  cartoonID = 'KO-M-D-6H', refImage = raster::stack(raster_KO_M_D_HR[['KO-M-D-6H']]),
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "ND <   > mKO", lines = lines_D_HR)
dev.off()

png(file = "Plots/plot_heatmap_substract_F_D_HL.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sub_F_D_HL, ID_KO_F_D, 
         imageList = imageList_KO_F_D_HL,
         refShape = 'target', crop = c(0,0,0,0), zlim=c(-1,1),
         flipRaster = 'y', flipOutline = 'y', adjustCoords = TRUE,
         outline = outline_D_HL, colpalette = colfunc, normalized = TRUE,
         plotCartoon = TRUE,  cartoonID = 'KO-M-D-6H', refImage = raster::stack(raster_KO_M_D_HL[['KO-M-D-6H']]),
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "ND <   > mKO", lines = lines_D_HL)
dev.off()

png(file = "Plots/plot_heatmap_substract_F_V_FR.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sub_F_V_FR, ID_KO_F_V, 
         imageList = imageList_KO_F_V_FR,
         refShape = 'target', crop = c(0,0,0,0), zlim=c(-1,1),
         flipRaster = 'y', flipOutline = 'y', adjustCoords = TRUE,
         outline = outline_V_FR, colpalette = colfunc, normalized = TRUE,
         plotCartoon = TRUE,  cartoonID = 'KO-M-V-6H', refImage = raster::stack(raster_KO_M_V_FR[['KO-M-V-6H']]),
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "ND <   > mKO", lines = lines_V_FR)
dev.off()

png(file = "Plots/plot_heatmap_substract_F_V_FL.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sub_F_V_FL, ID_KO_F_V, 
         imageList = imageList_KO_F_V_FL,
         refShape = 'target', crop = c(0,0,0,0), zlim=c(-1,1),
         flipRaster = 'y', flipOutline = 'y', adjustCoords = TRUE,
         outline = outline_V_FL, colpalette = colfunc, normalized = TRUE,
         plotCartoon = TRUE,  cartoonID = 'KO-M-V-6H', refImage = raster::stack(raster_KO_M_V_FL[['KO-M-V-6H']]),
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "ND <   > mKO", lines = lines_V_FL)
dev.off()

png(file = "Plots/plot_heatmap_substract_F_V_HR.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sub_F_V_HR, ID_KO_F_V, 
         imageList = imageList_KO_F_V_HR,
         refShape = 'target', crop = c(0,0,0,0), zlim=c(-1,1),
         flipRaster = 'y', flipOutline = 'y', adjustCoords = TRUE,
         outline = outline_V_HR, colpalette = colfunc, normalized = TRUE,
         plotCartoon = TRUE,  cartoonID = 'KO-M-V-6H', refImage = raster::stack(raster_KO_M_V_HR[['KO-M-V-6H']]),
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "ND <   > mKO", lines = lines_V_HR)
dev.off()

png(file = "Plots/plot_heatmap_substract_F_V_HL.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sub_F_V_HL, ID_KO_F_V, 
         imageList = imageList_KO_F_V_HL,
         refShape = 'target', crop = c(0,0,0,0), zlim=c(-1,1),
         flipRaster = 'y', flipOutline = 'y', adjustCoords = TRUE,
         outline = outline_V_HL, colpalette = colfunc, normalized = TRUE,
         plotCartoon = TRUE,  cartoonID = 'KO-M-V-6H', refImage = raster::stack(raster_KO_M_V_HL[['KO-M-V-6H']]),
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "ND <   > mKO", lines = lines_V_HL)
dev.off()


png(file = "Plots/plot_heatmap_substract_M_D_FR.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sub_M_D_FR, ID_KO_M_D, 
         imageList = imageList_KO_M_D_FR,
         refShape = 'target', crop = c(0,0,0,0), zlim=c(-1,1),
         flipRaster = 'y', flipOutline = 'y', adjustCoords = TRUE,
         outline = outline_D_FR, colpalette = colfunc, normalized = TRUE,
         plotCartoon = TRUE,  cartoonID = 'KO-M-D-6H', refImage = raster::stack(raster_KO_M_D_FR[['KO-M-D-6H']]),
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "ND <   > mKO", lines = lines_D_FR)
dev.off()

png(file = "Plots/plot_heatmap_substract_M_D_FL.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sub_M_D_FL, ID_KO_M_D, 
         imageList = imageList_KO_M_D_FL,
         refShape = 'target', crop = c(0,0,0,0), zlim=c(-1,1),
         flipRaster = 'y', flipOutline = 'y', adjustCoords = TRUE,
         outline = outline_D_FL, colpalette = colfunc, normalized = TRUE,
         plotCartoon = TRUE,  cartoonID = 'KO-M-D-6H', refImage = raster::stack(raster_KO_M_D_FL[['KO-M-D-6H']]),
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "ND <   > mKO", lines = lines_D_FL)
dev.off()

png(file = "Plots/plot_heatmap_substract_M_D_HR.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sub_M_D_HR, ID_KO_M_D, 
         imageList = imageList_KO_M_D_HR,
         refShape = 'target', crop = c(0,0,0,0), zlim=c(-1,1),
         flipRaster = 'y', flipOutline = 'y', adjustCoords = TRUE,
         outline = outline_D_HR, colpalette = colfunc, normalized = TRUE,
         plotCartoon = TRUE,  cartoonID = 'KO-M-D-6H', refImage = raster::stack(raster_KO_M_D_HR[['KO-M-D-6H']]),
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "ND <   > mKO", lines = lines_D_HR)
dev.off()

png(file = "Plots/plot_heatmap_substract_M_D_HL.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sub_M_D_HL, ID_KO_M_D, 
         imageList = imageList_KO_M_D_HL,
         refShape = 'target', crop = c(0,0,0,0), zlim=c(-1,1),
         flipRaster = 'y', flipOutline = 'y', adjustCoords = TRUE,
         outline = outline_D_HL, colpalette = colfunc, normalized = TRUE,
         plotCartoon = TRUE,  cartoonID = 'KO-M-D-6H', refImage = raster::stack(raster_KO_M_D_HL[['KO-M-D-6H']]),
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "ND <   > mKO", lines = lines_D_HL)
dev.off()

png(file = "Plots/plot_heatmap_substract_M_V_FR.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sub_M_V_FR, ID_KO_M_V, 
         imageList = imageList_KO_M_V_FR,
         refShape = 'target', crop = c(0,0,0,0), zlim=c(-1,1),
         flipRaster = 'y', flipOutline = 'y', adjustCoords = TRUE,
         outline = outline_V_FR, colpalette = colfunc, normalized = TRUE,
         plotCartoon = TRUE,  cartoonID = 'KO-M-V-6H', refImage = raster::stack(raster_KO_M_V_FR[['KO-M-V-6H']]),
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "ND <   > mKO", lines = lines_V_FR)
dev.off()

png(file = "Plots/plot_heatmap_substract_M_V_FL.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sub_M_V_FL, ID_KO_M_V, 
         imageList = imageList_KO_M_V_FL,
         refShape = 'target', crop = c(0,0,0,0), zlim=c(-1,1),
         flipRaster = 'y', flipOutline = 'y', adjustCoords = TRUE,
         outline = outline_V_FL, colpalette = colfunc, normalized = TRUE,
         plotCartoon = TRUE,  cartoonID = 'KO-M-V-6H', refImage = raster::stack(raster_KO_M_V_FL[['KO-M-V-6H']]),
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "ND <   > mKO", lines = lines_V_FL)
dev.off()

png(file = "Plots/plot_heatmap_substract_M_V_HR.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sub_M_V_HR, ID_KO_M_V, 
         imageList = imageList_KO_M_V_HR,
         refShape = 'target', crop = c(0,0,0,0), zlim=c(-1,1),
         flipRaster = 'y', flipOutline = 'y', adjustCoords = TRUE,
         outline = outline_V_HR, colpalette = colfunc, normalized = TRUE,
         plotCartoon = TRUE,  cartoonID = 'KO-M-V-6H', refImage = raster::stack(raster_KO_M_V_HR[['KO-M-V-6H']]),
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "ND <   > mKO", lines = lines_V_HR)
dev.off()

png(file = "Plots/plot_heatmap_substract_M_V_HL.png",
    width = 10, height = 6, units = "in", res = 1200)
plotHeat(sub_M_V_HL, ID_KO_M_V, 
         imageList = imageList_KO_M_V_HL,
         refShape = 'target', crop = c(0,0,0,0), zlim=c(-1,1),
         flipRaster = 'y', flipOutline = 'y', adjustCoords = TRUE,
         outline = outline_V_HL, colpalette = colfunc, normalized = TRUE,
         plotCartoon = TRUE,  cartoonID = 'KO-M-V-6H', refImage = raster::stack(raster_KO_M_V_HL[['KO-M-V-6H']]),
         cartoonFill = 'black', cartoonOrder = 'under', 
         legendTitle = "ND <   > mKO", lines = lines_V_HL)
dev.off()


### ANALYSIS - CALCULATING PATTERN AREA #####

area_KO_F_D_FR <- patArea(raster_KO_F_D_FR, ID_KO_F_D, refShape = 'target', type = 'RGB', 
                          outline = outline_D_FR, crop = c(0,0,0,0), adjustCoords = TRUE, 
                          imageList = imageList_KO_F_D_FR, cartoonID = 'KO-M-D-6H', flipRaster = 'y', flipOutline = 'y')

area_KO_F_D_FL <- patArea(raster_KO_F_D_FL, ID_KO_F_D, refShape = 'target', type = 'RGB', 
                          outline = outline_D_FL, crop = c(0,0,0,0), adjustCoords = TRUE, 
                          imageList = imageList_KO_F_D_FL, cartoonID = 'KO-M-D-6H', flipRaster = 'y', flipOutline = 'y')

area_KO_F_D_HR <- patArea(raster_KO_F_D_HR, ID_KO_F_D, refShape = 'target', type = 'RGB', 
                          outline = outline_D_HR, crop = c(0,0,0,0), adjustCoords = TRUE, 
                          imageList = imageList_KO_F_D_HR, cartoonID = 'KO-M-D-6H', flipRaster = 'y', flipOutline = 'y')

area_KO_F_D_HL <- patArea(raster_KO_F_D_HL, ID_KO_F_D, refShape = 'target', type = 'RGB', 
                          outline = outline_D_HL, crop = c(0,0,0,0), adjustCoords = TRUE, 
                          imageList = imageList_KO_F_D_HL, cartoonID = 'KO-M-D-6H', flipRaster = 'y', flipOutline = 'y')

area_KO_F_V_FR <- patArea(raster_KO_F_V_FR, ID_KO_F_V, refShape = 'target', type = 'RGB', 
                          outline = outline_V_FR, crop = c(0,0,0,0), adjustCoords = TRUE, 
                          imageList = imageList_KO_F_V_FR, cartoonID = 'KO-M-V-6H', flipRaster = 'y', flipOutline = 'y')

area_KO_F_V_FL <- patArea(raster_KO_F_V_FL, ID_KO_F_V, refShape = 'target', type = 'RGB', 
                          outline = outline_V_FL, crop = c(0,0,0,0), adjustCoords = TRUE, 
                          imageList = imageList_KO_F_V_FL, cartoonID = 'KO-M-V-6H', flipRaster = 'y', flipOutline = 'y')

area_KO_F_V_HR <- patArea(raster_KO_F_V_HR, ID_KO_F_V, refShape = 'target', type = 'RGB', 
                          outline = outline_V_HR, crop = c(0,0,0,0), adjustCoords = TRUE, 
                          imageList = imageList_KO_F_V_HR, cartoonID = 'KO-M-V-6H', flipRaster = 'y', flipOutline = 'y')

area_KO_F_V_HL <- patArea(raster_KO_F_V_HL, ID_KO_F_V, refShape = 'target', type = 'RGB', 
                          outline = outline_V_HL, crop = c(0,0,0,0), adjustCoords = TRUE, 
                          imageList = imageList_KO_F_V_HL, cartoonID = 'KO-M-V-6H', flipRaster = 'y', flipOutline = 'y')

area_KO_M_D_FR <- patArea(raster_KO_M_D_FR, ID_KO_M_D, refShape = 'target', type = 'RGB', 
                          outline = outline_D_FR, crop = c(0,0,0,0), adjustCoords = TRUE, 
                          imageList = imageList_KO_M_D_FR, cartoonID = 'KO-M-D-6H', flipRaster = 'y', flipOutline = 'y')

area_KO_M_D_FL <- patArea(raster_KO_M_D_FL, ID_KO_M_D, refShape = 'target', type = 'RGB', 
                          outline = outline_D_FL, crop = c(0,0,0,0), adjustCoords = TRUE, 
                          imageList = imageList_KO_M_D_FL, cartoonID = 'KO-M-D-6H', flipRaster = 'y', flipOutline = 'y')

area_KO_M_D_HR <- patArea(raster_KO_M_D_HR, ID_KO_M_D, refShape = 'target', type = 'RGB', 
                          outline = outline_D_HR, crop = c(0,0,0,0), adjustCoords = TRUE, 
                          imageList = imageList_KO_M_D_HR, cartoonID = 'KO-M-D-6H', flipRaster = 'y', flipOutline = 'y')

area_KO_M_D_HL <- patArea(raster_KO_M_D_HL, ID_KO_M_D, refShape = 'target', type = 'RGB', 
                          outline = outline_D_HL, crop = c(0,0,0,0), adjustCoords = TRUE, 
                          imageList = imageList_KO_M_D_HL, cartoonID = 'KO-M-D-6H', flipRaster = 'y', flipOutline = 'y')

area_KO_M_V_FR <- patArea(raster_KO_M_V_FR, ID_KO_M_V, refShape = 'target', type = 'RGB', 
                          outline = outline_V_FR, crop = c(0,0,0,0), adjustCoords = TRUE, 
                          imageList = imageList_KO_M_V_FR, cartoonID = 'KO-M-V-6H', flipRaster = 'y', flipOutline = 'y')

area_KO_M_V_FL <- patArea(raster_KO_M_V_FL, ID_KO_M_V, refShape = 'target', type = 'RGB', 
                          outline = outline_V_FL, crop = c(0,0,0,0), adjustCoords = TRUE, 
                          imageList = imageList_KO_M_V_FL, cartoonID = 'KO-M-V-6H', flipRaster = 'y', flipOutline = 'y')

area_KO_M_V_HR <- patArea(raster_KO_M_V_HR, ID_KO_M_V, refShape = 'target', type = 'RGB', 
                          outline = outline_V_HR, crop = c(0,0,0,0), adjustCoords = TRUE, 
                          imageList = imageList_KO_M_V_HR, cartoonID = 'KO-M-V-6H', flipRaster = 'y', flipOutline = 'y')

area_KO_M_V_HL <- patArea(raster_KO_M_V_HL, ID_KO_M_V, refShape = 'target', type = 'RGB', 
                          outline = outline_V_HL, crop = c(0,0,0,0), adjustCoords = TRUE, 
                          imageList = imageList_KO_M_V_HL, cartoonID = 'KO-M-V-6H', flipRaster = 'y', flipOutline = 'y')


area_ND_F_D_FR <- patArea(raster_ND_F_D_FR, ID_ND_F_D, refShape = 'target', type = 'RGB', 
                          outline = outline_D_FR, crop = c(0,0,0,0), adjustCoords = TRUE, 
                          imageList = imageList_ND_F_D_FR, cartoonID = 'KO-M-D-6H', flipRaster = 'y', flipOutline = 'y')

area_ND_F_D_FL <- patArea(raster_ND_F_D_FL, ID_ND_F_D, refShape = 'target', type = 'RGB', 
                          outline = outline_D_FL, crop = c(0,0,0,0), adjustCoords = TRUE, 
                          imageList = imageList_ND_F_D_FL, cartoonID = 'KO-M-D-6H', flipRaster = 'y', flipOutline = 'y')

area_ND_F_D_HR <- patArea(raster_ND_F_D_HR, ID_ND_F_D, refShape = 'target', type = 'RGB', 
                          outline = outline_D_HR, crop = c(0,0,0,0), adjustCoords = TRUE, 
                          imageList = imageList_ND_F_D_HR, cartoonID = 'KO-M-D-6H', flipRaster = 'y', flipOutline = 'y')

area_ND_F_D_HL <- patArea(raster_ND_F_D_HL, ID_ND_F_D, refShape = 'target', type = 'RGB', 
                          outline = outline_D_HL, crop = c(0,0,0,0), adjustCoords = TRUE, 
                          imageList = imageList_ND_F_D_HL, cartoonID = 'KO-M-D-6H', flipRaster = 'y', flipOutline = 'y')

area_ND_F_V_FR <- patArea(raster_ND_F_V_FR, ID_ND_F_V, refShape = 'target', type = 'RGB', 
                          outline = outline_V_FR, crop = c(0,0,0,0), adjustCoords = TRUE, 
                          imageList = imageList_ND_F_V_FR, cartoonID = 'KO-M-V-6H', flipRaster = 'y', flipOutline = 'y')

area_ND_F_V_FL <- patArea(raster_ND_F_V_FL, ID_ND_F_V, refShape = 'target', type = 'RGB', 
                          outline = outline_V_FL, crop = c(0,0,0,0), adjustCoords = TRUE, 
                          imageList = imageList_ND_F_V_FL, cartoonID = 'KO-M-V-6H', flipRaster = 'y', flipOutline = 'y')

area_ND_F_V_HR <- patArea(raster_ND_F_V_HR, ID_ND_F_V, refShape = 'target', type = 'RGB', 
                          outline = outline_V_HR, crop = c(0,0,0,0), adjustCoords = TRUE, 
                          imageList = imageList_ND_F_V_HR, cartoonID = 'KO-M-V-6H', flipRaster = 'y', flipOutline = 'y')

area_ND_F_V_HL <- patArea(raster_ND_F_V_HL, ID_ND_F_V, refShape = 'target', type = 'RGB', 
                          outline = outline_V_HL, crop = c(0,0,0,0), adjustCoords = TRUE, 
                          imageList = imageList_ND_F_V_HL, cartoonID = 'KO-M-V-6H', flipRaster = 'y', flipOutline = 'y')

area_ND_M_D_FR <- patArea(raster_ND_M_D_FR, ID_ND_M_D, refShape = 'target', type = 'RGB', 
                          outline = outline_D_FR, crop = c(0,0,0,0), adjustCoords = TRUE, 
                          imageList = imageList_ND_M_D_FR, cartoonID = 'KO-M-D-6H', flipRaster = 'y', flipOutline = 'y')

area_ND_M_D_FL <- patArea(raster_ND_M_D_FL, ID_ND_M_D, refShape = 'target', type = 'RGB', 
                          outline = outline_D_FL, crop = c(0,0,0,0), adjustCoords = TRUE, 
                          imageList = imageList_ND_M_D_FL, cartoonID = 'KO-M-D-6H', flipRaster = 'y', flipOutline = 'y')

area_ND_M_D_HR <- patArea(raster_ND_M_D_HR, ID_ND_M_D, refShape = 'target', type = 'RGB', 
                          outline = outline_D_HR, crop = c(0,0,0,0), adjustCoords = TRUE, 
                          imageList = imageList_ND_M_D_HR, cartoonID = 'KO-M-D-6H', flipRaster = 'y', flipOutline = 'y')

area_ND_M_D_HL <- patArea(raster_ND_M_D_HL, ID_ND_M_D, refShape = 'target', type = 'RGB', 
                          outline = outline_D_HL, crop = c(0,0,0,0), adjustCoords = TRUE, 
                          imageList = imageList_ND_M_D_HL, cartoonID = 'KO-M-D-6H', flipRaster = 'y', flipOutline = 'y')

area_ND_M_V_FR <- patArea(raster_ND_M_V_FR, ID_ND_M_V, refShape = 'target', type = 'RGB', 
                          outline = outline_V_FR, crop = c(0,0,0,0), adjustCoords = TRUE, 
                          imageList = imageList_ND_M_V_FR, cartoonID = 'KO-M-V-6H', flipRaster = 'y', flipOutline = 'y')

area_ND_M_V_FL <- patArea(raster_ND_M_V_FL, ID_ND_M_V, refShape = 'target', type = 'RGB', 
                          outline = outline_V_FL, crop = c(0,0,0,0), adjustCoords = TRUE, 
                          imageList = imageList_ND_M_V_FL, cartoonID = 'KO-M-V-6H', flipRaster = 'y', flipOutline = 'y')

area_ND_M_V_HR <- patArea(raster_ND_M_V_HR, ID_ND_M_V, refShape = 'target', type = 'RGB', 
                          outline = outline_V_HR, crop = c(0,0,0,0), adjustCoords = TRUE, 
                          imageList = imageList_ND_M_V_HR, cartoonID = 'KO-M-V-6H', flipRaster = 'y', flipOutline = 'y')

area_ND_M_V_HL <- patArea(raster_ND_M_V_HL, ID_ND_M_V, refShape = 'target', type = 'RGB', 
                          outline = outline_V_HL, crop = c(0,0,0,0), adjustCoords = TRUE, 
                          imageList = imageList_ND_M_V_HL, cartoonID = 'KO-M-V-6H', flipRaster = 'y', flipOutline = 'y')

area_KO_F_D_FR$Group <- 'KO-F'
area_KO_F_D_FL$Group <- 'KO-F'
area_KO_F_D_HR$Group <- 'KO-F'
area_KO_F_D_HL$Group <- 'KO-F'
area_KO_F_V_FR$Group <- 'KO-F'
area_KO_F_V_FL$Group <- 'KO-F'
area_KO_F_V_HR$Group <- 'KO-F'
area_KO_F_V_HL$Group <- 'KO-F'
area_KO_M_D_FR$Group <- 'KO-M'
area_KO_M_D_FL$Group <- 'KO-M'
area_KO_M_D_HR$Group <- 'KO-M'
area_KO_M_D_HL$Group <- 'KO-M'
area_KO_M_V_FR$Group <- 'KO-M'
area_KO_M_V_FL$Group <- 'KO-M'
area_KO_M_V_HR$Group <- 'KO-M'
area_KO_M_V_HL$Group <- 'KO-M'
area_ND_F_D_FR$Group <- 'ND-F'
area_ND_F_D_FL$Group <- 'ND-F'
area_ND_F_D_HR$Group <- 'ND-F'
area_ND_F_D_HL$Group <- 'ND-F'
area_ND_F_V_FR$Group <- 'ND-F'
area_ND_F_V_FL$Group <- 'ND-F'
area_ND_F_V_HR$Group <- 'ND-F'
area_ND_F_V_HL$Group <- 'ND-F'
area_ND_M_D_FR$Group <- 'ND-M'
area_ND_M_D_FL$Group <- 'ND-M'
area_ND_M_D_HR$Group <- 'ND-M'
area_ND_M_D_HL$Group <- 'ND-M'
area_ND_M_V_FR$Group <- 'ND-M'
area_ND_M_V_FL$Group <- 'ND-M'
area_ND_M_V_HR$Group <- 'ND-M'
area_ND_M_V_HL$Group <- 'ND-M'

area_KO_F_D_FR$Side <- 'Dorsal'
area_KO_F_D_FL$Side <- 'Dorsal'
area_KO_F_D_HR$Side <- 'Dorsal'
area_KO_F_D_HL$Side <- 'Dorsal'
area_KO_F_V_FR$Side <- 'Ventral'
area_KO_F_V_FL$Side <- 'Ventral'
area_KO_F_V_HR$Side <- 'Ventral'
area_KO_F_V_HL$Side <- 'Ventral'
area_KO_M_D_FR$Side <- 'Dorsal'
area_KO_M_D_FL$Side <- 'Dorsal'
area_KO_M_D_HR$Side <- 'Dorsal'
area_KO_M_D_HL$Side <- 'Dorsal'
area_KO_M_V_FR$Side <- 'Ventral'
area_KO_M_V_FL$Side <- 'Ventral'
area_KO_M_V_HR$Side <- 'Ventral'
area_KO_M_V_HL$Side <- 'Ventral'
area_ND_F_D_FR$Side <- 'Dorsal'
area_ND_F_D_FL$Side <- 'Dorsal'
area_ND_F_D_HR$Side <- 'Dorsal'
area_ND_F_D_HL$Side <- 'Dorsal'
area_ND_F_V_FR$Side <- 'Ventral'
area_ND_F_V_FL$Side <- 'Ventral'
area_ND_F_V_HR$Side <- 'Ventral'
area_ND_F_V_HL$Side <- 'Ventral'
area_ND_M_D_FR$Side <- 'Dorsal'
area_ND_M_D_FL$Side <- 'Dorsal'
area_ND_M_D_HR$Side <- 'Dorsal'
area_ND_M_D_HL$Side <- 'Dorsal'
area_ND_M_V_FR$Side <- 'Ventral'
area_ND_M_V_FL$Side <- 'Ventral'
area_ND_M_V_HR$Side <- 'Ventral'
area_ND_M_V_HL$Side <- 'Ventral'

area_KO_F_D_FR$Wing <- 'FR'
area_KO_F_D_FL$Wing <- 'FL'
area_KO_F_D_HR$Wing <- 'HR'
area_KO_F_D_HL$Wing <- 'HL'
area_KO_F_V_FR$Wing <- 'FR'
area_KO_F_V_FL$Wing <- 'FL'
area_KO_F_V_HR$Wing <- 'HR'
area_KO_F_V_HL$Wing <- 'HL'
area_KO_M_D_FR$Wing <- 'FR'
area_KO_M_D_FL$Wing <- 'FL'
area_KO_M_D_HR$Wing <- 'HR'
area_KO_M_D_HL$Wing <- 'HL'
area_KO_M_V_FR$Wing <- 'FR'
area_KO_M_V_FL$Wing <- 'FL'
area_KO_M_V_HR$Wing <- 'HR'
area_KO_M_V_HL$Wing <- 'HL'
area_ND_F_D_FR$Wing <- 'FR'
area_ND_F_D_FL$Wing <- 'FL'
area_ND_F_D_HR$Wing <- 'HR'
area_ND_F_D_HL$Wing <- 'HL'
area_ND_F_V_FR$Wing <- 'FR'
area_ND_F_V_FL$Wing <- 'FL'
area_ND_F_V_HR$Wing <- 'HR'
area_ND_F_V_HL$Wing <- 'HL'
area_ND_M_D_FR$Wing <- 'FR'
area_ND_M_D_FL$Wing <- 'FL'
area_ND_M_D_HR$Wing <- 'HR'
area_ND_M_D_HL$Wing <- 'HL'
area_ND_M_V_FR$Wing <- 'FR'
area_ND_M_V_FL$Wing <- 'FL'
area_ND_M_V_HR$Wing <- 'HR'
area_ND_M_V_HL$Wing <- 'HL'

area_all <- rbind(area_KO_F_D_FR, area_KO_F_D_FL, area_KO_F_D_HR, area_KO_F_D_HL, 
                       area_KO_F_V_FR, area_KO_F_V_FL, area_KO_F_V_HR, area_KO_F_V_HL, 
                       area_KO_M_D_FR, area_KO_M_D_FL, area_KO_M_D_HR, area_KO_M_D_HL, 
                       area_KO_M_V_FR, area_KO_M_V_FL, area_KO_M_V_HR, area_KO_M_V_HL, 
                       area_ND_F_D_FR, area_ND_F_D_FL, area_ND_F_D_HR, area_ND_F_D_HL, 
                       area_ND_F_V_FR, area_ND_F_V_FL, area_ND_F_V_HR, area_ND_F_V_HL, 
                       area_ND_M_D_FR, area_ND_M_D_FL, area_ND_M_D_HR, area_ND_M_D_HL, 
                       area_ND_M_V_FR, area_ND_M_V_FL, area_ND_M_V_HR, area_ND_M_V_HL)

write.csv(area_all, 'data/data_pattern_area.csv', row.names = F)
area_all$Group <- factor(area_all$Group, levels = c("ND-F", "KO-F", "ND-M", "KO-M"))

area_plot <- ggplot(area_all, aes(x = Area, fill = Group, colour = Group)) +
              facet_grid(Side ~ Wing) + geom_density(alpha = 0.5)
area_plot

area_all$side <- as.factor(area_all$Side)
area_all$side <- factor(area_all$Side, levels = c("Dorsal", "Ventral"))

area_all$Wing <- as.factor(area_all$Wing)
area_all$Wing <- factor(area_all$Wing, levels = c("FL", "FR", "HL", "HR"))

area_all$Group <- as.factor(area_all$Group)
area_all$Group <- factor(area_all$Group, levels = c("KO-F", "ND-F", "KO-M", "ND-M"))

wing.labs <- c("FL" = "Left forewing", 
               "FR" = "Right forewing",
               "HL" = "Left hindwing",
               "HR" = "Right hindwing")

group.labs <- c("ND-F" = "\u2640 ND", 
                "KO-F" = "\u2640 mKO", 
                "ND-M" = "\u2642 ND", 
                "KO-M" = "\u2642 mKO")

plot_area <- ggplot(area_all, aes(x = Area, y = Group)) + 
  facet_grid(Side ~ Wing, scales = "free", labeller = labeller(Wing = wing.labs)) + 
  stat_summary(aes(colour = Side, size = 2), 
               fun.min = function(z) {quantile(z,0.25)},
               fun.max = function(z) {quantile(z,0.75)},
               fun = median,
               geom = "pointrange", size = 0.6, position = position_dodge(0.5)) + 
  scale_y_discrete(labels = (text = group.labs)) + 
  scale_color_manual(values = c("grey30", "grey30")) + 
  xlab("\nRelative pattern area (mean \U00B1 CI)") + 
  ylab("") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        strip.background = element_blank(), 
        legend.position = "none")

plot_area

ggsave(filename = "Plots/plot_area1.png",
       width = 8, height = 4, device='png', dpi=1200)


plot_area <- ggplot(area_all, aes(x = Area, y = Group)) + 
  facet_grid(Side ~ Wing, scales = "free", labeller = labeller(Wing = wing.labs)) + 
  stat_summary(aes(colour = Side, size = 2), 
               fun.min = function(z) {quantile(z,0.25)},
               fun.max = function(z) {quantile(z,0.75)},
               fun = median,
               geom = "pointrange", size = 0.6, position = position_dodge(0.5)) + 
  scale_y_discrete(labels = (text = group.labs)) +
  scale_color_manual(values = c("grey30", "grey30")) + 
  xlab("\nRelative pattern area (mean \U00B1 CI)") + 
  ylab("") + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none")

plot_area

ggsave(filename = "Plots/plot_area2.png",
       width = 8, height = 4, device='png', dpi=1200)


plot_area <- ggplot(area_all, aes(x = Group, y = Area)) + 
  facet_grid(Wing ~ Side, scales = "free", labeller = labeller(Wing = wing.labs)) + 
  stat_summary(aes(colour = Side, size = 2), 
               fun.min = function(z) {quantile(z,0.25)},
               fun.max = function(z) {quantile(z,0.75)},
               fun = median,
               geom = "pointrange", size = 0.6, position = position_dodge(0.5)) + 
  scale_x_discrete(labels = (text = group.labs)) +
  scale_color_manual(values = c("gray39", "gray39")) + 
  ylab("Relative pattern area (mean \U00B1 CI)\n") + 
  xlab("") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5), 
        legend.position = "none",
        panel.background = element_rect(fill = "white", colour = "gray39"))

plot_area

ggsave(filename = "Plots/plot_area3.png",
       width = 4, height = 8, device='png', dpi=1200)


### ANALYSIS - CALCULATING PATTERN AREA DIFFERENCES #####

# see crispr_gwd.R

### ANALYSIS - PCA PLOTTING #####

# run edited patPCA function (to change pattern changes colour)
patPCA2 <- function (rList, popList, colList, symbolList = NULL, rListPredict = NULL, 
                     popListPredict = NULL, colListPredict = NULL, pcaListPredict = NULL, 
                     pcaPopListPredict = NULL, pcaColPredict = "red", symbolListPredict = NULL, 
                     plot = FALSE, plotType = "points", plotChanges = FALSE, 
                     PCx = 1, PCy = 2, plotCartoon = FALSE, refShape = NULL, 
                     outline = NULL, lines = NULL, landList = NULL, adjustCoords = FALSE, 
                     crop = c(0, 0, 0, 0), flipRaster = NULL, flipOutline = NULL, 
                     imageList = NULL, cartoonID = NULL, refImage = NULL, colpalette = NULL, 
                     normalized = NULL, cartoonOrder = "above", lineOrder = "above", 
                     cartoonCol = "gray", cartoonFill = NULL, plotLandmarks = FALSE, 
                     landCol = "black", zlim = c(-1, 1), legendTitle = "Predicted", 
                     xlab = "", ylab = "", main = "", ...) 
{
  print("making dataframe from rasters")
  for (r in 1:length(rList)) {
    rList[[r]][is.na(rList[[r]])] <- 0
    ras <- raster::as.data.frame(rList[[r]])
    colnames(ras) <- names(rList)[[r]]
    if (r == 1) {
      rasDF <- ras
    }
    else {
      rasDF <- cbind(rasDF, ras)
    }
  }
  groupCol <- c()
  for (p in 1:length(popList)) {
    for (ind in 1:length(popList[[p]])) {
      if (!is.null(symbolList)) {
        groupCol <- rbind(groupCol, c(popList[[p]][ind], 
                                      colList[p], symbolList[p]))
      }
      if (is.null(symbolList)) {
        groupCol <- rbind(groupCol, c(popList[[p]][ind], 
                                      colList[p]))
      }
    }
  }
  groupCol <- as.data.frame(groupCol)
  if (!is.null(symbolList)) {
    colnames(groupCol) <- c("sampleID", "col", "symbol")
  }
  if (is.null(symbolList)) {
    colnames(groupCol) <- c("sampleID", "col")
  }
  print("calculating prcomp")
  comp <- prcomp(t(rasDF))
  pcdata <- comp$x
  rotation <- comp$rotation
  summ <- summary(comp)
  xmin <- min(pcdata[, PCx])
  xmax <- max(pcdata[, PCx])
  ymin <- min(pcdata[, PCy])
  ymax <- max(pcdata[, PCy])
  if (!is.null(rListPredict)) {
    print("making dataframe for predict rasters")
    for (r in 1:length(rListPredict)) {
      rListPredict[[r]][is.na(rListPredict[[r]])] <- 0
      ras <- raster::as.data.frame(rListPredict[[r]])
      colnames(ras) <- names(rListPredict)[[r]]
      if (r == 1) {
        rasDFPredict <- ras
      }
      else {
        rasDFPredict <- cbind(rasDFPredict, ras)
      }
    }
    groupColPredict <- c()
    for (p in 1:length(popListPredict)) {
      for (ind in 1:length(popListPredict[[p]])) {
        if (!is.null(symbolListPredict)) {
          groupColPredict <- rbind(groupColPredict, 
                                   c(popListPredict[[p]][ind], colListPredict[p], 
                                     symbolListPredict[p]))
        }
        if (is.null(symbolListPredict)) {
          groupColPredict <- rbind(groupColPredict, 
                                   c(popListPredict[[p]][ind], colListPredict[p]))
        }
      }
    }
    groupColPredict <- as.data.frame(groupColPredict)
    if (!is.null(symbolListPredict)) {
      colnames(groupColPredict) <- c("sampleID", "col", 
                                     "symbol")
    }
    if (is.null(symbolListPredict)) {
      colnames(groupColPredict) <- c("sampleID", "col")
    }
    predicted <- as.data.frame(predict(comp, t(rasDFPredict)))
    xmin <- min(pcdata[, PCx], predicted[, PCx])
    xmax <- max(pcdata[, PCx], predicted[, PCx])
    ymin <- min(pcdata[, PCy], predicted[, PCy])
    ymax <- max(pcdata[, PCy], predicted[, PCy])
  }
  if (!is.null(pcaListPredict)) {
    points(pcaListPredict[, c(PCx, PCy)], col = pcaColPredict, 
           pch = pcaPopListPredict, cex = 3)
    xmin <- min(pcaListPredict[, PCx], xmin)
    xmax <- max(pcaListPredict[, PCx], xmax)
    ymin <- min(pcaListPredict[, PCy], ymin)
    ymax <- max(pcaListPredict[, PCy], ymax)
  }
  if (plot == TRUE) {
    if (plotChanges) {
      print("calculating changes")
      PCxmin <- min(pcdata[, PCx])
      PCxmax <- max(pcdata[, PCx])
      PCymin <- min(pcdata[, PCy])
      PCymax <- max(pcdata[, PCy])
      pc.vecMix <- rep(0, dim(pcdata)[1])
      pc.vecMix[PCx] <- PCxmin
      pc.vecMax <- rep(0, dim(pcdata)[1])
      pc.vecMax[PCx] <- PCxmax
      pc.vecMiy <- rep(0, dim(pcdata)[1])
      pc.vecMiy[PCy] <- PCymin
      pc.vecMay <- rep(0, dim(pcdata)[1])
      pc.vecMay[PCy] <- PCymax
      xMi <- pc.vecMix %*% t(rotation)
      xMa <- pc.vecMax %*% t(rotation)
      x2Mi <- t(matrix(xMi, ncol = dim(rList[[1]])[1], 
                       nrow = dim(rList[[1]])[2]))
      x2Ma <- t(matrix(xMa, ncol = dim(rList[[1]])[1], 
                       nrow = dim(rList[[1]])[2]))
      yMi <- pc.vecMiy %*% t(rotation)
      yMa <- pc.vecMay %*% t(rotation)
      y2Mi <- t(matrix(yMi, ncol = dim(rList[[1]])[1], 
                       nrow = dim(rList[[1]])[2]))
      y2Ma <- t(matrix(yMa, ncol = dim(rList[[1]])[1], 
                       nrow = dim(rList[[1]])[2]))
      mapMix <- raster::raster(x2Mi)
      mapMax <- raster::raster(x2Ma)
      mapMiy <- raster::raster(y2Mi)
      mapMay <- raster::raster(y2Ma)
      raster::extent(mapMix) <- raster::extent(rList[[1]])
      raster::extent(mapMax) <- raster::extent(rList[[1]])
      raster::extent(mapMiy) <- raster::extent(rList[[1]])
      raster::extent(mapMay) <- raster::extent(rList[[1]])
    }
    print("plotting")
    if (plotChanges) {
      mat <- matrix(c(4, 1, 1, 5, 1, 1, 6, 2, 3), 3, 3, 
                    byrow = TRUE)
      layout(mat, widths = c(1, 1, 1), heights = c(1, 
                                                   1, 1))
    }
    if (!plotChanges) {
      mat <- matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1), 3, 3, 
                    byrow = TRUE)
      layout(mat, widths = c(1, 1, 1), heights = c(1, 
                                                   1, 1))
    }
    if (plotType == "points" && is.null(symbolList)) {
      plot(comp$x[, c(PCx, PCy)], col = as.vector(groupCol$col), 
           pch = 20, xlim = c(xmin, xmax), ylim = c(ymin, 
                                                    ymax), xlab = paste("PC", PCx, " (", round(summ$importance[2, 
                                                                                                               PCx] * 100, 1), " %)"), ylab = paste("PC", 
                                                                                                                                                    PCy, " (", round(summ$importance[2, PCy] * 
                                                                                                                                                                       100, 1), " %)"), ...)
      if (!is.null(rListPredict)) {
        points(predicted[, c(PCx, PCy)], col = as.vector(groupColPredict$col), 
               pch = 20, cex = 3)
      }
    }
    if (plotType == "points" && !is.null(symbolList)) {
      plot(comp$x[, c(PCx, PCy)], col = as.vector(groupCol$col), 
           pch = as.numeric(as.vector(groupCol$symbol)), 
           xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
           xlab = paste("PC", PCx, " (", round(summ$importance[2, 
                                                               PCx] * 100, 1), " %)"), ylab = paste("PC", 
                                                                                                    PCy, " (", round(summ$importance[2, PCy] * 
                                                                                                                       100, 1), " %)"), ...)
      if (!is.null(rListPredict)) {
        points(predicted[, c(PCx, PCy)], col = as.vector(groupColPredict$col), 
               pch = as.numeric(as.vector(groupColPredict$symbol)), 
               cex = 3)
      }
      if (!is.null(pcaListPredict)) {
        points(pcaListPredict[, c(PCx, PCy)], col = pcaColPredict, 
               pch = pcaPopListPredict, cex = 3)
      }
    }
    if (plotType == "labels") {
      plot(comp$x[, c(PCx, PCy)], col = NA, pch = 19, 
           xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
           xlab = paste("PC", PCx, " (", round(summ$importance[2, 
                                                               PCx] * 100, 1), " %)"), ylab = paste("PC", 
                                                                                                    PCy, " (", round(summ$importance[2, PCy] * 
                                                                                                                       100, 1), " %)"))
      text(comp$x[, PCx], comp$x[, PCy], col = as.vector(groupCol$col), 
           as.character(groupCol$sampleID))
      if (!is.null(rListPredict)) {
        text(predicted[, c(PCx, PCy)], col = as.vector(groupColPredict$col), 
             as.character(groupColPredict$sampleID))
      }
    }
    if (plotChanges) {
      if (is.null(colpalette)) {
        colpalette <- c("#0073C299", "#7AA6DC99", "black", 
                        "#8F770099", "#EFC00099")
      }
      else {
        if (!is.vector(colpalette)) {
          stop("Specified color palette is not a vector")
        }
      }
      plotHeat(mapMix/max(abs(xMi)), rList, plotCartoon = plotCartoon, 
               refShape = refShape, outline = outline, lines = lines, 
               adjustCoords = adjustCoords, landList = landList, 
               crop = crop, flipRaster = flipRaster, flipOutline = flipOutline, 
               imageList = imageList, cartoonID = cartoonID, 
               colpalette = colpalette, normalized = normalized, 
               cartoonOrder = cartoonOrder, lineOrder = lineOrder, 
               cartoonCol = cartoonCol, cartoonFill = cartoonFill, 
               plotLandmarks = plotLandmarks, landCol = landCol, 
               zlim = zlim, xlab = xlab, ylab = ylab, main = main, 
               plotType = "PCA", refImage = refImage)
      mtext(paste("min PC", PCx, sep = " "), 1)
      plotHeat(mapMax/max(abs(xMa)), rList, plotCartoon = plotCartoon, 
               refShape = refShape, outline = outline, lines = lines, 
               adjustCoords = adjustCoords, landList = landList, 
               crop = crop, flipRaster = flipRaster, flipOutline = flipOutline, 
               imageList = imageList, cartoonID = cartoonID, 
               colpalette = colpalette, normalized = normalized, 
               cartoonOrder = cartoonOrder, lineOrder = lineOrder, 
               cartoonCol = cartoonCol, cartoonFill = cartoonFill, 
               plotLandmarks = plotLandmarks, landCol = landCol, 
               zlim = zlim, xlab = xlab, ylab = ylab, main = main, 
               plotType = "PCA", refImage = refImage)
      mtext(paste("max PC", PCx, sep = " "), 1)
      plotHeat(mapMay/max(abs(yMa)), rList, plotCartoon = plotCartoon, 
               refShape = refShape, outline = outline, lines = lines, 
               adjustCoords = adjustCoords, landList = landList, 
               crop = crop, flipRaster = flipRaster, flipOutline = flipOutline, 
               imageList = imageList, cartoonID = cartoonID, 
               colpalette = colpalette, normalized = normalized, 
               cartoonOrder = cartoonOrder, lineOrder = lineOrder, 
               cartoonCol = cartoonCol, cartoonFill = cartoonFill, 
               plotLandmarks = plotLandmarks, landCol = landCol, 
               zlim = zlim, xlab = xlab, ylab = ylab, main = main, 
               plotType = "PCA", refImage = refImage)
      mtext(paste("max PC", PCy, sep = " "), 2)
      plotHeat(mapMiy/max(abs(yMi)), rList, plotCartoon = plotCartoon, 
               refShape = refShape, outline = outline, lines = lines, 
               adjustCoords = adjustCoords, landList = landList, 
               crop = crop, flipRaster = flipRaster, flipOutline = flipOutline, 
               imageList = imageList, cartoonID = cartoonID, 
               colpalette = colpalette, normalized = normalized, 
               cartoonOrder = cartoonOrder, lineOrder = lineOrder, 
               cartoonCol = cartoonCol, cartoonFill = cartoonFill, 
               plotLandmarks = plotLandmarks, landCol = landCol, 
               zlim = zlim, xlab = xlab, ylab = ylab, main = main, 
               plotType = "PCA", refImage = refImage)
      mtext(paste("min PC", PCy, sep = " "), 2)
      colfunc <- colorRampPalette(colpalette)
      plot(NULL, xlim = c(0, 1), ylim = c(0, 1), type = "n", 
           axes = FALSE, xlab = "", ylab = "")
      plot(mapMay, col = colfunc(21), zlim = c(-1, 1), 
           legend.only = TRUE, legend.width = 5, horizontal = TRUE, 
           smallplot = c(0.3, 1, 0.5, 0.6), legend.args = list(text = legendTitle, 
                                                               side = 3, font = 2, line = 2.5, cex = 1))
    }
    return(list(t(rasDF), groupCol, comp))
  }
  else {
    return(comp)
  }
}


# individual wing pca plot
popList_D <- list(ID_KO_F_D, ID_KO_M_D, ID_ND_F_D, ID_ND_F_D)
colList_D <- c("grey", "grey", "black", "black")
symbolList_D <- c(16, 17, 16, 17)

popList_V <- list(ID_KO_F_V, ID_KO_M_V, ID_ND_F_V, ID_ND_F_V)
colList_V <- c("grey", "grey", "black", "black")
symbolList_V <- c(16, 17, 16, 17)

TotalList_D_FR <- c(raster_KO_F_D_FR, raster_KO_M_D_FR, 
                    raster_ND_F_D_FR, raster_ND_M_D_FR)

TotalList_D_FL <- c(raster_KO_F_D_FL, raster_KO_M_D_FL, 
                    raster_ND_F_D_FL, raster_ND_M_D_FL)

TotalList_D_HR <- c(raster_KO_F_D_HR, raster_KO_M_D_HR, 
                    raster_ND_F_D_HR, raster_ND_M_D_HR)

TotalList_D_HL <- c(raster_KO_F_D_HL, raster_KO_M_D_HL, 
                    raster_ND_F_D_HL, raster_ND_M_D_HL)

TotalList_V_FR <- c(raster_KO_F_V_FR, raster_KO_M_V_FR, 
                    raster_ND_F_V_FR, raster_ND_M_V_FR)

TotalList_V_FL <- c(raster_KO_F_V_FL, raster_KO_M_V_FL, 
                    raster_ND_F_V_FL, raster_ND_M_V_FL)

TotalList_V_HR <- c(raster_KO_F_V_HR, raster_KO_M_V_HR, 
                    raster_ND_F_V_HR, raster_ND_M_V_HR)

TotalList_V_HL <- c(raster_KO_F_V_HL, raster_KO_M_V_HL, 
                    raster_ND_F_V_HL, raster_ND_M_V_HL)


png(file = "Plots/plot_pca_D_FR.png",
    width = 7, height = 6, units = "in", res = 1200)
pcaOut_D_FR <- patPCA2(TotalList_D_FR, popList_D, colList_D, symbolList = symbolList_D, plot = TRUE, plotType = 'points', 
                       plotChanges = TRUE, PCx = 1, PCy = 2, plotCartoon = TRUE, refShape = 'target', 
                       outline = outline_D_FR, crop = c(0,0,0,0), flipRaster = 'y', flipOutline = 'y', 
                       imageList = imageList_KO_M_D_FR, cartoonID = 'KO-M-D-6H', normalized = TRUE, 
                       cartoonFill = 'black', cartoonOrder = 'under', lines = lines_D_FR,
                       refImage = raster::stack(raster_KO_M_D_FR[['KO-M-D-6H']]), legendTitle = 'PCA predicted presence of colour')
dev.off()

png(file = "Plots/plot_pca_D_FL.png",
    width = 7, height = 6, units = "in", res = 1200)
pcaOut_D_FL <- patPCA2(TotalList_D_FL, popList_D, colList_D, symbolList = symbolList_D, plot = TRUE, plotType = 'points', 
                       plotChanges = TRUE, PCx = 1, PCy = 2, plotCartoon = TRUE, refShape = 'target', 
                       outline = outline_D_FL, crop = c(0,0,0,0), flipRaster = 'y', flipOutline = 'y', 
                       imageList = imageList_KO_M_D_FL, cartoonID = 'KO-M-D-6H', normalized = TRUE, 
                       cartoonFill = 'black', cartoonOrder = 'under', lines = lines_D_FL,
                       refImage = raster::stack(raster_KO_M_D_FL[['KO-M-D-6H']]), legendTitle = 'PCA predicted presence of colour')
dev.off()

png(file = "Plots/plot_pca_D_HR.png",
    width = 7, height = 6, units = "in", res = 1200)
pcaOut_D_HR <- patPCA2(TotalList_D_HR, popList_D, colList_D, symbolList = symbolList_D, plot = TRUE, plotType = 'points', 
                       plotChanges = TRUE, PCx = 1, PCy = 2, plotCartoon = TRUE, refShape = 'target', 
                       outline = outline_D_HR, crop = c(0,0,0,0), flipRaster = 'y', flipOutline = 'y', 
                       imageList = imageList_KO_M_D_HR, cartoonID = 'KO-M-D-6H', normalized = TRUE, 
                       cartoonFill = 'black', cartoonOrder = 'under', lines = lines_D_HR,
                       refImage = raster::stack(raster_KO_M_D_HR[['KO-M-D-6H']]), legendTitle = 'PCA predicted presence of colour')
dev.off()

png(file = "Plots/plot_pca_D_HL.png",
    width = 7, height = 6, units = "in", res = 1200)
pcaOut_D_HL <- patPCA2(TotalList_D_HL, popList_D, colList_D, symbolList = symbolList_D, plot = TRUE, plotType = 'points', 
                       plotChanges = TRUE, PCx = 1, PCy = 2, plotCartoon = TRUE, refShape = 'target', 
                       outline = outline_D_HL, crop = c(0,0,0,0), flipRaster = 'y', flipOutline = 'y', 
                       imageList = imageList_KO_M_D_HL, cartoonID = 'KO-M-D-6H', normalized = TRUE, 
                       cartoonFill = 'black', cartoonOrder = 'under', lines = lines_D_HL,
                       refImage = raster::stack(raster_KO_M_D_HL[['KO-M-D-6H']]), legendTitle = 'PCA predicted presence of colour')
dev.off()


png(file = "Plots/plot_pca_V_FR.png",
    width = 7, height = 6, units = "in", res = 1200)
pcaOut_V_FR <- patPCA2(TotalList_V_FR, popList_V, colList_V, symbolList = symbolList_V, plot = TRUE, plotType = 'points', 
                       plotChanges = TRUE, PCx = 1, PCy = 2, plotCartoon = TRUE, refShape = 'target', 
                       outline = outline_V_FR, crop = c(0,0,0,0), flipRaster = 'y', flipOutline = 'y', 
                       imageList = imageList_KO_M_V_FR, cartoonID = 'KO-M-D-6H', normalized = TRUE, 
                       cartoonFill = 'black', cartoonOrder = 'under', lines = lines_V_FR,
                       refImage = raster::stack(raster_KO_M_V_FR[['KO-M-D-6H']]), legendTitle = 'PCA predicted presence of colour')
dev.off()

png(file = "Plots/plot_pca_V_FL.png",
    width = 7, height = 6, units = "in", res = 1200)
pcaOut_V_FL <- patPCA2(TotalList_V_FL, popList_V, colList_V, symbolList = symbolList_V, plot = TRUE, plotType = 'points', 
                       plotChanges = TRUE, PCx = 1, PCy = 2, plotCartoon = TRUE, refShape = 'target', 
                       outline = outline_V_FL, crop = c(0,0,0,0), flipRaster = 'y', flipOutline = 'y', 
                       imageList = imageList_KO_M_V_FL, cartoonID = 'KO-M-D-6H', normalized = TRUE, 
                       cartoonFill = 'black', cartoonOrder = 'under', lines = lines_V_FL,
                       refImage = raster::stack(raster_KO_M_V_FL[['KO-M-D-6H']]), legendTitle = 'PCA predicted presence of colour')
dev.off()

png(file = "Plots/plot_pca_V_HR.png",
    width = 7, height = 6, units = "in", res = 1200)
pcaOut_V_HR <- patPCA2(TotalList_V_HR, popList_V, colList_V, symbolList = symbolList_V, plot = TRUE, plotType = 'points', 
                       plotChanges = TRUE, PCx = 1, PCy = 2, plotCartoon = TRUE, refShape = 'target', 
                       outline = outline_V_HR, crop = c(0,0,0,0), flipRaster = 'y', flipOutline = 'y', 
                       imageList = imageList_KO_M_V_HR, cartoonID = 'KO-M-D-6H', normalized = TRUE, 
                       cartoonFill = 'black', cartoonOrder = 'under', lines = lines_V_HR,
                       refImage = raster::stack(raster_KO_M_V_HR[['KO-M-D-6H']]), legendTitle = 'PCA predicted presence of colour')
dev.off()

png(file = "Plots/plot_pca_V_HL.png",
    width = 7, height = 6, units = "in", res = 1200)
pcaOut_V_HL <- patPCA2(TotalList_V_HL, popList_V, colList_V, symbolList = symbolList_V, plot = TRUE, plotType = 'points', 
                       plotChanges = TRUE, PCx = 1, PCy = 2, plotCartoon = TRUE, refShape = 'target', 
                       outline = outline_V_HL, crop = c(0,0,0,0), flipRaster = 'y', flipOutline = 'y', 
                       imageList = imageList_KO_M_V_HL, cartoonID = 'KO-M-D-6H', normalized = TRUE, 
                       cartoonFill = 'black', cartoonOrder = 'under', lines = lines_V_HL,
                       refImage = raster::stack(raster_KO_M_V_HL[['KO-M-D-6H']]), legendTitle = 'PCA predicted presence of colour')
dev.off()


#combined wings pca plot
raster_D_FR <-  as.data.frame(pcaOut_D_FR[[1]])
raster_D_FL <-  as.data.frame(pcaOut_D_FL[[1]])
raster_D_HR <-  as.data.frame(pcaOut_D_HR[[1]])
raster_D_HL <-  as.data.frame(pcaOut_D_HL[[1]])
raster_V_FR <-  as.data.frame(pcaOut_V_FR[[1]])
raster_V_FL <-  as.data.frame(pcaOut_V_FL[[1]])
raster_V_HR <-  as.data.frame(pcaOut_V_HR[[1]])
raster_V_HL <-  as.data.frame(pcaOut_V_HL[[1]])

save(raster_D_FR, file = 'data/raster_D_FR.rda')
save(raster_D_FL, file = 'data/raster_D_FL.rda')
save(raster_D_HR, file = 'data/raster_D_HR.rda')
save(raster_D_HL, file = 'data/raster_D_HL.rda')
save(raster_V_FR, file = 'data/raster_V_FR.rda')
save(raster_V_FL, file = 'data/raster_V_FL.rda')
save(raster_V_HR, file = 'data/raster_V_HR.rda')
save(raster_V_HL, file = 'data/raster_V_HL.rda')

load('data/raster_D_FR.rda')
load('data/raster_D_FL.rda')
load('data/raster_D_HR.rda')
load('data/raster_D_HL.rda')
load('data/raster_V_FR.rda')
load('data/raster_V_FL.rda')
load('data/raster_V_HR.rda')
load('data/raster_V_HL.rda')

raster_D_comb <- cbind(raster_D_FL, raster_D_FR, raster_D_HL, raster_D_HR)
raster_V_comb <- cbind(raster_V_FR, raster_V_FL, raster_V_HR, raster_V_HL)

raster_id_D <- data.frame(rownames(raster_D_comb))
raster_id_V <- data.frame(rownames(raster_V_comb))
raster_id_D$group <- as.character(NA)
raster_id_V$group <- as.character(NA)
colnames(raster_id_D) <- c('id', 'group')
colnames(raster_id_V) <- c('id', 'group')

for(i in 1:nrow(raster_id_D)){
  if((raster_id_D[i,'id']) %in% ID_KO_F_D){
    raster_id_D[i,'group'] <- 'KO-F'
  }
  if((raster_id_D[i,'id']) %in% ID_KO_M_D){
    raster_id_D[i,'group'] <- 'KO-M'
  }
  if((raster_id_D[i,'id']) %in% ID_ND_F_D){
    raster_id_D[i,'group'] <- 'ND-F'
  }
  if((raster_id_D[i,'id']) %in% ID_ND_M_D){
    raster_id_D[i,'group'] <- 'ND-M'
  }
}

for(i in 1:nrow(raster_id_V)){
  if((raster_id_V[i,'id']) %in% ID_KO_F_V){
    raster_id_V[i,'group'] <- 'KO-F'
  }
  if((raster_id_V[i,'id']) %in% ID_KO_M_V){
    raster_id_V[i,'group'] <- 'KO-M'
  }
  if((raster_id_V[i,'id']) %in% ID_ND_F_V){
    raster_id_V[i,'group'] <- 'ND-F'
  }
  if((raster_id_V[i,'id']) %in% ID_ND_M_V){
    raster_id_V[i,'group'] <- 'ND-M'
  }
}

raster_D_combined <- cbind(raster_id_D, raster_D_comb)
raster_V_combined <- cbind(raster_id_V, raster_V_comb)
rm(raster_D_comb, raster_V_comb)

save(raster_D_combined, file = 'data/raster_D_combined.rda')
save(raster_V_combined, file = 'data/raster_V_combined.rda')

load('data/raster_D_combined.rda')
load('data/raster_V_combined.rda')

raster_D_combined$group <- factor(raster_D_combined$group, levels = c("KO-F", "KO-M", "ND-F", "ND-M"))
raster_V_combined$group <- factor(raster_V_combined$group, levels = c("KO-F", "KO-M", "ND-F", "ND-M"))

pcaOut_D_comb <- prcomp(raster_D_combined[, 3:360002], scale = FALSE)
pcaOut_V_comb <- prcomp(raster_V_combined[, 3:360002], scale = FALSE)

vis_pcaOut_D_comb <- fviz_pca_ind(pcaOut_D_comb,
                                  geom.ind = "point",
                                  col.ind = raster_D_combined$group,
                                  palette = c("grey", "grey", "black", "black"),
                                  addEllipses = TRUE,
                                  ellipse.type = "confidence",
                                  legend.title = "Groups",
                                  repel = TRUE,
                                  title = "",
                                  xlab = "\nPC1 13.39%", ylab = "PC2 7.46%\n") + 
  scale_shape_manual(values=c(16, 17, 16, 17))

vis_pcaOut_D_comb

eig_pcaOut_D_comb <- fviz_eig(pcaOut_D_comb)
eig_pcaOut_D_comb

geig_pcaOut_D_comb <- get_eig(pcaOut_D_comb)
geig_pcaOut_D_comb

dev.print(device = png,
          filename = "Plots/plot_pca_D_combined.png",
          width = 6, height = 4, units = "in", res = 1200)


vis_pcaOut_V_comb <- fviz_pca_ind(pcaOut_V_comb,
                                  geom.ind = "point",
                                  col.ind = raster_V_combined$group,
                                  palette = c("grey", "grey", "black", "black"),
                                  addEllipses = TRUE,
                                  ellipse.type = "confidence",
                                  legend.title = "Groups",
                                  repel = TRUE,
                                  title = "",
                                  xlab = "\nPC1 12.36%", ylab = "PC2 7.86%\n") + 
  scale_shape_manual(values=c(16, 17, 16, 17))

vis_pcaOut_V_comb

eig_pcaOut_V_comb <- fviz_eig(pcaOut_V_comb)
eig_pcaOut_V_comb

geig_pcaOut_V_comb <- get_eig(pcaOut_V_comb)
geig_pcaOut_V_comb

dev.print(device = png,
          filename = "Plots/plot_pca_V_combined.png",
          width = 6, height = 4, units = "in", res = 1200)


### EXTRA - LANDMARKS USED #####

# F_D_FR
landArray_F_D_FR <- lanArray(c(lanList_KO_F_D_FR, lanList_ND_F_D_FR))
transformed_F_D_FR <- Morpho::procSym(landArray_F_D_FR)

cartoonLandTrans_F_D_FR <- Morpho::computeTransform(transformed_F_D_FR$mshape, target_D_FR, type="tps")
outlineTrans_F_D_FR <- Morpho::applyTransform(as.matrix(outline_D_FR), cartoonLandTrans_F_D_FR)

XLIM_F_D_FR <- c(min(outlineTrans_F_D_FR[,1]),max(outlineTrans_F_D_FR[,1]))
YLIM_F_D_FR <- c(min(outlineTrans_F_D_FR[,2]),max(outlineTrans_F_D_FR[,2]))


lineList_F_D_FR <- list()
for(e in 1:length(lines_D_FR)){
  lineList_F_D_FR[[e]] <- read.table(lines_D_FR[e], header = FALSE)
}

cartoonLinesTrans_F_D_FR <- list()
for(e in 1:length(lines_D_FR)){
  cartoonLinesTrans_F_D_FR[[e]] <- Morpho::applyTransform(as.matrix(lineList_F_D_FR[[e]]), cartoonLandTrans_F_D_FR)
}


png(file = "Plots/plot_landmark_placement_F_D_FR.png",
    width = 10, height = 5, units = "in", res = 1200)

par(mar=c(0,2,0,0), oma=c(0,1,0,0), pty='m')
plot(NULL, xlim=XLIM_F_D_FR, ylim=YLIM_F_D_FR, asp = 1, xlab='', ylab='', xaxt='n', yaxt='n', axes=F)
polygon(outlineTrans_F_D_FR, col='white', border='gray60', xlim = XLIM_F_D_FR, ylim= YLIM_F_D_FR, asp=1)
for(e in 1:length(lineList_F_D_FR)){
  lines(cartoonLinesTrans_F_D_FR[[e]], col='gray60', xlim = XLIM_F_D_FR, ylim= YLIM_F_D_FR, asp=1)
}
#par(new=T)
#plot(transformed_F_D_FR$mshape[,1], transformed_F_D_FR$mshape[,2], pch=c(1,19,1,19,19,19,1,1,19,19,19,19,19,19,19,19,19,19), cex=2, xlim=XLIM_F_D_FR, ylim=YLIM_F_D_FR, asp=1, col = adjustcolor('red',alpha=0.3), xlab='', ylab='', xaxt='n', yaxt='n', axes=F)
#text(transformed_F_D_FR$mshape[,1], transformed_F_D_FR$mshape[,2], labels = c(1:18), cex = 2, pos = 2)
dev.off()


# F_D_FL
landArray_F_D_FL <- lanArray(c(lanList_KO_F_D_FL, lanList_ND_F_D_FL))
transformed_F_D_FL <- Morpho::procSym(landArray_F_D_FL)

cartoonLandTrans_F_D_FL <- Morpho::computeTransform(transformed_F_D_FL$mshape, target_D_FL, type="tps")
outlineTrans_F_D_FL <- Morpho::applyTransform(as.matrix(outline_D_FL), cartoonLandTrans_F_D_FL)

XLIM_F_D_FL <- c(min(outlineTrans_F_D_FL[,1]),max(outlineTrans_F_D_FL[,1]))
YLIM_F_D_FL <- c(min(outlineTrans_F_D_FL[,2]),max(outlineTrans_F_D_FL[,2]))


lineList_F_D_FL <- list()
for(e in 1:length(lines_D_FL)){
  lineList_F_D_FL[[e]] <- read.table(lines_D_FL[e], header = FALSE)
}

cartoonLinesTrans_F_D_FL <- list()
for(e in 1:length(lines_D_FL)){
  cartoonLinesTrans_F_D_FL[[e]] <- Morpho::applyTransform(as.matrix(lineList_F_D_FL[[e]]), cartoonLandTrans_F_D_FL)
}


png(file = "Plots/plot_landmark_placement_F_D_FL.png",
    width = 10, height = 5, units = "in", res = 1200)

par(mar=c(0,2,0,0), oma=c(0,1,0,0), pty='m')
plot(NULL, xlim=XLIM_F_D_FL, ylim=YLIM_F_D_FL, asp = 1, xlab='', ylab='', xaxt='n', yaxt='n', axes=F)
polygon(outlineTrans_F_D_FL, col='white', border='gray60', xlim = XLIM_F_D_FL, ylim= YLIM_F_D_FL, asp=1)
for(e in 1:length(lineList_F_D_FL)){
  lines(cartoonLinesTrans_F_D_FL[[e]], col='gray60', xlim = XLIM_F_D_FL, ylim= YLIM_F_D_FL, asp=1)
}
#par(new=T)
#plot(transformed_F_D_FL$mshape[,1], transformed_F_D_FL$mshape[,2], pch=c(1,19,1,19,19,19,1,1,19,19,19,19,19,19,19,19,19,19), cex=2, xlim=XLIM_F_D_FL, ylim=YLIM_F_D_FL, asp=1, col = adjustcolor('red',alpha=0.3), xlab='', ylab='', xaxt='n', yaxt='n', axes=F)
#text(transformed_F_D_FL$mshape[,1], transformed_F_D_FL$mshape[,2], labels = c(1:18), cex = 2, pos = 2)
dev.off()


# F_D_HR
landArray_F_D_HR <- lanArray(c(lanList_KO_F_D_HR, lanList_ND_F_D_HR))
transformed_F_D_HR <- Morpho::procSym(landArray_F_D_HR)

cartoonLandTrans_F_D_HR <- Morpho::computeTransform(transformed_F_D_HR$mshape, target_D_HR, type="tps")
outlineTrans_F_D_HR <- Morpho::applyTransform(as.matrix(outline_D_HR), cartoonLandTrans_F_D_HR)

XLIM_F_D_HR <- c(min(outlineTrans_F_D_HR[,1]),max(outlineTrans_F_D_HR[,1]))
YLIM_F_D_HR <- c(min(outlineTrans_F_D_HR[,2]),max(outlineTrans_F_D_HR[,2]))


lineList_F_D_HR <- list()
for(e in 1:length(lines_D_HR)){
  lineList_F_D_HR[[e]] <- read.table(lines_D_HR[e], header = FALSE)
}

cartoonLinesTrans_F_D_HR <- list()
for(e in 1:length(lines_D_HR)){
  cartoonLinesTrans_F_D_HR[[e]] <- Morpho::applyTransform(as.matrix(lineList_F_D_HR[[e]]), cartoonLandTrans_F_D_HR)
}


png(file = "Plots/plot_landmark_placement_F_D_HR.png",
    width = 10, height = 5, units = "in", res = 1200)

par(mar=c(0,2,0,0), oma=c(0,1,0,0), pty='m')
plot(NULL, xlim=XLIM_F_D_HR, ylim=YLIM_F_D_HR, asp = 1, xlab='', ylab='', xaxt='n', yaxt='n', axes=F)
polygon(outlineTrans_F_D_HR, col='white', border='gray60', xlim = XLIM_F_D_HR, ylim= YLIM_F_D_HR, asp=1)
for(e in 1:length(lineList_F_D_HR)){
  lines(cartoonLinesTrans_F_D_HR[[e]], col='gray60', xlim = XLIM_F_D_HR, ylim= YLIM_F_D_HR, asp=1)
}
#par(new=T)
#plot(transformed_F_D_HR$mshape[,1], transformed_F_D_HR$mshape[,2], pch=c(1,19,1,19,19,19,1,1,19,19,19,19,19,19,19,19,19,19), cex=2, xlim=XLIM_F_D_HR, ylim=YLIM_F_D_HR, asp=1, col = adjustcolor('red',alpha=0.3), xlab='', ylab='', xaxt='n', yaxt='n', axes=F)
#text(transformed_F_D_HR$mshape[,1], transformed_F_D_HR$mshape[,2], labels = c(1:18), cex = 2, pos = 2)
dev.off()


# F_D_HL
landArray_F_D_HL <- lanArray(c(lanList_KO_F_D_HL, lanList_ND_F_D_HL))
transformed_F_D_HL <- Morpho::procSym(landArray_F_D_HL)

cartoonLandTrans_F_D_HL <- Morpho::computeTransform(transformed_F_D_HL$mshape, target_D_HL, type="tps")
outlineTrans_F_D_HL <- Morpho::applyTransform(as.matrix(outline_D_HL), cartoonLandTrans_F_D_HL)

XLIM_F_D_HL <- c(min(outlineTrans_F_D_HL[,1]),max(outlineTrans_F_D_HL[,1]))
YLIM_F_D_HL <- c(min(outlineTrans_F_D_HL[,2]),max(outlineTrans_F_D_HL[,2]))


lineList_F_D_HL <- list()
for(e in 1:length(lines_D_HL)){
  lineList_F_D_HL[[e]] <- read.table(lines_D_HL[e], header = FALSE)
}

cartoonLinesTrans_F_D_HL <- list()
for(e in 1:length(lines_D_HL)){
  cartoonLinesTrans_F_D_HL[[e]] <- Morpho::applyTransform(as.matrix(lineList_F_D_HL[[e]]), cartoonLandTrans_F_D_HL)
}


png(file = "Plots/plot_landmark_placement_F_D_HL.png",
    width = 10, height = 5, units = "in", res = 1200)

par(mar=c(0,2,0,0), oma=c(0,1,0,0), pty='m')
plot(NULL, xlim=XLIM_F_D_HL, ylim=YLIM_F_D_HL, asp = 1, xlab='', ylab='', xaxt='n', yaxt='n', axes=F)
polygon(outlineTrans_F_D_HL, col='white', border='gray60', xlim = XLIM_F_D_HL, ylim= YLIM_F_D_HL, asp=1)
for(e in 1:length(lineList_F_D_HL)){
  lines(cartoonLinesTrans_F_D_HL[[e]], col='gray60', xlim = XLIM_F_D_HL, ylim= YLIM_F_D_HL, asp=1)
}
#par(new=T)
#plot(transformed_F_D_HL$mshape[,1], transformed_F_D_HL$mshape[,2], pch=c(1,19,1,19,19,19,1,1,19,19,19,19,19,19,19,19,19,19), cex=2, xlim=XLIM_F_D_HL, ylim=YLIM_F_D_HL, asp=1, col = adjustcolor('red',alpha=0.3), xlab='', ylab='', xaxt='n', yaxt='n', axes=F)
#text(transformed_F_D_HL$mshape[,1], transformed_F_D_HL$mshape[,2], labels = c(1:18), cex = 2, pos = 2)
dev.off()


# F_V_FR
landArray_F_V_FR <- lanArray(c(lanList_KO_F_V_FR, lanList_ND_F_V_FR))
transformed_F_V_FR <- Morpho::procSym(landArray_F_V_FR)

cartoonLandTrans_F_V_FR <- Morpho::computeTransform(transformed_F_V_FR$mshape, target_V_FR, type="tps")
outlineTrans_F_V_FR <- Morpho::applyTransform(as.matrix(outline_V_FR), cartoonLandTrans_F_V_FR)

XLIM_F_V_FR <- c(min(outlineTrans_F_V_FR[,1]),max(outlineTrans_F_V_FR[,1]))
YLIM_F_V_FR <- c(min(outlineTrans_F_V_FR[,2]),max(outlineTrans_F_V_FR[,2]))


lineList_F_V_FR <- list()
for(e in 1:length(lines_V_FR)){
  lineList_F_V_FR[[e]] <- read.table(lines_V_FR[e], header = FALSE)
}

cartoonLinesTrans_F_V_FR <- list()
for(e in 1:length(lines_V_FR)){
  cartoonLinesTrans_F_V_FR[[e]] <- Morpho::applyTransform(as.matrix(lineList_F_V_FR[[e]]), cartoonLandTrans_F_V_FR)
}


png(file = "Plots/plot_landmark_placement_F_V_FR.png",
    width = 10, height = 5, units = "in", res = 1200)

par(mar=c(0,2,0,0), oma=c(0,1,0,0), pty='m')
plot(NULL, xlim=XLIM_F_V_FR, ylim=YLIM_F_V_FR, asp = 1, xlab='', ylab='', xaxt='n', yaxt='n', axes=F)
polygon(outlineTrans_F_V_FR, col='white', border='gray60', xlim = XLIM_F_V_FR, ylim= YLIM_F_V_FR, asp=1)
for(e in 1:length(lineList_F_V_FR)){
  lines(cartoonLinesTrans_F_V_FR[[e]], col='gray60', xlim = XLIM_F_V_FR, ylim= YLIM_F_V_FR, asp=1)
}
#par(new=T)
#plot(transformed_F_V_FR$mshape[,1], transformed_F_V_FR$mshape[,2], pch=c(1,19,1,19,19,19,1,1,19,19,19,19,19,19,19,19,19,19), cex=2, xlim=XLIM_F_V_FR, ylim=YLIM_F_V_FR, asp=1, col = adjustcolor('red',alpha=0.3), xlab='', ylab='', xaxt='n', yaxt='n', axes=F)
#text(transformed_F_V_FR$mshape[,1], transformed_F_V_FR$mshape[,2], labels = c(1:18), cex = 2, pos = 2)
dev.off()


# F_V_FL
landArray_F_V_FL <- lanArray(c(lanList_KO_F_V_FL, lanList_ND_F_V_FL))
transformed_F_V_FL <- Morpho::procSym(landArray_F_V_FL)

cartoonLandTrans_F_V_FL <- Morpho::computeTransform(transformed_F_V_FL$mshape, target_V_FL, type="tps")
outlineTrans_F_V_FL <- Morpho::applyTransform(as.matrix(outline_V_FL), cartoonLandTrans_F_V_FL)

XLIM_F_V_FL <- c(min(outlineTrans_F_V_FL[,1]),max(outlineTrans_F_V_FL[,1]))
YLIM_F_V_FL <- c(min(outlineTrans_F_V_FL[,2]),max(outlineTrans_F_V_FL[,2]))


lineList_F_V_FL <- list()
for(e in 1:length(lines_V_FL)){
  lineList_F_V_FL[[e]] <- read.table(lines_V_FL[e], header = FALSE)
}

cartoonLinesTrans_F_V_FL <- list()
for(e in 1:length(lines_V_FL)){
  cartoonLinesTrans_F_V_FL[[e]] <- Morpho::applyTransform(as.matrix(lineList_F_V_FL[[e]]), cartoonLandTrans_F_V_FL)
}


png(file = "Plots/plot_landmark_placement_F_V_FL.png",
    width = 10, height = 5, units = "in", res = 1200)

par(mar=c(0,2,0,0), oma=c(0,1,0,0), pty='m')
plot(NULL, xlim=XLIM_F_V_FL, ylim=YLIM_F_V_FL, asp = 1, xlab='', ylab='', xaxt='n', yaxt='n', axes=F)
polygon(outlineTrans_F_V_FL, col='white', border='gray60', xlim = XLIM_F_V_FL, ylim= YLIM_F_V_FL, asp=1)
for(e in 1:length(lineList_F_V_FL)){
  lines(cartoonLinesTrans_F_V_FL[[e]], col='gray60', xlim = XLIM_F_V_FL, ylim= YLIM_F_V_FL, asp=1)
}
#par(new=T)
#plot(transformed_F_V_FL$mshape[,1], transformed_F_V_FL$mshape[,2], pch=c(1,19,1,19,19,19,1,1,19,19,19,19,19,19,19,19,19,19), cex=2, xlim=XLIM_F_V_FL, ylim=YLIM_F_V_FL, asp=1, col = adjustcolor('red',alpha=0.3), xlab='', ylab='', xaxt='n', yaxt='n', axes=F)
#text(transformed_F_V_FL$mshape[,1], transformed_F_V_FL$mshape[,2], labels = c(1:18), cex = 2, pos = 2)
dev.off()


# F_V_HR
landArray_F_V_HR <- lanArray(c(lanList_KO_F_V_HR, lanList_ND_F_V_HR))
transformed_F_V_HR <- Morpho::procSym(landArray_F_V_HR)

cartoonLandTrans_F_V_HR <- Morpho::computeTransform(transformed_F_V_HR$mshape, target_V_HR, type="tps")
outlineTrans_F_V_HR <- Morpho::applyTransform(as.matrix(outline_V_HR), cartoonLandTrans_F_V_HR)

XLIM_F_V_HR <- c(min(outlineTrans_F_V_HR[,1]),max(outlineTrans_F_V_HR[,1]))
YLIM_F_V_HR <- c(min(outlineTrans_F_V_HR[,2]),max(outlineTrans_F_V_HR[,2]))


lineList_F_V_HR <- list()
for(e in 1:length(lines_V_HR)){
  lineList_F_V_HR[[e]] <- read.table(lines_V_HR[e], header = FALSE)
}

cartoonLinesTrans_F_V_HR <- list()
for(e in 1:length(lines_V_HR)){
  cartoonLinesTrans_F_V_HR[[e]] <- Morpho::applyTransform(as.matrix(lineList_F_V_HR[[e]]), cartoonLandTrans_F_V_HR)
}


png(file = "Plots/plot_landmark_placement_F_V_HR.png",
    width = 10, height = 5, units = "in", res = 1200)

par(mar=c(0,2,0,0), oma=c(0,1,0,0), pty='m')
plot(NULL, xlim=XLIM_F_V_HR, ylim=YLIM_F_V_HR, asp = 1, xlab='', ylab='', xaxt='n', yaxt='n', axes=F)
polygon(outlineTrans_F_V_HR, col='white', border='gray60', xlim = XLIM_F_V_HR, ylim= YLIM_F_V_HR, asp=1)
for(e in 1:length(lineList_F_V_HR)){
  lines(cartoonLinesTrans_F_V_HR[[e]], col='gray60', xlim = XLIM_F_V_HR, ylim= YLIM_F_V_HR, asp=1)
}
#par(new=T)
#plot(transformed_F_V_HR$mshape[,1], transformed_F_V_HR$mshape[,2], pch=c(1,19,1,19,19,19,1,1,19,19,19,19,19,19,19,19,19,19), cex=2, xlim=XLIM_F_V_HR, ylim=YLIM_F_V_HR, asp=1, col = adjustcolor('red',alpha=0.3), xlab='', ylab='', xaxt='n', yaxt='n', axes=F)
#text(transformed_F_V_HR$mshape[,1], transformed_F_V_HR$mshape[,2], labels = c(1:18), cex = 2, pos = 2)
dev.off()


# F_V_HL
landArray_F_V_HL <- lanArray(c(lanList_KO_F_V_HL, lanList_ND_F_V_HL))
transformed_F_V_HL <- Morpho::procSym(landArray_F_V_HL)

cartoonLandTrans_F_V_HL <- Morpho::computeTransform(transformed_F_V_HL$mshape, target_V_HL, type="tps")
outlineTrans_F_V_HL <- Morpho::applyTransform(as.matrix(outline_V_HL), cartoonLandTrans_F_V_HL)

XLIM_F_V_HL <- c(min(outlineTrans_F_V_HL[,1]),max(outlineTrans_F_V_HL[,1]))
YLIM_F_V_HL <- c(min(outlineTrans_F_V_HL[,2]),max(outlineTrans_F_V_HL[,2]))


lineList_F_V_HL <- list()
for(e in 1:length(lines_V_HL)){
  lineList_F_V_HL[[e]] <- read.table(lines_V_HL[e], header = FALSE)
}

cartoonLinesTrans_F_V_HL <- list()
for(e in 1:length(lines_V_HL)){
  cartoonLinesTrans_F_V_HL[[e]] <- Morpho::applyTransform(as.matrix(lineList_F_V_HL[[e]]), cartoonLandTrans_F_V_HL)
}


png(file = "Plots/plot_landmark_placement_F_V_HL.png",
    width = 10, height = 5, units = "in", res = 1200)

par(mar=c(0,2,0,0), oma=c(0,1,0,0), pty='m')
plot(NULL, xlim=XLIM_F_V_HL, ylim=YLIM_F_V_HL, asp = 1, xlab='', ylab='', xaxt='n', yaxt='n', axes=F)
polygon(outlineTrans_F_V_HL, col='white', border='gray60', xlim = XLIM_F_V_HL, ylim= YLIM_F_V_HL, asp=1)
for(e in 1:length(lineList_F_V_HL)){
  lines(cartoonLinesTrans_F_V_HL[[e]], col='gray60', xlim = XLIM_F_V_HL, ylim= YLIM_F_V_HL, asp=1)
}
#par(new=T)
#plot(transformed_F_V_HL$mshape[,1], transformed_F_V_HL$mshape[,2], pch=c(1,19,1,19,19,19,1,1,19,19,19,19,19,19,19,19,19,19), cex=2, xlim=XLIM_F_V_HL, ylim=YLIM_F_V_HL, asp=1, col = adjustcolor('red',alpha=0.3), xlab='', ylab='', xaxt='n', yaxt='n', axes=F)
#text(transformed_F_V_HL$mshape[,1], transformed_F_V_HL$mshape[,2], labels = c(1:18), cex = 2, pos = 2)
dev.off()


### DAPC (extra) #####

dapc_D <- raster_D_combined[-c(1:2)]
rownames(dapc_D) <- c(1:nrow(dapc_D))
dapc_pops_D <- raster_D_combined[2]
dapc_D_clean <- dapc_D[ , which(apply(dapc_D, 2, var) != 0)]

dapcOut_D <- dapc(dapc_D_clean, dapc_pops_D$group,
                  var.contrib = TRUE, scale = TRUE,
                  n.pca = 80, n.da = 4 )
dapcOut_D

myCol <- c("grey", "grey", "black", "black")
png("Plots/plot_dapc_12_D_combined.png", width=7, height = 5, units = "in",  res = 600)
scatter.dapc(dapcOut_D, xax = 1, yax = 2,  clab=0.75, col=myCol, scree.da = FALSE )
par(xpd = TRUE)

myInset <- function(){
  temp_DAPC <- dapcOut_D$eig
  temp_DAPC <- (temp_DAPC)/sum(temp_DAPC)
  plot(temp_DAPC, col=(c("black","black","gray70","gray70","gray70","gray70","gray70")), ylim=c(0,0.6),xlim=c(0.5,10),
       xlab="DA Eigenvalues", ylab="Prop of Var",
       cex=1, pch=20, type="h", lwd=15, lend = 1, mgp=c(1.5,0.5,0))
}

axis(1, at = c(-15,-10,-5,5,10,15) ,labels = TRUE, tick = TRUE, pos = 0, mgp=c(3,0.75,0))
axis(2, at = c(-10,-5,5,10,15),labels = TRUE, tick = TRUE, pos = 0, las=2, mgp=c(3,0.75,0))
add.scatter(myInset(), posi="topleft",
            inset=c(-0.01,-0.09), ratio=.25,
            bg=transp("white"))
dev.off()


png("Plots/plot_dapc_23_D_combined.png", width=7, height = 5, units = "in",  res = 600)
scatter.dapc(dapcOut_D, xax = 2, yax = 3,  clab=0.75, col=myCol, scree.da = FALSE )
par(xpd = TRUE)

myInset <- function(){
  temp_DAPC <- dapcOut_D$eig
  temp_DAPC <- (temp_DAPC)/sum(temp_DAPC)
  plot(temp_DAPC, col=(c("black","black","gray70","gray70","gray70","gray70","gray70")), ylim=c(0,0.6),xlim=c(0.5,10),
       xlab="DA Eigenvalues", ylab="Prop of Var",
       cex=1, pch=20, type="h", lwd=15, lend = 1, mgp=c(1.5,0.5,0))
}

axis(1, at = c(-15,-10,-5,5,10,15) ,labels = TRUE, tick = TRUE, pos = 0, mgp=c(3,0.75,0))
axis(2, at = c(-10,-5,5,10,15),labels = TRUE, tick = TRUE, pos = 0, las=2, mgp=c(3,0.75,0))
add.scatter(myInset(), posi="topright",
            inset=c(-0.01,-0.09), ratio=.25,
            bg=transp("white"))
dev.off()


dapc_V <- raster_V_combined[-c(1:2)]
rownames(dapc_V) <- c(1:nrow(dapc_V))
dapc_pops_V <- raster_V_combined[2]
dapc_V_clean <- dapc_V[ , which(apply(dapc_V, 2, var) != 0)]

dapcOut_V <- dapc(dapc_V_clean, dapc_pops_V$group,
                  var.contrib = TRUE, scale = TRUE,
                  n.pca = 80, n.da = 4 )
dapcOut_V

myCol <- c("grey", "grey", "black", "black")
png("Plots/plot_dapc_12_V_combined.png", width=7, height = 5, units = "in",  res = 600)
scatter.dapc(dapcOut_V, xax = 1, yax = 2,  clab=0.75, col=myCol, scree.da = FALSE )
par(xpd = TRUE)

myInset <- function(){
  temp_DAPC <- dapcOut_V$eig
  temp_DAPC <- (temp_DAPC)/sum(temp_DAPC)
  plot(temp_DAPC, col=(c("black","black","gray70","gray70","gray70","gray70","gray70")), ylim=c(0,0.8),xlim=c(0.5,10),
       xlab="DA Eigenvalues", ylab="Prop of Var",
       cex=1, pch=20, type="h", lwd=15, lend = 1, mgp=c(1.5,0.5,0))
}

axis(1, at = c(-15,-10,-5,5,10,15) ,labels = TRUE, tick = TRUE, pos = 0, mgp=c(3,0.75,0))
axis(2, at = c(-10,-5,5,10,15),labels = TRUE, tick = TRUE, pos = 0, las=2, mgp=c(3,0.75,0))
add.scatter(myInset(), posi="topright",
            inset=c(-0.01,-0.09), ratio=.25,
            bg=transp("white"))
dev.off()


png("Plots/plot_dapc_23_V_combined.png", width=7, height = 5, units = "in",  res = 600)
scatter.dapc(dapcOut_V, xax = 2, yax = 3,  clab=0.75, col=myCol, scree.da = FALSE )
par(xpd = TRUE)

myInset <- function(){
  temp_DAPC <- dapcOut_V$eig
  temp_DAPC <- (temp_DAPC)/sum(temp_DAPC)
  plot(temp_DAPC, col=(c("black","black","gray70","gray70","gray70","gray70","gray70")), ylim=c(0,0.8),xlim=c(0.5,10),
       xlab="DA Eigenvalues", ylab="Prop of Var",
       cex=1, pch=20, type="h", lwd=15, lend = 1, mgp=c(1.5,0.5,0))
}

axis(1, at = c(-15,-10,-5,5,10,15) ,labels = TRUE, tick = TRUE, pos = 0, mgp=c(3,0.75,0))
axis(2, at = c(-10,-5,5,10,15),labels = TRUE, tick = TRUE, pos = 0, las=2, mgp=c(3,0.75,0))
add.scatter(myInset(), posi="bottomright",
            inset=c(-0.01,-0.09), ratio=.25,
            bg=transp("white"))
dev.off()

