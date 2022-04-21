# CASE 1


##########
# Note from authors of Wei and Chen: "I did not combine stochastic simulation results and deterministic finite difference simulation results with matched filter. Since Prof. Wei said the grid interval is too small for finite difference simulations"
# 
# Papers: Wei, S., Chen, M., Wang, X., Graves, R., Lindsey, E., Wang, T., Karakaş, Ç. and Helmberger, D., 2018. The 2015 Gorkha (Nepal) earthquake sequence: I. Source modeling and deterministic 3D ground shaking. Tectonophysics, 722, pp.447-461.
# Chen, M. and Wei, S., 2019. The 2015 Gorkha, Nepal, Earthquake Sequence: II. Broadband Simulation of Ground Motion in Kathmandu. Bulletin of the Seismological Society of America, 109(2), pp.672-687.
# 
# Data received from Wei Shengji and Mei Chen in July 2020

##########

# Set working libraries
#------------------------------------------
library(here) # sets location of .RProj as working directory
library(knitr) # useful for many rmarkdown layout functions
library(raster) # handling rasters
library(dplyr) # data wrangling
library(reshape) # data wrangling
library(ggplot2) # the ultimate plotter
library(geosphere)
library(sp) # raster plotting # needed for extract
library(sf)
library(gstat) # helps with geostatistics and kriging
library(plotly) # another nice plotter
library(automap) # gives out pretty variogram plots
library(DescTools) # reverse dataframe order
library(viridis) # for colour-blind palette
library(scico) # more scientific palette
library(fields) # for image.plot legends
library(xtable) # export table to latex
library(metR) # allows usage of geom_text_contour in ggplot
library(maps)
library(maptools)
library(grid)
library(xml2)
library(ggridges)

# Set folder location
folder <-  here()
# Set seed
set.seed(42)
# Define map projection of all spatial data
map_proj <- "+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
# Load building data
load(file = paste0(folder, "data/b70.realised.RData"))
load(file = paste0(folder, "data/b70.counterf.RData"))
# Load fragility curves
load(file = paste0(folder, "data/fcurv_URM.RData"))
load(file = paste0(folder, "data/fcurv_RC.RData"))
load(file = paste0(folder, "data/fcurv_Retrofitted.RData"))


# Load PGA data
pga <- read.table( paste0(folder, "data/shaking_model/stochastic_wei/pga.data"), head=TRUE, sep=" ")
colnames(pga) <- c("lon", "lat", "pga") # Change column names
pga$pga <- pga$pga * 0.0010197162129779282 # Convert (cm/s)/s to g-units #1 (cm/s)/s = 0.0010197162129779282 g
summary(pga$pga)
r.pga <- rasterFromXYZ(pga) # Convert PGA csv data to raster

        # ggplot() +
        #     geom_raster(data = pga, aes(x = lon, y = lat, fill = pga)) +
        #     scale_fill_scico(palette = 'bilbao', name = "PGA (%g)", direction = 1) +
        #     geom_point(data = b70.realised, aes(x = lon, y = lat), alpha = 0.9, size = 1.5, colour="black") +
        #     labs(x = "Longitude", y = "Latitude") +
        #     xlim(85.0, 85.7) + # set map bounds along x
        #     ylim(27.4, 27.95) + # set map bounds along y
        #     coord_quickmap()+
        #     theme_bw()

# Convert building locations to SPDF (Spatial Points Data Frame)
pts.b70 <- b70.realised 
coordinates(pts.b70) <- c("lon", "lat")  # set columns for coordinates
proj4string(pts.b70) <- CRS(map_proj) # define projection 
        #plot(pts.b70)

# Extract prediction at building point locations
pga.at.pts <- raster::extract(x = r.pga, # the raster that you wish to extract values from
                                 y = pts.b70, #point locations
                                 method = 'simple', # If 'simple' values for the cell a point falls in are returned.
                                 #buffer = 0, # specify buffer in meters if preferred
                                 na.rm=TRUE,
                                 sp =T # T means create spatial object 
)
pga.at.pts <- as.data.frame(pga.at.pts) # store extracted values to a new column named "pga"
        #summary(pga.at.pts$pga)

# Convert pga value to character. Useful for matching with fragility curves later. 
pga.at.pts$pga <- formatC( round(pga.at.pts$pga, 3 ), format='f', digits=3 )

# Attach pga values to building dataframes
b70.realised$pga <- pga.at.pts$pga
b70.counterf$pga <- pga.at.pts$pga

# Group rows based on building class
bldgs_URM <- b70.counterf %>% filter(class == 'Non-retrofitted URM')
bldgs_RC <- b70.counterf %>% filter(class == 'Non-retrofitted RC')

# Match PGA values extracted from the raster to that in the fragility curve
prob_dmg_URM <- merge(bldgs_URM, fcurv_URM, by.x='pga')
prob_dmg_RC <- merge(bldgs_RC, fcurv_RC, by.x='pga')
prob_dmg_Retrofitted <- merge(b70.realised, fcurv_Retrofitted, by.x='pga')

# Finalize dataframes with associated probabilities of exceedance
b70.counterf <- rbind(prob_dmg_URM,prob_dmg_RC)
b70.realised <- prob_dmg_Retrofitted
######

########
# FATALITY CALCULATION - COUNTERFACTUAL CASE
########

n <- 20000 # Set number of Monte Carlo simulations
m <- nrow(b70.counterf) # number of buildings
L <- matrix(nrow=m,ncol=n) # Make empty matrix for occupants exposed to collapse
c <- b70.counterf$frag # from fragility curve
C.bins <- matrix(nrow=m,ncol=n) # Make matrix for binary numbers, 0 and 1. 1 if collapse, 0 not collapse
O <- b70.counterf$occ_day # occupancy values
FR <- 0.2

## Expected total fatalities
EL <- sum(O * FR * c)

# Monte Carlo simulation of Bernoulli trials 
for (i in 1:n){ 
        C.bins[,i] <- rbinom(n = m, 1, c) 
}

# Fatality calculation for all simulated trials
for (i in 1:m){ #for all buildings
        for (j in 1:n){ # for all simulations
                L[i,j] <- O[i] * FR * C.bins[i,j] # Calculate fatalities 
        }}

# Get total fatalities for each realisation
L.total <- colSums(L)
hist(L.total)
summary(L.total)

# PLOT

# Density plot with semi-transparent fill
# https://r-charts.com/distribution/density-plot-ggplot2/
L.total.df.counterfactual <- data.frame(Fatalities=L.total)
        ggplot(L.total.df.counterfactual, aes(x=Fatalities)) +
                       geom_density(fill="#69b3a2",
                                    alpha=0.8,
                                    kernel = "gaussian",
                                    bw = "bcv", #bandwith
                                    adjust = 4
                                    ) +
                        theme_bw()


########
# FATALITY CALCULATION - REALISED CASE
########

n <- 20000 # Set number of Monte Carlo simulations
m <- nrow(b70.realised) # number of buildings
L <- matrix(nrow=m,ncol=n) # Make empty matrix for occupants exposed to collapse
c <- b70.realised$frag # from fragility curve
C.bins <- matrix(nrow=m,ncol=n) # Make matrix for binary numbers, 0 and 1. 1 if collapse, 0 not collapse
O <- b70.realised$occ_day # occupancy values
FR <- 0.2

## Expected total fatalities
EL <- sum(O * FR * c)

# Monte Carlo simulation of Bernoulli trials 
for (i in 1:n){ 
        C.bins[,i] <- rbinom(n = m, 1, c) 
}

# Fatality calculation for all simulated trials
for (i in 1:m){ #for all buildings
        for (j in 1:n){ # for all simulations
                L[i,j] <- O[i] * FR * C.bins[i,j] # Calculate fatalities 
        }}

# Get total fatalities for each realisation
L.total <- colSums(L)
hist(L.total)
summary(L.total)

# PLOT

# Density plot with semi-transparent fill
# https://r-charts.com/distribution/density-plot-ggplot2/
L.total.df.realised<- data.frame(Fatalities=L.total)
        ggplot(L.total.df.realised, aes(x=Fatalities)) +
                geom_density(fill="#69b3a2",
                             alpha=0.8,
                             kernel = "gaussian",
                             bw = "bcv", #bandwith
                             adjust = 4
                ) +
                theme_bw()

# Reshape data frames for plotting
fatality_all <- data.frame(index = c(1:n),
                           Counterfactual=L.total.df.counterfactual,
                           Actual=L.total.df.realised)
names(fatality_all)=c("index","Counterfactual","Actual")
fatality.melt <- melt(fatality_all, id.vars = "index")
names(fatality.melt)=c("index","Building_state","Fatalities")

        
# Calculate mean fatalities
mean_ret_fat <- mean(L.total.df.realised$Fatalities)
mean_unret_fat <- mean(L.total.df.counterfactual$Fatalities)
LivesSaved <- round(mean_unret_fat - mean_ret_fat)


# Density plot 
ggplot(fatality.melt, aes(x=Fatalities, y=Building_state, fill=Building_state)) +
                stat_density_ridges(
                                    #quantile_lines=TRUE, quantile_fun=function(x,...)mean(x),
                                    scale = 1.2,
                                    alpha=.9,
                                    #bw = "bcv", #bandwith
                                    bandwidth = 10,
                                    color="white"
                                    #adjust = .1
                )+
                scale_y_discrete(expand = c(0, 0)) + 
                scale_x_continuous(expand = c(0, 0)) +   # for both axes to remove unneeded padding
                #scale_fill_manual(values=c("#d7191c", "#2c7bb6"))+
                scale_fill_manual(values=c("#c41a00", "#142b5c"))+
                scale_color_manual(values=c("#c41a00", "#142b5c"))+
                guides(fill=FALSE, color=FALSE) + #remove legend
                coord_cartesian(clip = "off")+  # to avoid clipping of the very top of the top ridgeline
                xlim(c(0,300)) +
                labs(y = "Scenario",
                     title = paste("Estimated fatalities for", 70, "school buildings"),
                     subtitle = "(2015 Gorkha Earthquake scenario)")+ 
                theme_ridges(center = TRUE, grid = FALSE) +
                geom_segment(aes(x = mean_ret_fat, xend = mean_ret_fat, y=1.9, yend=3),
                             linetype="dotted",
                             size=0.4, color = "grey80", alpha=0.1)+
                geom_segment(aes(x = mean_unret_fat, xend = mean_unret_fat, y=1, yend=1.9),
                             linetype="dotted",
                             size=0.4, color = "grey80", alpha=0.1)+
                geom_segment(aes(x = mean_ret_fat, xend = mean_unret_fat, y=1.9, yend=1.9),
                             arrow = arrow(length = unit(0.2, "cm")), linetype=1, size=0.1, color = "black")+
                geom_segment(aes(x = mean_unret_fat, xend = mean_ret_fat, y=1.9, yend=1.9),
                             arrow = arrow(length = unit(0.2, "cm")), linetype=1, size=0.1, color = "black")+
                annotate(geom="text", x=mean_unret_fat+50, y=1.9, label=paste("Estimated lives saved =", LivesSaved), color="black", angle=0) 

        ggsave(file= here("graphics", "results_CASE1.png") ,
               #plot = last_plot(),
               bg = "white",
               width = 7, height = 3,
               scale =1.2,
               dpi = 300)