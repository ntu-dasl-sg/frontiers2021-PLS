# Visualize hazard curves based on the OQ events
## The aim of this code is purely to visualize the hazard curves for three school building locations


library(here)
library(dplyr)
library(reshape2)
library(ggplot2)
library(scales)
library(ggalt) #for spline line

### READ OPEN QUAKE OUTPUTS ### ############-------

# Set folder location
folder <- here("data", "oq", "seed2", "Output")
    ## We assume the plots won't change much if we use all 100,000 events to generate the curve, versus using only 10,000
    ## Since this is purely for visualization purposes only, we use only 10,000


# Record file names:
sitemesh.files <- list.files(paste0(folder, '/sitemesh/'))
events.files <- list.files(paste0(folder, path = '/events'))
gmf.files <-  list.files(paste0(folder, path = '/gmf'))
# Read files
sites.df <- read.table(paste0(folder, '/sitemesh/' , sitemesh.files[1] ), header = TRUE, sep = ',')  # site_id	lat lon
events.df <- read.table(paste0(folder, '/events/', events.files[1]), header = TRUE, sep = ',')  #event_id rup_id rlz_id year ses_id
gmf.df <- read.table(paste0(folder, '/gmf/', gmf.files[1]), header = TRUE, sep = ',') # site_id event_id gmv_PGA
# Count number of sites, events, and ses
site_list <- unique(sites.df$site_id)     # List of sites: # 5029 sites 
event_list <- unique(gmf.df$event_id)      # List of events: # 3,682 events
ses_list <- unique(events.df$rlz_id)     # List of ses: # 3270 ses with events.
# Add lat and lon to pga values. Takes a while
gmf.df    <- merge(gmf.df, sites.df, by = 'site_id')

## PICK ONE BUILDING LOCATION
gmf.at.site1    <- subset( gmf.df, lat == 27.73548) # A
gmf.at.site1 <- gmf.at.site1$gmv_PGA #get the pga only
gmf.at.site2    <- subset( gmf.df, lon == 85.31760) # C
gmf.at.site2 <- gmf.at.site2$gmv_PGA #get the pga only
gmf.at.site3    <- subset( gmf.df, lon == 85.44333) # B
gmf.at.site3 <- gmf.at.site3$gmv_PGA #get the pga only


# Get probability of exceedance
ses_length <- length(gmf.at.site1) # number of events experienced at site
rank.length <- 10000 # because there are 10,000 ses in OQ
rank <- 1:rank.length
ses_pick1 <- append(gmf.at.site1, rep(0,(rank.length-ses_length)) ) #add the ses with 0 events for ordering
ses_pick1.ordered <- ses_pick1[order(ses_pick1, decreasing=TRUE)]
HCA <- data.frame(ses_pick1.ordered, rank)
HCA$prob <- HCA$rank / (length(HCA$rank)+1)
head(HCA)

# Get probability of exceedance
ses_length <- length(gmf.at.site2) # number of events experienced at site
rank.length <- 10000 # because there are 10,000 ses in OQ
rank <- 1:rank.length
ses_pick2 <- append(gmf.at.site2, rep(0,(rank.length-ses_length)) ) #add the ses with 0 events for ordering
ses_pick2.ordered <- ses_pick2[order(ses_pick2, decreasing=TRUE)]
HCC <- data.frame(ses_pick2.ordered, rank)
HCC$prob <- HCC$rank / (length(HCC$rank)+1)
head(HCC)

# Get probability of exceedance
ses_length <- length(gmf.at.site3) # number of events experienced at site
rank.length <- 10000 # because there are 10,000 ses in OQ
rank <- 1:rank.length
ses_pick3 <- append(gmf.at.site3, rep(0,(rank.length-ses_length)) ) #add the ses with 0 events for ordering
ses_pick3.ordered <- ses_pick3[order(ses_pick3, decreasing=TRUE)]
HCB <- data.frame(ses_pick3.ordered, rank)
HCB$prob <- HCB$rank / (length(HCB$rank)+1)
head(HCB)


# Plot hazard curve for 1 location

ggplot(data=HCC, aes(y=prob, x=ses_pick3.ordered)) +
    geom_smooth(se=F)+
    scale_y_log10()+
    # scale_x_log10()+
    scale_x_continuous(expand = c(0, 0),
                       limits = c(0, 0.55),
                       breaks = seq(0, 0.55, by = 0.1),
                       labels=comma)+ #scales package
    annotation_logticks(side = "l") +
    labs(y = "Annual exceedance probability", x="PGA (g)") +
    theme_bw()+
    theme(axis.line = element_line(color='black'),
          plot.background = element_blank(),
          panel.border = element_blank())

ggsave(file= here("graphics", "haz_curve-v2.png") ,
       #plot = last_plot(),
       bg = "white",
       width = 7, height = 6,
       scale =.9,
       dpi = 300)



# Plot hazard curve for multiple locations

ggplot() +
    geom_smooth(data=HCC, aes(y=prob, x=ses_pick2.ordered), se=F,linetype = "longdash", color = "red")+
    geom_smooth(data=HCB, aes(y=prob, x=ses_pick3.ordered), se=F, linetype = "dotted", color = "red")+
    geom_smooth(data=HCA, aes(y=prob, x=ses_pick1.ordered), se=F, color = "red")+
    scale_y_log10()+
    # scale_x_log10()+
    scale_x_continuous(expand = c(0, 0),
                       limits = c(0, 0.55),
                       breaks = seq(0, 0.55, by = 0.1),
                       labels=comma)+ #scales package
    annotation_logticks(side = "l") +
    labs(y = "Annual exceedance probability", x="PGA (g)") +
    theme_bw()+
    theme(axis.line = element_line(color='black'),
          plot.background = element_blank(),
          panel.border = element_blank())

ggsave(file= here("graphics", "hazcurves_plot.png") ,
       #plot = last_plot(),
       bg = "white",
       width = 7, height = 6,
       scale =.9,
       dpi = 300)


