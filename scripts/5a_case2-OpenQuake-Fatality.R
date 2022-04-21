# CASE2 - Calculate fatalities based on stochastic event sets (ses) generated in OpenQuake
## For this case, a total of 100,000 ses were generated.
## In the folder "data/oq/", there are 10 folders with 10,000 ses each
## We run this code for each of the 10 folders, making sure that we change the "seed_num" variable
## For each run, the results are stored into RData files

library(here)
library(dplyr)
library(reshape2)
library(ggplot2)
library(scales)

seed_num <- "seed1" # Change this seed 
# Set folder location
folder <- here("data", "oq", seed_num, "Output")
load(here("data", "b5k.allunret.RData")) # non-retrofitted
load(here("data", "b5k.allret.RData")) # non-retrofitted

### READ OPEN QUAKE OUTPUTS ### ############-------

# Record file names:
sitemesh.files <- list.files(paste0(folder, '/sitemesh/'))
events.files <- list.files(paste0(folder, path = '/events'))
gmf.files <-  list.files(paste0(folder, path = '/gmf'))
# Read files
sites.df <- read.table(paste0(folder, '/sitemesh/' , sitemesh.files[1] ), header = TRUE, sep = ',')  # site_id	lat lon
events.df <- read.table(paste0(folder, '/events/', events.files[1]), header = TRUE, sep = ',')  #event_id rup_id rlz_id year ses_id
gmf.df <- read.table(paste0(folder, '/gmf/', gmf.files[1]), header = TRUE, sep = ',') # site_id event_id gmv_PGA
# Count number of sites, events, and ses
site_list <- unique(sites.df$site_id)     # List of sites: # --- sites 
event_list <- unique(gmf.df$event_id)      # List of events: # --- events
ses_list <- unique(events.df$rlz_id)     # List of ses: # --- ses with events.
# Add lat and lon to pga values. Takes a while
gmf.df    <- merge(gmf.df, sites.df, by = 'site_id')
##### SAVE RDATA
save(gmf.df, file = paste0(folder, "/gmf_phase1_",seed_num, ".RData"))
##### LOAD results of this chunk:
#load(file = paste0(folder, "gmf_phase1_",seed_num, ".RData"))

# Round off PGA in all SES - this is for matching PGA values with fragility curves
gmf.df$gmv_PGA <- round(gmf.df$gmv_PGA, 3)
names(gmf.df)[3] <- c("pga") #rename columns
head(gmf.df)
gmf.df$pga <- as.character((gmf.df$pga))
class(gmf.df$pga) #should be character for matching

# Attach building class + occupancy to gmf
b5k.allunret$site_id <- sites.df$site_id # Add site_id column to building info, so it's easy to match later
gmf.df    <- merge(gmf.df, b5k.allunret, by= 'site_id') # add building classes etc . TAKES A WHILE
gmf.df  <- subset(gmf.df, select = -c(lon.x, lat.x)) # drop duplicate columns
names(gmf.df)[4:5] <- c("lat", "lon") #rename columns
head(gmf.df)
##### SAVE RDATA
save(gmf.df, file = paste0(folder, "/gmf_phase2_",seed_num, ".RData"))
##### LOAD results of this chunk:
#load(file = paste0(folder, "/gmf_phase2_",seed_num, ".RData"))

# Load fragility curve data
load(here("data", "fcurv_RC.RData")) 
load(here("data", "fcurv_Retrofitted.RData")) 
load(here("data", "fcurv_URM.RData"))
frag <- data.frame(pga = fcurv_RC$pga,
                   RC = fcurv_RC$frag,
                   URM = fcurv_URM$frag,
                   Retrofitted = fcurv_Retrofitted$frag)
names(frag) <- c("pga", "Non-retrofitted RC", "Non-retrofitted URM", "Retrofitted")
frag_all.melt <- melt(frag, id.vars = "pga")
names(frag_all.melt) <- c("pga", "class", "prob")
class(frag_all.melt$pga)
unique(frag_all.melt$class)


# Find ground motion fields for each realization (not per event as given in output)
# Subset events database to get all events in each realization
# Subset by site_id and add together shaking from all events (in each realization) at that point
#gmf.df_list <- vector('list', n_samples) 
n_samples  <- 10000 # N stochastic event sets / number of 1-year SES
ses_fat <- numeric(length(ses_list))

k = 1
# Takes a while
for ( k in 1:length(ses_list)){ #for each realization (ses)
    i = ses_list[k]
    events.df.ss <- subset ( events.df, rlz_id == i ) # All events in realization i-1    # -1 because starts at 0 but I start at 1 
    gmf.df.ss    <- subset ( gmf.df, event_id %in% events.df.ss$event_id) # All PGA for the events in realization i-1
    # Get probability of building collapse for all events in this SES and for all sites
    gmf.df.prob.ses <-  merge(frag_all.melt, gmf.df.ss, by.x = c("pga", "class"), by.y = c("pga", "class"), all.x = FALSE, all.y = TRUE)
    # Calculate total fatalities for each ses
    gmf.df.prob.ses$fat <- gmf.df.prob.ses$occ_day * 0.2 * gmf.df.prob.ses$prob
    f <- sum(gmf.df.prob.ses$occ_day * 0.2 * gmf.df.prob.ses$prob, na.rm = T)
    # Get sum of all fatalities for all events
    ses_fat[k] <- f
    print(paste("SES ", k, "/", length(ses_list), " ... done.", sep = ""))
}
summary(ses_fat)

##### SAVE RDATA
save(ses_fat, file = paste0(folder, "/fats_",seed_num, ".RData"))
##### LOAD results of this chunk:
#load(file = paste0(folder, "/fats_",seed_num, ".RData"))

### RETROFITTED ##########

# Change the classes of all buildings to retrofitted
head(gmf.df)
gmf.df_ret <- gmf.df
gmf.df_ret$class <- "Retrofitted"   
head(gmf.df_ret)

ses_fat_ret <- numeric(length(ses_list))

#Initialize/clear dataframes
events.df.ss <- NA
gmf.df.ss <- NA
gmf.df.prob.ses <- NA

k=1
# Takes a while
for ( k in 1:length(ses_list)){ #for each realization (ses)
    i = ses_list[k]
    events.df.ss <- subset ( events.df, rlz_id == i ) # All events in realization i-1    # -1 because starts at 0 but I start at 1 
    gmf.df.ss    <- subset ( gmf.df_ret, event_id %in% events.df.ss$event_id) # All PGA for the events in realization i-1
    # Get probability of building collapse for all events in this SES and for all sites
    gmf.df.prob.ses <-  merge(frag_all.melt, gmf.df.ss, by.x = c("pga", "class"), by.y = c("pga", "class"), all.x = FALSE, all.y = TRUE)
    # Calculate total fatalities for each ses
    gmf.df.prob.ses$fat <- gmf.df.prob.ses$occ_day * 0.2 * gmf.df.prob.ses$prob
    f <- sum(gmf.df.prob.ses$occ_day * 0.2 * gmf.df.prob.ses$prob, na.rm = TRUE)
    # Get sum of all fatalities for all events
    ses_fat_ret[k] <- f
    print(paste("SES ", k, "/", length(ses_list), " ... done.", sep = ""))
}
summary(ses_fat_ret)

##### SAVE RDATA
save(ses_fat_ret, file = paste0(folder, "/fats_",seed_num, "_ret.RData"))
##### LOAD results of this chunk:
#load(file = paste0(folder, "/fats_",seed_num, ".RData"))
