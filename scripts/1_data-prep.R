# Prepare building dataset
library(here)
library(dplyr)

# Read Nepal school dataset
dat_allbldgs <- read.csv( here( "data","Exposure_School_Hosp_Nepal.csv"), head=TRUE, sep=",")

# View list of all building categories. 
bldg.categories <- unique(dat_allbldgs$building_usage)

# Get subset of educational facilities
dat_allsch <- filter(dat_allbldgs,
                     building_usage == "school" |
                     building_usage == "college" |
                     building_usage == "kindergarten" |
                     building_usage == "university" )

# Remove buildings with unknown retrofit state 
dat_ret.school <- dat_allsch %>% filter(retrofit != "N/A")

# Get columns of interest, and rename properly
dat_ret.school <- data.frame(lat=dat_ret.school$Lon,  #note the original database mistook lon as lat, and vice versa
                             lon=dat_ret.school$Lat, 
                             num_flr = dat_ret.school$num_floors,
                             occupants_day = dat_ret.school$occupants_day, 
                             occupants_morning = dat_ret.school$occupants_morning,
                             retrofit = dat_ret.school$retrofit,
                             struc_type = dat_ret.school$structur_type)

# Note that the data has two columns for day occupants
# Here, we just retain whichever is larger to be on the conservative side
dat_ret.school$occ_day <- pmax(dat_ret.school$occupants_day, dat_ret.school$occupants_morning)
dropcols <- c("occupants_morning")
dat_ret.school <- dat_ret.school[ , !(names(dat_ret.school) %in% dropcols)]

# Remove rows with no occupancy values during the day
dat_ret.school$occ_day[dat_ret.school$occ_day==""] <- "NA" #Replace empty cells with NA
dat_ret.school <- na.omit(dat_ret.school, cols="occ_day") # Remove if day occupancy = NA
dat_ret.school$occ_day <- as.numeric(dat_ret.school$occ_day) # Set occupancy numbers as numeric
dat_ret.school <- dat_ret.school %>% filter(occ_day != 0) # Remove if day occupancy = 0
dat_ret.school <- dat_ret.school %>% filter(occ_day != "No") #Remove rows with invalid entries
dat_ret.school <- dat_ret.school %>% filter(occ_day != "hall") 
dat_ret.school <- dat_ret.school %>% filter(occ_day != "NA") 

# Remove buildings with unknown number of floors
dat_ret.school <- na.omit(dat_ret.school, cols="num_flr") # Remove NA
dat_ret.school <- dat_ret.school %>% filter(num_flr != "N/A")# Remove NA
dat_ret.school$num_floors <- as.numeric(dat_ret.school$num_flr)
dat_ret.school<- dat_ret.school %>% filter(num_flr<6)

# Drop columns we don't need hereon. 
dropcols <- c("occupants_day", "num_flrs", "num_floors")
dat_ret.school <- dat_ret.school[ , !(names(dat_ret.school) %in% dropcols)]

# Remove strange occupancy values that could inflate our results.
dat_ret.school<- dat_ret.school %>% filter(occ_day!=38140) # too large?
dat_ret.school<- dat_ret.school %>% filter(occ_day!=6030)
dat_ret.school<- dat_ret.school %>% filter(occ_day!=3510)

# Make sure there are no invalid structure types
unique(dat_ret.school$struc_type) # Unique structural types
# [1] "Load_Bearing_Stone_Wall_in_Mud_Mortar"    "Load_Bearing_Brick_Wall_in_Mud_Mortar"    "Load_Bearing_Brick_Wall_in_Cement_Mortar"
# [4] "Non_Engineered_RC_Frame"                  "Engineered_RC_Frame"                      "Mixed"                                   
# [7] "Mud_Brick_in_Surkhi"                      "Adobe"                                    "Concrete_Block_in_Cement_Mortar"         
# [10] "Load_Bearing_Stone_Wall_in_Cement_Mortar" "Load_Bearing_Brick_Wall_in_cement_mortar"

# We filter the buildings based on available fragility curves. Make sure there are no invalid types
dat_ret.school <- dat_ret.school %>% filter(struc_type=='Load_Bearing_Brick_Wall_in_Mud_Mortar'  |
                                                struc_type=='Load_Bearing_Stone_Wall_in_Mud_Mortar'  |
                                                struc_type=='Load_Bearing_Brick_Wall_in_Cement_Mortar' |
                                                struc_type=='Load_Bearing_Stone_Wall_in_Cement_Mortar' |
                                                struc_type=='Mud_Brick_in_Surkhi' |
                                                struc_type=='Mixed' |
                                                struc_type=='Engineered_RC_Frame' |
                                                struc_type=='Non_Engineered_RC_Frame'|
                                                struc_type=='Adobe')

# Summary stats check
summary(dat_ret.school$occ_day)
unique(dat_ret.school$retrofit)
unique(dat_ret.school$struc_type)

# Save csv of all building data 
write.csv(dat_ret.school, file= here("data", "buildings_all.csv"), row.names = F)

# Filter retrofitted schools
dat_ret.school_ret <- dat_ret.school %>% filter(retrofit=='Yes')

# Save csv of building data - retrofitted
write.csv(dat_ret.school_ret, file= here("data", "buildings_ret_only.csv"), row.names = F)
