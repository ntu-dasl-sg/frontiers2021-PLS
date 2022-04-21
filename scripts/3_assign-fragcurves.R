# Assign building classes and fragility curves

library(here)
library(dplyr)
library(ggplot2)
library(reshape2)

# Read building data
bldgs_all <- read.csv(here("data", "buildings_all.csv"))
bldgs_ret <- read.csv(here("data", "buildings_ret_only.csv"))
unique(bldgs_all$struc_type) 

## ASSIGN BUILDING CLASSES TO 70 RETROFITTED BUILDINGS in DATABASE - REALISED CASE
b70.realised <- bldgs_ret
b70.realised$class <- "Retrofitted"

## ASSIGN BUILDING CLASSES TO 70 RETROFITTED BUILDINGS in DATABASE - COUNTERFACTUAL CASE
# Assign URM buildings
b70.counterf_URM <- bldgs_ret %>% filter(struc_type=='Load_Bearing_Stone_Wall_in_Mud_Mortar'  |
                                     struc_type=='Load_Bearing_Brick_Wall_in_Mud_Mortar'  |
                                     struc_type=='Load_Bearing_Brick_Wall_in_Cement_Mortar' |
                                     # struc_type=='Non_Engineered_RC_Frame' |
                                     # struc_type=='Engineered_RC_Frame' |
                                     struc_type=='Mixed' |
                                     struc_type=='Mud_Brick_in_Surkhi' |
                                     struc_type=='Adobe' |
                                     struc_type=='Concrete_Block_in_Cement_Mortar'|
                                     struc_type=='Load_Bearing_Stone_Wall_in_Cement_Mortar' |
                                     struc_type=='Load_Bearing_Brick_Wall_in_cement_mortar')
b70.counterf_URM$class <- "Non-retrofitted URM"

# Assign RC buildings
b70.counterf_RC <- bldgs_ret %>% filter(#struc_type=='Load_Bearing_Stone_Wall_in_Mud_Mortar'  |
                                    # struc_type=='Load_Bearing_Brick_Wall_in_Mud_Mortar'  |
                                    # struc_type=='Load_Bearing_Brick_Wall_in_Cement_Mortar' |
                                    struc_type=='Non_Engineered_RC_Frame' |
                                    struc_type=='Engineered_RC_Frame' 
                                    # struc_type=='Mixed' |
                                    # struc_type=='Mud_Brick_in_Surkhi' |
                                    # struc_type=='Adobe' |
                                    # struc_type=='Concrete_Block_in_Cement_Mortar'|
                                    # struc_type=='Load_Bearing_Stone_Wall_in_Cement_Mortar' |
                                    # struc_type=='Load_Bearing_Brick_Wall_in_cement_mortar'
                                    )
b70.counterf_RC$class <- "Non-retrofitted RC"
# Combine
b70.counterf <- rbind(b70.counterf_URM, b70.counterf_RC)
write.csv(b70.counterf_RC, "b70_rc.csv")
write.csv(b70.counterf_URM, "b70_urm.csv")



## ASSIGN BUILDING CLASSES TO 5207 BUILDINGS in DATABASE - ALL RETROFITTED
b5K.allret <- bldgs_all
b5K.allret$class <- "Retrofitted"

## ASSIGN BUILDING CLASSES TO 5207 BUILDINGS in DATABASE - ALL UNRETROFITTED
# Assign URM buildings
b5K.allret_URM <- b5K.allret %>% filter(struc_type=='Load_Bearing_Stone_Wall_in_Mud_Mortar'  |
                                             struc_type=='Load_Bearing_Brick_Wall_in_Mud_Mortar'  |
                                             struc_type=='Load_Bearing_Brick_Wall_in_Cement_Mortar' |
                                             # struc_type=='Non_Engineered_RC_Frame' |
                                             # struc_type=='Engineered_RC_Frame' |
                                             struc_type=='Mixed' |
                                             struc_type=='Mud_Brick_in_Surkhi' |
                                             struc_type=='Adobe' |
                                             struc_type=='Concrete_Block_in_Cement_Mortar'|
                                             struc_type=='Load_Bearing_Stone_Wall_in_Cement_Mortar' |
                                             struc_type=='Load_Bearing_Brick_Wall_in_cement_mortar')
b5K.allret_URM$class <- "Non-retrofitted URM"

# Assign RC buildings
b5K.allret_RC <- b5K.allret %>% filter(#struc_type=='Load_Bearing_Stone_Wall_in_Mud_Mortar'  |
    # struc_type=='Load_Bearing_Brick_Wall_in_Mud_Mortar'  |
    # struc_type=='Load_Bearing_Brick_Wall_in_Cement_Mortar' |
    struc_type=='Non_Engineered_RC_Frame' |
    struc_type=='Engineered_RC_Frame' 
    # struc_type=='Mixed' |
    # struc_type=='Mud_Brick_in_Surkhi' |
    # struc_type=='Adobe' |
    # struc_type=='Concrete_Block_in_Cement_Mortar'|
    # struc_type=='Load_Bearing_Stone_Wall_in_Cement_Mortar' |
    # struc_type=='Load_Bearing_Brick_Wall_in_cement_mortar'
)
b5K.allret_RC$class <- "Non-retrofitted RC"

write.csv(b5K.allret_RC, "b5k_rc.csv")
write.csv(b5K.allret_URM, "b5k_urm.csv")

# Combine
b5k.allunret <- rbind(b5K.allret_URM, b5K.allret_RC)

### SAVE ALL RDATA
save(b70.realised, file = here("Data", "b70.realised.RData"))
save(b70.counterf, file = here("Data", "b70.counterf.RData"))
save(b5K.allret, file = here("Data", "b5K.allret.RData"))
save(b5k.allunret, file = here("Data", "b5k.allunret.RData"))
