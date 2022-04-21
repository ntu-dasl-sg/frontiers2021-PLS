# Generate the annual fatality exceedance curve for 100,000 ses
## This incorporates a Bernoulli simulation to account for the 15% probability that any day is a school day
## We assume no uncertainty related to time of day

library(ggplot2)
library(scales)
library(here)

# Proportion of number of school hours in a year 
sch.rate <- (220*6) / (365*24) # = 0.1506849
set.seed(1)
# Set folder location
folder <- here("data", "calculated_fatalities")


# Load fatalities calculated
load(file = paste0(folder, "/fats_seed1_ret.RData"))
s1r <- ses_fat_ret
load(file = paste0(folder, "/fats_seed1.RData"))
s1u <- ses_fat
load(file = paste0(folder, "/fats_seed2_ret.RData"))
s2r <- ses_fat_ret
load(file = paste0(folder, "/fats_seed2.RData"))
s2u <- ses_fat
load(file = paste0(folder, "/fats_seed3_ret.RData"))
s3r <- ses_fat_ret
load(file = paste0(folder, "/fats_seed3.RData"))
s3u <- ses_fat
load(file = paste0(folder, "/fats_seed4_ret.RData"))
s4r <- ses_fat_ret
load(file = paste0(folder, "/fats_seed4.RData"))
s4u <- ses_fat
load(file = paste0(folder, "/fats_seed5_ret.RData"))
s5r <- ses_fat_ret
load(file = paste0(folder, "/fats_seed5.RData"))
s5u <- ses_fat
load(file = paste0(folder, "/fats_seed6_ret.RData"))
s6r <- ses_fat_ret
load(file = paste0(folder, "/fats_seed6.RData"))
s6u <- ses_fat
load(file = paste0(folder, "/fats_seed7_ret.RData"))
s7r <- ses_fat_ret
load(file = paste0(folder, "/fats_seed7.RData"))
s7u <- ses_fat
load(file = paste0(folder, "/fats_seed8_ret.RData"))
s8r <- ses_fat_ret
load(file = paste0(folder, "/fats_seed8.RData"))
s8u <- ses_fat
load(file = paste0(folder, "/fats_seed9_ret.RData"))
s9r <- ses_fat_ret
load(file = paste0(folder, "/fats_seed9.RData"))
s9u <- ses_fat
load(file = paste0(folder, "/fats_seed10_ret.RData"))
s10r <- ses_fat_ret
load(file = paste0(folder, "/fats_seed10.RData"))
s10u <- ses_fat

sescount <- 10 #number of 10k ses
n = 10000 # number of monte carlo simulations
m <- length(s1r) + length(s2r) + length(s3r) + length(s4r)+ length(s5r) + length(s6r) + length(s7r) + length(s8r) + length(s9r) + length(s10r)# number of ses

Bins <- matrix(nrow=m,ncol=n) # Make matrix for binary numbers, 0 and 1. 1 if school is open
L <- matrix(nrow=m,ncol=n) # Make empty matrix for fatalities when school is open
fat.mean <- numeric(length=m)

############# set
seed_num <- "seed100k"
ses_fat <- c(s1u,s2u, s3u,s4u,s5u,s6u,s7u,s8u,s9u,s10u)
ses_fat_ret <- c(s1r,s2r, s3r,s4r,s5r,s6r,s7r,s8r,s9r,s10r)
###########

### UNRETROFITTED
# Monte Carlo simulation of Bernoulli trials 
for (i in 1:n){  Bins[,i] <- rbinom(n = m, 1, sch.rate) }
# Fatality calculation for all simulated trials
for (i in 1:m){ #for all ses
    for (j in 1:n){ # for all simulations
        L[i,j] <- ses_fat[i]* Bins[i,j] }}# Calculate fatalities at time when school is open
for (i in 1:m){ fat.mean[i] <- mean(L[i,]) } # Get the mean fatalities for all realisations, for each ses
summary(fat.mean)
save(fat.mean, file = paste0(folder, "/fats_",seed_num, "_mean.RData"))
#load(file = paste0(folder, "/fats_",seed_num, "_mean.RData"))


### RETROFITTED
Bins.ret <- matrix(nrow=m,ncol=n) # Make matrix for binary numbers, 0 and 1. 1 if school is open
L.ret <- matrix(nrow=m,ncol=n) # Make empty matrix for fatalities when school is open
fat.mean.ret <- numeric(length=m)

# Monte Carlo simulation of Bernoulli trials 
for (i in 1:n){ Bins.ret[,i] <- rbinom(n = m, 1, sch.rate) }
# Fatality calculation for all simulated trials
for (i in 1:m){ #for all ses
    for (j in 1:n){ # for all simulations
        L.ret[i,j] <- ses_fat_ret[i]* Bins.ret[i,j] }}
for (i in 1:m){ #for all ses # Get the mean fatalities for all realisations, for each ses
    fat.mean.ret[i] <- mean(L.ret[i,])}
summary(fat.mean.ret)
save(fat.mean.ret, file = paste0(folder, "/fats_",seed_num, "_mean_ret.RData"))
#load(file = paste0(folder, "/fats_",seed_num, "_mean_ret.RData"))

######### DATAFRAME UNRETROFITTED
ses_length <- length(fat.mean)
rank.length <- 10000 * sescount 
rank <- 1:rank.length
ses_fatk <- append(fat.mean, rep(0,(rank.length-ses_length)) ) #add the ses with 0 events for ordering
ses_fat.ordered <- ses_fatk[order(ses_fatk, decreasing=TRUE)]
EP <- data.frame(ses_fat.ordered, rank)
EP$prob <- EP$rank / (length(EP$rank)+1)
head(EP)

######### DATAFRAME  BOTH UNRETROFITTED AND RETROFITTED
ses_length <- length(fat.mean.ret)
rank.length <- 10000 * sescount # length(ses_fat) # should be 10000
rank <- 1:rank.length
ses_fatk_ret <- append(fat.mean.ret, rep(0,(rank.length-ses_length)) ) #add the ses with 0 events for ordering
ses_fat.ordered_ret <- ses_fatk_ret[order(ses_fatk_ret, decreasing=TRUE)]
EP_ret <- data.frame(ses_fat.ordered_ret, rank)
EP_ret$prob <- EP_ret$rank / (length(EP_ret$rank)+1)
head(EP_ret)
# New dataframes
ep_unret <- EP
ep_ret <- EP_ret
names(ep_unret) <- c("fat", "rank", "prob")
names(ep_ret) <- c("fat", "rank", "prob")
# Add groups
ep_unret$Scenario <- "No buildings retrofitted"
ep_ret$Scenario <- "All buildings retrofitted"
ep_compiled <- rbind(ep_unret, ep_ret)
head(ep_compiled)

#PLot
ggplot(data=ep_compiled, aes(y=prob, x=fat, linetype=Scenario)) +
    geom_line()+
    scale_y_log10(breaks = c(1,.1, .01,.001,.0001), limits = c(1e-4, 1))+
    annotation_logticks(side = "l") +
    scale_x_continuous(expand = c(0, 0),
                       limits = c(0, 5200),
                      breaks = seq(0, 5200, by = 1000),
                       labels=comma)+ #scales package
    #coord_cartesian(ylim=c(NA,1)) +
    labs(y = "Annual exceedance probability", x="Number of fatalities due to building collapse") +
    #expand_limits(x = 0, y = 0)+
    theme_bw()+
    theme(axis.line = element_line(color='black'),
          plot.background = element_blank(),
          panel.border = element_blank(),
          legend.position = c(0.7, 0.8)
    )
ggsave(file= here("graphics", "loss_curve_ALL_100k.png") ,
       bg = "white",
       width = 7, height = 6,
       scale =.9,
       dpi = 300)

## AAL
n_samples <- 100000
AAL.unret <- sum(fat.mean, na.rm=TRUE) / n_samples
AAL.ret <- sum(fat.mean.ret, na.rm=TRUE) / n_samples

#### SAVE Calculated fataliteis and exceedance probability

save(EP, file = paste0(folder, "EP_unret",seed_num, ".RData"))
save(EP_ret, file = paste0(folder, "EP_ret",seed_num, ".RData"))

