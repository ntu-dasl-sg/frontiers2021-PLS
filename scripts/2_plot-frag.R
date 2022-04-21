# Plot fragility curves

# Reference of fragility curves
# We used the B.Posterior curves, DS5 (Collapse)
# https://link.springer.com/article/10.1007/s11069-020-04312-1
# https://discovery.ucl.ac.uk/id/eprint/10120856/1/AdhikariDAyalaandNorris_17WCEE2020.pdf

library(here)
library(dplyr)
library(ggplot2)
library(reshape2)


PGA <- seq(0,1,0.001)
# Set params
prob_URM <- plnorm(PGA, meanlog = log(0.55), sdlog = 0.76)
prob_RC <- plnorm(PGA, meanlog = log(1.13), sdlog = 0.84)
prob_Retrofitted <- plnorm(PGA, meanlog = log(1.133), sdlog = .452)
plot(prob_URM)
plot(prob_RC)

# Make data frames for fragility curves
fcurv_URM <- data.frame(pga=PGA, frag=prob_URM)
fcurv_RC <- data.frame(pga=PGA, frag=prob_RC)
fcurv_Retrofitted <- data.frame(pga=PGA, frag=prob_Retrofitted)

# change pga column to character with 3 decimal places
fcurv_URM$pga <- formatC( round(fcurv_URM$pga, 3 ), format='f', digits=3 )
fcurv_RC$pga <- formatC( round(fcurv_RC$pga, 3 ), format='f', digits=3 )
fcurv_Retrofitted$pga <- formatC( round(fcurv_Retrofitted$pga, 3 ), format='f', digits=3 )

# Collect all fragility curves to one dataframe
frag_all <- data.frame (PGA = PGA,
                        prob_URM= prob_URM,
                        prob_RC= prob_RC,
                        prob_Retrofitted = prob_Retrofitted
                        )

# Rename columns
names(frag_all)=c("PGA","URM","RC", "Retrofitted")
# Reshape for plotting
frag_all.melt <- melt(frag_all,id.vars = "PGA")
# Add column to differentiate non-retrofitted and unretrofitted
#frag_all.melt <- cbind(frag_all.melt,type=c(rep("Non-Retrofitted",length(PGA)*4),rep("Retrofitted",length(PGA)*1)))
#Rename columns
names(frag_all.melt)=c("PGA","Building_type","value")

## set the levels in order we want
frag_all.melt <- within(frag_all.melt,
                        Building_type <- factor(Building_type,
                                                levels=c("URM","RC", "Retrofitted")))
levels(frag_all.melt$Building_type)

# Plot
ggplot(data=frag_all.melt)+
    geom_line(aes(x=PGA, y=value, linetype=Building_type))+
    scale_linetype_manual(values=c(1,5,4,2,3), name="Building type",
                          labels=c("Non-retrofitted (URM)","Non-retrofitted (RC)", "Retrofitted (All)")) +  
    labs(x="Peak ground acceleration (g)",
         y = "Probability of Exceeding Collapse Damage",
         title="Collapse fragility curves for school buildings in Nepal")  +
    scale_x_continuous(expand = c(0, 0), 
                       limits = c(0, 1),
                       breaks = seq(0, 1, by = 0.1)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1),
                       breaks = seq(0, 1, by = 0.1)) +
    #annotate(geom="text", x=2.5, y=.05, label="Data source: (JICA, 2002)") +
    theme(
            panel.background = element_rect(fill = NA),
          panel.border = element_blank(),
          plot.title = element_text(face = "bold"),
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_line(colour = "gray90"),
          # panel.grid.minor = element_blank(),
          legend.key=element_blank())

# Save file
ggsave(file= here("graphics", "frag-curves.png"),
       width = 5, height = 3,
       scale =1.5,
       dpi = 300)

### SAVE ALL RDATA
save(fcurv_URM, file = here("Data", "fcurv_URM.RData"))
save(fcurv_RC, file = here("Data", "fcurv_RC.RData"))
save(fcurv_Retrofitted, file = here("Data", "fcurv_Retrofitted.RData"))

