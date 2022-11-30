# Plot phylogenetic tree
# author: Yuan Chai
# email: chaix026@umn.edu


rm(list=ls())

library(data.table)
library(ggplot2)
library(RColorBrewer)
library(maps)
library(geofacet)
library(usdata)

# Load the PD results
load("./data_intermediate/04_02_PD_Durum_region.Rdata")
load("./data_intermediate/04_02_PD_Spring_region.Rdata")
load("./data_intermediate/04_02_PD_Winter_region.Rdata")

# Combine data into long format
PD.Durum.sum.region$class3 <- "Durum"
PD.Spring.sum.region$class3 <- "Spring"
PD.Winter.sum.region$class3 <- "Winter"
PD.DSW <- rbind(PD.Durum.sum.region, PD.Spring.sum.region, PD.Winter.sum.region) 
PD.DSW$year <- as.integer(PD.DSW$year)
PD.DSW$PD0 <- as.numeric(PD.DSW$PD0)
PD.DSW$PD1 <- as.numeric(PD.DSW$PD1)
PD.DSW$PD2 <- as.numeric(PD.DSW$PD2)
PD.DSW[PD.DSW == 0] <- NA
PD.DSW <- as.data.table(PD.DSW)

# Alternative: not scaling 
WVA.Diversity <- PD.DSW
WVA.Diversity[, `:=`(PD0.a = PD0,
                     PD1.a = PD1,
                     PD2.a = PD2)]


# Summary statistics
sum_PD_st_yr <- WVA.Diversity[, .(meanD0=mean(PD0.a, na.rm=T), sdD0=sd(PD0.a, na.rm=T),
                                  meanD1=mean(PD1.a, na.rm=T), sdD0=sd(PD1.a, na.rm=T),
                                  meanD2=mean(PD2.a, na.rm=T), sdD0=sd(PD2.a, na.rm=T)), by=.(class3, state)]
sum_PD_st_yr <- sum_PD_st_yr[order(class3, state)]

write.csv(sum_PD_st_yr, file="./data_final/04_02_summary_PD_region_summary.csv")
WVA.Diversity[, .(meanD0=mean(PD0.a, na.rm=T), sdD0=sd(PD0.a, na.rm=T),
                  meanD1=mean(PD1.a, na.rm=T), sdD0=sd(PD1.a, na.rm=T),
                  meanD2=mean(PD2.a, na.rm=T), sdD0=sd(PD2.a, na.rm=T)),
              by=.(class3)] # Overall summary by class


# Save data in long format
WVA.PD.long <- melt(WVA.Diversity[, .(state, year, class3, PD0.a, PD1.a, PD2.a)], 
                    id.vars = c("state", "year", "class3"),
                    variable.name = "Variable")
save(WVA.PD.long, file="./data_intermediate/04_02_PD_DSW_region_plot.RData")



#### Plot ####

# Load PD results
load("./data_intermediate/04_02_PD_DSW_region_plot.RData")
WVA.PD.D.long <- WVA.PD.long

WVA.PD.D.long[state == "Central", state := "Central Plains"]
WVA.PD.D.long[state == "North Central", state := "Northern Plains"]
WVA.PD.D.long[, state:= factor(state, levels=c("Central Plains", "Northern Plains", "Pacific Northwest", "Southeast", "California"))]


# Plot2 based on US map


DSW.colPal <- c("#E34A33", "#31A354", "#3182BD")

plt.dat.DSW.PD1.region <- WVA.PD.D.long[Variable %in% c("PD1.a") & !is.na(value)]

plot.DSW.PD.region <- ggplot(data=plt.dat.DSW.PD1.region[year <= 2016]) + 
  geom_line(aes(x=year, y=value, colour=class3)) +  #linetype=Variable if needed
  geom_line(data=plt.dat.DSW.PD1.region[year >= 2016], aes(x=year, y=value, colour=class3), linetype = "dotted") +  #linetype=Variable if needed
  scale_y_continuous(name="Phylogenetic Diversity") +
  scale_x_continuous("Year", breaks=seq(1920, 2020, 10)) +
  scale_color_manual(name="Market Class", values=c("#e41a1c", "#4daf4a", "#377eb8"),
                     labels=c("Durum", "Spring", "Winter")) +
  facet_wrap(~state, ncol =1 ) +
  theme_bw() +
  theme(  #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    panel.border = element_rect(color="black"),
    plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) +
  theme(text=element_text(size=14),
        axis.text.x=element_text(angle=90, vjust=0.5, size=12),
        axis.title=element_text(face="bold"),
        strip.text.x = element_text(colour = "black"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.background = element_rect(fill="NA", colour = "NA")) 



plot.DSW.PD.region

save(plot.DSW.PD.region, file="./data_intermediate/plot.DSW.PD_region.RData")


# Save plot
png("./plot/04_PD1_DSW_region.png", width = 4, height = 16, units = "in", res= 300)
plot.DSW.PD.region
dev.off()


# PD1, PD2
plot.DSW.PD2.region <- ggplot(data=WVA.PD.D.long[Variable %in% c("PD1.a", "PD2.a") & !is.na(value)]) + 
  geom_line(aes(x=year, y=value, colour=class3, linetype=Variable)) +  #linetype=Variable if needed
  scale_y_continuous(name="Phylogenetic Diversity") +
  scale_x_continuous("Year", breaks=seq(1920, 2020, 10)) +
  scale_color_manual(name="Market Class", values=c("#e41a1c", "#4daf4a", "#377eb8"),
                     labels=c("Durum", "Spring", "Winter")) +
  # scale_linetype_manual(name="Diversity Index (order of 1)", values=c("solid", "longdash"), 
  #                       labels=c("Species Neutral Diversity", "Phylogenetic Diversity")) + 
  facet_wrap(~state) +
  theme_bw() +
  theme(  #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    panel.border = element_rect(color="black"),
    plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) +
  theme(text=element_text(size=14),
        axis.text.x=element_text(angle=90, vjust=0.5, size=12),
        axis.title=element_text(face="bold"),
        strip.text.x = element_text(colour = "black"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.background = element_rect(fill="NA", colour = "NA")) 



plot.DSW.PD2.region

# Save plot
png("./plot/04_PD1PD2_DSW_region.png", width = 10, height = 10, units = "in", res= 300)
plot.DSW.PD2.region
dev.off()
