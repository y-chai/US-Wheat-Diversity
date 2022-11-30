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
load("./data_intermediate/04_02_PD_Durum.Rdata")
load("./data_intermediate/04_02_PD_Spring.Rdata")
load("./data_intermediate/04_02_PD_Winter.Rdata")

# Combine data into long format
PD.Durum.sum$class3 <- "Durum"
PD.Spring.sum$class3 <- "Spring"
PD.Winter.sum$class3 <- "Winter"
PD.DSW <- rbind(PD.Durum.sum, PD.Spring.sum, PD.Winter.sum) 
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

write.csv(sum_PD_st_yr, file="./data_final/04_02_summary_PD_summary.csv")
WVA.Diversity[, .(meanD0=mean(PD0.a, na.rm=T), sdD0=sd(PD0.a, na.rm=T),
                  meanD1=mean(PD1.a, na.rm=T), sdD0=sd(PD1.a, na.rm=T),
                  meanD2=mean(PD2.a, na.rm=T), sdD0=sd(PD2.a, na.rm=T)),
              by=.(class3)] # Overall summary by class


# Save data in long format
WVA.PD.long <- melt(WVA.Diversity[, .(state, year, class3, PD0.a, PD1.a, PD2.a)], 
                    id.vars = c("state", "year", "class3"),
                    variable.name = "Variable")
save(WVA.PD.long, file="./data_intermediate/04_02_PD_DSW_plot.RData")



#### Plot ####

# Load PD results
load("./data_intermediate/04_02_PD_DSW_plot.RData")
WVA.PD.D.long <- WVA.PD.long

# Plot2 based on US map
# map grid

# # grid customization
# my_grid <- us_state_grid1
# my_grid <- my_grid[(my_grid$name %in% unique(WVA.PD.D.long$state)), ]
# # grid_preview(my_grid)
# 
# my_grid <- as.data.table(my_grid)
# 
# # Reorganize PNW
# my_grid$col[my_grid$code == "ID"] <- 1
# my_grid$row[my_grid$code == "ID"] <- 4
# my_grid$row[my_grid$code == "CA"] <- 5
# 
# # Reorganize central states
# my_grid[col >= 3,  `:=`(col = col -1)]
# my_grid[col > 4,  `:=`(col = col -1)]
# 
# my_grid$col[my_grid$code == "TX"] <- 2
# my_grid$row[my_grid$code == "TX"] <- 5
# 
# my_grid$col[my_grid$code == "OK"] <- 4
# my_grid$row[my_grid$code == "OK"] <- 5

my_grid <- fread("./data_original/my_us_state_grid.csv")
DSW.colPal <- c("#E34A33", "#31A354", "#3182BD")

plt.dat.DSW.PD1 <- WVA.PD.D.long[Variable %in% c("PD1.a") & !is.na(value)]

plot.DSW.PD <- ggplot(data= plt.dat.DSW.PD1[year <= 2016]) + 
  geom_line(aes(x=year, y=value, colour=class3)) +  
  geom_line(data= plt.dat.DSW.PD1[year >= 2016], aes(x=year, y=value, colour=class3), linetype = "dotted") + 
  scale_y_continuous(name="Phylogenetic Diversity") +
  scale_x_continuous("Year", breaks=seq(1920, 2020, 10)) +
  scale_color_manual(name="Market Class", values=c("#e41a1c", "#4daf4a", "#377eb8"),
                     labels=c("Durum", "Spring", "Winter")) +
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
        legend.background = element_rect(fill="NA", colour = "NA")) +
  facet_geo(~state, grid = my_grid, label = "name")



plot.DSW.PD

save(plot.DSW.PD, file="./data_intermediate/plot.DSW.PD.RData")


# Save plot
png("./plot/04_PD1_DSW.png", width = 8, height = 16, units = "in", res= 300)
plot.DSW.PD
dev.off()


# PD1, PD2
plot.DSW.PD2 <- ggplot(data=WVA.PD.D.long[Variable %in% c("PD1.a", "PD2.a") & !is.na(value)]) + 
  geom_line(aes(x=year, y=value, colour=class3, linetype=Variable)) +  #linetype=Variable if needed
  scale_y_continuous(name="Phylogenetic Diversity") +
  scale_x_continuous("Year", breaks=seq(1920, 2020, 10)) +
  scale_color_manual(name="Market Class", values=c("#e41a1c", "#4daf4a", "#377eb8"),
                     labels=c("Durum", "Spring", "Winter")) +
  # scale_linetype_manual(name="Diversity Index (order of 1)", values=c("solid", "longdash"), 
  #                       labels=c("Species Neutral Diversity", "Phylogenetic Diversity")) + 
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
        legend.background = element_rect(fill="NA", colour = "NA")) +
#  facet_geo(~state, grid = my_grid, label = "name")
  facet_wrap(~state, ncol=4)


plot.DSW.PD2

# Save plot
png("./plot/04_PD1PD2_DSW.png", width = 8, height = 16, units = "in", res= 300)
plot.DSW.PD2
dev.off()
