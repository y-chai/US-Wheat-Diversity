# Descriptive analysis for US wheat diversity
# author: Yuan Chai
# email: chaix026@umn.edu

rm(list=ls())

library(data.table)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)

# Load data
load("./data_intermediate/01_1_WVA_data.Rdata")
WVA.data <- WVA.data.final
MV.list <- fread("./data_original/01_1a_MV.list.csv")

# Number of varieties
length(unique(WVA.data$var_std))
dat_Var_Class <- unique(WVA.data[, .(var_std, class3)])
nrow(dat_Var_Class)
dat_Var_Class[, .N, by=class3]
dat_Var_Class[, .(N_Per = .N/nrow(dat_Var_Class)), by=class3]

# Number of states by class by year
WVA.data.st.avail <- WVA.data[, .(DataAvail = T ), by=.(year, state, class3)]

ggplot(WVA.data.st.avail) +
  geom_tile(aes(x=year, y=class3, fill=class3)) +
  facet_wrap(~state)

WVA.data.avail <- WVA.data.st.avail[, .N, by=.(year, class3)]

ggplot(WVA.data.avail) +
  geom_point(aes(x=year, y=N, color=class3), size=2) +
  scale_x_continuous("Year", breaks = seq(1920, 2019, by=5)) +
  scale_y_continuous("Number of States", breaks = seq(2, 16, by=2)) +
  scale_color_manual(name="Market Class", values=c("#e41a1c", "#4daf4a", "#377eb8"),
                     labels=c("Durum", "Spring", "Winter")) +
  theme_bw() +
  theme(text=element_text(size=18),
        axis.text.x=element_text(angle=90, vjust=0.5, colour = "black"),
        axis.title=element_text(face="bold", colour = "black"),
        strip.text.x = element_text(colour = "black"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.background = element_rect(fill="NA", colour = "NA"))

ggsave("./plot/02_Data Availability.png")

# Area by variety by year (aggregate over states)
WVA.data.bY <- WVA.data[, .(TotArea=sum(area)), by=.(year, var_std)
                        ][order(year, var_std)][, var_std := factor(var_std, levels=unique(var_std))]

WVA.data.bY <- WVA.data[, .(TotArea=sum(area)), by=.(year, var_std)][order(year, var_std)]
WVA.data.bY <- merge(WVA.data.bY, MV.list[, .(var_std, Year0)], by="var_std", all = FALSE, all.x = T)
WVA.data.bY <- WVA.data.bY[order(year, var_std)]

# Area share of each variety
WVA.data.bY[, YrTotArea := sum(TotArea), by=year][, AreaShr := TotArea/YrTotArea]
summary(WVA.data.bY$AreaShr)

# varieties by dominance group
WVA.data.bY[, AreaShrGrp := cut(AreaShr, breaks = c(0, 0.0005, 0.005, 0.01, 0.05, 0.5), dig.lab = 3)]
WVA.data.bY <- WVA.data.bY[order(year, -TotArea)]
WVA.data.bY[, TopN := 1:.N, by=year][, TopNGrp := cut(TopN, breaks = c(0, 5, 10, 50, 100, 350))]

# variety age
WVA.data.bY[, Yr1 := min(year), by=.(var_std)]
WVA.data.bY[is.na(Year0), Year0 := Yr1][, VarAge := (year-Year0+1)]

# ************* 
# Figure 1 ----
# *************
## Dynamics of top varieties 

# Top N varieties each year
topN <- 5

yr.labels = data.table(yr = unique(WVA.data.bY$year))
yr.labels[, yr.n := yr %% 5 ]
yr.labels[, yr.lab := ""]
yr.labels[yr.n == 0, yr.lab := as.character(yr)]
yr.lab <- yr.labels$yr.lab
new.yr.lab <- yr.lab

# TopN 
WVA.data.bY[, MinN := min(TopN), by=.(var_std)]
var.tN.sel <- unique(WVA.data.bY[MinN <= 5, .(var_std)])

# MiddleN
var.m20N.sel <- unique(WVA.data.bY[(MinN > 5 & MinN <= 20), .(var_std)])

# BottomN
var.bN.sel <- unique(WVA.data.bY[MinN > 20, .(var_std)])

# # Number of years a topN variety stays topN
WVA.data.bY[, TopVarStd := var_std]
WVA.data.bY[ (TopVarStd %in% var.m20N.sel$var_std), TopVarStd := "OTHER-TOP20-VARIETIES"]
WVA.data.bY[ (TopVarStd %in% var.bN.sel$var_std), TopVarStd := "OTHER-SMALL-VARIETIES"]

# Prepare data for plotting
WVA.tN.plot <- WVA.data.bY[, .(TotArea = sum(TotArea)), by=.(year, TopVarStd)]

# Merge with NASS data to identify "OTHER_UNKNOWN" category
load("./data_final/01_3_data.survey.St.wide.RData")

# State List by wheat classes (manually selcted based on the "summary_NoV_st_class.csv")
durum.state <- c("California", "Montana", "North Dakota", "South Dakota")
spring.state <- c("Idaho", "Minnesota", "Montana", "North Dakota",
                  "Oregon", "South Dakota", "Washington")
winter.state <- c("California", "Colorado", "Idaho", "Indiana", "Kansas", "Kentucky",  
                  "Montana", "Nebraska", "North Dakota", "Oklahoma", "Oregon", "South Dakota", "Texas",       
                  "Washington", "Wyoming")
state.sel <- unique(c(durum.state, spring.state, winter.state))

data.survey.St.wide[is.na(data.survey.St.wide)] <- 0
data.survey.St.wide[, TotAreaNASS := 0]
data.survey.St.wide[StateName %in% toupper(durum.state), TotAreaNASS := TotAreaNASS + PlantAcre.durum.S]
data.survey.St.wide[StateName %in% toupper(spring.state), TotAreaNASS := TotAreaNASS + PlantAcre.spring.S]
data.survey.St.wide[StateName %in% toupper(winter.state), TotAreaNASS := TotAreaNASS + PlantAcre.winter.S]

dat.NASS.tot <- data.survey.St.wide[, .(TotAreaNASS = sum(TotAreaNASS)), by=.(year=CropYear)]
dat.WVA.tot <- WVA.tN.plot[, .(TotAreaWVA = sum(TotArea)), by=.(year)]
dat.Other.tot <- merge(dat.WVA.tot, dat.NASS.tot, by="year", all.x=T)
dat.Other.tot <- dat.Other.tot[, .(TopVarStd="OTHER-UNKNOWN", TotArea=max(TotAreaNASS-TotAreaWVA, 0)), by=.(year)]


# WVA.tN.plot <- rbind(WVA.tN.plot, dat.Other.tot)
WVA.tN.plot[, TopVarStd := factor(TopVarStd, levels = c(var.tN.sel$var_std, "OTHER-TOP20-VARIETIES", "OTHER-SMALL-VARIETIES"))]
WVA.tN.plot[, AreaShare := TotArea / sum(TotArea), by=.(year)]
WVA.tN.plot <- WVA.tN.plot[order(year, TopVarStd)]


# Create plot
fx_topN.plot <- function(data.tN.plot, lgd.title, lgd.row, positionchoice) {
  
  NumColor <- length(unique(data.tN.plot$TopVarStd))
  ColorChoice <- c(colorRampPalette(brewer.pal(9, "Set1"))(NumColor-1), "#bdbdbd")
  
  
  ggplot(data.tN.plot, aes(x=as.character(year), y=AreaShare, fill=TopVarStd)) +
    geom_bar(stat="identity", color="grey90", position = positionchoice) +
    scale_fill_manual(name = lgd.title, values = ColorChoice) +
    scale_y_continuous(name="Acreage Share") +
    scale_x_discrete(labels = new.yr.lab) +
    xlab("")+
    # scale_x_continuous(breaks=c(seq(1919, 1984, 5), seq(1986, 2016, 2))) +
    theme_bw() +
    theme(#panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      panel.border = element_rect(color="black"),
      plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) +
    theme(text=element_text(size=20),
          axis.text.x=element_text(angle=90, vjust=0.5, colour = "black", face="bold"),
          axis.title=element_text(face="bold"),
          strip.text.x = element_text(colour = "black"),
          legend.position = "right",
          legend.justification = 'left',
          legend.background = element_rect(fill="NA", colour = "grey"),
          legend.text=element_text(size=12),
          legend.title = element_text(size = 12, face="bold"),
          legend.key.width = unit(0.3, "cm"),
          legend.key.height = unit(0.5, "cm"))+
    guides(fill=guide_legend(nrow=lgd.row,byrow=F))
}


p.topN <- fx_topN.plot(WVA.tN.plot, "Top Varieties", 10, "stack") +
  theme(legend.position = "bottom")
p.topN

# Save figure
png("./Plot/02_Figure 1.png", width = 16, height = 10, units = "in", res= 300)
p.topN
dev.off()

# Save data for figure
write.csv(WVA.tN.plot, file="./data_final/02_0_WVA.tN.plot.csv", row.names = F)


# *************
# Figure 2 ----
# *************
 # Area weighted variety age 
dat.AWA <- WVA.data.bY[, .(AWA = sum(AreaShr * VarAge)), by=.(year)]

# Proportion of new and old varieties
dat.PNV <- WVA.data.bY[VarAge <= 5, .(PNV = sum(AreaShr)), by=.(year)]
dat.POV <- WVA.data.bY[VarAge >= 15, .(POV = sum(AreaShr)), by=.(year)]

# Proportin of Top 5 varieties
dat.PTV <- WVA.data.bY[TopN <= 5, .(PTV = sum(AreaShr)), by=.(year) ]

# number of varieties per million acres
WVA.bY.bA <- WVA.data.bY[, AgeGroup := cut(VarAge, c(0, 5, 10, 15, Inf), 
                                       labels = c("1-5 years", "6-10 years", "11-15 years", ">15 years"))
                     ][, .(NumVar = length(unique(var_std)), 
                           AgeArea = sum(TotArea)), by=.(year, AgeGroup)
                       ]
WVA.bY.bA[, TotArea := sum(AgeArea), by=.(year)]
dat.NPM <- WVA.bY.bA[, .(NPM = sum(NumVar) / (sum(AgeArea)/10^6),
                         NVar = sum(NumVar),
                         Area = sum(AgeArea)), by=.(year)]

# ---- Combo plot ---

p.VarietyNewnessCombo <- 
  ggplot() +
  geom_line(data=dat.PTV, aes(x=year,y=PTV, color="Proportion of Top 5 Varieties", linetype="Proportion of Top 5 Varieties"), size=1) +
  geom_line(data=dat.PNV, aes(x=year,y=PNV, color="Proportion of New Varieties",  linetype="Proportion of New Varieties"), size=1)+
  geom_line(data=dat.AWA, aes(x=year,y=AWA/40, color="Weighted Average Age (Years)", linetype="Weighted Average Age (Years)"), size=1) +
  geom_line(data=dat.NPM, aes(x=year,y=NPM/40, color="Number of Varieties Per Million Acres",  linetype="Number of Varieties Per Million Acres"), size=1) +
  scale_x_continuous(name="Year", breaks=seq(1920, 2020, 5)) +
  scale_y_continuous(limits = c(0, 1), name="Proportion of area planted",
                     sec.axis = sec_axis(~.*40, name="Years, or Number of Varieties")) +
  scale_colour_manual(values = c("black", "#377eb8", "#4daf4a",  "#e41a1c"), name=NULL) + 
  scale_linetype_manual(values=c("longdash", "solid", "dotted", "dashed"),name=NULL) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(color="black"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) +
  theme(text=element_text(size=14),
        axis.text.x=element_text(angle=90, hjust=0.5, vjust=0.5),
        axis.title=element_text(face="bold")) +
  theme(legend.justification=c(1,-0.2), 
        legend.position=c(0.9,0.7), 
        legend.text=element_text(size=12), 
        legend.title=element_text(size=12), 
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size = 14), 
        #        legend.key = element_blank(), 
        legend.background = element_rect(color="grey", size = 0.1),
        legend.key.width=unit(4,"line"))

p.VarietyNewnessCombo

png("./plot/02_Figure 2.png", width = 10, height = 6, units = "in", res= 300)
p.VarietyNewnessCombo
dev.off()

# Export data for figure 2
dat.plot.combo <- merge(dat.AWA, merge(dat.NPM, merge(dat.PTV, merge(dat.PNV, dat.POV, by="year"), by="year"), by="year"), by="year") 
write.csv(dat.plot.combo, file="./data_final/02_0_plot_combo.csv", row.names = F)


# Stats ----
# Variety longevity
dat.longevity <- WVA.data[, .(area = sum(area)), by=.(var_std, year, class3)
                          ][, .(longevity = .N), by=.(var_std, class3)]
dat.longevity[longevity >= 15]
dat.longevity[longevity == 1]
