# Plot the PD decomposition results
# author: Yuan Chai
# email: chaix026@umn.edu

rm(list=ls())

library(data.table)
library(ggplot2)
library(RColorBrewer)
library(maps)
library(geofacet)
library(usdata)

# ##############################
# ### Spatial Decomposition ####
# ##############################
# # Load decomposition results (by year)
# load("./data_intermediate/PD_Durum_by_year_abr_region.RData")
# PD.Decom.Durum.yr.abr <- D_yr.abr
# load("./data_intermediate/PD_Spring_by_year_abr_region.RData")
# PD.Decom.Spring.yr.abr <- D_yr.abr
# load("./data_intermediate/PD_Winter_by_year_abr_region.RData")
# PD.Decom.Winter.yr.abr <- D_yr.abr
# 
# 
# # Function to extract decomposition data
# fx_exDecom_yr <- function(PD.Class.yr, q=1, mktclass) {
#   PD.Decom.Class.yr <- as.data.table(t(PD.Class.yr[[q+1]]), keep.rownames="year")
#   PD.Decom.Class.yr$q <- q
#   PD.Decom.Class.yr$class3 <- mktclass
#   return(PD.Decom.Class.yr)
# }
# 
# PD1.Decom.Durum.yr <- fx_exDecom_yr(PD.Decom.Durum.yr.abr, 1, "Durum")
# PD1.Decom.Spring.yr <- fx_exDecom_yr(PD.Decom.Spring.yr.abr, 1, "Spring")
# PD1.Decom.Winter.yr <- fx_exDecom_yr(PD.Decom.Winter.yr.abr, 1, "Winter")
# PD1.Decom.DSW.yr <- rbind(PD1.Decom.Durum.yr, PD1.Decom.Spring.yr, PD1.Decom.Winter.yr)
# PD1.Decom.DSW.yr.long <- melt(PD1.Decom.DSW.yr, id.vars = c("year", "class3", "q"),
#                               variable.name = "Div")
# PD1.Decom.DSW.yr.long <- PD1.Decom.DSW.yr.long[!is.na(PD1.Decom.DSW.yr.long$value)]
# PD1.Decom.DSW.yr.long$Div <- factor(PD1.Decom.DSW.yr.long$Div, labels = c("alpha~Diversity~(effective~varieties~per~state)",
#                                                                           "beta~Diversity~(effective~states)",
#                                                                           "gamma~Diversity~(overall)"))
# 
# # Plot 
# 
# yr.labels = data.table(yr = sort(unique(PD1.Decom.DSW.yr.long$year)))
# yr.labels[, yr.n := as.numeric(yr) %% 5 ]
# yr.labels[, yr.lab := ""]
# yr.labels[yr.n == 0, yr.lab := as.character(yr)]
# yr.lab <- yr.labels$yr.lab
# 
# 
# # # Save figure
# # png("./plot/PD1_Decomp_DSW_yr_region.png", width = 10, height = 10, units = "in", res= 300)
# # 
# # ggplot() +
# #   geom_line(data=PD1.Decom.DSW.yr.long[year <= 2016], aes(x=year, y=value, group=class3, color=class3), size=0.8) +
# #   geom_line(data=PD1.Decom.DSW.yr.long[year >= 2016], aes(x=year, y=value, group=class3, color=class3), linetype = "dotted", size=0.8) +
# #   facet_wrap(~Div, ncol=1, scales = "free_y", labeller= label_parsed) +
# #   scale_y_continuous(name="Diversity") +
# #   scale_x_discrete("Year", labels = yr.lab) +
# #   scale_color_manual(name="Market Class", values=c("#e41a1c", "#4daf4a", "#377eb8"),
# #                      labels=c("Durum", "Spring", "Winter")) +
# #   theme_bw() +
# #   theme(  #panel.grid.major = element_blank(),
# #     panel.grid.minor = element_blank(),
# #     strip.background = element_blank(),
# #     panel.border = element_rect(color="black"),
# #     plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) +
# #   theme(text=element_text(size=18),
# #         axis.text.x=element_text(angle=90, vjust=0.5, colour = "black"),
# #         axis.title=element_text(face="bold", colour = "black"),
# #         strip.text.x = element_text(colour = "black"),
# #         legend.position = "bottom",
# #         legend.direction = "horizontal",
# #         legend.box = "horizontal",
# #         legend.background = element_rect(fill="NA", colour = "NA"))
# # 
# # dev.off()

##############################
### Temporal Decomposition ###
##############################
# Load decomposition results (by state)
load("./data_intermediate/PD_Durum_by_state_abr_region.RData")
PD.Decom.Durum.st.abr <- D_st.abr

load("./data_intermediate/PD_Spring_by_state_abr_region.RData")
PD.Decom.Spring.st.abr <- D_st.abr

load("./data_intermediate/PD_Winter_by_state_abr_region.RData")
PD.Decom.Winter.st.abr <- D_st.abr

# Function to extract decomposition data

fx_exDecom_st <- function(PD.Class.st, q=1, mktclass) {
  PD.Decom.Class.st <- as.data.table(t(PD.Class.st[[q+1]]), keep.rownames="state")
  PD.Decom.Class.st$q <- q
  PD.Decom.Class.st$class3 <- mktclass
  return(PD.Decom.Class.st)
}

PD1.Decom.Durum.st <- fx_exDecom_st(PD.Decom.Durum.st.abr, 1, "Durum")
PD1.Decom.Spring.st <- fx_exDecom_st(PD.Decom.Spring.st.abr, 1, "Spring")
PD1.Decom.Winter.st <- fx_exDecom_st(PD.Decom.Winter.st.abr, 1, "Winter")
PD1.Decom.DSW.st <- rbind(PD1.Decom.Durum.st, PD1.Decom.Spring.st, PD1.Decom.Winter.st)

PD1.Decom.DSW.st[state == "Central", state := "Central Plains"]
PD1.Decom.DSW.st[state == "North Central", state := "Northern Plains"]

write.csv(PD1.Decom.DSW.st, file="./data_final/04_03_PD1.Decom.DSW.st.region.csv")

# Plot

DSW.colPal <- c("#E34A33", "#31A354", "#3182BD")

# Save figure


PD1.Decom.DSW.st[, state:= factor(state, levels=c("Central Plains", "Northern Plains", "Pacific Northwest", "Southeast", "California"))]

plt_PD1_Decomp_DSW_st_region <- ggplot(data=PD1.Decom.DSW.st) + 
  geom_rect(aes(xmin=0, xmax=alpha, ymin=0, ymax=beta, color=class3, fill=class3), alpha=0.1,
            size = 0.8, linetype = 1) +
  scale_y_continuous(name=expression (paste(beta, " diversity (effective epochs)"))) +
  scale_x_continuous(name=expression(paste("Regional ", alpha, " diversity")), breaks = c(seq(0, 12, 2))) +
#  scale_x_continuous(name=" ", breaks = c(seq(0, 12, 2))) +
  scale_color_manual(name="Market Class", values=c("#e41a1c", "#4daf4a", "#377eb8"),
                     labels=c("Durum", "Spring", "Winter")) +
  scale_fill_manual(name="Market Class", values=c("#e41a1c", "#4daf4a", "#377eb8"),
                    labels=c("Durum", "Spring", "Winter")) +
  facet_wrap(~state, ncol =1 ) +
  theme_bw() +
  theme(  #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    panel.border = element_rect(color="black"),
    plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) +
  theme(text=element_text(size=18),
        axis.text.x=element_text(hjust=0.5),
        axis.title=element_text(face="bold"),
        strip.text.x = element_text(colour = "black"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.background = element_rect(fill="NA", colour = "NA")) 

save(plt_PD1_Decomp_DSW_st_region, file="./data_intermediate/plt_PD1_Decomp_DSW_st_region.RData")

# Export figure
png("./plot/05_PD1_Decomp_DSW_st_region.png", width=4, height=16, units="in", res=300)
plt_PD1_Decomp_DSW_st_region
dev.off()
