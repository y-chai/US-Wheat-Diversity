# Plot the PD decomposition results
# author: Yuan Chai
# email: chaix026@umn.edu

rm(list=ls())

library(data.table)
library(ggplot2)
library(gridExtra)


# PD combo plot
load("./data_intermediate/plot.DSW.PD.RData")
load("./data_intermediate/plot.DSW.PD_region.RData")

plot.DSW.PD.region <- plot.DSW.PD.region +
  theme(legend.position = "none", plot.margin = margin(0, 0.1, 0.5, 0, "in"))

plot.DSW.PD <- plot.DSW.PD +
  scale_y_continuous(name=" ") +
  theme(plot.margin = margin(0, 0.1, 0, 0, "in"))


png("./plot/06_PD1_combo.png", width=9, height=12, units="in", res=300)

grid.arrange(plot.DSW.PD.region, plot.DSW.PD, ncol=2, widths = 2:3)

dev.off()

# Temporal decompose combo plot
load("./data_intermediate/plt_PD1_Decomp_DSW_st.RData")
load("./data_intermediate/plt_PD1_Decomp_DSW_st_region.RData")


plt_PD1_Decomp_DSW_st_region <- plt_PD1_Decomp_DSW_st_region +
  theme(legend.position = "none", plot.margin = margin(0, 0.1, 0.5, 0, "in"))

plt_PD1_Decomp_DSW_st <- plt_PD1_Decomp_DSW_st +
  scale_x_continuous(name=expression(paste("State-level ", alpha, " diversity"))) +
  theme(plot.margin = margin(0, 0.1, 0, 0, "in"))

png("./plot/06_PD1_Decomp_DSW_st_combo.png", width=9, height=12, units="in", res=300)

grid.arrange(plt_PD1_Decomp_DSW_st_region, plt_PD1_Decomp_DSW_st, ncol=2, widths = 2:3)

dev.off()
