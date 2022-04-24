# Create a panel data for analysis
# author: Yuan Chai
# email: chaix026@umn.edu

rm(list=ls())

library(data.table)

# ---- Load data----
load("./data_intermediate/01_1_WVA_data.Rdata")
WVA.DSW <- WVA.data.final


# Sort data
WVA.DSW <- WVA.DSW[order(year, state, area)]
WVA.DSW[, var_COP := var_std]

# ---- CreatePanel Fxn ----
CreatePanel <- function(temp.dt) {
  # Year list
  year.list <- unique(temp.dt$year)
  year.list <- year.list[order(year.list)]
  
  # State list
  state.list <- unique(temp.dt$state)
  state.list <- state.list[order(state.list)]
  
  # Variety list
  var.list <- unique(temp.dt$var_COP)
  var.list <- var.list[order(var.list)]
  
  # Create a complete panel data
  temp.panel <- as.data.table(expand.grid(year=year.list, state=state.list, var_COP=var.list))
  setkey(temp.panel, year, state, var_COP)
  setkey(temp.dt, year, state, var_COP)
  temp.panel <- merge(temp.panel, temp.dt, all.x=TRUE)
  temp.panel[, state:=factor(state, levels=state.list)]
  temp.panel[, var_COP:=factor(var_COP, levels=var.list)]
  
  return(temp.panel)
}


# ---- DSW panel ----
# Durum
WVA.Durum.panel <- CreatePanel(WVA.DSW[class3 == "Durum"])
save(WVA.Durum.panel, file="./data_intermediate/01_4_WVA_Durum_panel.Rdata")

# Spring
WVA.Spring.panel <- CreatePanel(WVA.DSW[class3 == "Spring"])
save(WVA.Spring.panel, file="./data_intermediate/01_4_WVA_Spring_panel.Rdata")

# Winter
WVA.Winter.panel <- CreatePanel(WVA.DSW[class3 == "Winter"])
save(WVA.Winter.panel, file="./data_intermediate/01_4_WVA_Winter_panel.Rdata")
