# Calculate Phylogenetic Diversity (PD)
# author: Yuan Chai
# email: chaix026@umn.edu

rm(list=ls())

library(data.table)
library(entropart)

#### Load DSW data ####
load("./data_intermediate/01_4_WVA_Durum_panel.Rdata")
load("./data_intermediate/01_4_WVA_Spring_panel.Rdata")
load("./data_intermediate/01_4_WVA_Winter_panel.Rdata")

#### Load COP data ####

# load("./Mid Data/01_2_PT.var.sel.upgma.DSW.RData")      # upgma tree for selected (pedigree known varieties)
load("./data_intermediate/01_2_PT.var.sel.upgma.RData")      # upgma tree for selected (pedigree known varieties)
PT.var.upgma.DSW <- PT.var.sel.upgma
print("finish loading data")

#### Match COP with area data

var.list <- PT.var.upgma.DSW$tip.label

# ---- CreatePanel Fxn ----

CreatePanel <- function(temp.dt) {
  # Year list
  year.list <- unique(temp.dt$year)
  year.list <- year.list[order(year.list)]
  
  # State list
  state.list <- unique(temp.dt$state)
  state.list <- state.list[order(state.list)]
  
  # Create a complete panel data
  temp.panel <- as.data.table(expand.grid(year=year.list, state=state.list, var_COP=var.list))
  setkey(temp.panel, year, state, var_COP)
  setkey(temp.dt, year, state, var_COP)
  temp.panel <- merge(temp.panel, temp.dt, all.x=TRUE)
  temp.panel[, state:=factor(state, levels=state.list)]
  temp.panel[, var_COP:=factor(var_COP, levels=var.list)]
  
  return(temp.panel)
}

WVA.Durum.COP <- CreatePanel(WVA.Durum.panel[, .(year, state, var_COP, area)])
WVA.Durum.COP[is.na(WVA.Durum.COP)] <- 0

WVA.Spring.COP <- CreatePanel(WVA.Spring.panel[, .(year, state, var_COP, area)])
WVA.Spring.COP[is.na(WVA.Spring.COP)] <- 0

WVA.Winter.COP <- CreatePanel(WVA.Winter.panel[, .(year, state, var_COP, area)])
WVA.Winter.COP[is.na(WVA.Winter.COP)] <- 0

print("finish create panel")


#############################################################
# Function to calcuate PD

fx_PD <- function(WVA.Class.COP) {
  
  # Year and state list
  Class.year <- unique(WVA.Class.COP$year)
  Class.state <- unique(WVA.Class.COP$state)
  
  # Calculate PD, in single loop
  PD.Class.sum <- as.data.frame(matrix(NA, nrow= length(Class.state) * length(Class.year), ncol=5,
                                       dimnames = list(c(), c("state", "year", "PD0", "PD1", "PD2"))))
  Nr <- 0
  print("Start Calculating PD")
  for (s in Class.state) {
    # State loop
    cat(paste("\nRow", Nr+1, s, "\n"))
    
    for (y in Class.year) {
      # Year loop
      cat(paste(y, ""))
      
      # Get the state-year data
      Ns <- as.AbdVector(WVA.Class.COP[state == s & year == y, .(area)])
      names(Ns) <- var.list
      Ps <- as.ProbaVector(Ns)
      
      # Calculate PD0, PD1 and PD2
      if( sum(Ns) != 0 ) {
        PD0 <- ChaoPD(Ps, q=0, PhyloTree=PT.var.upgma.DSW)
        PD1 <- ChaoPD(Ps, q=1, PhyloTree=PT.var.upgma.DSW)
        PD2 <- ChaoPD(Ps, q=2, PhyloTree=PT.var.upgma.DSW)
      } else {
        PD0 <- 0
        PD1 <- 0
        PD2 <- 0
      }
      
      # Row number for final result
      Nr <- Nr +1
      
      # Store final result
      PD.Class.sum[Nr, ] <- c(s, y, PD0, PD1, PD2)
    }
  }
  
  return(PD.Class.sum)
}


PD.Durum.sum <- fx_PD(WVA.Durum.COP)
PD.Spring.sum <- fx_PD(WVA.Spring.COP)
PD.Winter.sum <- fx_PD(WVA.Winter.COP)


save(PD.Durum.sum, file="./data_intermediate/04_02_PD_Durum.Rdata")
save(PD.Spring.sum, file="./data_intermediate/04_02_PD_Spring.Rdata")
save(PD.Winter.sum, file="./data_intermediate/04_02_PD_Winter.Rdata")
