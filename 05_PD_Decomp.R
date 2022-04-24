#### Calculate the PD decomposition
# author: Yuan Chai
# email: chaix026@umn.edu


rm(list=ls())

library(entropart)
library(data.table)

#### Load DSW data ####
load("./data_intermediate/01_4_WVA_Durum_panel.Rdata")
load("./data_intermediate/01_4_WVA_Spring_panel.Rdata")
load("./data_intermediate/01_4_WVA_Winter_panel.Rdata")

#### Load COP data ####
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

####################
# Read in panel data

for (wheat.class in c("Spring", "Winter", "Durum")) {
  if(wheat.class == "Spring") {
    WVA.panel <- WVA.Spring.COP  ############### Change Accordingly
  } else if (wheat.class == "Winter") {
    WVA.panel <- WVA.Winter.COP  ############### Change Accordingly
  } else {
    WVA.panel <- WVA.Durum.COP  ############### Change Accordingly
  }
  
  # WVA.panel <- WVA.Spring.COP  ############### Change Accordingly
  WVA.panel[is.na(WVA.panel)] <- 0
  
  year.list <- unique(WVA.panel$year)
  state.list <- unique(WVA.panel$state)
  
  ############################
  # Spatial Diversity by Year
  ############################
  
  # List to store average alpha(a), beta(b) and gamma(r)
  m.abr <- matrix(ncol=length(year.list), nrow=3)
  colnames(m.abr) <- year.list
  rownames(m.abr) <- c("alpha", "beta", "gamma")
  D_yr.abr <- list(D0=m.abr, D1=m.abr, D2=m.abr)
  
  rm(m.abr)
  
  # Calculate 
  
  q.seq <- c(0, 1, 2) # Diversity Orders
  
  for (t in 1:length(year.list)){ # Loop through years
    
    yr <- year.list[t]
    
    # If 1985, skip because of no data
    if(yr == "2020") {
      
      print("end")
      next
      
    } else {
      
      print(yr)
      
      # Year t matrix
      WVA.yr <- dcast(WVA.panel[year==yr], var_COP ~ state, value.var = "area")
      rownames(WVA.yr) <- WVA.yr$var_COP
      WVA.yr$var_COP <- NULL
      
      ###################################
      # Alpha Diversity for communities #
      ###################################
      # MetaCommunity for Year t
      MC.yr <- MetaCommunity(WVA.yr)
      
      ##############################
      # Beta and Gamma Diversities #
      ##############################
      # Select only states with non-all-zero data
      WVA.yr.sel <- WVA.yr[, colSums(WVA.yr) > 0, with=FALSE]
      N.sel <- ncol(WVA.yr.sel)
      
      if(N.sel < 2) {
        print(paste("Year ", yr, " Less than 2 states"))
        next
        
      } else {
        # MetaCommunity for Year t
        rownames(WVA.yr.sel) <- rownames(WVA.yr)
        MC.yr.sel <- MetaCommunity(WVA.yr.sel)
        
        
        for (qq in 1:length(q.seq)) {
          # Diversity Decompose
          D.yr.abr <- DivPart(q=q.seq[qq], MC=MC.yr.sel, Biased = TRUE, Correction = "None", Tree = PT.var.upgma.DSW)
          
          # Save data
          D_yr.abr[[qq]][, t] <- c(D.yr.abr$TotalAlphaDiversity,
                                   D.yr.abr$TotalBetaDiversity,
                                   D.yr.abr$GammaDiversity)
          print(paste("Meta Community: order", q.seq[qq]))
        }
      }
      
      
      # Clear space
      rm(MC.yr, MC.yr.sel, D.yr.ma, D.yr.abr)
    }
    
  }
  
  # Export data
  save(D_yr.abr, file=paste0("./data_intermediate/PD_", wheat.class, "_by_year_abr.RData"))           #Change as need
  write.csv(D_yr.abr[[1]], file=paste0("./data_intermediate/PD0_", wheat.class, "_by_year_abr.csv"))  #Change as need
  write.csv(D_yr.abr[[2]], file=paste0("./data_intermediate/PD1_", wheat.class, "_by_year_abr.csv"))  #Change as need
  write.csv(D_yr.abr[[3]], file=paste0("./data_intermediate/PD2_", wheat.class, "_by_year_abr.csv"))  #Change as need
  
}


# ###################################
# # Temporal Diversity by State
# ###################################

for (wheat.class in c("Spring","Winter", "Durum")) {
  if(wheat.class == "Spring") {
    WVA.panel <- WVA.Spring.COP  ############### Change Accordingly
    print("Spring Wheat")
  } else if (wheat.class == "Winter") {
    WVA.panel <- WVA.Winter.COP  ############### Change Accordingly
    print("Winter Wheat")
  } else {
    WVA.panel <- WVA.Durum.COP  ############### Change Accordingly
    print("Durum Wheat")
  }
  
  # WVA.panel <- WVA.Spring.COP  ############### Change Accordingly
  WVA.panel[is.na(WVA.panel)] <- 0
  
  year.list <- unique(WVA.panel$year)
  state.list <- unique(WVA.panel$state)
  #state.list <- c("North Dakota", "South Dakota")
  
  ###################################
  # Temporal Diversity by State
  ###################################
  
  # Initialize matrices and list
  # List to store average alpha, beta and gamma
  m.abr <- matrix(ncol=length(state.list), nrow=3)
  colnames(m.abr) <- state.list
  rownames(m.abr) <- c("alpha", "beta", "gamma")
  D_st.abr <- list(D0=m.abr, D1=m.abr, D2=m.abr)
  
  rm(m.abr)
  
  # Calculate
  
  q.seq <- c(0, 1, 2) # Diversity Orders
  
  for (s in 1:length(state.list)){ # Loop through states
    
    st <- state.list[s]
    print(as.character(st))
    
    # State s matrix
    WVA.st <- dcast(WVA.panel[state==st], var_COP ~ year, value.var = "area")
    rownames(WVA.st) <- WVA.st$var_COP
    WVA.st$var_COP <- NULL
    
    
    ##############################
    # Diversities #
    ##############################
    # Select only years with non-all-zero data
    WVA.st.sel <- WVA.st[, colSums(WVA.st) != 0, with=FALSE]
    rownames(WVA.st.sel) <- rownames(WVA.st)
    N.sel <- ncol(WVA.st.sel)
    
    # MetaCommunity over years
    MC.st.sel <- MetaCommunity(WVA.st.sel)
    
    
    for (qq in 1:length(q.seq)) {
      # Diversity Decompose
      D.st.abr <- DivPart(q=q.seq[qq], MC=MC.st.sel, Biased = TRUE, Correction = "None", Tree = PT.var.upgma.DSW)
      
      # Save data
      D_st.abr[[qq]][, s] <- c(D.st.abr$TotalAlphaDiversity,
                               D.st.abr$TotalBetaDiversity,
                               D.st.abr$GammaDiversity)
      print(paste("Meta Community: order", q.seq[qq]))
    }
    
    
    # Clear space
    rm(MC.st.sel, D.st.abr)
  }
  
  
  # Export data
  save(D_st.abr, file=paste0("./data_intermediate/PD_", wheat.class, "_by_state_abr.RData"))          #Change as need
  write.csv(D_st.abr[[1]], file=paste0("./data_intermediate/PD0_", wheat.class, "_by_state_abr.csv"))  #Change as need
  write.csv(D_st.abr[[2]], file=paste0("./data_intermediate/PD1_", wheat.class, "_by_state_abr.csv"))  #Change as need
  write.csv(D_st.abr[[3]], file=paste0("./data_intermediate/PD2_", wheat.class, "_by_state_abr.csv"))  #Change as need
  
  
}

