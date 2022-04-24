# read in US wheat area-by-variety data and pre-process
# author: Yuan Chai
# email: chaix026@umn.edu

rm(list = ls())

# libraries
library(data.table)
library(ggplot2)
library(stringr)
library(usdata)
library(imputeTS)


# import data ----
# area-by-variety data
WVA.data <- fread("./data_original/01_WVA.data.csv")

# variety release years
MV.list <- fread("./data_original/01_1a_MV.list.csv")

# variety aliases
gris_alias <- fread("./data_original/1.0_alias_reformat.tsv", sep="\t")


# Redistribute mixture into equal shares ----
sel.mix <-  grep("^MIX:", unique(WVA.data$var_std), value=TRUE)
WVA.data.uni <- WVA.data[! (var_std %in% sel.mix)]
WVA.data.mix <- WVA.data[(var_std %in% sel.mix)]

# Function: Split the mixture
fx.MixSplit <- function(x){
  x <- setDF(x)
  var_split <- unlist(strsplit(x[,"var_std"], split=":"))[-1]
  temp <- data.table("state_abb"=x[, "state_abb"],"state"=x[, "state"], "year"=x[,"year"], "var_std" = var_split, "class3"=x[, "class3"],
                     "area" = round(x[,"area"]/(length(var_split)), 2))
  return(temp)
}

WVA.data.mix.split <- data.frame()
for (i in 1:nrow(WVA.data.mix)){
  temp.split <- fx.MixSplit(WVA.data.mix[i, ])
  WVA.data.mix.split <- rbind(WVA.data.mix.split, temp.split)
}

rm(temp.split)

# Integrate mix data back
WVA.data <- rbind(WVA.data.uni, WVA.data.mix.split)
WVA.data <- WVA.data[, .(area=sum(area, na.rm=T), class3=first(class3)), by=.(state_abb, state, year, var_std)]
rm(WVA.data.mix, WVA.data.mix.split, WVA.data.uni)

# Interpolate for missing years ----
ST.list <- unique(WVA.data[, .(state, state_abb)])

# For each state, interpolate missing years to build a complete panel

p.WVA <- data.table()


for (ST.sel in ST.list$state_abb) {
  
  print(ST.sel)
  # Select data for a state
  WVA.ST <- WVA.data[state_abb == ST.sel,]
  
  # Create panel
  yr.range <- unique(WVA.ST$year)
  var.list <- unique(WVA.ST[ , .(var_std, class3)])
  p.WVA.ST <- as.data.table(expand.grid(var_std=var.list$var_std, year=min(yr.range):max(yr.range)))
  
  # Merge in exisiting data and keep track
  p.WVA.ST <- merge(p.WVA.ST, WVA.ST[, .(var_std, year, area)], 
                    by.x=c("var_std", "year"), by.y=c("var_std", "year"), all.x=T)
  
  # Get first and last years when data available
  p.WVA.ST <- merge(p.WVA.ST, MV.list, by=c("var_std"), all.x=T)
  # p.WVA.ST[, YearOri := FALSE]
  p.WVA.ST[ year %in% yr.range, YearOri := TRUE]
  p.WVA.ST[ YearOri & is.na(area), area := 0]
  p.WVA.ST[year < Year0 & is.na(area), area := 0]
  p.WVA.ST[year > YearN & is.na(area), area := 0]
  
  # Interpolate the data
  p.WVA.ST[, area := na_interpolation(ts(area)), by=.(var_std)]
  p.WVA.ST[, state_abb := ST.sel]
  
  
  # Save the data
  p.WVA.ST <- merge(p.WVA.ST, var.list, all.x=T)
  p.WVA <- rbind(p.WVA, p.WVA.ST)
  
  # Remove and iterate
  rm(yr.range, var.list, WVA.ST, p.WVA.ST)
}

p.WVA <- merge(p.WVA, ST.list, all.x=T)

# Standardize variety names
p.WVA <- merge(p.WVA, gris_alias, by.x="var_std", by.y="VAR_STD", all.x=TRUE)
p.WVA[ is.na(GRIS_NAME), GRIS_NAME := var_std]
p.WVA[, var_std := GRIS_NAME]
p.WVA[, GRIS_NAME := NULL]
p.WVA <- p.WVA[, .(area=sum(area)), by=.(year, state_abb, state, var_std, class3)]
setorder(p.WVA, state_abb, year)

# Drop varieties that have very small shares
p.WVA[, area_class := sum(area, na.rm=TRUE), by=.(state, year, class3)]
p.WVA[, area_share_class := area / area_class]

c_cutoff_tiny_share <- 0.005
p.WVA[, var_tiny_share_styr := (area_share_class < c_cutoff_tiny_share)]

# Final data
WVA.data.final <- p.WVA[var_tiny_share_styr == FALSE, .(year, state_abb, state, var_std, class3, area)]

# Save final data
save(WVA.data.final, file="./data_intermediate/01_1_WVA_data.Rdata")
