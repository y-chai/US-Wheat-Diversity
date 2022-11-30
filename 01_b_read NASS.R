# Read in NASS wheat data
# author: Yuan Chai
# email: chaix026@umn.edu

# Read in NASS data
rm(list=ls())
library(data.table)
library(dplyr)
library(ggplot2)

###################################
##        STATE LEVEL DATA       ##
###################################

###############
# SURVEY data #
###############

# For this study, use SURVEY data only
data.survey.St <- fread("./data_original/NASS_S_ST_PlantAcre_1909_2020.csv")

# Select data
data.survey.St <- data.survey.St[, .(CropYear=as.numeric(Year),
                                     StateName=`State`,
                                     StateCode=`State ANSI`,
                                     Desc=`Data Item`, 
                                     Value=as.numeric(gsub(",", "", Value)))]

# Select these variables
Desc.sel.S.St <- unique(data.survey.St$Desc)

# Create Description Variables
data.survey.St[Desc == Desc.sel.S.St[1], DescF := "PlantAcre.all.S"]
data.survey.St[Desc == Desc.sel.S.St[2], DescF := "PlantAcre.winter.S"]
data.survey.St[Desc == Desc.sel.S.St[3], DescF := "PlantAcre.durum.S"]
data.survey.St[Desc == Desc.sel.S.St[4], DescF := "PlantAcre.spring.S"]


# Change to wide format
data.survey.St.wide <- dcast(data.survey.St, CropYear + StateCode + StateName ~ DescF,
                             value.var="Value")

# Fill in PlantAcre.all.S values
data.survey.St.wide[is.na(PlantAcre.all.S), `:=`(PlantAcre.all.S = sum(PlantAcre.winter.S,
                                                                       PlantAcre.spring.S,
                                                                       PlantAcre.durum.S,
                                                                       na.rm=TRUE)),
                    by=.(CropYear, StateCode, StateName)]

# Table Wheat Area by Class
data_table_area_by_class <- data.survey.St[, .(Value = sum(Value, na.rm = T)), by=.(CropYear, DescF)] 
table_area_by_class <- dcast(data_table_area_by_class, CropYear ~ DescF, value.var="Value")

write.csv(table_area_by_class, file="./data_final/01_3_table_area_by_class.csv", na = "", row.names = F)

# State List by wheat classes (manually selcted based on the data availability)
durum.state <- c("California", "Montana", "North Dakota", "South Dakota")
spring.state <- c("Idaho", "Minnesota", "Montana", "North Dakota",
                  "Oregon", "South Dakota", "Washington")
winter.state <- c("California", "Colorado", "Idaho", "Indiana", "Kansas", "Kentucky",  
                  "Montana", "Nebraska", "North Dakota", "Oklahoma", "Oregon", "South Dakota", "Texas",       
                  "Washington", "Wyoming")
state.sel <- unique(c(durum.state, spring.state, winter.state))

# Selected states
data.survey.St.wide[, StateIncluded := FALSE]
data.survey.St.wide[StateName %in% toupper(state.sel), StateIncluded := TRUE]

summary_StateIncluded = data.survey.St.wide[, .(PlantAcre.all.S = sum(PlantAcre.all.S, na.rm=T),
                                                PlantAcre.durum.S = sum(PlantAcre.durum.S, na.rm = T),
                                                PlantAcre.spring.S = sum(PlantAcre.spring.S, na.rm=T),
                                                PlantAcre.winter.S = sum(PlantAcre.winter.S, na.rm=T)),
                                            by=.(CropYear, StateIncluded)]


summary_StateIncluded[, PlantAcre.all.S.TOTAL := sum(PlantAcre.all.S), by=.(CropYear)]
summary_StateIncluded[, PlantAcre.all.S.SHARE := PlantAcre.all.S / PlantAcre.all.S.TOTAL]

summary_StateIncluded[, PlantAcre.durum.S.TOTAL := sum(PlantAcre.durum.S), by=.(CropYear)]
summary_StateIncluded[, PlantAcre.durum.S.SHARE := PlantAcre.durum.S / PlantAcre.durum.S.TOTAL]

summary_StateIncluded[, PlantAcre.spring.S.TOTAL := sum(PlantAcre.spring.S), by=.(CropYear)]
summary_StateIncluded[, PlantAcre.spring.S.SHARE := PlantAcre.spring.S / PlantAcre.spring.S.TOTAL]

summary_StateIncluded[, PlantAcre.winter.S.TOTAL := sum(PlantAcre.winter.S), by=.(CropYear)]
summary_StateIncluded[, PlantAcre.winter.S.SHARE := PlantAcre.winter.S / PlantAcre.winter.S.TOTAL]

write.csv(summary_StateIncluded, file="./data_final/01_3_summary_StateIncluded.csv", row.names = F)

# Select only above states
data.survey.St.wide <- data.survey.St.wide[StateName %in% toupper(state.sel)]

# Save data
save(data.survey.St.wide, file="./data_final/01_3_data.survey.St.wide.RData")

