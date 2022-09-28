library(tidyverse)
library(data.table)
library(tidyr)
library(dplyr)
library(lubridate)
library(reshape2)

##Intersected dbf file lat long and MUKEY
MethowMukey<- read.csv("C:/Users/mdredwanahmad.khan/OneDrive - Washington State University (email.wsu.edu)/Projects/Partial leasing/Crops and grids/Methow/Methow_Inter_Soil_IR.csv")
MethowMukey<-  MethowMukey[, c(4,5,6,19,20,31)]
names(MethowMukey)<- c("Crop_Name","Acres","Irrigation","lat","long","MUKEY")


###Selected crop file for partial leasing
select<- read.csv("F:/Partial Water Leasing/test_soil/selectedcrops.csv")
select<- select[,-1]

#Methow 
Methow_M<- filter(MethowMukey,Crop_Name %in% select$Crop_Name )

##Required Manipulation
Methow_M<- Methow_M%>% 
  mutate(long= gsub("-","",long),
         lat= paste(lat,"N",sep = ""),
         long= paste(long,"W", sep = ""),
         lat_long= paste(lat,long,sep=""),
         region= "Methow")

###Removing duplicate
Methow_M1<- Methow_M[!duplicated(Methow_M$lat_long),]

##Set working path
setwd("F:/Partial Water Leasing/test_soil/")


##Loops for separting soil file
grids<- Methow_M1$lat_long
Mukey<- Methow_M1$MUKEY
i=1
for (i in 1:length(grids)){
  tmp <- cat(c("[inherit]",paste0("0../",Mukey[[i]],".sil")),sep = "\n",file = paste0(grids[[i]],".sil"),append = TRUE)
}
