library(tidyverse)
library(data.table)
library(tidyr)
library(dplyr)
library(lubridate)
library(Metrics)
library(viridisLite)
library(viridis)
library(usethis)
library(devtools)
#install.packages("arsenal")
library(car)
library(ggpubr)
library(corrplot)
library(RColorBrewer)
library(reshape2)
library(arsenal)
library(readxl)
library(stringi)

#rm(list = ls())

####WSDA Master file of 2020 WA####


WSDA_master<- read.csv("C:/Users/mdredwanahmad.khan/OneDrive - Washington State University (email.wsu.edu)/Projects/Partial leasing/Crops and grids/MasterFile/WA_Crops_Masterfile_WSDA_20.csv")
selected<- subset(WSDA_master, Selected_for_Partial_Leasing== "YES")
soil<- fread("F:/Partial Water Leasing/CropSyst_file_Fabio/all_calibrated_plus_uncalibrated_soil_210106.txt")
soil<- soil[,c(2:4)]
names(soil)<- c("Grid_Number","lat","long")


#################
##Okanogan 2020##
#################

Okanogangrids<-  read.csv("F:/Partial Water Leasing/Unique grids/NEWDBF_SUBBASIN_WR/Joined_OkanoganIrr_6km_GWIS_WR_SubBS.csv")
Okanogangrids<- Okanogangrids[,c(2,3,4,14,20,21,30,36,61,64,65,70)] #%>% rename(Crop_Name=CropType)
names(Okanogangrids)<- c("Crop_Name","Acres","Irrigation","CropGroup","lat","long","WRIA","WR_DOC_ID","areaacres_huc10","huc10","HUC10_NAME", "WRIA_ID" )

Okanogan<- Okanogangrids%>% 
  select(Crop_Name,Acres,Irrigation,lat,long,WRIA,WR_DOC_ID,areaacres_huc10,huc10,HUC10_NAME,WRIA_ID)%>%
  left_join(soil%>% select(lat,long,Grid_Number))
Okanogan_selected<- filter(Okanogan,Crop_Name %in% selected$Crop_Name )


##WallaWalla###

WallaWallagrids<-  read.csv("F:/Partial Water Leasing/Unique grids/NEWDBF_SUBBASIN_WR/Joined_WallaWallaIrr_6km_GWIS_WR_SubBS.csv")
WallaWallagrids<- WallaWallagrids[,c(8,9,10,20,25,26,35,41,67,70,71,77,80)] #%>% rename(Crop_Name=CropType)
names(WallaWallagrids)<- c("Crop_Name","Acres","Irrigation","CropGroup","lat","long","County","WR_DOC_ID","areaacres_huc10","huc10","HUC10_NAME", "WRIA_ID","WRIA" )

WallaWalla<- WallaWallagrids%>% 
  select(Crop_Name,Acres,Irrigation,lat,long,WRIA,WR_DOC_ID,areaacres_huc10,huc10,HUC10_NAME,WRIA_ID)%>%
  left_join(soil%>% select(lat,long,Grid_Number))
WallaWalla_selected<- filter(WallaWalla,Crop_Name %in% selected$Crop_Name )


####Yakima###

Yakimagrids<-  read.csv("F:/Partial Water Leasing/Unique grids/NEWDBF_SUBBASIN_WR/Joined_YakimaIrr_6km_GWIS_WR_SubBS.csv")
Yakimagrids<- Yakimagrids[,c(7,8,9,19,24,25,34,40,65,68,69,75,78)] #%>% rename(Crop_Name=CropType)
names(Yakimagrids)<- c("Crop_Name","Acres","Irrigation","CropGroup","lat","long","County","WR_DOC_ID","areaacres_huc10","huc10","HUC10_NAME", "WRIA_ID","WRIA" )

Yakima<- Yakimagrids%>% 
  select(Crop_Name,Acres,Irrigation,lat,long,WRIA,WR_DOC_ID,areaacres_huc10,huc10,HUC10_NAME,WRIA_ID)%>%
  left_join(soil%>% select(lat,long,Grid_Number))
Yakima_selected<- filter(Yakima,Crop_Name %in% selected$Crop_Name )

###################################
#####Methow 2020 including grids###
###################################

Methowgrids<-  read.csv("F:/Partial Water Leasing/Unique grids/NEWDBF_SUBBASIN_WR/Joined_MethowIrr_6km_GWIS_WR_SubBS.csv")
Methowgrids<- Methowgrids[,c(7,8,9,19,22,23,31,56,59,60)] #%>% rename(Crop_Name=CropType)
names(Methowgrids)<- c("Crop_Name","Acres","Irrigation","CropGroup","lat","long","WR_DOC_ID","areaacres_huc10","huc10","HUC10_NAME")

Methow<- Methowgrids%>% 
  select(Crop_Name,Acres,Irrigation,lat,long,WR_DOC_ID,areaacres_huc10,huc10,HUC10_NAME)%>%
  left_join(soil%>% select(lat,long,Grid_Number))
Methow_selected<- filter(Methow,Crop_Name %in% selected$Crop_Name )
Methow_selected<- Methow_selected %>% 
  mutate(WRIA="Methow")

###Wenatchee

Wenatcheegrids<-  read.csv("F:/Partial Water Leasing/Unique grids/NEWDBF_SUBBASIN_WR/Joined_WenatcheeIrr_6km_GWIS_WR_SubBS.csv")
Wenatcheegrids<- Wenatcheegrids[,c(7,8,9,19,22,23,31,56,59,60,66,69)] #%>% rename(Crop_Name=CropType)
names(Wenatcheegrids)<- c("Crop_Name","Acres","Irrigation","CropGroup","lat","long","WR_DOC_ID","areaacres_huc10","huc10","HUC10_NAME", "WRIA_ID", "WRIA")

Wenatchee<- Wenatcheegrids%>% 
  select(Crop_Name,Acres,Irrigation,lat,long,WRIA,WR_DOC_ID,areaacres_huc10,huc10,HUC10_NAME,WRIA_ID)%>%
  left_join(soil%>% select(lat,long,Grid_Number))
Wenatchee_selected<- filter(Wenatchee,Crop_Name %in% selected$Crop_Name )
Wenatchee_selected<- Wenatchee_selected[,-11]


Methow<- read.csv("C:/Users/mdredwanahmad.khan/OneDrive - Washington State University (email.wsu.edu)/Projects/Partial leasing/Crops and grids/Methow/Joined_Methow_WSDAirrigated_6kmgrids.csv")
Methow<-  Methow[, c(2,3,4,14,18,19)]
names(Methow)<- c("Crop_Name","Acres","Irrigation","CropGroup","lat","long")

#Selecting crop 
Methowgrp<- Methow %>% 
  dplyr::group_by(Crop_Name) %>% 
  dplyr::summarise(Acres= sum(Acres, na.rm = TRUE))
Methowgrp<- filter(Methowgrp,Crop_Name %in% selected$Crop_Name )

#Joining Grid number based on lat long
Methow<- Methow%>% 
  select(Crop_Name,Acres,Irrigation,lat,long)%>%
  left_join(soil%>% select(lat,long,Grid_Number))
Methow_selected<- filter(Methow,Crop_Name %in% selected$Crop_Name )

##Cleaning the data
Methow_selected<- Methow_selected %>% 
  mutate(long= gsub("-","",long),
         lat= paste(lat,"N",sep = ""),
         long= paste(long,"W", sep = ""),
         lat_long= paste(lat,long,sep=""),
         region= "Methow")