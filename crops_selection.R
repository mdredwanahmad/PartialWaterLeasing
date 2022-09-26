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

####WSDA Madter file of 2020 WA####


WSDA_master<- read.csv("C:/Users/mdredwanahmad.khan/OneDrive - Washington State University (email.wsu.edu)/Projects/Partial leasing/Crops and grids/MasterFile/WA_Crops_Masterfile_WSDA_20.csv")
selected<- subset(WSDA_master, Selected_for_Partial_Leasing== "YES")
soil<- fread("F:/Partial Water Leasing/CropSyst_file_Fabio/all_calibrated_plus_uncalibrated_soil_210106.txt")
soil<- soil[,c(2:4)]
names(soil)<- c("Grid_Number","lat","long")

#write.csv(selected,"C:/Users/mdredwanahmad.khan/OneDrive - Washington State University (email.wsu.edu)/Projects/Partial leasing/Crops and grids/selectedcrops.csv" )


#################
##Okanogan 2020##
#################

Okanogan<- read.csv("C:/Users/mdredwanahmad.khan/OneDrive - Washington State University (email.wsu.edu)/Projects/Partial leasing/Crops and grids/Oknagon_WSDA_Irrigated_2020.csv")
Okanogan<- Okanogan %>% 
  dplyr::group_by(CropType) %>% 
  dplyr::summarise(Acres= sum(Acres, na.rm = TRUE),
                   ExactAcres= sum(ExactAcres, na.rm = TRUE))
names(Okanogan)<- c("Crop_Name","Acres","Exact_Acres")

Okanoganselected<- filter(Okanogan,Crop_Name %in% selected$Crop_Name )


####Load all crops grown and grids of Okanogan####

Okanogangrids<-  read.csv("C:/Users/mdredwanahmad.khan/OneDrive - Washington State University (email.wsu.edu)/Projects/Partial leasing/Crops and grids/Joined_OkanoganIrrigated_6kmgrids.csv")
Okanogangrids<- Okanogangrids[,c(2,3,4,14,20,21)] #%>% rename(Crop_Name=CropType)
names(Okanogangrids)<- c("Crop_Name","Acres","Irrigation","CropGroup","lat","long")

###Filter for only annual and selected hay crops of Walla Walla
Okanogangrids<- filter(Okanogangrids,Crop_Name %in% Okanoganselected$Crop_Name)
#Okanogangrids<- filter(Okanogangrids,Crop_Name %in% selected$Crop_Name)
Okanogangrids<- Okanogangrids%>% 
  select(Crop_Name,Acres,Irrigation,lat,long)%>%
  left_join(soil%>% select(lat,long,Grid_Number))

#Manipulating data for folder requirement
Okanogangrids<- Okanogangrids %>% 
  mutate(long= gsub("-","",long),
         lat= paste(lat,"N",sep = ""),
         long= paste(long,"W", sep = ""),
         lat_long= paste(lat,long,sep=""),
         region= "Okanogan")

#Wheat grids
Wheat<- filter(Okanogangrids, Crop_Name == "Wheat")
wp<- data.frame(unique(Wheat$Grid_Number))
write.csv(wp,"F:/Partial Water Leasing/Unique grids/wheatOk.csv")

#Loops for separating unique grids for crops
cropsO<- unique(Okanogangrids$Crop_Name)
cropsO<- cropsO[-3]
i=1
for (i in 1:length(cropsO)){
  req_crop <- cropsO[[i]]
  crop <- filter(Okanogangrids, Crop_Name == req_crop)
  uniquegrid<- data.frame(unique(crop$lat_long))
  write.csv(uniquegrid, paste0("F:/Partial Water Leasing/Unique grids/Okanogan/",cropsO[i],
                               ".csv"))
}

##Alfalfa Hay/Grass Hay##
alfalfa_hay_grass<- filter(Okanogangrids, Crop_Name %in% c("Alfalfa/Grass Hay"))
uniquegrid<- data.frame(unique(alfalfa_hay_grass$lat_long))
write.csv(uniquegrid,"C:/Users/mdredwanahmad.khan/OneDrive - Washington State University (email.wsu.edu)/Projects/Partial leasing/Crops and grids/Okanogan/alfalfa_hay_grass.csv")



###################
##WallaWalla 2020##
###################

Wallawalla<- read.csv("C:/Users/mdredwanahmad.khan/OneDrive - Washington State University (email.wsu.edu)/Projects/Partial leasing/Crops and grids/WW_WSDA2020_IR_Clip.csv")
Wallawalla<- Wallawalla %>% 
  dplyr::group_by(CropType) %>% 
  dplyr::summarise(Acres= sum(Acres, na.rm = TRUE),
                   ExactAcres= sum(ExactAcres, na.rm = TRUE))
names(Wallawalla)<- c("Crop_Name","Acres","Exact_Acres")

Wallawallaselected<- filter(Wallawalla,Crop_Name %in% selected$Crop_Name )


####Load all crops grown and grids of Walla Walla####

Wallawallagrids<-  read.csv("C:/Users/mdredwanahmad.khan/OneDrive - Washington State University (email.wsu.edu)/Projects/Partial leasing/Crops and grids/Joined_WallaWallIrrigated_6km_update.csv")
Wallawallagrids<- Wallawallagrids[,c(2,3,4,14,20,21)] #%>% rename(Crop_Name=CropType)
names(Wallawallagrids)<- c("Crop_Name","Acres","Irrigation","CropGroup","lat","long")
Wallawallagrids<- Wallawallagrids%>% 
  select(Crop_Name,Acres,Irrigation,lat,long)%>%
  left_join(soil%>% select(lat,long,Grid_Number))

###Filter for only annual and selected hay crops of Walla Walla
Wallawallagrids<- filter(Wallawallagrids,Crop_Name %in% Wallawallaselected$Crop_Name)
Wallawallagrids<- Wallawallagrids %>% 
  mutate(long= gsub("-","",long),
         lat= paste(lat,"N",sep = ""),
         long= paste(long,"W", sep = ""),
         lat_long= paste(lat,long,sep=""),
         region= "WallaWalla")

##Alfalfa##
alfalfa<- filter(Wallawallagrids, Crop_Name %in% c("Alfalfa Hay"))
alfalfa<- filter(Wallawallagrids, Crop_Name %in% c("Alfalfa Hay","Alfalfa Seed","Alfalfa/Grass Hay"))
uniquegrid<- data.frame(unique(alfalfa$lat_long))
write.csv(uniquegrid,"F:/Partial Water Leasing/Unique grids/Walla Walla/Alfalfa/alfalfagrids.csv")

##Wheat##
wheat<- filter(Wallawallagrids, Crop_Name %in% c("Wheat","Wheat Fallow" ))
uniquegrid<- data.frame(unique(wheat$lat_long))
write.csv(uniquegrid,"F:/Partial Water Leasing/Unique grids/Walla Walla/Alfalfa/wheatgrids.csv")

##Loops for separating unique grids for each crop
Wallawallagrids<- subset(Wallawallagrids, Wallawallagrids$Crop_Name !="Alfalfa/Grass Hay")
crops<- unique(Wallawallagrids$Crop_Name)
i=1
for (i in 1:length(crops)){
  req_crop <- crops[[i]]
  crop <- filter(Wallawallagrids, Crop_Name == req_crop)
  uniquegrid<- data.frame(unique(crop$lat_long))
  write.csv(uniquegrid, paste0("F:/Partial Water Leasing/Unique grids/Walla Walla/test/",crops[i],
                               ".csv"))
}



###############
##Yakima 2020##
###############

Yakima<- read.csv("C:/Users/mdredwanahmad.khan/OneDrive - Washington State University (email.wsu.edu)/Projects/Partial leasing/Crops and grids/Yakima_WSDA_Irrigated_2020.csv")
Yakima<- Yakima %>% 
  dplyr::group_by(CropType) %>% 
  dplyr::summarise(Acres= sum(Acres, na.rm = TRUE),
                   ExactAcres= sum(ExactAcres, na.rm = TRUE))
names(Yakima)<- c("Crop_Name","Acres","Exact_Acres")

Yakimaselected<- filter(Yakima,Crop_Name %in% selected$Crop_Name )


####Load all crops grown and grids of Yakima####

Yakimagrids<-  read.csv("C:/Users/mdredwanahmad.khan/OneDrive - Washington State University (email.wsu.edu)/Projects/Partial leasing/Crops and grids/Joined_YakimaIrrigated_6kmgrids.csv")
Yakimagrids<- Yakimagrids[,c(2,3,4,14,20,21)] #%>% rename(Crop_Name=CropType)
names(Yakimagrids)<- c("Crop_Name","Acres","Irrigation","CropGroup","lat","long")

###Filter for only annual and selected hay crops of Yakima
Yakimagrids<- filter(Yakimagrids,Crop_Name %in% Yakimaselected$Crop_Name)
Yakimagrids<- Yakimagrids%>% 
  select(Crop_Name,Acres,Irrigation,lat,long)%>%
  left_join(soil%>% select(lat,long,Grid_Number))

#Manipulating data 
Yakimagrids<- Yakimagrids %>% 
  mutate(long= gsub("-","",long),
         lat= paste(lat,"N",sep = ""),
         long= paste(long,"W", sep = ""),
         lat_long= paste(lat,long,sep=""),
         region= "Yakima")

#Wheat<- filter(Yakimagrids, Crop_Name == "Wheat")
#wp<- Wheat[!duplicated(data.frame(t(apply(Wheat[c("Grid_Number","lat_long")],1,sort)))),]

##Loops for separating unique grids for each crops
cropsY<- unique(Yakimagrids$Crop_Name)
cropsY<- cropsY[-2]
i=1
for (i in 1:length(cropsY)){
  req_crop <- cropsY[[i]]
  crop <- filter(Yakimagrids, Crop_Name == req_crop)
  uniquegrid<- data.frame(unique(crop$lat_long))
  write.csv(uniquegrid, paste0("F:/Partial Water Leasing/Unique grids/Yakima/",cropsY[i],
                               ".csv"))
}

##Alfalfa Hay/Grass Hay##
alfalfa_hay_grass<- filter(Yakimagrids, Crop_Name %in% c("Alfalfa/Grass Hay"))
uniquegrid<- data.frame(unique(alfalfa_hay_grass$lat_long))
write.csv(uniquegrid,"C:/Users/mdredwanahmad.khan/OneDrive - Washington State University (email.wsu.edu)/Projects/Partial leasing/Crops and grids/Yakima/alfalfa_hay_grass.csv")


###################################
#####Methow 2020 including grids###
###################################

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



########################################
#### Wenatchee 2020 including grids ####
########################################


Wenatchee<- read.csv("C:/Users/mdredwanahmad.khan/OneDrive - Washington State University (email.wsu.edu)/Projects/Partial leasing/Crops and grids/Wenatchee/Joined_Wenatchee_WSDAirrigated_6kmgrids.csv")
Wenatchee<- Wenatchee[,c(2,3,4,14,18,19)]
names(Wenatchee)<- c("Crop_Name","Acres","Irrigation","CropGroup","lat","long")

#Selecting crop 
Wenatcheegrp<- Wenatchee %>% 
  dplyr::group_by(Crop_Name) %>% 
  dplyr::summarise(Acres= sum(Acres, na.rm = TRUE))
Wenatcheegrp<- filter(Wenatcheegrp,Crop_Name %in% selected$Crop_Name )

#Joining Grid number based on lat long
Wenatchee<- Wenatchee%>% 
  select(Crop_Name,Acres,Irrigation,lat,long)%>%
  left_join(soil%>% select(lat,long,Grid_Number))
Wenatchee_selected<- filter(Wenatchee,Crop_Name %in% selected$Crop_Name )

##Cleaning the data
Wenatchee_selected<- Wenatchee_selected %>% 
  mutate(long= gsub("-","",long),
         lat= paste(lat,"N",sep = ""),
         long= paste(long,"W", sep = ""),
         lat_long= paste(lat,long,sep=""),
         region= "Wenatchee")



####Creating data table for Generator####

crops_grids<- rbind(Okanogangrids,Wallawallagrids,Yakimagrids)

#Seperating Alfalfa/Grass Hay
cpa<- subset(crops_grids, Crop_Name=="Alfalfa/Grass Hay")
cpa$Crop_Name<- gsub('Alfalfa/Grass Hay','Alfalfa Hay', cpa$Crop_Name)

drop<- c("Alfalfa/Grass Hay","Onion Seed","Sod Farm","Buckwheat","Wildlife Feed")
crops_grids<- subset(crops_grids, !Crop_Name %in% drop)

crops_grids<- rbind(crops_grids, cpa)

#stringi packages
crops_grids$Crop_Name<- stri_replace_all_regex(crops_grids$Crop_Name,
                                               pattern = c("Corn, Field", "Alfalfa Hay","Grass Hay", "Triticale Hay", "Wheat", "Barley Hay", "Corn, Sweet", "Sudangrass", "Alfalfa Seed", "Wheat Fallow",
                                                           "Onion", "Corn Seed", "Canola", "Timothy", "Rye", "Barley", "Oat Hay", "Oat", "Hops", "Yellow Mustard", "Triticale" ),
                                               replacement = c("Corn_grain","Alfalfa_Hay","Grass_Hay","Triticale_Hay", "Winter_wheat","Barley_Hay","Sweet_Corn","Sudangrass","Alfalfa_Seed","Winter_wheat", 
                                                               "Onion", "Corn_grain","Canola","Timothy","Rye", "Barley_Spring","Oats_hay", "Oat", "Hops", "Yellow_Mustard", "Triticale"),
                                               vectorize= FALSE)
Barley_Spring<- filter(crops_grids, Crop_Name=="Barley_Spring")
Barley_Hay <- filter(crops_grids, Crop_Name=="Barley_Spring_Hay")
Barley_Hay$Crop_Name<- gsub('Barley_Spring_Hay','Barley_Hay', Barley_Hay$Crop_Name)
Barley<- rbind(Barley_Spring,Barley_Hay)

crops_grids$Crop_Name<- gsub('Winter_wheat Fallow', 'Winter_wheat', crops_grids$Crop_Name)
winter<- filter(crops_grids, Crop_Name== "Winter_wheat")
winter$Crop_Name<- gsub('Winter_wheat','Spring_wheat', winter$Crop_Name)
drop<- c("Barley_Spring", "Barley_Spring_Hay")
crops_grids<- subset(crops_grids, !Crop_Name %in% drop)

crops_grids<- rbind(crops_grids,winter)
crops_grids<- rbind(crops_grids,Barley)
crops_grids1<- crops_grids[!duplicated(data.frame(t(apply(crops_grids[c("Crop_Name","Grid_Number","lat_long")],1,sort)))),]
crops_grids2<- crops_grids[!duplicated(data.frame(t(apply(crops_grids[c("Crop_Name","Irrigation","Grid_Number","lat_long","region")],1,sort)))),]

#write.csv(crops_grids,"F:/Partial Water Leasing/Unique grids/crops_grids_update.csv" )



####Merging methow and wenatchee ####

crops<- rbind(Methow_selected, Wenatchee_selected)
#crops<- crops[!duplicated(data.frame(t(apply(crops[c("Crop_Name","Grid_Number","lat_long")],1,sort)))),]

cp<- filter(crops, Crop_Name=="Alfalfa/Grass Hay")
cp$Crop_Name<- stri_replace_all_regex(cp$Crop_Name,
                                               pattern = c("Alfalfa/Grass Hay"),
                                               replacement = c("Alfalfa Hay"),
                                               vectorize= FALSE)
crops<- subset(crops, !Crop_Name=="Alfalfa/Grass Hay")
crops<- rbind(crops,cp)

crops$Crop_Name<- stri_replace_all_regex(crops$Crop_Name,
                                               pattern = c("Wheat", "Grass Hay", "Alfalfa Hay", "Onion", "Wheat Fallow","Triticale Hay", "Barley Hay"),
                                               replacement = c("Winter_wheat","Grass_Hay","Alfalfa_Hay","Onion","Winter_wheat","Triticale_Hay", "Barley_Hay"),
                                               vectorize= FALSE)
crops$Crop_Name<- gsub('Winter_wheat Fallow', 'Winter_wheat', crops$Crop_Name)
winter<- filter(crops, Crop_Name== "Winter_wheat")
winter$Crop_Name<- gsub('Winter_wheat','Spring_wheat', winter$Crop_Name)
crops<- rbind(crops,winter)

crops1<- crops[!duplicated(data.frame(t(apply(crops[c("Crop_Name","Grid_Number","lat_long")],1,sort)))),]
write.csv(crops1,"F:/Partial Water Leasing/Unique grids/crops_grids_forMW.csv")



####Creating folder test folder based on list#####

region<- list.files("F:/Workspace/test_folder/")
i= 1
cp<- rep()
#j=1
#k=1
for (i in 1:length(region)){
  #region= region[[i]]
  working_path<- paste0("F:/Workspace/test_folder/",region[i],"/")
  #listwp<- list.files(working_path)
  Crops<- filter(crops_grids, crops_grids$region== region[i])
  Crops<- cp
  #Crops<- Crops[[i]]
  croplist<- unique(Crops$Crop_Name)
  #croplist_folder<- croplist[[i]]
  #croplist_folder<- list.files(paste0("F:/Workspace/PWL/Scenarios/test_site_05_13_22/optimal/",region[i]))
  for (j in seq_along(croplist)){
    folder<-dir.create(paste0(working_path,croplist[j]))
  }
  #k=i=j
  grids<- filter(crops_grids,crops_grids$region==region[i] & crops_grids$Crop_Name== croplist[j])  
  Grids<- unique(grids$lat_long)
  
  for(k in seq_along(Grids)){
    target_path<- paste0(working_path,croplist[j],"/")
    uniqueGrids<- Grids[[k]]
    folder<-dir.create(paste0(target_path,uniqueGrids[k]))
  }
}
  