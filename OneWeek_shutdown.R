library(tidyverse)
library(data.table)
library(tidyr)
library(dplyr)
library(lubridate)
library(Metrics)
library(devtools)
#install.packages("arsenal")
library(car)
library(ggpubr)
library(corrplot)
library(RColorBrewer)
library(reshape2)
library(readxl)
library(ggpmisc)
library(ggplot2)
library (gganimate)
#library (rgdal)
library(grid)
library(stringi)
library(future)
library(purrr)
library(furrr)
#install.packages("pheatmap")
library(patchwork) # To display 2 charts together
library(hrbrthemes)
library(maps)
options("scipen"=100, "digits"=5)

##Annual and Hay Crops 
crp_y<- c("Corn_grain", "Spring_wheat", "Sweet_Corn", "Winter_wheat", "Alfalfa_Seed",  
          "Barley_Spring","Canola","Hops","Oats","Onion","Triticale","Yellow_Mustard")
bmass<- c("Alfalfa_Hay","Barley_Hay", "Grass_Hay", "Sudangrass","Triticale_Hay",
          "Oats_hay", "Rye", "Timothy")

###Optimal Irrigation###

optimal<- fread("F:/Partial Water Leasing/Result_of_CroSyst_Run/NEW_RUN/optimal/result.dat", fill = TRUE, nrows = Inf)
names(optimal)<- c("Year", "planting_date", "harvest_date", "yield", "used_biomass", "irrig", "precip", "region", "crop", "site")
optimal<- optimal %>% 
  separate(Year, into= c("Year", "Month","Day"), sep="-") %>%
  mutate(simulation= "optimal")
optimal<- optimal[,-c(2,3)]
annual_crops<- filter(optimal, crop %in% crp_y)
annual_crops$yield_kgha<- annual_crops$yield
Hay_crops<- filter(optimal, crop %in% bmass)
Hay_crops$yield_kgha<- Hay_crops$used_biomass
#optimal<- rbind(annual_crops,Hay_crops)
optimal<- merge(Hay_crops,annual_crops, all.x = TRUE, all.y = TRUE,sort = TRUE)  ##instead of rbind

##rearranging and Manipulating data
optimal_ir<- optimal%>% 
  mutate(irrig_optimal_mm= irrig,
         yield_optimal_kgha= yield_kgha)


###ACreages information form crop and grids file
crops_grids<- read.csv("F:/Partial Water Leasing/Unique grids/combined_crops_grids_WR.csv")
crops_grids<- crops_grids[,-c(1,9,12,14,15)]
names(crops_grids)<- c("crop","Acres","Irrigation","lat","lon","WR_DOC_ID","areaacres_huc10","HUC10_NAME","Grid_Number","region","site")
##Changing the name of Oats Hay to Oats_hay
crops_grids$crop<- gsub('Oats Hay','Oats_hay', crops_grids$crop)
crops_grids$lat<- formatC(crops_grids$lat, digits = 5, format = "f")
crops_grids$lon<- formatC(crops_grids$lon, digits = 5, format = "f")
##modifying based on requirements
crops_grids<- crops_grids %>% 
  mutate(Crop_site_region= paste0(crop,"_",site,"_",region))
crops_grids<- crops_grids[!duplicated(crops_grids$Crop_site_region),]


####Joining dataframe to get the acreages information####
optimal_ir1<- optimal_ir %>% 
  dplyr::select(Year,planting_date,harvest_date,crop,yield_kgha,irrig,precip,region,site,simulation,irrig_optimal_mm, yield_optimal_kgha ) %>% 
  left_join(crops_grids %>%  dplyr::select(crop, Acres,Grid_Number, site, region,WR_DOC_ID,HUC10_NAME)) %>% 
  mutate(area_ha_opt= Acres*0.405,
         irrigation_m3= ((irrig*0.001)*(area_ha_opt*10000)),
         #irrigation_m3= ((irrig*0.001)*(Acres*4046.86))#Acres,
         Irrigation_cfs_w_opt= ((irrigation_m3*3.28084*3.28084*3.28084)/(3600*24*7)),
         precip_m3= ((precip*0.001)*(area_ha_opt*10000)),
         total_yield_optimal_kg= yield_kgha*area_ha_opt,
         #total_irrig_cfs_opt= irrigation_cfs*area_ha_opt,
         total_irrig_mm_opt= irrig*area_ha_opt,
         Crop_site_region= paste0(crop,"_",site,"_",region), 
         subbasin_WR= paste0(HUC10_NAME,"_",WR_DOC_ID))

#check<- optimal_ir1 %>%
  #filter_all(any_vars( is.na(.)))
#apply(is.na(optimal_ir1), 2, which) #checking if there are any NA value



##################################
#########One week shutdown########
##################################

filesfolder<- list.files("F:/Partial Water Leasing/Result_of_CroSyst_Run/NEW_RUN/Oneweek_Shutdown/")
#filesfolder<- filesfolder[-c(3)]
path1<- "F:/Partial Water Leasing/Result_of_CroSyst_Run/NEW_RUN/Oneweek_Shutdown/"
onewkstdwn<- rep()

for (i in 1:length(filesfolder)){
  p1=filesfolder[[i]]
  #p1[i]=p1
  #set_path<- paste0(path1,p1)
  result1<- fread(paste0(path1,p1,"/result.dat"), fill = TRUE, nrows = Inf)
  names(result1)<- c("Year", "planting_date", "harvest_date", "yield", "used_biomass", "irrig", "precip", "region", "crop", "site")
  result1<- result1 %>% 
    separate(Year, into= c("Year", "Month","Day"), sep="-") %>%
    mutate(simulation= p1)
  result1<- result1[,-c(2,3)]
  annual<- filter(result1, crop %in% crp_y) %>% 
    mutate(yield_kgha= yield)
  hay<- filter(result1, crop %in% bmass) %>% 
    mutate(yield_kgha= used_biomass)
  result1<- rbind(hay,annual)
  output3<- rbind(onewkstdwn, result1)
  onewkstdwn<- output3
}

onewkstdwn<- rbind(onewkstdwn,optimal) %>% 
  mutate(Crop_site_region= paste0(crop,"_",site,"_",region))
oneweekst<- filter(onewkstdwn, Crop_site_region %in% optimal_ir1$Crop_site_region)

##Percentage Yield and irrigation calculation and weighting with the area
onewkstdwn_t<- oneweekst %>%
  dplyr::select(Year,planting_date,harvest_date,yield,used_biomass,yield_kgha,irrig,precip,region,crop,site,simulation) %>% 
  left_join(optimal_ir1 %>% dplyr::select(Year,planting_date,region,crop,site,Acres,irrig_optimal_mm,yield_optimal_kgha,total_yield_optimal_kg, Irrigation_cfs_w_opt,WR_DOC_ID,HUC10_NAME)) %>% 
  mutate(area_ha= Acres*0.405,
         irrigation_m3= ((irrig*0.001)*(area_ha*10000)),
         Irrigation_cfs_w= ((irrigation_m3*3.28084*3.28084*3.28084)/(3600*24*7)), ##Cfs_to_cfd
         total_yield_kg= yield_kgha*area_ha,
         Percentage_irrig= (Irrigation_cfs_w/Irrigation_cfs_w_opt)*100,
         Percentage_yield= (total_yield_kg/total_yield_optimal_kg)*100)  #%>% na.omit() ##There is some NA value in the data frame because of some mismatch, should look at this
onewkstdwn_t1<- onewkstdwn_t %>%  na.omit()
onewkstdwn_t1$Percentage_yield<- ifelse(onewkstdwn_t1$Percentage_yield>100,100,onewkstdwn_t1$Percentage_yield)
#apply(is.na(onewkstdwn_t), 2, which)
#check<- onewkstdwn_t1 %>%
  #filter_all(any_vars( is.na(.)))


####Region Crop simulation and yearly median across the grids for crops####
meadonewk<- onewkstdwn_t1 %>%      #####One more dataframe will be nedded for the quantile to make ribbon plot
  group_by(region,HUC10_NAME, crop, Year, simulation) %>% 
  summarise(irrig= median(irrig, na.rm = TRUE),
            yield= median(yield_kgha, na.rm=TRUE),
            total_irrig= median(Irrigation_cfs_w, na.rm = TRUE),
            total_yield= median(total_yield_kg, na.rm=TRUE),
            Percentage_irrigation= median(Percentage_irrig,na.rm = TRUE),
            Percentage_yield= median(Percentage_yield,na.rm = TRUE))



##Preparing boxplot for comparing percentage irrigation and and yield reduction due to oneweek shutdown
###Loop for creating plot  ##After meeting kirti####
reg_ws<- unique(onewkstdwn_t1 $region)   
pafl<- "F:/Partial Water Leasing/Figure/Oneweek_New/IP_Comp_BoxPlot_Subbasin_V2/"
m=1
n=1
for ( m in 1: length(reg_ws)){
  rw<- reg_ws[[m]]
  ws<- dir.create(paste0(pafl,rw))
  sep_ws<- filter(onewkstdwn_t1, region==rw)
  setwd(paste0(pafl,rw,"/"))
  sepdata<- sep_ws[,c("simulation","region","HUC10_NAME","crop","Percentage_irrig","Percentage_yield")]  #Removing extra column form main dataframe
  #sepdata<- sep_ws[,c("simulation","region","HUC10_NAME","crop","Percentage_irrigation","Percentage_yield")]
  sepd1<- melt(sepdata, id= c("simulation","region","HUC10_NAME","crop"))  #melting data for required shape for the graph
  sepd1<- filter(sepd1, simulation!="optimal")
  sepd1 <- sepd1 %>% 
    mutate(Month = paste0(substr(simulation,1,3)),
           shutdown = paste0(substr(simulation,4,6)),
           simulation = paste0(Month,"_",shutdown)) 
  
  sepd1$simulation <- factor(sepd1$simulation , ordered = TRUE, levels = c("MAY_1ST","MAY_2ND","MAY_3RD","MAY_4TH","JUN_1ST","JUN_2ND","JUN_3RD","JUN_4TH","JUL_1ST","JUL_2ND","JUL_3RD","JUL_4TH","JUL_END","AUG_1ST","AUG_2ND"))
  
  #Plot the graph for whole picture
  P1<- ggplot(sepd1, aes(x=simulation,y= value,fill=variable))+
    geom_boxplot(outlier.shape = NA)+  scale_y_continuous("",limits=c(0,100), breaks=seq(0,100, by=10))+
    labs(x= "", y= "", title = paste0(rw,"_One week shutdown scenario"))+
    coord_cartesian(ylim = c(0,100))+theme_bw()+ 
    theme(plot.title = element_text(size = 20),axis.title = element_text(size = 20,face="bold"), 
          legend.text = element_text(size=18), legend.title = element_blank(),axis.text.x = element_text(size = 25,color = "black",angle = 45, hjust=1,face="bold"),
          axis.text.y = element_text(size = 25,color = "black",angle = 0, hjust=1,face="bold"),legend.position = "bottom")
  
  ##Save the overall plot 
  ggsave(P1, file=paste0(rw,".png"), width = 40, height = 30,dpi = 300, units = "cm")
  
  CRP<- filter(sepd1, crop %in% crp_y)
  HAY<- crp<- filter(sepd1, crop %in% bmass)
  #Filtering out subbasin
  un_subbasin<- unique(sepd1$HUC10_NAME)
  for (n in 1:length(un_subbasin)){
    c1= un_subbasin[[n]] #Select the crop
    unsubbsn<- filter(sepd1, HUC10_NAME==c1)  #filter the crop
    
    #Plot the graph for each basin
    P2<- ggplot(unsubbsn, aes(x=simulation,y= value,fill=variable))+
      geom_boxplot(outlier.shape = NA)+ scale_y_continuous("",limits=c(0,100), breaks=seq(0,100, by=10))+
      labs(x= "", y= "", title = paste0(c1,"_",rw))+
      coord_cartesian(ylim = c(0,100))+theme_bw()+ 
      theme(plot.title = element_text(size = 20),axis.title = element_text(size = 20,face="bold"), 
            legend.text = element_text(size=18), legend.title = element_blank(),axis.text.x = element_text(size = 25,color = "black",angle = 45, hjust=1,face="bold"),
            axis.text.y = element_text(size = 25,color = "black",angle = 0, hjust=1,face="bold"),legend.position = "bottom")
    ##Save the overall plot 
    ggsave(P2, file=paste0(c1,"_",rw,".png"), width = 40, height = 30,dpi = 300, units = "cm")
    
    ##Sep for Crop
    unsub_crp<- filter(CRP, HUC10_NAME==c1)
    P3<- ggplot(unsub_crp, aes(x=simulation,y= value,fill=variable))+
      geom_boxplot(outlier.shape = NA)+ scale_y_continuous("",limits=c(0,100), breaks=seq(0,100, by=10))+
      labs(x= "", y= "", title = paste0(c1,"_CRP"))+
      coord_cartesian(ylim = c(0,100))+theme_bw()+ 
      theme(plot.title = element_text(size = 20),axis.title = element_text(size = 20,face="bold"), 
            legend.text = element_text(size=18), legend.title = element_blank(),axis.text.x = element_text(size = 25,color = "black",angle = 45, hjust=1,face="bold"),
            axis.text.y = element_text(size = 25,color = "black",angle = 0, hjust=1,face="bold"),legend.position = "bottom")
    ##Save the overall plot 
    ggsave(P3, file=paste0(c1,"_CRP_",rw,".png"), width = 40, height = 30,dpi = 300, units = "cm")
    
    ##Sep for HAY
    unsub_hay<- filter(HAY, HUC10_NAME==c1)
    P4<- ggplot(unsub_hay, aes(x=simulation,y= value,fill=variable))+
      geom_boxplot(outlier.shape = NA)+ scale_y_continuous("",limits=c(0,100), breaks=seq(0,100, by=10))+
      labs(x= "", y= "", title = paste0(c1,"_HAY"))+
      coord_cartesian(ylim = c(50,100))+theme_bw()+ 
      theme(plot.title = element_text(size = 20),axis.title = element_text(size = 20,face="bold"), 
            legend.text = element_text(size=18), legend.title = element_blank(),axis.text.x = element_text(size = 25,color = "black",angle = 45, hjust=1,face="bold"),
            axis.text.y = element_text(size = 25,color = "black",angle = 0, hjust=1,face="bold"),legend.position = "bottom")
    ##Save the overall plot 
    ggsave(P4, file=paste0(c1,"_HAY_",rw,".png"), width = 40, height = 30,dpi = 300, units = "cm")
  }
}



####Data manipulation for site wise mean across years###
##each side will hav eonly one value instead of 40 values##
grid_meadian<- onewkstdwn_t1 %>%    
  group_by(region,HUC10_NAME,site, crop, simulation) %>% 
  summarise(irrig= median(irrig, na.rm = TRUE),
            yield= median(yield_kgha, na.rm=TRUE),
            total_irrig= median(Irrigation_cfs_w, na.rm = TRUE),
            total_yield= median(total_yield_kg, na.rm=TRUE),
            area= median(area_ha, na.rm=TRUE),
            Percentage_irrigation= median(Percentage_irrig,na.rm = TRUE),
            Percentage_yield= median(Percentage_yield,na.rm = TRUE)) %>% 
  mutate(Crop_site_region= paste0(crop,"_",site,"_",region))


###Function for calculating different across simulation and site
Crop_site_region<- unique(grid_meadian$Crop_site_region)
diff_func<- function(Crop_site_region){
  df_tmp <- grid_meadian[grid_meadian$Crop_site_region==Crop_site_region[[1]],] 
  
  opt_yield <- df_tmp[df_tmp$simulation == "optimal",9] %>% as.numeric()
  opt_irrigation <- df_tmp[df_tmp$simulation == "optimal",8] %>% as.numeric()
  
  df_tmp <-df_tmp %>% 
    mutate(yield_reduction = opt_yield-total_yield,
           Instreamflow_AugP= opt_irrigation- total_irrig)
  options("scipen"=100, "digits"=5)
  
  return(df_tmp)
}

plan(multisession,workers = 2)  ##usimng two system out of 4 of my PC
grid_meadian_up <- future_map_dfr(Crop_site_region,diff_func)
plan(sequential)
grid_meadian_up$yield_reduction<- format(grid_meadian_up$yield_reduction, scientific = FALSE) %>% as.numeric()
grid_meadian_up$Instreamflow_AugP<- format(grid_meadian_up$Instreamflow_AugP, scientific = FALSE)%>% as.numeric()


###calculating sum of grids (site)###
grid_sum<- grid_meadian_up %>% 
  group_by(region,HUC10_NAME,site, simulation) %>% 
  summarise(total_irrig_cfs= sum(total_irrig, na.rm = TRUE),
            total_yield_kg= sum(total_yield, na.rm=TRUE),
            total_area_ha= sum(area, na.rm=TRUE),
            Percentage_irrigation= median(Percentage_irrigation,na.rm = TRUE),
            Percentage_yield= median(Percentage_yield,na.rm = TRUE),
            yield_reduction= sum(yield_reduction, na.rm= TRUE),
            Instreamflow_AP= sum(Instreamflow_AugP, na.rm= TRUE))

###JOining Lat long to the grids##
GD_SUM<- grid_sum %>% 
  dplyr::select(region,HUC10_NAME,site,simulation,total_irrig_cfs,total_yield_kg,total_area_ha,Percentage_irrigation,Percentage_yield,yield_reduction, Instreamflow_AP) %>% 
  left_join(crops_grids %>% dplyr::select(Grid_Number, site, region,HUC10_NAME,lat,lon)) %>% distinct()
GD_SUM$Instreamflow_AP<- round(GD_SUM$Instreamflow_AP, digits = 4)
GD_SUM$yield_reduction<- round(GD_SUM$yield_reduction, digits = 4)
#write.csv(GD_SUM,"F:/Partial Water Leasing/Unique grids/Streamflow_grids/grids_IsP.csv")

###Calculating Sum for huc across site###
huc_sum<- grid_sum %>% 
  group_by(region,HUC10_NAME, simulation) %>% 
  summarise(total_irrig_cfs= sum(total_irrig_cfs, na.rm = TRUE),
            total_yield_kg= sum(total_yield_kg, na.rm=TRUE),
            total_area_ha= sum(total_area_ha, na.rm=TRUE),
            Percentage_irrigation= mean(Percentage_irrigation,na.rm = TRUE),
            Percentage_yield= mean(Percentage_yield,na.rm = TRUE),
            yield_reduction= sum(yield_reduction, na.rm= TRUE),
            Instreamflow_AP= sum(Instreamflow_AP, na.rm= TRUE))


##Centroids for huc###
huc_centroid<-  read.csv("F:/Partial Water Leasing/Unique grids/Sub_basin_grids_centroids.csv")
huc_sum1<- left_join(huc_sum, huc_centroid, by= c("region","HUC10_NAME"))
huc_sum1$Instreamflow_AP<- round(huc_sum1$Instreamflow_AP, digits = 3) ###Found some of the Instreamflow_AP value in Salmon creek, Pine Creek , Umtanum creek yakima, Lower Checwuch river is negative like -0.000055, almost zero. So rounded it up but have to look it to later. 
huc_sum1$yield_reduction<- round(huc_sum1$yield_reduction, digits = 3)


####Loops for creating compariosn plot of Instreqamflow potentials and Percentage yields####
huc_sum2<- huc_sum[huc_sum$simulation!="optimal",]
ab<- data.frame(Number= 1:15, ##Dataframe for sorting simulation
                simulation= c('MAY1ST','MAY2ND', 'MAY3RD','MAY4TH','JUN1ST','JUN2ND','JUN3RD','JUN4TH','JUL1ST','JUL2ND','JUL3RD','JUL4TH','JULEND','AUG1ST','AUG2ND'))

output_directory<- "F:/Partial Water Leasing/Figure/Oneweek_New/SB1_V2/"
p=1
q=1
basin<- unique(huc_sum2$region)
for ( p in 1: length(basin)){
  b<- basin[[p]]
  dir<- dir.create(paste0(output_directory,b))
  basin_sub<- filter(huc_sum2, region==b)
  sb<- unique(basin_sub$HUC10_NAME)
  for ( q in 1: length(sb)){
    sb1<- sb[[q]]
    #dir1<- dir.create(paste0(output_directory,b,"/",sb1))
    directory<- paste0(output_directory,b,"/")
    huc10<- filter(huc_sum2, HUC10_NAME==sb1) %>% 
      left_join(ab, by="simulation") %>% arrange(Number)
    
    png(paste(directory,sb1,b,".png"), width=30, height=20, res=300, units = "cm")
    par(mgp=c(2.5,1,0),mar=c(6,4,4,6))
    plot(huc10$Number,huc10$Percentage_yield,type="b",lwd=3,col="red",xaxt="n",cex.axis=1.15,font=2,pch=17,
         main=paste0("Yield percentage and Instreamflow Augment. Pot. across one week shutdown scenarios","\n",sb1,"_",b),ylab = expression(bold("")),xlab = expression(bold("")), cex.lab=1.15,ylim = c(0,100) )
    axis(side=1,at=1:15,labels=ab$simulation,font = 2)
    axis(side=2,col.axis="red",font = 2,cex.lab=1.15,cex.axis=1.15)
    par(new =TRUE)
    plot(huc10$Number,huc10$Instreamflow_AP,col="blue",type = "b", lty = 1,pch=15,lwd = 2,axes = FALSE,cex=1.10, xlab = " ", ylab = " ")  ## This one will add secondary axis
    axis(4, col="blue",col.axis="blue",las=3, font = 2,cex.axis=1.25,ylim = c(0,max(huc10$Instreamflow_AP)))
    mtext(expression(bold("")), side = 4, line = 3, cex=1.15)
    legend("bottomright",legend=c("Percentage of the Yield","Instreamflow Aug. Pot. Cfs"),text.col=c("red","blue"),pch=c(17,15),col=c("red","blue"), cex = 1.15)
    dev.off()
  }
}



####Loops for creating compariosn plot of Percentage yields and Perntage irrigation####
output_directory<- "F:/Partial Water Leasing/Figure/Oneweek_New/SB2/"
p=1
q=1
basin<- unique(huc_sum2$region)
for ( p in 1: length(basin)){
  b<- basin[[p]]
  dir<- dir.create(paste0(output_directory,b))
  basin_sub<- filter(huc_sum2, region==b)
  sb<- unique(basin_sub$HUC10_NAME)
  for ( q in 1: length(sb)){
    sb1<- sb[[q]]
    #dir1<- dir.create(paste0(output_directory,b,"/",sb1))
    directory<- paste0(output_directory,b,"/")
    huc10<- filter(huc_sum2, HUC10_NAME==sb1) %>% 
      left_join(ab, by="simulation") %>% arrange(Number)
    
    png(paste(directory,sb1,b,".png"), width=30, height=20, res=300, units = "cm")
    par(mgp=c(2.5,1,0),mar=c(6,4,4,6))
    plot(huc10$Number,huc10$Percentage_yield,type="b",lwd=3,col="#8B008B",xaxt="n",cex.axis=1.10,font=2,
         main=paste0("Percentage Yields and Irrigation comparison for one-week shutdown wrt the optimal","\n",sb1,"_",b),ylab = expression(bold("Percentage change in the Yield wrt the optimal")),xlab =expression(bold("Simulation of Oneweek Shutdown")),cex.lab=1.15)
    axis(side=1,at=1:15,labels=ab$simulation,font = 2)
    par(new =TRUE)
    plot(huc10$Number,huc10$Percentage_irrigation,col="#8B0046",type = "b", lty = 1,lwd = 2,axes = FALSE,cex=1.10, xlab = " ", ylab = " ")  ## This one will add secondary axis
    axis(4, col="#8B0046",col.axis="#8B0046",las=3,font=2,cex.axis=1.25)
    mtext(expression(bold("Percentage changes in the Irrigation wrt the optimal")), side = 4, line = 3, cex = 1.15)
    legend("bottomleft",legend=c("Percentage of the Yield","Percentage of the Irrigation"),text.col=c("#8B008B","#8B0046"),pch=c(12,13),col=c("#8B008B","#8B0046"), cex = 1)
    dev.off()
  }
}


###Loop for creating plot  cropwise plot###
meadONE <- meadonewk %>% 
  mutate(Month = paste0(substr(simulation,1,3)),
         shutdown = paste0(substr(simulation,4,6)),
         simulation = paste0(Month,"_",shutdown)) 

Six_Sm<- c("JUN_4TH","JUL_1ST","JUL_2ND","JUL_3RD","JUL_4TH","JUL_END")
#four_Sm<- c("JUN4TH","JUL1ST","JUL2ND","JUL3RD" )
meadweek<- meadONE[meadONE$simulation %in% Six_Sm,]

reg_ws<- unique(meadweek$region)
pafl<- "F:/Partial Water Leasing/Figure/Oneweek_New/SB3_V2/SIx/VV22/"
m=1
n=1
for ( m in 1: length(reg_ws)){
  rw<- reg_ws[[m]]
  ws<- dir.create(paste0(pafl,rw))
  sep_ws<- filter(meadweek, region==rw)
  sepdata<- sep_ws[,c("simulation","region","HUC10_NAME","crop","Percentage_irrigation","Percentage_yield")]  #Removing extra column
  sepd1<- melt(sepdata, id= c("simulation","region","HUC10_NAME","crop"))  #melting data for required shape for the graph
  un_subbasin<- unique(sepd1$HUC10_NAME)
  for (n in 1:length(un_subbasin)){
    c1= un_subbasin[[n]] #Select the crop
    #sub_dir<- dir.create(paste0(pafl,rw,"/",c1))
    setwd(paste0(pafl,rw,"/"))
    unsubbsn<- filter(sepd1, HUC10_NAME==c1)  #filter the crop
    unsubbsn$simulation<-  as.factor(unsubbsn$simulation)   #factoring and levelling required for chronological order
    unsubbsn$simulation<- factor(unsubbsn$simulation, levels= c("JUN_4TH","JUL_1ST","JUL_2ND","JUL_3RD","JUL_4TH","JUL_END"))
    
    ##Creating plot for each basin for crops
    G1<- ggplot(unsubbsn, aes(x=crop,y= value,fill=variable))+
      geom_boxplot(outlier.shape = NA)+  
      labs(x= "", y= "", title = paste0(c1,"_",rw))+ scale_y_continuous("",limits=c(0,100), breaks=seq(0,100,20))+
      coord_cartesian(ylim = c(0,100))+theme_bw()+ facet_wrap(~simulation, ncol = 2, nrow = 8)+
      theme(plot.title = element_text(size = 18),axis.title = element_text(size = 18,face="bold"), 
            strip.text = element_text(size = 16,face="bold"),axis.text.x = element_text(size = 16,color = "black",angle = 70, hjust=1,face="bold"),
            axis.text.y = element_text(size = 16,color = "black",angle = 0, hjust=1,face="bold"),legend.text = element_text(size=16), legend.title = element_blank(),legend.position = "bottom")
    
    ##Savuing the plot  
    ggsave(G1, file=paste0(c1,"_",rw,".png"), width = 40, height = 30,dpi = 300, units = "cm")
  }
}


######################
#### STREAM_FLOW  ####
######################
#streamflow<- read.csv("F:/Partial Water Leasing/Streamflow_subwatershwd/Streamflow.csv")   ##MainHourly data frame
#streamflow<- streamflow %>% na.omit()
##Stream_Daily
StreamM<- read.csv("F:/Partial Water Leasing/Streamflow_subwatershwd/StreamM_Daily.csv")

#####Simulation Week##
MAY1ST<- c(121:127)
MAY2ND<- c(128:134)
MAY3RD<- c(135:141)
MAY4TH<- c(142:148)
JUN1ST<- c(149:155)
JUN2ND<- c(156:162)
JUN3RD<- c(163:169)
JUN4TH<- c(170:176)
JUL1ST<- c(177:183)
JUL2ND<- c(184:190)
JUL3RD<- c(191:197)
JUL4TH<- c(198:204)
JULEND<- c(205:211)
AUG1ST<- c(212:219)
AUG2ND<- c(220:227)


Stream1<- StreamM[StreamM$DOY %in% (121:227),] %>% 
  mutate(simulation= ifelse(DOY %in% MAY1ST,"MAY1ST",
                            ifelse(DOY %in% MAY2ND,"MAY2ND",
                                   ifelse(DOY %in% MAY3RD,"MAY3RD",
                                          ifelse(DOY %in% MAY4TH,"MAY4TH",
                                                 ifelse(DOY %in% JUN1ST,"JUN1ST",
                                                        ifelse(DOY %in% JUN2ND,"JUN2ND",
                                                               ifelse(DOY %in% JUN3RD,"JUN3RD",
                                                                      ifelse(DOY %in% JUN4TH,"JUN4TH",
                                                                             ifelse(DOY %in% JUL1ST,"JUL1ST",
                                                                                    ifelse(DOY %in% JUL2ND,"JUL2ND",
                                                                                           ifelse(DOY %in% JUL3RD,"JUL3RD",
                                                                                                  ifelse(DOY %in% JUL4TH,"JUL4TH",
                                                                                                         ifelse(DOY %in% JULEND,"JULEND",
                                                                                                                ifelse(DOY %in% AUG1ST,"AUG1ST",
                                                                                                                       ifelse(DOY %in% AUG2ND,"AUG2ND",0))))))))))))))))


Stream<-Stream1 %>% 
  group_by(site_no,simulation) %>% 
  summarise(Discharge_cfs= mean(Discharge_cfs, na.rm = TRUE))
head(Stream)


##Read the Stream Location File##
StreamLocation<- read.csv("F:/Partial Water Leasing/Streamflow_subwatershwd/Streamgauge_LOcations.csv")
StreamML<- left_join(Stream,StreamLocation, by=c("site_no"="SITENO")) %>% 
  mutate(Discharge_log= log(Discharge_cfs)) %>% arrange(region)
StreamML<- filter(StreamML, SITENAME!="MOXEE DRAIN AT BIRCHFIELD ROAD NEAR UNION GAP, WA")

#write.csv(StreamML,"F:/Partial Water Leasing/Streamflow_subwatershwd/Gauge_LC_WK.csv")

###Editing The Name
StreamML2<- StreamML %>% 
  mutate(SITENAME= gsub(", WA","", as.character(SITENAME)),
         SITENAME= gsub("ABOVE",",", as.character(SITENAME)),
         SITENAME= gsub("BELOW","_", as.character(SITENAME)),
         SITENAME= gsub("NEAR","NR", as.character(SITENAME)),
         SITENAME= gsub("RIVER","RV", as.character(SITENAME)),
         SITENAME= gsub("DIVERSION","DIVR", as.character(SITENAME)),
         SITENAME= gsub("CREEK","CR", as.character(SITENAME)),
         SITENAME= gsub("AT","@", as.character(SITENAME))) %>% 
  filter(region!="Wenatchee")
#write.csv(StreamML2,"F:/Partial Water Leasing/Streamflow_subwatershwd/STREAM.csv")


#For order the SITENAME by region#
Mt<- filter(StreamML2, region=="Methow") %>% arrange(SITENAME)
Ok<- filter(StreamML2, region=="Okanogan") %>% arrange(SITENAME)
WL<- filter(StreamML2, region=="Walla Walla") %>% arrange(SITENAME)
Yk<- filter(StreamML2, region=="Yakima") %>% arrange(SITENAME)
StreamML3<- rbind(Mt,Ok)
StreamML3<- rbind(StreamML3,WL)
StreamML3<- rbind(StreamML3,Yk)
Station<- c("CHEWUCH RV @ WINTHROP", "METHOW RV @ CARLTON", "METHOW RV @ TWISP","METHOW RV @ WINTHROP", "METHOW RV NR P@EROS","SALMON CR , DIVR NR OKANOGAN",
            "WALLA WALLA RV NR TOUCHET" ,"TOPPENISH CR NR FORT SIMCOE","YAKIMA RV , AHTANUM CR @ UNION GAP","YAKIMA RV @ KIONA",  "YAKIMA RV @ MABTON", "YAKIMA RV @ UMTANUM")


##Creating matrix to create heatmap
USGS_Stream <- StreamML3 %>% 
  dplyr::select(simulation,SITENAME,Discharge_cfs,region) %>% 
  pivot_wider(.,id_cols = c(SITENAME,region),names_from = simulation,values_from = Discharge_cfs) %>% 
  filter(region!="Wenatchee")

##Filtering for selected station and editing the Simulation Name##
USGS_Stream<-USGS_Stream[USGS_Stream$SITENAME %in% Station,]
USGS_Stream3<- USGS_Stream[,c(1,3:17)]
USGS_STream_melt <- USGS_Stream3 %>% 
  melt(.,id = "SITENAME") %>% 
  mutate(Month = paste0(substr(variable,1,3)),
         shutdown = paste0(substr(variable,4,6)),
         simulation = paste0(Month,"_",shutdown)) 

USGS_STream_melt$simulation <- factor(USGS_STream_melt$simulation , ordered = TRUE, levels = c("MAY_1ST","MAY_2ND","MAY_3RD","MAY_4TH","JUN_1ST","JUN_2ND","JUN_3RD","JUN_4TH","JUL_1ST","JUL_2ND","JUL_3RD","JUL_4TH","JUL_END","AUG_1ST","AUG_2ND"))


##Final HeatMap Code
ggplot(data = USGS_STream_melt,aes(simulation,SITENAME,fill= value))+
  geom_tile()+
  scale_fill_gradient(low = "white", high = "blue",trans = "log10",name = "Discharge_cfs")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.text =  element_text(size=10,face="bold"),
        legend.position = "right",
        legend.key.size = unit(1, "cm"),
        legend.key.width = unit(0.5,"cm"))+
  theme(axis.text.x=element_text(size=12,face="bold",colour = "black",angle = 45,hjust = 1),
        axis.text.y=element_text(size=12,face="bold",colour = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())


##Heatmaply
breaks_m<- c(0,20,50,100,200,500,1000,3000,6000,9000,12500)
labelsm<- c(0,20,50,100,200,500,1000,3000,6000,9000,12500)

heatmaply(USGS_Stream1, #normalize(plot_matrix),
          Colv=NA,
          Rowv = NA,
          seriate = "GW",
          #scale_fill_gradient_fun = gradient_col2,
          #scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(low = "blue", high = "red", midpoint = 500, limits = c(0, 12500), trans="log"),
          scale_fill_gradient_fun = ggplot2::scale_fill_gradient(name="Discharge_cfs", trans="log",low = "blue", high = "red",breaks = breaks_m, labels = as.character(labelsm)),
          #scale_fill_gradient_fun = ggplot2::scale_fill_gradient(name="Discharge_cfs", trans="log",option="turbo",breaks = breaks_m, labels = as.character(labelsm)),
          k_row =4,
          margins = c(NA,200,60,NA),
          fontsize_row = 16,
          fontsize_col = 12,
          row_text_angle =0,
          column_text_angle =360,
          #main=paste("Heatmap for the deviation of Solar Radiation"), xlab= "Hour", ylab = "Month"
)



###Flow Accumulation File loaded from Bhupi##
Methow<- read.csv("F:/Partial Water Leasing/Streamflow_subwatershwd/FC/Methow.csv")
Okanogan<- read.csv("F:/Partial Water Leasing/Streamflow_subwatershwd/FC/okanogan.csv")
WallaWalla<- read.csv("F:/Partial Water Leasing/Streamflow_subwatershwd/FC/Wallawalla.csv")
Yakima<- read.csv("F:/Partial Water Leasing/Streamflow_subwatershwd/FC/Yakima.csv")

#put all data frames into list
df_list <- list(Methow, Okanogan, WallaWalla,Yakima)      

#merge all data frames together
Data<- Reduce(function(x, y) merge(x, y, all=TRUE), df_list)

#manipulating and creating data frame
Data1_Gf<- Data %>% 
  dplyr::select(value,simulation,region.x,SITENAME) %>% 
  inner_join(.,StreamML,by=c("SITENAME","simulation")) %>% 
  mutate(relative_flow = (value/Discharge_cfs)*100) #%>% 
  #filter(relative_flow<=10)

##renaming Site
Data1_Gf1<- Data1_Gf %>% 
  mutate(SITENAME= gsub(", WA","", as.character(SITENAME)),
         SITENAME= gsub("ABOVE",",", as.character(SITENAME)),
         SITENAME= gsub("BELOW","_", as.character(SITENAME)),
         SITENAME= gsub("NEAR","NR", as.character(SITENAME)),
         SITENAME= gsub("RIVER","RV", as.character(SITENAME)),
         SITENAME= gsub("DIVERSION","DIVR", as.character(SITENAME)),
         SITENAME= gsub("CREEK","CR", as.character(SITENAME)),
         SITENAME= gsub("AT","@", as.character(SITENAME))) %>% 
  filter(region!="Wenatchee")

##Filtering to order based on region
MTW<- filter(Data1_Gf1, region=="Methow") %>% arrange(SITENAME)
OKN<- filter(Data1_Gf1, region=="Okanogan") %>% arrange(SITENAME)
WLW<- filter(Data1_Gf1, region=="Walla Walla") %>% arrange(SITENAME)
YKM<- filter(Data1_Gf1, region=="Yakima") %>% arrange(SITENAME)
Data1_Gf2<- rbind(MTW,OKN)
Data1_Gf2<- rbind(Data1_Gf2,WLW)
Data1_Gf2<- rbind(Data1_Gf2,YKM)
DF<- filter(Data1_Gf2, relative_flow>10)
StationTo_plot<- c(unique(DF$SITENAME))

##Creating matrix to create heatmap
USGS_fraction <- Data1_Gf2 %>% 
  dplyr::select(simulation,SITENAME,relative_flow,region)%>% 
  pivot_wider(.,id_cols = c(SITENAME,region),names_from = simulation,values_from = relative_flow) %>% 
  filter(region!="Wenatchee")
#Check<- filter(USGS_fraction, SITENAME=="TWISP RV NR TWISP")
USGS_fraction <- USGS_fraction[complete.cases(USGS_fraction),]
#USGS_fraction2<- USGS_fraction[USGS_fraction$SITENAME %in% StationTo_plot,]

USGS_fraction1 <- USGS_fraction[,-(1:2)]
USGS_fraction1<-USGS_fraction1[,c(12,13,14,15,8,9,10,11,3,4,5,6,7,1,2)]
rownames(USGS_fraction1) <- USGS_fraction$SITENAME


heatmaply(USGS_fraction1, #normalize(plot_matrix),
          Colv=NA,
          Rowv = NA,
          seriate = "GW",
          #scale_fill_gradient_fun = gradient_col2,
          #scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(low = "blue", high = "red", midpoint = 500, limits = c(0, 12500), trans="log"),
          scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(name="Relative_Percentage",low = "blue", high = "red", midpoint=20, limits=c(0,50)),
          #scale_fill_gradient_fun = ggplot2::scale_fill_gradient(name="Discharge_cfs", trans="log",option="turbo",breaks = breaks_m, labels = as.character(labelsm)),
          k_row =4,
          #margins = c(NA,200,60,NA),
          fontsize_row = 16,
          fontsize_col = 12,
          row_text_angle =0,
          column_text_angle =360,
          #main=paste("Heatmap for the deviation of Solar Radiation"), xlab= "Hour", ylab = "Month"
)



###Creati ng dataframe for Relative Percentage Plot for HUC10###
StreamMH<- StreamML %>% 
  group_by(HUC10_NAME,simulation) %>% 
  summarise(Discharge_cfs= mean(Discharge_cfs, na.rm = TRUE))

huc_sum21<- left_join(huc_sum2,StreamMH, by=c("HUC10_NAME","simulation")) %>% na.omit() %>% 
  mutate(Relative_percentage=(Instreamflow_AP/Discharge_cfs)*100) 
huc_sum21<- left_join(huc_sum21,ab, by="simulation") %>% 
  mutate(Contributing_flow= (Instreamflow_AP+Discharge_cfs))


####Relative Percentage plot###
output_directory<- "F:/Partial Water Leasing/Figure/Oneweek_New/SB5_V2/"
r=1
s=1
basin<- unique(huc_sum21$region)
for ( p in 1: length(basin)){
  b<- basin[[p]]
  dir<- dir.create(paste0(output_directory,b))
  basin_sub<- filter(huc_sum21, region==b)
  sb<- unique(basin_sub$HUC10_NAME)
  for ( q in 1: length(sb)){
    sb1<- sb[[q]]
    #dir1<- dir.create(paste0(output_directory,b,"/",sb1))
    directory<- paste0(output_directory,b,"/")
    huc10<- filter(huc_sum21, HUC10_NAME==sb1) %>% arrange(Number)
    
    png(paste(directory,sb1,b,".png"), width=30, height=20, res=300, units = "cm")
    par(mgp=c(2.5,1,0),mar=c(6,4,4,6))
    plot(huc10$Number,huc10$Percentage_yield,type="b",lwd=3,col="red",xaxt="n",cex.axis=1.15,font=2,pch=17,
         main=paste0("Yield percentage and Instreamflow Augment. Pot. across one week shutdown scenarios","\n",sb1,"_",b),ylab = expression(bold("")),xlab = expression(bold("")), cex.lab=1.15,ylim = c(0,100) )
    axis(side=1,at=1:15,labels=ab$simulation,font = 2)
    axis(side=2,col.axis="red",font = 2,cex.lab=1.15,cex.axis=1.15)
    par(new =TRUE)
    plot(huc10$Number,huc10$Relative_percentage,col="blue",type = "b",pch=15, lty = 1,lwd = 2,axes = FALSE,cex=1.10, xlab = " ", ylab = " ")  ## This one will add secondary axis
    axis(4, col="blue",col.axis="blue",las=3, font = 2,cex.axis=1.25,ylim = c(0,max(huc10$Relative_percentage)))
    mtext(expression(bold("")), side = 4, line = 3, cex=1.15)
    legend("bottomright",legend=c("Percentage of the Yield","RLTV Percent. of Instreamflow Aug."),text.col=c("red","blue"),pch=c(17,15),col=c("red","blue"), cex = 1.15)
    dev.off()
  }
}



####Incremental plot for Sub-Basin###
output_directory<- "F:/Partial Water Leasing/Figure/Oneweek_New/SB6_V2/"
r=1
s=1
basin<- unique(huc_sum21$region)
for ( p in 1: length(basin)){
  b<- basin[[p]]
  dir<- dir.create(paste0(output_directory,b))
  setwd(paste0(output_directory,b,"/"))
  basin_sub<- filter(huc_sum21, region==b)
  sb<- unique(basin_sub$HUC10_NAME)
  for ( q in 1: length(sb)){
    sb1<- sb[[q]]
    #dir1<- dir.create(paste0(output_directory,b,"/",sb1))
    #directory<- paste0(output_directory,b,"/")
    huc10<- filter(huc_sum21, HUC10_NAME==sb1) %>% arrange(Number)
    huc10$simulation<-  as.factor(huc10$simulation)   #factoring and levelling required for chronological order
    huc10$simulation<- factor(huc10$simulation, 
                            levels= c('MAY1ST','MAY2ND', 'MAY3RD','MAY4TH','JUN1ST','JUN2ND','JUN3RD','JUN4TH','JUL1ST','JUL2ND','JUL3RD','JUL4TH','JULEND','AUG1ST','AUG2ND'))
    
    Q1<- ggplot(huc10, aes(x=simulation))+geom_line( mapping = aes(y=Contributing_flow, color="Discharge+Instreamflow_AP"), group=1)+
      geom_point( mapping = aes(y=Contributing_flow, color="Discharge+Instreamflow_AP"), group=1)+
      geom_line(mapping = aes(y=Discharge_cfs, color="Discharge_cfs"), group=2)+
      geom_point(mapping = aes(y=Discharge_cfs, color="Discharge_cfs"), group=2)+
      scale_y_continuous("",limits=c(0,round(max(huc10$Contributing_flow))), breaks=seq(0,round(max(huc10$Contributing_flow)), by=round(max(huc10$Contributing_flow)/10)))+theme_bw()+
      labs(x= "", y= "", title = paste0("Streamflow Comparison for one-week Shutdown","\n",sb1))+ 
      theme(axis.text= element_text(size = 17,face="bold"),plot.title = element_text(size = 20),axis.title = element_text(size = 20,face="bold"), 
            legend.text = element_text(size=20), legend.title = element_text(size=20),axis.text.x = element_text(size = 20,angle = 45, hjust=1,face="bold"))
    
    ggsave(Q1, file=paste0(sb1,".png"), width = 40, height = 30,dpi = 300, units = "cm")
  }
}



#####FOR GRIDS
Data_GF<- Data1_Gf1 %>% 
  mutate(Contributing_flow= (value+Discharge_cfs)) #%>% na.omit()
#check<- filter(Data_GF, SITENAME== "NINEMILE CR @ EASTLAKE RD NR OROVILLE")
output_directory<- "F:/Partial Water Leasing/Figure/Oneweek_New/SB6_V2/Gauge_Station/"
a=1
d=1
basin<- unique(Data_GF$region)
for ( a in 1: length(basin)){
  b<- basin[[a]]
  dir<- dir.create(paste0(output_directory,b))
  setwd(paste0(output_directory,b,"/"))
  basin_sub<- filter(Data_GF, region==b)
  sb<- unique(basin_sub$SITENAME)
  for ( d in 1: length(sb)){
    sb1<- sb[[d]]
    #dir1<- dir.create(paste0(output_directory,b,"/",sb1))
    #directory<- paste0(output_directory,b,"/")
    site<- filter(Data_GF, SITENAME==sb1) 
    site$simulation<-  as.factor(site$simulation)   #factoring and levelling required for chronological order
    site$simulation<- factor(site$simulation, 
                              levels= c('MAY1ST','MAY2ND', 'MAY3RD','MAY4TH','JUN1ST','JUN2ND','JUN3RD','JUN4TH','JUL1ST','JUL2ND','JUL3RD','JUL4TH','JULEND','AUG1ST','AUG2ND'))
    
    Q2<- ggplot(site, aes(x=simulation))+geom_line( mapping = aes(y=Contributing_flow, color="Discharge+Instreamflow_AP"), group=1)+
      geom_point( mapping = aes(y=Contributing_flow, color="Discharge+Instreamflow_AP"), group=1)+
      geom_line(mapping = aes(y=Discharge_cfs, color="Discharge_cfs"), group=1)+
      geom_point(mapping = aes(y=Discharge_cfs, color="Discharge_cfs"), group=1)+
      scale_y_continuous("",limits=c(0,round(max(site$Contributing_flow)+2)), breaks=seq(0,round(max(site$Contributing_flow)), by=round(max(site$Contributing_flow)/10)))+theme_bw()+
      labs(x= "", y= "",title = paste0("Streamflow Comparison for one-week Shutdown","\n",sb1))+ 
      theme(axis.text= element_text(size = 17,face="bold"),plot.title = element_text(size = 20),axis.title = element_text(size = 20,face="bold"), 
            legend.text = element_text(size=15), legend.title = element_text(size=17),axis.text.x = element_text(size = 20,angle = 45, hjust=1,face="bold"))
    
    ggsave(Q2, file=paste0(sb1,".png"), width = 40, height = 30,dpi = 300, units = "cm")
  }
}

##For Sepearate Station
AT<- Data_GF[Data_GF$SITENAME=="AHTANUM CR @ UNION GAP",]
AT$simulation<-  as.factor(AT$simulation)   #factoring and levelling required for chronological order
AT$simulation<- factor(AT$simulation, 
                         levels= c('MAY1ST','MAY2ND', 'MAY3RD','MAY4TH','JUN1ST','JUN2ND','JUN3RD','JUN4TH','JUL1ST','JUL2ND','JUL3RD','JUL4TH','JULEND','AUG1ST','AUG2ND'))

AT <- AT %>% 
  mutate(Month = paste0(substr(simulation,1,3)),
         shutdown = paste0(substr(simulation,4,6)),
         simulation = paste0(Month,"_",shutdown)) 

AT$simulation <- factor(AT$simulation , ordered = TRUE, levels = c("MAY_1ST","MAY_2ND","MAY_3RD","MAY_4TH","JUN_1ST","JUN_2ND","JUN_3RD","JUN_4TH","JUL_1ST","JUL_2ND","JUL_3RD","JUL_4TH","JUL_END","AUG_1ST","AUG_2ND"))

plot_AT <- ggplot(AT, aes(x=simulation))+geom_line( mapping = aes(y=Contributing_flow, color="Discharge+Instreamflow_AP"), group=1,size = 1)+
  geom_point( mapping = aes(y=Contributing_flow, color="Discharge+Instreamflow_AP"), group=1,size = 2)+
  geom_line(mapping = aes(y=Discharge_cfs, color="Discharge_cfs"), group=1,size = 1,linetype = "dashed")+
  geom_point(mapping = aes(y=Discharge_cfs, color="Discharge_cfs"), group=1,size = 2)+
  scale_y_continuous("",limits=c(0,round(max(AT$Contributing_flow)+2)), breaks=seq(0,round(max(AT$Contributing_flow)), by=round(max(AT$Contributing_flow)/10)))+theme_bw()+
  labs(x= "", y= "",title = paste0("Streamflow Comparison for one-week Shutdown","\n","AHTANUM CR @ UNION GAP"))+ 
  theme(plot.title = element_text(size = 20),axis.title = element_text(size = 20,face="bold"), 
        legend.text = element_text(size=15), legend.title = element_text(size=17),axis.text.x = element_text(size = 25,color = "black",angle = 45, hjust=1,face="bold"),axis.text.y = element_text(size = 25,angle = 0, hjust=1,face="bold",color = "black"),
        legend.position="none")

ggsave(plot_AT, file="F:/Partial Water Leasing/Figure/Oneweek_New/SB6_V2/Gauge_Station/plot_AT_V2.png", width = 40, height = 30,dpi = 300, units = "cm")


###Matrix For Creating ribbonplot instead of lineplot above####

QuantileRibbon_Si<- Stream1 %>%      
  group_by(site_no,Year, simulation) %>% 
  summarise(Q25_Dis= quantile(Discharge_cfs, probs = 0.25,na.rm = T),
            Q50_Dis= quantile(Discharge_cfs, probs = 0.5,na.rm = T),
            Q75_Dis= quantile(Discharge_cfs, probs = 0.75,na.rm = T),
            Q10_Dis= quantile(Discharge_cfs, probs = 0.1,na.rm = T),
            Q90_Dis= quantile(Discharge_cfs, probs = 0.9,na.rm = T))

RibbonLoc<- left_join(QuantileRibbon_Si,StreamLocation, by=c("site_no"="SITENO")) %>% arrange(region)
Accum_Site<- Data[,c("value","simulation","site_no","SITENAME")]

Ribbon_data<- inner_join(RibbonLoc,Accum_Site, by=c("simulation","site_no","SITENAME"))

