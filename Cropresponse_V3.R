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
library(ggpmisc)
library(ggplot2)
library (gganimate)
#library (rgdal)
library(grid)

###Dark Mode####
##https://r-coder.com/rstudio-themes/#:~:text=tmtheme%20to%20.rstheme-,Change%20RStudio%20themes,from%20light%20to%20dark%20themes.
#Apply Night owlish
##rstudioapi::addTheme("https://raw.githubusercontent.com/batpigandme/night-owlish/master/rstheme/night-owlish.rstheme", apply = TRUE)
#Synthwave85 <- "https://raw.githubusercontent.com/jnolis/synthwave85/master/Synthwave85.rstheme"
#rstudioapi::addTheme(Synthwave85, apply = TRUE)
#yule_theme <- "https://raw.githubusercontent.com/gadenbuie/yule-rstudio/master/Yule-RStudio.rstheme"
#rstudioapi::addTheme("https://raw.githubusercontent.com/gadenbuie/oceanic-eighties/master/oceanic-eighties.rstheme", apply = TRUE)


##Creating manual color palette
n <- 40
palette1 <- rainbow(n)  
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
pie(rep(1,n), col=sample(col_vector, n))

####Count the list##
#ab<- list.files("F:/Workspace/MainRun Scenarios/optimal/", pattern = ".", all.files = TRUE, recursive = TRUE, full.names = TRUE)

optimal<- fread("F:/Partial Water Leasing/CropSyst run Kamiak/Edited_Final Run 06112022/optimal/result.dat", fill = TRUE, nrows = Inf)
names(optimal)<- c("Year", "planting_date", "harvest_date", "yield", "used_biomass", "irrig", "precip", "region", "crop", "site")
optimal<- optimal %>% 
  separate(Year, into= c("Year", "Month","Day"), sep="-") %>%
  mutate(simulation= "optimal")
optimal<- optimal[,-c(2,3)]
optimal_ir<- optimal %>% 
  mutate(irrig_optimal= irrig)

###Continuous Deficit result###

sim<- list.files("F:/Partial Water Leasing/CropSyst run Kamiak/Edited_Final Run 06112022/Continuous/")
path<- "F:/Partial Water Leasing/CropSyst run Kamiak/Edited_Final Run 06112022/Continuous/"
simulation<- rep()
for (i in 1:length(sim)){
  p1=sim[[i]]
  #p1[i]=p1
  set_path<- paste0(path,p1)
  result<- fread(paste0(path,p1,"/result.dat"), fill = TRUE, nrows = Inf)
  names(result)<- c("Year", "planting_date", "harvest_date", "yield", "used_biomass", "irrig", "precip", "region", "crop", "site")
  result<- result %>% 
    separate(Year, into= c("Year", "Month","Day"), sep="-") %>%
    mutate(simulation= p1)
  result<- result[,-c(2,3)]
  output1<- rbind(simulation, result)
  simulation<- output1
}
data<- rbind(simulation,optimal)
data<- data %>%
  dplyr::select(Year,planting_date,harvest_date,yield,used_biomass,irrig,precip,region,crop,site,simulation) %>% 
  left_join(optimal_ir %>% dplyr::select(Year,planting_date,region,crop,site,irrig_optimal)) %>% 
  mutate(Percentage_irrig= (irrig/irrig_optimal)*100)

Wallawalla1<- filter(data, region== "WallaWalla")
Yakima1<- filter(data, region== "Yakima")
Okanogan1<- filter(data, region== "Okanogan")
setwd("C:/Users/mdredwanahmad.khan/OneDrive - Washington State University (email.wsu.edu)/Projects/Partial leasing/Figure/Crop response Curve/Okanogan/")
Alfalfa<- filter(Okanogan1, crop== "Winter_wheat")

ggplot(Alfalfa, mapping = aes(x =irrig,y=yield)) +
  geom_line(aes(color= Year, linetype= Year))+geom_smooth(method = "lm", formula = y ~ poly(x,3, raw=TRUE))+
  stat_poly_eq(formula = y ~ poly(x,3, raw=TRUE),
               aes(label = paste(..eq.label.., ..rr.label..,  sep = "*\", \"*")),label.x = 0.03,label.y = 0.95, parse = TRUE, size= 2.5)+
  labs( title= "Winter_wheat\nContinuous Deficit Irrigation", y="Yield ( kg/ha)", x = "Irrigation water applied (mm/year)")+ theme_bw()+
  scale_color_viridis_d(option = "turbo")+
  theme(axis.text= element_text(size = 12),plot.title = element_text(size = 12),axis.title = element_text(size = 12))+facet_wrap(~site)

ggplot(Alfalfa, mapping = aes(x =Percentage_irrig,y=yield)) +
  geom_line(aes(color= Year, linetype= Year))+geom_smooth(method = "lm", formula = y ~ poly(x,3, raw=TRUE))+
  stat_poly_eq(formula = y ~ poly(x,3, raw=TRUE),
               aes(label = paste(..eq.label.., ..rr.label..,  sep = "*\", \"*")),label.x = 0.03,label.y = 0.95, parse = TRUE, size= 2.5)+
  labs( title= "Winter_wheat\nContinuous Deficit Irrigation", y="Yield ( kg/ha)", x = "Percentage Irrigation applied of optimal amount")+ theme_bw()+
  scale_color_viridis_d(option = "turbo")+
  theme(axis.text= element_text(size = 12),plot.title = element_text(size = 12),axis.title = element_text(size = 12))+facet_wrap(~site)

Alfalfa<- filter(Wallawalla1, crop== "Spring_wheat")
drop<- c("1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992")
Alfalfa<- subset(Alfalfa, !Year %in% drop)
ggplot(Alfalfa, mapping = aes(x =irrig,y=yield)) +
  geom_line(aes(color= Year, linetype= Year))+geom_smooth(method = "lm", formula = y ~ poly(x,3, raw=TRUE))+
  stat_poly_eq(formula = y ~ poly(x,3, raw=TRUE),
               aes(label = paste(..eq.label.., ..rr.label..,  sep = "*\", \"*")),label.x = 0.03,label.y = 0.95, parse = TRUE, size= 2.5)+
  labs( title= "Winter_wheat\nContinuous Deficit Irrigation (For the Whole basin)", y="Yield ( kg/ha)", x = "Irrigation water applied (mm/year)")+ theme_bw()+
  scale_color_manual(values=palette1)+
  theme(axis.text= element_text(size = 12),plot.title = element_text(size = 12),axis.title = element_text(size = 12))

###biomass

Alfalfa<- filter(Okanogan1, crop== "Barley_Hay")

ggplot(Alfalfa, mapping = aes(x =irrig,y=used_biomass)) +
  geom_line(aes(color= Year, linetype= Year))+geom_smooth(method = "lm", formula = y ~ poly(x,3, raw=TRUE))+
  stat_poly_eq(formula = y ~ poly(x,3, raw=TRUE),
               aes(label = paste(..eq.label.., ..rr.label..,  sep = "*\", \"*")),label.x = 0.03,label.y = 0.95, parse = TRUE, size= 2.0)+
  labs( title= "Barley_Hay\nContinuous Deficit Irrigation", y="Biomass for Beneficial Use ( kg/ha)", x = "Irrigation water applied (mm/year)")+ theme_bw()+
  scale_color_viridis_d(option = "turbo")+
  theme(axis.text= element_text(size = 12),plot.title = element_text(size = 12),axis.title = element_text(size = 12))+facet_wrap(~site)

ggplot(Alfalfa, mapping = aes(x =Percentage_irrig,y=used_biomass)) +
  geom_line(aes(color= Year, linetype= Year))+geom_smooth(method = "lm", formula = y ~ poly(x,3, raw=TRUE))+
  stat_poly_eq(formula = y ~ poly(x,3, raw=TRUE),
               aes(label = paste(..eq.label.., ..rr.label..,  sep = "*\", \"*")),label.x = 0.03,label.y = 0.95, parse = TRUE, size= 2.0)+
  labs( title= "Barley_Hay\nContinuous Deficit Irrigation", y="Biomass for Beneficial Use ( kg/ha)", x = "Percentage Irrigation applied of optimal amount")+ theme_bw()+
  scale_color_viridis_d(option = "turbo")+
  theme(axis.text= element_text(size = 12),plot.title = element_text(size = 12),axis.title = element_text(size = 12))+facet_wrap(~site)

ggplot(Alfalfa, mapping = aes(x =irrig,y=used_biomass)) +
  geom_line(aes(color= Year, linetype= Year))+geom_smooth(method = "lm", formula = y ~ poly(x,3, raw=TRUE))+
  stat_poly_eq(formula = y ~ poly(x,3, raw=TRUE),
               aes(label = paste(..eq.label.., ..rr.label..,  sep = "*\", \"*")),label.x = 0.03,label.y = 0.95, parse = TRUE, size= 2.5)+
  labs( title= "Barley_Hay\nContinuous Deficit Irrigation (For the Whole basin)", y="Biomass for Beneficial Use ( kg/ha)", x = "Irrigation water applied (mm/year)")+ theme_bw()+
  scale_color_viridis_d(option = "turbo")+
  theme(axis.text= element_text(size = 12),plot.title = element_text(size = 12),axis.title = element_text(size = 12))

ggplot(Alfalfa, mapping = aes(x =irrig,y=yield)) +
  geom_point( aes(y= yield))+geom_smooth(method = "lm", formula = y ~ poly(x,3, raw=TRUE))+ 
  stat_poly_eq(formula = y ~ poly(x,3, raw=TRUE),aes(label = paste(..eq.label.., ..rr.label..,  sep = "*\", \"*")),label.x = 0.03,label.y = 0.95, parse = TRUE, size=1.5)+
  facet_wrap(~Year)

Al<- Alfalfa %>% 
  group_by(Year,simulation, crop,region) %>% 
  summarise(irrig= mean(irrig, na.rm=TRUE),
            yield= mean(yield, na.rm=TRUE),
            used_biomass= mean(used_biomass, na.rm=TRUE),
            precip= mean(precip, na.rm=TRUE))

ggplot(Al, mapping =  aes( x = irrig, y = used_biomass, fill = factor(Year )) ) +    # print bar chart
  geom_boxplot()+scale_color_viridis_c(option = "turbo")+theme_bw()

###Fifteen days###
sim<- list.files("F:/Partial Water Leasing/CropSyst run Kamiak/Edited_Final Run 06112022/Fifteen days of Deficit/")
path<- "F:/Partial Water Leasing/CropSyst run Kamiak/Edited_Final Run 06112022/Fifteen days of Deficit/"
simulation2<- rep()
for (i in 1:length(sim)){
  p1=sim[[i]]
  #p1[i]=p1
  set_path<- paste0(path,p1)
  result2<- fread(paste0(path,p1,"/result.dat"), fill = TRUE, nrows = Inf)
  names(result2)<- c("Year", "planting_date", "harvest_date", "yield", "used_biomass", "irrig", "precip", "region", "crop", "site")
  result2<- result2 %>% 
    separate(Year, into= c("Year", "Month","Day"), sep="-") %>%
    mutate(simulation= p1)
  result2<- result2[,-c(2,3)]
  output2<- rbind(simulation2, result2)
  simulation2<- output2
}
data2<- rbind(simulation2,optimal)
ALfalfa<- filter(data2, crop== "Spring_wheat",region== "WallaWalla")
sum15days<- ALfalfa %>% 
  group_by(Year,simulation, crop,region) %>% 
  summarise(irrig= mean(irrig, na.rm=TRUE),
            yield= mean(yield, na.rm=TRUE),
            used_biomass= mean(used_biomass, na.rm=TRUE),
            precip= mean(precip, na.rm=TRUE))
sm<- data2 %>% 
  group_by(simulation, region) %>% 
  summarise(biomass= sum(used_biomass, na.rm=TRUE),
            yield= sum(yield,na.rm=TRUE),
            irrig= sum(irrig, na.rm=TRUE),
            precip= sum(precip, na.rm=TRUE))
sm<- data2 %>% 
  group_by(simulation, region, Year) %>% 
  summarise(biomass= mean(used_biomass, na.rm=TRUE),
            yield= mean(yield,na.rm=TRUE),
            irrig= sum(irrig, na.rm=TRUE),
            precip= sum(precip, na.rm=TRUE)) 
sm<- sm[with(sm,order(2,3))]
ggplot(ALfalfa, mapping = aes(x =irrig,y=used_biomass)) +
  geom_point( aes(y= used_biomass))+geom_smooth(method = "lm", formula = y ~ poly(x,3, raw=TRUE))+ 
  stat_poly_eq(formula = y ~ poly(x,3, raw=TRUE),aes(label = paste(..eq.label.., ..rr.label..,  sep = "*\", \"*")),label.x = 0.03,label.y = 0.95, parse = TRUE)+
  facet_wrap(~Year)
ggplot(Alfalfa, mapping = aes(x =irrig,y=used_biomass)) +
  geom_point( aes(y= used_biomass))+ geom_line(aes(y= used_biomass))+geom_smooth(method = "lm", formula = y ~ poly(x,3, raw=TRUE))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3, raw=TRUE))+stat_poly_eq(formula = y ~ poly(x,3, raw=TRUE),
                                                                             aes(label = paste(..eq.label.., ..rr.label..,  sep = "*\", \"*")),label.x = 0.03,label.y = 0.95, parse = TRUE, size= 2.5)+
  labs( title= "Alfalfa_Hay\nContinuous Deficit Irrigation", y="Above ground biomass( kg/ha)", x = "Irrigation water applied (mm/year)")+ theme_bw()+
  theme(axis.text= element_text(size = 12),plot.title = element_text(size = 12),axis.title = element_text(size = 12))+facet_wrap(~Year)


ggplot(Alfalfa, mapping = aes(x =irrig,y=used_biomass)) +
  geom_line(aes(color= Year, linetype= Year))+geom_smooth(method = "lm", formula = y ~ poly(x,3, raw=TRUE))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3, raw=TRUE))+stat_poly_eq(formula = y ~ poly(x,3, raw=TRUE),
                                                                             aes(label = paste(..eq.label.., ..rr.label..,  sep = "*\", \"*")),label.x = 0.03,label.y = 0.95, parse = TRUE, size= 2.5)+
  labs( title= "Alfalfa_Hay\nContinuous Deficit Irrigation", y="Above ground biomass( kg/ha)", x = "Irrigation water applied (mm/year)")+ theme_bw()+
  scale_color_viridis_d(option = "turbo")+
  theme(axis.text= element_text(size = 12),plot.title = element_text(size = 12),axis.title = element_text(size = 12))+facet_wrap(~site)

Alfa<- Alfalfa %>% 
  pivot_wider(names_from = simulation, values_from = c("irrig","yield"))

ggplot()+geom_ribbon(data=Alfalfa, aes(x= irrig, ymin= ))

Corn_grain<- filter(data1, crop=="Spring_wheat")

ggplot(Corn_grain, mapping = aes(x =irrig,y=yield)) +
  geom_point( aes(y= yield)) +
  geom_smooth(method = "lm", formula = y ~ poly(x,3, raw=TRUE))+stat_poly_eq(formula = y ~ poly(x,3, raw=TRUE),
                                                                             aes(label = paste(..eq.label.., ..rr.label..,  sep = "*/", /"*")),label.x = 0.03,label.y = 0.95, parse = TRUE)+
  labs( title= "Winter Wheat/nContinuous Deficit Irrigation ", y="Yield ( kg/ha)", x = "Irrigation water applied (mm/year)")+ theme_bw()+
  theme(axis.text= element_text(size = 14),plot.title = element_text(size = 14),axis.title = element_text(size = 14))+facet_wrap(~region)

stat<- Wallawalla1 %>% 
  group_by(crop , simulation) %>% 
  summarise(Yield_kg_ha= mean(yield, na.rm=TRUE),
            biomass_kg_ha= mean(used_biomass, na.rm = TRUE),
            Irrigation_mm= mean(irrig, na.rm=TRUE),
            Precim_mm= mean(precip, na.rm = TRUE))%>%
  arrange(crop,simulation)

###Deficit After Flowering###

dfaft<- list.files("F:/Partial Water Leasing/CropSyst run Kamiak/Edited_Final Run 06112022/AFTER FLOWERING1/")
#sim<- sim[3:12]
pathdfaft<- "F:/Partial Water Leasing/CropSyst run Kamiak/Edited_Final Run 06112022/AFTER FLOWERING1/"
simulation1<- rep()
for (i in 1:length(dfaft)){
  p1=sim[[i]]
  #p1[i]=p1
  set_path<- paste0(path,p1)
  result<- fread(paste0(path,p1,"/result.dat"), fill = TRUE, nrows = Inf)
  result<- result[,c(100,101,99,22,23,38,39)] %>% 
    mutate(simulation= p1)
  output1<- rbind(simulation1, result)
  simulation1<- output1
}
data2<- rbind(simulation1,opt)
drop<- c("Alfalfa_Hay","Grass_Hay","Triticale_Hay", "Barley_Hay","Sudangrass",
         "Timothy","Rye", "Oats_hay")
data2<- subset(data2, !crop %in% drop)
Wallawalla1<- filter(data2, region== "WallaWalla")
Yakima1<- filter(data1, region== "Yakima")
Okanogan1<- filter(data1, region== "Okanogan")

stat<- Wallawalla1 %>% 
  group_by(crop , simulation, region) %>% 
  summarise(Yield_kg_ha= mean(yield, na.rm=TRUE),
            biomass_kg_ha= mean(used_biomass, na.rm = TRUE),
            Irrigation_mm= mean(irrig, na.rm=TRUE),
            Precim_mm= mean(precip, na.rm = TRUE))

Corn_grain<- filter(data2, crop=="Hops")

ggplot(Corn_grain, mapping = aes(x =irrig,y=yield)) +
  geom_point( aes(y= yield)) +
  geom_smooth(method = "lm", formula = y ~ poly(x,3, raw=TRUE))+stat_poly_eq(formula = y ~ poly(x,3, raw=TRUE),
                                                                             aes(label = paste(..eq.label.., ..rr.label..,  sep = "*/", /"*")),label.x = 0.03,label.y = 0.95, parse = TRUE)+
  labs( title= "Hops/nDeficit Irrigation After Flowering", y="Yield ( kg/ha)", x = "Irrigation water applied (mm/year)")+ theme_bw()+
  theme(axis.text= element_text(size = 14),plot.title = element_text(size = 14),axis.title = element_text(size = 14))+facet_wrap(~region)

###Deficit Before Flowering####

sim<- list.files("F:/Workspace/PWL/Scenarios/Kamika/Before Flowering/")
sim<- sim[3:12]
path<- "F:/Workspace/PWL/Scenarios/Kamika/Before Flowering/"
simulation2<- rep()
for (i in 1:length(sim)){
  p1=sim[[i]]
  #p1[i]=p1
  set_path<- paste0(path,p1)
  result<- fread(paste0(path,p1,"/result.dat"), fill = TRUE, nrows = Inf)
  result<- result %>% 
    mutate(simulation= p1)
  output2<- rbind(simulation2, result)
  simulation2<- output2
}

data3<- rbind(simulation2,opt)


Alfalfa<- filter(data3, crop== "Alfalfa_Hay")

ggplot(Alfalfa, mapping = aes(x =irrig,y=used_biomass)) +
  geom_point( aes(y= used_biomass)) +
  geom_smooth(method = "lm", formula = y ~ poly(x,3, raw=TRUE))+stat_poly_eq(formula = y ~ poly(x,3, raw=TRUE),
                                                                             aes(label = paste(..eq.label.., ..rr.label..,  sep = "*/", /"*")),label.x = 0.03,label.y = 0.95, parse = TRUE)+
  labs( title= "Alfalfa Hay/nContinuous Deficit Irrigation ", y="Above ground biomass( kg/ha)", x = "Irrigation water applied (mm/year)")+ theme_bw()+
  theme(axis.text= element_text(size = 14),plot.title = element_text(size = 14),axis.title = element_text(size = 14))+facet_wrap(~region)

Corn_grain<- filter(data1, crop=="Spring_wheat")

ggplot(Corn_grain, mapping = aes(x =irrig,y=yield)) +
  geom_point( aes(y= yield)) +
  geom_smooth(method = "lm", formula = y ~ poly(x,3, raw=TRUE))+stat_poly_eq(formula = y ~ poly(x,3, raw=TRUE),
                                                                             aes(label = paste(..eq.label.., ..rr.label..,  sep = "*/", /"*")),label.x = 0.03,label.y = 0.95, parse = TRUE)+
  labs( title= "Winter Wheat/nContinuous Deficit Irrigation ", y="Yield ( kg/ha)", x = "Irrigation water applied (mm/year)")+ theme_bw()+
  theme(axis.text= element_text(size = 14),plot.title = element_text(size = 14),axis.title = element_text(size = 14))+facet_wrap(~region)


#########One week shutdown########
filesfolder<- list.files("F:/Partial Water Leasing/CropSyst run Kamiak/Edited_Final Run 06112022/Oneweek_shutdown/")
path1<- "F:/Partial Water Leasing/CropSyst run Kamiak/Edited_Final Run 06112022/Oneweek_shutdown/"
df<- rep()
for (i in 1:length(filesfolder)){
  p1=filesfolder[[i]]
  #p1[i]=p1
  set_path<- paste0(path1,p1)
  result1<- fread(paste0(path1,p1,"/result.dat"), fill = TRUE, nrows = Inf)
  names(result1)<- c("Year", "planting_date", "harvest_date", "yield", "used_biomass", "irrig", "precip", "region", "crop", "site")
  result1<- result1 %>% 
    separate(Year, into= c("Year", "Month","Day"), sep="-") %>%
    mutate(simulation= p1)
  result1<- result1[,-c(2,3)]
  output2<- rbind(df, result1)
  df<- output2
}
data1<- rbind(df,optimal)
data1<- data1 %>%
  dplyr::select(Year,planting_date,harvest_date,yield,used_biomass,irrig,precip,region,crop,site,simulation) %>% 
  left_join(optimal_ir %>% dplyr::select(Year,planting_date,region,crop,site,irrig_optimal)) %>% 
  mutate(Percentage_irrig= (irrig/irrig_optimal)*100)

tst<- data1 %>% 
  group_by(region, crop, Year, simulation) %>% 
  summarise(irrig= mean(irrig, na.rm = TRUE),
            yield= mean(yield, na.rm=TRUE),
            used_biomass= mean(used_biomass, na.rm = TRUE),
            per_irri= mean(Percentage_irrig,na.rm = TRUE))

drop<- c("MAY3RD")
tst<- subset(tst, !simulation %in% drop)
ggplot(tst, aes(fill=simulation, y=per_irri, x=per_irri))+
  geom_boxplot()

ggplot(tst, aes(fill=simulation, y=per_irri, x=irrig))+
  geom_boxplot()+facet_wrap()
ggplot(tst, aes(fill=simulation, y=per_irri, x=irrig))+
  geom_violin(trim=FALSE)+facet_wrap(~simulation)

crp<- filter(tst, used_biomass==0)
biomass<- filter(tst, yield==0)

ggplot(crp, mapping = aes(x =per_irri,y=yield)) +
  geom_line(aes(color= simulation, linetype= simulation))+
  labs( title= "Barley_Hay\nContinuous Deficit Irrigation", y="Yield ( kg/ha)", x = "Percentage Irrigation")+ theme_bw()+
  scale_color_viridis_d(option = "turbo")+
  theme(axis.text= element_text(size = 12),plot.title = element_text(size = 12),axis.title = element_text(size = 12))
library(ggplot2)
ggplot(crp, aes(x=simulation, y=per_irri,fill=region))+
  geom_violin(trim=FALSE)

tst1<- data1 %>% 
  group_by(simulation) %>% 
  summarise(irrig= mean(irrig, na.rm = TRUE),
            yield= mean(yield, na.rm=TRUE),
            used_biomass= mean(used_biomass, na.rm = TRUE),
            per_irri= mean(Percentage_irrig,na.rm = TRUE),
            optimal_irrig=mean(irrig_optimal,na.rm = TRUE))
sum<- data1 %>% 
  group_by(region, crop, Year, simulation) %>% 
  summarise(irrig= sum(irrig, na.rm = TRUE),
            yield= sum(yield, na.rm=TRUE),
            used_biomass= sum(used_biomass, na.rm = TRUE),
            per_irri= mean(Percentage_irrig,na.rm = TRUE))
op<- optimal %>% 
  group_by(region, crop, Year, simulation) %>% 
  summarise(irrig_op= sum(irrig, na.rm = TRUE),
            yield_op= sum(yield, na.rm=TRUE),
            used_biomass_op= sum(used_biomass, na.rm = TRUE))

sum<- sum %>%
  dplyr::select(region,crop,Year,simulation,yield,used_biomass,irrig,per_irri) %>% 
  left_join(op %>% dplyr::select(region,crop, Year, irrig_op, yield_op, used_biomass_op))
