library(data.table)
library(dplyr)
library(zoo) #for some reason the function na.locf is just in the zoo package for me now
library(magrittr) # For extra-piping operators (eg. %<>%)
library(psych) #for descriptives

library(MatchIt) 
library(lmtest) #for coeftest
library(sandwich) #for vcovCL

library(did) #for treatment effect over distinct periods of time

library(stringr) #for extracting numbers from factors
library(ggplot2)
library(readr) #for reading text files;

#devtools::install_github("bcallaway11/did") #to get the last update of the did package (it is not in cran yet);

#1.Relatedness----
#1.1.MNE Level
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
DataLong_Subs_sim <- read.csv("Data/Data_matched_MNEs.csv", sep = ";", header = TRUE, dec=",")

#1.1.1.Simple Aggregation
example_attgt <- att_gt(yname = "Relatedness_Cos2", #the complexity extended;
                        tname = "CurrentYear",
                        idname = "id",
                        gname = "first.treat",
                        xformla = ~1,
                        data = DataLong_Subs_sim
)
agg.simple <- aggte(example_attgt, type = "simple")
summary(agg.simple) #effect:  1.0371, error: 0.1693, [95%  Conf. Int.]: 0.7054      1.3689 *
#aggregated of type "simple" doesn't allow plots;

#1.1.2.Dynamic Effects and Event Studies
agg.es <- aggte(example_attgt, type = "dynamic")
summary(agg.es) #effect 1.1526, error: 0.2611, [95%  Conf. Int.]: 0.6409      1.6643 *
ggdid(agg.es) 

jpeg("Figures/Figure1a.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "a) Average effect by length of exposure for the MNE") + 
  theme(legend.position="right") +
  ylab("Estimated effect on Relatedness") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","purple")) + #royalblue1 #dodgerblue2  #green3 #springgreen3
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
dev.off()

table(DataLong_Subs_sim$Group)
length(unique(DataLong_Subs_sim$id)) #6930

#create new dataframe for MNEs without holding-related sectors (6420 and 7010)
table(DataLong_Subs_sim$CategoryNace) #6468 from 6420 and 2016 from 7010;
Data_Long_filtered <- DataLong_Subs_sim[DataLong_Subs_sim$CategoryNace != 6420,]
Data_Long_filtered <- Data_Long_filtered[Data_Long_filtered$CategoryNace != 7010,]

table(Data_Long_filtered$CategoryNace) #0 from 6420 and 0 from 7010;

TreatedUnits <- Data_Long_filtered[Data_Long_filtered$treat == 1,]
length(unique(TreatedUnits$id)) #1054
table(TreatedUnits$CategoryNace) #no 6420 nor 7010

TreatedUnits <- DataLong_Subs_sim[DataLong_Subs_sim$treat == 1,]
length(unique(TreatedUnits$id)) #1155, so 101 treated units are lost by excluding both codes, all of them are from code 6420 
table(TreatedUnits$CategoryNace)
rm(TreatedUnits)

#1.1.3.Simple Aggregation - robustness without holdings
example_attgt <- att_gt(yname = "Relatedness_Cos2", #the complexity extended;
                        tname = "CurrentYear",
                        idname = "id",
                        gname = "first.treat",
                        xformla = ~1,
                        data = Data_Long_filtered
)

agg.simple <- aggte(example_attgt, type = "simple")
summary(agg.simple) #effect: 1.1141, error: 0.1794, [95%  Conf. Int.]: 0.7625      1.4657 *

#1.1.4.Dynamic Effects and Event Studies - robustness without holdings
agg.es <- aggte(example_attgt, type = "dynamic")
summary(agg.es) #effect 1.2534, error: 0.2807, [95%  Conf. Int.]: 0.7032      1.8036 *
ggdid(agg.es) 

jpeg("Figures/Figure1b.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "b) Average effect by length of exposure for the MNE - no holding companies") + 
  theme(legend.position="right") +
  ylab("Estimated effect on Relatedness") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","orange")) + #royalblue1 #dodgerblue2  #green3 #springgreen3 yellowgreen
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
dev.off()

#calculate the mean Turnover_extended of treated (1) versus non treated (0)
DataLong_Subs_sim3 <- data.table(DataLong_Subs_sim)
DataLong_Subs_sim3$treat <- as.numeric(ifelse(DataLong_Subs_sim3$first.treat==0,0,1))
DataLong_Subs_sim3[first.treat == 0 | first.treat != 0 , mean(Relatedness_Cos2), by = treat ] #
#10.09532 for non-treated (0) and 11.86333 for treated (1)
#considering the effects seen (4.1916 for simple and 6.3581 for dynamic):
1.0371/10.09532 #0.1027308 increase on average for the simple
1.1526/10.09532 #0.1141717 for dynamic
rm(DataLong_Subs_sim3)

#calculate the mean Turnover_extended of treated (1) versus non treated (0) with holdings excluded
DataLong_Subs_sim3 <- data.table(Data_Long_filtered)
DataLong_Subs_sim3$treat <- as.numeric(ifelse(DataLong_Subs_sim3$first.treat==0,0,1))
DataLong_Subs_sim3[first.treat == 0 | first.treat != 0 , mean(Relatedness_Cos2), by = treat ] #
#10.34620 for non-treated (0) and 12.01965 for treated (1)
#considering the effects seen (4.1916 for simple and 6.3581 for dynamic):
1.1141/10.34620 #0.107682 increase on average for the simple
1.2534/10.34620 #0.1211459 for dynamic
rm(DataLong_Subs_sim3)

#2.Relatedness effects across sectors -----
#2.1.By most used codes-----
#This is the first proposed distance measure, in which all companies that belong to ICT-related sectors (NACE 5829, 6201, 6311, 
#6312, 6391, and 6399) are put in the closest to AI category, MNEs previously classified into the “Less used codes” (which 
#comprehends the 20% NACE codes less used by AI adopters) are put in the category with the greatest distance to AI, and all 
#remaining MNEs are put in a median group
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
DataLong_Subs_sim <- read.csv("Data/Data_matched_MNEs.csv", sep = ";", header = TRUE, dec=",")

table(DataLong_Subs_sim$Nace_4d)
FreqNacecodes <- as.data.frame(table(DataLong_Subs_sim$Nace_4d))
FreqNacecodes[order(-FreqNacecodes$Freq),][1:10,] #6201 (9576), 6420 (6468), 5829 (6300)

#option 2: picking the first two digits:
DataLong_Subs_sim_group1b <- DataLong_Subs_sim[DataLong_Subs_sim$Nace_4d == "6201"|
                                                 DataLong_Subs_sim$Nace_4d == "5829"|
                                                 DataLong_Subs_sim$Nace_4d == "6311"|
                                                 DataLong_Subs_sim$Nace_4d == "6312"|
                                                 DataLong_Subs_sim$Nace_4d == "6391"|
                                                 DataLong_Subs_sim$Nace_4d == "6399",]

DataLong_Subs_sim_group3b <- DataLong_Subs_sim[DataLong_Subs_sim$CategoryNace == "Less_used_codes",]
'%notin%' <- Negate('%in%')
DataLong_Subs_sim_group2b<-DataLong_Subs_sim[DataLong_Subs_sim$id %notin% DataLong_Subs_sim_group1b$id,]
DataLong_Subs_sim_group2b<-DataLong_Subs_sim_group2b[DataLong_Subs_sim_group2b$id %notin% DataLong_Subs_sim_group3b$id,]
dim(DataLong_Subs_sim_group1b)[1] #16716
dim(DataLong_Subs_sim_group2b)[1] #55020
dim(DataLong_Subs_sim_group3b)[1] #25368
dim(DataLong_Subs_sim_group1b)[1]+ dim(DataLong_Subs_sim_group2b)[1] +dim(DataLong_Subs_sim_group3b)[1] #97104
dim(DataLong_Subs_sim)[1] #97020, so its close but not the same... probably due to some repeated Ids;

TreatedUnits_group1 <- DataLong_Subs_sim_group1b[DataLong_Subs_sim_group1b$treat == 1,]
length(unique(TreatedUnits_group1$id)) #204
rm(TreatedUnits_group1)

TreatedUnits_group2 <- DataLong_Subs_sim_group2b[DataLong_Subs_sim_group2b$treat == 1,]
length(unique(TreatedUnits_group2$id)) #655
rm(TreatedUnits_group2)

TreatedUnits_group3 <- DataLong_Subs_sim_group3b[DataLong_Subs_sim_group3b$treat == 1,]
length(unique(TreatedUnits_group3$id)) #302
rm(TreatedUnits_group3)

#2.1.1.Calculate effects for each group 
#2.1.1.1.Group 2
#2.1.1.1.1.Simple effects
example_attgt <- att_gt(yname = "Relatedness_Cos2", #the complexity extended;
                        tname = "CurrentYear",
                        idname = "id",
                        gname = "first.treat",
                        xformla = ~1,
                        data = DataLong_Subs_sim_group2b
)

agg.simple <- aggte(example_attgt, type = "simple")
summary(agg.simple) #effect:  0.7605, error: 0.2126, [95%  Conf. Int.]: 0.3439      1.1772 *

#2.1.1.1.2.Dynamic Effects and Event Studies - Group 2
agg.es <- aggte(example_attgt, type = "dynamic")
summary(agg.es) #effect 0.8074, error: 0.327, [95%  Conf. Int.]: 0.1665      1.4483 *
ggdid(agg.es) 

jpeg("Figures/FigureAppendix_A61_b.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "b) Average effect by length of exposure for the entire company - Group non-IT") + 
  theme(legend.position="right") +
  ylab("Estimated effect on Relatedness") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","purple")) + #royalblue1 #dodgerblue2  #green3 #springgreen3
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
dev.off()

#2.1.1.2.Group 1
#2.1.1.2.1.Simple effects
example_attgt <- att_gt(yname = "Relatedness_Cos2", #the relatedness extended;
                        tname = "CurrentYear",
                        idname = "id",
                        gname = "first.treat",
                        xformla = ~1,
                        data = DataLong_Subs_sim_group1b
)

agg.simple <- aggte(example_attgt, type = "simple")
summary(agg.simple) #effect:  2.426, error: 0.4647, [95%  Conf. Int.]: 1.5152      3.3368 *

#2.1.1.2.2.Dynamic Effects
agg.es <- aggte(example_attgt, type = "dynamic")
summary(agg.es) #effect: 2.888, error: 0.5903, [95%  Conf. Int.]: 1.731      4.0449 *
ggdid(agg.es) 

jpeg("Figures/FigureAppendix_A61_a.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "a) Average effect by length of exposure for the entire company - Group IT") + 
  theme(legend.position="right") +
  ylab("Estimated effect on Relatedness") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","purple")) + #royalblue1 #dodgerblue2  #green3 #springgreen3
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
dev.off()

#2.1.1.3.Group 3
#2.1.1.3.1.Simple effects
example_attgt <- att_gt(yname = "Relatedness_Cos2", #the relatedness extended;
                        tname = "CurrentYear",
                        idname = "id",
                        gname = "first.treat",
                        xformla = ~1,
                        data = DataLong_Subs_sim_group3b
)

agg.simple <- aggte(example_attgt, type = "simple")
summary(agg.simple) #effect:  0.6218, error: 0.3102, [95%  Conf. Int.]: 0.0137      1.2299 *

#2.1.1.3.2.Dynamic Effects and Event Studies Group 3
agg.es <- aggte(example_attgt, type = "dynamic")
summary(agg.es) #effect: 0.5714, error: 0.4656, [95%  Conf. Int.]: -0.3413       1.484
ggdid(agg.es) 

jpeg("Figures/FigureAppendix_A61_c.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "c) Average effect by length of exposure for the entire company - Group less used Nace codes") + 
  theme(legend.position="right") +
  ylab("Estimated effect on Relatedness") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","purple")) + #royalblue1 #dodgerblue2  #green3 #springgreen3
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
dev.off()


#2.2. By calculation of a distance measure-----
#2.2.1.Calculate the average distance of sectors ------
#In the average distance of every NACE code to the ‘AI cluster’ is measured, and quartiles are used to create three 
#distinct categories (see for a detailed description in Appendix A5)
rm(list=ls())
SquareMatrixSectors<- read.csv("Data/SquareMatrixSectors.csv", sep = ";", header = T, dec=",")
SquareMatrixSectors <- SquareMatrixSectors[,c(-length(SquareMatrixSectors))]
#replace NA values by 0
SquareMatrixSectors[is.na(SquareMatrixSectors)] <- 0

#from https://www.r-bloggers.com/2021/07/how-to-calculate-mean-absolute-error-in-r/
library(Metrics)
predicted <- SquareMatrixSectors[SquareMatrixSectors$X=="AI",]
predicted<- predicted[,(-1)]
predicted<-as.vector(predicted,mode='numeric')

observed1 <- SquareMatrixSectors[SquareMatrixSectors$X=="6201",]
observed1<- observed1[,(-1)]
observed1<-as.vector(observed1,mode='numeric')

mae(as.vector(SquareMatrixSectors[484,][,(-1)],mode='numeric'),predicted) #12.35701
mae(observed1, predicted) #12.35701

AverageDistance <- as.data.frame("Sector", col.names = "a")

for (i in 1:length(SquareMatrixSectors$X)){
  
  AverageDistance[i] <- mae(as.vector(SquareMatrixSectors[i,][,(-1)],mode='numeric'),predicted)
}

AverageDistance2 <- as.data.frame(t(AverageDistance))
Sectors <- as.data.frame(SquareMatrixSectors[,1])
AverageDistance2 <- cbind(AverageDistance2,Sectors)
names(AverageDistance2) <- c("AverageDistance", "Sector")
write.csv2(AverageDistance2, file = "Data/Distance_categories.csv", row.names = F)

rm(list=ls())
Distance_categories <- read.csv("Data/Distance_categories.csv", sep = ";", header = TRUE, dec=",")
Distance_categories <- Distance_categories[complete.cases(Distance_categories$AverageDistance), ]

Distance_categories$Quartile <- ifelse(Distance_categories$AverageDistance>=quantile(Distance_categories$AverageDistance)[[4]], "bottom",
                           ifelse(Distance_categories$AverageDistance<quantile(Distance_categories$AverageDistance)[[4]] & 
                                    Distance_categories$AverageDistance>quantile(Distance_categories$AverageDistance)[[2]], "medium", "top"))

Distance_categories_test <- Distance_categories[Distance_categories$Sector == "6201"|
                          Distance_categories$Sector == "5829"|
                          Distance_categories$Sector == "6311"|
                          Distance_categories$Sector == "6312"|
                          Distance_categories$Sector == "6391"|
                          Distance_categories$Sector == "6399",]

table(Distance_categories$Quartile)
write.csv2(Distance_categories, file = "Data/Distance_categories2.csv", row.names = F)

#2.2.2.Apply the calculated distances to separate sectors -----
rm(list=ls())
DataLong_Subs_sim <- read.csv("Data/Data_matched_MNEs.csv", sep = ";", header = TRUE, dec=",")
table(DataLong_Subs_sim$Nace_4d)

CategoriesNace <- read.csv("Data/Distance_categories2.csv", sep = ";", header = TRUE, dec=",")
names(CategoriesNace) <- c("AvgDistance", "Nace_4d", "Quartile")

DataLong_Subs_sim$Nace_4d <- as.character(DataLong_Subs_sim$Nace_4d)

DataLong_Subs_sim <- left_join(DataLong_Subs_sim,CategoriesNace, by = "Nace_4d")
table(is.na(DataLong_Subs_sim$Quartile)) #95074  1946  T
table(DataLong_Subs_sim$Quartile)

DataLong_Subs_sim$Quartile[is.na(DataLong_Subs_sim$Quartile)] <- "bottom"
table(is.na(DataLong_Subs_sim$Quartile))
table(DataLong_Subs_sim$Quartile)

DataLong_Subs_sim_group1 <- DataLong_Subs_sim[DataLong_Subs_sim$Quartile == "top",]
DataLong_Subs_sim_group2 <- DataLong_Subs_sim[DataLong_Subs_sim$Quartile == "medium",]
DataLong_Subs_sim_group3 <- DataLong_Subs_sim[DataLong_Subs_sim$Quartile == "bottom",]

TreatedUnits_group1 <- DataLong_Subs_sim_group1[DataLong_Subs_sim_group1$treat == 1,]
length(unique(TreatedUnits_group1$id)) #400
rm(TreatedUnits_group1)

TreatedUnits_group2 <- DataLong_Subs_sim_group2[DataLong_Subs_sim_group2$treat == 1,]
length(unique(TreatedUnits_group2$id)) #368
rm(TreatedUnits_group2)

TreatedUnits_group3 <- DataLong_Subs_sim_group3[DataLong_Subs_sim_group3$treat == 1,]
length(unique(TreatedUnits_group3$id)) #387
rm(TreatedUnits_group3)

#Simple Aggregation - Group 1
example_attgt <- att_gt(yname = "Relatedness_Cos2", #the complexity extended;
                        tname = "CurrentYear",
                        idname = "id",
                        gname = "first.treat",
                        xformla = ~1,
                        data = DataLong_Subs_sim_group1
)

agg.simple <- aggte(example_attgt, type = "simple")
summary(agg.simple) #effect:  1.9896, error: 0.2968, [95%  Conf. Int.]: 1.4078      2.5713 *


#Dynamic Effects and Event Studies Group 1
agg.es <- aggte(example_attgt, type = "dynamic")
summary(agg.es) #effect 2.4126, error: 0.4458, [95%  Conf. Int.]: 1.5389      3.2863 *
ggdid(agg.es) 

quantile(CategoriesNace$AvgDistance) 
#0%      25%      50%      75%     100% 
#0.00000 15.78271 16.58037 18.40327 22.97757

jpeg("Figures/FigureAppendix_A62_a.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "a) Average effect by length of exposure for the entire company - Closer quantile") + 
  theme(legend.position="right") +
  ylab("Estimated effect on Relatedness") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","purple")) + #royalblue1 #dodgerblue2  #green3 #springgreen3
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
dev.off()

#Simple Aggregation - Group 2
example_attgt <- att_gt(yname = "Relatedness_Cos2", #the complexity extended;
                        tname = "CurrentYear",
                        idname = "id",
                        gname = "first.treat",
                        xformla = ~1,
                        data = DataLong_Subs_sim_group2
)

agg.simple <- aggte(example_attgt, type = "simple")
summary(agg.simple) #effect:  0.2494, error: 0.3022, [95%  Conf. Int.]: -0.3429      0.8417


#Dynamic Effects and Event Studies Group 2
agg.es <- aggte(example_attgt, type = "dynamic")
summary(agg.es) #effect -0.0162, error: 0.4394, [95%  Conf. Int.]: -0.8775      0.8451 
ggdid(agg.es) 

jpeg("Figures/FigureAppendix_A62_b.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "b) Average effect by length of exposure for the entire company - Medium quantile") + 
  theme(legend.position="right") +
  ylab("Estimated effect on Relatedness") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","purple")) + #royalblue1 #dodgerblue2  #green3 #springgreen3
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
dev.off()

#Simple Aggregation - Group 3
example_attgt <- att_gt(yname = "Relatedness_Cos2", #the complexity extended;
                        tname = "CurrentYear",
                        idname = "id",
                        gname = "first.treat",
                        xformla = ~1,
                        data = DataLong_Subs_sim_group3
)

agg.simple <- aggte(example_attgt, type = "simple")
summary(agg.simple) #effect:  0.7766, error: 0.2433, [95%  Conf. Int.]: 0.2997      1.2535 *

#Dynamic Effects and Event Studies - Group 3
agg.es <- aggte(example_attgt, type = "dynamic")
summary(agg.es) #effect 0.8564, error: 0.3782, [95%  Conf. Int.]: 0.1152      1.5977 *
ggdid(agg.es) 

jpeg("Figures/FigureAppendix_A62_c.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "c) Average effect by length of exposure for the entire company - Bottom quantile") + 
  theme(legend.position="right") +
  ylab("Estimated effect on Relatedness") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","purple")) + #royalblue1 #dodgerblue2  #green3 #springgreen3
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
dev.off()

#3. Calculate effects on No. of patents owned by the MNE------
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
DataLong_Subs_2006 <- read.csv("Data/DataLong_Subs_2006.csv", sep = ";", header = TRUE, dec=",")

#double check descriptives
Descript_data<-DataLong_Subs_2006[DataLong_Subs_2006$CurrentYear==2019,]
table(Descript_data$Size_class)
#Large company Medium sized company        Small company   Very large company 
#870                  684                  522                 4854 
rm(Descript_data)

#check number of treated units:
Units <- DataLong_Subs_2006[DataLong_Subs_2006$treat == 1,]
length(unique(Units$id)) #1155
rm(Units)

#3.1.Simple Aggregation
example_attgt <- att_gt(yname = "NoPatentsYearGUOtotal", 
                        tname = "CurrentYear",
                        idname = "id",
                        gname = "first.treat",
                        xformla = ~1, 
                        data = DataLong_Subs_2006
)

agg.simple <- aggte(example_attgt, type = "simple")
summary(agg.simple) #effect 25.0168, error:  4.9943, [95%  Conf. Int.]: 15.2281     34.8054 *


#3.2.Dynamic Effects and Event Studies
agg.es <- aggte(example_attgt, type = "dynamic")
summary(agg.es) #effect 35.4822, error: 7.0511, [95%  Conf. Int.]:   21.6622     49.3022 *
ggdid(agg.es) 

jpeg("Figures/Figure2a.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "a) Average effect by length of exposure for the MNE - All companies") + 
  theme(legend.position="right") +
  ylab("Number of Patents owned by the MNE") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","#3399FF")) + #3399FF
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
dev.off()

#calculate the mean NoPatentsYearGUOtotal of treated (1) versus non treated (0)
DataLong_Subs_sim3 <- data.table(DataLong_Subs_2006)
DataLong_Subs_sim3$treat <- as.numeric(ifelse(DataLong_Subs_sim3$first.treat==0,0,1))
DataLong_Subs_sim3[first.treat == 0 | first.treat != 0 , mean(NoPatentsYearGUOtotal), by = treat ] #
#62.48194 for non-treated (0) and 108.51725 for treated (1)
#considering the effects seen (25.0168 for simple and 35.4822 for dynamic):
25.0168/62.48194 #0.4003845 for simple (regarding the turnover, the increase was: 0.1442532 increase on average for the simple)
35.4822/62.48194 #0.5678793 for dynamic(regarding the turnover, the increase was: 0.2509677 for dynamic)
rm(DataLong_Subs_sim3)

##3.3.Calculate the effects linked to the companies for which there is Turnover information
DataLong_Subs <- read.csv("Data/Data_matched_MNEs.csv", sep = ";", header = TRUE, dec=",")
DataLong_Subs <- DataLong_Subs[DataLong_Subs$CurrentYear>2010,]

DataLong_Subs_sim <- DataLong_Subs
DataLong_Subs_sim_Turnover <- DataLong_Subs_sim
#fill missing data with the data from the previous year;
DataLong_Subs_sim %<>% group_by(id) %>% 
  mutate(Turnover_extended = na.locf0(Turnover)) %>% 
  mutate(RAndD_Expenses_extended = na.locf0(RAndD_Expenses)) %>% 
  ungroup
DataLong_Subs_sim %<>% group_by(id) %>% 
  mutate(Turnover_extended = na.locf0(Turnover_extended, fromLast = TRUE)) %>% 
  mutate(RAndD_Expenses_extended = na.locf0(RAndD_Expenses_extended, fromLast = TRUE)) %>% 
  ungroup

table(is.na(DataLong_Subs_sim$Turnover_extended)) #6156 T out of 56214  F
table(is.na(DataLong_Subs_sim$RAndD_Expenses_extended)) #29268  T out of 33102  F

DataLong_Subs_sim_Turnover <- DataLong_Subs_sim[is.na(DataLong_Subs_sim$Turnover_extended)==F,]
DataLong_Subs_sim_RAndD_Expenses <- DataLong_Subs_sim[is.na(DataLong_Subs_sim$RAndD_Expenses_extended)==F,]

#descriptives for Turnover:
Descript_data<-DataLong_Subs_sim_Turnover[DataLong_Subs_sim_Turnover$CurrentYear==2019,]
table(Descript_data$Size_class)
#Large company Medium sized company        Small company   Very large company 
#724                  496                  263                 4763 
rm(Descript_data)

#descriptives for RAndD_Expenses:
Descript_data<-DataLong_Subs_sim_RAndD_Expenses[DataLong_Subs_sim_RAndD_Expenses$CurrentYear==2019,]
table(Descript_data$Size_class)
#Large company Medium sized company   Very large company 
#67                   18                 3593 
rm(Descript_data)

#calculate the mean Turnover_extended of treated (1) versus non treated (0)
DataLong_Subs_sim3 <- data.table(DataLong_Subs_sim_Turnover)
DataLong_Subs_sim3$treat <- as.numeric(ifelse(DataLong_Subs_sim3$first.treat==0,0,1))
DataLong_Subs_sim3[first.treat == 0 | first.treat != 0 , mean(Turnover_extended), by = treat ] #
#868362.8 for 0 and 2265071.0 for 1
rm(DataLong_Subs_sim3)

DataLong_Subs_sim_Turnover <- DataLong_Subs_sim#[is.na(DataLong_Subs_sim$Turnover_extended)==F,]
table(DataLong_Subs_sim$CurrentYear)

DataLong_Subs_sim_Turnover %<>% 
  group_by(id) %>% 
  mutate(Number_NAs = sum(is.na(Turnover)==T)) %>% 
  ungroup()
table(DataLong_Subs_sim_Turnover$Number_NAs)
(3717+2844+3033+2187+2295+1719+1764+5553+6156) #29268
29268/(29268+33102)
DataLong_Subs_sim_Turnover<- DataLong_Subs_sim_Turnover[DataLong_Subs_sim_Turnover$Number_NAs < 3,] 

#fill missing data with the relatedness from the previous year;
DataLong_Subs_sim_Turnover %<>% group_by(id) %>% 
  mutate(Turnover_extended = na.locf0(Turnover)) %>% 
  mutate(RAndD_Expenses_extended = na.locf0(RAndD_Expenses)) %>% 
  ungroup

DataLong_Subs_sim_Turnover %<>% group_by(id) %>% 
  mutate(Turnover_extended = na.locf0(Turnover_extended, fromLast = TRUE)) %>%  # changed from Turnover to Turnover_extended
  mutate(RAndD_Expenses_extended = na.locf0(RAndD_Expenses_extended, fromLast = TRUE)) %>% 
  ungroup


length(unique(DataLong_Subs_sim_Turnover$id)) #4407
DataLong_Subs_sim_Turnover$LogTurnoverExtended <- log10(DataLong_Subs_sim_Turnover$Turnover_extended)
length(unique(DataLong_Subs_sim_Turnover$id)) #4407

#exclude problematic values:
#Infinte values
Exclude1<-DataLong_Subs_sim_Turnover[DataLong_Subs_sim_Turnover$LogTurnoverExtended == Inf,] #37 observations

'%notin%' <- Negate('%in%')
DataLong_Subs_sim_Turnover<-DataLong_Subs_sim_Turnover[DataLong_Subs_sim_Turnover$id %notin% Exclude1$id,] 
rm(Exclude1)

#-Infinte
Exclude2<-DataLong_Subs_sim_Turnover[DataLong_Subs_sim_Turnover$LogTurnoverExtended == -Inf,] #569 observations
DataLong_Subs_sim_Turnover<-DataLong_Subs_sim_Turnover[DataLong_Subs_sim_Turnover$id %notin% Exclude2$id,] #
rm(Exclude2)

#NaN
Exclude2<-DataLong_Subs_sim_Turnover[is.nan(DataLong_Subs_sim_Turnover$LogTurnoverExtended) == T,] #20 observations
DataLong_Subs_sim_Turnover<-DataLong_Subs_sim_Turnover[DataLong_Subs_sim_Turnover$id %notin% Exclude2$id,] #
rm(Exclude2)

#NA
Exclude3<-DataLong_Subs_sim_Turnover[is.na(DataLong_Subs_sim_Turnover$LogTurnoverExtended) == T,] #0
DataLong_Subs_sim_Turnover<-DataLong_Subs_sim_Turnover[DataLong_Subs_sim_Turnover$id %notin% Exclude3$id,] #nothing changed
rm(Exclude3)

#double check descriptives
Descript_data<-DataLong_Subs_sim_Turnover[DataLong_Subs_sim_Turnover$CurrentYear==2019,]
table(Descript_data$Size_class)
#Large company Medium sized company        Small company   Very large company 
#324                  162                   42                 3731 
rm(Descript_data)
3731/(324+162+42+3731) #0.8760272

#check number of treated units:
Units <- DataLong_Subs_sim_Turnover[DataLong_Subs_sim_Turnover$treat == 1,]
length(unique(Units$id)) #690
rm(Units)

TurnoverCompanies<-DataLong_Subs_2006[DataLong_Subs_2006$id %in% DataLong_Subs_sim_Turnover$id,]
length(unique(TurnoverCompanies$id)) #4259
length(unique(DataLong_Subs_sim_Turnover$id)) #4259
length(unique(DataLong_Subs_2006$id)) #6930

#3.3.1.Simple Aggregation
example_attgt <- att_gt(yname = "NoPatentsYearGUOtotal", 
                        tname = "CurrentYear",
                        idname = "id",
                        gname = "first.treat",
                        xformla = ~1, 
                        data = TurnoverCompanies
)

agg.simple <- aggte(example_attgt, type = "simple")
summary(agg.simple) #effect 32.1339, error:  6.5797, [95%  Conf. Int.]: 19.238     45.0298 *


#3.3.2.Dynamic Effects and Event Studies
agg.es <- aggte(example_attgt, type = "dynamic")
summary(agg.es) #effect 43.9521, error: 9.6614, [95%  Conf. Int.]:  25.016     62.8881 *
ggdid(agg.es) 

jpeg("Figures/Figure2b.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "b) Average effect by length of exposure for the MNE - Companies with Turnover data available") + 
  theme(legend.position="right") +
  ylab("Number of Patents owned by the MNE") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","#0000FF")) + #3399FF
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
dev.off()

#3.3.3.Exclude holding companies
#effect for MNE without holding-related sectors (6420 and 7010)
table(TurnoverCompanies$CategoryNace) #1372 from 6420 and 826 from 7010;
Data_Long_filtered <- TurnoverCompanies[TurnoverCompanies$CategoryNace != 6420,]
Data_Long_filtered <- Data_Long_filtered[Data_Long_filtered$CategoryNace != 7010,]

table(Data_Long_filtered$CategoryNace) #0 from 6420 and 0 from 7010;

TreatedUnits <- Data_Long_filtered[Data_Long_filtered$treat == 1,]
length(unique(TreatedUnits$id)) #669
table(TreatedUnits$CategoryNace) #no 6420 nor 7010

TreatedUnits <- TurnoverCompanies[TurnoverCompanies$treat == 1,]
length(unique(TreatedUnits$id)) #690
table(TreatedUnits$CategoryNace)
rm(TreatedUnits)

#3.3.3.1.Simple Aggregation - robustness without holdings
example_attgt <- att_gt(yname = "NoPatentsYearGUOtotal", 
                        tname = "CurrentYear",
                        idname = "id",
                        gname = "first.treat",
                        xformla = ~1,
                        data = Data_Long_filtered
)

agg.simple <- aggte(example_attgt, type = "simple")
summary(agg.simple) #effect 32.7225, error:  7.3124, [95%  Conf. Int.]: 18.3905     47.0545 * #here there is a mistake in the paper

#3.3.3.2.Dynamic Effects and Event Studies
agg.es <- aggte(example_attgt, type = "dynamic")
summary(agg.es) #effect 44.8831, error: 9.3096, [95%  Conf. Int.]:  26.6366     63.1296 * #here there is another mistake in the paper
ggdid(agg.es) 

jpeg("Figures/Figure2c.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "c) Average effect by length of exposure for the MNE - no holding companies") + 
  theme(legend.position="right") +
  ylab("Number of Patents owned by the MNE") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","orange")) + #3399FF
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
dev.off()

#calculate the mean NoPatentsYearGUOtotal of treated (1) versus non treated (0)
DataLong_Subs_sim3 <- data.table(TurnoverCompanies)
DataLong_Subs_sim3$treat <- as.numeric(ifelse(DataLong_Subs_sim3$first.treat==0,0,1))
DataLong_Subs_sim3[first.treat == 0 | first.treat != 0 , mean(NoPatentsYearGUOtotal), by = treat ] #
#89.89707 for non-treated (0) and 162.58126 for treated (1)
#considering the effects seen (32.7225 for simple and 44.8831 for dynamic):
32.1339/89.89707 #0.3574521 for simple (regarding the turnover, the increase was: 0.1442532 increase on average for the simple)
43.9521/89.89707 #0.4889158 for dynamic(regarding the turnover, the increase was: 0.2509677 for dynamic)
rm(DataLong_Subs_sim3)

#calculate the mean NoPatentsYearGUOtotal of treated (1) versus non treated (0) excluding holdings:
DataLong_Subs_sim3 <- data.table(Data_Long_filtered)
DataLong_Subs_sim3$treat <- as.numeric(ifelse(DataLong_Subs_sim3$first.treat==0,0,1))
DataLong_Subs_sim3[first.treat == 0 | first.treat != 0 , mean(NoPatentsYearGUOtotal), by = treat ] #
#92.03545 for non-treated (0) and 165.69144 for treated (1)
#considering the effects seen (31.5452 for simple and 44.7753 for dynamic):
32.7225/92.03545 #0.3555423 for simple 
44.8831/92.03545 #0.4876719 for dynamic
rm(DataLong_Subs_sim3)

#4.number of patents generated per unit of Turnover ----
#double check descriptives for Turnover:
Descript_data<-DataLong_Subs_sim_Turnover[DataLong_Subs_sim_Turnover$CurrentYear==2019,]
table(Descript_data$Size_class)
#Large company Medium sized company        Small company   Very large company 
#324                  162                   42                 3731 
rm(Descript_data)

#check number of treated units:
Units <- DataLong_Subs_sim_RAndD_Expenses[DataLong_Subs_sim_RAndD_Expenses$treat == 1,]
length(unique(Units$id)) #636
rm(Units)

DataLong_Subs_sim_Turnover$PatentsPerUnitTurnover <- DataLong_Subs_sim_Turnover$NoPatentsYearGUOtotal/DataLong_Subs_sim_Turnover$Turnover_extended

length(unique(DataLong_Subs_sim_Turnover$id)) #4259
#exclude problematic values:
#Infinte values
Exclude1<-DataLong_Subs_sim_Turnover[DataLong_Subs_sim_Turnover$PatentsPerUnitTurnover == Inf,] #0 observations

'%notin%' <- Negate('%in%')
DataLong_Subs_sim_Turnover<-DataLong_Subs_sim_Turnover[DataLong_Subs_sim_Turnover$id %notin% Exclude1$id,] #nothing changed
rm(Exclude1)

#-Infinte
Exclude2<-DataLong_Subs_sim_Turnover[DataLong_Subs_sim_Turnover$PatentsPerUnitTurnover == -Inf,] #0 observations
DataLong_Subs_sim_Turnover<-DataLong_Subs_sim_Turnover[DataLong_Subs_sim_Turnover$id %notin% Exclude2$id,] #
rm(Exclude2)

#NaN
Exclude2<-DataLong_Subs_sim_Turnover[is.nan(DataLong_Subs_sim_Turnover$PatentsPerUnitTurnover) == T,] #0 observations
DataLong_Subs_sim_Turnover<-DataLong_Subs_sim_Turnover[DataLong_Subs_sim_Turnover$id %notin% Exclude2$id,] #nothing happened
rm(Exclude2)

#NA
Exclude3<-DataLong_Subs_sim_Turnover[is.na(DataLong_Subs_sim_Turnover$PatentsPerUnitTurnover) == T,] #0
DataLong_Subs_sim_Turnover<-DataLong_Subs_sim_Turnover[DataLong_Subs_sim_Turnover$id %notin% Exclude3$id,] #nothing changed
rm(Exclude3)

#4.1.Simple Aggregation
example_attgt <- att_gt(yname = "PatentsPerUnitTurnover", 
                        tname = "CurrentYear",
                        idname = "id",
                        gname = "first.treat",
                        xformla = ~1, 
                        data = DataLong_Subs_sim_Turnover
)
#Dropped 50 units that were already treated in the first period.
agg.simple <- aggte(example_attgt, type = "simple")
summary(agg.simple) #effect -0.007, error:  0.0144, [95%  Conf. Int.]: -0.0353      0.0212


#4.2.Dynamic Effects and Event Studies
agg.es <- aggte(example_attgt, type = "dynamic")
summary(agg.es) #effect -0.0127, error: 0.0145 , [95%  Conf. Int.]:  -0.0411      0.0157 
ggdid(agg.es) 

jpeg("Figures/Figure3a.jpg", width = 10, height = 3, units = 'in', res = 400)
ggdid(agg.es, title = "a) Average effect by length of exposure for the MNE") + 
  theme(legend.position="right") +
  ylab("No Patents per unit turnover") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","#33CC00")) + #66CC33 339900
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
dev.off()

#calculate the mean Turnover_extended of treated (1) versus non treated (0)
DataLong_Subs_sim3 <- data.table(DataLong_Subs_sim_Turnover)
DataLong_Subs_sim3$treat <- as.numeric(ifelse(DataLong_Subs_sim3$first.treat==0,0,1))
DataLong_Subs_sim3[first.treat == 0 | first.treat != 0 , mean(PatentsPerUnitTurnover), by = treat ] #
#0.00839168 for non-treated (0) and 0.00984794 for treated (1)
#considering the effects seen (-0.007 for simple and -0.0127 for dynamic):
0.007/0.00839168 #0.8341595 decrease on average for the simple
0.0127/0.00839168 #1.513404 for dynamic
rm(DataLong_Subs_sim3)

#calculate the mean Turnover_extended of treated (1) versus non treated (0)
DataLong_Subs_sim3 <- data.table(DataLong_Subs_sim_Turnover)
DataLong_Subs_sim3$treat <- as.numeric(ifelse(DataLong_Subs_sim3$first.treat==0,0,1))
DataLong_Subs_sim3[first.treat == 0 | first.treat != 0 , mean(Turnover_extended), by = treat ] 
#1,136,101 for non-treated and 3,235,768 for treated;
1136101+1136101*1.9 #roughly 190% increase in turnover;

#4.3.now excluding holding companies:
#effect for MNE without holding-related sectors (6420 and 7010)
table(DataLong_Subs_sim_Turnover$CategoryNace) #882 from 6420 and 531 from 7010;
Data_Long_filtered <- DataLong_Subs_sim_Turnover[DataLong_Subs_sim_Turnover$CategoryNace != 6420,]
Data_Long_filtered <- Data_Long_filtered[Data_Long_filtered$CategoryNace != 7010,]

table(Data_Long_filtered$CategoryNace) #0 from 6420 and 0 from 7010;

TreatedUnits <- Data_Long_filtered[Data_Long_filtered$treat == 1,]
length(unique(TreatedUnits$id)) #669
table(TreatedUnits$CategoryNace) #no 6420 nor 7010

TreatedUnits <- DataLong_Subs_sim_Turnover[DataLong_Subs_sim_Turnover$treat == 1,]
length(unique(TreatedUnits$id)) #690, so 21 treated units are lost by excluding both codes, 99 lines of them are from code 6420, 90 lines from 7010
table(TreatedUnits$CategoryNace)
rm(TreatedUnits)

#4.3.1.Simple Aggregation - robustness without holdings
example_attgt <- att_gt(yname = "PatentsPerUnitTurnover", 
                        tname = "CurrentYear",
                        idname = "id",
                        gname = "first.treat",
                        xformla = ~1, 
                        data = Data_Long_filtered
)
#Dropped 49 units that were already treated in the first period.
agg.simple <- aggte(example_attgt, type = "simple")
summary(agg.simple) #effect -0.0132, error:  0.0143, [95%  Conf. Int.]: -0.0412      0.0148

#4.3.2.Dynamic Effects and Event Studies
agg.es <- aggte(example_attgt, type = "dynamic")
summary(agg.es) #effect -0.0134, error: 0.012 , [95%  Conf. Int.]:  -0.0369      0.0102
ggdid(agg.es) 

jpeg("Figures/Figure3b.jpg", width = 10, height = 3, units = 'in', res = 400)
ggdid(agg.es, title = "b) Average effect by length of exposure for the MNE - no holdings") + 
  theme(legend.position="right") +
  ylab("No. Patents per unit turnover") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","orange")) + #tan3
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
dev.off()

#calculate the mean Turnover_extended of treated (1) versus non treated (0) excluding holdings
DataLong_Subs_sim3 <- data.table(Data_Long_filtered)
DataLong_Subs_sim3$treat <- as.numeric(ifelse(DataLong_Subs_sim3$first.treat==0,0,1))
DataLong_Subs_sim3[first.treat == 0 | first.treat != 0 , mean(PatentsPerUnitTurnover), by = treat ] #
#0.008304803 for non-treated (0) and 0.005139279 for treated (1)
#considering the effects seen (-0.007 for simple and -0.0127 for dynamic):
0.0132/0.008304803 #1.589442 decrease on average for the simple
0.0134/0.008304803 #1.613524 for dynamic
rm(DataLong_Subs_sim3)

#5.R&D expenses/n of patents -----
DataLong_Subs_sim_RAndD_Expenses <- DataLong_Subs_sim

DataLong_Subs_sim_RAndD_Expenses %<>% 
  group_by(id) %>% 
  mutate(Number_NAs = sum(is.na(RAndD_Expenses)==T)) %>% 
  ungroup()
table(DataLong_Subs_sim_RAndD_Expenses$Number_NAs)
DataLong_Subs_sim_RAndD_Expenses<- DataLong_Subs_sim_RAndD_Expenses[DataLong_Subs_sim_RAndD_Expenses$Number_NAs < 3,] 

#fill missing data with the relatedness from the previous year;
DataLong_Subs_sim_RAndD_Expenses %<>% group_by(id) %>% 
  mutate(Turnover_extended = na.locf0(Turnover)) %>% 
  mutate(RAndD_Expenses_extended = na.locf0(RAndD_Expenses)) %>% 
  ungroup

DataLong_Subs_sim_RAndD_Expenses %<>% group_by(id) %>% 
  mutate(Turnover_extended = na.locf0(Turnover, fromLast = TRUE)) %>% 
  mutate(RAndD_Expenses_extended = na.locf0(RAndD_Expenses_extended, fromLast = TRUE)) %>% 
  ungroup

#apply the formula
DataLong_Subs_sim_RAndD_Expenses$RandDExpensesTransformed <- (DataLong_Subs_sim_RAndD_Expenses$RAndD_Expenses_extended)*(DataLong_Subs_sim_RAndD_Expenses$Turnover_extended)
DataLong_Subs_sim_RAndD_Expenses$ShareRandDExpensesperNopatents <- (DataLong_Subs_sim_RAndD_Expenses$RandDExpensesTransformed)/(DataLong_Subs_sim_RAndD_Expenses$NoPatentsYearGUOtotal)

#now drop the complete company where the conditions below hold:
#Infinte values
Exclude1<-DataLong_Subs_sim_RAndD_Expenses[DataLong_Subs_sim_RAndD_Expenses$ShareRandDExpensesperNopatents == Inf,] #1025 lines

'%notin%' <- Negate('%in%')
DataLong_Subs_sim_RAndD_Expenses<-DataLong_Subs_sim_RAndD_Expenses[DataLong_Subs_sim_RAndD_Expenses$id %notin% Exclude1$id,] #
rm(Exclude1)

#-Infinte
Exclude2<-DataLong_Subs_sim_RAndD_Expenses[DataLong_Subs_sim_RAndD_Expenses$ShareRandDExpensesperNopatents == -Inf,] #581 lines
DataLong_Subs_sim_RAndD_Expenses<-DataLong_Subs_sim_RAndD_Expenses[DataLong_Subs_sim_RAndD_Expenses$id %notin% Exclude2$id,] #
rm(Exclude2)

#NaN
Exclude2<-DataLong_Subs_sim_RAndD_Expenses[is.nan(DataLong_Subs_sim_RAndD_Expenses$ShareRandDExpensesperNopatents) == T,] #547 lines
DataLong_Subs_sim_RAndD_Expenses<-DataLong_Subs_sim_RAndD_Expenses[DataLong_Subs_sim_RAndD_Expenses$id %notin% Exclude2$id,] #
rm(Exclude2)

#NA
Exclude3<-DataLong_Subs_sim_RAndD_Expenses[is.na(DataLong_Subs_sim_RAndD_Expenses$ShareRandDExpensesperNopatents) == T,] #30 lines
DataLong_Subs_sim_RAndD_Expenses<-DataLong_Subs_sim_RAndD_Expenses[DataLong_Subs_sim_RAndD_Expenses$id %notin% Exclude3$id,] #
rm(Exclude3)

#calculate the mean RAndD_Expenses of treated (1) versus non treated (0)
DataLong_test <- data.table(DataLong_Subs_sim_RAndD_Expenses)
DataLong_test$treat <- as.numeric(ifelse(DataLong_test$first.treat==0,0,1))
DataLong_test[first.treat == 0 | first.treat != 0 , mean(RAndD_Expenses_extended), by = treat ] #
#4.846589 for 0 and 6.701868 for 1

DataLong_test[first.treat == 0 | first.treat != 0 , mean(ShareRandDExpensesperNopatents), by = treat ] #
#37215.73 for 0 and 112120.46 for 1
rm(DataLong_test)

#double check descriptives for Turnover:
Descript_data<-DataLong_Subs_sim_RAndD_Expenses[DataLong_Subs_sim_RAndD_Expenses$CurrentYear==2019,]
table(Descript_data$Size_class)
#Large company Medium sized company   Very large company 
#10                    1                 2610 
rm(Descript_data)

#check number of treated units:
Units <- DataLong_Subs_sim_RAndD_Expenses[DataLong_Subs_sim_RAndD_Expenses$treat == 1,]
length(unique(Units$id)) #444
rm(Units)

#5.1.Simple Aggregation
example_attgt <- att_gt(yname = "ShareRandDExpensesperNopatents",
                        tname = "CurrentYear",
                        idname = "id",
                        gname = "first.treat",
                        xformla = ~1, 
                        data = DataLong_Subs_sim_RAndD_Expenses
)
#Dropped 38 units that were already treated in the first period.
agg.simple <- aggte(example_attgt, type = "simple")
summary(agg.simple) #effect -29865.73, error: 18008.61, [95%  Conf. Int.]: -65161.96    5430.502 

#5.2.Dynamic Effects and Event Studies
agg.es <- aggte(example_attgt, type = "dynamic")
summary(agg.es) #effect -27061.3, error:  18619.97 , [95%  Conf. Int.]:  -63555.77    9433.175 
ggdid(agg.es) 

jpeg("Figures/Figure3c.jpg", width = 10, height = 3, units = 'in', res = 400)
ggdid(agg.es, title = "c) Average effect by length of exposure for the MNE") + 
  theme(legend.position="right") +
  ylab("R&D expenses/No. Patents") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","#FF3333")) + #CC0033 #dodgerblue2  #green3 #springgreen3
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
dev.off()

#5.3.now excluding holding companies:
#New: effect for MNE without holding-related sectors (6420 and 7010)
table(DataLong_Subs_sim_RAndD_Expenses$CategoryNace) #72 from 6420 and 36 from 7010;
Data_Long_filtered <- DataLong_Subs_sim_RAndD_Expenses[DataLong_Subs_sim_RAndD_Expenses$CategoryNace != 6420,]
Data_Long_filtered <- Data_Long_filtered[Data_Long_filtered$CategoryNace != 7010,]

table(Data_Long_filtered$CategoryNace) #0 from 6420 and 0 from 7010;

TreatedUnits <- Data_Long_filtered[Data_Long_filtered$treat == 1,]
length(unique(TreatedUnits$id)) #442
table(TreatedUnits$CategoryNace) #no 6420 nor 7010

TreatedUnits <- DataLong_Subs_sim_RAndD_Expenses[DataLong_Subs_sim_RAndD_Expenses$treat == 1,]
length(unique(TreatedUnits$id)) #444, so 2 treated units are lost by excluding both codes, all of them are from code 6420 
table(TreatedUnits$CategoryNace)
rm(TreatedUnits)

#5.3.1.Simple Aggregation - robustness without holdings
#Simple Aggregation
example_attgt <- att_gt(yname = "ShareRandDExpensesperNopatents",
                        tname = "CurrentYear",
                        idname = "id",
                        gname = "first.treat",
                        xformla = ~1, 
                        data = Data_Long_filtered
)
#Dropped 38 units that were already treated in the first period.
agg.simple <- aggte(example_attgt, type = "simple")
summary(agg.simple) #effect -30110.03, error: 17407.32, [95%  Conf. Int.]: -64227.76    4007.693

#5.3.2.Dynamic Effects and Event Studies
agg.es <- aggte(example_attgt, type = "dynamic")
summary(agg.es) #effect -27347.35, error:  17481.74 , [95%  Conf. Int.]:  -61610.93    6916.232
ggdid(agg.es) 

jpeg("Figures/Figure3d.jpg", width = 10, height = 4, units = 'in', res = 400)
ggdid(agg.es, title = "d) Average effect by length of exposure for the MNE - no holding companies") + 
  theme(legend.position="right") +
  ylab("R&D expenses/No. Patents") + 
  xlab("Length of exposure") + 
  scale_color_manual(values=c("grey70","tan3")) + #CC0033 #dodgerblue2  #green3 #springgreen3
  labs(color   = "Treatment")+ 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1, alpha = 0.3)
dev.off()

#calculate the mean Turnover_extended of treated (1) versus non treated (0)
DataLong_Subs_sim3 <- data.table(DataLong_Subs_sim_RAndD_Expenses)
DataLong_Subs_sim3$treat <- as.numeric(ifelse(DataLong_Subs_sim3$first.treat==0,0,1))
DataLong_Subs_sim3[first.treat == 0 | first.treat != 0 , mean(ShareRandDExpensesperNopatents), by = treat ] #
#37215.73 for non-treated (0) and 112120.46 for treated (1)
#considering the effects seen (-29865.73 for simple and -27061.3 for dynamic):
29865.73/37215.73 #0.8025029 decrease on average for the simple
27061.3/37215.73 #0.7271468 decrease for dynamic
rm(DataLong_Subs_sim3)
37215.73 - 29865.73*37215.73  #-1111437728
(1-29865.73)*37215.73 #-1111437728
112120.46 + 29865.73*112120.46 #-3348671506

DataLong_Subs_sim3 <- data.table(Data_Long_filtered)
DataLong_Subs_sim3$treat <- as.numeric(ifelse(DataLong_Subs_sim3$first.treat==0,0,1))
DataLong_Subs_sim3[first.treat == 0 | first.treat != 0 , mean(ShareRandDExpensesperNopatents), by = treat ] #
#37271.02 for non-treated (0), 112576.00 for treated (1)

#considering the effects seen (-30110.03 for simple and -27347.35 for dynamic) for holdings excluded:
30110.03/37271.02 #0.8078671 decrease on average for the simple
27347.35/37271.02 #0.733743 decrease for dynamic

table(DataLong_Subs_sim_RAndD_Expenses$Group)
length(unique(DataLong_Subs_sim_RAndD_Expenses$id)) #2621

#to do ----
#explain the code a bit better;
#describe in the general file what the files are, check them if it is okay legally, explain ids can be repeated,
#maybe paste some figures, cite paper, etc.