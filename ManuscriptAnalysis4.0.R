# Created 8.02.2023
# Rochester Gradient Analysis
# Hayden Bock
#
#
#
# (1)	Does urbanization negatively impact soil animal communities?
# (2)	What are the most important site characteristics shaping soil 
#     animal community composition?
# (3)	Does urbanization homogenize soil animal communities?
# 
#
#
#
#
#

# Package management ------------------------------------------------------
# install.packages("econullnetr")#null model testing in R https://cran.r-project.org/web/packages/econullnetr/vignettes/econullnetr-intro.html
# install.packages("emmeans")
# install.packages("jtools")
# install.packages("corrr")
# install.packages("ggcorrplot")
# install.packages("FactoMineR")
# install.packages("factoextra")
# install_github("kassambara/factoextra")
# install.packages("mediation") #uncomment this command in case you haven't installed the mediation package yet.
# install.packages("spdep")
# install.packages("deldir")
# install.packages("rgdal")
# install.packages("rgeoda")
# install.packages("gvlma")
# install.packages("stargazer")
# install.packages("rockchalk")
# install.packages("ggpmisc")
# install.packages("sjPlot")
# install.packages("sjmisc")
# install.packages("sjlabelled")
# install.packages("randomForest")
# install.packages("datasets")
# install.packages("caret")
# install.packages("janitor")
# install.packages("rfviz")
# install.packages("randomForestExplainer")
#install.packages("ade4")
#install.packages("rfUtilities")


# Package Library -----
library(rfUtilities)
library(FactoMineR)
library(agricolae)
library(rpart)  
library(rpart.plot) 
library(randomForest)
library(datasets)
library(caret)
library(readxl)
library(janitor)
library(mediation)
library(econullnetr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(readr)
library(readxl)
library(vegan)
library(corrgram)
library(tidyr)
library(purrr)
library(emmeans)
library(jtools)
library(tidyselect)
library(corrr)
library(ggcorrplot)
library(FactoMineR)
library("devtools")
library(factoextra)
library(spdep)
library(deldir)
library(rgdal)
library(rgeoda)
library(gvlma)
library(stargazer)
library(rockchalk)
library(ggpubr)
library(ggpmisc)
library(car)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(lme4)
library(randomForestExplainer)
library(ade4)

#Load data ---------------------------------------------------------------
Rochester_Mesofauna_Density <- read_csv("Finalized_Manuscript_Analysis/data/ParkCommunityMeasureANDSpeciesDensity.csv") 
Environmental_Data <- read_excel("Finalized_Manuscript_Analysis/data/Environmental_Data.xlsx")
Concise_Enviro <- read_excel("Finalized_Manuscript_Analysis/data/Environmental_Data_Concise.xlsx")
NMDS_Data <- read_csv("Finalized_Manuscript_Analysis/data/NMDS_DATA.csv")
Div.Env <-read_csv("Finalized_Manuscript_Analysis/data/Clean_Env_and_Div.csv")
Concise_Div.Env <-read_csv("Finalized_Manuscript_Analysis/data/Concise.Div.Env.csv")
#weights <- read.gal("GeoDa_rookContiguity.gal", override.id=TRUE)
#neighbors <- read_csv("GeoDa.csv") %>% select(Park,Rook_connect,conect_5km,Area_connect,connect_1km)




#Answer Q1: Does urbanization negatively impact soil animal communities?----

#make a variable for tukey analysis
NMDS_Data$UrbanTimepoint <- paste(NMDS_Data$Urban_Kmeans_Cluster, NMDS_Data$Timepoint, sep="")

#Plot Abundance
Abundance_aov <- aov(`Abundance_per_kg`~UrbanTimepoint, data = NMDS_Data)
summary(Abundance_aov)
AbundanceHSD <- HSD.test(Abundance_aov,"UrbanTimepoint")

Abundance <- NMDS_Data %>% 
  ggplot(aes(x = PC1_Urban, y = Abundance_per_kg, color = Timepoint)) + 
  geom_point() +
  geom_smooth(method = "lm") + 
  scale_color_manual(values = c("orange", "darkgreen")) +
  theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "grey30"), 
        legend.position = "none")


#Plot Richness
Richness_aov <- aov(`Richness`~UrbanTimepoint, data = NMDS_Data)
summary(Richness_aov)
RichnessHSD <- HSD.test(Richness_aov,"UrbanTimepoint")


Richness <-NMDS_Data %>% 
  ggplot(aes(x = PC1_Urban, y = Richness, color = Timepoint)) + 
  geom_point() +
  geom_smooth(method = "lm") + 
  scale_color_manual(values = c("orange", "darkgreen")) +
  theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "grey30"), 
        legend.position = "none")


#Plot Shannon
Shannon_aov <- aov(`Shannon_WholePark`~UrbanTimepoint, data = NMDS_Data)
summary(Shannon_aov)
ShannonHSD <- HSD.test(Shannon_aov,"UrbanTimepoint")

Shannon <- NMDS_Data %>% 
  ggplot(aes(x = PC1_Urban, y = Shannon_WholePark, color = Timepoint)) + 
  geom_point() +
  geom_smooth(method = "lm") + 
  scale_color_manual(values = c("orange", "darkgreen")) +
  theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "grey30"), 
        legend.position = "none") 


#Plot BrayCurtis
BrayCurtis_aov <- aov(`BrayCurtis_BetweenPark`~UrbanTimepoint, data = NMDS_Data)
summary(BrayCurtis_aov)
BrayCurtisHSD <- HSD.test(BrayCurtis_aov,"UrbanTimepoint")


BrayCurtis <- NMDS_Data %>% 
  ggplot(aes(x = PC1_Urban, y = BrayCurtis_BetweenPark, color = Timepoint)) + 
  geom_point() +
  geom_smooth(method = "lm") + 
  scale_color_manual(values = c("orange", "darkgreen")) +
  theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "grey30"), 
        legend.position = "none")


NMDS_Data %>% group_by(Urban_Kmeans_Cluster) %>% mutate(urban = mean(PC1_Urban)) %>% select(urban) %>% print(n =150)


figure1 <- ggarrange(Abundance,
                     Richness,
                     Shannon,
                     BrayCurtis,
                     labels = c("A", "B", "C", "D"),
                     ncol = 2, nrow = 2)

ggsave(filename = "Finalized_Manuscript_Analysis/figureone_PCA.jpg", width = 8, height = 7.5, device='jpeg', dpi=400)



#Q1 is done!

#Q1 is done!

#Answer Q2:	What are the most important site characteristics shaping soil animal community composition?----

NMDS_Data[is.na(NMDS_Data)] <- 0

NMDS_Data <- NMDS_Data %>% rename(Distance_From_Geographic_Center = `Distance.From.CityCenter(km)`,
                                  Soil_WHC = Soil_Water_Holding_Capacity,
                                  Soil_N = Soil_Nitrogen_Percentage,
                                  Soil_C = Soil_Carbon_Percentage,
                                  Impervious_Surface = Imperviousness_in_500m_boundary,
                                  Population_Density = Population_Density_in_Surrounding_Area,
                                  Bulk_Density = BD_mean,
                                  Median_Household_Income = MedianIncome)






com = NMDS_Data[,31:153]
com[is.na(com)] <- 0

env <- NMDS_Data %>% select(Timepoint, 
                            -Municipality, 
                            -Year,
                            -Name,
                            Urban_Kmeans_Cluster, 
                             Median_Household_Income, 
                            `Park_Area(m2)`:Shannon_WholePark) %>% select(-PC1_Urban,
                                                                          -PC2_SoilSpatial, 
                                                                          -PC3_SoilResource,)


#convert com to a matrix
m_com = as.matrix(com)




nmds = metaMDS(com, distance = "bray", k = 3, trymax = 1000)
en = envfit(nmds, env, choices=c(1:3), permutations = 999, na.rm = TRUE)

en


plot(nmds)
plot(en)

#PerMANOVA on data
Community <- NMDS_Data[,31:153]

d.manova <- adonis2(Community ~ NMDS_Data$Urban_Kmeans_Cluster*NMDS_Data$Timepoint, method = "euclidean", data= NMDS_Data)
d.manova


library(pairwiseAdonis)
adonis.pw_urban <- pairwise.adonis(x = Community,
                                  factors= NMDS_Data$Urban_Kmeans_Cluster,
                                  sim.method='euclidean',
                                  p.adjust.m='holm')

adonis.pw_timepoint <- pairwise.adonis(x = Community,
                                   factors= NMDS_Data$Timepoint,
                                   sim.method='euclidean',
                                   p.adjust.m='holm')


adonis.pw_urban <- as.data.frame(adonis.pw_urban)
adonis.pw_urban$F.Model <- round(adonis.pw_urban$F.Model, 2)
adonis.pw_urban$R2 <- round(adonis.pw_urban$R2, 2)
adonis.pw_urban

adonis.pw_timepoint <- as.data.frame(adonis.pw_timepoint)
adonis.pw_timepoint$F.Model <- round(adonis.pw_timepoint$F.Model, 2)
adonis.pw_timepoint$R2 <- round(adonis.pw_timepoint$R2, 2)
adonis.pw_timepoint








#extract coordinates
data.scores = as.data.frame(scores(nmds)$sites)
data.scores$UrbanCluster = NMDS_Data$Urban_Kmeans_Cluster
data.scores$Timepoint = NMDS_Data$Timepoint
data.scores$Year = NMDS_Data$Year

#extract continous and categorical env 
en_coord_cont = as.data.frame(scores(en, "vectors")) * ordiArrowMul(en)
en_coord_cat = as.data.frame(scores(en, "factors")) * ordiArrowMul(en)

#subtract the non-significant vectors (Soil N, Soil Clay, Beta diversity)
#en_coord_cont <- en_coord_cont[-c(10), ]


#manually recode variables for color coding
en_coord_cont <- cbind(Variable = rownames(en_coord_cont), en_coord_cont)
rownames(en_coord_cont) <- 1:nrow(en_coord_cont)
#write_csv(en_coord_cont, "Finalized_Manuscript_Analysis/env.NMDS.csv")

#import the data again with correct coding
en_coord_cont <- read_csv("Finalized_Manuscript_Analysis/env.NMDS.csv")
en_coord_cont <-en_coord_cont %>% column_to_rownames(., var = "Variable")

library(stringr)
data.scores$Group <- str_c(data.scores$UrbanCluster, ' ', data.scores$Season)

vif(en_coord_cont)

#plot with ggplot
gg_1.2.dim = ggplot(data = data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(data = data.scores, aes(color = as.character(Group), shape = as.character(Timepoint)), size = 2, alpha = 0.5) + 
  scale_shape_manual(values=c(19, 15, 1, 0))+
  #vectors
  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2, colour = as.character(Variable_Class)), #vectors
               data = en_coord_cont, linewidth =1, alpha = 0.4) +
  scale_colour_manual(values = c("blue", "black", "forestgreen", "red", "orange", "purple"))+
  
  geom_text(data = en_coord_cont, aes(x = NMDS1, y = NMDS2+0.008, colour = as.character(Variable_Class)), size = 3, #vector label
            fontface = "bold", position=position_jitter(), label = row.names(en_coord_cont)) + 
  
  ylim(1,-1)+xlim(1.5,-1.5) + #axis limits
  
  theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "grey30"), 
        axis.ticks = element_blank(), axis.text = element_blank(), legend.key = element_blank(), 
        legend.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        legend.text = element_text(size = 9, colour = "grey30")) + 
  labs(colour = "Site Factor")

gg_1.2.dim
ggsave(filename = "Finalized_Manuscript_Analysis/nmds1.2.jpg", width = 10, height = 6, device='jpeg', dpi=400)







gg_2.3.dim = ggplot(data = data.scores, aes(x = NMDS2, y = NMDS3)) + 
  geom_point(data = data.scores, aes(color = as.character(Group), shape = as.character(Timepoint)), size = 2, alpha = 0.5) + 
  scale_shape_manual(values=c(19, 15, 1, 0))+
  #vectors
  geom_segment(aes(x = 0, y = 0, xend = NMDS2, yend = NMDS3, colour = as.character(Variable_Class)), #vectors
               data = en_coord_cont, linewidth =1, alpha = 0.4) +
  scale_colour_manual(values = c("blue", "black", "forestgreen", "red", "orange", "purple"))+
  
  geom_text(data = en_coord_cont, aes(x = NMDS2, y = NMDS3+0.008, colour = as.character(Variable_Class)), size = 3, #vector label
            fontface = "bold", position=position_jitter(), label = row.names(en_coord_cont)) + 
  
  ylim(1,-1)+xlim(1.5,-1.5) + #axis limits
  
  theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "grey30"), 
        axis.ticks = element_blank(), axis.text = element_blank(), legend.key = element_blank(), 
        legend.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        legend.text = element_text(size = 9, colour = "grey30")) + 
  labs(colour = "Site Factor")

gg_2.3.dim
ggsave(filename = "Finalized_Manuscript_Analysis/nmds2.3.jpg", width = 10, height = 6, device='jpeg', dpi=400)





  #Random Forest Approach for Abundance -----
# Make 2 RF's - one for highly urban, one for rural
#Lets start with random seed so the outcome will be repeatable and store train and test data.

set.seed(222)


#rename to readable/simple variables.
TempRF <- NMDS_Data %>% select(Park:Shannon_WholePark) %>% select(-Park,
                                                              -Year,
                                                              -Name, 
                                                              -Municipality,
                                                              -`PC1_Urban`,
                                                              -`PC2_SoilSpatial`,
                                                              -`PC3_SoilResource`)
TempRF <- TempRF %>% rename(Park_Area = `Park_Area(m2)`,
                            Perimeter_Area_Ratio = `Perimeter-Area_ratio`,
                            Shannon = Shannon_WholePark,
                            Abundance = Abundance_per_kg,
                            Bray_Curtis = BrayCurtis_BetweenPark)

Ab_RF <- TempRF %>% select(      -Bray_Curtis,
                                    -Shannon,
                                    -Richness,
                                 -`Distance.From.CityCenter(km)`) 


ind <- sample(2, nrow(Ab_RF), replace = TRUE, prob = c(0.7, 0.3))
train <- Ab_RF[ind==1,]
test <- Ab_RF[ind==2,]

train <- as.data.frame(train)

#create high and low urban DF

train_highurban <- train %>% filter(Urban_Kmeans_Cluster == "High_Urban")
test_highurban <- test %>% filter(Urban_Kmeans_Cluster == "High_Urban")

train_lowurban <- train %>% filter(Urban_Kmeans_Cluster == "Low_Urban")
test_lowurban <- test %>% filter(Urban_Kmeans_Cluster == "Low_Urban")


#Random Forest in R
AbundanceRF_highurban <- randomForest(Abundance~., data=train_highurban, ntree=5000, na.action=na.omit, importance=TRUE) 
AbundanceRF_lowurban <- randomForest(Abundance~., data=train_lowurban, ntree=5000, na.action=na.omit, importance=TRUE) 

print(AbundanceRF_highurban)
print(AbundanceRF_lowurban)

library(rfUtilities)
help(print.significance)
plot(AbundanceRF_highurban)

#Extract MSEincrease%
AbundanceImp_highurban<-importance(AbundanceRF_highurban)  #defaults scale=TRUE, standardizes  
AbundanceImp_highurban<-data.frame(AbundanceImp_highurban) #convert to df.

AbundanceImp_lowurban<-importance(AbundanceRF_lowurban)  #defaults scale=TRUE, standardizes  
AbundanceImp_lowurban<-data.frame(AbundanceImp_lowurban) #convert to df.


importance(AbundanceRF_lowurban, scale=FALSE) #This makes it match AbundanceRF$importance
importance(AbundanceRF_highurban, scale=FALSE)





  #Random Forest Approach for Richness -----
#Lets start with random seed so the outcome will be repeatable and store train and test data.

set.seed(222)

Rich_RF <- TempRF %>% select(      -Bray_Curtis,
                                 -Shannon,
                                 -Abundance) 

ind <- sample(2, nrow(Rich_RF), replace = TRUE, prob = c(0.7, 0.3))
train <- Rich_RF[ind==1,]
test <- Rich_RF[ind==2,]

train <- as.data.frame(train)

#create high and low urban DF

train_highurban <- train %>% filter(Urban_Kmeans_Cluster == "High_Urban")
test_highurban <- test %>% filter(Urban_Kmeans_Cluster == "High_Urban")

train_lowurban <- train %>% filter(Urban_Kmeans_Cluster == "Low_Urban")
test_lowurban <- test %>% filter(Urban_Kmeans_Cluster == "Low_Urban")

#Random Forest in R
RichnessRF_highurban <- randomForest(Richness~., data=train_highurban, ntree=5000, na.action=na.omit, importance=TRUE) 
RichnessRF_lowurban <- randomForest(Richness~., data=train_lowurban, ntree=5000, na.action=na.omit, importance=TRUE) 

print(RichnessRF_highurban)
print(RichnessRF_lowurban)


#Extract MSEincrease%
RichnessImp_highurban<-importance(RichnessRF_highurban)  #defaults scale=TRUE, standardizes  
RichnessImp_highurban<-data.frame(RichnessImp_highurban) #convert to df.

RichnessImp_lowurban<-importance(RichnessRF_lowurban)  #defaults scale=TRUE, standardizes  
RichnessImp_lowurban<-data.frame(RichnessImp_lowurban) #convert to df.



  #Random Forest Approach for shannon Diversity -----
#Lets start with random seed so the outcome will be repeatable and store train and test data.

set.seed(222)

Shannon_RF <- TempRF %>% select(      -Bray_Curtis,
                                   -Richness,
                                   -Abundance) 



ind <- sample(2, nrow(Shannon_RF), replace = TRUE, prob = c(0.7, 0.3))
train <- Shannon_RF[ind==1,]
test <- Shannon_RF[ind==2,]

train <- as.data.frame(train)

#create high and low urban DF

train_highurban <- train %>% filter(Urban_Kmeans_Cluster == "High_Urban")
test_highurban <- test %>% filter(Urban_Kmeans_Cluster == "High_Urban")

train_lowurban <- train %>% filter(Urban_Kmeans_Cluster == "Low_Urban")
test_lowurban <- test %>% filter(Urban_Kmeans_Cluster == "Low_Urban")



#Random Forest in R
ShannonRF_highurban <- randomForest(Shannon~., data=train_highurban, ntree=5000, na.action=na.omit, importance=TRUE) 
ShannonRF_lowurban <- randomForest(Shannon~., data=train_lowurban, ntree=5000, na.action=na.omit, importance=TRUE) 

print(ShannonRF_highurban)
print(ShannonRF_lowurban)


#Extract MSEincrease%
ShannonImp_highurban<-importance(ShannonRF_highurban)  #defaults scale=TRUE, standardizes  
ShannonImp_highurban<-data.frame(ShannonImp_highurban) #convert to df.

ShannonImp_lowurban<-importance(ShannonRF_lowurban)  #defaults scale=TRUE, standardizes  
ShannonImp_lowurban<-data.frame(ShannonImp_lowurban) #convert to df.




  #Random Forest Approach for Beta Diversity -----
#Lets start with random seed so the outcome will be repeatable and store train and test data.

set.seed(222)

BC_RF <- TempRF %>% select(      -Shannon,
                                      -Richness,
                                      -Abundance) 


ind <- sample(2, nrow(BC_RF), replace = TRUE, prob = c(0.7, 0.3))
train <- BC_RF[ind==1,]
test <- BC_RF[ind==2,]

train <- as.data.frame(train)

#create high and low urban DF

train_highurban <- train %>% filter(Urban_Kmeans_Cluster == "High_Urban")
test_highurban <- test %>% filter(Urban_Kmeans_Cluster == "High_Urban")

train_lowurban <- train %>% filter(Urban_Kmeans_Cluster == "Low_Urban")
test_lowurban <- test %>% filter(Urban_Kmeans_Cluster == "Low_Urban")


#Random Forest in R
BetaRF_highurban <- randomForest(`Bray_Curtis`~., data=train_highurban, ntree=5000, na.action=na.omit, importance=TRUE) 
BetaRF_lowurban <- randomForest(`Bray_Curtis`~., data=train_lowurban, ntree=5000, na.action=na.omit, importance=TRUE) 

print(BetaRF_highurban)
print(BetaRF_lowurban)


#Extract MSEincrease%
BetaImp_highurban<-importance(BetaRF_highurban)  #defaults scale=TRUE, standardizes  
BetaImp_highurban<-data.frame(BetaImp_highurban) #convert to df.

BetaImp_lowurban<-importance(BetaRF_lowurban)  #defaults scale=TRUE, standardizes  
BetaImp_lowurban<-data.frame(BetaImp_lowurban) #convert to df.




#### Combine [vaiable]Imp data frames and make a figure.


AbundanceImp_highurban <- AbundanceImp_highurban  %>% rename(Abundance_MseInc_highurban = X.IncMSE) %>% select(Abundance_MseInc_highurban)
AbundanceImp_highurban <- cbind(Variable = rownames(AbundanceImp_highurban), AbundanceImp_highurban)
rownames(AbundanceImp_highurban) <- 1:nrow(AbundanceImp_highurban)

AbundanceImp_lowurban <- AbundanceImp_lowurban  %>% rename(Abundance_MseInc_lowurban = X.IncMSE) %>% select(Abundance_MseInc_lowurban)
AbundanceImp_lowurban <- cbind(Variable = rownames(AbundanceImp_lowurban), AbundanceImp_lowurban)
rownames(AbundanceImp_lowurban) <- 1:nrow(AbundanceImp_lowurban)




RichnessImp_highurban <- RichnessImp_highurban %>% rename(Richness_MseInc_highurban = X.IncMSE) %>% select(Richness_MseInc_highurban)
RichnessImp_highurban <- cbind(Variable = rownames(RichnessImp_highurban), RichnessImp_highurban)
rownames(RichnessImp_highurban) <- 1:nrow(RichnessImp_highurban)

RichnessImp_lowurban <- RichnessImp_lowurban %>% rename(Richness_MseInc_lowurban = X.IncMSE) %>% select(Richness_MseInc_lowurban)
RichnessImp_lowurban <- cbind(Variable = rownames(RichnessImp_lowurban), RichnessImp_lowurban)
rownames(RichnessImp_lowurban) <- 1:nrow(RichnessImp_lowurban)



ShannonImp_highurban <- ShannonImp_highurban %>% rename(Shannon_MseInc_highurban = X.IncMSE) %>% select(Shannon_MseInc_highurban)
ShannonImp_highurban <- cbind(Variable = rownames(ShannonImp_highurban), ShannonImp_highurban)
rownames(ShannonImp_highurban) <- 1:nrow(ShannonImp_highurban)

ShannonImp_lowurban <- ShannonImp_lowurban %>% rename(Shannon_MseInc_lowurban = X.IncMSE) %>% select(Shannon_MseInc_lowurban)
ShannonImp_lowurban <- cbind(Variable = rownames(ShannonImp_lowurban), ShannonImp_lowurban)
rownames(ShannonImp_lowurban) <- 1:nrow(ShannonImp_lowurban)



BetaImp_highurban <- BetaImp_highurban %>% rename(Beta_MseInc_highurban = X.IncMSE) %>% select(Beta_MseInc_highurban)
BetaImp_highurban <- cbind(Variable = rownames(BetaImp_highurban), BetaImp_highurban)
rownames(BetaImp_highurban) <- 1:nrow(BetaImp_highurban)



BetaImp_lowurban <- BetaImp_lowurban %>% rename(Beta_MseInc_lowurban = X.IncMSE) %>% select(Beta_MseInc_lowurban)
BetaImp_lowurban <- cbind(Variable = rownames(BetaImp_lowurban), BetaImp_lowurban)
rownames(BetaImp_lowurban) <- 1:nrow(BetaImp_lowurban)



# join the various dataframes
Temp <- left_join(AbundanceImp_highurban, AbundanceImp_lowurban)
Temp2 <-left_join(Temp, RichnessImp_highurban) 
Temp3 <-left_join(Temp2, RichnessImp_lowurban) 
Temp4 <-left_join(Temp3, ShannonImp_highurban) 
Temp5 <-left_join(Temp4, ShannonImp_lowurban) 
Temp6<-left_join(Temp5, BetaImp_highurban) 
Temp7<-left_join(Temp6, BetaImp_lowurban) 


MSE_Imp_DF <- Temp7

#write_csv(MSE_Imp_DF, "Finalized_Manuscript_Analysis/MSE_Imp_DF.csv")



  #Q2 Plot ----
MSE_Imp_DF <- read_csv("Finalized_Manuscript_Analysis/MSE_Imp_DF.csv") #raw %MSE
#Standardized_MSE_RF <- read_csv("Finalized_Manuscript_Analysis/Standardized_MSE_RF.csv")#Standardized using Scale() base R function

#Standardized_MSE_RF <- Standardized_MSE_RF[1:17,]


Ahigh_MSE <- MSE_Imp_DF %>% select(Variable, Variable_Class, Abundance_MseInc_highurban)
Alow_MSE <- MSE_Imp_DF %>% select(Variable, Variable_Class, Abundance_MseInc_lowurban)


Rhigh_MSE <- MSE_Imp_DF %>% select(Variable, Variable_Class, Richness_MseInc_highurban)
Rlow_MSE <- MSE_Imp_DF %>% select(Variable, Variable_Class, Richness_MseInc_lowurban)


Shigh_MSE <- MSE_Imp_DF %>% select(Variable, Variable_Class, Shannon_MseInc_highurban)
Slow_MSE <- MSE_Imp_DF %>% select(Variable, Variable_Class, Shannon_MseInc_lowurban)


Bhigh_MSE <- MSE_Imp_DF %>% select(Variable, Variable_Class, Beta_MseInc_highurban)
Blow_MSE <- MSE_Imp_DF %>% select(Variable, Variable_Class, Beta_MseInc_lowurban)











#plot abundance
#A1
Abundance.high <- Ahigh_MSE %>% 
  ggplot(aes(y = reorder(Variable, Abundance_MseInc_highurban), x= Abundance_MseInc_highurban,  fill = Variable_Class)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("red", "orange", "purple", "black")) + 
  theme_bw() + theme(axis.title.x = element_blank(), 
                     axis.title.y = element_blank(),
                     legend.position = "none")+ xlim(-10, 45) + ggtitle("High-Urban Parks")
#A2
Abundance.low <- Alow_MSE %>% 
  ggplot(aes(y = reorder(Variable, Abundance_MseInc_lowurban), x= Abundance_MseInc_lowurban,  fill = Variable_Class)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("red", "orange", "purple", "black"))+ 
  theme_bw() + theme(axis.title.x = element_blank(), 
                     axis.title.y = element_blank(),
                     legend.position = "none")+ xlim(-10, 45) + ggtitle("Low-Urban Parks")
#plot richness
#B1
Richness.high <- Rhigh_MSE %>% 
  ggplot(aes(y = reorder(Variable, Richness_MseInc_highurban), x= Richness_MseInc_highurban,  fill = Variable_Class)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("red", "orange", "purple", "black"))+ 
  theme_bw() + theme(axis.title.x = element_blank(), 
                     axis.title.y = element_blank(),
                     legend.position = "none")+ xlim(-10, 45)
#B2
Richness.low <- Rlow_MSE %>% 
  ggplot(aes(y = reorder(Variable, Richness_MseInc_lowurban), x= Richness_MseInc_lowurban,  fill = Variable_Class)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("red", "orange", "purple", "black"))+ 
  theme_bw() + theme(axis.title.x = element_blank(), 
                     axis.title.y = element_blank(),
                     legend.position = "none")+ xlim(-10, 45)
#plot shannon
#C1
Shannon.high <- Shigh_MSE %>% 
  ggplot(aes(y = reorder(Variable, Shannon_MseInc_highurban), x= Shannon_MseInc_highurban,  fill = Variable_Class)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("red", "orange", "purple", "black"))+ 
  theme_bw() + theme(axis.title.x = element_blank(), 
                       axis.title.y = element_blank(),
                       legend.position = "none")+ xlim(-10, 45)
Shannon.low <- Slow_MSE %>% 
  ggplot(aes(y = reorder(Variable, Shannon_MseInc_lowurban), x= Shannon_MseInc_lowurban,  fill = Variable_Class)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("red", "orange", "purple", "black"))+ 
  theme_bw()+ theme(axis.title.x = element_blank(), 
                    axis.title.y = element_blank(),
                    legend.position = "none")+ xlim(-10, 45)
#plot Beta
#D1
Beta.high <- Bhigh_MSE %>% 
  ggplot(aes(y = reorder(Variable, Beta_MseInc_highurban), x= Beta_MseInc_highurban,  fill = Variable_Class)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("red", "orange", "purple", "black"))+ 
  theme_bw() + theme(axis.title.y = element_blank(), legend.position = "none")+ labs(x = "Relative MSE Increase")+ xlim(-10, 45)
#D2
Beta.low <- Blow_MSE %>% 
  ggplot(aes(y = reorder(Variable, Beta_MseInc_lowurban), x= Beta_MseInc_lowurban,  fill = Variable_Class)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("red", "orange", "purple", "black"))+ 
  theme_bw() + theme(axis.title.y = element_blank(), legend.position = "none")+ labs(x = "Relative MSE Increase")+ xlim(-10, 45)





figure2 <- ggarrange(Abundance.high,
                     Abundance.low,
                     Richness.high,
                     Richness.low,
                     Shannon.high,
                     Shannon.low,
                     Beta.high,
                     Beta.low,
                     labels = c("A1", "A2", "B1", "B2", "C1", "C2", "D1", "D2"),
                     ncol = 2, nrow = 4)

ggsave(filename = "Finalized_Manuscript_Analysis/figuretwo.vertical.jpg", width = 10, height = 14, device='jpeg', dpi=400)

# Q2 is Basically Done!






# Answer Q3: Does urbanization act as a deterministic or stochastic ecological filter on soil animal communities? ----


NullDF <- NMDS_Data %>% select(Park, Timepoint, Year, Urban_Kmeans_Cluster, Entomobryidae:Araneae)
NullDF <- NullDF %>% group_by(Park, Timepoint, Urban_Kmeans_Cluster) %>% summarise(across(Entomobryidae:Araneae, sum))

NullDF <- as.data.frame(NullDF)

#convert to pres/abs
NullDF[,4:126][NullDF[,4:126] >0] <- 1

#final cleaning
NullDF$Urban_Kmeans_Cluster <- as.factor(NullDF$Urban_Kmeans_Cluster)
NullDF=na.omit(NullDF)

    ### My Question: Are urban soil animal communities shaped more by stochastic or deterministic forces (sensu Segre et al. 2014)?
    #Literature: Chase 2011 Ecosphere; Segre et al. 2014 Eco Letters
    
    #RESOURCE: https://figshare.com/articles/dataset/Supplement_1_R_script_for_calculating_the_Raup-Crick_metric_as_described_in_the_text_/3563721?backTo=/collections/Using_null_models_to_disentangle_variation_in_community_dissimilarity_from_variation_in_-diversity/3308220
    # adapted from Chase et al. 2011 Ecosphere
    # vegdist resource: https://rdrr.io/cran/vegan/man/vegdist.html
    
    #vegan raup-crick option 
    # resource: https://rdrr.io/rforge/vegan/man/raupcrick.html
    
    Envir <- NMDS_Data %>% select(Park, 
                                  Timepoint,
                                  Urban_Kmeans_Cluster, 
                                  Name:Soil_Moisture) %>% 
                            select(-Distance_From_Geographic_Center)
    
    Envir <- Envir %>% rename(Park_Area = `Park_Area(m2)`,
                         Soil_WHC = Soil_Water_Holding_Capacity,
                         Soil_N = Soil_Nitrogen_Percentage,
                         Soil_C = Soil_Carbon_Percentage,
                         Impervious_Surface = Imperviousness_in_500m_boundary,
                         Population_Density = Population_Density_in_Surrounding_Area,
                         Bulk_Density = BD_mean,
                         Median_Household_Income = MedianIncome)


#make a jaccard and Raup-crick distance matrix of the the fauna community data.
#subset data so that the ID variables (PArk, Cluste, Timepoint) are not included.
jacdm <- vegdist(NullDF[, -1:-3], method="jaccard")  
rcdmr1 <-raupcrick(NullDF[, -1:-3], nsimul = 999,  null = "r1") 


#check for difference between mean similarity between Spring/Fall... we see that it is small.
mean(jacdm)
mean(rcdmr1)


#plot distribution of similarity values between the matrices
op <- par(mfrow=c(4,1), mar=c(4,4,1,1)+.1)  
hist(jacdm)
hist(rcdmr1)
par(op1)


#NMDS of jaccard and Raup-Crick Similarity
jacNMDS <- metaMDS(jacdm, k = 3, try = 1000) 
rcNMDS <- metaMDS(rcdmr1, k = 3, try = 1000)

# make a list of colors to use
colvec <- c("tan1", "plum3", "lightskyblue3", "hotpink1")
# make a list of point shapes to use
pchvec <- c(6,17,18, 22)


#now add NMDS results to data frames for later plotting
RC_results <- cbind(NullDF[,1:3], rcNMDS$points) %>%
  left_join(Envir)

JAC_results <- cbind(NullDF[,1:3], jacNMDS$points) %>%
  left_join(Envir)


###Test to see if the points are spread more or less between timepoints and urbanization levels. This would
# tell us that sites are becoming more or less homogenous.
# REFERENCE: https://stats.stackexchange.com/questions/314184/betadisper-is-significant-can-i-still-do-adonis-and-anosim

#raup
ggplot(data = RC_results, aes(x = MDS1, y = MDS2, color = Timepoint, shape = as.factor(Urban_Kmeans_Cluster))) + geom_point(size = 4)

#now test for statistical differences in the dispersion of the points.
seasonurban_betadisper_rc <- betadisper(rcdmr1, group=interaction(NullDF$Urban_Kmeans_Cluster, NullDF$Timepoint))
anova(seasonurban_betadisper_rc) #p = 0.2592
TukeyHSD(seasonurban_betadisper_rc)
seasonurban_betadisper_rc #spring is more dispersed than fall

table(NullDF$Timepoint, NullDF$Urban_Kmeans_Cluster) #groupings are uneven

#jac
ggplot(data = JAC_results, aes(x = MDS1, y = MDS2, color = Timepoint, shape = as.factor(Urban_Kmeans_Cluster))) + geom_point(size = 4)
ggplot(data = JAC_results, aes(x = MDS1, y = MDS3, color = Timepoint, shape = as.factor(Urban_Kmeans_Cluster))) + geom_point(size = 4)


seasonurban_betadisper_jac <- betadisper(jacdm, group=interaction(NullDF$Urban_Kmeans_Cluster, NullDF$Timepoint))
anova(seasonurban_betadisper_jac) #p = 0.03909; this says the smaller group (high urban) has greater dispersion; therefore the PERMANOVA is too liberal (or reject a null falsely) 
TukeyHSD(seasonurban_betadisper_jac)
seasonurban_betadisper_jac

# not a big difference between jaccard and raup, telling us that there may be systematic ecological processes acting on the communities. 
# but no big differences in spread, showing that the differences are small.




#now try to plot jac/raup with ellipse for grouping [https://github.com/jfq3/ggordiplots]
devtools::install_github("jfq3/ggordiplots")
install.packages("ggordiplots")
library(ggordiplots)

jacdm <- vegdist(NullDF[, -1:-3], method="jaccard") 
rcdmr1 <-raupcrick(NullDF[, -1:-3], nsimul = 999,  null = "r1") 

Raup_ord <- cmdscale(rcdmr1, k =3, eig = TRUE, add = TRUE) 
Jac_ord <- cmdscale(jacdm, k =3, eig = TRUE, add = TRUE) 

  #Q3 Plot ------
#now plot the ordinations

#notes- issue with assigning colors and symbols for timepoints/urbanization, I will need to dive into that more.
#RESOURCE: https://john-quensen.com/wp-content/uploads/2020/12/Modifying_Plots_Made_with_ggordiplots.html

plt1 <- gg_ordiplot(Raup_ord, groups = interaction(NullDF$Urban_Kmeans_Cluster,NullDF$Timepoint),
                    spiders = TRUE) 
plt1
 names(plt1)
ord.data <- plt1$df_ord

head(ord.data)
ord.data$Timepoint <- NullDF$Timepoint
ord.data$Urban_Kmeans_Cluster <- NullDF$Urban_Kmeans_Cluster


ellipse.plot<-plt1$df_ellipse
ellipse.plot <- ellipse.plot %>%
  mutate(Timepoint = case_when(
    endsWith(Group, "l") ~ "Fall",
    endsWith(Group, "g") ~ "Spring"))
ellipse.plot <- ellipse.plot %>%
  mutate(Urban_Kmeans_Cluster = case_when(
    startsWith(Group, "H") ~ "High_Urban",
    startsWith(Group, "L") ~ "Low_Urban"))

#Raup Plot!
Plot1_Null <- ggplot() +  
geom_path(data = ellipse.plot, aes(x = x, y = y, linetype = Timepoint, color = Urban_Kmeans_Cluster)) + 
  geom_point(data = ord.data, aes(x = x, y = y, shape = Timepoint, color = Urban_Kmeans_Cluster), size = 3, alpha = 0.5) + 
  scale_shape_manual(values=c(19, 15, 1, 0))+
  scale_colour_manual(values = c("black", "forestgreen"))+
  theme_bw() + theme(axis.title.x = element_blank(), 
                     axis.title.y = element_blank()) + 
  theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "grey30"), 
        axis.ticks = element_blank(),  legend.key = element_blank(), 
        legend.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        legend.text = element_text(size = 9, colour = "grey30")) +
  labs(colour = "Urbanization Cluster")





plt2 <- gg_ordiplot(Jac_ord, groups = interaction(NullDF$Urban_Kmeans_Cluster,NullDF$Timepoint),
                    spiders = TRUE) 
plt2
names(plt2)

ord.data2 <- plt2$df_ord

head(ord.data2)
ord.data2$Timepoint <- NullDF$Timepoint
ord.data2$Urban_Kmeans_Cluster <- NullDF$Urban_Kmeans_Cluster


ellipse.plot2<-plt2$df_ellipse
ellipse.plot2 <- ellipse.plot2 %>%
  mutate(Timepoint = case_when(
    endsWith(Group, "l") ~ "Fall",
    endsWith(Group, "g") ~ "Spring"))
ellipse.plot2 <- ellipse.plot2 %>%
  mutate(Urban_Kmeans_Cluster = case_when(
    startsWith(Group, "H") ~ "High_Urban",
    startsWith(Group, "L") ~ "Low_Urban"))


#Jaccard Plot!
Plot2_Null <- ggplot() +  
  geom_path(data = ellipse.plot2, aes(x = x, y = y, linetype = Timepoint, color = Urban_Kmeans_Cluster)) + 
  geom_point(data = ord.data2, aes(x = x, y = y, shape = Timepoint, color = Urban_Kmeans_Cluster), size = 3, alpha = 0.5) + 
  scale_shape_manual(values=c(19, 15, 1, 0))+
  scale_colour_manual(values = c("black", "forestgreen"))+
  theme_bw() + theme(axis.title.x = element_blank(), 
                     axis.title.y = element_blank()) + 
  theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "grey30"), 
        axis.ticks = element_blank(),  legend.key = element_blank(), 
        legend.title = element_text(size = 10, face = "bold", colour = "grey30"), 
        legend.text = element_text(size = 9, colour = "grey30")) + 
  labs(colour = "Urbanization Cluster")


Plot1_Null #raup
Plot2_Null #jaccard

figure3 <- ggarrange(Plot1_Null,
                     Plot2_Null,
                     labels = c("A", "B"),
                     ncol = 2, nrow = 1)

ggsave(filename = "Finalized_Manuscript_Analysis/NullModel_NMDS.jpg", width = 12, height = 5, device='jpeg', dpi=400)




#PERMANOVA Resource: https://data.marcoplebani.com/annotated-code-for-performing-multivariate-statistics/#:~:text=Permutation%2Dbased%20Multivariate%20Analysis%20of,differ%20significantly%20from%20the%20others%3F
if (!require(pairwiseAdonis)) install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(pairwiseAdonis)
library(permute)

permuteplan <- with(NullDF, how(within=Within(type="free"), plots=Plots(strata=Park, type="free")))
permuteplanUrban1 <- with(NullDF[NullDF$Urban_Kmeans_Cluster == "1",], how(within=Within(type="none"), plots=Plots(strata=Park, type="free")))
permuteplanUrban2 <- with(NullDF[NullDF$Urban_Kmeans_Cluster == "2",], how(within=Within(type="none"), plots=Plots(strata=Park, type="free")))


adonis2(NullDF[NullDF$Urban_Kmeans_Cluster == "Low_Urban",4:126]~Timepoint, data = NullDF[NullDF$Urban_Kmeans_Cluster == "Low_Urban",],strata = NullDF[NullDF$Urban_Kmeans_Cluster == "Low_Urban",]$Park)
adonis2(NullDF[NullDF$Urban_Kmeans_Cluster == "High_Urban",4:126]~Timepoint, data = NullDF[NullDF$Urban_Kmeans_Cluster == "High_Urban",],strata = NullDF[NullDF$Urban_Kmeans_Cluster == "High_Urban",]$Park)



#10.11 - Read this carefully and understand how to interpret adonis liberally/conservatively -  https://stats.stackexchange.com/questions/314184/betadisper-is-significant-can-i-still-do-adonis-and-anosim










# Supplemental Data: ----
  #make table for species presence----
TABLE <- NMDS_Data %>% group_by(Park, Urban_Kmeans_Cluster, Timepoint) %>% summarise_at(vars(Entomobryidae:Araneae), mean, na.rm = TRUE)
TABLE[,4:126][TABLE[,4:126]>0] <-1

Sum <- TABLE %>% group_by(Park, Timepoint) %>% mutate(sum = rowSums(across(c(Entomobryidae:Araneae))))

Sum <- TABLE %>% group_by(Park, Timepoint) %>% 
  select(starts_with("Orib")) %>% 
  mutate(oribatidsum = rowSums(across(3:24)))

Sum <- TABLE %>% group_by(Park, Timepoint) %>% 
  select(starts_with("Meso")) %>% 
  mutate(Mesosum = rowSums(across(3:41)))

Rowsum<-NMDS_Data %>% select(Entomobryidae:Araneae) %>% mutate(sumdensity = rowSums(across(1:123)*1000))
Rowsum$sumdensity

Table_long<- TABLE %>%
  pivot_longer(-c(Urban_Kmeans_Cluster, Timepoint), names_to = "Species", values_to = "Presence")
write_csv(Table_long, "TableLong.csv")

NMDS_Data

  #Calculate Coefficient of Variance -----
H <- NMDS_Data %>% group_by(Park, Timepoint) %>% filter(Urban_Kmeans_Cluster == "High_Urban")
L <- NMDS_Data %>% group_by(Park, Timepoint) %>% filter(Urban_Kmeans_Cluster == "Low_Urban")

  
  
  High_Urban <- sapply(H, function(x) sd(x, na.rm=T) / mean(x, na.rm=T) * 100)
  High_Urban.SE <- sapply(H, function(x) sd(x, na.rm=T) / sqrt(length(x, na.rm=T)))
  
  Low_Urban <- sapply(L, function(x) sd(x, na.rm=T) / mean(x, na.rm=T) * 100)
  
DataSet<-cbind(High_Urban, Low_Urban)

DataSet <- as.data.frame(DataSet)


#final dataset
clean_data <- DataSet[-c(2,4:6,24:26,31:153), ]

write_csv(clean_data, "CoeffVar_variables.csv")
  
  #means table of soil characteristics based on suggestions ----
x <- Div.Env %>% 
  dplyr::group_by(Park, Timepoint) %>% mutate(Sand_Percent = mean(Soil_Sand),
                                                Silt_Percent = mean(Soil_Silt),
                                                Clay_Percent = mean(Soil_Clay),
                                                C_Percent = mean(Soil_Carbon_Percentage),
                                                N_Percent = mean(Soil_Nitrogen_Percentage),
                                                BD = mean(Bulk_Density),
                                                WHC = mean(Soil_Water_Holding_Capacity),
                                                Soil_Sat = mean(Soil_Saturation),
                                                Soil_pH = mean(Soil_pH), 
                                              
                                                Sand_SE = sd(Soil_Sand)/sqrt(4),
                                                Silt_SE = sd(Soil_Silt)/sqrt(4),
                                                Clay_SE = sd(Soil_Clay)/sqrt(4),
                                                C_SE = sd(Soil_Carbon_Percentage)/sqrt(4),
                                                N_SE = sd(Soil_Nitrogen_Percentage)/sqrt(4),
                                                BD_SE = sd(Bulk_Density)/sqrt(4),
                                                WHC_SE = sd(Soil_Water_Holding_Capacity)/sqrt(4),
                                                Soil_Sat_SE = sd(Soil_Saturation)/sqrt(4),
                                                Soil_pH_SE = sd(Soil_pH)/sqrt(4)) %>% 
                                                select(Park, Timepoint,
                                                       Sand_Percent, 
                                                       Silt_Percent, 
                                                       Clay_Percent, 
                                                       C_Percent, 
                                                       N_Percent,
                                                       BD,
                                                       WHC,
                                                       Soil_Sat,
                                                       Soil_pH,
                                                       Sand_SE,
                                                       Silt_SE,
                                                       Clay_SE,
                                                       C_SE,
                                                       N_SE,
                                                       BD_SE,
                                                       WHC_SE,
                                                       Soil_Sat_SE,
                                                       Soil_pH_SE)
                                                       
                                                       
                                              
                                                        


write_csv(x, "SoilCharacteristic.csv")

x$Park
  #Discussion Moisture Calc ----

SoilMoisture <- Environmental_Data %>% mutate(FallMoistureAVG = (Fall2021_MoisturePercent + Fall2022_MoisturePercent/2)) %>%
  mutate(SpringMoistureAVG = (Spring2021_MoisturePercent + Spring2022_MoisturePercent/2)) %>% select(Urban_Kmeans_Cluster, SpringMoistureAVG, FallMoistureAVG)

F.Moist <- t.test(FallMoistureAVG~Urban_Kmeans_Cluster, data = SoilMoisture)
summary(F.Moist)

summary(SoilMoisture~Urban_Kmeans_Cluster)

H <- SoilMoisture %>% filter(Urban_Kmeans_Cluster == "High_Urban")
L <- SoilMoisture %>% filter(Urban_Kmeans_Cluster == "Low_Urban")

range(H$FallMoistureAVG)
range(L$FallMoistureAVG)  


