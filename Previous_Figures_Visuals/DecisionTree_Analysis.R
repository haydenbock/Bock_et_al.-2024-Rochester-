
# packages ----------------------------------------------------------------
install.packages("randomForest")
install.packages("datasets")
install.packages("caret")
install.packages("janitor")

library(randomForest)
library(datasets)
library(caret)
library(readxl)
library(janitor)

# TUTORIAL:: https://www.r-bloggers.com/2021/04/random-forest-in-r/ ---------

data<-iris
str(data)

data$Species <- as.factor(data$Species)
table(data$Species)

#Lets start with random seed so the outcome will be repeatable and store train and test data.
set.seed(222)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]

#Random Forest in R
rf <- randomForest(Species~., data=train, proximity=TRUE) 
print(rf)

#Prediction adn Confusin matrix from training set
p1 <- predict(rf, train)
confusionMatrix(p1, train$ Species)

#error rate of Random Forest
plot(rf)

#tuning MTRY
t <- tuneRF(train[,-5], train[,5],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 150,
            trace = TRUE,
            improve = 0.05)

#number of nodes for tree
hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")

#Variable importance
varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")

importance(rf) #higher values indicate more important variables


partialPlot(rf, train, Petal.Width, "setosa") #The inference should be, if the petal width is less than 1.5 then higher chances of classifying into Setosa class.


#Multi-dimensional Scaling Plot of Proximity Matrix; Dimension plot also can create from random forest model.
MDSplot(rf, train$Species)




# Import and Clean Data ---------------------------------------------------

Environmental_Data <- read_excel("Environmental_Data.xlsx")

Environmental_Data1 <- Environmental_Data %>% clean_names() #alleviates issue with variables not being read by random forest https://stackoverflow.com/questions/61639109/error-in-evalpredvars-data-env-object-example-not-found-in-randomforest

Environmental_Data1 <- Environmental_Data1 %>% rename( Park = park)
Environmental_Data1[is.na(Environmental_Data1)] <- 0

Environmental_Data1 <- Environmental_Data1 %>% select(-name, #subtract variables that dont make sense for RF; this doesn't account for other moisture variables that don't match the analysis year.
                                                      -census_block,
                                                      -year_established,
                                                      -years_old,
                                                      -mean_latitude,
                                                      -mean_longitude)



  
Mesofauna_Diversity_Indices <- read_csv("Mesofauna_Diversity_Indices.csv")
Rochester_Mesofauna_Density <- read_csv("Rochester_Mesofauna_Density.csv")
Mite_Collembola_Ratio <- read_csv("Mite.Collembola.Ratio.csv")

  
  
  

# Predicting Species Abundance Sp2021---------------------------------------------

#Spring 2021 Data Cleaning
Sp21 <- Mesofauna_Diversity_Indices %>% 
  filter(Year == "2021" & Timepoint == "Spring") 

Sp21_Env <- Environmental_Data1 %>% select(-fall2021_moisture_percent,
                                           -fall2022_moisture_percent,
                                           -fa2021_soil_saturation,
                                           -fa2022_soil_saturation,
                                           -spring2022_moisture_percent,
                                           -sp2022_soil_saturation) #Subtract other diversity indice variables that are not important in the moment

Sp21 <- left_join(Sp21,Sp21_Env, by = "Park")

Sp21 <- Sp21 %>% select(-simpson, -shannon, -municipality, -soil_nitrogen_percentage, -soil_nitrogen_se)


#Spring 2021 Random forest
  
  #Lets start with random seed so the outcome will be repeatable and store train and test data.
  set.seed(222)
  ind <- sample(2, nrow(Sp21), replace = TRUE, prob = c(0.7, 0.3))
  train <- Sp21[ind==1,]
  test <- Sp21[ind==2,]
  
  train <- as.data.frame(train)
  
  
  
  #Random Forest in R
  rf <- randomForest(Abundance~., data=train, proximity=TRUE) 
  print(rf)
  
  
  #Prediction and Confusion matrix from training set
  p1 <- predict(rf, train)
  p1<-as.numeric(p1)
  confusionMatrix(p1, train$Abundance)
  
  str(p1)
  str(train$Abundance)
  #error rate of Random Forest
  plot(rf)
  
  #tuning MTRY
  t <- tuneRF(train[,-5], train[,5],
              stepFactor = 0.5,
              plot = TRUE,
              ntreeTry = 150,
              trace = TRUE,
              improve = 0.05)
  
  #number of nodes for tree
  hist(treesize(rf),
       main = "No. of Nodes for the Trees",
       col = "green")
  
  #Variable importance
  varImpPlot(rf,
             sort = T,
             n.var = 10,
             main = "Top 10 - Variable Importance")
  
  importance(rf) #higher values indicate more important variables
  
  
  partialPlot(rf, train, Abundance) #The inference should be, if the petal width is less than 1.5 then higher chances of classifying into Setosa class.
  
  
  #Multi-dimensional Scaling Plot of Proximity Matrix; Dimension plot also can create from random forest model.
  MDSplot(rf, train$Abundance)

  
# Predicting Species Abundance Fa2021---------------------------------------------
  
  #Fall 2021 Data Cleaning
  Fa21 <- Mesofauna_Diversity_Indices %>% 
    filter(Year == "2021" & Timepoint == "Fall") 
  
  Fa21_Env <- Environmental_Data1 %>% select(-spring2021_moisture_percent,
                                             -fall2022_moisture_percent,
                                             -sp2021_soil_saturation,
                                             -fa2022_soil_saturation,
                                             -spring2022_moisture_percent,
                                             -sp2022_soil_saturation) #Subtract other diversity indice variables that are not important in the moment
  
  Fa21 <- left_join(Fa21,Fa21_Env, by = "Park")
  
  Fa21 <- Fa21 %>% select(-simpson, -shannon, -municipality, -soil_nitrogen_percentage, -soil_nitrogen_se)
  
  
  #Fall 2021 Random forest
  
  #Lets start with random seed so the outcome will be repeatable and store train and test data.
  set.seed(222)
  ind <- sample(2, nrow(Fa21), replace = TRUE, prob = c(0.7, 0.3))
  train <- Fa21[ind==1,]
  test <- Fa21[ind==2,]
  
  train <- as.data.frame(train)
  
  
  
  #Random Forest in R
  rf <- randomForest(Abundance~., data=train, proximity=TRUE) 
  print(rf)
  
  
  #Prediction and Confusion matrix from training set
  p1 <- predict(rf, train)
  p1<-as.numeric(p1)
  confusionMatrix(p1, train$Abundance)
  
  str(p1)
  str(train$Abundance)
  #error rate of Random Forest
  plot(rf)
  
  #tuning MTRY
  t <- tuneRF(train[,-5], train[,5],
              stepFactor = 0.5,
              plot = TRUE,
              ntreeTry = 150,
              trace = TRUE,
              improve = 0.05)
  
  #number of nodes for tree
  hist(treesize(rf),
       main = "No. of Nodes for the Trees",
       col = "green")
  
  #Variable importance
  varImpPlot(rf,
             sort = T,
             n.var = 10,
             main = "Top 10 - Variable Importance")
  
  importance(rf) #higher values indicate more important variables
  
  
  partialPlot(rf, train, Abundance) #The inference should be, if the petal width is less than 1.5 then higher chances of classifying into Setosa class.
  
  
  #Multi-dimensional Scaling Plot of Proximity Matrix; Dimension plot also can create from random forest model.
  MDSplot(rf, train$Abundance)
  
  
# Predicting Species Abundance Sp2022---------------------------------------------
  
  #Spring 2022 Data Cleaning
  Sp22 <- Mesofauna_Diversity_Indices %>% 
    filter(Year == "2022" & Timepoint == "Spring") 
  
  Sp22_Env <- Environmental_Data1 %>% select(-fall2021_moisture_percent,
                                             -fall2022_moisture_percent,
                                             -fa2021_soil_saturation,
                                             -fa2022_soil_saturation,
                                             -spring2021_moisture_percent,
                                             -sp2021_soil_saturation) #Subtract other diversity indice variables that are not important in the moment
  
  Sp22 <- left_join(Sp22,Sp22_Env, by = "Park")
  
  Sp22 <- Sp22 %>% select(-simpson, -shannon, -municipality, -soil_nitrogen_percentage, -soil_nitrogen_se)
  
  
  #Spring 2021 Random forest
  
  #Lets start with random seed so the outcome will be repeatable and store train and test data.
  set.seed(222)
  ind <- sample(2, nrow(Sp22), replace = TRUE, prob = c(0.7, 0.3))
  train <- Sp22[ind==1,]
  test <- Sp22[ind==2,]
  
  train <- as.data.frame(train)
  
  
  
  #Random Forest in R
  rf <- randomForest(Abundance~., data=train, proximity=TRUE) 
  print(rf)
  
  
  #Prediction and Confusion matrix from training set
  p1 <- predict(rf, train)
  p1<-as.numeric(p1)
  confusionMatrix(p1, train$Abundance)
  
  str(p1)
  str(train$Abundance)
  #error rate of Random Forest
  plot(rf)
  
  #tuning MTRY
  t <- tuneRF(train[,-5], train[,5],
              stepFactor = 0.5,
              plot = TRUE,
              ntreeTry = 150,
              trace = TRUE,
              improve = 0.05)
  
  #number of nodes for tree
  hist(treesize(rf),
       main = "No. of Nodes for the Trees",
       col = "green")
  
  #Variable importance
  varImpPlot(rf,
             sort = T,
             n.var = 10,
             main = "Top 10 - Variable Importance")
  
  importance(rf) #higher values indicate more important variables
  
  
  partialPlot(rf, train, Abundance) #The inference should be, if the petal width is less than 1.5 then higher chances of classifying into Setosa class.
  
  
  #Multi-dimensional Scaling Plot of Proximity Matrix; Dimension plot also can create from random forest model.
  MDSplot(rf, train$Abundance)
  
  
# Predicting Species Abundance Fa2022---------------------------------------------
  
  #Fall 2022 Data Cleaning
  Fa22 <- Mesofauna_Diversity_Indices %>% 
    filter(Year == "2022" & Timepoint == "Fall") 
  
  Fa22_Env <- Environmental_Data1 %>% select(-spring2021_moisture_percent,
                                             -fall2021_moisture_percent,
                                             -sp2021_soil_saturation,
                                             -fa2021_soil_saturation,
                                             -spring2022_moisture_percent,
                                             -sp2022_soil_saturation) #Subtract other diversity indice variables that are not important in the moment
  
  Fa22 <- left_join(Fa22,Fa22_Env, by = "Park")
  
  Fa22 <- Fa22 %>% select(-simpson, -shannon, -municipality, -soil_nitrogen_percentage, -soil_nitrogen_se)
  
  Fa22[is.na(Fa22)] <- 0
  
  #Fall 2021 Random forest
  
  #Lets start with random seed so the outcome will be repeatable and store train and test data.
  set.seed(222)
  ind <- sample(2, nrow(Fa22), replace = TRUE, prob = c(0.7, 0.3))
  train <- Fa22[ind==1,]
  test <- Fa22[ind==2,]
  
  train <- as.data.frame(train)
  
  
  
  #Random Forest in R
  rf <- randomForest(Abundance~., data=train, proximity=TRUE) 
  print(rf)
  
  
  #Prediction and Confusion matrix from training set
  p1 <- predict(rf, train)
  p1<-as.numeric(p1)
  confusionMatrix(p1, train$Abundance)
  
  str(p1)
  str(train$Abundance)
  #error rate of Random Forest
  plot(rf)
  
  #tuning MTRY
  t <- tuneRF(train[,-5], train[,5],
              stepFactor = 0.5,
              plot = TRUE,
              ntreeTry = 150,
              trace = TRUE,
              improve = 0.05)
  
  #number of nodes for tree
  hist(treesize(rf),
       main = "No. of Nodes for the Trees",
       col = "green")
  
  #Variable importance
  varImpPlot(rf,
             sort = T,
             n.var = 10,
             main = "Top 10 - Variable Importance")
  
  importance(rf) #higher values indicate more important variables
  
  
  partialPlot(rf, train, Abundance) #The inference should be, if the petal width is less than 1.5 then higher chances of classifying into Setosa class.
  
  
  #Multi-dimensional Scaling Plot of Proximity Matrix; Dimension plot also can create from random forest model.
  MDSplot(rf, train$Abundance)
  
  
  
  
  
  
  
  
# Predicting park alpha diversity Sp2021 --------------------------------
  Total_Micro_AlphaDiversity <- read_csv("Total+Micro_AlphaDiversity.csv")
  
  #Spring 2021 Data Cleaning
  Sp21 <- Total_Micro_AlphaDiversity %>% 
    filter(Year == "2021" & Timepoint == "Spring") 
  
  Sp21_Env <- Environmental_Data1 %>% select(-fall2021_moisture_percent,
                                             -fall2022_moisture_percent,
                                             -fa2021_soil_saturation,
                                             -fa2022_soil_saturation,
                                             -spring2022_moisture_percent,
                                             -sp2022_soil_saturation) #Subtract other diversity indice variables that are not important in the moment
  
  Sp21 <- left_join(Sp21_Env,Sp21, by = "Park")
  
  Sp21 <- Sp21 %>% select(-Plot.simpson, 
                          -Plot.Abundance,
                          -Plot.shannon.micro,
                          -Plot.simpson.micro,
                          -Plot.Abundance.micro, 
                          -municipality, -soil_nitrogen_percentage, -soil_nitrogen_se, -soil_carbon_se, -Park)
 
   #remove NA
  Sp21["Plot.shannon"][is.na(Sp21["Plot.shannon"])] <- 0

  

  #Spring 2021 Random forest
  
  #Lets start with random seed so the outcome will be repeatable and store train and test data.
  set.seed(222)
  ind <- sample(2, nrow(Sp21), replace = TRUE, prob = c(0.7, 0.3))
  train <- Sp21[ind==1,]
  test <- Sp21[ind==2,]
  
  train <- as.data.frame(train)
  
  #remove NA
  train <- train[, colSums(is.na(train)) == 0]
  
  
  
  #Random Forest in R
  rf <- randomForest(Plot.shannon~., data=train, proximity=TRUE) 
  print(rf)
  
  
  #Prediction and Confusion matrix from training set
  p1 <- predict(rf, train)
  p1<-as.numeric(p1)
  confusionMatrix(p1, train$Abundance)
  
  str(p1)
  str(train$Abundance)
  #error rate of Random Forest
  plot(rf)
  
  #tuning MTRY
  t <- tuneRF(train[,-5], train[,5],
              stepFactor = 0.5,
              plot = TRUE,
              ntreeTry = 150,
              trace = TRUE,
              improve = 0.05)
  
  #number of nodes for tree
  hist(treesize(rf),
       main = "No. of Nodes for the Trees",
       col = "green")
  
  #Variable importance
  varImpPlot(rf,
             sort = T,
             n.var = 10,
             main = "Top 10 - Variable Importance")
  
  importance(rf) #higher values indicate more important variables
  
  
  partialPlot(rf, train, Abundance) #The inference should be, if the petal width is less than 1.5 then higher chances of classifying into Setosa class.
  
  
  #Multi-dimensional Scaling Plot of Proximity Matrix; Dimension plot also can create from random forest model.
  MDSplot(rf, train$Abundance)
  
  
  


# Predicting park alpha diversity Fa2021 --------------------------------
  Total_Micro_AlphaDiversity <- read_csv("Total+Micro_AlphaDiversity.csv")
  
  #Fall 2021 Data Cleaning
  Fa21 <- Total_Micro_AlphaDiversity %>% 
    filter(Year == "2021" & Timepoint == "Fall") 
  
  Fa21_Env <- Environmental_Data1 %>% select(-spring2021_moisture_percent,
                                             -fall2022_moisture_percent,
                                             -sp2021_soil_saturation,
                                             -fa2022_soil_saturation,
                                             -spring2022_moisture_percent,
                                             -sp2022_soil_saturation) #Subtract other diversity indice variables that are not important in the moment
  
  Fa21 <- left_join(Fa21_Env,Fa21, by = "Park")
  
  Fa21 <- Fa21 %>% select(-Plot.simpson, 
                          -Plot.Abundance,
                          -Plot.shannon.micro,
                          -Plot.simpson.micro,
                          -Plot.Abundance.micro, 
                          -municipality, -soil_nitrogen_percentage, -soil_nitrogen_se, -soil_carbon_se, -Park, -UniqueID, -Plot_Number)
  
  #remove NA
  Fa21["Plot.shannon"][is.na(Fa21["Plot.shannon"])] <- 0
  
  
  
  #Fall 2021 Random forest
  
  #Lets start with random seed so the outcome will be repeatable and store train and test data.
  set.seed(222)
  ind <- sample(2, nrow(Fa21), replace = TRUE, prob = c(0.7, 0.3))
  train <- Fa21[ind==1,]
  test <- Fa21[ind==2,]
  
  train <- as.data.frame(train)
  
  #remove NA
  train <- train[, colSums(is.na(train)) == 0]
  
  
  
  #Random Forest in R
  rf <- randomForest(Plot.shannon~., data=train, proximity=TRUE) 
  print(rf)
  
  
  #Prediction and Confusion matrix from training set
  p1 <- predict(rf, train)
  p1<-as.numeric(p1)
  confusionMatrix(p1, train$Abundance)
  
  str(p1)
  str(train$Abundance)
  #error rate of Random Forest
  plot(rf)
  
  #tuning MTRY
  t <- tuneRF(train[,-5], train[,5],
              stepFactor = 0.5,
              plot = TRUE,
              ntreeTry = 150,
              trace = TRUE,
              improve = 0.05)
  
  #number of nodes for tree
  hist(treesize(rf),
       main = "No. of Nodes for the Trees",
       col = "green")
  
  #Variable importance
  varImpPlot(rf,
             sort = T,
             n.var = 10,
             main = "Top 10 - Variable Importance")
  
  importance(rf) #higher values indicate more important variables
  
  
  partialPlot(rf, train, Abundance) #The inference should be, if the petal width is less than 1.5 then higher chances of classifying into Setosa class.
  
  
  #Multi-dimensional Scaling Plot of Proximity Matrix; Dimension plot also can create from random forest model.
  MDSplot(rf, train$Abundance)
  
  
  
  
  
  
  
  
# Predict Acari:Collembola Ratio ------------------------------------------
  #Spring 2021 Data Cleaning
  Df <- as.tibble(Mite_Collembola_Ratio)
  

  Df[is.na(Df)] <- 0
  Df[Df == "Inf"] <- 0
  
  AC.Ratio <- Df
  
  Sp21.AC <- AC.Ratio %>% 
    filter(Year == "2021" & Timepoint == "Spring" & A.C.ratio != 0) 
  
  Sp21_Env <- Environmental_Data1 %>% select(-fall2021_moisture_percent,
                                             -fall2022_moisture_percent,
                                             -fa2021_soil_saturation,
                                             -fa2022_soil_saturation,
                                             -spring2022_moisture_percent,
                                             -sp2022_soil_saturation) #Subtract other diversity indice variables that are not important in the moment
  
  Sp21 <- left_join(Sp21,Sp21_Env, by = "Park")
  
  #Sp21 <- Sp21 %>% select(-simpson, -shannon, -municipality, -soil_nitrogen_percentage, -soil_nitrogen_se)
  
  
  #Spring 2021 Random forest
  
  #Lets start with random seed so the outcome will be repeatable and store train and test data.
  set.seed(222)
  ind <- sample(2, nrow(Sp21), replace = TRUE, prob = c(0.7, 0.3))
  train <- Sp21[ind==1,]
  test <- Sp21[ind==2,]
  
  train <- as.data.frame(train)
  
  
  
  
  #Random Forest in R
  rf <- randomForest(A.C.ratio~., data=train, proximity=TRUE) 
  print(rf)
  
  
  #Prediction and Confusion matrix from training set
  p1 <- predict(rf, train)
  p1<-as.numeric(p1)
  confusionMatrix(p1, train$Abundance)
  
  str(p1)
  str(train$Abundance)
  #error rate of Random Forest
  plot(rf)
  
  #tuning MTRY
  t <- tuneRF(train[,-5], train[,5],
              stepFactor = 0.5,
              plot = TRUE,
              ntreeTry = 150,
              trace = TRUE,
              improve = 0.05)
  
  #number of nodes for tree
  hist(treesize(rf),
       main = "No. of Nodes for the Trees",
       col = "green")
  
  #Variable importance
  varImpPlot(rf,
             sort = T,
             n.var = 10,
             main = "Top 10 - Variable Importance")
  
  importance(rf) #higher values indicate more important variables
  
  
  partialPlot(rf, train, Abundance) #The inference should be, if the petal width is less than 1.5 then higher chances of classifying into Setosa class.
  
  
  #Multi-dimensional Scaling Plot of Proximity Matrix; Dimension plot also can create from random forest model.
  MDSplot(rf, train$Abundance)
  
  


  Mite_Collembola_Ratio <- read_csv("Mite.Collembola.Ratio.csv")
  
  #Spring 2021 Data Cleaning
  Sp21 <- Mite_Collembola_Ratio %>% 
    filter(Year == "2021" & Timepoint == "Spring") 
  
  Sp21_Env <- Environmental_Data1 %>% select(-fall2021_moisture_percent,
                                             -fall2022_moisture_percent,
                                             -fa2021_soil_saturation,
                                             -fa2022_soil_saturation,
                                             -spring2022_moisture_percent,
                                             -sp2022_soil_saturation) #Subtract other diversity indice variables that are not important in the moment
  
  Sp21 <- left_join(Sp21_Env,Sp21, by = "Park")
  
  #Sp21 <- Sp21 %>% select(-Plot.simpson, 
  #                        -Plot.Abundance,
  #                        -Plot.shannon.micro,
  #                        -Plot.simpson.micro,
  #                        -Plot.Abundance.micro, 
  #                        -municipality, -soil_nitrogen_percentage, -soil_nitrogen_se, -soil_carbon_se, -Park)
  
  #remove NA
  Sp21["A.C.ratio"][is.na(Sp21["A.C.ratio"])] <- 0

  Sp21<- Sp21 %>% filter(A.C.ratio != "Inf")
  
  
  #Spring 2021 Random forest
  
  #Lets start with random seed so the outcome will be repeatable and store train and test data.
  set.seed(222)
  ind <- sample(2, nrow(Sp21), replace = TRUE, prob = c(0.7, 0.3))
  train <- Sp21[ind==1,]
  test <- Sp21[ind==2,]
  
  train <- as.data.frame(train)
  
  #remove NA
  train <- train[, colSums(is.na(train)) == 0]
  train <- train[, colSums(is.infinite(train)) == 0]
  
  
  #Random Forest in R
  rf <- randomForest(A.C.ratio~., data=train, proximity=TRUE) 
  print(rf)
  
  
  #Prediction and Confusion matrix from training set
  p1 <- predict(rf, train)
  p1<-as.numeric(p1)
  confusionMatrix(p1, train$Abundance)
  
  str(p1)
  str(train$Abundance)
  #error rate of Random Forest
  plot(rf)
  
  #tuning MTRY
  t <- tuneRF(train[,-5], train[,5],
              stepFactor = 0.5,
              plot = TRUE,
              ntreeTry = 150,
              trace = TRUE,
              improve = 0.05)
  
  #number of nodes for tree
  hist(treesize(rf),
       main = "No. of Nodes for the Trees",
       col = "green")
  
  #Variable importance
  varImpPlot(rf,
             sort = T,
             n.var = 10,
             main = "Top 10 - Variable Importance")
  
  importance(rf) #higher values indicate more important variables
  
  
  partialPlot(rf, train, Abundance) #The inference should be, if the petal width is less than 1.5 then higher chances of classifying into Setosa class.
  
  
  #Multi-dimensional Scaling Plot of Proximity Matrix; Dimension plot also can create from random forest model.
  MDSplot(rf, train$Abundance)
  
  
  
  