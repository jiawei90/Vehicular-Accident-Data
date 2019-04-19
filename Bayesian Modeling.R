library(e1071)
library(caret)
library("xlsx")
library("PerformanceAnalytics")
library('corrplot')
library(RColorBrewer)
library(bnclassify)
# ReferenceL http://www.bioconductor.org/install/#install-bioconductor-packages
# BiocManager::install("graph", version = "3.8")
library(graph)

#BiocManager::install("Rgraphviz")
library("Rgraphviz")

set.seed(5678)


# Reading of input data file
setwd("C:\\Users\\chee.jw\\Dropbox\\NUS\\Semester 3\\KE 5107\\Lecture Notes\\Barry")
vehicleSafety <- read.csv("vehicle_safety_NASS2010_2000_2012.csv")
vehicleSafety_backup <- vehicleSafety

##### Exploratory Analysis - Correlation #####
# Data preparation for correlation analysis
vehicleSafety_Cor <- na.omit(vehicleSafety_backup)
vehicleSafety_Cor$GV_WGTCDTR <- NULL
vehicleSafety_Cor$OA_BAGDEPLY <- NULL
vehicleSafety_Cor$OA_SEX <- NULL
vehicleSafety_Cor$VE_GAD1 <- NULL

# Visualization #1 of correlation matrix
chart.Correlation(vehicleSafety_Cor[, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)], histogram=TRUE, pch=19)
chart.Correlation(vehicleSafety_Cor[, c(1,2,3,4,10,11,13,15,16,17)], histogram=TRUE, pch=19)

# visualization #2 (alternative) of the correlation matrix
M <- cor(vehicleSafety_Cor)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

p.mat <- cor.mtest(vehicleSafety_Cor)
corrplot(M, type="upper", order="hclust", 
         p.mat = p.mat, sig.level = 0.01)


##### Output to genie for exploratory analysis #####

# define no. of bins
numBins = 7
divisor = 100/numBins

vehicleSafety_noNA <- list()
count = 1
while (count <= 21)
{
  vehicleSafety_noNA[[count]] <- vehicleSafety[,count][!is.na(vehicleSafety[,count])]
  count = count + 1
}

# Binning for specific group of variables
vehicleSafety_genie <- vehicleSafety_backup
vehicleSafety_genie$OA_MAIS <- ifelse(vehicleSafety_genie$OA_MAIS < 2, "Minor", ifelse(vehicleSafety_genie$OA_MAIS < 4, "Moderate", ifelse(vehicleSafety_genie$OA_MAIS < 7, "Severe")))
vehicleSafety_genie$OA_MAIS <- as.factor(vehicleSafety_genie$OA_MAIS)

vehicleSafety_genie$OA_MANUSE <- ifelse(vehicleSafety_genie$OA_MANUSE == 0, "Not Used", ifelse(vehicleSafety_genie$OA_MANUSE == 1, "Used", NA))
vehicleSafety_genie$OA_MANUSE <- as.factor(vehicleSafety_genie$OA_MANUSE)

vehicleSafety_genie$GV_LANES <- ifelse(vehicleSafety_genie$GV_LANES < 3, "1 and 2", ifelse(vehicleSafety_genie$GV_LANES < 5, "3 and 4", ifelse(vehicleSafety_genie$GV_LANES < 8, " 5 and above")))
vehicleSafety_genie$GV_LANES <- as.factor(vehicleSafety_genie$GV_LANES)

vehicleSafety_genie$GV_SPLIMIT <- ifelse(vehicleSafety_genie$GV_SPLIMIT < 30, "1. <30mph", ifelse(vehicleSafety_genie$GV_SPLIMIT < 40, "2. <40mph", ifelse(vehicleSafety_genie$GV_SPLIMIT < 50, "3. <50mph", ifelse(vehicleSafety_genie$GV_SPLIMIT < 100, "4. > 50mph"))))
vehicleSafety_genie$GV_SPLIMIT <- as.factor(vehicleSafety_genie$GV_SPLIMIT)

vehicleSafety_genie[vehicleSafety_genie==""] <- NA

# Binning of typical variables
vehicleSafety_bin_genie <- vehicleSafety_genie
count = 1
while (count <= 21)
{
  if (is.factor(vehicleSafety_genie[,count])) 
  {
    vehicleSafety_bin_genie[, count] <- ifelse(is.na(vehicleSafety_genie[, count]), "Not Applicable", as.character(vehicleSafety_genie[,count]))
    vehicleSafety_bin_genie[, count] <- as.factor(vehicleSafety_bin_genie[, count])
  }
  else
  {
    percentile <- ecdf(vehicleSafety_noNA[[count]])
    postfix <- ceiling(percentile(vehicleSafety_genie[, count])*100/divisor)*divisor
    postfix <- ifelse(postfix == 100, 99.9, round(postfix)) 
    postfix <- paste(colnames(vehicleSafety_genie[count]), postfix)
    vehicleSafety_bin_genie[, count] <- ifelse(is.na(vehicleSafety_genie[,count]), "Not Applicable", postfix)
    vehicleSafety_bin_genie[, count] <- as.factor(vehicleSafety_bin_genie[, count])
  }
  
  count = count + 1
}

# Output as csv as input to genie for unsupervised learning
write.csv(vehicleSafety_bin_genie, row.names = FALSE, "C:\\Users\\chee.jw\\Dropbox\\NUS\\Semester 3\\KE 5107\\CA\\CA 3\\Genie\\vehicleSafety_BIN_GenieInput_full2.csv")


##### NAIVE BAYES WITH GAUSSIAN ASSUMPTION FOR CONTINUOUS VARIABLE #####
# Run this to categorize injury scales
vehicleSafety <- vehicleSafety[!is.na(vehicleSafety$OA_MAIS),]
vehicleSafety$OA_MAIS <- ifelse(vehicleSafety$OA_MAIS < 2, "Minor", ifelse(vehicleSafety$OA_MAIS < 4, "Moderate", ifelse(vehicleSafety$OA_MAIS < 7, "Severe")))
vehicleSafety$OA_MANUSE <- ifelse(vehicleSafety$OA_MANUSE == 0, "Not Used", ifelse(vehicleSafety$OA_MANUSE == 1, "Used", NA))

# Converting the columns in the data frame to categorical data
vehicleSafety$OA_MAIS <- as.factor(vehicleSafety$OA_MAIS)
vehicleSafety$GV_LANES <- as.factor(vehicleSafety$GV_LANES)
vehicleSafety$OA_MANUSE <- as.factor(vehicleSafety$OA_MANUSE)
vehicleSafety[vehicleSafety==""] <- NA

# splitting dataset to train and test 
split <- floor(nrow(vehicleSafety)*0.75)
train_ind <- sample(seq_len(nrow(vehicleSafety)), size = split)
vehicleSafety_train <- vehicleSafety[train_ind,]
vehicleSafety_test <- vehicleSafety[-train_ind,]

# naive bayes with gaussian distribution for continuous variable
nb_gaussian <- naiveBayes(OA_MAIS~., data=vehicleSafety_train)
nb_gaussian_pred <- predict(nb_gaussian, vehicleSafety_test, type="class")
confusion <- table(nb_gaussian_pred, vehicleSafety_test$OA_MAIS, dnn=c("Prediction","Actual"))
confusionMatrix(confusion)
accuracy(nb_gaussian_pred, vehicleSafety_test$OA_MAIS)
params_nb_gaussian <- nb_gaussian[2]$tables

# Output conditional probabilities to excel
setwd("C:\\Users\\chee.jw\\Dropbox\\NUS\\Semester 3\\KE 5107\\CA\\CA 3\\Conditional Probability Output\\With Categorization\\Naive Bayes Gaussian")
count = 1
list_params_nb_gaussian <- list()
while (count <= 20)
{
  list_params_nb_gaussian[[count]] <- data.frame(params_nb_gaussian[count])
  fileNameX <- paste(colnames(list_params_nb_gaussian[[count]])[1], ".xlsx", sep = "")
  write.xlsx(list_params_nb_gaussian[[count]], fileNameX, sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
  count = count + 1
  filename <- ""
}

##### THE FOLLOWING USES THE BNCLASSIFY PACKAGE TO CONSTRUCT VARIOUS NETWORK STRUCTURES OF NAIVE BAYES MODEL #####

###### Data Pre-processing #####
# generate a list of numbers without missing data for computation of percentile
vehicleSafety_noNA <- list()
count = 1
while (count <= 21)
{
  vehicleSafety_noNA[[count]] <- vehicleSafety[,count][!is.na(vehicleSafety[,count])]
  count = count + 1
}

# Automated Binning (20 Bins)
vehicleSafety_bin <- vehicleSafety
count = 1
while (count <= 21)
{
  if (is.factor(vehicleSafety[,count])) 
  {
    vehicleSafety_bin[, count] <- ifelse(is.na(vehicleSafety[, count]), "Not Applicable", as.character(vehicleSafety[,count]))
    vehicleSafety_bin[, count] <- as.factor(vehicleSafety_bin[, count])
  }
  else
  {
    percentile <- ecdf(vehicleSafety_noNA[[count]])
    vehicleSafety_bin[, count] <- ifelse(is.na(vehicleSafety[,count]), "Not Applicable", paste(colnames(vehicleSafety[count]), ceiling(percentile(vehicleSafety[, count])*100/5)*5))
    vehicleSafety_bin[, count] <- as.factor(vehicleSafety_bin[, count])
  }
    
  count = count + 1
}

# split into train and test datasets
vehicleSafety_bin_train <- vehicleSafety_bin[train_ind,]
vehicleSafety_bin_test <- vehicleSafety_bin[-train_ind,]

##### NAIVE BAYES with Binning #####
nb_bin <- nb('OA_MAIS', vehicleSafety_bin_train)
nb_bin <- lp(nb_bin, vehicleSafety_bin_train, smooth = 100)
nb_bin_pred <- predict(nb_bin, vehicleSafety_bin_test)
confusion <- table(nb_bin_pred, vehicleSafety_bin_test$OA_MAIS, dnn=c("Prediction","Actual"))
confusionMatrix(confusion)
accuracy(nb_bin_pred, vehicleSafety_bin_test$OA_MAIS)

# Various visualizations of the network structure
plot(nb_bin, layoutType = 'twopi', fontsize = 15)
plot(nb_bin, layoutType = 'neato', fontsize = 15)

# explore impact of smoothing parameter on accuracy
# accuracy improves when smoothing parameter is > 100
count = 0
accuracyList_nb_bin <- list()
while(count < 15)
{
  mySmooth = 0.00001 * (10^count)
  nb_bin <- nb('OA_MAIS', vehicleSafety_bin_train)
  nb_bin <- lp(nb_bin, vehicleSafety_bin_train, smooth = mySmooth)
  nb_bin_pred <- predict(nb_bin, vehicleSafety_bin_test)
  
  count = count + 1
  accuracyList_nb_bin[count] <- accuracy(nb_bin_pred, vehicleSafety_bin_test$OA_MAIS)
  print(count)
}


# export conditional probability
params_nb_bin <- params(nb_bin)
setwd("C:\\Users\\chee.jw\\Dropbox\\NUS\\Semester 3\\KE 5107\\CA\\CA 3\\Conditional Probability Output\\With Categorization\\Naive Bayes")
count = 1
list_params_nb_bin <- list()
while (count <= 21)
{
  list_params_nb_bin[[count]] <- data.frame(params_nb_bin[count])
  fileNameX <- paste(colnames(list_params_nb_bin[[count]])[1], ".xlsx", sep = "")
  write.xlsx(list_params_nb_bin[[count]], fileNameX, sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
  count = count + 1
  filename <- ""
}

##### Hill-climbing tree augmented naive Bayes (TAN-HC) #####
# epsilon and smoothing parameters are derived using the iterative approach
nb_tan_hc <- tan_hc("OA_MAIS", vehicleSafety_bin_train, k = 10, epsilon = 0.0000001, smooth = 100, cache_reset = NULL)
nb_tan_hc <- lp(nb_tan_hc, vehicleSafety_bin_train, smooth = 100)
nb_tan_pred <- predict(nb_tan_hc, vehicleSafety_bin_test)
confusion_tan_hc <- table(nb_tan_pred, vehicleSafety_bin_test$OA_MAIS, dnn=c("Prediction","Actual"))
confusionMatrix(confusion_tan_hc)
accuracy(nb_tan_pred, vehicleSafety_bin_test$OA_MAIS)
plot(nb_tan_hc, layoutType = 'twopi', fontsize = 15)
plot(nb_tan_hc, layoutType = 'neato', fontsize = 15)
plot(nb_tan_hc, layoutType = 'fdp', fontsize = 15)

nb_tan_hc$.params
nb_tan_hc$.params$GV_LANES

# explore impact of epsilon and smoothing parameters on accuracy (TAN-HC)
countEpsilon = 0
countSmooth = 0
accuracyList_nb_tan_hc <- list()
while(countEpsilon < 10)
{
  myEpsilon = 1 / (10^countEpsilon)
  tempVector = c()
  while (countSmooth < 10)
  {
    mySmooth = 0.00001 * (10^countSmooth)
    nb_tan_hc <- tan_hc("OA_MAIS", vehicleSafety_bin_train, k = 10, epsilon = myEpsilon, smooth = 0, cache_reset = NULL)
    nb_tan_hc <- lp(nb_tan_hc, vehicleSafety_bin_train, smooth = mySmooth)
    nb_tan_pred <- predict(nb_tan_hc, vehicleSafety_bin_test)
    
    countSmooth = countSmooth + 1
    tempVector[countSmooth] <- accuracy(nb_tan_pred, vehicleSafety_bin_test$OA_MAIS)
  }
  
  countEpsilon = countEpsilon + 1
  accuracyList_nb_tan_hc[[countEpsilon]] <- tempVector
  countSmooth = 0
  print(countEpsilon)
}

write.xlsx(accuracyList_nb_tan_hc, "C:\\Users\\chee.jw\\Dropbox\\NUS\\Semester 3\\KE 5107\\CA\\CA 3\\accuracyList_nb_tan_hc.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)


###### Hill-climbing super-parent tree augmented naive Bayes (TAN-HCSP) #####
# epsilon and smoothing parameters are derived using the iterative approach
nb_tan_hcsp <- tan_hcsp("OA_MAIS", vehicleSafety_bin_train, k = 10, epsilon = 0.0000001, smooth = 100, cache_reset = NULL)
nb_tan_hcsp <- lp(nb_tan_hcsp, vehicleSafety_bin_train, smooth = 100)
nb_tan_hcsp_pred <- predict(nb_tan_hcsp, vehicleSafety_bin_test)
confusion_tan_hcsp <- table(nb_tan_hcsp_pred, vehicleSafety_bin_test$OA_MAIS, dnn=c("Prediction","Actual"))
confusionMatrix(confusion_tan_hcsp)
accuracy(nb_tan_hcsp_pred, vehicleSafety_bin_test$OA_MAIS)
plot(nb_tan_hcsp, layoutType = 'twopi', fontsize = 15)
plot(nb_tan_hcsp, layoutType = 'neato', fontsize = 15)
plot(nb_tan_hcsp, layoutType = 'fdp', fontsize = 15)

# explore impact of epsilon and smoothing parameters on accuracy (TAN-HCSP)
countEpsilon = 0
countSmooth = 0
accuracyList_nb_tan_hcsp <- list()
while(countEpsilon < 10)
{
  myEpsilon = 1 / (10^countEpsilon)
  tempVector = c()
  while (countSmooth < 10)
  {
    mySmooth = 0.00001 * (10^countSmooth)
    nb_tan_hcsp <- tan_hcsp("OA_MAIS", vehicleSafety_bin_train, k = 10, epsilon = myEpsilon, smooth = 0, cache_reset = NULL)
    nb_tan_hcsp <- lp(nb_tan_hcsp, vehicleSafety_bin_train, smooth = mySmooth)
    nb_tan_hcsp_pred <- predict(nb_tan_hcsp, vehicleSafety_bin_test)
    
    countSmooth = countSmooth + 1
    tempVector[countSmooth] <- accuracy(nb_tan_hcsp_pred, vehicleSafety_bin_test$OA_MAIS)
  }
  
  countEpsilon = countEpsilon + 1
  accuracyList_nb_tan_hcsp[[countEpsilon]] <- tempVector
  countSmooth = 0
  print(countEpsilon)
}

write.xlsx(accuracyList_nb_tan_hcsp, "C:\\Users\\chee.jw\\Dropbox\\NUS\\Semester 3\\KE 5107\\CA\\CA 3\\accuracyList_nb_tan_hcsp.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)


###### Forward sequential selection and joining (FSSJ) #####
nb_fssj <- fssj("OA_MAIS", vehicleSafety_bin_train, k = 10, epsilon = 0.0001, smooth = 0, cache_reset = NULL)
nb_fssj <- lp(nb_fssj, vehicleSafety_bin_train, smooth = 0.1)
nb_fssj_pred <- predict(nb_fssj, vehicleSafety_bin_test)
confusion_fssj <- table(nb_fssj_pred, vehicleSafety_bin_test$OA_MAIS, dnn=c("Prediction","Actual"))
confusionMatrix(confusion_fssj)
accuracy(nb_fssj_pred, vehicleSafety_bin_test$OA_MAIS)
plot(nb_fssj, layoutType = 'twopi', fontsize = 15)
plot(nb_fssj, layoutType = 'fdp', fontsize = 15)


###### Chow-Liu's algorithm for one-dependence estimators (CL-ODE) #####
nb_tan_cl <- tan_cl("OA_MAIS", vehicleSafety_bin_train, score = "aic")
nb_tan_cl <- lp(nb_tan_cl, vehicleSafety_bin_train, smooth = 100)
nb_tan_cl_pred <- predict(nb_tan_cl, vehicleSafety_bin_test)
confusion_tan_cl <- table(nb_tan_cl_pred, vehicleSafety_bin_test$OA_MAIS, dnn=c("Prediction","Actual"))
confusionMatrix(confusion_tan_cl)
accuracy(nb_tan_cl_pred, vehicleSafety_bin_test$OA_MAIS)
plot(nb_tan_cl, layoutType = 'twopi', fontsize = 15)
plot(nb_tan_cl, layoutType = 'fdp', fontsize = 15)
plot(nb_tan_cl, layoutType = 'neato', fontsize = 15)

# explore impact of smoothing parameter on accuracy
count = 0
accuracyList_nb_tan_cl <- list()
while(count < 10)
{
  mySmooth = 0.00001 * (10^count)
  nb_tan_cl <- tan_cl("OA_MAIS", vehicleSafety_bin_train, score = "aic")
  nb_tan_cl <- lp(nb_tan_cl, vehicleSafety_bin_train, smooth = mySmooth)
  nb_tan_cl_pred <- predict(nb_tan_cl, vehicleSafety_bin_test)
  
  count = count + 1
  accuracyList_nb_tan_cl[count] <- accuracy(nb_tan_cl_pred, vehicleSafety_bin_test$OA_MAIS)
}

plot(nb_tan_cl, layoutType = 'twopi', fontsize = 15)
families(nb_tan_cl)
params_nb_tan_cl <- params(nb_tan_cl)
nparams(nb_tan_cl)

setwd("C:\\Users\\chee.jw\\Dropbox\\NUS\\Semester 3\\KE 5107\\CA\\CA 3\\Conditional Probability Output")

count = 1
list_params_nb_tan_cl <- list()
while (count <= 21)
{
  list_params_nb_tan_cl[[count]] <- data.frame(params_nb_tan_cl[count])
  fileNameX <- paste(colnames(list_params_nb_tan_cl[[count]])[1], ".xlsx", sep = "")
  write.xlsx(list_params_nb_tan_cl[[count]], fileNameX, sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
  count = count + 1
  filename <- ""
}




