#################################################################################
#                XGBoost -Demo Code                                             #
#                Marketing - Binary - Demo (Bank TeleMarketing Campaign)        #
#                                                                               #
#################################################################################
### Load ibraries and packages

#install.packages("caTools")
#install.packages("doParallel")
#install.packages("xgboost")
# install.packages("caret")
# install.packages("e1071")
#install.packages("fastDummies")
#devtools::install_github("liuyanguu/SHAPforxgboost")
library("SHAPforxgboost")
library(fastDummies)
library(caret)
library(e1071)
library(xgboost)
library(doParallel) 
library(caTools)
library(dplyr)


#1 Upload the Data /Users/ricardomendez/Documents/GSSG/R_&_MySQ_Demos/XGBoost/XGBoost for Business in Python and R/bank-full.csv
# This dataset is public available for research. The details are described in [Moro et al., 2011].<br> 
#   Please include this citation if you plan to use this database: <br><br>
#   
#   [Moro et al., 2011] S. Moro, R. Laureano and P. Cortez. Using Data Mining for Bank Direct Marketing:<br> 
#   An Application of the CRISP-DM Methodology.<br> 
#   In P. Novais et al. (Eds.), Proceedings of the European Simulation and Modelling Conference - <br>
#   ESM'2011, pp. 117-121, Guimarães, Portugal, October, 2011. EUROSIS.<br><br>
# 
#   Available at: [pdf] http://hdl.handle.net/1822/14838 <br>
#                 [bib] http://www3.dsi.uminho.pt/pcortez/bib/2011-esm-1.txt <br><br>
# 
# 1. Title: Bank Marketing <br>
# 
# 2. Sources<br>
#    Created by: Paulo Cortez (Univ. Minho) and Sérgio Moro (ISCTE-IUL) @ 2012<br>
#     
# 3. Past Usage:<br>
# 
#   The full dataset was described and analyzed in:<br> 
# 
#   S. Moro, R. Laureano and P. Cortez. Using Data Mining for Bank Direct Marketing: An Application of <br>
#   the CRISP-DM Methodology.<br> 
#   In P. Novais et al. (Eds.), Proceedings of the European Simulation and Modelling Conference - ESM'2011,<br> 
#   pp. 117-121, Guimarães, Portugal, October, 2011. EUROSIS.<br><br>
#   
#   There are two datasets:<br><br>
#   1) bank-full.csv with all examples, ordered by date (from May 2008 to November 2010).<br>
#   2) DataSample.csv with 10% of the examples (4521), randomly selected from bank-full.csv.<br><br>
#   
#   <b> THE ORIGINAL DATA SET HAS 45,211 OBSERVATIONS AND 17 VARIABLES; 10% WILL BE UPLOADED TO </b>
#   <b> UNDERSTAND THE DATA STRUCTURE. </b>"),


Data <- read.csv("DataSample.csv")

# Data <- read.csv("bank-full.csv", sep = ";") if requires the original data set please
# write us at info@gssg.com.co

### Data sample to work with scenarios if required ######

# DataSample <- sample_n(Data,4521) #10% random sample
# write.csv(DataSample,"DataSample.csv", row.names =FALSE)

#1.1Check Data structure
str(Data)

head(Data)

#1.2 Select only numeric values from the Data Structure (Work with dplyr)

dataset <- Data %>% select_if(is.numeric)

#1.3 Check summary statistics
summary(dataset)
plot(dataset$balance) #There are some peak values, but XGBoost will mange them corectly; same happens with the Duration variable
# find correlation
cor(dataset) # there is not correlation between variables, but XGBoost will not be affected.

#1.4 Include the dependent variable in the data set and rename it

dataset <- cbind(Data$y,dataset)
colnames(dataset)[1] <- "yes"

# 2 Split Data set into Trining and Test, use caTools Package

set.seed(1502)
split <- sample.split(dataset$yes,SplitRatio = 0.8) #Meaning: 80% of data will be TRUE, and 20% will be set to False. The TRUE values will be used to build the training set, and the FALSE values to build the TEST set.

training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

#2.1Isolate the Y variable an convert it to to numeric values

train.y <- as.numeric(as.factor(training_set$yes)) - 1 
test.y <- as.numeric(as.factor(test_set$yes)) - 1 # R cannot transform directly from char to numeric, this step is required. The (-1) is a trick to convert Yes and No into "zeros" and "ones"

#2.2 Isolate the X Variables, they have to be transformed to Matrix in R.They are integers which is ok for XGBoost

train.x <- as.matrix(training_set[,2:ncol(training_set)])
test.x <- as.matrix(test_set[,2:ncol(test_set)])

#3. Set the parameters - Check meaning of each one.

Parameters <- list (eta = 0.3,
                    max_depth = 6,
                    subsample = 1,
                    colsample_bytree = 1,
                    minchild_weight = 1,
                    gamma = 0,
                    set.seed = 1502,
                    eval_metric = "auc",
                    objective = "binary:logistic",
                    booster = "gbtree")

#4. Set Up parallel running; to increase machine efficiency. Detect cores first


detectCores()

#5 Run XGBoost

model1 <- xgboost(data = train.x,
                  label = train.y,
                  sed.seed = 1502,
                  nthread = 3,
                  nround = 100,
                  params = Parameters,
                  print_every_n = 50,
                  early_stopping_rounds = 10) #Looks like the model is overfitted.
#6 Predict with xgboost
Predictions1 <- predict(model1,newdata = test.x)#Results are not 0 or 1,
Predictions1 <- ifelse(Predictions1 > 0.5,1,0)# 0.5 is a correct aproximation value, because of the AUC curve 

#7 Evaluate the model with the confussion matrix

confusionMatrix(table(Predictions1, test.y)) #ConfusionMatrix runs as table; results show a low value for Specificity


#8. Transform the original Character variables (Job, marital, education, etc..) into Dummy variables, so the data set will be ready to work. It is easey with this package

dataset_dummy <- dummy_cols(Data, remove_first_dummy = TRUE)
dataset_dummy <- dataset_dummy[,(18:ncol(dataset_dummy))]

#9. Join all columns in the dataset to prepare the final dataset

dataset <- cbind(dataset,dataset_dummy)
dataset <- dataset %>% select (-y_yes) #another way to remove the y column; there are two y columns, and can work only with one. 

#10. #######################################################
#Run the xgboost again with the final dataset#
############################################################
# 10.1 Split Data set into Trining and Test, use caTools Package

set.seed(1502)
split <- sample.split(dataset$yes,SplitRatio = 0.8) #Meaning: 80% of data will be TRUE, and 20% will be set to False. The TRUE values will be used to build the training set, and the FALSE values to build the TEST set.

training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

#10.2 Isolate the Y variable an convert it to to numeric values

train.y <- as.numeric(as.factor(training_set$yes)) - 1 
test.y <- as.numeric(as.factor(test_set$yes)) - 1# R cannot transfor directly form char to numeric, this step is required. The (-1) is a trick to convert Yes and No into "ceros" and "ones"

#10.3.2Isolate the X Variables, they have to be transformed to Matrix in R.They are integers which is ok for XGBoost

train.x <- as.matrix(training_set[,2:ncol(training_set)])
test.x <- as.matrix(test_set[,2:ncol(test_set)])

#10.6 Run XGBoost

model2 <- xgboost(data = train.x,
                  label = train.y,
                  sed.seed = 1502,
                  nthread = 3,
                  nround = 100,
                  params = Parameters,
                  print_every_n = 50,
                  early_stopping_rounds = 10) 

#10.7 Predict again with model 2

Predictions2 <- predict(model2,newdata = test.x)#Results are not 0 or 1,
Predictions2 <- ifelse(Predictions2 > 0.5,1,0)# 0.5 is a correct aproximation value, because of the AUC curve 

confusionMatrix(table(Predictions2, test.y)) #ConfusionMatrix runs as table; results show a better value for Specificity


#######################################################
#11. START THE HYPER-PARAMETER TUNNING - Many models will be run by the function itself; It will set up and find the best parameters ###########################
#Do parallel processing 
### This code presents an error in R:
#cpu <- makeCluster(4)
#registerDoParallel(cpu)
#May use this code instead: 

N_cores <- detectCores() -1 

# create the cluster for caret to use
#cl <- makePSOCKcluster(no_cores)
cl <- parallel::makeCluster(N_cores, setup_strategy = "sequential")
registerDoParallel(cl)

#12. State in parameters

Y <- as.factor(as.numeric(as.factor(dataset$yes)) - 1) 
X <- as.matrix(dataset[,2:ncol(dataset)])

#13. State the crossvalidation parameters
tune_control <- trainControl( method = "cv",
                              allowParallel = TRUE,
                              number = 5)

#14 Set the parameters

tune_grid <- expand.grid(nrounds = seq(from = 50, to = 600, by = 50),
                         eta = c(0.1,0.2,0.3,0.4),
                         max_depth = seq(2,10, by = 2),
                         subsample = c(0.5, 0.7, 1),
                         colsample_bytree = 1,
                         min_child_weight = 1,
                         gamma = 0)

#15 Cross validation and parameter tuning start (It will take some time!! check It !!)

start <- Sys.time()
xgb_tune <- train(x = X,
                  y = Y,
                  method = "xgbTree",
                  trControl = tune_control,
                  tuneGrid = tune_grid)
end <- Sys.time()

#16 Check for the best parameters
xgb_tune$bestTune
View(xgb_tune$results)

#################################################################
# 17. HYPER-PARAMETER TUNNING (2 round)##
#It´s possible to do as many rounds as needed##
#################################################################
#Do parallel processing 

N_cores <- detectCores()
cl <- parallel::makeCluster(N_cores, setup_strategy = "sequential")
registerDoParallel(cl)

#17.1 Set the parameters

tune_grid2 <- expand.grid(nrounds = seq(from = 50, to = 600, by = 50),
                          eta = xgb_tune$bestTune$eta,
                          max_depth = xgb_tune$bestTune$max_depth,
                          subsample = xgb_tune$bestTune$subsample,
                          colsample_bytree = c(0.5,0.7,1),
                          min_child_weight = seq(1,6,by = 2),
                          gamma = c(0,0.05,0.1,0.15))

#17.2 Cross validation and parameter tuning start (It will take some time!! check It !!)

start <- Sys.time()
xgb_tune2 <- train(x = X,
                   y = Y,
                   method = "xgbTree",
                   trControl = tune_control,
                   tuneGrid = tune_grid2)
end <- Sys.time()

# Check for the best parameters
xgb_tune2$bestTune
View(xgb_tune2$results)


#17.3 Thirth round ( Run XGBoost for the last time; might do it several times up to your best accuracy)

#17.4 Set parameters 3

Parameters3 <- list (eta = xgb_tune2$bestTune$eta,
                     max_depth = xgb_tune2$bestTune$max_depth,
                     subsample = xgb_tune2$bestTune$subsample,
                     colsample_bytree = xgb_tune2$bestTune$colsample_bytree,
                     minchild_weight = xgb_tune2$bestTune$min_child_weight,
                     gamma = xgb_tune2$bestTune$gamma,
                     set.seed = 1502,
                     eval_metric = "auc",
                     objective = "binary:logistic",
                     booster = "gbtree")

#17.5 Run XGBoost for the model 3

model3 <- xgboost(data = train.x,
                  label = train.y,
                  sed.seed = 1502,
                  nthread = 4,
                  nround = xgb_tune2$bestTune$nrounds,
                  params = Parameters3,
                  print_every_n = 50,
                  early_stopping_rounds = 10)

#17.6 Predictions part 3

Predictions3 <- predict(model3,newdata = test.x)#Results are not 0 or 1,

Predictions3 <- ifelse(Predictions3 > 0.05,1,0)

#0.05 may be a correct aproximation value, because of the AUC curve 

#17.8 Cheking Accuracy

cm3 <- confusionMatrix(table(Predictions3, test.y)) #ConfusionMatrix runs as table; results show a better value for Specificity    

####################################################################
#18 Important drivers ### Most important business value conclusion #
####################################################################



#18.2To return the SHAP values and ranked features by mean|SHAP|
shap_values <- shap.values(xgb_model = model3, X_train = test.x)

# The ranked features by mean |SHAP|
shap_values$mean_shap_score

# To prepare the long-format data:
shap_long <- shap.prep(xgb_model = model3, X_train = test.x)

#18.3 **SHAP summary plot**
shap.plot.summary(shap_long)
shap.plot.summary(shap_long, x_bound = 1.2, dilute = 10)

# Alternatives options to make the same plot:
# option 1: from the xgboost model
shap.plot.summary.wrap1(model3, X = as.matrix(test.x))

# option 2: supply a self-made SHAP values dataset (e.g. sometimes as output from cross-validation)
shap.plot.summary.wrap2(shap_values$shap_score, as.matrix(test.x))

#18.4 **SHAP dependence plot**

# prepare the data using either: 
# (this step is slow since it calculates all the combinations of features.)
data_int <- shap.prep.interaction(xgb_mod = model3, X_train = as.matrix(test.x))
shap.plot.dependence(data_long = shap_long, x= "balance",
                     y = "default_yes", color_feature = "default_yes")

# without color version but has marginal distribution, just plot SHAP value against feature value:
shap.plot.dependence(data_long = shap_long, "default_yes") 

##18.7*** SHAP FORCE PLOT ***#####

# choose to show top 4 features by setting `top_n = 4`, set 6 clustering groups.
plot_data <- shap.prep.stack.data(shap_contrib = shap_values$shap_score, top_n = 6, n_groups = 6)

# choose to zoom in at location 500, set y-axis limit using `y_parent_limit`
# it is also possible to set y-axis limit for zoom-in part alone using `y_zoomin_limit`
shap.plot.force_plot(plot_data, zoom_in_location = 8500, y_parent_limit = c(-1,1))
shap.plot.force_plot_bygroup(plot_data)



