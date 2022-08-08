# compare spatial and non-spatial cross validation and parameter tuning
rm(list = ls())
#load libraries 

# remotes::install_github("mlr-org/mlr",force = TRUE)
library(randomForest)
library(mlbench)
library(caret)
library(data.table)
library(mlr)
library(tidymodels)
library(tidyverse)
library(ranger)
library(cowplot)
library(dplyr)
library(future)
library(lgr)
library(sperrorest)
library(BBmisc)
library(ParamHelpers)
##################Spatial and non-spatial-based cross validation##########################

df <-read.csv("C:/Users/xg1m22/OneDrive - University of Southampton/Desktop/Australia_classification/new_index_new/joindata/phenological_retrival_3VIs.csv", header=TRUE)
df$year<-as.factor(substr(df$ID,6,9))
df<-na.omit(df)
# create task with spatial coordinates
task = makeClassifTask(data = as.data.frame(df[,5:33]), target='Species',coordinates = as.data.frame(points1[,3:4]))
learner.rf = makeLearner("classif.ranger", predict.type = "prob")
set.seed(123)
# getDefaultMeasure(task)
# Mean misclassification error(mmce)
# 10 spatial fold cross validation
resampling = makeResampleDesc("SpCV", iters = 10)
out = resample(learner = learner.rf, task = task,
               resampling = resampling, measures = list(mmce))
mean(out$measures.test$mmce)
# mmce.test.mean= 0.4595547

# 3 repeated 10 spatial fold cross validation
resampling = makeResampleDesc("SpRepCV", fold = 10, reps = 3)
out = resample(learner = learner.rf, task = task,
               resampling = resampling, measures = list(mmce))
mean(out$measures.test$mmce)
# mmce.test.mean= 0.3910559

# 3 repeated 10 non-spatial fold cross validation
resampling2 = makeResampleDesc("repeatedcv", folds = 10, reps = 3)
out = resample(learner = learner.rf, task = task,
               resampling = resampling2, measures = list(mmce))
mean(out$measures.test$mmce)
# mmce.test.mean=0.1957466



# Visualization of spatial and non-spatial partitions

rdesc1 = makeResampleDesc("SpRepCV", folds = 10, reps = 3)
rdesc2 = makeResampleDesc("RepCV", folds = 10, reps = 3)

r1 = resample(makeLearner("classif.ranger"), task, rdesc1, show.info = FALSE)
r2 = resample(makeLearner("classif.ranger"), task, rdesc2, show.info = FALSE)
plots = createSpatialResamplingPlots(task, list( "SpRepCV" = r1), crs = 4326, repetitions = 1,x.axis.breaks = c(110,115,120,125,130))# , y.axis.breaks = c(-3.975))

plots = createSpatialResamplingPlots(task, list( "RepCV" = r2), crs = 4326, repetitions = 1,x.axis.breaks = c(110,115,120,125,130))

plot_grid(plotlist = plots[["Plots"]], ncol = 5, nrow = 4)


###################Hyperparameter tuning#############################################
############Spatial-based Cross-Validation################
tuning_method = makeTuneControlRandom(maxit = 30)#30 models were fitted for each spatial subfold,  with random selected hyperparameters combination
ps = makeParamSet(makeIntegerParam('num.trees', lower = 10, upper = 1000),
                  makeIntegerParam('mtry', lower = 1, upper = 9))
# 10 times reample were applied for each subfold
inner = makeResampleDesc("SpCV", iters = 10)
#In total 10 spatial folds are splited and the partitioning repeated 30 times
outer = makeResampleDesc("SpRepCV", folds = 10, rep = 30)
wrapped_lrn_random = makeTuneWrapper(learner = learner.rf, 
                                     resampling = inner,
                                     par.set = ps,
                                     control = tuning_method, 
                                     show.info = TRUE,
                                     measures = list(mmce))
# speed up the process
library(parallelMap)
parallelStart(mode = "multicore", level = "`mlr`.tuneParams", cpus = 4, mc.set.seed = TRUE)
set.seed(123)

resa_RF_spatial = mlr::resample(learner = wrapped_lrn_random, 
                            task = task,
                            resampling = outer,
                            extract = getTuneResult,
                            show.info = TRUE, 
                            measures = list(mmce))


# [Tune-x] 6: num.trees=935; mtry=2
# [Tune-y] 6: mmce.test.mean=0.5732460 ; time: 2.3 min

write_rds(resa_RF_spatial, 'C:/Users/xg1m22/OneDrive - University of Southampton/Desktop/Australia_classification/tree-crop-classificatio-xr/spatial_based_RF.rds')


###### Non-Spatial Cross-Validation########
df <-read.csv("C:/Users/xg1m22/OneDrive - University of Southampton/Desktop/Australia_classification/new_index_new/joindata/phenological_retrival_3VIs.csv", header=TRUE)
df$year<-as.factor(substr(df$ID,6,9))
features <- df[, 6:33]
response <- df[, 5]
df <- cbind(response, features) %>%
  mutate(
    response = as.factor(response)
  )

# Customize RF with two parameter grid Search
customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

control <- trainControl(method="repeatedcv", number=10, repeats=30)

tunegrid <- expand.grid(.mtry=c(2,3,4,5,6,7,8), .ntree=c(100,200,300,400,500,600,700,800,900))
set.seed(123)

RF_nonspatial <- train(response~., data=df, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)


RF_nonspatial$results
summary(RF_nonspatial)
plot(RF_nonspatial)
write_rds(RF_nonspatial, 'C:/Users/xg1m22/OneDrive - University of Southampton/Desktop/Australia_classification/tree-crop-classificatio-xr/custom_non_spatial_RF.rds')

