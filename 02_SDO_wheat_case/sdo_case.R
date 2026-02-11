###############################################################
## Info
## SDO, 19 Oct 2025
## Main script for the Australian wheat production case study
## Performs SDO-based spatial prediction and model validation
## Generates:
##   - Figure 5: Initial wheat production distribution (refined in ArcGIS)
##   - Figure 8: Correlation analysis
##   - Figure 9: Spatial distribution of AT SDO variable
##   - Figure 10: Spatial distribution of TP, NDVI, and SND SDO variables
##   - Figure 12(b)(c): Variable importance plots for individual predictors
##   - Exports CSVs for Aspatial SVM and SDO-SVM predictions
##   - Computes R², RMSE, and MAE for seven ML models (Table 2)
###############################################################

## setwd to current directory
script_file_path <- rstudioapi::getSourceEditorContext()$path
script_dir <- dirname(script_file_path)
setwd(script_dir)

source("generate_sdo_var.R")

## data and packages
load("wheat.rda")

library(reshape2)
library(ggplot2)
library(corrplot)
library(SecDim)
library(caret)
library(gridExtra)
library(glmnet)
library(randomForest)
library(class)
library(gbm)
library(e1071)
library(nnet)
library(xgboost)
library(Cubist)
library(sf)
library(data.table)

## Figure 5  ----  plot wheat data, then use ArcGIS output the shapefile map
ggplot(dt_points, aes(x = x, y = y, color = WP)) +
  geom_point(size = 1) +
  scale_color_distiller(palette = "RdBu", direction = -1) +  
  coord_fixed() +
  theme_test() 

# Figure 8 ---- Calc correlation coefficient include wind speed
m1 <- cor(dt_points[,3:13])
corrplot(m1, method = 'square', addCoef.col = 'black', tl.pos = 'd',
         cl.pos = 'r', col = COL2('RdYlBu'))

# Remove wind speed (WS)
dt_points <- dt_points[,-9] # drop WS
dt_grids <- dt_grids[-9]

###############################################################
## 1. generate SDO variables
pointlocation <- dt_points[, c("x", "y")] # x at sample locations or x at prediction locations
gridlocation <- dt_grids[, c("x", "y")] # x grids aspatial data

## generate SDO variables for samples
system.time({
  sdovars <- list()
  for (i in 1:9){
    vi <- generate_sdo_var(pointlocation, gridlocation, dt_grids[, i+3],
                           distbuf = seq(100000, 700000, 100000))
    sdovars[[i]] <- vi
  }
  names(sdovars) <- names(dt_grids)[4:12]
})


## Figure 9 & Figure 10 ----  plot 9 groups of figures for SDO variables
state_shp_path <- "STE_AUS_shape/STE_Aus_pro.shp"
state_shp <- st_read(state_shp_path)

# plot SDO-AT variables
spdf_melted <- cbind(dt_points[,c("x", "y")], sdovars[[1]]) 
spdf_melted <- melt(as.data.table(spdf_melted), id.vars = c("x","y"))
spdf_melted_sf <- st_as_sf(spdf_melted, coords = c("x","y"), crs = 3577, remove = FALSE)
p1 <- ggplot() +
  geom_sf(data = state_shp, fill = NA, color = "gray60", size = 0.3) +  # plot boundary
  geom_sf(data = spdf_melted_sf, aes(color = value), size = 0.4) +      # plot points
  facet_wrap(~ variable, nrow = 2) +
  scale_color_gradient2(
    low = "blue", mid = "lightgray", high = "red",
    midpoint = 0, name = NULL
  ) +
  coord_sf(crs = st_crs(3577), datum = NA) +
  theme_test() +
  ggtitle("AT") +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14)
  )
print(p1)
# ggsave("04_01_sdo_vars_AT.pdf", p1, width = 15, height = 5, units = "in")

# plot SDO-ETa variables
spdf_melted <- cbind(dt_points[,c("x", "y")], sdovars[[2]]) 
spdf_melted <- melt(as.data.table(spdf_melted), id.vars = c("x","y"))
spdf_melted_sf <- st_as_sf(spdf_melted, coords = c("x","y"), crs = 3577, remove = FALSE)
p2 <- ggplot() +
  geom_sf(data = state_shp, fill = NA, color = "gray60", size = 0.3) +  # plot boundary
  geom_sf(data = spdf_melted_sf, aes(color = value), size = 0.4) +      # plot points
  facet_wrap(~ variable, nrow = 2) +
  scale_color_gradient2(
    low = "blue", mid = "lightgray", high = "red",
    midpoint = 0, name = NULL
  ) +
  coord_sf(crs = st_crs(3577), datum = NA) +
  theme_test() +
  ggtitle("ETa") +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14)
  )
print(p2)
# ggsave("04_02_sdo_vars_ETa.pdf", p2, width = 15, height = 5, units = "in")

# plot SDO-EVI variables
spdf_melted <- cbind(dt_points[,c("x", "y")], sdovars[[3]]) 
spdf_melted <- melt(as.data.table(spdf_melted), id.vars = c("x","y"))
spdf_melted_sf <- st_as_sf(spdf_melted, coords = c("x","y"), crs = 3577, remove = FALSE)
p3 <- ggplot() +
  geom_sf(data = state_shp, fill = NA, color = "gray60", size = 0.3) +  # plot boundary
  geom_sf(data = spdf_melted_sf, aes(color = value), size = 0.4) +      # plot points
  facet_wrap(~ variable, nrow = 2) +
  scale_color_gradient2(
    low = "blue", mid = "lightgray", high = "red",
    midpoint = 0, name = NULL
  ) +
  coord_sf(crs = st_crs(3577), datum = NA) +
  theme_test() +
  ggtitle("EVI") +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14)
  )
print(p3)
# ggsave("04_03_sdo_vars_EVI.pdf", p3, width = 15, height = 5, units = "in")

# plot SDO-NDVI variables
spdf_melted <- cbind(dt_points[,c("x", "y")], sdovars[[4]]) 
spdf_melted <- melt(as.data.table(spdf_melted), id.vars = c("x","y"))
spdf_melted_sf <- st_as_sf(spdf_melted, coords = c("x","y"), crs = 3577, remove = FALSE)
p4 <- ggplot() +
  geom_sf(data = state_shp, fill = NA, color = "gray60", size = 0.3) +  # plot boundary
  geom_sf(data = spdf_melted_sf, aes(color = value), size = 0.4) +      # plot points
  facet_wrap(~ variable, nrow = 2) +
  scale_color_gradient2(
    low = "blue", mid = "lightgray", high = "red",
    midpoint = 0, name = NULL
  ) +
  coord_sf(crs = st_crs(3577), datum = NA) +
  theme_test() +
  ggtitle("NDVI") +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14)
  )
print(p4)
# ggsave("04_04_sdo_vars_NDVI.pdf", p4, width = 15, height = 5, units = "in")

# plot SDO-TP variables
spdf_melted <- cbind(dt_points[,c("x", "y")], sdovars[[5]]) 
spdf_melted <- melt(as.data.table(spdf_melted), id.vars = c("x","y"))
spdf_melted_sf <- st_as_sf(spdf_melted, coords = c("x","y"), crs = 3577, remove = FALSE)
p5 <- ggplot() +
  geom_sf(data = state_shp, fill = NA, color = "gray60", size = 0.3) +  # plot boundary
  geom_sf(data = spdf_melted_sf, aes(color = value), size = 0.4) +      # plot points
  facet_wrap(~ variable, nrow = 2) +
  scale_color_gradient2(
    low = "blue", mid = "lightgray", high = "red",
    midpoint = 0, name = NULL
  ) +
  coord_sf(crs = st_crs(3577), datum = NA) +
  theme_test() +
  ggtitle("TP") +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14)
  )
print(p5)
# ggsave("04_05_sdo_vars_TP.pdf", p5, width = 15, height = 5, units = "in")

# plot SDO-NTO variables
spdf_melted <- cbind(dt_points[,c("x", "y")], sdovars[[6]]) 
spdf_melted <- melt(as.data.table(spdf_melted), id.vars = c("x","y"))
spdf_melted_sf <- st_as_sf(spdf_melted, coords = c("x","y"), crs = 3577, remove = FALSE)
p6 <- ggplot() +
  geom_sf(data = state_shp, fill = NA, color = "gray60", size = 0.3) +  # plot boundary
  geom_sf(data = spdf_melted_sf, aes(color = value), size = 0.4) +      # plot points
  facet_wrap(~ variable, nrow = 2) +
  scale_color_gradient2(
    low = "blue", mid = "lightgray", high = "red",
    midpoint = 0, name = NULL
  ) +
  coord_sf(crs = st_crs(3577), datum = NA) +
  theme_test() +
  ggtitle("NTO") +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14)
  )
print(p6)
# ggsave("04_06_sdo_vars_NTO.pdf", p6, width = 15, height = 5, units = "in")

# plot SDO-PTO variables
spdf_melted <- cbind(dt_points[,c("x", "y")], sdovars[[7]]) 
spdf_melted <- melt(as.data.table(spdf_melted), id.vars = c("x","y"))
spdf_melted_sf <- st_as_sf(spdf_melted, coords = c("x","y"), crs = 3577, remove = FALSE)
p7 <- ggplot() +
  geom_sf(data = state_shp, fill = NA, color = "gray60", size = 0.3) +  # plot boundary
  geom_sf(data = spdf_melted_sf, aes(color = value), size = 0.4) +      # plot points
  facet_wrap(~ variable, nrow = 2) +
  scale_color_gradient2(
    low = "blue", mid = "lightgray", high = "red",
    midpoint = 0, name = NULL
  ) +
  coord_sf(crs = st_crs(3577), datum = NA) +
  theme_test() +
  ggtitle("PTO") +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14)
  )
print(p7)
# ggsave("04_07_sdo_vars_PTO.pdf", p7, width = 15, height = 5, units = "in")

# plot SDO-SLT variables
spdf_melted <- cbind(dt_points[,c("x", "y")], sdovars[[8]]) 
spdf_melted <- melt(as.data.table(spdf_melted), id.vars = c("x","y"))
spdf_melted_sf <- st_as_sf(spdf_melted, coords = c("x","y"), crs = 3577, remove = FALSE)
p8 <- ggplot() +
  geom_sf(data = state_shp, fill = NA, color = "gray60", size = 0.3) +  # plot boundary
  geom_sf(data = spdf_melted_sf, aes(color = value), size = 0.4) +      # plot points
  facet_wrap(~ variable, nrow = 2) +
  scale_color_gradient2(
    low = "blue", mid = "lightgray", high = "red",
    midpoint = 0, name = NULL
  ) +
  coord_sf(crs = st_crs(3577), datum = NA) +
  theme_test() +
  ggtitle("SLT") +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14)
  )
print(p8)
# ggsave("04_08_sdo_vars_SLT.pdf", p8, width = 15, height = 5, units = "in")

# plot SDO-SND variables
spdf_melted <- cbind(dt_points[,c("x", "y")], sdovars[[9]]) 
spdf_melted <- melt(as.data.table(spdf_melted), id.vars = c("x","y"))
spdf_melted_sf <- st_as_sf(spdf_melted, coords = c("x","y"), crs = 3577, remove = FALSE)
p9 <- ggplot() +
  geom_sf(data = state_shp, fill = NA, color = "gray60", size = 0.3) +  # plot boundary
  geom_sf(data = spdf_melted_sf, aes(color = value), size = 0.4) +      # plot points
  facet_wrap(~ variable, nrow = 2) +
  scale_color_gradient2(
    low = "blue", mid = "lightgray", high = "red",
    midpoint = 0, name = NULL
  ) +
  coord_sf(crs = st_crs(3577), datum = NA) +
  theme_test() +
  ggtitle("SND") +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14)
  )
print(p9)
# ggsave("04_09_sdo_vars_SND.pdf", p9, width = 15, height = 5, units = "in")


###############################################################


## rawdata for modelling using original data; sdodata for SDO modelling
rawdata <- dt_points[, -c(1,2)] 
sdodata <- cbind(dt_points[, -c(1,2)], do.call(cbind, sdovars))

## modelling and cross validation
set.seed(123)
train.kfold <- trainControl(method = "cv", number = 5)

## generate SDO variables for unknown locations
pointlocation <- dt_grids[, c("x", "y")]
system.time({
  sdovars <- list()
  for (i in 1:9){
    vi <- generate_sdo_var(pointlocation, gridlocation, dt_grids[, i+3],
                           distbuf = seq(100000, 700000, 100000))
    sdovars[[i]] <- vi
  }
  names(sdovars) <- names(dt_grids)[4:12]
})

sdodata_unknown <- cbind(dt_grids[, -c(1,2)], do.call(cbind, sdovars))

# predict #####################

## SVM
set.seed(123)
predmodel.aspatial.svm <- train(WP ~ ., data = rawdata, method = "svmRadial")
predvalue.aspatial.svm <- predict(predmodel.aspatial.svm, newdata = dt_grids)

set.seed(123)
predmodel.sdo.svm <- train(WP ~ ., data = sdodata, method = "svmRadial")
predvalue.sdo.svm <- predict(predmodel.sdo.svm, newdata = sdodata_unknown)

out_pred <- cbind(gridlocation, predvalue.aspatial.svm,  predvalue.sdo.svm)
colnames(out_pred) <- c("x", "y", "SVM.Aspatial", "SVM.SDO")
# Figure 12 (a)(c)  ----  Results of RF predictions based asaptial model and SDO model
out_pred_melt <- melt(as.data.table(out_pred), id.vars = c("x", "y"))
ggplot(out_pred_melt, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  facet_wrap(~ variable, nrow = 2) +
  scale_fill_distiller(palette = "RdBu", direction = -1) +  
  coord_fixed() +
  theme_test() 
# write the prediction result of SVM to .csv file
# write.csv(out_pred, "predvalue_svm.csv")


# Compare seven machine learning models based aspatial model and SDO model

# Table 2  ----  Statistics of R2, RMSE, and MAE of 7 ML models
## 01. RF
set.seed(123)
kfold.aspatial.rf <- train(WP ~ .,
                           data = rawdata,
                           method = "rf",
                           trControl = train.kfold)
print(kfold.aspatial.rf)

set.seed(123)
kfold.sdo.rf <- train(WP ~ .,
                      data = sdodata,
                      method = "rf",
                      trControl = train.kfold)
print(kfold.sdo.rf)


## 02. Cubist
set.seed(123)
kfold.aspatial.cubist <- train(WP ~ .,
                               data = rawdata,
                               method = "cubist",
                               trControl = train.kfold)
print(kfold.aspatial.cubist)

set.seed(123)
kfold.sdo.cubist <- train(WP ~ .,
                          data = sdodata,
                          method = "cubist",
                          trControl = train.kfold)
print(kfold.sdo.cubist)

## 03. XGBoost
set.seed(123)
kfold.aspatial.xgb <- train(WP ~ .,
                            data = rawdata,
                            method = "xgbTree",
                            trControl = train.kfold,
                            verbosity = 0)
print(kfold.aspatial.xgb)

set.seed(123)
kfold.sdo.xgb <- train(WP ~ .,
                       data = sdodata,
                       method = "xgbTree",
                       trControl = train.kfold,
                       verbosity = 0)
print(kfold.sdo.xgb)

## 04. Support Vector Machine, SVM
set.seed(123)
kfold.aspatial.svm <- train(WP ~ .,
                            data = rawdata,
                            method = "svmRadial",
                            trControl = train.kfold)
print(kfold.aspatial.svm)
importance <- varImp(kfold.aspatial.svm, scale = FALSE)
imp_data <- importance$importance
imp_data$Variable <- rownames(imp_data)
# Figure 12 (b)  ----  Aspatial SVM importance
ggplot(imp_data, aes(x = reorder(Variable, Overall, FUN = max), y = Overall)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "white") +
  geom_text(aes(label = round(Overall, 2)), 
            vjust = 0.5, 
            hjust = -0.3, 
            color = "black", 
            size = 5) + 
  theme_classic() +
  theme(axis.text.x = element_text()) + 
  labs(x = "Variable", y = "Importance", title = "Variable Importance") + 
  coord_flip() 
sort(imp_data$Overall, decreasing = T)
# ggsave("05_01_kfold_raw_SVM_importance.pdf", width = 8, height = 6, units = "in")


set.seed(123)
kfold.sdo.svm <- train(WP ~ .,
                       data = sdodata,
                       method = "svmRadial",
                       trControl = train.kfold)
print(kfold.sdo.svm)
importance <- varImp(kfold.sdo.svm, scale = FALSE)
imp_data <- importance$importance
imp_data$Variable <- rownames(imp_data)
top30_imp_data <- imp_data[order(imp_data$Overall, decreasing = TRUE), ][1:30, ]
# Figure 12 (d)  ----  SDO SVM importance
ggplot(top30_imp_data, aes(x = reorder(Variable, Overall, FUN = max), y = Overall)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "white") +
  geom_text(aes(label = round(Overall, 2)), 
            vjust = 0.5, 
            hjust = -0.3, 
            color = "black", 
            size = 5) + 
  theme_classic() +
  theme(axis.text.x = element_text()) + 
  labs(x = "Variable", y = "Importance", title = "Variable Importance") + 
  coord_flip() 
sort(top30_imp_data$Overall, decreasing = T)
# ggsave("05_02_kfold_sdo_SVM_importance_30.pdf", width = 8, height = 10, units = "in")


## 05. Gradient Boosting Machine, GBM
set.seed(123)
kfold.aspatial.gbm <- train(WP ~ .,
                            data = rawdata,
                            method = "gbm",
                            trControl = train.kfold,
                            verbose = FALSE)
print(kfold.aspatial.gbm)

set.seed(123)
kfold.sdo.gbm <- train(WP ~ .,
                       data = sdodata,
                       method = "gbm",
                       trControl = train.kfold,
                       verbose = FALSE)
print(kfold.sdo.gbm)

## 06. K-Nearest Neighbors, KNN
set.seed(123)
kfold.aspatial.knn <- train(WP ~ .,
                            data = rawdata,
                            method = "knn",
                            trControl = train.kfold)
print(kfold.aspatial.knn)

set.seed(123)
kfold.sdo.knn <- train(WP ~ .,
                       data = sdodata,
                       method = "knn",
                       trControl = train.kfold)
print(kfold.sdo.knn)

# 07. Elastic Net Regression, ENR
set.seed(123)
kfold.aspatial.enet <- train(WP ~ .,
                             data = rawdata,
                             method = "glmnet",
                             trControl = train.kfold)
print(kfold.aspatial.enet)

set.seed(123)
kfold.sdo.enet <- train(WP ~ .,
                        data = sdodata,
                        method = "glmnet",
                        trControl = train.kfold)
print(kfold.sdo.enet)


