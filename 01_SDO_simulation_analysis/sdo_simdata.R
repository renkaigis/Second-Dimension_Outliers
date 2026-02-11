###############################################################
## Info
## SDO, 19 Oct 2025
## Core script for SDO model simulation and validation
## Performs spatial prediction, model evaluation, and visualization
## Used to generate Figures 2, 3, and 4 in the manuscript
###############################################################

## setwd to current directory
script_file_path <- rstudioapi::getSourceEditorContext()$path
script_dir <- dirname(script_file_path)
setwd(script_dir)

source("generate_sdo_var.R")

## data and packages
load("dt_sim.rda")

library(ggplot2)
library(corrplot)
library(SecDim)
library(caret)
library(reshape2)
library(cowplot)

## Figure 2 (a)  --- plot sim data Y
ggplot(dt_points, aes(x = lo, y = la, color = y)) +
  geom_point(size = 2) +
  scale_color_distiller(palette = "RdYlBu", direction = -1) +  
  coord_fixed() +
  theme_test() 

## Figure 2 (b)  --- plot sim data X
sim_melted <- melt(dt_points[,-3], id.vars = c("lo", "la"))
ggplot(sim_melted, aes(x = lo, y = la, color = value)) +
  geom_point() +
  facet_wrap(~ variable) +
  scale_color_distiller(palette = "PuOr", direction = -1) +  
  coord_fixed() +
  theme_test() 

## Figure 2 (c)  --- plot correlation analysis of sim data 
m <- cor(dt_points[,3:7])
corrplot(m, method = 'square', addCoef.col = 'black', tl.pos = 'd',
         cl.pos = 'n', col = COL2('BrBG'))

## generate SDO variables
pointlocation <- dt_points[, c("lo", "la")]
gridlocation <- dt_grids[, c("lo", "la")]

## generate SDO variables for samples
system.time({
  sdovars <- list()
  for (i in 1:4){
    vi <- generate_sdo_var(pointlocation, gridlocation, dt_grids[, i+3],
                           distbuf = seq(2, 7, 1))
    sdovars[[i]] <- vi
  }
  names(sdovars) <- names(dt_grids)[4:7]
})

## Figure 3  ----   plot the distribution of SDO_X
spdf_melted <- cbind(dt_points[,1:2], sdovars[[1]])
spdf_melted <- melt(spdf_melted, id.vars = c("lo", "la"))

ggplot(spdf_melted, aes(x = lo, y = la, color = value)) +
  geom_point(size = 1) +
  facet_wrap(~ variable, nrow = 2) +
  scale_color_gradient2(low = "blue", mid = "lightgray", high = "red", 
                        midpoint = 0) +  
  coord_fixed() +
  theme_test() +
  labs(color = expression(SDO.x[1]))

spdf_melted <- cbind(dt_points[,1:2], sdovars[[2]])
spdf_melted <- melt(spdf_melted, id.vars = c("lo", "la"))

ggplot(spdf_melted, aes(x = lo, y = la, color = value)) +
  geom_point(size = 1) +
  facet_wrap(~ variable, nrow = 2) +
  scale_color_gradient2(low = "blue", mid = "lightgray", high = "red", 
                        midpoint = 0) +  
  coord_fixed() +
  theme_test() +
  labs(color = expression(SDO.x[2]))

spdf_melted <- cbind(dt_points[,1:2], sdovars[[3]])
spdf_melted <- melt(spdf_melted, id.vars = c("lo", "la"))

ggplot(spdf_melted, aes(x = lo, y = la, color = value)) +
  geom_point(size = 1) +
  facet_wrap(~ variable, nrow = 2) +
  scale_color_gradient2(low = "blue", mid = "lightgray", high = "red", 
                        midpoint = 0) +  
  coord_fixed() +
  theme_test() +
  labs(color = expression(SDO.x[3]))

spdf_melted <- cbind(dt_points[,1:2], sdovars[[4]])
spdf_melted <- melt(spdf_melted, id.vars = c("lo", "la"))

ggplot(spdf_melted, aes(x = lo, y = la, color = value)) +
  geom_point(size = 1) +
  facet_wrap(~ variable, nrow = 2) +
  scale_color_gradient2(low = "blue", mid = "lightgray", high = "red", 
                        midpoint = 0) +  
  coord_fixed() +
  theme_test() +
  labs(color = expression(SDO.x[4]))


## rawdata for modelling using original data; sdodata for SDO modelling
rawdata <- dt_points[, -c(1,2)]
sdodata <- cbind(dt_points[, -c(1,2)], do.call(cbind, sdovars))

## modelling and cross validation
set.seed(123)
train.kfold <- trainControl(method = "cv", number = 5)

set.seed(123)
kfold.raw.rf <- train(y ~.,
                      data = rawdata,
                      method = "rf",
                      trControl = train.kfold)
print(kfold.raw.rf)

set.seed(123)
kfold.raw.svm <- train(y ~.,
                     data = rawdata,
                     method = "svmRadial",
                     trControl = train.kfold)
print(kfold.raw.svm)


set.seed(123)
kfold.sdo.rf <- train(y ~.,
                        data = sdodata,
                        method = "rf",
                        trControl = train.kfold)
print(kfold.sdo.rf)


set.seed(123)
kfold.sdo.svm <- train(y ~.,
                        data = sdodata,
                        method = "svmRadial",
                        trControl = train.kfold)
print(kfold.sdo.svm)

## Figure 4 (c)   ----  importance
importance <- varImp(kfold.raw.rf, scale = FALSE) # Aspatial RF
plot(importance)
sort(importance$importance$Overall, decreasing = TRUE)

importance <- varImp(kfold.sdo.rf, scale = FALSE) # SDO RF
plot(importance)
sort(importance$importance$Overall, decreasing = TRUE)

## predict at unknown locations
pointlocation <- dt_grids[, c("lo", "la")] # x at sample locations or x at prediction locations
gridlocation <- dt_grids[, c("lo", "la")] # x grids raw data

## generate SDO variables for unknown locations
system.time({
  sdovars <- list()
  for (i in 1:4){
    vi <- generate_sdo_var(pointlocation, gridlocation, dt_grids[, i+3],
                           distbuf = seq(2, 7, 1))
    sdovars[[i]] <- vi
  }
  names(sdovars) <- names(dt_grids)[4:7]
})

sdodata_unknown <- cbind(dt_grids[, -c(1,2)], do.call(cbind, sdovars))

# predict
set.seed(123)
predmodel.raw.rf <- train(y ~ ., data = rawdata, method = "rf")
predvalue.raw.rf <- predict(predmodel.raw.rf, newdata = dt_grids)

set.seed(123)
predmodel.raw.svm <- train(y ~ ., data = rawdata, method = "svmRadial", )
predvalue.raw.svm <- predict(predmodel.raw.svm, newdata = dt_grids)

set.seed(123)
predmodel.sdo.rf <- train(y ~ ., data = sdodata, method = "rf")
predvalue.sdo.rf <- predict(predmodel.sdo.rf, newdata = sdodata_unknown)

set.seed(123)
predmodel.sdo.svm <- train(y ~ ., data = sdodata, method = "svmRadial")
predvalue.sdo.svm <- predict(predmodel.sdo.svm, newdata = sdodata_unknown)

# Figure 4 (a) (b)  ----  plot the results of predictions
out_pred <- cbind(gridlocation, predvalue.raw.rf, predvalue.raw.svm, predvalue.sdo.rf, predvalue.sdo.svm)
out_pred_melt <- melt(out_pred, id.vars = c("lo", "la"))

p1 <- ggplot(subset(out_pred_melt, variable == "predvalue.raw.rf"), aes(x = lo, y = la, fill = value)) +
  geom_tile() +
  scale_fill_distiller(palette = "RdBu", direction = -1) +
  coord_fixed() +
  theme_test() +
  theme(legend.position = "right") + 
  labs(title = "Aspatial RF")
p2 <- ggplot(subset(out_pred_melt, variable == "predvalue.raw.svm"), aes(x = lo, y = la, fill = value)) +
  geom_tile() +
  scale_fill_distiller(palette = "RdBu", direction = -1) +
  coord_fixed() +
  theme_test() +
  theme(legend.position = "right") + 
  labs(title = "Aspatial SVM")
p3 <- ggplot(subset(out_pred_melt, variable == "predvalue.sdo.rf"), aes(x = lo, y = la, fill = value)) +
  geom_tile() +
  scale_fill_distiller(palette = "RdBu", direction = -1) +
  coord_fixed() +
  theme_test() +
  theme(legend.position = "right") + 
  labs(title = "SDO-RF")
p4 <- ggplot(subset(out_pred_melt, variable == "predvalue.sdo.svm"), aes(x = lo, y = la, fill = value)) +
  geom_tile() +
  scale_fill_distiller(palette = "RdBu", direction = -1) +
  coord_fixed() +
  theme_test() +
  theme(legend.position = "right") + 
  labs(title = "SDO-SVM")

plot_grid(p1, p2, p3, p4, nrow = 2)

# plot 
yy <- data.frame("y" = dt_grids$y, predvalue.raw.rf, predvalue.raw.svm, predvalue.sdo.rf, predvalue.sdo.svm)
yy_melt <- melt(yy, id.vars = c("y"))

p1 <- ggplot(subset(yy_melt, variable == "predvalue.raw.rf"), aes(x = y, y = value)) +
  geom_point(color = "#00BFC4") +
  geom_smooth(method = "lm", color="red", fill = "#E79A94") +
  scale_x_continuous(limits = c(3,8.5), name = "Actual Y values") +
  scale_y_continuous(limits = c(3,8.5), name = "Predicted Values") +
  geom_abline(slope = 1, intercept = 0, color = "black") + 
  coord_fixed() +
  theme_test() +
  labs(title = "Aspatial RF")
p2 <- ggplot(subset(yy_melt, variable == "predvalue.raw.svm"), aes(x = y, y = value)) +
  geom_point(color = "#00BFC4") +
  geom_smooth(method = "lm", color="red", fill = "#E79A94") +
  scale_x_continuous(limits = c(3,8.5), name = "Actual Y values") +
  scale_y_continuous(limits = c(3,8.5), name = "Predicted Values") +
  geom_abline(slope = 1, intercept = 0, color = "black") + 
  coord_fixed() +
  theme_test() +
  labs(title = "Aspatial SVM")
p3 <- ggplot(subset(yy_melt, variable == "predvalue.sdo.rf"), aes(x = y, y = value)) +
  geom_point(color = "#00BFC4") +
  geom_smooth(method = "lm", color="red", fill = "#E79A94") +
  scale_x_continuous(limits = c(3,8.5), name = "Actual Y values") +
  scale_y_continuous(limits = c(3,8.5), name = "Predicted Values") +
  geom_abline(slope = 1, intercept = 0, color = "black") + 
  coord_fixed() +
  theme_test() +
  labs(title = "SDO-RF")
p4 <- ggplot(subset(yy_melt, variable == "predvalue.sdo.svm"), aes(x = y, y = value)) +
  geom_point(color = "#00BFC4") +
  geom_smooth(method = "lm", color="red", fill = "#E79A94") +
  scale_x_continuous(limits = c(3,8.5), name = "Actual Y values") +
  scale_y_continuous(limits = c(3,8.5), name = "Predicted Values") +
  geom_abline(slope = 1, intercept = 0, color = "black") + 
  coord_fixed() +
  theme_test() +
  labs(title = "SDO-SVM")

plot_grid(p1, p2, p3, p4, nrow = 2)



