###############################################################
## Info
## SDO, 19 Oct 2025
## Sensitivity analysis for the SDO model
## Evaluates model robustness under varying buffer and outlier thresholds
## Generates Figure 11 in the manuscript
###############################################################


## setwd to current directory
script_file_path <- rstudioapi::getSourceEditorContext()$path
script_dir <- dirname(script_file_path)
setwd(script_dir)

source("generate_sdo_var.R")

## data and packages
load("wheat.rda")

library(ggplot2)
library(corrplot)
library(SecDim)
library(caret)

dt_points <- dt_points[, -9] # drop WS
dt_grids <- dt_grids[-9]

###############################################################
## 1. generate SDO variables

pointlocation <- dt_points[, c("x", "y")] # x at sample locations or x at prediction locations
gridlocation <- dt_grids[, c("x", "y")] # x grids aspatial data

## generate SDO variables for wheat case, default sd = 2
system.time({
  sdovars <- list()
  for (i in 1:9) {
    vi <- generate_sdo_var(pointlocation, gridlocation, dt_grids[, i + 3],
      distbuf = seq(100000, 700000, 100000)
    )
    sdovars[[i]] <- vi
  }
  names(sdovars) <- names(dt_grids)[4:12]
})

#### when you used sd = 1.5
# system.time({
#   sdovars <- list()
#   for (i in 1:9) {
#     vi <- generate_sdo_var(pointlocation, gridlocation, dt_grids[, i + 3],
#                            distbuf = seq(100000, 700000, 100000), sd = 1.5
#     )
#     sdovars[[i]] <- vi
#   }
#   names(sdovars) <- names(dt_grids)[4:12]
# })

## rawdata for modelling using original data; sdodata for SDO modelling
rawdata <- dt_points[, -c(1, 2)]
sdodata <- cbind(dt_points[, -c(1, 2)], do.call(cbind, sdovars))

## 2. Support Vector Machine, SVM
set.seed(123)
train.kfold <- trainControl(method = "cv", number = 5)
kfold.sdo.svm <- train(WP ~ .,
  data = sdodata,
  method = "svmRadial",
  trControl = train.kfold
)
# print the metric results when thredshold = 100000, interval = 1000000, and sd = 2
# To obtain other results, simply modify the corresponding parameter values below
print(kfold.sdo.svm)

# Threshold
# 1,6,1   0.3177771   2.24%
# 1,7,1   0.3108200
# 1,8,1   0.3122704   0.47%

# Interval
# 1,7,1   0.3108200   
# 1,7,2   0.3106636   -0.05%
# 1,7,3   0.3165277   1.83%

# SD, set sd = 1.5 or 2.5 when generate SDO variables, such as:
# vi <- generate_sdo_var(pointlocation, gridlocation, dt_grids[, i + 3],
#     distbuf = seq(100000, 700000, 100000), sd = 1.5) # or sd = 2.5
# 1.5   0.3191283   2.67%
# 2     0.3108200
# 2.5   0.3095818   -0.40%


##########

# 3. Plot the sensitivity analysis figures - Figure 11

library(ggplot2)
library(gridExtra)

# data
data1 <- data.frame(
  Buffer = c("1,6,1", "1,7,1", "1,8,1"),
  RMSE = c(0.3178, 0.3108, 0.3123)
)

##########
# plot Figure 11(a)
p1 <- ggplot(data1, aes(x = Buffer, y = RMSE, fill = Buffer)) +
  geom_bar(stat = "identity", color = "black", width = 0.4) +
  geom_text(aes(label = round(RMSE, 6)), vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("#8da0cb", "#fc8d62", "#8da0cb")) +
  labs(title = "RMSE Change for Different Buffer Threshold", x = "Buffer", y = "RMSE") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none", panel.grid.major.x = element_blank())

# data
data2 <- data.frame(
  Buffer = c("1,7,1", "1,7,2", "1,7,3"),
  RMSE = c(0.3108, 0.3107, 0.3165)
)


##########
# plot Figure 11(b)
p2 <- ggplot(data2, aes(x = Buffer, y = RMSE, fill = Buffer)) +
  geom_bar(stat = "identity", color = "black", width = 0.4) +
  geom_text(aes(label = round(RMSE, 6)), vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("#fc8d62", "#8da0cb", "#8da0cb")) +
  labs(title = "RMSE Change for Different Buffer Interval", x = "Buffer", y = "RMSE") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none", panel.grid.major.x = element_blank())

data3 <- data.frame(
  Threshold = c("1.5 SD", "2.0 SD", "2.5 SD"),
  RMSE      = c(0.3191, 0.3108, 0.3096)
)

##########
# Plot Figure 11(c)
p3 <- ggplot(data3, aes(x = Threshold, y = RMSE, fill = Threshold)) +
  geom_bar(stat = "identity", color = "black", width = 0.4) +
  geom_text(aes(label = round(RMSE, 6)), vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("#8da0cb", "#fc8d62", "#8da0cb")) +
  labs(title = "RMSE: Outlier Threshold", x = "Z-Score Threshold", y = "RMSE") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none", panel.grid.major.x = element_blank())

##########
# Figure 11 - (a) (b) (c)
p <- grid.arrange(p1, p2, p3, ncol = 3)
# ggsave("sensitivity-all.pdf", plot = p, width = 15, height = 4, units = "in")


##########
# change data - Plot Figure 11(c)
data3 <- data.frame(
  Label = factor(c("Delta[1]", "Delta[2]", "Delta[3]", "Delta[4]", "Delta[5]", "Delta[6]"), levels = c("Delta[1]", "Delta[2]", "Delta[3]", "Delta[4]", "Delta[5]", "Delta[6]")),
  Change = c(2.24, 0.47, -0.05, 1.83, 2.67, -0.40)
)

# plot
ggplot(data3, aes(x = Label, y = Change, fill = Change > 0)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1) + # 0 轴
  geom_hline(yintercept = c(-5, 5), linetype = "dashed", color = "gray40") + # ±5%
  coord_flip() +
  scale_fill_manual(values = c("#fc8d62", "#66c2a5")) +
  labs(title = "Percentage Change in RMSE", x = "", y = "Change (%)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none", panel.grid.major.y = element_blank()) +
  scale_x_discrete(labels = parse(text = levels(data3$Label)))
# ggsave("sensitivity_change_new.pdf", width = 5, height = 4, units = "in")


