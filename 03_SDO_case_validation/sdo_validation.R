###############################################################
## Info
## SDO, 19 Oct 2025
## Model validation and visualization for the SDO model
## Used to generate Figures 13 and 14 in the manuscript
###############################################################


## setwd to current directory
script_file_path <- rstudioapi::getSourceEditorContext()$path
script_dir <- dirname(script_file_path)
setwd(script_dir)

library(ggplot2)
library(tidyr)
library(gridExtra)

# Figure 13 (b)(c)  ----  plot the SDO and aspatial predictions for cross sections
cross_data1 <- read.csv("cross_sectionA_points.csv")

data_long1 <- pivot_longer(cross_data1, cols = c(SVM.SDO, SVM.Aspatial), 
                          names_to = "Model", values_to = "Value")

p1 <- ggplot(data_long1, aes(x = x, y = Value, color = Model)) +
  geom_line(linewidth = 1) +
  labs(x = "Longitude", y = "Value", title = "Comparison of SVM.SDO and SVM.Aspatial") +
  theme_test() +
  scale_color_manual(values = c("SVM.SDO" = "#d62728", "SVM.Aspatial" = "#1f77b4"))

cross_data2 <- read.csv("cross_sectionB_points.csv")

data_long2 <- pivot_longer(cross_data2, cols = c(SVM.SDO, SVM.Aspatial), 
                           names_to = "Model", values_to = "Value")

p2 <- ggplot(data_long2, aes(x = x, y = Value, color = Model)) +
  geom_line(linewidth = 1) +
  labs(x = "Longitude", y = "Value", title = "Comparison of SVM.SDO and SVM.Aspatial") +
  theme_test() +
  scale_color_manual(values = c("SVM.SDO" = "#d62728", "SVM.Aspatial" = "#1f77b4"))

p <- grid.arrange(p1, p2, ncol =2)
# ggsave("Fig-13-cross_section.pdf", p, width = 12, height = 5, units = "in")s



##########################################################

# Figure 14  ----  Density map in Australia and each state
library(plyr)
library(dplyr)

# plot the resutls in whole Australia
d1 <- read.csv("predvalue_svm.csv")
data_long <- pivot_longer(d1, cols = c(SVM.SDO, SVM.Aspatial), 
                          names_to = "Model", values_to = "Value") 
mu <- ddply(data_long, "Model", summarise, grp.mean=mean(Value))
x_min <- quantile(data_long$Value, 0.05)  # Top 5%
x_max <- quantile(data_long$Value, 0.95)  #Bottom 5%
density_sdo <- density(data_long$Value[data_long$Model == "SVM.SDO"])
density_Aspatial <- density(data_long$Value[data_long$Model == "SVM.Aspatial"])
density_data <- data.frame(
  x = density_sdo$x,
  y_sdo = density_sdo$y,
  y_Aspatial = approx(density_Aspatial$x, density_Aspatial$y, xout = density_sdo$x)$y 
)
p1 <- ggplot(data_long, aes(x = Value)) +
  geom_line(data = density_data, aes(x = x, y = y_sdo, color = "SVM.SDO"), size = 1) +
  geom_line(data = density_data, aes(x = x, y = y_Aspatial, color = "SVM.Aspatial"), size = 1) +
  geom_ribbon(data = density_data %>% filter(x < x_min),
              aes(x = x, ymin = pmin(y_sdo, y_Aspatial), ymax = pmax(y_sdo, y_Aspatial)),
              fill = "green", alpha = 0.2) +
  geom_ribbon(data = density_data %>% filter(x > x_max),
              aes(x = x, ymin = pmin(y_sdo, y_Aspatial), ymax = pmax(y_sdo, y_Aspatial)),
              fill = "green", alpha = 0.2) +
  geom_vline(data = mu, aes(xintercept = grp.mean, color = Model), linetype = "dashed") +
  theme_test() +
  labs(title = "Australia", x = "Value", y = "Density") +
    scale_color_manual(values = c("SVM.SDO" = "#d62728", "SVM.Aspatial" = "#1f77b4"))
print(p1)

# plot the results in New South Wales (NSW)
d2 <- read.csv("predvalue_svm_NSW.csv")
data_long <- pivot_longer(d2, cols = c(SVM.SDO, SVM.Aspatial), 
                          names_to = "Model", values_to = "Value") 
mu <- ddply(data_long, "Model", summarise, grp.mean=mean(Value))
x_min <- quantile(data_long$Value, 0.05) 
x_max <- quantile(data_long$Value, 0.95)
density_sdo <- density(data_long$Value[data_long$Model == "SVM.SDO"])
density_Aspatial <- density(data_long$Value[data_long$Model == "SVM.Aspatial"])
density_data <- data.frame(
  x = density_sdo$x,
  y_sdo = density_sdo$y,
  y_Aspatial = approx(density_Aspatial$x, density_Aspatial$y, xout = density_sdo$x)$y
)
p2 <- ggplot(data_long, aes(x = Value)) +
  geom_line(data = density_data, aes(x = x, y = y_sdo, color = "SVM.SDO"), size = 1) +
  geom_line(data = density_data, aes(x = x, y = y_Aspatial, color = "SVM.Aspatial"), size = 1) +
  geom_ribbon(data = density_data %>% filter(x < x_min),
              aes(x = x, ymin = pmin(y_sdo, y_Aspatial), ymax = pmax(y_sdo, y_Aspatial)),
              fill = "green", alpha = 0.4) +
  geom_ribbon(data = density_data %>% filter(x > x_max),
              aes(x = x, ymin = pmin(y_sdo, y_Aspatial), ymax = pmax(y_sdo, y_Aspatial)),
              fill = "green", alpha = 0.4) +
  geom_vline(data = mu, aes(xintercept = grp.mean, color = Model), linetype = "dashed") +
  theme_test() +
  labs(title = "New South Wales", x = "Value", y = "Density") +
  scale_color_manual(values = c("SVM.SDO" = "#d62728", "SVM.Aspatial" = "#1f77b4"))
print(p2)

# plot the results in Victoria (VIC)
d3 <- read.csv("predvalue_svm_VIC.csv")
data_long <- pivot_longer(d3, cols = c(SVM.SDO, SVM.Aspatial), 
                          names_to = "Model", values_to = "Value") 
mu <- ddply(data_long, "Model", summarise, grp.mean=mean(Value))
x_min <- quantile(data_long$Value, 0.05) 
x_max <- quantile(data_long$Value, 0.95)  
density_sdo <- density(data_long$Value[data_long$Model == "SVM.SDO"])
density_Aspatial <- density(data_long$Value[data_long$Model == "SVM.Aspatial"])
density_data <- data.frame(
  x = density_sdo$x,
  y_sdo = density_sdo$y,
  y_Aspatial = approx(density_Aspatial$x, density_Aspatial$y, xout = density_sdo$x)$y
)
p3 <- ggplot(data_long, aes(x = Value)) +
  geom_line(data = density_data, aes(x = x, y = y_sdo, color = "SVM.SDO"), size = 1) +
  geom_line(data = density_data, aes(x = x, y = y_Aspatial, color = "SVM.Aspatial"), size = 1) +
  geom_ribbon(data = density_data %>% filter(x < x_min),
              aes(x = x, ymin = pmin(y_sdo, y_Aspatial), ymax = pmax(y_sdo, y_Aspatial)),
              fill = "green", alpha = 0.4) +
  geom_ribbon(data = density_data %>% filter(x > x_max),
              aes(x = x, ymin = pmin(y_sdo, y_Aspatial), ymax = pmax(y_sdo, y_Aspatial)),
              fill = "green", alpha = 0.4) +
  geom_vline(data = mu, aes(xintercept = grp.mean, color = Model), linetype = "dashed") +
  theme_test() +
  labs(title = "Victoria", x = "Value", y = "Density") +
  scale_color_manual(values = c("SVM.SDO" = "#d62728", "SVM.Aspatial" = "#1f77b4"))
print(p3)


# plot the results in Queensland (QLD)
d4 <- read.csv("predvalue_svm_QLD.csv")
data_long <- pivot_longer(d4, cols = c(SVM.SDO, SVM.Aspatial), 
                          names_to = "Model", values_to = "Value") 
mu <- ddply(data_long, "Model", summarise, grp.mean=mean(Value))
x_min <- quantile(data_long$Value, 0.05) 
x_max <- quantile(data_long$Value, 0.95) 
density_sdo <- density(data_long$Value[data_long$Model == "SVM.SDO"])
density_Aspatial <- density(data_long$Value[data_long$Model == "SVM.Aspatial"])
density_data <- data.frame(
  x = density_sdo$x,
  y_sdo = density_sdo$y,
  y_Aspatial = approx(density_Aspatial$x, density_Aspatial$y, xout = density_sdo$x)$y
)
p4 <- ggplot(data_long, aes(x = Value)) +
  geom_line(data = density_data, aes(x = x, y = y_sdo, color = "SVM.SDO"), size = 1) +
  geom_line(data = density_data, aes(x = x, y = y_Aspatial, color = "SVM.Aspatial"), size = 1) +
  geom_ribbon(data = density_data %>% filter(x < x_min),
              aes(x = x, ymin = pmin(y_sdo, y_Aspatial), ymax = pmax(y_sdo, y_Aspatial)),
              fill = "green", alpha = 0.4) +
  geom_ribbon(data = density_data %>% filter(x > x_max),
              aes(x = x, ymin = pmin(y_sdo, y_Aspatial), ymax = pmax(y_sdo, y_Aspatial)),
              fill = "green", alpha = 0.4) +
  geom_vline(data = mu, aes(xintercept = grp.mean, color = Model), linetype = "dashed") +
  theme_test() +
  labs(title = "Queensland", x = "Value", y = "Density") +
  scale_color_manual(values = c("SVM.SDO" = "#d62728", "SVM.Aspatial" = "#1f77b4"))
print(p4)

# plot the results in South Australia (SA)
d5 <- read.csv("predvalue_svm_SA.csv")
data_long <- pivot_longer(d5, cols = c(SVM.SDO, SVM.Aspatial), 
                          names_to = "Model", values_to = "Value") 
mu <- ddply(data_long, "Model", summarise, grp.mean=mean(Value))
x_min <- quantile(data_long$Value, 0.05) 
x_max <- quantile(data_long$Value, 0.95) 
density_sdo <- density(data_long$Value[data_long$Model == "SVM.SDO"])
density_Aspatial <- density(data_long$Value[data_long$Model == "SVM.Aspatial"])
density_data <- data.frame(
  x = density_sdo$x,
  y_sdo = density_sdo$y,
  y_Aspatial = approx(density_Aspatial$x, density_Aspatial$y, xout = density_sdo$x)$y 
)
p5 <- ggplot(data_long, aes(x = Value)) +
  geom_line(data = density_data, aes(x = x, y = y_sdo, color = "SVM.SDO"), size = 1) +
  geom_line(data = density_data, aes(x = x, y = y_Aspatial, color = "SVM.Aspatial"), size = 1) +
  geom_ribbon(data = density_data %>% filter(x < x_min),
              aes(x = x, ymin = pmin(y_sdo, y_Aspatial), ymax = pmax(y_sdo, y_Aspatial)),
              fill = "green", alpha = 0.4) +
  geom_ribbon(data = density_data %>% filter(x > x_max),
              aes(x = x, ymin = pmin(y_sdo, y_Aspatial), ymax = pmax(y_sdo, y_Aspatial)),
              fill = "green", alpha = 0.4) +
  geom_vline(data = mu, aes(xintercept = grp.mean, color = Model), linetype = "dashed") +
  theme_test() +
  labs(title = "South Australia", x = "Value", y = "Density") +
  scale_color_manual(values = c("SVM.SDO" = "#d62728", "SVM.Aspatial" = "#1f77b4"))
print(p5)

# plot the results in Western Australia (WA)
d6 <- read.csv("predvalue_svm_WA.csv")
data_long <- pivot_longer(d6, cols = c(SVM.SDO, SVM.Aspatial), 
                          names_to = "Model", values_to = "Value") 
mu <- ddply(data_long, "Model", summarise, grp.mean=mean(Value))
x_min <- quantile(data_long$Value, 0.05)
x_max <- quantile(data_long$Value, 0.95) 
density_sdo <- density(data_long$Value[data_long$Model == "SVM.SDO"])
density_Aspatial <- density(data_long$Value[data_long$Model == "SVM.Aspatial"])
density_data <- data.frame(
  x = density_sdo$x,
  y_sdo = density_sdo$y,
  y_Aspatial = approx(density_Aspatial$x, density_Aspatial$y, xout = density_sdo$x)$y 
)
p6 <- ggplot(data_long, aes(x = Value)) +
  geom_line(data = density_data, aes(x = x, y = y_sdo, color = "SVM.SDO"), size = 1) +
  geom_line(data = density_data, aes(x = x, y = y_Aspatial, color = "SVM.Aspatial"), size = 1) +
  geom_ribbon(data = density_data %>% filter(x < x_min),
              aes(x = x, ymin = pmin(y_sdo, y_Aspatial), ymax = pmax(y_sdo, y_Aspatial)),
              fill = "green", alpha = 0.4) +
  geom_ribbon(data = density_data %>% filter(x > x_max),
              aes(x = x, ymin = pmin(y_sdo, y_Aspatial), ymax = pmax(y_sdo, y_Aspatial)),
              fill = "green", alpha = 0.4) +
  geom_vline(data = mu, aes(xintercept = grp.mean, color = Model), linetype = "dashed") +
  theme_test() +
  labs(title = "Western Australia", x = "Value", y = "Density") +
  scale_color_manual(values = c("SVM.SDO" = "#d62728", "SVM.Aspatial" = "#1f77b4"))
print(p6)

p_density <- grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 3)
# ggsave("Fig-14-density.pdf", p_density, width = 12, height = 7, units = "in")


