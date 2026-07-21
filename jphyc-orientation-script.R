##### Load packages
library(ggplot2)
library(car)

##### Set working directory and import data
data <- read.csv("jphyc-orientation-data.csv", header = TRUE)

##### Clean up factors
data$Age <- factor(data$Age, levels = c("Mature", "Juvenile"))
data$Orientation <- factor(data$Orientation, levels = c("Lengthwise", "Crosswise"))

##### Remove rows with missing values
data  <- data %>% filter(!is.na(maximum_load))
data <- data %>% filter(!is.na(Angle))

##### Raw maximum load
p1 <- ggplot(data, aes(x = Age, y = maximum_load, fill = Orientation)) +
  geom_boxplot(position = position_dodge(width = 0.75), width = 0.65) +
  labs(x = "Life stage", y = "Force to tear (N)") +
  my_theme

p1

##### Log maximum load
p2 <- ggplot(data, aes(x = Age, y = log_maximum_load, fill = Orientation)) +
  geom_boxplot(position = position_dodge(width = 0.75), width = 0.65) +
  labs(x = "Life stage", y = "log(Force to tear)") +
  my_theme

p2

##### Raw angle

p3 <- ggplot(data, aes(x = Age, y = Angle, fill = Orientation)) +
  geom_boxplot(position = position_dodge(width = 0.75), width = 0.65) +
  labs(x = "Life stage", y = "Angle of deflection (degrees)") +
  my_theme

p3

##### ANOVA's (Two-way with interaction)

##### Raw maximum load
mod_load_raw <- aov(maximum_load ~ Age * Orientation, data = data)
summary(mod_load_raw)
TukeyHSD(mod_load_raw)
##### Levene's tests on response grouped by the 4 treatment combinations
leveneTest(maximum_load ~ interaction(Age, Orientation), data = data)
##### Shapiro-Wilk tests on model residuals
shapiro.test(residuals(mod_load_raw))

##### Log maximum load
mod_load_log <- aov(log_maximum_load ~ Age * Orientation, data = data)
summary(mod_load_log)
TukeyHSD(mod_load_log)
##### Levene's tests on response grouped by the 4 treatment combinations
leveneTest(log_maximum_load ~ interaction(Age, Orientation), data = data)
##### Shapiro-Wilk tests on model residuals
shapiro.test(residuals(mod_load_log))

###### Diagnostic plots

par(mfrow = c(2, 2))
plot(mod_load_raw)

par(mfrow = c(2, 2))
plot(mod_load_log)

par(mfrow = c(2, 2))
plot(mod_angle_raw)

par(mfrow = c(2, 2))
plot(mod_angle_log)

par(mfrow = c(1, 1))

###### Thickness
p4 <- ggplot(data, aes(x = Age, y = thickness_mm, fill = Orientation)) +
  geom_boxplot(position = position_dodge(width = 0.75), width = 0.65) +
  labs(x = "Life stage", y = "Thickness (mm)") +
  my_theme

p4

mod_thick <- aov(thickness_mm ~ Age * Orientation, data = data)
summary(mod_thick)
TukeyHSD(mod_thick)

leveneTest(thickness_mm ~ interaction(Age, Orientation), data = data)
shapiro.test(residuals(mod_thick))



























