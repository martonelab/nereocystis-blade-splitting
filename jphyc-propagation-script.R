# Load data:
prop_data <- read.csv("jphyc-propagation-data.csv", header = TRUE, sep = ",")

##### Fit full model with all biologically plausible terms (9 predictors)
logit_prop_data_full <- glm(increase.split.binary ~ split.initial.mm +
								Category + 
								split.initial.mm:Category +
								width +
								speed.percent:midline.length +
								ratio +
								area +
								speed.percent +
								midline.length,
									family = binomial, data = prop_data)

summary(logit_prop_data_full) 



##### Drop length (p = 0.99) (8 predictors)
logit_prop_data <- glm(increase.split.binary ~ split.initial.mm +
								Category + 
								split.initial.mm:Category +
								width +
								speed.percent:midline.length +
								ratio +
								area +
								speed.percent,
									family = binomial, data = prop_data)

summary(logit_prop_data) 



##### Drop midline.length:speed.percent (p = 0.74) (7 predictors)
logit_prop_data <- glm(increase.split.binary ~ split.initial.mm +
								Category + 
								split.initial.mm:Category +
								width +
								ratio +
								area +
								speed.percent,
									family = binomial, data = prop_data)

summary(logit_prop_data) 



##### Drop area (p = 0.76) (6 predictors)

logit_prop_data <- glm(increase.split.binary ~ split.initial.mm +
								Category + 
								split.initial.mm:Category +
								width +
								ratio +
								speed.percent,
									family = binomial, data = prop_data)
summary(logit_prop_data) 



##### Drop ratio (p = 0.66) (5 predictors)
logit_prop_data <- glm(increase.split.binary ~ split.initial.mm +
								Category + 
								split.initial.mm:Category +
								width +
								speed.percent,
									family = binomial, data = prop_data)

summary(logit_prop_data) 



##### Drop crack type:crack length (p = 0.24) (4 predictors)
logit_prop_data <- glm(increase.split.binary ~ split.initial.mm +
								Category + 
								width +
								speed.percent,
									family = binomial, data = prop_data)


summary(logit_prop_data) 



##### Drop speed (p = 0.14) (3 predictors)
logit_prop_data <- glm(increase.split.binary ~ split.initial.mm +
								Category + 
								width,
									family = binomial, data = prop_data)



summary(logit_prop_data) 



##### Drop width (p = 0.06) (2 predictors)
logit_prop_data <- glm(increase.split.binary ~ split.initial.mm +
								Category,
									family = binomial, data = prop_data)


summary(logit_prop_data)



##### Final model:
logit_prop_data <- glm(increase.split.binary ~ split.initial.mm + Category,
									family = binomial, data = prop_data)
summary(logit_prop_data) 

b <- coef(logit_prop_data)

# Artificial
split_artificial <- -b["(Intercept)"] / b["split.initial.mm"]

# Natural
split_natural <- -(b["(Intercept)"] + b["CategoryNatural"]) / b["split.initial.mm"]

split_artificial
split_natural


