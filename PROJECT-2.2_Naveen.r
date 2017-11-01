############# Project-2.2 ############## 
# Data collected for several hundred used General Motors (GM) cars allows us to develop a 
# multivariate regression model to determine car values based on a variety of characteristics
# such as mileage, make, model, cruise control, and so on. 

# getwd()
# setwd ("F:/ACADGILD/Business Analytics With R/ASSIGNMENTS/_Project-2")

# install.packages("readxl")
library("readxl")
car_data<- read_excel("F:/ACADGILD/Business Analytics With R/ASSIGNMENTS/_Project-2/GMCars_data.xlsx")
View(car_data)
str(car_data)

car_data$Make <- factor(car_data$Make, levels = c('Cadillac','Chevrolet','Pontiac'), labels = c(1,2,3))

# install.packages("corrplot")
library(corrplot)

# Calculating Correlation
# names(car_data)
cor_relation <- cor(car_data[,c(-3)])
head(round(cor_relation,3))
corrplot(cor_relation, method= "number")
# pairs(cor_relation)

#### Developing Regression Model #####

# Splitting the car_datainto the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(car_data$sales, SplitRatio = 2/3)
training_set = subset(car_data, split == TRUE)
test_set = subset(car_data, split == FALSE)

# regressorting Multiple Linear Regression to the Training set
regressor = lm(formula = sales ~ .,
               data = training_set)
par(mfrow=c(2,2))
plot(regressor)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)
head(y_pred)

# Visualising the Training set results
# install.packages("ggplot2")
# names(car_data)
# library(ggplot2)
# ggplot() +
#   geom_point(aes(x = training_set$attractiveness, y = training_set$sales),
#              colour = 'red') +
#   geom_line(aes(x = training_set$attractiveness, y = predict(regressor, newdata = training_set)),
#             colour = 'blue') +
#   ggtitle('sales vs Experience (Training set)') +
#   xlab('Years of experience') +
#   ylab('sales')

# Building the optimal model using Backward Elimination
regressor = lm(formula = sales ~ .,
               data = training_set)
summary(regressor)

# Extracting Coefficients
summary(regressor)$coeff
anova(regressor)

AIC(regressor)
BIC(regressor)

#### Variable Selection Methods ####
#Stepwise Selection based on AIC
install.packages('MASS')
library(MASS)
step <- stepAIC(regressor, direction="both")
summary(step)

#Backward Selection based on AIC
step <- stepAIC(regressor, direction="backward")
summary(step)

#Forward Selection based on AIC
step <- stepAIC(regressor, direction="forward")
summary(step)
