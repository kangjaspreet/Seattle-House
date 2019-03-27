rm(list=ls())
library(MASS)
library(car)
library(data.table)
library(zipcode)
library(ggplot2)

# Note for the day: Combine year and month, then create new model 
a <- read.csv("C:/Users/Jaspo/Documents/R Projects/kc.csv")
str(a) 
date <- substr(a$date, 1, 6)
a$date <- substr(a$date, 1, 6)
a$date <- as.factor(a$date)

summary(a[,-c(1,2,17,18,19,16)])
### This section converts the zipcode to city
b <- a[order(a$zipcode), ]
zc <- data.frame("zip" = b$zipcode)
data(zipcode)
c <- as.data.frame(merge(zc, zipcode))
b <- cbind(b, "city" = c$city)
b <- b[,-c(1, 17, 18, 19)] # Getting rid of 
names(b)
###
nlevels(b$city)
# the distribution is skewed to the right. We take a log transformation
# Now the disrtibution looks normal with little skewness
qplot(price, data=b) + geom_histogram(binwidth=100000) 
qplot(log(price), data=b) + geom_histogram()
colSums(is.na(b)) # No NA values, so no data cleansing is needed


model1 <- lm(log(price)~., data = b) 
summary(model1)
plot(model1$fitted.values, log(b$price)) # fitted values vs logged observtion 
ggplot(b, aes(x = model1$fitted.values, y = log(price))) +
  geom_point()

plot(model1) # 4 plots

step(model1, direction="both") # Performing stepwise selection

model1 <- lm(log(price) ~. - sqft_basement, data = b)
summary(model1)
plot(model1$fitted.values, log(b$price)) # fitted values vs logged observtion 
ggplot(b, aes(x = model1$fitted.values, y = log(price))) +
  geom_point()

plot(model1) # 4 plots


#set.seed(1)
#random <- sample(21613, 15129)
#train <- b[random, ]
#test <- b[-random, ]


# Seeing if multicollinearity exists

 # alias(model1) # sqft_basement seems to be the alias coefficient. Or sqft_living and sqft_above?
vif(model1) # sqft_living and sqft_above have VIF's > 5. Solution would be to remove one of these
model1 <- lm(log(price) ~. - sqft_above - sqft_basement, data = b)
vif(model1) # sqft_living still has high VIF
model1 <- lm(log(price) ~. - sqft_living - sqft_basement, data = b)
vif(model1) # sqft_living causes sqft_above to go below 5, so we keep sqft_above and remove sqft_living

summary(model1)
step(model1,direction="both") 

reduced_model <- lm(log(price)~. - sqft_basement - sqft_living, data = b)
summary(reduced_model)

plot(reduced_model$fitted.values, log(b$price)) # fitted values vs logged observtion 
ggplot(b, aes(x = reduced_model$fitted.values, y = log(price))) +
  geom_point()

par(mfrow=c(2,2))
plot(reduced_model)# 4 plots


# Outliers seem to have a strong effect on our model. We address that here
cooksd <- cooks.distance(reduced_model) # calculating cooks distance
head(cooksd)
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))]) 
index <- rep(NA, 982)
for (i in 1:982) {
  index[i] <- which(rownames(b) == influential[i])
}


b_cook <- b[-index,]

model_remove <- lm(log(price)~. - sqft_basement - sqft_living, data = b_cook)
summary(model_remove)

# fitted values vs logged observtion 
ggplot(b_cook, aes(x = model_remove$fitted.values, y = log(price))) +
  geom_point()

par(mfrow=c(2,2))
plot(model_remove) # 4 plots

comm <- cbind(b_cook$price[1:100], exp(model_remove$fitted.values[1:100]))
comm <- cbind(comm, comm[,1] - comm[,2])
comm.df <- data.frame("Observed" = comm[,1], "Predicted" = comm[,2], "Difference" = comm[,3])
comm.df
mean(abs(comm[,3]))

comm_full <- cbind(b_cook$price, exp(model_remove$fitted.values))
comm_full <- cbind(comm_full, comm_full[,1] - comm_full[,2])
mean(abs(comm_full[,3]))
