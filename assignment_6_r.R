rm(list=ls())
cat("/014")
setwd("C:/Users/vnath/Documents/Uni/STTN 317/assignment_6")

library(ggplot2)
library(corrplot)
library(olsrr)


df <- data.frame(read.csv("rent_data.csv"))
attach(df)

# ===== Question 1 ===== #
head(df)


# ===== Question 2 ===== #

# price and city
boxplot(price~area, main="Price by city", xlab="City", ylab="Price", col=c("khaki","gold","yellow"))

# price and size of apartment
ggplot(df, aes(x=prop_sqr_m, y=price, color=area)) + geom_point(size=2, ) + ggtitle("Price and size of apartment") + 
  labs(x="Property size /square meters", y="Price /R")

# distance to nearest academic bookshop and price
ggplot(df, aes(x=dist_to_bookshop, y=price, color=area)) + geom_point(size=2) + ggtitle("Price and distance to nearest bookstores") + 
  labs(x="Distance to nearest bookstore /km", y="Price /R")
# distance to varsity and price
ggplot(df, aes(x=dist_to_varsity, y=price, color=area)) + geom_point(size=2) + labs(x="Distance to varsity /m", y="Price /R") + ggtitle("Distance to varsity and price")
# kitchen and price
boxplot(price~kitchen, main="Price by kitchen", xlab="Kitchen", ylab="Price /R", col=c("aquamarine","darkgrey"))
# bedroom, bathroom, and price
boxplot(price~bedr, main="Price by number of bedrooms", xlab="number of bedrooms", ylab="price /R", col=c("aquamarine","skyblue","cornflowerblue"))
boxplot(price~bathr, main="Price by number of bathrooms", xlab="number of bathrooms", ylab="price /R", col=c("lightgreen","green","darkgreen"))

# ===== Question 3 ===== #
df2 <- subset(df, select = c(-kitchen,-area))
round(cor(df2),3)
head(df2)

# ===== Question 4 ===== #
# first we test for normality of the errors
Model1 <- lm(price ~ bedr + bathr + prop_sqr_m + dist_to_varsity + dist_to_bookshop, data=df)
summary(Model1)
anova(Model1)


# ===== Question 5 ===== #

df_scaled <- as.data.frame(scale(subset(df, select=c(-area, -kitchen))))
mod <- lm(price ~ bedr + bathr + prop_sqr_m + dist_to_varsity + dist_to_bookshop, data=df_scaled)
ols_coll_diag(mod)
Model2 <- lm(price ~ bathr +  prop_sqr_m + dist_to_varsity + dist_to_bookshop, data=df_scaled)
ols_coll_diag(Model2)


# ===== Question 6 =====#
Model3 <- lm(price ~ dist_to_varsity + bedr + area + kitchen, data=df)
summary(df)
result <- ols_step_all_possible(Model3)
maxrsq <- max(result$adjr)
i <- which(result$adjr == maxrsq)
result$predictors[i]





# ===== Question 7 ===== #
influence.measures(Model3)
influence <- influence.measures(Model3)
influence
b <- influence$infmat[,1:6]
dfbetas_infl <- b>2/sqrt(300)
sum_infl <- c()

for (i in 1:300){
  cnt=0  
  for(j in 1:6){
    if(b[i,j]>2/sqrt(300)){
      cnt = cnt + 1
    }
  sum_infl[i] <- cnt
  }
}
sum_infl
del_indx <- c(which(sum_infl>0))
del_vals <- c(Model3$residuals[del_indx])

rstud <- c((del_vals-mean(del_vals))/sd(del_vals))

new_rent_df <- df[-del_indx,]

# ===== Question 8 ==== #
Model4 <- lm(price ~ dist_to_varsity + bedr + area + kitchen, data=new_rent_df)

# ===== Question 9 ==== #
res4 <- Model4$residuals

# formal hypothesis test
# H0: errors are normal vs HA: errors not normal
shapiro.test(res4)
# at significance level alpha = 0.05 we reject H0 and conclude
qqnorm(res4)
qqline(res4)
hist(res4, freq=FALSE)
d <- density(res4)
lines(d, lty=2)

r41 <- res4[1:(268/2)]
r42 <- res4[(length(res4)/2):268]
var.test(r41,r42)

# ===== Question 10 ==== #
new_data <- data.frame(dist_to_varsity = 5, bedr = 3, area = "CapeTown", kitchen = "modern")
predicted_price <- predict(Model4, newdata = new_data)
predicted_price
summary(Model4)
