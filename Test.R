######Task :
library(tidyr)
library(dplyr)
require(foreign)
require(ggplot2)
require(boot)


###read data.txt file

doc <-read.table("adult.data.txt", stringsAsFactors=TRUE, sep=",")


###Put column names

colNames <- c("age", "workclass", "fnlwgt", "education", "education_num", "marital_status", "occupation", "relationship", "race", "sex", "capital_gain", "capital_loss", "hours_per_week", "native_country", "seuils")
colnames(doc) <- colNames


# Change somes variables as numeric
doc$age <- as.numeric(doc$age)
doc$fnlwgt <- as.numeric(doc$fnlwgt)
doc$education_num <- as.numeric(doc$education_num)
doc$capital_gain <- as.numeric(doc$capital_gain)
doc$capital_loss <- as.numeric(doc$capital_loss)
doc$hours_per_week <- as.numeric(doc$hours_per_week)

# Testing for missing values
c.age <- sd(doc$age, na.rm=TRUE)
c.fnlwgt <- sd(doc$fnlwgt, na.rm=TRUE)
c.education_num <- sd(doc$education_num, na.rm=TRUE)
c.capital_gain<- sd(doc$capital_gain, na.rm=TRUE)
c.capital_loss <- sd(doc$capital_loss, na.rm=TRUE)
c.hours_per_week <- sd(doc$hours_per_week, na.rm=TRUE)


# Somes command to know the dimension, the name of row and a resume of doc
dim(doc)
names(doc)
str(doc)
head(doc)
glimpse (doc)

#Identify missing value for example "age"
missing <- is.na(doc$age)


#### visualize some data
ggplot(data=doc, aes(x=c.education_num, y=seuils, col=sex)) + geom_point(position="jitter")


# create predictor 
doc$capital <- ifelse(doc$capital_gain==0, -doc$capital_loss, doc$capital_gain)
doc$c.capital <- (doc$capital - mean(doc$capital, na.rm=TRUE)) / (2 * sd(doc$capital, na.rm=TRUE))
summary(doc$capital)

# create model 1

model1 <- glm(seuils ~ sex + age + education_num + workclass + occupation + c.capital, data=doc, family=binomial(link="logit"))


# create model 2

model2 <- glm(seuils ~ sex * age + I(age^2) + education_num + workclass + occupation + c.capital, data=doc, family=binomial(link="logit"))


# load test file

test <- read.table("adult.test", stringsAsFactors=TRUE, sep=",", skip=1)
colNames <- c("age", "workclass", "fnlwgt", "education", "education_num", "marital_status", "occupation", "relationship", "race", "sex", "capital_gain", "capital_loss", "hours_per_week", "native_country", "seuils")
colnames(test) <- colNames


# change variables as numeric for test

test$age <- as.numeric(test$age)
test$fnlwgt <- as.numeric(test$fnlwgt)
test$education_num <- as.numeric(test$education_num)
test$capital_gain <- as.numeric(test$capital_gain)
test$capital_loss <- as.numeric(test$capital_loss)
test$hours_per_week <- as.numeric(test$hours_per_week)

# generate `c.capital` predictor 
test$capital <- ifelse(test$capital_gain==0, -test$capital_loss, test$capital_gain)
test$c.capital <- (test$capital - mean(test$capital, na.rm=TRUE)) / (2 * sd(test$capital, na.rm=TRUE))



# count predicators and table

glm.probs <- predict(model2, newdata=test, type="response")
glm.pred = rep(" <=50K.", length(test$seuils))
glm.pred[glm.probs >= 0.5] = " >50K."
table(glm.pred, test$seuils)

# test prediction
1 - mean(glm.pred == test$seuils)




#add

revenu %>%
  summarize(total = n(),
            percent_income = mean(doc == "<=50K"))


by_income <- revenu %>%
  group_by(V14) %>%
  summarize(total = n(),
            percent_income = mean(doc == "<=50K"))


by_income %>%
  arrange(desc(percent_income))

##Task 2



x1 = c(2, 4, 1)
x2 = c(4, 1, 1)
x3 = c(2, -1, 3)

## example of a matrix
X = rbind(x1,x2,x3)

## Dimension of this Matrix
dim(X)

######inverse this matrix
solve(X)


#####Other Task:


### create data
customer <- data.frame(memberid=c(11112222,11112222,11112222,11116666,22222444),year_month=c(201701, 201702, 201703, 201702, 201702), status=c("engaged","engaged","passionate","shy", "shy"))

###identify each category
levels(customer$status)


#Test the format of Date
format(customer$year_month, format="%Y %b")


###Data Cleaning: separate year_month to "year" and "month" and we don't need column "memberid" for our analyze
customer1 <- separate(customer, year_month, c("Year", "Month"))

###occurence of each category
table(customer$status)

##### data we can use probably a regression model 
Model=lm(status~customer1,data=customer)
