#load library 
library(readr)
library(dplyr)
library(ggplot2)


#read data
lung_cancer <- read_csv("survey lung cancer.csv")
View(lung_cancer)
glimpse(lung_cancer)


#remove duplicate value
lung_cancer <- distinct(lung_cancer)

    
#convert to factor
lung_cancer$GENDER                   <- as.factor(lung_cancer$GENDER)
lung_cancer$SMOKING                  <- as.factor(lung_cancer$SMOKING)
lung_cancer$YELLOW_FINGERS           <- as.factor(lung_cancer$YELLOW_FINGERS)
lung_cancer$ANXIETY                  <- as.factor(lung_cancer$ANXIETY)
lung_cancer$PEER_PRESSURE            <- as.factor(lung_cancer$PEER_PRESSURE)
lung_cancer$`CHRONIC DISEASE`        <- as.factor(lung_cancer$`CHRONIC DISEASE`)
lung_cancer$FATIGUE                  <- as.factor(lung_cancer$FATIGUE)
lung_cancer$ALLERGY                  <- as.factor(lung_cancer$ALLERGY)
lung_cancer$WHEEZING                 <- as.factor(lung_cancer$WHEEZING)
lung_cancer$`ALCOHOL CONSUMING`      <- as.factor(lung_cancer$`ALCOHOL CONSUMING`)
lung_cancer$COUGHING                 <- as.factor(lung_cancer$COUGHING)
lung_cancer$`SHORTNESS OF BREATH`    <- as.factor(lung_cancer$`SHORTNESS OF BREATH`)
lung_cancer$`SWALLOWING DIFFICULTY`  <- as.factor(lung_cancer$`SWALLOWING DIFFICULTY`)
lung_cancer$`CHEST PAIN`             <- as.factor(lung_cancer$`CHEST PAIN`)
lung_cancer$LUNG_CANCER              <- as.factor(lung_cancer$LUNG_CANCER)


#First we See the lung cancer rates 
ggplot(lung_cancer,aes(x=LUNG_CANCER, fill= LUNG_CANCER))+
  theme_bw()+geom_bar() + 
  ggtitle("Lung Cancer Rates")


#see the probability 
prop.table(table(lung_cancer$LUNG_CANCER))


#see the lung cancer rates by age 
ggplot(lung_cancer,aes(x = LUNG_CANCER, y=AGE)) + 
  theme_bw()+
  geom_boxplot(aes(fill=LUNG_CANCER))+ 
  labs(y="Age",
       x="Laung cancer",
       title="Lung Cancer Rates by Age")


#Now see the lung cancer rates by Gender
ggplot(lung_cancer,aes(x=GENDER, fill= LUNG_CANCER)) +
  theme_bw()+
  geom_bar() + 
  ggtitle("Lung Cancer Rates by Gender")


#density
ggplot(lung_cancer,aes(x =AGE, fill=LUNG_CANCER)) + 
  theme_bw()+
  facet_wrap(GENDER ~ SMOKING)+
  geom_density(alpha=0.5)+
  labs(y="Age",
       x="Laung cancer",
       title="Lung Cancer Rates by Age, Gender and Smoking")


#see the cancer rates compare by age, anxiety,fatigue and alcohol consuming
ggplot(lung_cancer,aes(x = AGE, fill=LUNG_CANCER)) + 
  theme_bw()+
  facet_wrap(ANXIETY ~FATIGUE ~`ALCOHOL CONSUMING`)+
  geom_dotplot(binwidth = 3)+
  labs(y="Age",
       x="Laung cancer",
       title="Lung Cancer Rates by Age,Anxiety,Fatigue and Alcohol consuming")


#lung cancer rates compared to age, coughing and swallowing difficulty variable
ggplot(lung_cancer,aes(x = AGE, fill=LUNG_CANCER)) + 
  theme_bw()+
  geom_histogram(binwidth = 5) +
  facet_grid(COUGHING~`SWALLOWING DIFFICULTY`)+
  labs(y="Count",
       x="Age",
       title="Lung Cancer Rates by Age, Coughing and Swallowing difficulty")


#lung cancer rates compared to gender, smoke and yellow finger variable
ggplot(lung_cancer,aes(x=GENDER, fill = LUNG_CANCER)) + 
  theme_bw()+
  facet_wrap(SMOKING~YELLOW_FINGERS)+
  geom_bar() + 
  ggtitle("Lung Cancer Rates by Gender, Smoke and Yellow Finger")


#convert to numeric data
lung_cancer$LUNG_CANCER <- as.numeric(lung_cancer$LUNG_CANCER)



#build linear regression model
model <- lm(formula = LUNG_CANCER~  + SMOKING + YELLOW_FINGERS + ANXIETY + 
              PEER_PRESSURE + `CHRONIC DISEASE` + 
              `SHORTNESS OF BREATH` + FATIGUE + ALLERGY + WHEEZING +
              `ALCOHOL CONSUMING`  + COUGHING +`SHORTNESS OF BREATH`+
              `SWALLOWING DIFFICULTY` + `CHEST PAIN`, data = lung_cancer)

model
summary(model)


#anova test
anova(model)


#for model comparison 
AIC(model)
BIC(model)


#predict the linear model 
set.seed(100)
#row indices for training model
trainingrowindex <- sample(1:nrow(lung_cancer), 0.8*nrow(lung_cancer))
#model training data
trainingdata <- lung_cancer[trainingrowindex,]
#test data
testdata <- lung_cancer[-trainingrowindex,]

#develope the model on training data
linearmodel <- lm(formula = LUNG_CANCER~  + SMOKING + YELLOW_FINGERS + ANXIETY + 
                    PEER_PRESSURE + `CHRONIC DISEASE` + 
                    `SHORTNESS OF BREATH` + FATIGUE + ALLERGY + WHEEZING +
                    `ALCOHOL CONSUMING`  + COUGHING +`SHORTNESS OF BREATH`+
                    `SWALLOWING DIFFICULTY` + `CHEST PAIN`, data = trainingdata)

#predict distance
distancepredict <- predict(linearmodel,testdata)

#review the measure
summary(linearmodel)

#prediction accuracy and error rate
#actual prediction data frame
actualpredicts <- data.frame(cbind(actuals = testdata$LUNG_CANCER, predicteds =distancepredict))

#correlation accuracy
cor_accuracy <-cor(actualpredicts)
head(actualpredicts)

#min max accuracy and map
minmaxacc <- mean(apply(actualpredicts, 1, min)) / apply(actualpredicts, 1, max)
summary(minmaxacc)

