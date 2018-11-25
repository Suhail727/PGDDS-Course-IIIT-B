library(MASS)
library(car)
library(tidyr)
library(dplyr)
library(ggplot2)

#Loading Data
cars<- read.csv("CarPrice_Assignment.csv",stringsAsFactors = F)

#Look at the data
str(cars)

##################################################
#Data Cleaning
##################################################
#Remove ID Column
cars<- cars[,-1]

#Convert column names to lower case for ease of use
colnames(cars) <- tolower(colnames(cars))

#Check for duplicate rows and missing values
nrow(unique(cars))
sum(is.na(cars))
#No duplicate rows or missing values

#Column carname contains both Company and model Name
#Split the name and exclude the model Name (consider only company name)
cars<-separate(cars,carname,c('companyname','modelname'),sep=" ", extra = "merge", fill = "right")
cars['modelname']=NULL
#convert company name to lower case
cars$companyname <- tolower(cars$companyname)

#check the company name for duplicates of same company
unique(cars$companyname)
table(cars$company)

# There are a few duplicates which needs to be taken care of
correct_companyname <- function(car_companyname){
  corrected_name <- car_companyname 
  if(car_companyname == "alfa-romero"){
    corrected_name <- "alfa-romeo" #No such company as alfa-romero
  } else if(car_companyname == "maxda"){
    corrected_name <- "mazda" #was misspelled
  } else if(car_companyname == "porcshce"){
    corrected_name <- "porsche" #was misspelled
  } else if(car_companyname == "toyouta") {
    corrected_name <- "toyota" #was misspelled
  } else if(car_companyname %in% c("vokswagen", "vw")){
    corrected_name <- "volkswagen" #vw is assumed to be acronym of volkswagen
  } 
  return (corrected_name)
}
# Using sapply to correct all car companynames
cars$companyname <- sapply(cars$companyname, correct_companyname)
#Check data again
table(cars$companyname)


#######################Checking for outliers############################
###1 wheelbase
quantile(cars$wheelbase, seq(0,1,0.01))
#there is jump from 99-100 quantile. So set limit to 115.544
cars$wheelbase[which(cars$wheelbase > 115.544 )] <- 115.544

###2 carlength
quantile(cars$carlength, seq(0,1,0.01))
#jump after 95%. So set limit to 192.700
cars$carlength[which(cars$carlength > 192.700 )] <- 192.700

###3 carwidth
quantile(cars$carwidth, seq(0,1,0.01))
#jump between 0% and 1%. So set all values below one percent to 62.536
cars$carwidth[which(cars$carwidth < 62.536 )] <- 62.536

###4 carheight
quantile(cars$carheight, seq(0,1,0.01))
#No random jump in values

###5 curbweight
quantile(cars$curbweight, seq(0,1,0.01))
#jump between 0% and 1% So set all values below one percent to 1819.72
cars$curbweight[which(cars$curbweight < 1819.72 )] <- 1819.72

###6 enginesize
quantile(cars$enginesize, seq(0,1,0.01))
#jump between from 0% to 3% and 93% to 100% So set limits to 90.00 and 183.00 respectively
cars$enginesize[which(cars$enginesize < 90.00 )] <- 90.00
cars$enginesize[which(cars$enginesize > 183.00 )] <- 183.00

###7 boreratio
quantile(cars$boreratio, seq(0,1,0.01))
#No random jump in values

###8 stroke 
quantile(cars$stroke, seq(0,1,0.01))
#No random jump in values

###9 compressionratio
quantile(cars$compressionratio, seq(0,1,0.01))
#jump above 90% Set limit to 10.940
cars$compressionratio[which(cars$compressionratio > 10.9400 )] <- 10.9400

###10 horsepower
quantile(cars$horsepower, seq(0,1,0.01))
#jump above 97% Set limit to 184.00
cars$horsepower[which(cars$horsepower > 184.00 )] <- 184.00

###11 peakrpm
quantile(cars$peakrpm, seq(0,1,0.01))
#jump above 99% Set limit to 6000
cars$peakrpm[which(cars$peakrpm > 6000 )] <- 6000

###12 citympg
quantile(cars$citympg, seq(0,1,0.01))
#jump above 98% Set limit to 38.00
cars$citympg[which(cars$citympg > 38.00 )] <- 38.00

###13 highwaympg
quantile(cars$highwaympg, seq(0,1,0.01))
#jum above 99% Set limit to 49.88
cars$highwaympg[which(cars$highwaympg > 49.88 )] <- 49.88

###14 price
quantile(cars$price, seq(0,1,0.01))
#jump above 98% Set limit to 36809.60
cars$price[which(cars$price > 36809.60 )] <- 36809.60

#######################Creating Dummy Variables############################

###1 companyname
summary(factor(cars$companyname))
dummy_1<-data.frame(model.matrix(~companyname,cars))
dummy_1[,1]<-NULL
cars_1<-cbind(cars[,-2],dummy_1)

###2 fueltype
summary(factor(cars$fueltype))
cars_1$fueltype <- as.factor(cars_1$fueltype)
levels(cars_1$fueltype) <- c(1,0) 
str(cars_1$fueltype)

###3 aspiration
summary(factor(cars$aspiration))
cars_1$aspiration <- as.factor(cars_1$aspiration)
levels(cars_1$aspiration) <- c(1,0)
str(cars_1$aspiration)

###4 doornumber
summary(factor(cars$doornumber))
cars_1$doornumber <- as.factor(cars_1$doornumber)
levels(cars_1$doornumber) <- c(1,0)
str(cars_1$doornumber)

###5 carbody
summary(factor(cars$carbody))
dummy_2<-data.frame(model.matrix(~carbody,cars))
dummy_2[,1]<-NULL
cars_2<-cbind(cars_1[,-5],dummy_2)

###6 drivewheel
summary(factor(cars$drivewheel))
dummy_3<-data.frame(model.matrix(~drivewheel,cars))
dummy_3[,1]<-NULL
cars_3<-cbind(cars_2[,-5],dummy_3)

###7 enginelocation
summary(factor(cars$enginelocation))
cars_3$enginelocation <- as.factor(cars_3$enginelocation)
levels(cars_3$enginelocation) <- c(1,0)
str(cars_3$enginelocation)

###8 enginetype
summary(factor(cars$enginetype))
dummy_4<-data.frame(model.matrix(~enginetype,cars))
dummy_4[,1]<-NULL
cars_4<-cbind(cars_3[,-11],dummy_4)

###9 cylindernumber
summary(factor(cars$cylindernumber))
dummy_5<-data.frame(model.matrix(~cylindernumber,cars))
dummy_5[,1]<-NULL
cars_5<-cbind(cars_4[,-11],dummy_5)

###10 fuelsystem
summary(factor(cars$fuelsystem))
dummy_6<-data.frame(model.matrix(~fuelsystem,cars))
dummy_6[,1]<-NULL
cars_6<-cbind(cars_5[,-12],dummy_6)

##################################################################
#Data Analysis
##################################################################

#set the seed
set.seed(100)

#Generate indices for test data which is 70% of entire data
trainindices= sample(1:nrow(cars_6), 0.7*nrow(cars_6))

#Store the train and test data in separate dataframes
train = cars_6[trainindices,]
test = cars_6[-trainindices,]

#Generate model_1
model_1 <-lm(price~.,data=train)
summary(model_1)
#Pass the model_1 to stepAIC function
step <- stepAIC(model_1, direction="both")
step

#stepAIC has removed the varables which it thinks is insignificant
#store the significant values returned by stepAIC into model_2
model_2<-lm(formula = price ~ symboling + aspiration + enginelocation + 
              wheelbase + carwidth + curbweight + enginesize + boreratio + 
              stroke + horsepower + companynameaudi + companynamebmw + 
              companynamebuick + companynamechevrolet + companynamedodge + 
              companynamehonda + companynameisuzu + companynamejaguar + 
              companynamemazda + companynamemercury + companynamemitsubishi + 
              companynamenissan + companynamepeugeot + companynameplymouth + 
              companynamerenault + companynamesaab + companynamesubaru + 
              companynametoyota + companynamevolkswagen + companynamevolvo + 
              carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
              drivewheelfwd + enginetyperotor + cylindernumberfive + cylindernumberfour + 
              cylindernumbersix, data = train)
summary(model_2)

#CHECK FOR MULTICOLLINEARITY
#Check VIF values
sort(vif(model_2), decreasing = TRUE)

#enginesize has VIF of 33.26207 and p-value of 0.126644 (which is >0.05) So remove it from the model
model_3<-lm(formula = price ~ symboling + aspiration + enginelocation + 
              wheelbase + carwidth + curbweight + boreratio + 
              stroke + horsepower + companynameaudi + companynamebmw + 
              companynamebuick + companynamechevrolet + companynamedodge + 
              companynamehonda + companynameisuzu + companynamejaguar + 
              companynamemazda + companynamemercury + companynamemitsubishi + 
              companynamenissan + companynamepeugeot + companynameplymouth + 
              companynamerenault + companynamesaab + companynamesubaru + 
              companynametoyota + companynamevolkswagen + companynamevolvo + 
              carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
              drivewheelfwd + enginetyperotor + cylindernumberfive + cylindernumberfour + 
              cylindernumbersix, data = train)
summary(model_3)
sort(vif(model_3), decreasing = TRUE)

#cylindernumberfour, curbweight, carbodysedan, carbodyhatchback, cylindernumbersix all have
#high VIF values but p-values are <0.05. So they cannot be removed from model.

#wheelbase has VIF of 12.665174 and p-value 0.257006 (which is >0.05). So remove it from the model
model_4<-lm(formula = price ~ symboling + aspiration + enginelocation + 
              carwidth + curbweight + boreratio + 
              stroke + horsepower + companynameaudi + companynamebmw + 
              companynamebuick + companynamechevrolet + companynamedodge + 
              companynamehonda + companynameisuzu + companynamejaguar + 
              companynamemazda + companynamemercury + companynamemitsubishi + 
              companynamenissan + companynamepeugeot + companynameplymouth + 
              companynamerenault + companynamesaab + companynamesubaru + 
              companynametoyota + companynamevolkswagen + companynamevolvo + 
              carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
              drivewheelfwd + enginetyperotor + cylindernumberfive + cylindernumberfour + 
              cylindernumbersix, data = train)
summary(model_4)
sort(vif(model_4), decreasing = TRUE)

#cylindernumberfour, curbweight, carbodysedan, carbodyhatchback, cylindernumbersix, companynametoyota, carwidth   
#all have high VIF values but p-values are <0.05. So they cannot be removed from model.

#horsepower has VIF value 10.737123, and p-value 0.069733 (which is >0.05) So remove it from the model
model_5 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
                carwidth + curbweight + boreratio + stroke + fuelsystem4bbl + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + cylindernumberthree + 
                enginetypel + enginetypeohcf + drivewheelfwd + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + companynameaudi + 
                companynamebmw + companynamebuick + companynamedodge + companynamehonda + 
                companynameisuzu + companynamejaguar + companynamemazda + 
                companynamemercury + companynamemitsubishi + companynamenissan + 
                companynameplymouth + companynamerenault + companynamesaab + 
                companynametoyota + companynamevolkswagen + companynamevolvo, 
              data = train)
summary(model_5)
sort(vif(model_5), decreasing = TRUE)

#cylindernumberfour, curbweight, carbodysedan, carbodyhatchback, cylindernumbersix, companynametoyota, carwidth   
#enginetypeohcf,carbodywagon all have high VIF values but p-values are <0.05. So they cannot be removed from model.

#boreratio has VIF value 7.416939 and p-value 0.298119 (which is >0.05) So remove it from the model
model_6 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
                carwidth + curbweight + stroke + fuelsystem4bbl + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + cylindernumberthree + 
                enginetypel + enginetypeohcf + drivewheelfwd + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + companynameaudi + 
                companynamebmw + companynamebuick + companynamedodge + companynamehonda + 
                companynameisuzu + companynamejaguar + companynamemazda + 
                companynamemercury + companynamemitsubishi + companynamenissan + 
                companynameplymouth + companynamerenault + companynamesaab + 
                companynametoyota + companynamevolkswagen + companynamevolvo, 
              data = train)
summary(model_6)
sort(vif(model_6), decreasing = TRUE)

#cylindernumberfour, curbweight, carbodysedan, carbodyhatchback, cylindernumbersix, companynametoyota, carwidth   
#carbodywagon, companynamehonda ,companynamemazda, companynamenissan, enginetypeohcf  all have high VIF values
#but p-values are <0.05. So they cannot be removed from model.

#drivewheelfwd has VIF value 6.334387 and p-value 0.188209 (which is >0.05) So remove it from the model
model_7 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
                carwidth + curbweight + stroke + fuelsystem4bbl + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + cylindernumberthree + 
                enginetypel + enginetypeohcf + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + companynameaudi + 
                companynamebmw + companynamebuick + companynamedodge + companynamehonda + 
                companynameisuzu + companynamejaguar + companynamemazda + 
                companynamemercury + companynamemitsubishi + companynamenissan + 
                companynameplymouth + companynamerenault + companynamesaab + 
                companynametoyota + companynamevolkswagen + companynamevolvo, 
              data = train)
summary(model_7)
sort(vif(model_7), decreasing = TRUE)

#cylindernumberfour, curbweight, carbodysedan, carbodyhatchback, cylindernumbersix, companynametoyota, carwidth   
#carbodywagon all have high VIF values
#but p-values are <0.05. So they cannot be removed from model.

#companynamehonda has VIF value 6.951827 and p-value 0.050156 (which is >0.05) So remove it from the model
model_8 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
                carwidth + curbweight + stroke + fuelsystem4bbl + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + cylindernumberthree + 
                enginetypel + enginetypeohcf + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + companynameaudi + 
                companynamebmw + companynamebuick + companynamedodge + 
                companynameisuzu + companynamejaguar + companynamemazda + 
                companynamemercury + companynamemitsubishi + companynamenissan + 
                companynameplymouth + companynamerenault + companynamesaab + 
                companynametoyota + companynamevolkswagen + companynamevolvo, 
              data = train)
summary(model_8)
sort(vif(model_8), decreasing = TRUE)

#cylindernumberfour, carbodysedan, carbodyhatchback, curbweight, cylindernumbersix, carwidth, carbodywagon, 
#cylindernumberfive, companynamebuick, fuelsystem4bbl, stroke, enginetypeohcf, enginetypel,
#all have high VIF values but p-values are <0.05. So they cannot be removed from model.

#symboling has VIF value 3.529029 and p-value 0.858605 (which is >0.05) So remove it from the model
model_9 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + stroke + fuelsystem4bbl + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + cylindernumberthree + 
                enginetypel + enginetypeohcf + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + companynameaudi + 
                companynamebmw + companynamebuick + companynamedodge + 
                companynameisuzu + companynamejaguar + companynamemazda + 
                companynamemercury + companynamemitsubishi + companynamenissan + 
                companynameplymouth + companynamerenault + companynamesaab + 
                companynametoyota + companynamevolkswagen + companynamevolvo, 
              data = train)
summary(model_9)
sort(vif(model_9), decreasing = TRUE)

#cylindernumberfour, carbodysedan, carbodyhatchback, curbweight, cylindernumbersix, carwidth, carbodywagon, 
#cylindernumberfive, companynamebuick, fuelsystem4bbl, stroke, enginetypeohcf, enginetypel, carbodyhardtop
#all have high VIF values but p-values are <0.05. So they cannot be removed from model.

#cylindernumberthree has VIF value 3.234758 and p-value 0.074857(which is >0.05) So remove it from the model
model_10 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + stroke + fuelsystem4bbl + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                enginetypel + enginetypeohcf + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + companynameaudi + 
                companynamebmw + companynamebuick + companynamedodge + 
                companynameisuzu + companynamejaguar + companynamemazda + 
                companynamemercury + companynamemitsubishi + companynamenissan + 
                companynameplymouth + companynamerenault + companynamesaab + 
                companynametoyota + companynamevolkswagen + companynamevolvo, 
              data = train)
summary(model_10)
sort(vif(model_10), decreasing = TRUE)

#carbodysedan, carbodyhatchback, curbweight, cylindernumberfour, carwidth, cylindernumbersix, 
#carbodywagon, companynamebuick, cylindernumberfive, enginetypeohcf, carbodyhardtop, stroke,
#companynametoyota, fuelsystem4bbl
#all have high VIF values but p-values are <0.05. So they cannot be removed from model.

#companynameaudi has VIF value 2.578511, and p-value 0.347057(which is >0.05) So remove it from the model
model_11 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + stroke + fuelsystem4bbl + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 enginetypel + enginetypeohcf + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + 
                 companynamebmw + companynamebuick + companynamedodge + 
                 companynameisuzu + companynamejaguar + companynamemazda + 
                 companynamemercury + companynamemitsubishi + companynamenissan + 
                 companynameplymouth + companynamerenault + companynamesaab + 
                 companynametoyota + companynamevolkswagen + companynamevolvo, 
               data = train)
summary(model_11)
sort(vif(model_11), decreasing = TRUE)

#carbodysedan, carbodyhatchback, curbweight, cylindernumberfour, carwidth, cylindernumbersix, 
#carbodywagon, companynamebuick, cylindernumberfive, enginetypeohcf, carbodyhardtop, stroke,
#companynametoyota, fuelsystem4bbl, enginelocation
#all have high VIF values but p-values are <0.05. So they cannot be removed from model.

#companynamevolvo has VIF value 2.249237 and p-value 0.239774(which is >0.05) So remove it from the model
model_12 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + stroke + fuelsystem4bbl + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 enginetypel + enginetypeohcf + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + 
                 companynamebmw + companynamebuick + companynamedodge + 
                 companynameisuzu + companynamejaguar + companynamemazda + 
                 companynamemercury + companynamemitsubishi + companynamenissan + 
                 companynameplymouth + companynamerenault + companynamesaab + 
                 companynametoyota + companynamevolkswagen, 
               data = train)
summary(model_12)
sort(vif(model_12), decreasing = TRUE)

#carbodysedan, carbodyhatchback, curbweight, cylindernumberfour, carwidth, cylindernumbersix, 
#carbodywagon, companynamebuick, cylindernumberfive, enginetypeohcf, carbodyhardtop, stroke,
#companynametoyota, fuelsystem4bbl, enginelocation
#have high VIF values (>2) but p-values are <0.05. So they cannot be removed from model.

#All other variables have VIF values between 1 and 2. 
#Next, check for correlation between independent variables
#check for first two highest VIF value variables
cor(train$carbodyhatchback, train$carbodysedan)
#There is a correlation of about 64%
#Remove variable with higher p-value among the two
#carbodyhatchback p-value is 4.43e-05
#carbodysedan p-value is 8.46e-05
#remove carbodysedan from model
model_13 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + stroke + fuelsystem4bbl + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 enginetypel + enginetypeohcf + carbodyhardtop + 
                 carbodyhatchback + carbodywagon + 
                 companynamebmw + companynamebuick + companynamedodge + 
                 companynameisuzu + companynamejaguar + companynamemazda + 
                 companynamemercury + companynamemitsubishi + companynamenissan + 
                 companynameplymouth + companynamerenault + companynamesaab + 
                 companynametoyota + companynamevolkswagen, 
               data = train)
summary(model_13)
sort(vif(model_13), decreasing = TRUE)

#check for correlation between first two highest VIF value variables
cor(train$curbweight, train$cylindernumberfour)
#There is a correlation of about 58%
#Remove variable with higher p-value among the two
#curbweight p-value is 4.80e-13
#cylindernumberfour p-value is 1.19e-10
#remove cylindernumberfour from model
model_14 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + stroke + fuelsystem4bbl + 
                 cylindernumberfive + cylindernumbersix + 
                 enginetypel + enginetypeohcf + carbodyhardtop + 
                 carbodyhatchback + carbodywagon + 
                 companynamebmw + companynamebuick + companynamedodge + 
                 companynameisuzu + companynamejaguar + companynamemazda + 
                 companynamemercury + companynamemitsubishi + companynamenissan + 
                 companynameplymouth + companynamerenault + companynamesaab + 
                 companynametoyota + companynamevolkswagen, 
               data = train)
sort(vif(model_14), decreasing = TRUE)
summary(model_14)

#check for correlation between first two highest VIF value variables
cor(train$curbweight, train$carwidth)
#There is a correlation of about 89%
#Remove variable with higher p-value among the two
#remove carwidth from model
model_15 <- lm(formula = price ~ aspiration + enginelocation + 
                 curbweight + stroke + fuelsystem4bbl + 
                 cylindernumberfive + cylindernumbersix + 
                 enginetypel + enginetypeohcf + carbodyhardtop + 
                 carbodyhatchback + carbodywagon + 
                 companynamebmw + companynamebuick + companynamedodge + 
                 companynameisuzu + companynamejaguar + companynamemazda + 
                 companynamemercury + companynamemitsubishi + companynamenissan + 
                 companynameplymouth + companynamerenault + companynamesaab + 
                 companynametoyota + companynamevolkswagen, 
               data = train)
sort(vif(model_15), decreasing = TRUE)
summary(model_15)

#check for highest VIF value variables
#Remove variable with higher p-value among the two
#remove cylindernumbersix from model
model_16 <- lm(formula = price ~ aspiration + enginelocation + 
                 curbweight + stroke + fuelsystem4bbl + 
                 cylindernumberfive + 
                 enginetypel + enginetypeohcf + carbodyhardtop + 
                 carbodyhatchback + carbodywagon + 
                 companynamebmw + companynamebuick + companynamedodge + 
                 companynameisuzu + companynamejaguar + companynamemazda + 
                 companynamemercury + companynamemitsubishi + companynamenissan + 
                 companynameplymouth + companynamerenault + companynamesaab + 
                 companynametoyota + companynamevolkswagen, 
               data = train)
sort(vif(model_16), decreasing = TRUE)
summary(model_16)

#check for correlation between first two highest VIF value variables
cor(train$curbweight, train$stroke)
#Not high correlation (~21%)
cor(train$stroke, train$enginetypeohcf)
#Not high correlation

#Check for highest p-value among variables
#Remove aspration as p-value is 0.988
model_17 <- lm(formula = price ~ enginelocation + 
                 curbweight + stroke + fuelsystem4bbl + 
                 cylindernumberfive + 
                 enginetypel + enginetypeohcf + carbodyhardtop + 
                 carbodyhatchback + carbodywagon + 
                 companynamebmw + companynamebuick + companynamedodge + 
                 companynameisuzu + companynamejaguar + companynamemazda + 
                 companynamemercury + companynamemitsubishi + companynamenissan + 
                 companynameplymouth + companynamerenault + companynamesaab + 
                 companynametoyota + companynamevolkswagen, 
               data = train)
sort(vif(model_17), decreasing = TRUE)
summary(model_17)

#check for correlation between highest VIF value variables
cor(train$stroke, train$enginetypeohcf)
cor(train$enginetypeohcf, train$curbweight)
#Not highly correlated
#Check for highest p-value among variables
#Remove carbodyhardtop as p-value is 0.653291
model_18 <- lm(formula = price ~ enginelocation + 
                 curbweight + stroke + fuelsystem4bbl + 
                 cylindernumberfive + 
                 enginetypel + enginetypeohcf +
                 carbodyhatchback + carbodywagon + 
                 companynamebmw + companynamebuick + companynamedodge + 
                 companynameisuzu + companynamejaguar + companynamemazda + 
                 companynamemercury + companynamemitsubishi + companynamenissan + 
                 companynameplymouth + companynamerenault + companynamesaab + 
                 companynametoyota + companynamevolkswagen, 
               data = train)
sort(vif(model_18), decreasing = TRUE)
summary(model_18)

# Checking Correlation among variable having high VIF values
cor(train$enginetypeohcf, train$stroke)
cor(train$stroke, train$curbweight)
#no high correlation
#Check for highest p-value among variables
#Remove carbodyhatchback as p-value is 0.58858
model_19 <- lm(formula = price ~ enginelocation + 
                 curbweight + stroke + fuelsystem4bbl + 
                 cylindernumberfive + 
                 enginetypel + enginetypeohcf +
                 carbodywagon + 
                 companynamebmw + companynamebuick + companynamedodge + 
                 companynameisuzu + companynamejaguar + companynamemazda + 
                 companynamemercury + companynamemitsubishi + companynamenissan + 
                 companynameplymouth + companynamerenault + companynamesaab + 
                 companynametoyota + companynamevolkswagen, 
               data = train)
sort(vif(model_19), decreasing = TRUE)
summary(model_19)

# Checking Correlation among variable having high VIF values
cor(train$enginetypeohcf, train$stroke)
cor(train$stroke, train$curbweight)
#no high correlation
#Check for highest p-value among variables
#Remove cylindernumberfive as p-value is 0.296674
model_20 <- lm(formula = price ~ enginelocation + 
                 curbweight + stroke + fuelsystem4bbl + 
                 enginetypel + enginetypeohcf +
                 carbodywagon + 
                 companynamebmw + companynamebuick + companynamedodge + 
                 companynameisuzu + companynamejaguar + companynamemazda + 
                 companynamemercury + companynamemitsubishi + companynamenissan + 
                 companynameplymouth + companynamerenault + companynamesaab + 
                 companynametoyota + companynamevolkswagen, 
               data = train)
sort(vif(model_20), decreasing = TRUE)
summary(model_20)

# Checking Correlation among variable having high VIF values
cor(train$enginetypeohcf, train$curbweight)
cor(train$stroke, train$curbweight)
#no high correlation
#Check for highest p-value among variables
#Remove companynamerenault as p-value is 0.297524
model_21 <- lm(formula = price ~ enginelocation + 
                 curbweight + stroke + fuelsystem4bbl + 
                 enginetypel + enginetypeohcf +
                 carbodywagon + 
                 companynamebmw + companynamebuick + companynamedodge + 
                 companynameisuzu + companynamejaguar + companynamemazda + 
                 companynamemercury + companynamemitsubishi + companynamenissan + 
                 companynameplymouth + companynamesaab + 
                 companynametoyota + companynamevolkswagen, 
               data = train)
sort(vif(model_21), decreasing = TRUE)
summary(model_21)

# Checking Correlation among variable having high VIF values
cor(train$enginetypeohcf, train$curbweight)
cor(train$stroke, train$curbweight)
#no high correlation
#Check for highest p-value among variables
#Remove companynamemercury as p-value is 0.258532
model_22 <- lm(formula = price ~ enginelocation + 
                 curbweight + stroke + fuelsystem4bbl + 
                 enginetypel + enginetypeohcf +
                 carbodywagon + 
                 companynamebmw + companynamebuick + companynamedodge + 
                 companynameisuzu + companynamejaguar + companynamemazda + 
                 companynamemitsubishi + companynamenissan + 
                 companynameplymouth + companynamesaab + 
                 companynametoyota + companynamevolkswagen, 
               data = train)
sort(vif(model_22), decreasing = TRUE)
summary(model_22)

# Checking Correlation among variable having high VIF values
cor(train$enginetypeohcf, train$curbweight)
cor(train$stroke, train$curbweight)
#no high correlation
#Check for highest p-value among variables
#Remove fuelsystem4bbl as p-value is 0.231236
model_23 <- lm(formula = price ~ enginelocation + 
                 curbweight + stroke +
                 enginetypel + enginetypeohcf +
                 carbodywagon + 
                 companynamebmw + companynamebuick + companynamedodge + 
                 companynameisuzu + companynamejaguar + companynamemazda + 
                 companynamemitsubishi + companynamenissan + 
                 companynameplymouth + companynamesaab + 
                 companynametoyota + companynamevolkswagen, 
               data = train)
sort(vif(model_23), decreasing = TRUE)
summary(model_23)

# Checking Correlation among variable having high VIF values
cor(train$enginetypeohcf, train$curbweight)
cor(train$stroke, train$curbweight)
#no high correlation
#Check for highest p-value among variables
#Remove companynamenissan as p-value is 0.171707
model_24 <- lm(formula = price ~ enginelocation + 
                 curbweight + stroke +
                 enginetypel + enginetypeohcf +
                 carbodywagon + 
                 companynamebmw + companynamebuick + companynamedodge + 
                 companynameisuzu + companynamejaguar + companynamemazda + 
                 companynamemitsubishi + 
                 companynameplymouth + companynamesaab + 
                 companynametoyota + companynamevolkswagen, 
               data = train)
sort(vif(model_24), decreasing = TRUE)
summary(model_24)

# Checking Correlation among variable having high VIF values
cor(train$enginetypeohcf, train$curbweight)
cor(train$stroke, train$curbweight)
#no high correlation
#Check for highest p-value among variables
#Remove companynamedodge as p-value is 0.113548
model_25 <- lm(formula = price ~ enginelocation + 
                 curbweight + stroke +
                 enginetypel + enginetypeohcf +
                 carbodywagon + 
                 companynamebmw + companynamebuick+
                 companynameisuzu + companynamejaguar + companynamemazda + 
                 companynamemitsubishi + 
                 companynameplymouth + companynamesaab + 
                 companynametoyota + companynamevolkswagen, 
               data = train)
sort(vif(model_25), decreasing = TRUE)
summary(model_25)

# Checking Correlation among variable having high VIF values
cor(train$enginetypeohcf, train$curbweight)
cor(train$stroke, train$curbweight)
#no high correlation
#Check for highest p-value among variables
#Remove companynamesaab as p-value is 0.107023
model_26 <- lm(formula = price ~ enginelocation + 
                 curbweight + stroke +
                 enginetypel + enginetypeohcf +
                 carbodywagon + 
                 companynamebmw + companynamebuick+
                 companynameisuzu + companynamejaguar + companynamemazda + 
                 companynamemitsubishi + 
                 companynameplymouth +
                 companynametoyota + companynamevolkswagen, 
               data = train)
sort(vif(model_26), decreasing = TRUE)
summary(model_26)

# Checking Correlation among variable having high VIF values
cor(train$enginetypeohcf, train$curbweight)
cor(train$stroke, train$curbweight)
#no high correlation
#Check for highest p-value among variables
#Remove companynamemazda as p-value is 0.125890
model_27 <- lm(formula = price ~ enginelocation + 
                 curbweight + stroke +
                 enginetypel + enginetypeohcf +
                 carbodywagon + 
                 companynamebmw + companynamebuick+
                 companynameisuzu + companynamejaguar+
                 companynamemitsubishi + 
                 companynameplymouth +
                 companynametoyota + companynamevolkswagen, 
               data = train)
sort(vif(model_27), decreasing = TRUE)
summary(model_27)


# Checking Correlation among variable having high VIF values
cor(train$enginetypeohcf, train$curbweight)
cor(train$stroke, train$curbweight)
#no high correlation
#Check for highest p-value among variables
#Remove companynamevolkswagen as p-value is 0.154026
model_28 <- lm(formula = price ~ enginelocation + 
                 curbweight + stroke +
                 enginetypel + enginetypeohcf +
                 carbodywagon + 
                 companynamebmw + companynamebuick+
                 companynameisuzu + companynamejaguar+
                 companynamemitsubishi + companynameplymouth+
                 companynametoyota, 
               data = train)
sort(vif(model_28), decreasing = TRUE)
summary(model_28)

# Checking Correlation among variable having high VIF values
cor(train$enginetypeohcf, train$curbweight)
cor(train$stroke, train$curbweight)
#no high correlation
#Check for highest p-value among variables
#Remove companynameplymouth as p-value is 0.165990
model_29 <- lm(formula = price ~ enginelocation + curbweight + stroke +
                 enginetypel + enginetypeohcf + carbodywagon + 
                 companynamebmw + companynamebuick+
                 companynameisuzu + companynamejaguar+
                 companynamemitsubishi + companynametoyota, 
               data = train)
sort(vif(model_29), decreasing = TRUE)
summary(model_29)
  
# Checking Correlation among variable having high VIF values
cor(train$enginetypeohcf, train$curbweight)
#No Correlation

#All p-values are below 0.05 and are significant
#There are a final of 12 variables in model_29 which is the final model with 
#Adjusted R-squared values is 0.9392

#Use model_29 to predict price values
Predict_price <- predict(model_29,test[,-which(names(test)=="price")])

#To check accuracy of prediction, find correlation between actual and predicted value (r squared value)
rsquared_value <- (cor(test$price,Predict_price))^2
rsquared_value

#R squared value of test data is ~87% which is quite close to training data r-squared value of ~93%
#The independent variables in model_29 are driving factors for the price
#Thus we have the final equation as per Linear Regresion obtained from the final model_29 is as below
# price = (20862.4366 * enginelocation) + (10.9389 * carweight) + (-2762.1029 * stroke) + 
#         (-4420.2623 * enginetypel) + (-3232.7043 * enginetypeohcf) + (-2507.2509 * carbodywagon) + 
#         (7460.0005 * companynamebmw) + (6828.4227 * companynamebuick) + (-4385.1449 * companynameisuzu) + 
#         (6412.9037 * companynamejaguar) + (-1850.2018 * companynamemitsubishi) + 
#         (-2196.2495 * companynametoyota) + (-5472.4314)

#Plot the actual and prediced values
residualPlot(model_29)
#Residual plot is quite scattered and thus can be considered as white noise