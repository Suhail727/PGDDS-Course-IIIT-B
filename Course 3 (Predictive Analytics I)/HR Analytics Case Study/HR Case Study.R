#############################XYZ Company Attrition ###########################
##############################################################################
#Business Understanding
#Data Understanding
#Data Preparation and EDA
#Model Building 
#Model Evaluation
######################################################################################################
############   Business Understanding   ############
######################################################################################################

#A large company named XYZ, employs, at any given point of time, around 4000 employees. However,
#every year, around 15% of its employees leave the company and need to be replaced with the 
#talent pool available in the job market. The management believes that this level of attrition 
#(employees leaving, either on their own or because they got fired) is bad for the company


## AIM:
#Required to model the probability of attrition using a logistic regression. The results thus 
#obtained will be used by the management to understand what changes they should make to their 
#workplace, in order to get most of their employees to stay.


######################################################################################################
############   Data Understanding   ############
######################################################################################################

library(MASS)
library(ggplot2)
library(stringr)
library(dplyr)
library(cowplot)
library(GGally)
library(car)
library(e1071)
library(caTools)
library(caret)
library(ROCR)


# Loading the given data
employee_survey_data<- read.csv("employee_survey_data.csv", stringsAsFactors = T)
general_data        <- read.csv("general_data.csv", stringsAsFactors = T) 
manager_survey_data <- read.csv("manager_survey_data.csv", stringsAsFactors = T)
in_time             <- read.csv("in_time.csv", stringsAsFactors = T)
out_time            <- read.csv("out_time.csv", stringsAsFactors = T)

str(employee_survey_data)    # 4410 obs of 4 variables 
str(general_data)            # 4410 obs of 24 variables including the target variable
str(manager_survey_data)     # 4410 obs of 3 variables
str(in_time)                 # 4410 obs of 262 variables
str(out_time)                # 4410 obs of 262 variables

#Rename the first column as EmployeeID for both in_time and out_time datasets
names(in_time) <- str_replace(names(in_time),"X","in")
names(out_time) <- str_replace(names(out_time),"X","out")
colnames(in_time)[1] <- "EmployeeID"
colnames(out_time)[1] <- "EmployeeID"

#####Intime and Outtime data cleaning and derived metrics
#check for leave
sum(is.na(in_time) != is.na(out_time))

ofc_hours <- data.frame(matrix(ncol = ncol(in_time), nrow = nrow(in_time)))
names(ofc_hours) <- str_replace(names(in_time),"in","hrs")
ofc_hours$EmployeeID <- in_time$EmployeeID
for(i in 2:ncol(ofc_hours)) {
  ofc_hours[,i] <- round(as.POSIXct(out_time[,i], format = "%Y-%m-%d %H:%M") - as.POSIXct(in_time[,i], 
                                                                                          format = "%Y-%m-%d %H:%M"),2)
}
ofc_hours <- sapply(ofc_hours, as.numeric)
ofc_hours <- as.data.frame(ofc_hours)

#Get total leave days leaving out the 12 national holidays
ofc_hours$leave_count   <- apply(ofc_hours[,2:262],1,function(x) {sum(is.na(x))-12})
# Get avarage working time of each employee
ofc_hours$avg_ofc_hours <- apply(ofc_hours[,2:262],1,function(x) {mean(x,na.rm = T)})

#####Other attributes

length(unique(tolower(employee_survey_data$EmployeeID))) 
length(unique(tolower(general_data$EmployeeID)))           
length(unique(tolower(manager_survey_data$EmployeeID))) 
setdiff(general_data$EmployeeID,employee_survey_data$EmployeeID)
setdiff(general_data$EmployeeID,manager_survey_data$EmployeeID)
#shows identical employee ID's across data

#Merging the data
hr_attrition  <- merge(general_data,employee_survey_data, by="EmployeeID", all = F)
hr_attrition  <- merge(hr_attrition,manager_survey_data, by="EmployeeID", all = F)

#Adding leave count and average office hours 
ofc_hours <- ofc_hours[order(ofc_hours$EmployeeID),] 
sum(hr_attrition$EmployeeID != ofc_hours$EmployeeID)  
hr_attrition$leave_count    <- ofc_hours$leave_count
hr_attrition$avg_ofc_hours  <- ofc_hours$avg_ofc_hours  

#set avg_ofc_hours to 0 and 1 depending on working for less than 8 hours or more
hr_attrition$avg_ofc_hours <- ifelse(hr_attrition$avg_ofc_hours >= 8,1,0)

######################################################################################################
############   Data Preparation and EDA   ############
######################################################################################################

# check for missing values
sum(is.na(hr_attrition))
#111 NA values


anyNA(hr_attrition$Age)                            
anyNA(hr_attrition$Attrition)                      
anyNA(hr_attrition$BusinessTravel)                 
anyNA(hr_attrition$Department)                     
anyNA(hr_attrition$DistanceFromHome)               
anyNA(hr_attrition$Education)                      
anyNA(hr_attrition$EducationField)                 
anyNA(hr_attrition$EmployeeCount)                  
anyNA(hr_attrition$EmployeeNumber)                 
anyNA(hr_attrition$EnvironmentSatisfaction)        #Has Missing values
anyNA(hr_attrition$Gender)                         
anyNA(hr_attrition$JobInvolvement)                 
anyNA(hr_attrition$JobLevel)                       
anyNA(hr_attrition$JobRole)                        
anyNA(hr_attrition$JobSatisfaction)                #Has Missing Values
anyNA(hr_attrition$MaritalStatus)                  
anyNA(hr_attrition$MonthlyIncome)                  
anyNA(hr_attrition$NumCompaniesWorked)             #Has Missing values
anyNA(hr_attrition$Over18)                         
anyNA(hr_attrition$PercentSalaryHike)              
anyNA(hr_attrition$PerformanceRating)              
anyNA(hr_attrition$RelationshipSatisfaction)       
anyNA(hr_attrition$StandardHours)                  
anyNA(hr_attrition$StockOptionLevel)               
anyNA(hr_attrition$TotalWorkingYears)              #Has Missing values
anyNA(hr_attrition$TrainingTimesLastYear)          
anyNA(hr_attrition$WorkLifeBalance)                #Has Missing values  
anyNA(hr_attrition$YearsAtCompany)                 
anyNA(hr_attrition$YearsSinceLastPromotion)        
anyNA(hr_attrition$YearsWithCurrManager)           

#cleaning NA for all variables of above by replacing with median or mean
hr_attrition$JobSatisfaction[which(is.na(hr_attrition$JobSatisfaction))]<-
  median(hr_attrition$JobSatisfaction,na.rm = TRUE)
hr_attrition$WorkLifeBalance[which(is.na(hr_attrition$WorkLifeBalance))]<-
  median(hr_attrition$WorkLifeBalance,na.rm = TRUE)
hr_attrition$NumCompaniesWorked[which(is.na(hr_attrition$NumCompaniesWorked))]<-
  mean(hr_attrition$NumCompaniesWorked,na.rm = TRUE)
hr_attrition$TotalWorkingYears[which(is.na(hr_attrition$TotalWorkingYears))]<-
  mean(hr_attrition$TotalWorkingYears,na.rm = TRUE)
hr_attrition$EnvironmentSatisfaction[which(is.na(hr_attrition$EnvironmentSatisfaction))]<-
  median(hr_attrition$EnvironmentSatisfaction,na.rm = TRUE)

#Check for NA values again
sum(is.na(hr_attrition))
#Check for duplicates
which(duplicated(hr_attrition))

#####Bar Charts 
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

# Barchart of Education, Education field and Job level against Attrition
plot_grid(ggplot(hr_attrition, aes(x=Education,fill=factor(Attrition)))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(hr_attrition, aes(x=EducationField,fill=factor(Attrition)))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(hr_attrition, aes(x=JobLevel,fill=factor(Attrition)))+ geom_bar(position = "fill"),
          align = "h") 
#Higher Attrition factors
# Education 2 
# Education Filed-Human Resource 
# job level 2 

# Barchart of jobrole, Marital status, Business travel and Department against Attrition
plot_grid(ggplot(hr_attrition, aes(x=JobRole,fill=factor(Attrition)))+ geom_bar(position = "fill")+bar_theme1, 
          ggplot(hr_attrition, aes(x=MaritalStatus,fill=factor(Attrition)))+ geom_bar(position = "fill"),
          ggplot(hr_attrition, aes(x=BusinessTravel,fill=factor(Attrition)))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(hr_attrition, aes(x=Department,fill=factor(Attrition)))+bar_theme1+ geom_bar(position = "fill"),
          align = "h")
#Higher Attrition factors
# Job roles with  Research Director 
# Marital status - Single
# Employees with Business travel
# HR department 


# Barchart of EnvironmentSatisfaction, JobSatisfaction and WorkLifeBalance against Attrition
plot_grid(ggplot(hr_attrition, aes(x=EnvironmentSatisfaction,fill=factor(Attrition)))+geom_bar(position = "fill")+bar_theme1,
          ggplot(hr_attrition, aes(x=JobSatisfaction,fill=factor(Attrition)))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(hr_attrition, aes(x=WorkLifeBalance,fill=factor(Attrition)))+ geom_bar(position = "fill"),
          align = "h")  

# Barchart of JobInvolvement, PerformanceRating against Attrition
plot_grid(ggplot(hr_attrition, aes(x=JobInvolvement,fill=factor(Attrition)))+geom_bar(position = "fill")+bar_theme1,
          ggplot(hr_attrition, aes(x=PerformanceRating,fill=factor(Attrition)))+ geom_bar(position = "fill"),
          align = "h")   
#Less job involvement, more attrition


#####Boxplots and histogram
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())
box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

# Age
plot_grid(ggplot(hr_attrition, aes(Age))+ geom_histogram(binwidth = 10),
          ggplot(hr_attrition, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
# 30 to 45 age group employees are more in the organisation

#DistanceFromHome
plot_grid(ggplot(hr_attrition, aes(DistanceFromHome))+ geom_histogram(binwidth = 10),
          ggplot(hr_attrition, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
#Most employees are near the organization

#MonthlyIncome
plot_grid(ggplot(hr_attrition, aes(MonthlyIncome))+ geom_histogram(binwidth = 10000),
          ggplot(hr_attrition, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
#More number of less income employees

#NumCompaniesWorked
plot_grid(ggplot(hr_attrition, aes(NumCompaniesWorked))+ geom_histogram(binwidth = 2),
          ggplot(hr_attrition, aes(x="",y=NumCompaniesWorked))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
#This is first company for most employees

#PercentSalaryHike
plot_grid(ggplot(hr_attrition, aes(PercentSalaryHike))+ geom_histogram(binwidth = 10),
          ggplot(hr_attrition, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
# Most employees get less than 15%

# Leave count
plot_grid(ggplot(hr_attrition, aes(leave_count))+ geom_histogram(binwidth = 10),
          ggplot(hr_attrition, aes(x="",y=leave_count))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#TrainingTimesLastYear
plot_grid(ggplot(hr_attrition, aes(TrainingTimesLastYear))+ geom_histogram(binwidth = 2),
          ggplot(hr_attrition, aes(x="",y=TrainingTimesLastYear))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#YearsAtCompany
plot_grid(ggplot(hr_attrition, aes(YearsAtCompany))+ geom_histogram(binwidth = 10),
          ggplot(hr_attrition, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
# More employees spend less than 5 years working for the company 

#YearsSinceLastPromotion
plot_grid(ggplot(hr_attrition, aes(YearsSinceLastPromotion))+ geom_histogram(binwidth = 1),
          ggplot(hr_attrition, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
# Employees are more when years since promotion is less than 3

#TotalWorkingYears
plot_grid(ggplot(hr_attrition, aes(TotalWorkingYears))+ geom_histogram(binwidth = 1),
          ggplot(hr_attrition, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
#Employees with 10 years of ecperience are highest

#####Checking for Correlation
ggpairs(hr_attrition[, c("Age", "DistanceFromHome", "MonthlyIncome","NumCompaniesWorked",
                         "YearsWithCurrManager","PercentSalaryHike","TrainingTimesLastYear",
                         "YearsAtCompany","YearsSinceLastPromotion","TotalWorkingYears")])
#Years at company and years with current manager are highly correlated (corr 0.77)
#Total working years and years at company (corr 0.63)
#Years since last promotion and Years in company (corr 0.62)

#####Outliers
View(sapply(hr_attrition[,c(2,6,7,14,15,17,19,20,22:24,30)], 
            function(x) quantile(x,seq(0,1,.01),na.rm = T)))

#TotalWorkingYears has got some outliers after 99 percentile, replace with 99% value
quantile(hr_attrition$TotalWorkingYears,seq(0,1,0.01))
hr_attrition$TotalWorkingYears[which(hr_attrition$TotalWorkingYears > 35)] <- 35

#YearsAtCompany has got some outliers after 98 percentile, replace with 98% value
quantile(hr_attrition$YearsAtCompany,seq(0,1,0.01))
hr_attrition$YearsAtCompany[which(hr_attrition$YearsAtCompany > 24)] <- 24

#YearsWithCurrManager has got some outliers after 99 percentile, replace with 99% value
quantile(hr_attrition$YearsWithCurrManager,seq(0,1,0.01))
hr_attrition$YearsWithCurrManager[which(hr_attrition$YearsWithCurrManager > 14)] <- 14

######Standardization
# Normalising continuous features 
hr_attrition$Age<- scale(hr_attrition$Age) 
hr_attrition$DistanceFromHome<- scale(hr_attrition$DistanceFromHome)
hr_attrition$MonthlyIncome<- scale(hr_attrition$MonthlyIncome)
hr_attrition$NumCompaniesWorked<- scale(hr_attrition$NumCompaniesWorked)
hr_attrition$PercentSalaryHike<- scale(hr_attrition$PercentSalaryHike)
hr_attrition$StockOptionLevel<- scale(hr_attrition$StockOptionLevel)
hr_attrition$TotalWorkingYears<- scale(hr_attrition$TotalWorkingYears)
hr_attrition$TrainingTimesLastYear<- scale(hr_attrition$TrainingTimesLastYear)
hr_attrition$YearsAtCompany<- scale(hr_attrition$YearsAtCompany)
hr_attrition$YearsSinceLastPromotion<- scale(hr_attrition$YearsSinceLastPromotion)
hr_attrition$YearsWithCurrManager<- scale(hr_attrition$YearsWithCurrManager)
hr_attrition$leave_count<- scale(hr_attrition$leave_count)

str(hr_attrition)

hr_attrition$Attrition <- ifelse(hr_attrition$Attrition == "Yes", 1,0)
hr_attrition$Gender    <- ifelse(hr_attrition$Gender == "Male",1,0)
hr_attrition$Over18    <- ifelse(hr_attrition$Over18 == "Y", 1,0)

# creating a dataframe of categorical features
hr_categorical <- hr_attrition[,c(25,26,27,12,13,4,5,7,8,28,11,29)]

# converting categorical attributes to factor
hr_categorical_fact<- data.frame(sapply(hr_categorical, function(x) factor(x)))
str(hr_categorical_fact)

# creating dummy variables
dummies<- data.frame(sapply(hr_categorical_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =hr_categorical_fact))[,-1]))

#Final dataframe
hr_attrition_final<- cbind(hr_attrition[,-c(25,26,27,12,13,4,5,7,8,28,11,29)],dummies) 
hr_attrition_final <- hr_attrition_final[,-1] #remove employee id
str(hr_attrition_final)

######################################################################################################
############   Model Building   ############
######################################################################################################

set.seed(100)
#Random data for training and test
indices = sample.split(hr_attrition_final$Attrition, SplitRatio = 0.7)
# generate the train and test data
train = hr_attrition_final[indices,]
test = hr_attrition_final[!(indices),]

#Model1
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) 

# Stepwise selection
model_2<- stepAIC(model_1, direction="both")
summary(model_2)

# Removing multicollinearity
sort(vif(model_2),decreasing = T)

# Removing EducationField.xLife.Sciences as VIF is 15.27 and also the p value is high 0.12
model_3 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                 avg_ofc_hours + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + Education.x5 + 
                 EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobInvolvement.x2 + JobInvolvement.x3 + 
                 JobLevel.x2, family = "binomial", data = train)
summary(model_3) 
sort(vif(model_3),decreasing = T)

# Remove YearsAtCompany has high vif 4.83 and P as 0.32
model_4 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 avg_ofc_hours + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + Education.x5 + 
                 EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobInvolvement.x2 + JobInvolvement.x3 + 
                 JobLevel.x2, family = "binomial", data = train)
summary(model_4) 
sort(vif(model_4),decreasing = T)

#Remove MaritalStatus.xMarried as it has got high VIF 2.14 and high p value 0.12
model_5 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 avg_ofc_hours + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Department.xResearch...Development + Department.xSales + Education.x5 + 
                 EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobInvolvement.x2 + JobInvolvement.x3 + 
                 JobLevel.x2, family = "binomial", data = train)
summary(model_5) 
sort(vif(model_5),decreasing = T)

#All variables wih high VIF have low p-value
#Check for correlation between the HIGH VIF variables
cor(hr_attrition_final$BusinessTravel.xTravel_Frequently,hr_attrition_final$BusinessTravel.xTravel_Rarely)
#-0.75 High negative correlation
#remove BusinessTravel.xTravel_Frequently 

model_7 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 avg_ofc_hours + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + Department.xSales 
               + Education.x5 +EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobInvolvement.x2 + JobInvolvement.x3 + 
                 JobLevel.x2, family = "binomial", data = train)
summary(model_7) 
sort(vif(model_7),decreasing = T)

#Remove JobInvolvement.x2 as P-value is 0.17
model_8 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 avg_ofc_hours + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + Department.xSales 
               + Education.x5 +EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobInvolvement.x3 + JobLevel.x2, family = "binomial", data = train)
summary(model_8) 
sort(vif(model_8),decreasing = T)

#All variables wih high VIF have low p-value
#Check for correlation between the HIGH VIF variables
cor(hr_attrition_final$Department.xSales,hr_attrition_final$Department.xResearch...Development)
#Very high negative correlation -0.91
#Remove Department.xSales 
model_9 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 avg_ofc_hours + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + Education.x5 
               + EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobInvolvement.x3 + JobLevel.x2, family = "binomial", data = train)
summary(model_9) 
sort(vif(model_9),decreasing = T)

#remove EducationField.xMedical as p value is 0.38
model_10 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  avg_ofc_hours + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + Education.x5 
                + EducationField.xMarketing + EducationField.xOther + 
                  EducationField.xTechnical.Degree + JobInvolvement.x3 + JobLevel.x2, family = "binomial", data = train)
summary(model_10) 
sort(vif(model_10),decreasing = T)

#remove JobInvolvement.x3 as p value is 0.305
model_11 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  avg_ofc_hours + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + Education.x5 
                + EducationField.xMarketing + EducationField.xOther + 
                  EducationField.xTechnical.Degree + JobLevel.x2, family = "binomial", data = train)
summary(model_11) 
sort(vif(model_11),decreasing = T)

#remove distance from home as p value is 0.098
model_12 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  avg_ofc_hours + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + Education.x5 
                + EducationField.xMarketing + EducationField.xOther + 
                  EducationField.xTechnical.Degree + JobLevel.x2, family = "binomial", data = train)
summary(model_12) 
sort(vif(model_12),decreasing = T)

#remove BusinessTravel.xTravel_Rarely as p value is 0.07
model_13 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  avg_ofc_hours + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  Department.xResearch...Development + Education.x5 + EducationField.xMarketing + EducationField.xOther + 
                  EducationField.xTechnical.Degree + JobLevel.x2, family = "binomial", data = train)
summary(model_13) 
sort(vif(model_13),decreasing = T)

#Remove Education.x5 which as p value is 0.068
model_14 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  avg_ofc_hours + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  Department.xResearch...Development + EducationField.xMarketing + EducationField.xOther + 
                  EducationField.xTechnical.Degree + JobLevel.x2, family = "binomial", data = train)
summary(model_14) 
sort(vif(model_14),decreasing = T)

#Remove EducationField.xTechnical.Degree as p value is 0.05
model_15 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  avg_ofc_hours + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  Department.xResearch...Development + EducationField.xMarketing + EducationField.xOther + 
                  JobLevel.x2, family = "binomial", data = train)
summary(model_15) 
sort(vif(model_15),decreasing = T)

#remove EducationField.xOther as p value is 0.06
model_16 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  avg_ofc_hours + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  Department.xResearch...Development + EducationField.xMarketing + 
                  JobLevel.x2, family = "binomial", data = train)
summary(model_16) 
sort(vif(model_16),decreasing = T)

#remove EducationField.xMarketing as p value is 0.06
model_17 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  avg_ofc_hours + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  Department.xResearch...Development + JobLevel.x2, family = "binomial", data = train)
summary(model_17)
sort(vif(model_17),decreasing = T)

#remove Department.xResearch...Development as p value is 0.19
model_18 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  avg_ofc_hours + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  JobLevel.x2, family = "binomial", data = train)
summary(model_18) 
sort(vif(model_18),decreasing = T)

#remove JobRole.xLaboratory.Technician as p value is 0.02 
model_19 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  avg_ofc_hours + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + JobLevel.x2, family = "binomial", data = train)
summary(model_19) 
sort(vif(model_19),decreasing = T)

#remove JobRole.xResearch.Scientist as p value is 0.05
model_20 <- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  avg_ofc_hours + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobRole.xResearch.Director + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + JobLevel.x2, family = "binomial", data = train)
summary(model_20) 
sort(vif(model_20),decreasing = T)

#remove NumCompaniesWorked as p value is 0.02
model_21 <- glm(formula = Attrition ~ Age + MonthlyIncome + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  avg_ofc_hours + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobRole.xResearch.Director + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + JobLevel.x2, family = "binomial", data = train)
summary(model_21) 
sort(vif(model_21),decreasing = T)

#remove MonthlyIncome as p value is 0.01
model_22 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  avg_ofc_hours + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobRole.xResearch.Director + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + JobLevel.x2, family = "binomial", data = train)
summary(model_22) 
sort(vif(model_22),decreasing = T)

#remove JobLevel.x2 as p value is relatively high at 0.003
model_23 <- glm(formula = Attrition ~ Age + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  avg_ofc_hours + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobRole.xResearch.Director + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle, family = "binomial", data = train)
summary(model_23) 
sort(vif(model_23),decreasing = T)

#remove JobRole.xResearch.Director as p value is relatively high at 0.001
model_24 <- glm(formula = Attrition ~ Age + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + avg_ofc_hours + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + WorkLifeBalance.x2 + 
                  WorkLifeBalance.x3 + WorkLifeBalance.x4 + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle, family = "binomial", data = train)
summary(model_24) 
sort(vif(model_24),decreasing = T)

#remove JobRole.xSales.Executive as p value is relatively high at 0.003
model_25 <- glm(formula = Attrition ~ Age + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + avg_ofc_hours + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + WorkLifeBalance.x2 + 
                  WorkLifeBalance.x3 + WorkLifeBalance.x4 + MaritalStatus.xSingle, family = "binomial", data = train)
summary(model_25) 
sort(vif(model_25),decreasing = T)

#Remove WorkLifeBalance.x4 as p value is relatively high at 0.002 
model_26 <- glm(formula = Attrition ~ Age + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + avg_ofc_hours + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + WorkLifeBalance.x2 + 
                  WorkLifeBalance.x3 + MaritalStatus.xSingle, family = "binomial", data = train)
summary(model_26) 
sort(vif(model_26),decreasing = T)

#remove WorkLifeBalance.x2 p value is relatively high at 0.008
model_27 <- glm(formula = Attrition ~ Age + TotalWorkingYears + TrainingTimesLastYear +  
                  YearsSinceLastPromotion + YearsWithCurrManager + avg_ofc_hours + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x3 + MaritalStatus.xSingle, family = "binomial", data = train)
summary(model_27) 
sort(vif(model_27),decreasing = T)

#Check for correlation between the HIGH VIF variables
cor(hr_attrition_final$Age,hr_attrition_final$TotalWorkingYears)
#remove TotalWorkingYears
model_28 <- glm(formula = Attrition ~ Age + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + avg_ofc_hours + EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + 
                  JobSatisfaction.x3 + JobSatisfaction.x4 + WorkLifeBalance.x3 + MaritalStatus.xSingle 
                , family = "binomial", data = train)

summary(model_28)
sort(vif(model_28),decreasing = T)

#Model 28 is the final model with 13 significant variables
final_model<- model_28

######################################################################################################
############   Model Evaluation   ############
######################################################################################################

str(test)
test_pred = predict(final_model, type = "response", 
                    newdata = test[,-2])

summary(test_pred)

test$prob <- test_pred

#### Use the probability cutoff of 50% 
test_pred_Attrition_50 <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_Attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))
table(test_actual_Attrition,test_pred_Attrition_50)
test_conf_50 <- confusionMatrix(test_pred_Attrition_50, test_actual_Attrition, positive = "Yes")
# Withith 50 percent cutoff
# Accuracy - 85%
# Sensitivity - 22%
# Specificity - 98%


#### Use the probability cutoff of 40% since sensitivity is very low
test_pred_Attrition_40 <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))
table(test_actual_Attrition,test_pred_Attrition_40)
test_conf_40 <- confusionMatrix(test_pred_Attrition_40, test_actual_Attrition, positive = "Yes")
# Withith 40 percent cutoff
# Accuracy - 85%
# Sensitivity - 31%
# Specificity - 95%

#Finding Optimal Cutoff value
perform_fn <- function(cutoff) 
{
  predicted_Attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_Attrition, test_actual_Attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}
# Creating cutoff values from 0.002365931 0.890867781 for plotting and initiallizing a matrix of 100 X 3.
summary(test_pred)
s = seq(.01,.80,length=100)
OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)+
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)+
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)+
lines(s,OUT[,2],col="darkgreen",lwd=2)+
lines(s,OUT[,3],col=4,lwd=2)+
box()+
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.02)]
cutoff <- min(cutoff)

#Use this min cutoff value
test_cutoff_Attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
table(test_actual_Attrition,test_cutoff_Attrition)
conf_final <- confusionMatrix(test_cutoff_Attrition, test_actual_Attrition, positive = "Yes")

acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]
acc
sens
spec

#####KS Statistic
test_cutoff_Attrition_KS <- ifelse(test_cutoff_Attrition=="Yes",1,0)
test_actual_Attrition_KS <- ifelse(test_actual_Attrition=="Yes",1,0)


pred_object_test<- prediction(test_cutoff_Attrition_KS, test_actual_Attrition_KS)
performance_measures_test<- performance(pred_object_test, "tpr", "fpr")
ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])
max(ks_table_test)
# 0.4928055

######Lift and Gain Charts
lift <- function(labels , predicted_prob,groups=10) {
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(test_actual_Attrition_KS, test_pred, groups = 10)

#Plotting the Chart
plot(Attrition_decile$Cumlift, type="l", lwd=2, col="red",# lty=4,
     xlim = c(0,10),
     ylim = c(0,4),
     main = "Lift Chart",
     xlab = "Decile",
     ylab = "Lift")+
abline(h=1, col="brown")+
axis(1, 1:10)+
abline(h=0:10, v=0:10, lty=3)

library(InformationValue)
ks_plot(test_actual_Attrition_KS, test_cutoff_Attrition_KS)
detach("package:InformationValue", unload=TRUE)

######################################################################################################
############   Leading Factors for Attrition   ############
#######################################################################################################                            COEFFICIENTS
#  Age                        -0.45799    
#  TrainingTimesLastYear      -0.18525    
#  YearsSinceLastPromotion     0.37649    
#  YearsWithCurrManager       -0.63028    
#  avg_ofc_hours               1.29066    
#  EnvironmentSatisfaction.x2 -0.92218    
#  EnvironmentSatisfaction.x3 -0.96936    
#  EnvironmentSatisfaction.x4 -1.19211    
#  JobSatisfaction.x2         -0.59763    
#  JobSatisfaction.x3         -0.50662    
#  JobSatisfaction.x4         -1.18175    
#  WorkLifeBalance.x3         -0.36410    
#  MaritalStatus.xSingle       1.04169    

#Age                        - Younger employees have high risk of leaving

#TrainingTimesLastYear      - More trainings, then employee tends to stay 

#Years Since Last Promotion - More frequent promotions, then employee tends to stay

#Years with Current manager - The higher the number of years with same manager, then employee tends to stay 

#Environment Satisfaction   - The higher, the lesser chances of leaving 

#Job Satisfaction           - The higher, the lesser chances of leaving

#Work life balance          - The higher, the lesser chances of leaving

#Average working hours      - The higher the employee works above 8 hours, the higher chances are of him leaving

#Marital status single      - Single Employees have higher chances of leaving the company.

