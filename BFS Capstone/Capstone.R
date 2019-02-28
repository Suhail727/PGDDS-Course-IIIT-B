# head(ddata)
# str(ddata)
# dim(ddata)
# sum(is.na(ddata))
# # 1428 NULL Values Present
# colSums(is.na(ddata))
# # No.of dependents- 3
# # Performance.tag- 1425  
# names(ddata)

# # install.packages("stringr")
# # install.packages("tidyr")
# # install.packages("plyr")
# # install.packages("dplyr", INSTALL_opts = c('--no-lock'))
# # install.packages("ggplot2")
# # install.packages("lubridate")
# # install.packages("corrplot")
# # install.packages("VIM")
# # install.packages("treemap")
# # install.packages("cowplot")
# # install.packages("Information")
# # install.packages("DMwR")
# # install.packages("merTools")
library(dplyr)
library(ggplot2)
library(cowplot)
library(corrplot)
library(GGally)
library(Information)
library(plyr)
library(caTools)
library(MASS)
library(car)
library(caret)

ddata <- read.csv("Demographic data.csv", stringsAsFactors = F,na.strings = c("NA",'',"Na",' '))
cbdata <- read.csv("Credit Bureau data.csv", stringsAsFactors = F,na.strings = c("NA",'',"Na",' '))

# Check the data
dim(ddata)
# Rows: 71295 Cols:12
dim(cbdata)
# Rows: 71295 Cols:19

# Check for unique rows
length(unique(ddata$Application.ID))
length(unique(cbdata$Application.ID))
# 71292 Unique values in both datasets. 3 Duplicates in both.

# Remove the duplicate Rows
ddata <- distinct(ddata, Application.ID,.keep_all = TRUE)
cbdata <- distinct(cbdata, Application.ID,.keep_all = TRUE)

# Remove Performance Tag from demographic data before joinin with credit bureau data
ddata <- ddata[ , !(names(ddata) %in% 'Performance.Tag')]

# Merge the data on ApplicationID
data <- merge(x = ddata, y = cbdata, by = "Application.ID")

# Have a look at the data
head(data)

####################### Data Cleaning ###################################
# Age 
summary(data$Age)
# Age has negative values also. Ideally, age should be grater than 18 to get credit card.
# Remove records where age < 18
data <- data %>% filter(Age>=18)
#----------------------------------------------------------------------------------------
# Income
summary(data$Income)
# Income cannot be negative. Remove those rows
data <- data %>% filter(Income>=0)
#----------------------------------------------------------------------------------------
# Check for NA Values
colSums(is.na(data))
# Gender- 1
# Marital Status- 5
# No of Dependents- 2
# Education- 119
# Profession- 13
# Type of Residence- 8
# Avgas CC Utilization in last 12 months- 1053
# No.of.trades.opened.in.last.6.months -1
# Presence.of.open.home.loan- 272
# Outstanding.Balance- 272
# Performance.Tag- 1425

# Separate the dataset with NA values in Performance tag for Testing the model
data_with_perf_tag_na <- data[which(is.na(data$Performance.Tag)),]

# Remove rows with NA for Performance Tag
data <- data[!(is.na(data$Performance.Tag)),]
summary(data)

# Remove other rows with NA values
data <- na.omit(data)
summary(data)


######### Find Outliers for numeric variables

#### Income 
plot_grid(ggplot(data, aes(Income))+ geom_histogram(binwidth = 12.5) + ggtitle("Income"),
          ggplot(data, aes(x="",y=Income))+ geom_boxplot(width=0.1)+ ggtitle("Income"), 
          align = "v",ncol = 2)
# No visible Outliers
#----------------------------------------------------------------------------------------
#### No.of.months.in.current.residence
plot_grid(ggplot(data, aes(No.of.months.in.current.residence))+ geom_histogram(binwidth = 15) + ggtitle("No.of.months.in.current.residence"),
          ggplot(data, aes(x="",y=No.of.months.in.current.residence))+ geom_boxplot(width=0.1)+ ggtitle("No.of.months.in.current.residence"), 
          align = "v",ncol = 2)
# No visible Outliers
#----------------------------------------------------------------------------------------
#### No.of.months.in.current.company
plot_grid(ggplot(data, aes(No.of.months.in.current.company))+ geom_histogram(binwidth = 15) + ggtitle("No.of.months.in.current.company"),
          ggplot(data, aes(x="",y=No.of.months.in.current.company))+ geom_boxplot(width=0.1)+ ggtitle("No.of.months.in.current.company"), 
          align = "v",ncol = 2)
# Data has few Outliers
quantile(data$No.of.months.in.current.company,seq(0,1,.01))
# Sudden jump from 99% to 100%. Cap values to 74
data$No.of.months.in.current.company[which(data$No.of.months.in.current.company>74)] <- 74
#----------------------------------------------------------------------------------------
#### Avgas.CC.Utilization.in.last.12.months
plot_grid(ggplot(data, aes(Avgas.CC.Utilization.in.last.12.months))+ geom_histogram(binwidth = 15) + ggtitle("Avgas.CC.Utilization.in.last.12.months"),
          ggplot(data, aes(x="",y=Avgas.CC.Utilization.in.last.12.months))+ geom_boxplot(width=0.1)+ ggtitle("Avgas.CC.Utilization.in.last.12.months"), 
          align = "v",ncol = 2)
# Data has few Outliers
quantile(data$Avgas.CC.Utilization.in.last.12.months,seq(0,1,.01))
# Sudden jump from 94% to 95%. Cap values to 91
data$Avgas.CC.Utilization.in.last.12.months[which(data$Avgas.CC.Utilization.in.last.12.months>91)] <- 91
#----------------------------------------------------------------------------------------
#### Outstanding.Balance
plot_grid(ggplot(data, aes(Outstanding.Balance))+ geom_histogram(binwidth = 60000) + ggtitle("Outstanding.Balance"),
          ggplot(data, aes(x="",y=Outstanding.Balance))+ geom_boxplot(width=0.1)+ ggtitle("Outstanding.Balance"), 
          align = "v",ncol = 2)
# No visible Outliers
#----------------------------------------------------------------------------------------
#### Total.No.of.Trades
plot_grid(ggplot(data, aes(Total.No.of.Trades))+ geom_histogram(binwidth = 10) + ggtitle("Total.No.of.Trades"),
          ggplot(data, aes(x="",y=Total.No.of.Trades))+ geom_boxplot(width=0.1)+ ggtitle("Total.No.of.Trades"), 
          align = "v",ncol = 2)
# Data has Outliers
quantile(data$Total.No.of.Trades,seq(0,1,.01))
# Sudden jump from 99% to 100%. Cap values to 31
data$Total.No.of.Trades[which(data$Total.No.of.Trades>31)] <- 31


######### Derived Variables
data <- mutate(data, age_group = ifelse(Age <=30, 'Young',
	if_else(Age > 30 & Age <= 60, 'MiddleAge','SeniorCitizen')))

data <- mutate(data, salary_group = ifelse(Income <=10, 'Low Income',
	if_else(Income > 10 & Income <= 30, 'Middle Income','High Income')))



####################### Plotting Data ###################################
# Correlation between numeric variables
ggpairs(data[, c('Age', 'No.of.dependents', 'Income', 'No.of.months.in.current.residence', 'No.of.months.in.current.company',
				'No.of.times.90.DPD.or.worse.in.last.6.months', 'No.of.times.60.DPD.or.worse.in.last.6.months',
				'No.of.times.30.DPD.or.worse.in.last.6.months', 'No.of.times.90.DPD.or.worse.in.last.12.months',
				'No.of.times.60.DPD.or.worse.in.last.12.months', 'No.of.times.30.DPD.or.worse.in.last.12.months',
				'Avgas.CC.Utilization.in.last.12.months','No.of.trades.opened.in.last.6.months',
				'No.of.trades.opened.in.last.12.months', 'No.of.PL.trades.opened.in.last.6.months',
				'No.of.PL.trades.opened.in.last.12.months', 'No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.',
				'No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.', 'Outstanding.Balance',
				'Total.No.of.Trades')])

# Gender vs Performance Tag
plot_1<- ggplot(data, aes(factor(Performance.Tag), group = Gender)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) +
  facet_grid(~Gender) +
  labs(title='Gender vs Performance Tag', x='Performance Tag',y='Percentage') + 
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  scale_fill_discrete(name = "Performance Tag")
plot_1

# Age Group vs Performance Tag
plot_2<- ggplot(data, aes(factor(Performance.Tag), group = age_group)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) +
  labs(title='Age Group vs Performance Tag', x='Performance Tag',y='Percentage') + 
  facet_grid(~age_group) +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  scale_fill_discrete(name = "Performance Tag")
plot_2

# Salary Group vs Performance Tag
plot_3<- ggplot(data, aes(factor(Performance.Tag), group = salary_group)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) +
  labs(title='Salary Group vs Performance Tag', x='Performance Tag',y='Percentage') + 
  facet_grid(~salary_group) +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  scale_fill_discrete(name = "Performance Tag")
plot_3

# Education vs Performance Tag
plot_4<- ggplot(data, aes(factor(Performance.Tag), group = Education)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) +
  labs(title='Education vs Performance Tag', x='Performance Tag',y='Percentage') + 
  facet_grid(~Education) +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  scale_fill_discrete(name = "Performance Tag")
plot_4

# Profession vs Performance Tag
plot_5<- ggplot(data, aes(factor(Performance.Tag), group = Profession)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) +
  labs(title='Profession vs Performance Tag', x='Performance Tag',y='Percentage') + 
  facet_grid(~Profession) +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  scale_fill_discrete(name = "Performance Tag")
plot_5

####################### WOE and IV ###################################
# Data for WOE after removing the derived variables
data_WOE <- data[ , -which(names(data) %in% c("age_group","salary_group"))]
summary(data_WOE)

# Binning
data_WOE$Age <- 
  as.factor(cut(data_WOE$Age, breaks = 15))

data_WOE$No.of.dependents <- 
  as.factor(cut(data_WOE$No.of.dependents, breaks = 6))

data_WOE$Income <- 
  as.factor(cut(data_WOE$Income, breaks = 15))

data_WOE$No.of.months.in.current.residence <- 
  as.factor(cut(data_WOE$No.of.months.in.current.residence, breaks = 15))

data_WOE$No.of.months.in.current.company <- 
  as.factor(cut(data_WOE$No.of.months.in.current.company, breaks = 15))

data_WOE$No.of.times.90.DPD.or.worse.in.last.6.months <- 
  as.factor(cut(data_WOE$No.of.times.90.DPD.or.worse.in.last.6.months, breaks = 8))

data_WOE$No.of.times.60.DPD.or.worse.in.last.6.months <- 
  as.factor(cut(data_WOE$No.of.times.60.DPD.or.worse.in.last.6.months, breaks = 8))

data_WOE$No.of.times.30.DPD.or.worse.in.last.6.months <- 
  as.factor(cut(data_WOE$No.of.times.30.DPD.or.worse.in.last.6.months, breaks = 8))

data_WOE$No.of.times.90.DPD.or.worse.in.last.12.months <- 
  as.factor(cut(data_WOE$No.of.times.90.DPD.or.worse.in.last.12.months, breaks = 10))

data_WOE$No.of.times.60.DPD.or.worse.in.last.12.months <- 
  as.factor(cut(data_WOE$No.of.times.60.DPD.or.worse.in.last.12.months, breaks = 10))

data_WOE$No.of.times.30.DPD.or.worse.in.last.12.months <- 
  as.factor(cut(data_WOE$No.of.times.30.DPD.or.worse.in.last.12.months, breaks = 10))

data_WOE$Avgas.CC.Utilization.in.last.12.months <- 
  as.factor(cut(data_WOE$Avgas.CC.Utilization.in.last.12.months, breaks = 10))

data_WOE$No.of.trades.opened.in.last.6.months <- 
  as.factor(cut(data_WOE$No.of.trades.opened.in.last.6.months, breaks = 10))

data_WOE$No.of.trades.opened.in.last.12.months <- 
  as.factor(cut(data_WOE$No.of.trades.opened.in.last.12.months, breaks = 10))

data_WOE$No.of.PL.trades.opened.in.last.6.months <- 
  as.factor(cut(data_WOE$No.of.PL.trades.opened.in.last.6.months, breaks = 10))

data_WOE$No.of.PL.trades.opened.in.last.12.months <- 
  as.factor(cut(data_WOE$No.of.PL.trades.opened.in.last.12.months, breaks = 10))

data_WOE$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <- 
  as.factor(cut(data_WOE$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., breaks = 15))

data_WOE$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- 
  as.factor(cut(data_WOE$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., breaks = 18))

data_WOE$Presence.of.open.home.loan <- 
  as.factor(cut(data_WOE$Presence.of.open.home.loan, breaks = 3))

data_WOE$Outstanding.Balance <- 
  as.factor(cut(data_WOE$Outstanding.Balance, breaks = 15))

data_WOE$Total.No.of.Trades <- 
  as.factor(cut(data_WOE$Total.No.of.Trades, breaks = 15))

data_WOE$Presence.of.open.auto.loan <- 
  as.factor(cut(data_WOE$Presence.of.open.auto.loan, breaks = 3))

IV <- create_infotables(data = data_WOE, y='Performance.Tag',bins = 10, parallel = FALSE)
IV_value <- data.frame(IV$Summary)

# Avgas.CC.Utilization.in.last.12.months	                        0.3196123
# No.of.trades.opened.in.last.12.months	                            0.3075795
# No.of.PL.trades.opened.in.last.12.months	                        0.2686921
# No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.	0.2663039
# Outstanding.Balance												0.2550174
# Total.No.of.Trades												0.2533396
# No.of.times.30.DPD.or.worse.in.last.6.months						0.2499031
# No.of.PL.trades.opened.in.last.6.months							0.2334098
# No.of.times.30.DPD.or.worse.in.last.12.months						0.2228656
# No.of.times.90.DPD.or.worse.in.last.12.months						0.2204230
# No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.	0.2188786
# No.of.times.60.DPD.or.worse.in.last.6.months						0.2156838

IV$Tables

#### Plotting Transformed Variables
plot_infotables(IV,"Age")
# Person in age 18-25 are more likely to default
plot_infotables(IV,"Gender")
# Male Customers have high chance of defaulting
plot_infotables(IV,"Marital.Status..at.the.time.of.application.")
# Single People have high WOE value
plot_infotables(IV,"No.of.dependents")
# People with around 2 dependents are more likely to default
plot_infotables(IV,"Income")
# Ironically people with Higher Incomes have low WOE value
plot_infotables(IV,"Education")
# "Others" has high WOE
plot_infotables(IV,"Profession")
# SE has high WOE
plot_infotables(IV,"Type.of.residence")
# People whose type of residence is not disclosed have low WOE
plot_infotables(IV,"No.of.months.in.current.residence")
plot_infotables(IV,"No.of.months.in.current.company")
# People with 50-60 months with current company have low WOE
plot_infotables(IV,"No.of.times.90.DPD.or.worse.in.last.6.months")
plot_infotables(IV,"No.of.times.60.DPD.or.worse.in.last.6.months")
plot_infotables(IV,"No.of.times.30.DPD.or.worse.in.last.6.months")

plot_infotables(IV,"No.of.times.90.DPD.or.worse.in.last.12.months")
plot_infotables(IV,"No.of.times.60.DPD.or.worse.in.last.12.months")
plot_infotables(IV,"No.of.times.30.DPD.or.worse.in.last.12.months")

plot_infotables(IV,"Avgas.CC.Utilization.in.last.12.months")

plot_infotables(IV,"No.of.trades.opened.in.last.6.months")

#Replacing the actual values of the variables with corresponding WOE values
data_WOE$Age <- mapvalues(data_WOE$Age, from = IV$Tables$Age$Age, to = IV$Tables$Age$WOE)
data_WOE$Gender <- mapvalues(data_WOE$Gender, from = IV$Tables$Gender$Gender, to = IV$Tables$Gender$WOE)
data_WOE$Marital.Status..at.the.time.of.application. <- mapvalues(data_WOE$Marital.Status..at.the.time.of.application., from = IV$Tables$Marital.Status..at.the.time.of.application.$Marital.Status..at.the.time.of.application., to = IV$Tables$Marital.Status..at.the.time.of.application.$WOE)
data_WOE$No.of.dependents <- mapvalues(data_WOE$No.of.dependents, from = IV$Tables$No.of.dependents$No.of.dependents, to = IV$Tables$No.of.dependents$WOE)
data_WOE$Income <- mapvalues(data_WOE$Income, from = IV$Tables$Income$Income, to = IV$Tables$Income$WOE)
data_WOE$Education <- mapvalues(data_WOE$Education, from = IV$Tables$Education$Education, to = IV$Tables$Education$WOE)
data_WOE$Profession <- mapvalues(data_WOE$Profession, from = IV$Tables$Profession$Profession, to = IV$Tables$Profession$WOE)
data_WOE$Type.of.residence <- mapvalues(data_WOE$Type.of.residence, from = IV$Tables$Type.of.residence$Type.of.residence, to = IV$Tables$Type.of.residence$WOE)
data_WOE$No.of.months.in.current.residence <- mapvalues(data_WOE$No.of.months.in.current.residence, from = IV$Tables$No.of.months.in.current.residence$No.of.months.in.current.residence, to = IV$Tables$No.of.months.in.current.residence$WOE)
data_WOE$No.of.months.in.current.company <- mapvalues(data_WOE$No.of.months.in.current.company, from = IV$Tables$No.of.months.in.current.company$No.of.months.in.current.company, to = IV$Tables$No.of.months.in.current.company$WOE)
data_WOE$No.of.times.90.DPD.or.worse.in.last.6.months <- mapvalues(data_WOE$No.of.times.90.DPD.or.worse.in.last.6.months, from = IV$Tables$No.of.times.90.DPD.or.worse.in.last.6.months$No.of.times.90.DPD.or.worse.in.last.6.months, to = IV$Tables$No.of.times.90.DPD.or.worse.in.last.6.months$WOE)
data_WOE$No.of.times.60.DPD.or.worse.in.last.6.months <- mapvalues(data_WOE$No.of.times.60.DPD.or.worse.in.last.6.months, from = IV$Tables$No.of.times.60.DPD.or.worse.in.last.6.months$No.of.times.60.DPD.or.worse.in.last.6.months, to = IV$Tables$No.of.times.60.DPD.or.worse.in.last.6.months$WOE)
data_WOE$No.of.times.30.DPD.or.worse.in.last.6.months <- mapvalues(data_WOE$No.of.times.30.DPD.or.worse.in.last.6.months, from = IV$Tables$No.of.times.30.DPD.or.worse.in.last.6.months$No.of.times.30.DPD.or.worse.in.last.6.months, to = IV$Tables$No.of.times.30.DPD.or.worse.in.last.6.months$WOE)
data_WOE$No.of.times.90.DPD.or.worse.in.last.12.months <- mapvalues(data_WOE$No.of.times.90.DPD.or.worse.in.last.12.months, from = IV$Tables$No.of.times.90.DPD.or.worse.in.last.12.months$No.of.times.90.DPD.or.worse.in.last.12.months, to = IV$Tables$No.of.times.90.DPD.or.worse.in.last.12.months$WOE)
data_WOE$No.of.times.60.DPD.or.worse.in.last.12.months <- mapvalues(data_WOE$No.of.times.60.DPD.or.worse.in.last.12.months, from = IV$Tables$No.of.times.60.DPD.or.worse.in.last.12.months$No.of.times.60.DPD.or.worse.in.last.12.months, to = IV$Tables$No.of.times.60.DPD.or.worse.in.last.12.months$WOE)
data_WOE$No.of.times.30.DPD.or.worse.in.last.12.months <- mapvalues(data_WOE$No.of.times.30.DPD.or.worse.in.last.12.months, from = IV$Tables$No.of.times.30.DPD.or.worse.in.last.12.months$No.of.times.30.DPD.or.worse.in.last.12.months, to = IV$Tables$No.of.times.30.DPD.or.worse.in.last.12.months$WOE)
data_WOE$Avgas.CC.Utilization.in.last.12.months <- mapvalues(data_WOE$Avgas.CC.Utilization.in.last.12.months, from = IV$Tables$Avgas.CC.Utilization.in.last.12.months$Avgas.CC.Utilization.in.last.12.months, to = IV$Tables$Avgas.CC.Utilization.in.last.12.months$WOE)
data_WOE$No.of.trades.opened.in.last.6.months <- mapvalues(data_WOE$No.of.trades.opened.in.last.6.months, from = IV$Tables$No.of.trades.opened.in.last.6.months$No.of.trades.opened.in.last.6.months, to = IV$Tables$No.of.trades.opened.in.last.6.months$WOE)
data_WOE$No.of.trades.opened.in.last.12.months <- mapvalues(data_WOE$No.of.trades.opened.in.last.12.months, from = IV$Tables$No.of.trades.opened.in.last.12.months$No.of.trades.opened.in.last.12.months, to = IV$Tables$No.of.trades.opened.in.last.12.months$WOE)
data_WOE$No.of.PL.trades.opened.in.last.6.months <- mapvalues(data_WOE$No.of.PL.trades.opened.in.last.6.months, from = IV$Tables$No.of.PL.trades.opened.in.last.6.months$No.of.PL.trades.opened.in.last.6.months, to = IV$Tables$No.of.PL.trades.opened.in.last.6.months$WOE)
data_WOE$No.of.PL.trades.opened.in.last.12.months <- mapvalues(data_WOE$No.of.PL.trades.opened.in.last.12.months, from = IV$Tables$No.of.PL.trades.opened.in.last.12.months$No.of.PL.trades.opened.in.last.12.months, to = IV$Tables$No.of.PL.trades.opened.in.last.12.months$WOE)
data_WOE$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <- mapvalues(data_WOE$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., from = IV$Tables$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., to = IV$Tables$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.$WOE)
data_WOE$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- mapvalues(data_WOE$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., from = IV$Tables$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., to = IV$Tables$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.$WOE)
data_WOE$Presence.of.open.home.loan <- mapvalues(data_WOE$Presence.of.open.home.loan, from = IV$Tables$Presence.of.open.home.loan$Presence.of.open.home.loan, to = IV$Tables$Presence.of.open.home.loan$WOE)
data_WOE$Outstanding.Balance <- mapvalues(data_WOE$Outstanding.Balance, from = IV$Tables$Outstanding.Balance$Outstanding.Balance, to = IV$Tables$Outstanding.Balance$WOE)
data_WOE$Total.No.of.Trades <- mapvalues(data_WOE$Total.No.of.Trades, from = IV$Tables$Total.No.of.Trades$Total.No.of.Trades, to = IV$Tables$Total.No.of.Trades$WOE)
data_WOE$Presence.of.open.auto.loan <- mapvalues(data_WOE$Presence.of.open.auto.loan, from = IV$Tables$Presence.of.open.auto.loan$Presence.of.open.auto.loan, to = IV$Tables$Presence.of.open.auto.loan$WOE)


model_data <- data_WOE
#Removing ID column
model_data <- model_data[,-which(names(model_data) %in% c("Application.ID"))]

#Splitting into train and test data
set.seed(1)
split_indices <- sample.split(model_data$Performance.Tag, SplitRatio = 0.70)

train_WOE <- model_data[split_indices, ]
test_WOE <- model_data[!split_indices, ]

#Initial Model 
logistic_1 <- glm(Performance.Tag ~ ., family = "binomial", data = train_WOE) 
summary(logistic_1) #AIC: 16180

#StepAIC run
logistic_2 <- stepAIC(logistic_1, direction = "both")
summary(logistic_2) #AIC: 16040

logistic_3 <- glm(Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
                    Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.12.months + 
                    No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,family = "binomial", data= train_WOE)
summary(logistic_3)
vif(logistic_3)


### Model Evaluation

### Test Data ####

temp_model_1<- logistic_3
#predicted probabilities of Performance for test data

test_pred = predict(temp_model_1, type = "response", 
                    newdata = test_WOE[,-35])


# Let's see the summary 

summary(test_pred)

# Add the prediction probability with the test data set for further model evaluation steps.
test_WOE$prob <- test_pred

# View the test dataset including prediction probabity.
View(test_WOE)

# Let's use the probability cutoff of 50%.

test_pred_performance <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_performance <- factor(ifelse(test_WOE$Performance.Tag==1,"Yes","No"))


table(test_actual_performance,test_pred_performance)
#test_pred_performance
#test_actual_performance    No
#No  19693
#Yes   867

test_conf <- confusionMatrix(test_pred_performance, test_actual_performance, positive = "Yes")
test_conf

perform_fn <- function(cutoff) 
{
  predicted_performance <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_performance, test_actual_performance, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.01 to 0.80 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability
summary(test_pred)

# create 100s of sequence number between 0.01 and 0.80, which will be used as cut-off value
# in each iteration to find out optimal value.
s = seq(.01,.80,length=100)

# Initialize a 100x3 matrix with 0 as default value.
OUT = matrix(0,100,3)

# Call the perform_fn in a loop and assign the output to OUT matrix for "sensitivity", "specificity", "accuracy".
for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2) +
  axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5) +
  axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5) +
  lines(s,OUT[,2],col="darkgreen",lwd=2) +
  lines(s,OUT[,3],col=4,lwd=2) 


# Add legends to the plot created earlier.

legend(0.60,0.75,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy")) 

# Add a box over the plot.
box()

# Calcualte the cut-off value based on nominal difference between Sensitivity and Specificity.
cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.12)]


# Let's choose a cutoff value of 0.0498 for final model

test_cutoff_performance <- factor(ifelse(test_pred >=0.0498, "Yes", "No"))
conf_final <- confusionMatrix(test_cutoff_performance, test_actual_performance, positive = "Yes")

# Get Accuracy, Sensitivity and Specificity from the confusion matrix using 
# cut-off value - 0.1776.
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]
acc
sens
spec
