library(dplyr)
library(ggplot2)
library(stringr)
ELSE<-TRUE
#Read the CSV into a data frame
uber_data<-read.csv("Uber Request Data.csv",header = T,stringsAsFactors = F,na.strings = T)

#Make the date and time formats consistent
uber_data$Request.timestamp<-str_replace_all(uber_data$Request.timestamp,"/","-")
uber_data$Drop.timestamp<-str_replace_all(uber_data$Drop.timestamp,"/","-")

#Check type of the time column
class(uber_data$Request.timestamp)

#Convert time columns to Posixct datetime format for date manipulations.
uber_data$request_time<-as.POSIXct(uber_data$Request.timestamp,format="%d-%m-%Y %H:%M")
uber_data$drop_time<-as.POSIXct(uber_data$Drop.timestamp,format="%d-%m-%Y %H:%M")

#Check type of the datetime column
class(uber_data$request_time)

#Now that the time is in correct datetime format, we can now derive day and hour column separately.
#Deriving the Day and Hour from datetime from request_time
uber_data$request_day<-format(uber_data$request_time,"%d")
uber_data$request_hour<-format(uber_data$request_time,"%H")

#Convert Hour columnn to Numeric for data manipulation
uber_data$request_hour<-as.numeric(uber_data$request_hour)

#Split the day into time slots to determine the most busy time periods.
uber_data<-uber_data %>% mutate(.,time_slot = with(.,case_when(
  (request_hour<5) ~ "1-Early_Morning",
  (request_hour<10) ~ "2-Morning",
  (request_hour<14) ~ "3-Noon",
  (request_hour<18) ~ "4-Evening",
  (request_hour<22) ~ "5-Night",
  ELSE ~ "6-Late Night"
)))



# Plot 1 : Status of Uber Cars throughout the day
timeslot_status <- ggplot(uber_data %>% filter(Status != "Trip Completed"),aes(x=factor(time_slot),fill=Pickup.point))
plot01 <- timeslot_status + geom_bar(position = "dodge") +
  ggtitle("Timeslot status for Uber Cabs to and from Airport and City")+
  labs(x="Timeslot of day", y="No. of Cabs")+
  labs(fill="Pickup Point of Cabs")
#Displaying Plot
plot01

# Plot 2 : Hourly status of Uber cabs (Cancelled vs No Cars Available)
hourly_status <- ggplot(uber_data %>% filter(Status != "Trip Completed"),aes(x=factor(request_hour),fill=Status))
plot02 <- hourly_status + geom_bar(position = "dodge") +
  ggtitle("Hourly Status for Uber Cabs")+
  labs(x="Time(in hours)", y="No. of Cabs")+
  labs(fill="Status of Cabs")
#Displaying Plot
plot02

# Plot 3 : Hourly status of Uber cabs from City to Airport (Cancelled vs No Cars Available) 
hourly_status_city_airport <- ggplot(uber_data %>% filter(Status != "Trip Completed",Pickup.point=="City"),aes(x=factor(request_hour),fill=Status))
plot03 <- hourly_status_city_airport + geom_bar(position = "dodge") +
  ggtitle("Hourly Status for Uber Cabs from City to Airport")+
  labs(x="Time(in hours)", y="No. of Cabs")+
  labs(fill="Status of Cabs")
#Displaying Plot
plot03


# Plot 4 : Hourly status of Uber cabs from Airport to City (Cancelled vs No Cars Available) 
hourly_status_airport_city <- ggplot(uber_data %>% filter(Status != "Trip Completed",Pickup.point=="Airport"),aes(x=factor(request_hour),fill=Status))
plot04 <- hourly_status_airport_city + geom_bar(position = "dodge") +
  ggtitle("Hourly Status for Uber Cabs from Airport to City")+
  labs(x="Time(in hours)", y="No. of Cabs")+
  labs(fill="Status of Cabs")
#Displaying Plot
plot04




#Find out the gap between supply and demand and show the same using plots

#To find the demand, we need to identify the customers who did not complete the trip
#create a new column to identify those customers.
uber_data$trip_completed<- ifelse(uber_data$Status=="Trip Completed","Completed","Not Completed")

#Now plot the trip_completed vs trip_not_completed data to determine which timeslot the demand is high.
demand_status <- ggplot(uber_data,aes(x=factor(time_slot),fill=trip_completed))
plot05 <- demand_status + geom_bar(position = "dodge") +
  ggtitle("Demand vs Supply across different times of the day")+
  labs(x="Timeslot of day", y="No. of Cabs")+
  labs(fill="Status of Trip")
#Displaying Plot
plot05

#Find the types of requests (city-airport or airport-city) for which the gap is the most severe in the identified time slots

#City Demand.vs.Supply Graph
city_demand_supply<- ggplot(uber_data%>%filter(Pickup.point=="City"),aes(x=factor(time_slot),fill=trip_completed))
plot06 <- city_demand_supply + geom_bar(position = "dodge") +
  ggtitle("Demand vs Supply of Cabs from City")+
  labs(x="Timeslot of day", y="No. of Cabs")+
  labs(fill="Status of Trip")
#Displaying Plot
plot06

#Airport Demand.vs.Supply Graph
airport_demand_supply<- ggplot(uber_data%>%filter(Pickup.point=="Airport"),aes(x=factor(time_slot),fill=trip_completed))
plot07 <- airport_demand_supply + geom_bar(position = "dodge") +
  ggtitle("Demand vs Supply of Cabs from Airport")+
  labs(x="Timeslot of day", y="No. of Cabs")+
  labs(fill="Status of Trip")
#Displaying Plot
plot07


