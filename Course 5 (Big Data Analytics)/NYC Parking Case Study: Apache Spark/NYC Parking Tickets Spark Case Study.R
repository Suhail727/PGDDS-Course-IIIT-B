#########################################################################################################################
############################################ Loading and Cleaning the data ##############################################
#########################################################################################################################
# Essential commands

# Load SparkR
spark_path <- '/usr/local/spark'

if (nchar(Sys.getenv("SPARK_HOME")) < 1) {
  Sys.setenv(SPARK_HOME = spark_path)
}

library(SparkR, lib.loc = c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib")))

sparkR.session(master = "yarn", sparkConfig = list(spark.driver.memory = "1g"))

# Before executing any hive-sql query from RStudio, you need to add a jar file in RStudio 
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")


# Load the data from CSV into R Studio
nyc_parking_2015 <- SparkR::read.df("/common_folder/nyc_parking/Parking_Violations_Issued_-_Fiscal_Year_2015.csv",
                                    source = "CSV", inferSchema = "true", header = "true")
nyc_parking_2016 <- SparkR::read.df("/common_folder/nyc_parking/Parking_Violations_Issued_-_Fiscal_Year_2016.csv",
                                    source = "CSV", inferSchema = "true", header = "true")
nyc_parking_2017 <- SparkR::read.df("/common_folder/nyc_parking/Parking_Violations_Issued_-_Fiscal_Year_2017.csv",
                                    source = "CSV", inferSchema = "true", header = "true")


# Retain only Distinct Rows 
nyc_parking_distinct_2015 <- dropDuplicates(nyc_parking_2015)
nyc_parking_distinct_2016 <- dropDuplicates(nyc_parking_2016)
nyc_parking_distinct_2017 <- dropDuplicates(nyc_parking_2017)

# Have a look at the data
head(nyc_parking_distinct_2015)
head(nyc_parking_distinct_2016)
head(nyc_parking_distinct_2017)

# Rename columns to make it easier to work with
colnames(nyc_parking_distinct_2015) <- gsub("\\s+","_",gsub("^\\s+|\\s+$", "" , colnames(nyc_parking_distinct_2015)))
colnames(nyc_parking_distinct_2016) <- gsub("\\s+","_",gsub("^\\s+|\\s+$", "" , colnames(nyc_parking_distinct_2016)))
colnames(nyc_parking_distinct_2017) <- gsub("\\s+","_",gsub("^\\s+|\\s+$", "" , colnames(nyc_parking_distinct_2017)))


# Create new column from Issue Date column in Date Format
nyc_parking_distinct_2015$Issue_Date_Format <- to_date(nyc_parking_distinct_2015$Issue_Date, 'MM/dd/yyyy')
nyc_parking_distinct_2016$Issue_Date_Format <- to_date(nyc_parking_distinct_2016$Issue_Date, 'MM/dd/yyyy')
nyc_parking_distinct_2017$Issue_Date_Format <- to_date(nyc_parking_distinct_2017$Issue_Date, 'MM/dd/yyyy')

# Create View
createOrReplaceTempView(nyc_parking_distinct_2015, "nyc_2015")
createOrReplaceTempView(nyc_parking_distinct_2016, "nyc_2016")
createOrReplaceTempView(nyc_parking_distinct_2017, "nyc_2017")

#########################################################################################################################
############################################ Examine the data ###########################################################
#########################################################################################################################

#########################################################################################################################
# 1. Find the total number of tickets for each year.
#########################################################################################################################

######################################################## 2015 ###########################################################
tickets_2015 <- SparkR::sql("select count(distinct Summons_Number) from nyc_2015")
head(tickets_2015)
# 10951256 Total number of Tickets
######################################################## 2016 ###########################################################
tickets_2016 <- SparkR::sql("select count(distinct Summons_Number) from nyc_2016")
head(tickets_2016)
# 10626899 Total number of Tickets
######################################################## 2017 ###########################################################
tickets_2017 <- SparkR::sql("select count(distinct Summons_Number) from nyc_2017")
head(tickets_2017)
# 10803028 Total number of Tickets
#########################################################################################################################
# 2. Find out the number of unique states from where the cars that got parking tickets came from.
#########################################################################################################################
######################################################## 2015 ###########################################################
unique_states_2015 <- SparkR::sql("select distinct Registration_State from nyc_2015")
nrow(unique_states_2015)
# 69 Unique States
# Find State with maximum Entries
max <- SparkR::sql("SELECT Registration_State, COUNT(Registration_State) FROM nyc_2015 
	GROUP BY Registration_State ORDER BY COUNT(Registration_State) DESC LIMIT 1")
head(max)
# NY has maximum entries
# Check for Numeric entries
head(unique_states_2015,n=69)
# 99 is found which is invalid
# Replace 99 with NY
nyc_parking_distinct_2015$Registration_State <- ifelse(nyc_parking_distinct_2015$Registration_State == 99, "NY", nyc_parking_distinct_2015$Registration_State)
# Create View again
createOrReplaceTempView(nyc_parking_distinct_2015, "nyc_2015")
# Check Number of Unique States Again
unique_states_2015 <- SparkR::sql("select distinct Registration_State from nyc_2015")
nrow(unique_states_2015)
# 68 Unique States

######################################################## 2016 ###########################################################
unique_states_2016 <- SparkR::sql("select distinct Registration_State from nyc_2016")
nrow(unique_states_2016)
# 68 Unique States
# Find State with maximum Entries
max <- SparkR::sql("SELECT Registration_State, COUNT(Registration_State) FROM nyc_2016 
	GROUP BY Registration_State ORDER BY COUNT(Registration_State) DESC LIMIT 1")
head(max)
# NY has maximum entries
# Check for Numeric entries
head(unique_states_2016,n=68)
# 99 is found which is invalid
# Replace 99 with NY
nyc_parking_distinct_2016$Registration_State <- ifelse(nyc_parking_distinct_2016$Registration_State == 99, "NY", nyc_parking_distinct_2016$Registration_State)
# Create View again
createOrReplaceTempView(nyc_parking_distinct_2016, "nyc_2016")
# Check Number of Unique States Again
unique_states_2016 <- SparkR::sql("select distinct Registration_State from nyc_2016")
nrow(unique_states_2016)
# 67 Unique States

######################################################## 2017 ###########################################################
unique_states_2017 <- SparkR::sql("select distinct Registration_State from nyc_2017")
nrow(unique_states_2017)
# 67 Unique States
# Find State with maximum Entries
max <- SparkR::sql("SELECT Registration_State, COUNT(Registration_State) FROM nyc_2017
GROUP BY Registration_State ORDER BY COUNT(Registration_State) DESC LIMIT 1")
head(max)
# NY has maximum entries
# Check for Numeric entries
head(unique_states_2017,n=67)
# 99 is found which is invalid
# Replace 99 with NY
nyc_parking_distinct_2017$Registration_State <- ifelse(nyc_parking_distinct_2017$Registration_State == 99, "NY", nyc_parking_distinct_2017$Registration_State)
# Create View again
createOrReplaceTempView(nyc_parking_distinct_2017, "nyc_2017")
# Check Number of Unique States Again
unique_states_2017 <- SparkR::sql("select distinct Registration_State from nyc_2017")
nrow(unique_states_2017)
# 66 Unique States

#########################################################################################################################
# 3. Some parking tickets don’t have the address for violation location on them, which is a cause for concern.
#    Write a query to check the number of such tickets.
#########################################################################################################################
######################################################## 2015 ###########################################################
no_addr_2015 <- SparkR::sql("select * from nyc_2015 where House_Number is null or Street_Name is null")
nrow(no_addr_2015)
# 1807864 Tickets dont have any address
######################################################## 2016 ###########################################################
no_addr_2016 <- SparkR::sql("select * from nyc_2016 where House_Number is null or Street_Name is null")
nrow(no_addr_2016)
# 2035232 Tickets dont have any address
######################################################## 2017 ###########################################################
no_addr_2017 <- SparkR::sql("select * from nyc_2017 where House_Number is null or Street_Name is null")
nrow(no_addr_2017)
# 2289944 Tickets dont have any address

#########################################################################################################################
############################################ Aggregation tasks ##########################################################
#########################################################################################################################

#########################################################################################################################
# 1. How often does each violation code occur? Display the frequency of the top five violation codes.
#########################################################################################################################
######################################################## 2015 ###########################################################
top_violation_2015<- SparkR::sql("select count(*) as Frequency, Violation_Code 
	from nyc_2015 group by Violation_Code order by Frequency desc")
head(top_violation_2015, n=5)
#   Frequency Violation_Code                                                      
# 1   1501614             21
# 2   1324586             38
# 3    924627             14
# 4    761571             36
# 5    746278             37
######################################################## 2016 ###########################################################
top_violation_2016<- SparkR::sql("select count(*) as Frequency, Violation_Code
from nyc_2016 group by Violation_Code order by Frequency desc")
head(top_violation_2016, n=5)
#   Frequency Violation_Code                                                      
# 1   1531587             21
# 2   1253512             36
# 3   1143696             38
# 4    875614             14
# 5    686610             37
######################################################## 2017 ###########################################################
top_violation_2017<- SparkR::sql("select count(*) as Frequency, Violation_Code
	from nyc_2017 group by Violation_Code order by Frequency desc")
head(top_violation_2017, n=5)
#   Frequency Violation_Code                                                      
# 1   1528588             21
# 2   1400614             36
# 3   1062304             38
# 4    893498             14
# 5    618593             20
#########################################################################################################################
# 2. How often does each 'vehicle body type' get a parking ticket? How about the 'vehicle make'? 
#########################################################################################################################
######################################################## 2015 ###########################################################
vehicle_body_type_2015 <- SparkR::sql("select count(*) as Frequency, Vehicle_Body_Type from nyc_2015 group by Vehicle_Body_Type order by Frequency desc")
vehicle_make_2015 <- SparkR::sql("select count(*) as Frequency, Vehicle_Make from nyc_2015 group by Vehicle_Make order by Frequency desc")
head(vehicle_body_type_2015, n=5)
#   Frequency Vehicle_Body_Type                                                   
# 1   3451963              SUBN
# 2   3102510              4DSD
# 3   1605228               VAN
# 4    840442              DELV
# 5    453992               SDN
head(vehicle_make_2015, n=5)
#   Frequency Vehicle_Make                                                        
# 1   1417303         FORD
# 2   1123523        TOYOT
# 3   1018049        HONDA
# 4    837569        NISSA
# 5    836389        CHEVR
######################################################## 2016 ###########################################################
vehicle_body_type_2016 <- SparkR::sql("select count(*) as Frequency, Vehicle_Body_Type from nyc_2016 group by Vehicle_Body_Type order by Frequency desc")
vehicle_make_2016 <- SparkR::sql("select count(*) as Frequency, Vehicle_Make from nyc_2016 group by Vehicle_Make order by Frequency desc")
head(vehicle_body_type_2016, n=5)
#   Frequency Vehicle_Body_Type                                                   
# 1   3466037              SUBN
# 2   2992107              4DSD
# 3   1518303               VAN
# 4    755282              DELV
# 5    424043               SDN
head(vehicle_make_2016, n=5)
#   Frequency Vehicle_Make                                                        
# 1   1324774         FORD
# 2   1154790        TOYOT
# 3   1014074        HONDA
# 4    834833        NISSA
# 5    759663        CHEVR
######################################################## 2017 ###########################################################
vehicle_body_type_2017 <- SparkR::sql("select count(*) as Frequency, Vehicle_Body_Type from nyc_2017 group by Vehicle_Body_Type order by Frequency desc")
vehicle_make_2017 <- SparkR::sql("select count(*) as Frequency, Vehicle_Make from nyc_2017 group by Vehicle_Make order by Frequency desc")
head(vehicle_body_type_2017, n=5)
#   Frequency Vehicle_Body_Type                                                   
# 1   3719802              SUBN
# 2   3082020              4DSD
# 3   1411970               VAN
# 4    687330              DELV
# 5    438191               SDN
head(vehicle_make_2017, n=5)
#   Frequency Vehicle_Make                                                        
# 1   1280958         FORD
# 2   1211451        TOYOT
# 3   1079238        HONDA
# 4    918590        NISSA
# 5    714655        CHEVR
#########################################################################################################################
# 3. A precinct is a police station that has a certain zone of the city under its command. 
#    Find the (5 highest) frequency of tickets for each of the following:
#    3.1 'Violation Precinct' (this is the precinct of the zone where the violation occurred). 
#         Using this, can you make any insights for parking violations in any specific areas of the city?
#    3.2 'Issuer Precinct' (this is the precinct that issued the ticket)
#########################################################################################################################
######################################################## 2015 ###########################################################
top_voilation_precinct_2015 <- SparkR::sql("select count(*) as Frequency, Violation_Precinct
from nyc_2015 group by Violation_Precinct order by Frequency desc")
top_issuer_precinct_2015 <- SparkR::sql("select count(*) as Frequency, Issuer_Precinct
from nyc_2015 group by Issuer_Precinct order by Frequency desc")
head(top_voilation_precinct_2015, n=6)
#   Frequency Violation_Precinct                                                  
# 1   1633006                  0
# 2    559716                 19
# 3    400887                 18
# 4    384596                 14
# 5    307808                  1
# 6    300557                114
head(top_issuer_precinct_2015, n=6)
#   Frequency Issuer_Precinct                                                     
# 1   1834343               0
# 2    544946              19
# 3    391501              18
# 4    369725              14
# 5    298594               1
# 6    295601             114
######################################################## 2016 ###########################################################
top_voilation_precinct_2016 <- SparkR::sql("select count(*) as Frequency, Violation_Precinct
from nyc_2016 group by Violation_Precinct order by Frequency desc")
top_issuer_precinct_2016 <- SparkR::sql("select count(*) as Frequency, Issuer_Precinct
from nyc_2016 group by Issuer_Precinct order by Frequency desc")
head(top_voilation_precinct_2016, n=6)
#   Frequency Violation_Precinct                                                  
# 1   1868655                  0
# 2    554465                 19
# 3    331704                 18
# 4    324467                 14
# 5    303850                  1
# 6    291336                114
head(top_issuer_precinct_2016, n=6)
#   Frequency Issuer_Precinct                                                     
# 1   2140274               0
# 2    540569              19
# 3    323132              18
# 4    315311              14
# 5    295013               1
# 6    286924             114
######################################################## 2017 ###########################################################
top_voilation_precinct_2017 <- SparkR::sql("select count(*) as Frequency, Violation_Precinct
from nyc_2017 group by Violation_Precinct order by Frequency desc")
top_issuer_precinct_2017 <- SparkR::sql("select count(*) as Frequency, Issuer_Precinct
from nyc_2017 group by Issuer_Precinct order by Frequency desc")
head(top_voilation_precinct_2017, n=6)
#   Frequency Violation_Precinct                                                  
# 1   2072400                  0
# 2    535671                 19
# 3    352450                 14
# 4    331810                  1
# 5    306920                 18
# 6    296514                114
head(top_issuer_precinct_2017, n=6)
#  Frequency Issuer_Precinct                                                     
# 1   2388479               0
# 2    521513              19
# 3    344977              14
# 4    321170               1
# 5    296553              18
# 6    289950             114

#########################################################################################################################
# 4. Find the violation code frequency across three precincts which have issued the most number of tickets - 
#    do these precinct zones have an exceptionally high frequency of certain violation codes? 
#    Are these codes common across precincts?
#########################################################################################################################
######################################################## 2015 ###########################################################
# In 2015, Top 3 Issuer Precincts are 19,18,14
violation_code_2015 <- SparkR::sql("select count(*) as Frequency, Violation_Code, Issuer_Precinct
	from (select Violation_Code, Issuer_Precinct from nyc_2015 where Issuer_Precinct = 19
	union all
	select Violation_Code, Issuer_Precinct from nyc_2015 where Issuer_Precinct = 18
	union all
	select Violation_Code, Issuer_Precinct from nyc_2015 where Issuer_Precinct = 14)
	group by Violation_Code, Issuer_Precinct
	order by Frequency desc
	")
head(violation_code_2015, n=10)
#    Frequency Violation_Code Issuer_Precinct                                     
# 1     121004             14              18
# 2      90437             38              19
# 3      80368             69              14
# 4      79738             37              19
# 5      77269             14              14
# 6      60589             14              19
# 7      57218             69              18
# 8      56416             21              19
# 9      56318             16              19
# 10     44082             46              19
# Violation code 14 is common among all Top 3 Issuer Precincts and has high frequency.
######################################################## 2016 ###########################################################
# In 2016, Top 3 Issuer Precincts are 19,18,14
violation_code_2016 <- SparkR::sql("select count(*) as Frequency, Violation_Code, Issuer_Precinct
from (select Violation_Code, Issuer_Precinct from nyc_2016 where Issuer_Precinct = 19
union all
select Violation_Code, Issuer_Precinct from nyc_2016 where Issuer_Precinct = 18
union all
select Violation_Code, Issuer_Precinct from nyc_2016 where Issuer_Precinct = 14)
group by Violation_Code, Issuer_Precinct
order by Frequency desc
")
head(violation_code_2016, n=10)
#  Frequency Violation_Code Issuer_Precinct                                     
# 1      99857             14              18
# 2      77183             38              19
# 3      75641             37              19
# 4      73016             46              19
# 5      67932             69              14
# 6      62426             14              14
# 7      61742             14              19
# 8      58719             21              19
# 9      52354             16              19
# 10     47881             69              18
# Violation code 14 is common among all Top 3 Issuer Precincts and has high frequency.
######################################################## 2017 ###########################################################
# In 2017, Top 3 Issuer Precincts are 19,14,1
violation_code_2017 <- SparkR::sql("select count(*) as Frequency, Violation_Code, Issuer_Precinct
from (select Violation_Code, Issuer_Precinct from nyc_2017 where Issuer_Precinct = 19
union all
select Violation_Code, Issuer_Precinct from nyc_2017 where Issuer_Precinct = 14
union all
select Violation_Code, Issuer_Precinct from nyc_2017 where Issuer_Precinct = 1)
group by Violation_Code, Issuer_Precinct
order by Frequency desc
")
head(violation_code_2017, n=10)
#  Frequency Violation_Code Issuer_Precinct                                     
# 1      86390             46              19
# 2      73837             14              14
# 3      73522             14               1
# 4      72437             37              19
# 5      72344             38              19
# 6      58026             69              14
# 7      57563             14              19
# 8      54700             21              19
# 9      39857             31              14
# 10     38937             16               1
# Violation code 14 is common among all Top 3 Issuer Precincts and has high frequency.

#########################################################################################################################
# 5. You’d want to find out the properties of parking violations across different times of the day: 
#    5.1 Find a way to deal with missing values, if any.
#    5.2 The Violation Time field is specified in a strange format. 
#        Find a way to make this into a time attribute that you can use to divide into groups.
#    5.3 Divide 24 hours into six equal discrete bins of time. 
#        The intervals you choose are at your discretion. For each of these groups, find the three most commonly occurring violations
#    5.4 Now, try another direction. For the 3 most commonly occurring violation codes,
#        find the most common time of the day (in terms of the bins from the previous part)
#########################################################################################################################

######################################################## 2015 ###########################################################
# Have a look at Violation Time column
head(select(nyc_parking_distinct_2015, nyc_parking_distinct_2015$Violation_Time))
# Get Hour in 24 Hour format from Violation Time for Binning
nyc_2015_date_formatted <- SparkR::sql("SELECT *, 
                                CASE
                                    WHEN SUBSTR(TRIM(Violation_Time), -1) = 'P' 
                                         THEN IF(SUBSTR(TRIM(Violation_Time), 1, 2) = 12, SUBSTR(TRIM(Violation_Time), 1, 2) + 0, SUBSTR(TRIM(Violation_Time), 1, 2) + 12)
                                    WHEN SUBSTR(TRIM(Violation_Time), -1) = 'A' THEN SUBSTR(TRIM(Violation_Time), 1, 2) + 0
                                END  AS Violation_Time_24Hr
                                FROM nyc_2015")
# For using SQL, create a temporary view
createOrReplaceTempView(nyc_2015_date_formatted, "nyc_2015_new")
# Check for NULL values
missing_violation_time<- SparkR::sql("select * from nyc_2015_new where Violation_Time_24Hr is  NULL")
nrow(missing_violation_time)
# 1574 rows have Missing values which is insignificant compares to the entire data size

# Remove the NULL values
violation_time_not_null_2015<- SparkR::sql("select * from nyc_2015_new where Violation_Time_24Hr is not NULL")
createOrReplaceTempView(violation_time_not_null_2015, "nyc_2015_new")
# Considered Hour bins as below
# Bin 1 -- 0 to 3 hrs
# Bin 2 -- 4 to 7 hrs
# Bin 3 -- 8 to 11 hrs
# Bin 4 -- 12 to 15 hrs
# Bin 5 -- 16 to 19 hrs
# Bin 6 -- 20 to 23 hrs
nyc_2015_bins <- SparkR::sql("SELECT *, 
                                CASE
                                    WHEN (Violation_Time_24Hr >= 0 AND Violation_Time_24Hr <= 3) THEN 1
                                    WHEN (Violation_Time_24Hr >= 4 AND Violation_Time_24Hr <= 5) THEN 2
                                    WHEN (Violation_Time_24Hr >= 6 AND Violation_Time_24Hr <= 11) THEN 3
                                    WHEN (Violation_Time_24Hr >= 12 AND Violation_Time_24Hr <= 15) THEN 4
                                    WHEN (Violation_Time_24Hr >= 16 AND Violation_Time_24Hr <= 19) THEN 5
                                    WHEN (Violation_Time_24Hr >= 20 AND Violation_Time_24Hr <= 23) THEN 6
                                END  AS Hour_Bin
                                FROM nyc_2015_new")
createOrReplaceTempView(nyc_2015_bins, "nyc_2015_new")

# Find the Common Violations across all Bins
bin1_2015 <- SparkR::sql("SELECT Hour_Bin, Violation_Code, COUNT(1) As Frequency 
                                        FROM nyc_2015_new 
                                        WHERE Hour_Bin = 1
                                        GROUP BY Hour_Bin, Violation_Code
                                        ORDER BY Frequency DESC
                                        LIMIT 3")

bin2_2015 <- SparkR::sql("SELECT Hour_Bin, Violation_Code, COUNT(1) As Frequency
                                        FROM nyc_2015_new 
                                        WHERE Hour_Bin = 2
                                        GROUP BY Hour_Bin, Violation_Code
                                        ORDER BY Frequency DESC
                                        LIMIT 3")

bin3_2015 <- SparkR::sql("SELECT Hour_Bin, Violation_Code, COUNT(1) As Frequency
                                        FROM nyc_2015_new 
                                        WHERE Hour_Bin = 3
                                        GROUP BY Hour_Bin, Violation_Code
                                        ORDER BY Frequency DESC
                                        LIMIT 3")

bin4_2015 <- SparkR::sql("SELECT Hour_Bin, Violation_Code, COUNT(1) As Frequency
                                        FROM nyc_2015_new 
                                        WHERE Hour_Bin = 4
                                        GROUP BY Hour_Bin, Violation_Code
                                        ORDER BY Frequency DESC
                                        LIMIT 3")

bin5_2015 <- SparkR::sql("SELECT Hour_Bin, Violation_Code, COUNT(1) As Frequency
                                        FROM nyc_2015_new 
                                        WHERE Hour_Bin = 5
                                        GROUP BY Hour_Bin, Violation_Code
                                        ORDER BY Frequency DESC
                                        LIMIT 3")

bin6_2015 <- SparkR::sql("SELECT Hour_Bin, Violation_Code, COUNT(1) As Frequency
                                        FROM  nyc_2015_new
                                        WHERE Hour_Bin = 6
                                        GROUP BY Hour_Bin, Violation_Code
                                        ORDER BY Frequency DESC
                                        LIMIT 3")
violation_bins_2015 <- rbind(bin1_2015, bin2_2015, bin3_2015, bin4_2015, bin5_2015, bin6_2015)
head(violation_bins_2015, n=20)
#   Hour_Bin Violation_Code Frequency
# 1         1             21   63574
# 2         1             40   36491
# 3         1             78   34842

# 4         2             40   22154
# 5         2             14   17387
# 6         2              7   15746

# 7         3             21 1294480
# 8         3             38  451531
# 9         3             36  416910

# 10        4             38  568341
# 11        4             37  417626
# 12        4             36  323526

# 13        5             38  241327
# 14        5             37  175802
# 15        5              7  168888

# 16        6              7   81981
# 17        6             38   62418
# 18        6             14   45821

# Find most common times of day, when 3 most commonly violation codes are occurring for year 2015
common_violation_bin_2015 <- SparkR::sql("SELECT Hour_Bin, COUNT(1) as Frequency 
                                 FROM nyc_2015_new bin
                                 JOIN
                                 (SELECT Violation_Code, COUNT(1) as Frequency 
                                  FROM nyc_2015_new 
                                  GROUP BY Violation_Code 
                                  ORDER BY Frequency DESC
                                  LIMIT 3) top
                                 ON bin.Violation_Code = top.Violation_Code
                                 GROUP BY Hour_Bin 
                                 ORDER BY Frequency DESC")
head(common_violation_bin_2015, n = 6)
# Most Common times Of day when Top 3 Violation Codes occur
#   Hour_Bin Frequency                                                            
# 1        3   2160793
# 2        4    977448
# 3        5    390688
# 4        6    108826
# 5        1     90697
# 6        2     22319
######################################################## 2016 ###########################################################
# Get Hour in 24 Hour format from Violation Time for Binning
nyc_2016_date_formatted <- SparkR::sql("SELECT *, 
                                CASE
                                    WHEN SUBSTR(TRIM(Violation_Time), -1) = 'P' 
                                         THEN IF(SUBSTR(TRIM(Violation_Time), 1, 2) = 12, SUBSTR(TRIM(Violation_Time), 1, 2) + 0, SUBSTR(TRIM(Violation_Time), 1, 2) + 12)
                                    WHEN SUBSTR(TRIM(Violation_Time), -1) = 'A' THEN SUBSTR(TRIM(Violation_Time), 1, 2) + 0
                                END  AS Violation_Time_24Hr
                                FROM nyc_2016")
# For using SQL, create a temporary view
createOrReplaceTempView(nyc_2016_date_formatted, "nyc_2016_new")
# Check for NULL values
missing_violation_time<- SparkR::sql("select * from nyc_2016_new where Violation_Time_24Hr is  NULL")
nrow(missing_violation_time)
# 4290 rows have Missing values which is insignificant compares to the entire data size

# Remove the NULL values
violation_time_not_null_2016<- SparkR::sql("select * from nyc_2016_new where Violation_Time_24Hr is not NULL")
createOrReplaceTempView(violation_time_not_null_2016, "nyc_2016_new")
# Considered Hour bins as below
# Bin 1 -- 0 to 3 hrs
# Bin 2 -- 4 to 7 hrs
# Bin 3 -- 8 to 11 hrs
# Bin 4 -- 12 to 15 hrs
# Bin 5 -- 16 to 19 hrs
# Bin 6 -- 20 to 23 hrs
nyc_2016_bins <- SparkR::sql("SELECT *, 
                                CASE
                                    WHEN (Violation_Time_24Hr >= 0 AND Violation_Time_24Hr <= 3) THEN 1
                                    WHEN (Violation_Time_24Hr >= 4 AND Violation_Time_24Hr <= 5) THEN 2
                                    WHEN (Violation_Time_24Hr >= 6 AND Violation_Time_24Hr <= 11) THEN 3
                                    WHEN (Violation_Time_24Hr >= 12 AND Violation_Time_24Hr <= 15) THEN 4
                                    WHEN (Violation_Time_24Hr >= 16 AND Violation_Time_24Hr <= 19) THEN 5
                                    WHEN (Violation_Time_24Hr >= 20 AND Violation_Time_24Hr <= 23) THEN 6
                                END  AS Hour_Bin
                                FROM nyc_2016_new")
createOrReplaceTempView(nyc_2016_bins, "nyc_2016_new")

# Find the Common Violations across all Bins
bin1_2016 <- SparkR::sql("SELECT Hour_Bin, Violation_Code, COUNT(*) As Frequency 
                                        FROM nyc_2016_new 
                                        WHERE Hour_Bin = 1
                                        GROUP BY Hour_Bin, Violation_Code
                                        ORDER BY Frequency DESC
                                        LIMIT 3")

bin2_2016 <- SparkR::sql("SELECT Hour_Bin, Violation_Code, COUNT(*) As Frequency
                                        FROM nyc_2016_new 
                                        WHERE Hour_Bin = 2
                                        GROUP BY Hour_Bin, Violation_Code
                                        ORDER BY Frequency DESC
                                        LIMIT 3")

bin3_2016 <- SparkR::sql("SELECT Hour_Bin, Violation_Code, COUNT(*) As Frequency
                                        FROM nyc_2016_new 
                                        WHERE Hour_Bin = 3
                                        GROUP BY Hour_Bin, Violation_Code
                                        ORDER BY Frequency DESC
                                        LIMIT 3")

bin4_2016 <- SparkR::sql("SELECT Hour_Bin, Violation_Code, COUNT(*) As Frequency
                                        FROM nyc_2016_new 
                                        WHERE Hour_Bin = 4
                                        GROUP BY Hour_Bin, Violation_Code
                                        ORDER BY Frequency DESC
                                        LIMIT 3")

bin5_2016 <- SparkR::sql("SELECT Hour_Bin, Violation_Code, COUNT(*) As Frequency
                                        FROM nyc_2016_new 
                                        WHERE Hour_Bin = 5
                                        GROUP BY Hour_Bin, Violation_Code
                                        ORDER BY Frequency DESC
                                        LIMIT 3")

bin6_2016 <- SparkR::sql("SELECT Hour_Bin, Violation_Code, COUNT(*) As Frequency
                                        FROM  nyc_2016_new
                                        WHERE Hour_Bin = 6
                                        GROUP BY Hour_Bin, Violation_Code
                                        ORDER BY Frequency DESC
                                        LIMIT 3")
violation_bins_2016 <- rbind(bin1_2016, bin2_2016, bin3_2016, bin4_2016, bin5_2016, bin6_2016)
head(violation_bins_2016, n=20)
#  Hour_Bin Violation_Code Frequency
# 1         1             21     67798
# 2         1             40     37261
# 3         1             78     29473

# 4         2             40     21763
# 5         2             14     19307
# 6         2             20     11651

# 7         3             21   1318487
# 8         3             36    666586
# 9         3             14    397077

# 10        4             36    545717
# 11        4             38    488363
# 12        4             37    383379

# 13        5             38    211267
# 14        5             37    161655
# 15        5             14    134976

# 16        6              7     60924
# 17        6             38     53174
# 18        6             40     44973

# Find most common times of day, when 3 most commonly violation codes are occurring for year 2016
common_violation_bin_2016 <- SparkR::sql("SELECT Hour_Bin, COUNT(*) as Frequency 
                                 FROM nyc_2016_new bin
                                 JOIN
                                 (SELECT Violation_Code, COUNT(*) as Frequency 
                                  FROM nyc_2016_new 
                                  GROUP BY Violation_Code 
                                  ORDER BY Frequency DESC
                                  LIMIT 3) top
                                 ON bin.Violation_Code = top.Violation_Code
                                 GROUP BY Hour_Bin 
                                 ORDER BY Frequency DESC")
head(common_violation_bin_2016, n = 6)
# Most Common times Of day when Top 3 Violation Codes occur
# Hour_Bin Frequency                                                            
# 1        3   2375082
# 2        4   1172721
# 3        5    253073
# 4        1     68123
# 5        6     53602
# 6        2      5088
######################################################## 2017 ###########################################################
# Get Hour in 24 Hour format from Violation Time for Binning
nyc_2017_date_formatted <- SparkR::sql("SELECT *, 
                                CASE
                                    WHEN SUBSTR(TRIM(Violation_Time), -1) = 'P' 
                                         THEN IF(SUBSTR(TRIM(Violation_Time), 1, 2) = 12, SUBSTR(TRIM(Violation_Time), 1, 2) + 0, SUBSTR(TRIM(Violation_Time), 1, 2) + 12)
                                    WHEN SUBSTR(TRIM(Violation_Time), -1) = 'A' THEN SUBSTR(TRIM(Violation_Time), 1, 2) + 0
                                END  AS Violation_Time_24Hr
                                FROM nyc_2017")
# For using SQL, create a temporary view
createOrReplaceTempView(nyc_2017_date_formatted, "nyc_2017_new")
# Check for NULL values
missing_violation_time<- SparkR::sql("select * from nyc_2017_new where Violation_Time_24Hr is  NULL")
nrow(missing_violation_time)
# 85 rows have Missing values which is insignificant compares to the entire data size

# Remove the NULL values
violation_time_not_null_2017<- SparkR::sql("select * from nyc_2017_new where Violation_Time_24Hr is not NULL")
createOrReplaceTempView(violation_time_not_null_2017, "nyc_2017_new")
# Considered Hour bins as below
# Bin 1 -- 0 to 3 hrs
# Bin 2 -- 4 to 7 hrs
# Bin 3 -- 8 to 11 hrs
# Bin 4 -- 12 to 15 hrs
# Bin 5 -- 16 to 19 hrs
# Bin 6 -- 20 to 23 hrs
nyc_2017_bins <- SparkR::sql("SELECT *, 
                                CASE
                                    WHEN (Violation_Time_24Hr >= 0 AND Violation_Time_24Hr <= 3) THEN 1
                                    WHEN (Violation_Time_24Hr >= 4 AND Violation_Time_24Hr <= 5) THEN 2
                                    WHEN (Violation_Time_24Hr >= 6 AND Violation_Time_24Hr <= 11) THEN 3
                                    WHEN (Violation_Time_24Hr >= 12 AND Violation_Time_24Hr <= 15) THEN 4
                                    WHEN (Violation_Time_24Hr >= 16 AND Violation_Time_24Hr <= 19) THEN 5
                                    WHEN (Violation_Time_24Hr >= 20 AND Violation_Time_24Hr <= 23) THEN 6
                                END  AS Hour_Bin
                                FROM nyc_2017_new")
createOrReplaceTempView(nyc_2017_bins, "nyc_2017_new")

# Find the Common Violations across all Bins
bin1_2017 <- SparkR::sql("SELECT Hour_Bin, Violation_Code, COUNT(*) As Frequency 
                                        FROM nyc_2017_new 
                                        WHERE Hour_Bin = 1
                                        GROUP BY Hour_Bin, Violation_Code
                                        ORDER BY Frequency DESC
                                        LIMIT 3")

bin2_2017 <- SparkR::sql("SELECT Hour_Bin, Violation_Code, COUNT(*) As Frequency
                                        FROM nyc_2017_new 
                                        WHERE Hour_Bin = 2
                                        GROUP BY Hour_Bin, Violation_Code
                                        ORDER BY Frequency DESC
                                        LIMIT 3")

bin3_2017 <- SparkR::sql("SELECT Hour_Bin, Violation_Code, COUNT(*) As Frequency
                                        FROM nyc_2017_new 
                                        WHERE Hour_Bin = 3
                                        GROUP BY Hour_Bin, Violation_Code
                                        ORDER BY Frequency DESC
                                        LIMIT 3")

bin4_2017 <- SparkR::sql("SELECT Hour_Bin, Violation_Code, COUNT(*) As Frequency
                                        FROM nyc_2017_new 
                                        WHERE Hour_Bin = 4
                                        GROUP BY Hour_Bin, Violation_Code
                                        ORDER BY Frequency DESC
                                        LIMIT 3")

bin5_2017 <- SparkR::sql("SELECT Hour_Bin, Violation_Code, COUNT(*) As Frequency
                                        FROM nyc_2017_new 
                                        WHERE Hour_Bin = 5
                                        GROUP BY Hour_Bin, Violation_Code
                                        ORDER BY Frequency DESC
                                        LIMIT 3")

bin6_2017 <- SparkR::sql("SELECT Hour_Bin, Violation_Code, COUNT(*) As Frequency
                                        FROM  nyc_2017_new
                                        WHERE Hour_Bin = 6
                                        GROUP BY Hour_Bin, Violation_Code
                                        ORDER BY Frequency DESC
                                        LIMIT 3")
violation_bins_2017 <- rbind(bin1_2017, bin2_2017, bin3_2017, bin4_2017, bin5_2017, bin6_2017)
head(violation_bins_2017, n=20)
#  Hour_Bin Violation_Code Frequency
# 1         1             21     73160
# 2         1             40     45960
# 3         1             14     29312

# 4         2             40     26225
# 5         2             14     15630
# 6         2             20     13724

# 7         3             21   1296759
# 8         3             36    785361
# 9         3             14    399934

# 10        4             36    588395
# 11        4             38    462859
# 12        4             37    337096

# 13        5             38    203232
# 14        5             37    145784
# 15        5             14    144749

# 16        6              7     65593
# 17        6             38     47029
# 18        6             14     44779

# Find most common times of day, when 3 most commonly violation codes are occurring for year 2017
common_violation_bin_2017 <- SparkR::sql("SELECT Hour_Bin, COUNT(1) as Frequency 
                                 FROM nyc_2017_new bin
                                 JOIN
                                 (SELECT Violation_Code, COUNT(1) as Frequency 
                                  FROM nyc_2017_new 
                                  GROUP BY Violation_Code 
                                  ORDER BY Frequency DESC
                                  LIMIT 3) top
                                 ON bin.Violation_Code = top.Violation_Code
                                 GROUP BY Hour_Bin 
                                 ORDER BY Frequency DESC")
head(common_violation_bin_2017, n = 6)
# Most Common times Of day when Top 3 Violation Codes occur
#   Hour_Bin Frequency                                                            
# 1        3   2430699
# 2        4   1203568
# 3        5    230641
# 4        1     73523
# 5        6     47392
# 6        2      5638
#########################################################################################################################
# 6. Let’s try and find some seasonality in this data 
#    6.1 First, divide the year into some number of seasons, and find frequencies of tickets for each season
#    6.2 Then, find the three most common violations for each of these seasons.
#########################################################################################################################
######################################################## 2015 ###########################################################
# Create Month using Issue Date column
nyc_2015_issue_date_formatted <- withColumn(nyc_parking_distinct_2015, "Month", month(nyc_parking_distinct_2015$Issue_Date_Format))
# Create New View
createOrReplaceTempView(nyc_2015_issue_date_formatted, "nyc_2015_new")

# Create Bins Based on Seasonality
nyc_2015_season_bins <- SparkR::sql("SELECT *, 
                                CASE
                                    WHEN (Month >= 1 AND Month <= 3) THEN 1
                                    WHEN (Month >= 4 AND Month <= 6) THEN 2
                                    WHEN (Month >= 7 AND Month <= 9) THEN 3
                                    WHEN (Month >= 10 AND Month <= 12) THEN 4
                                END  AS season_bin
                                FROM nyc_2015_new")
# Replace View
createOrReplaceTempView(nyc_2015_season_bins, "nyc_2015_new")
# Frequencies of tickets for each Season Bin.
season_bin_tickets_2015 <- SparkR::sql("SELECT season_bin, COUNT(Summons_Number) AS Total_Tickets 
                                        FROM nyc_2015_new 
                                        GROUP BY season_bin 
                                        ORDER BY season_bin ASC")
head(season_bin_tickets_2015)
# Number of Tickets in each Season
# season_bin Total_Tickets                                                      
# 1          1       2475936
# 2          2       3250418
# 3          3       2789425
# 4          4       2435478

# Top Violation Code in each Season Bin
violation_code_season1_2015 <- SparkR::sql("select season_bin, Violation_Code, count(Summons_Number) as Total_Tickets from nyc_2015_new 
	where season_bin == 1 group by season_bin, Violation_Code order by season_bin, Total_Tickets desc limit 3")
violation_code_season2_2015 <- SparkR::sql("select season_bin, Violation_Code, count(Summons_Number) as Total_Tickets from nyc_2015_new 
	where season_bin == 2 group by season_bin, Violation_Code order by season_bin, Total_Tickets desc limit 3")
violation_code_season3_2015 <- SparkR::sql("select season_bin, Violation_Code, count(Summons_Number) as Total_Tickets from nyc_2015_new 
	where season_bin == 3 group by season_bin, Violation_Code order by season_bin, Total_Tickets desc limit 3")
violation_code_season4_2015 <- SparkR::sql("select season_bin, Violation_Code, count(Summons_Number) as Total_Tickets from nyc_2015_new
 where season_bin == 4 group by season_bin, Violation_Code order by season_bin, Total_Tickets desc limit 3")
# Combine All
season1_season2 <- rbind(violation_code_season1_2015,violation_code_season2_2015)
season3_season4 <- rbind(violation_code_season3_2015,violation_code_season4_2015)
violation_code_season_bins_2015 <- rbind(season1_season2,season3_season4)

head(violation_code_season_bins_2015, n=15)
# Common Violations in every season of 2015
#   season_bin Violation_Code Total_Tickets
# 1           1             38        336762
# 2           1             21        281600
# 3           1             14        220029

# 4           2             21        471580
# 5           2             38        346719
# 6           2             14        262595

# 7           3             21        397871
# 8           3             38        348466
# 9           3             14        234606

# 10          4             21        350563
# 11          4             38        292639
# 12          4             14        207397

######################################################## 2016 ###########################################################
# Create Month using Issue Date column
nyc_2016_issue_date_formatted <- withColumn(nyc_parking_distinct_2016, "Month", month(nyc_parking_distinct_2016$Issue_Date_Format))
# Create New View
createOrReplaceTempView(nyc_2016_issue_date_formatted, "nyc_2016_new")

# Create Bins Based on Seasonality
nyc_2016_season_bins <- SparkR::sql("SELECT *, 
                                CASE
                                    WHEN (Month >= 1 AND Month <= 3) THEN 1
                                    WHEN (Month >= 4 AND Month <= 6) THEN 2
                                    WHEN (Month >= 7 AND Month <= 9) THEN 3
                                    WHEN (Month >= 10 AND Month <= 12) THEN 4
                                END  AS season_bin
                                FROM nyc_2016_new")
# Replace View
createOrReplaceTempView(nyc_2016_season_bins, "nyc_2016_new")
# Frequencies of tickets for each Season Bin.
season_bin_tickets_2016 <- SparkR::sql("SELECT season_bin, COUNT(Summons_Number) AS Total_Tickets 
                                        FROM nyc_2016_new 
                                        GROUP BY season_bin 
                                        ORDER BY season_bin ASC")
head(season_bin_tickets_2016)
# Number of Tickets in each Season
# season_bin Total_Tickets                                                      
# 1          1       2671331
# 2          2       2425877
# 3          3       2728663
# 4          4       2801028

# Top Violation Code in each Season Bin
violation_code_season1_2016 <- SparkR::sql("select season_bin, Violation_Code, count(Summons_Number) as Total_Tickets from nyc_2016_new 
	where season_bin == 1 group by season_bin, Violation_Code order by season_bin, Total_Tickets desc limit 3")
violation_code_season2_2016 <- SparkR::sql("select season_bin, Violation_Code, count(Summons_Number) as Total_Tickets from nyc_2016_new 
	where season_bin == 2 group by season_bin, Violation_Code order by season_bin, Total_Tickets desc limit 3")
violation_code_season3_2016 <- SparkR::sql("select season_bin, Violation_Code, count(Summons_Number) as Total_Tickets from nyc_2016_new 
	where season_bin == 3 group by season_bin, Violation_Code order by season_bin, Total_Tickets desc limit 3")
violation_code_season4_2016 <- SparkR::sql("select season_bin, Violation_Code, count(Summons_Number) as Total_Tickets from nyc_2016_new
 where season_bin == 4 group by season_bin, Violation_Code order by season_bin, Total_Tickets desc limit 3")
# Combine All
season1_season2 <- rbind(violation_code_season1_2016,violation_code_season2_2016)
season3_season4 <- rbind(violation_code_season3_2016,violation_code_season4_2016)
violation_code_season_bins_2016 <- rbind(season1_season2,season3_season4)

head(violation_code_season_bins_2016, n=15)
# Common Violations in every season of 2016
#  season_bin Violation_Code Total_Tickets
# 1           1             21        349644
# 2           1             36        341787
# 3           1             38        308999

# 4           2             21        348473
# 5           2             36        294015
# 6           2             38        254909

# 7           3             21        403720
# 8           3             38        305360
# 9           3             14        234943

# 10          4             36        433966
# 11          4             21        429750
# 12          4             38        274428
######################################################## 2017 ###########################################################
# Create Month using Issue Date column
nyc_2017_issue_date_formatted <- withColumn(nyc_parking_distinct_2017, "Month", month(nyc_parking_distinct_2017$Issue_Date_Format))
# Create New View
createOrReplaceTempView(nyc_2017_issue_date_formatted, "nyc_2017_new")

# Create Bins Based on Seasonality
nyc_2017_season_bins <- SparkR::sql("SELECT *, 
                                CASE
                                    WHEN (Month >= 1 AND Month <= 3) THEN 1
                                    WHEN (Month >= 4 AND Month <= 6) THEN 2
                                    WHEN (Month >= 7 AND Month <= 9) THEN 3
                                    WHEN (Month >= 10 AND Month <= 12) THEN 4
                                END  AS season_bin
                                FROM nyc_2017_new")
# Replace View
createOrReplaceTempView(nyc_2017_season_bins, "nyc_2017_new")
# Frequencies of tickets for each Season Bin.
season_bin_tickets_2017 <- SparkR::sql("SELECT season_bin, COUNT(Summons_Number) AS Total_Tickets 
                                        FROM nyc_2017_new 
                                        GROUP BY season_bin 
                                        ORDER BY season_bin ASC")
head(season_bin_tickets_2017)
# Number of Tickets in each Season
# season_bin Total_Tickets                                                      
# 1          1       2671332
# 2          2       3018840
# 3          3       2463936
# 4          4       2648920

# Top Violation Code in each Season Bin
violation_code_season1_2017 <- SparkR::sql("select season_bin, Violation_Code, count(Summons_Number) as Total_Tickets from nyc_2017_new 
	where season_bin == 1 group by season_bin, Violation_Code order by season_bin, Total_Tickets desc limit 3")
violation_code_season2_2017 <- SparkR::sql("select season_bin, Violation_Code, count(Summons_Number) as Total_Tickets from nyc_2017_new 
	where season_bin == 2 group by season_bin, Violation_Code order by season_bin, Total_Tickets desc limit 3")
violation_code_season3_2017 <- SparkR::sql("select season_bin, Violation_Code, count(Summons_Number) as Total_Tickets from nyc_2017_new 
	where season_bin == 3 group by season_bin, Violation_Code order by season_bin, Total_Tickets desc limit 3")
violation_code_season4_2017 <- SparkR::sql("select season_bin, Violation_Code, count(Summons_Number) as Total_Tickets from nyc_2017_new
 where season_bin == 4 group by season_bin, Violation_Code order by season_bin, Total_Tickets desc limit 3")
# Combine All
season1_season2 <- rbind(violation_code_season1_2017,violation_code_season2_2017)
season3_season4 <- rbind(violation_code_season3_2017,violation_code_season4_2017)
violation_code_season_bins_2017 <- rbind(season1_season2,season3_season4)

head(violation_code_season_bins_2017, n=15)
# Common Violations in every season of 2017
#  season_bin Violation_Code Total_Tickets
# 1           1             21        374202
# 2           1             36        348240
# 3           1             38        287017

# 4           2             21        421184
# 5           2             36        369902
# 6           2             38        266909

# 7           3             21        385774
# 8           3             38        244985
# 9           3             36        239879

# 10          4             36        442593
# 11          4             21        347428
# 12          4             38        263393
#########################################################################################################################
# 7. The fines collected from all the parking violation constitute a revenue source for the NYC police department. 
#    Let’s take an example of estimating that for the three most commonly occurring codes.
#    7.1 Find total occurrences of the three most common violation codes
#    7.2 Then, visit the website:
#        http://www1.nyc.gov/site/finance/vehicles/services-violation-codes.page
#        It lists the fines associated with different violation codes. They’re divided into two categories,
#        one for the highest-density locations of the city, the other for the rest of the city. For simplicity, take an average of the two.
#    7.3 Using this information, find the total amount collected for the three violation codes with maximum tickets. 
#        State the code which has the highest total collection.
#    7.4 What can you intuitively infer from these findings?
#########################################################################################################################

######################################################## 2015 ###########################################################
# Find total occurrences of the three most common violation codes
top_violation_code_2015 <- summarize(groupBy(nyc_parking_distinct_2015, nyc_parking_distinct_2015$Violation_Code), 
                              count= n(nyc_parking_distinct_2015$Violation_Code))
                        
head(arrange(top_violation_code_2015, desc(top_violation_code_2015$count)), n=3)
# Top 3 Violation Codes in 2015
#  Violation_Code   count                                                        
# 1             21 1501614
# 2             38 1324586
# 3             14  924627
# Enter Fine Amount for respective Violation Codes
fines_2015 <- sql("SELECT *, 
                           CASE WHEN Violation_Code = 21 THEN 55
                           WHEN Violation_Code = 38 THEN 50
                           WHEN Violation_Code = 14 THEN 115
                           END  as fine_amount FROM nyc_2015_new")
createOrReplaceTempView(fines_2015, "nyc_2015_new")

total_fines_2015 <- SparkR::sql("SELECT Violation_Code, SUM(fine_amount) AS Total_Fine_Amount
                                          FROM nyc_2015_new 
                                          GROUP BY Violation_Code 
                                          ORDER BY Total_Fine_Amount DESC ")

head(total_fines_2015, n=3)
# Total Fine Amount collected from the Top 3 Violations
#  Violation_Code Total_Fine_Amount                                              
# 1             14         106332105
# 2             21          82588770
# 3             38          66229300
######################################################## 2016 ###########################################################
top_violation_code_2016 <- summarize(groupBy(nyc_parking_distinct_2016, nyc_parking_distinct_2016$Violation_Code), 
                              count= n(nyc_parking_distinct_2016$Violation_Code))
                        
head(arrange(top_violation_code_2016, desc(top_violation_code_2016$count)), n=3)
# Top 3 Violation Codes in 2016
#  Violation_Code   count                                                        
# 1             21 1531587
# 2             36 1253512
# 3             38 1143696

# Enter Fine Amount for respective Violation Codes
fines_2016 <- sql("SELECT *, 
                           CASE WHEN Violation_Code = 21 THEN 55
                           WHEN Violation_Code = 38 THEN 50
                           WHEN Violation_Code = 36 THEN 50
                           END  as fine_amount FROM nyc_2016_new")
createOrReplaceTempView(fines_2016, "nyc_2016_new")

total_fines_2016 <- SparkR::sql("SELECT Violation_Code, SUM(fine_amount) AS Total_Fine_Amount
                                          FROM nyc_2016_new 
                                          GROUP BY Violation_Code 
                                          ORDER BY Total_Fine_Amount DESC ")

head(total_fines_2016, n=3)
# Total Fine Amount collected from the Top 3 Violations
#   Violation_Code Total_Fine_Amount                                              
# 1             21          84237285
# 2             36          62675600
# 3             38          57184800
######################################################## 2017 ###########################################################
top_violation_code_2017 <- summarize(groupBy(nyc_parking_distinct_2017, nyc_parking_distinct_2017$Violation_Code), 
                              count= n(nyc_parking_distinct_2017$Violation_Code))
                        
head(arrange(top_violation_code_2017, desc(top_violation_code_2017$count)), n=3)
# Top 3 Violation Codes in 2017
#  Violation_Code   count                                                        
# 1             21 1528588
# 2             36 1400614
# 3             38 1062304
# Enter Fine Amount for respective Violation Codes
fines_2017 <- sql("SELECT *, 
                           CASE WHEN Violation_Code = 21 THEN 55
                           WHEN Violation_Code = 38 THEN 50
                           WHEN Violation_Code = 36 THEN 50
                           END  as fine_amount FROM nyc_2017_new")
createOrReplaceTempView(fines_2017, "nyc_2017_new")

total_fines_2017 <- SparkR::sql("SELECT Violation_Code, SUM(fine_amount) AS Total_Fine_Amount
                                          FROM nyc_2017_new 
                                          GROUP BY Violation_Code 
                                          ORDER BY Total_Fine_Amount DESC ")

head(total_fines_2017, n=3)
# Total Fine Amount collected from the Top 3 Violations
# Violation_Code Total_Fine_Amount                                              
# 1             21          84072340
# 2             36          70030700
# 3             38          53115200

# While Violation Code 14 had brought in the most amount in 2015 as the fine amount is more than double the fine ($115 compared to $50 and $55)
# of other 2 Violation Codes.
# In 2016 and 2017, Violation Code 21 had brought in the most revenue compared to other Violations
# And Violation Code 14 has not been in the Top 3 Violations in 2016 and 2017
# 
# Violation Codes 21, 36 and 38 have constantly been the top Violations for two consecutive years (2016-2017) and
# the revenue is also proportional to the number of Violation tickets in 2016 and 2017 
# as the Fine amount is more or less the same for the top 3 Violation Codes 21,36,38 (~$50) 
#########################################################################################################################
######################################################## END ############################################################
#########################################################################################################################