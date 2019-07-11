# IST 687 Project
install.packages("ggplot2")
library(ggplot2)
install.packages("sqldf")
library(sqldf)
install.packages("rworldmap")
library(rworldmap)
install.packages("arules")
library(arules)
install.packages("arulesViz")
library(arulesViz)
install.packages("randomForest")
library("randomForest")
install.packages("kernlab")
library(kernlab)
install.packages("ggmap")
library(ggmap)

# Set working directory
setwd("~/Desktop/Spring 18/IST 687/IST687 project data")

# Loading six months data into variables
JanDataHyatt <- read.csv("out-201501.csv")[,c(19,23,51,54,56,70,137:145,147,168,171,175:176,179,
                202,203,205,208,218,219,221,223,232,237)]

MarchDataHyatt <- read.csv("out-201403.csv")[,c(19,23,51,54,56,70,137:145,147,168,171,175:176,179,
                202,203,205,208,218,219,221,223,232,237)]

MayDataHyatt <- read.csv("out-201405.csv")[,c(19,23,51,54,56,70,137:145,147,168,171,175:176,179,
                202,203,205,208,218,219,221,223,232,237)]

JulyDataHyatt <- read.csv("out-201407.csv")[,c(19,23,51,54,56,70,137:145,147,168,171,175:176,179,
                202,203,205,208,218,219,221,223,232,237)]

OctDataHyatt <- read.csv("out-201410.csv")[,c(19,23,51,54,56,70,137:145,147,168,171,175:176,179,
                202,203,205,208,218,219,221,223,232,237)]

DecDataHyatt <- read.csv("out-201412.csv")[,c(19,23,51,54,56,70,137:145,147,168,171,175:176,179,
                202,203,205,208,218,219,221,223,232,237)]

# Combining all the data sets into a data set
CompletesixMonthData <-  rbind(JanDataHyatt, MarchDataHyatt, MayDataHyatt, JulyDataHyatt, OctDataHyatt, DecDataHyatt)

colnames(CompletesixMonthData) <-
  c("LengthofCustomerStay","PurposeofVisit","NumberOfRoomsBooked","NightlyRate","CountryOfCustomer",
    "OfferUsedByCustomer", "CustomerMetric_LikelihoodtoRecommend","CustomerMetric_OverallStatisfaction",
    "CustomerMetric_GuestRoomStatisfaction","CustomerMetric_Tranquility","CustomerMetric_ConditionOfHotel",
    "CusotmerMetric_QualityofCustomerService","CustomerMetric_StaffCare","CustomerMetric_InternetSatisfaction",
    "CustomerMetric_CheckInProcessQuality","CustomerMetric_FBOverallExperience","StateOfHotel",
    "CountryOfHotel","LatitudeOfHotel","LongitudeOfHotel","Hotel_NPS_Goals","HotelFlag_BusinessCenter","HotelFlag_Casino",
    "HotelFlag_ConventionSpace","HotelFlag_FitnessCenter","HotelFlag_Resort","HotelFlag_RestaurantOnsite",
    "HotelFlag_ShuttleService","HotelFlag_SPA","CustomerType_NPS","Days_Until_Next_Stay")

# Displaying six months data
View(CompletesixMonthData)

## Creating general promoter count and detractor count plot  
# Plot_1: Creating a plot with count for no. of detractors in each country
ggplot(subset(CompletesixMonthData, CustomerType_NPS == "Detractor"), aes(x = CountryOfHotel)) + 
  geom_bar(col = "white", fill = "red") + ggtitle("Country wise Detractor Score") + 
  labs(x = "State", y = "Detractor Count") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Plot_2: Creating a plot with count for no. of promoters in each country
ggplot(subset(CompletesixMonthData, CustomerType_NPS == "Promoter"), aes(x = CountryOfHotel)) + 
  geom_bar(col = "white", fill = "green") + ggtitle("Country wise Promoter Score") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(x = "State", y = "Promoter Count") 

## STEP NPS CALCULATION
# Calculating Promoter Percentage, Detractor Percentage, NPS percentage
# Counting and storing total number of promoters per country in NPS_file
NPS_file <- sqldf('select count(*) as No_of_promoters, CountryofHotel from
                  CompletesixMonthData where CustomerType_NPS = "Promoter" Group By CountryofHotel')
View(NPS_file)

# Counting and storing total number of detractors per country in NPS_file1
NPS_file1 <- sqldf('select count(*) as No_of_detractors, CountryofHotel from 
                   CompletesixMonthData where CustomerType_NPS = "Detractor" Group By CountryofHotel')
View(NPS_file1)

# Query to combine total number of promoters and detractors against the country of hotel respectively
NPS_file2 <- sqldf('Select No_of_detractors, No_of_promoters, a.CountryofHotel from NPS_file AS a,
                   NPS_file1 AS b where a.CountryofHotel = b.CountryofHotel')
View(NPS_file2)

# Applying left join on the data stored in NPS_file & NPS_file1
NPS_file3 <- sqldf('select a.No_of_promoters, b.No_of_detractors,
                   a.CountryofHotel from NPS_file a LEFT JOIN NPS_file1 b ON a.CountryofHotel = b.CountryofHotel')
View(NPS_file3)

# Counting and storing total number of passives per country in NPS_file4
NPS_file4 <- sqldf('select count(*) as No_of_Passive ,CountryofHotel
                   from CompletesixMonthData where CustomerType_NPS = "Passive" Group By
                   CountryofHotel')
View(NPS_file4)

NPS_file5 <- sqldf('select a.No_of_promoters, a.No_of_detractors,
                   a.CountryofHotel, b.No_of_Passive from NPS_file3 a LEFT JOIN NPS_file4
                   b ON a.CountryofHotel = b.CountryofHotel')
# Displays total number of detractors, promoters & passives 
View(NPS_file5)

NPS_file5$No_of_promoters[is.na(NPS_file5$No_of_promoters)] <- 0
NPS_file5$No_of_detractors[is.na(NPS_file5$No_of_detractors)] <- 0
NPS_file5$No_of_Passive[is.na(NPS_file5$No_of_Passive)] <- 0

# Calculating total number of visitors
NPS_file5$Total_Visiters <- NPS_file5$No_of_promoters +
  NPS_file5$No_of_detractors + NPS_file5$No_of_Passive

# Calculating detractor percentage
NPS_file5$Detractor_Percentage <- NPS_file5$No_of_detractors/
  NPS_file5$Total_Visiters * 100

# Calculating promoter percentage
NPS_file5$Promoter_Percentage <- NPS_file5$No_of_promoters/
  NPS_file5$Total_Visiters * 100

# Calculating Net Promoter Score value
NPS_file5$NPS <- NPS_file5$Promoter_Percentage - NPS_file5$Detractor_Percentage

NPS_file5 <- sqldf('select * from NPS_file5 order by Total_Visiters')
View(NPS_file5)

# Creating a plot of Detractor percentage according to country of hotel
ggplot(NPS_file5, aes(CountryOfHotel, Detractor_Percentage)) + 
  geom_bar(stat = "identity", col = "Black", fill="red") + ggtitle("Country wise Detractor Percentage") + 
  labs(x = "Country ", y = "Detractor Percentage") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Creating a plot of Promoter percentage according to country of hotel
ggplot(NPS_file5, aes(CountryOfHotel, Promoter_Percentage)) + 
  geom_bar(stat = "identity", col = "Black", fill = "light blue") + ggtitle("Country wise Promoter Percentage") + 
  labs(x = "Country ", y = "Promoter Percentage") + theme(axis.text.x=element_text(angle = 90, hjust = 1))

# Creating a plot of NPS value according to country of hotel
ggplot(NPS_file5, aes(CountryOfHotel,NPS)) + geom_bar(stat = "identity", col = "white", fill = "dark blue") + 
  ggtitle("Country wise NPS value") + labs(x = "Country", y = "NPS value") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Creating a scatter plot of NPS value according to country of hotel
ggplot(NPS_file5, aes(NPS, CountryOfHotel)) + geom_point(colour = "red", size = 2) + ggtitle("Country wise NPS value") +
  theme(axis.text.x = element_text(hjust = 0.5, color = "Black"))

# Creating a world map with NPS values plotted on the maps
NPS_file5$CountryOfHotel <- gsub("([a-z])([A-Z])", "\\1 \\2", NPS_file5$CountryOfHotel)
Worldmap <- joinCountryData2Map(NPS_file5, joinCode = "NAME", nameJoinColumn = "CountryOfHotel")
mapCountryData(Worldmap, nameColumnToPlot = "NPS", colorPalette = "topo", catMethod = "fixedWidth")

# Applying Association rules to figure out the amenities that affect the NPS value
# Making a subset of the complete six month dataset
AmenitiesData <- subset(CompletesixMonthData, select = c(2,22:29,30))

# Convert the data type into factors
AmenitiesData$CustomerType_NPS<- as.factor(AmenitiesData$CustomerType_NPS)
AmenitiesData$PurposeofVisit<- as.factor(AmenitiesData$PurposeofVisit)
AmenitiesData$HotelFlag_SPA <- as.factor(AmenitiesData$HotelFlag_SPA)
AmenitiesData$HotelFlag_ShuttleService <- as.factor(AmenitiesData$HotelFlag_ShuttleService)
AmenitiesData$HotelFlag_RestaurantOnsite<- as.factor(AmenitiesData$HotelFlag_RestaurantOnsite)
AmenitiesData$HotelFlag_Resort <- as.factor(AmenitiesData$HotelFlag_Resort)
AmenitiesData$HotelFlag_FitnessCenter <- as.factor(AmenitiesData$HotelFlag_FitnessCenter)
AmenitiesData$HotelFlag_Casino <- as.factor(AmenitiesData$HotelFlag_Casino)
AmenitiesData$HotelFlag_BusinessCenter <- as.factor(AmenitiesData$HotelFlag_BusinessCenter)
AmenitiesData$HotelFlag_ConventionSpace <- as.factor(AmenitiesData$HotelFlag_ConventionSpace)
View(AmenitiesData)

# Applying the rule
rules_amenities <- apriori(AmenitiesData, parameter = list(support = 0.5, confidence = 0.6))
View(rules_amenities)

# Inspecting the rules obtained
inspect(rules_amenities)

# Ploting the rules
plot(rules_amenities)

rulesDF <- data.frame(LHS = labels(lhs(rules_amenities)), RHS = labels(rhs(rules_amenities)), quality(rules_amenities))
View(rulesDF)

# Query to order by lift 
rulesDFfinal <- sqldf('select * from rulesDF order by lift ')
View(rulesDFfinal)
rulesPromoter <- rulesDFfinal[rulesDFfinal$RHS == '{CustomerType_NPS=Promoter}', ]
View(rulesPromoter)
rulesPurposeofVisit <- rulesDFfinal[rulesDFfinal$RHS == '{PurposeofVisit=BUSINESS}', ]
View(rulesPurposeofVisit)

## RANDOM FOREST MODEL
# Subsetting the data 
MetricData <- subset(CompletesixMonthData, select = c(2,7:16,18,30))
View(MetricData)

# Creating a MetricData subset for random forest
MetricData_RF <- MetricData
MetricData_RF$CustomerMetric_LikelihoodtoRecommend <- ifelse(MetricData_RF$CustomerMetric_LikelihoodtoRecommend  >= 6,1,0)
MetricData_RF$CustomerMetric_LikelihoodtoRecommend <- as.character(MetricData_RF$CustomerMetric_LikelihoodtoRecommend )
MetricData_RF$CustomerMetric_LikelihoodtoRecommend  <- as.factor(MetricData_RF$CustomerMetric_LikelihoodtoRecommend)
MetricData_RF$CustomerMetric_LikelihoodtoRecommend[is.na(MetricData_RF$CustomerMetric_LikelihoodtoRecommend)] <- 0
View(MetricData_RF)

output.forest1 <- randomForest(CustomerMetric_LikelihoodtoRecommend ~ CustomerMetric_Tranquility, data = MetricData_RF)

## Working on the US data
uscustomers <- subset(CompletesixMonthData, CompletesixMonthData$CountryOfHotel == 'United States')
View(uscustomers)

USAMap <- geocode("USA")
USA.map <- get_map(location = USAMap, zoom = 4)
ggmap(USA.map)

# Working on the US data to calculate detractors
uscustomersdetractors <- subset(CompletesixMonthData, CompletesixMonthData$CountryOfHotel == 'United States' & CompletesixMonthData$CustomerType_NPS == 'Detractor',)
View(uscustomersdetractors)

# Plotting a graph of detractors
ggplot(subset(uscustomersdetractors, CustomerType_NPS == "Detractor"), aes(x=StateOfHotel)) + 
  geom_bar(col = "white", fill="red") + ggtitle("State wise Detractor Score") + 
  labs(x = "State", y = "Detractor Count") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Working on the US data to calculate promoters
uscustomerspromoters <- subset(CompletesixMonthData, CompletesixMonthData$CountryOfHotel == 'United States' & CompletesixMonthData$CustomerType_NPS == 'Promoter', )
View(uscustomerspromoters)

# Plotting a graph of promoters
ggplot(subset(uscustomerspromoters, CustomerType_NPS == "Promoter"), aes(x = StateOfHotel)) + 
  geom_bar(col = "white", fill = "dark green") + ggtitle("State wise Promoter Score") + 
  labs(x = "State", y = "Promoter Count") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Creating the	Support Vector Machine model
# Convert the column of Likelihood to Recommend to factors
CompletesixMonthData$CustomerMetric_LikelihoodtoRecommend <- ifelse(CompletesixMonthData$CustomerMetric_LikelihoodtoRecommend >= 6,1,0)
CompletesixMonthData$CustomerMetric_LikelihoodtoRecommend <- as.factor(CompletesixMonthData$CustomerMetric_LikelihoodtoRecommend)	

# Randomly	sample 2/3	data	as a training dataset and the	rest	data as a test dataset
set.seed(10)
randIndex <- sample(1:dim(CompletesixMonthData)[1])
cutPoint2_3 <- floor(2*dim(CompletesixMonthData)[1]/3)
trainData <- CompletesixMonthData[randIndex[1:cutPoint2_3], ]
View(trainData)

testData <- CompletesixMonthData[randIndex[(cutPoint2_3 + 1):dim(CompletesixMonthData)[1]], ]
View(testData)

# Applying the function of Support Vector Machine
svmModel1 <- ksvm(CustomerMetric_LikelihoodtoRecommend ~ HotelFlag_BusinessCenter + HotelFlag_Casino 
                  + HotelFlag_ConventionSpace + HotelFlag_Resort + HotelFlag_SPA, data = trainData,	kernel = "rbfdot",	kpar = "automatic",	C = 20,	cross = 3)	

svmModel1