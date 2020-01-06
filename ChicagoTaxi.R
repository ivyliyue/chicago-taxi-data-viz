library(ggplot2)
library(psych)
library(dplyr)
library(lubridate)
library(igraph)

###### Import Data ######
#Import Dataset
library(readr)
taxi_sample <- read_csv("~/Documents/DePaul Documents/DePaul University/Master of Science in Predictive Analytics /CSC 465 Data Visualization/Final Project/taxi_sample.csv", 
                        +     col_types = cols(EndDate = col_date(format = "%m/%d/%y"), 
                                               +         StartDate = col_date(format = "%m/%d/%y")))

#Import the community area name dataset
COMMUNITY_AREA_NAME <- read_excel("~/Documents/DePaul Documents/DePaul University/Master of Science in Predictive Analytics /CSC 465 Data Visualization/Final Project/COMMUNITY AREA NAME.xlsx")

# Rename the community_area_name dataset
Community_Name = COMMUNITY_AREA_NAME

# Merge two data frames with same column: DropoffCommunityArea
taxi_updated<- merge(taxi_sample, Community_Name, by.x=c("DropoffCommunityArea"), by.y=c("COMMUNITY AREA"))

# Rename the Dropoff Community Area column
names(taxi_updated)[25] = "Dropoff_Community_Name"

# Merge two data frames with same column: PickupCommunityArea
taxi_updated_2<- merge(taxi_updated, Community_Name, by.x=c("PickupCommunityArea"), by.y=c("COMMUNITY AREA"))

# Rename the Pickup Community Area column
names(taxi_updated_2)[26] = "Pickup_Community_Name"

# Reorder the columns using column names
taxi_updated_2 <- taxi_updated_2[c('Observation','StartDate','StartTime','EndDate','EndTime','Company','Extras','Fare','PaymentType','Tips','Tolls',
                                   'TripMiles','TripSeconds','TripTotal','DropoffCensusTract','DropoffCentroidLatitude','DropoffCentroidLongitude','DropoffCentroidLocation','DropoffCommunityArea',
                                   'Dropoff_Community_Name','PickupCencusTract','PickupCentroidLatitude','PickupCentroidLongitude','PickupCentroidLocation','PickupCommunityArea','Pickup_Community_Name')]

## Format the time columns
# Check the class of date and time
class(taxi_updated_2$StartDate)
class(taxi_updated_2$StartTime)
class(taxi_updated_2$EndDate)
class(taxi_updated_2$EndTime)
class(taxi_updated_2$StartDateTime)
class(taxi_updated_2$EndDateTime)

# Convert character data to POSIXlt date and time

taxi_updated_2$Start_Date_Time = as.POSIXct(paste(taxi_updated_2$StartDate, taxi_updated_2$StartTime), format="%Y-%m-%d %H:%M:%S")
taxi_updated_2$End_Date_Time = as.POSIXct(paste(taxi_updated_2$EndDate, taxi_updated_2$EndTime), format="%Y-%m-%d %H:%M:%S")

# Reorder the columns using column names
taxi_updated_2 <- taxi_updated_2[c('Observation','StartDate','StartTime','Start_Date_Time','EndDate','EndTime','End_Date_Time','Company','Extras','Fare','PaymentType','Tips','Tolls',
                                   'TripMiles','TripSeconds','TripTotal','DropoffCensusTract','DropoffCentroidLatitude','DropoffCentroidLongitude','DropoffCentroidLocation','DropoffCommunityArea',
                                   'Dropoff_Community_Name','PickupCencusTract','PickupCentroidLatitude','PickupCentroidLongitude','PickupCentroidLocation','PickupCommunityArea','Pickup_Community_Name')]

###### Exploratory Data Analysis ######

# Count of missing values
is.na(taxi_updated_2)
colSums(is.na(taxi_updated_2))

# Missing Values: Company: 8838, Tolls: 2, others: 0

# Descriptive Statistics
describe(taxi_updated_2)

## Variable Extra has extreme values of $4169.83, $800
taxi_updated_3 = taxi_updated_2[-c(11453,19703),]

## Trip total have values where TripMile = 0 and TripSeconds = 0
taxi_Problem = taxi_updated_3 %>% 
select(Observation,StartDate,StartTime,EndDate,EndTime,Company,Extras,Fare,PaymentType,Tips,Tolls,
       TripMiles,TripSeconds,TripTotal,DropoffCensusTract,DropoffCentroidLatitude,DropoffCentroidLongitude,DropoffCentroidLocation,DropoffCommunityArea,
       Dropoff_Community_Name,PickupCencusTract,PickupCentroidLatitude,PickupCentroidLongitude,PickupCentroidLocation,PickupCommunityArea,Pickup_Community_Name) %>%
filter(TripMiles == 0 & TripSeconds == 0)

# Select Subset of Observations where either TripMiles != 0 or TripSeconds != 0
taxi_updated_4 = subset(taxi_updated_3, TripMiles != 0 | TripSeconds != 0, select=Observation:Pickup_Community_Name)

# TripMiles Extreme Values
taxi_updated_5 = subset(taxi_updated_4, TripMiles < 1000, select=Observation:Pickup_Community_Name)

# TripSeconds Extreme Values
taxi_updated_6 = subset(taxi_updated_5, TripSeconds < 15000, select=Observation:Pickup_Community_Name)

# Fares Extreme Values
taxi_updated_7 = subset(taxi_updated_6, Fare < 150, select=Observation:Pickup_Community_Name)

# Fare + Extra + tips = TripTotal
taxi_Inconsistent = subset(taxi_updated_6, Fare + Extras + Tips + Tolls != TripTotal, select=Observation:Pickup_Community_Name)

# Inconsistent Date: TripMiles != Fare
# Fare = $3.25 + (TripMiles - 1) * 2.25 
# For 100 miles, Fare should be at least $226
# For 60 miles, Fare should be at least $136
taxi_updated_8 = subset(taxi_updated_7, TripMiles < 60, select=Observation:Pickup_Community_Name)

# Delete Inconsistent data
taxi_updated_9 = subset(taxi_updated_8, !(TripMiles %in% c(55.30,48.50,44.30)), select=Observation:Pickup_Community_Name)
taxi_updated_9 = subset(taxi_updated_8, !(TripMiles %in% c(43.80,38.90,32.90)), select=Observation:Pickup_Community_Name)

# All Observations that have TripMiles > Fare is incorrect
taxi_updated_10 = subset(taxi_updated_9, TripMiles < Fare, select=Observation:Pickup_Community_Name)

# Export the Dataset to Excel.xlsx
library(openxlsx)
write.xlsx(taxi_updated_10, file = "taxi_cleaned.xlsx")

## Include the column Pickup Zipcode-Community
# Import the dataset taxi_cleaned
taxi_cleaned <- read_excel("Documents/DePaul Documents/DePaul University/Master of Science in Predictive Analytics /CSC 465 Data Visualization/Final Project/taxi_cleaned.xlsx")

# Combine the Zipcode column that was created using Reverse Geo-coding

# Reformat the time
taxi_cleaned$Start_Date_Time = as.POSIXct(paste(taxi_cleaned$StartDate, taxi_cleaned$StartTime), format="%Y-%m-%d %H:%M:%S")
taxi_cleaned$End_Date_Time = as.POSIXct(paste(taxi_cleaned$EndDate, taxi_cleaned$EndTime), format="%Y-%m-%d %H:%M:%S")



# Histograms for Variable Extra: highly skewed to the right 
ggplot(taxi_updated_10, aes(Extras)) + geom_histogram(breaks=seq(0, 8, by=0.5))
ggplot(taxi_updated_10, aes(Fare)) + geom_histogram()


# Circle Network Graph
Pickup_Dropff = data.frame(taxi_updated_10$Pickup_Community_Name,taxi_updated_10$Dropoff_Community_Name)

g <- graph.data.frame(Pickup_Dropff,directed = FALSE)
par(mar=c(0,0,0,0))
V(g)$name
plot(g,layout = layout.circle,vertex.size = 4,vertex.label = V(g)$name,vertex.label.cex = 0.8, vertex.label.dist = 0.4,vertex.label.color = "black")

#Create Violin plot For TripMiles 
#Import pivot table
Pickup_ <- read_excel("Desktop/Pickup .xlsx",col_types = c("text", "numeric"))

# Base plot
# Exclude trip miles = 0
Pickup_1 = subset(Pickup_, TripMiles > 1 & TripMiles <= 20, select=Pickup_Community_Name:TripMiles)
# Since the large range, log transform the scale 
Pickup_1$log2 = log2(Pickup_1$TripMiles)
p = ggplot(Pickup_1, aes(x = reorder(Pickup_Community_Name, log2, FUN=median), y = log2, fill = Pickup_Community_Name))
p + geom_violin()  


# boxplot overlaid
p + geom_violin() + geom_boxplot(width = 0.1, fill = "black", outlier.colour = NA) + 
  stat_summary(fun.y = median, geom = "point", fill = "white", shape = 21, size = 2.5) + 
  ggtitle("Violin plot: Distributions of the TripMiles for the Top 5 Locations") +
  theme(plot.title = element_text(hjust = 0.5, size = 22,face = "bold")) +                              #Align the title to the middle; Increase the size of the title 
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold")) +   #Increase the size of the axis label and tick marks)
  theme(legend.text=element_text(size=12)) + scale_y_continuous(breaks=seq(0, 5, 1)) +
  scale_x_discrete("Pickup_Community_Name, Order by Median Log2 TripMiles") +
  scale_y_continuous("log 2 TripMiles") +
  theme(legend.title = element_text(colour="black", size=12, face="bold"))

#Create Violin plot For Tips
#Import pivot table
Pickup_ <- read_excel("Desktop/Pickup_Top .xlsx",col_types = c("text", "numeric"))

# Base plot
# Exclude tips < 1
Pickup_T = subset(Pickup_Top, Tips >= 1, select=Observation:Pickup_Zipcode)
# Since the large range, log transform the scale 
Pickup_T$log2 = log2(Pickup_T$Tips)
q = ggplot(Pickup_T, aes(reorder(Pickup_Community_Name, log2, FUN=median), y = log2, fill = Pickup_Community_Name))
q + geom_violin()  

q + geom_violin() + geom_boxplot(width = 0.1, fill = "black", outlier.colour = NA) + 
  stat_summary(fun.y = median, geom = "point", fill = "white", shape = 21, size = 2.5) + 
  ggtitle("Violin plot: Distributions of the Tips for the Top 5 Locations") +
  theme(plot.title = element_text(hjust = 0.5, size = 22,face = "bold")) +                              #Align the title to the middle; Increase the size of the title 
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold")) +   #Increase the size of the axis label and tick marks)
  theme(legend.text=element_text(size=12)) + scale_y_continuous(breaks=seq(0, 5, 1)) +
  scale_x_discrete("Pickup_Community_Name, Order by Median Log2 Tips") +
  scale_y_continuous("log 2 Tips") +
  theme(legend.title = element_text(colour="black", size=12, face="bold"))

#Network Graph
#Import Network_cleaned
Network_cleaned <- read_excel("Desktop/Network_cleaned.xlsx", sheet = "Sheet2")

#Create a dataframe
Network_1 = data.frame(Network_cleaned$Pickup_Community_Name,Network_cleaned$Dropoff_Community_Name)

library(igraph)
g = graph.data.frame(Network_1,directed = TRUE)

plot(g,layout = layout.circle,vertex.size = 60, vertex.label = Network_1$Pickup_Community_Name
                                              , vertex.label.cex = 1,
                                              , vertex.label.color = "black"
                                              , edge.arrow.size = 0.5
                                              , edge.color = "red")

#Network Graph
#Import Network_cleaned_full
Network_cleaned <- read_excel("Desktop/Network_cleaned.xlsx", sheet = "Sheet2")

#Create a dataframe
Network_F = data.frame(Network_cleaned$Pickup_Community_Name,Network_cleaned$Dropoff_Community_Name)

library(igraph)
g = graph.data.frame(Network_F,directed = TRUE)

plot(g,vertex.size = 60, vertex.label = Network_1$Pickup_Community_Name
     , vertex.label.cex = 1,
     , vertex.label.color = "black"
     , edge.arrow.size = 0.5
     , edge.color = "red")
