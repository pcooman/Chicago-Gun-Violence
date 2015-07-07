library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(grid)
library(reshape2)
library(ggmap)

data_dir <- "input"

# Original database can be found at the City of Chicago Data Portal (.csv file size = 1.3 GB)

# Start by filtering the data to only retain rows that mention "handgun" or "firearm"

#data <- read_csv(file.path(data_dir, "Chicago_City_Crimes.csv"))
## Rename column 6 ""Primary Type" --> Type
#colnames(data)[6] <- "Type"
#data_guns <- data[grepl("HANDGUN",data$Description) | grepl("FIREARM",data$Description), ]
#write_csv(data_guns,file.path(data_dir, "Chicago_Guns_Data.csv"))

data <- read_csv(file.path(data_dir, "Chicago_Guns_Data.csv"))

map <- get_map(location = "Chicago", maptype = "satellite", color = "bw")

#---------------------------------------------------------------------------------------------------------------------------
# Plot Assaults with Handgun since 2001
data_assault <- data %>% filter(Type == "ASSAULT")

windows()
p <- ggmap(map) +
  geom_point(data=data_assault, aes(x=Longitude,y=Latitude), color = "Red", alpha = 0.05) +
  ggtitle("Assaults with Handgun in Chicago\n 2001 - Present \n")
print(p)

#---------------------------------------------------------------------------------------------------------------------------
# Plot Assaults with Handgun in the most recent three years
data_assault_recent <- data_assault %>% filter(Year == 2015 | Year == 2014 | Year == 2013 | Year == 2012)

windows()
p <- ggmap(map) +
  geom_point(data=data_assault_recent, aes(x=Longitude,y=Latitude), color = "Red", alpha = 0.05) +
  facet_wrap(~Year, nrow = 1, ncol = 4) +
  ggtitle("Assaults with Handgun in Chicago\n 2001 - Present \n")
print(p)

#---------------------------------------------------------------------------------------------------------------------------
# Contingency Table of Gun Crimes By Year
GunCrimesByYear <- table(data$Type,data$Year) 
GunCrimesByYear

#---------------------------------------------------------------------------------------------------------------------------
# Plot Number of Assaults By Year
AssaultsByYear <- data_assault %>% group_by(Year) %>% summarize(Count = length(Type))

windows()
p <- ggplot(data = AssaultsByYear, aes(x=Year,y=Count)) +
  geom_line() +
  geom_point(color="Red",pch=1,size=5) + 
  ylim(0,3600) +
  scale_x_continuous(breaks = 2001:2015, labels = 2001:2015) +
  ggtitle("Total number of Assaults with Handgun by Year")
print(p)