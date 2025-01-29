## Installing and Loading

installed.packages("dplyr")
install.packages('tidyverse')
install.packages("janitor")
install.packages("data.table")
install.packages("psych")
install.packages("hrbrthemes")
library(tidyverse)
library(janitor)
library(data.table)
library(psych)
library(hrbrthemes)
library(dplyr)

### Loading Data

data01 <- read.csv("C:/Users/austi/Downloads/divydata/202401-divvy-tripdata.csv")
data02 <- read.csv("C:/Users/austi/Downloads/divydata/202402-divvy-tripdata.csv")
data03 <- read.csv("C:/Users/austi/Downloads/divydata/202403-divvy-tripdata.csv")
data04 <- read.csv("C:/Users/austi/Downloads/divydata/202404-divvy-tripdata.csv")
data05 <- read.csv("C:/Users/austi/Downloads/divydata/202405-divvy-tripdata.csv")
data06 <- read.csv("C:/Users/austi/Downloads/divydata/202406-divvy-tripdata.csv")
data07 <- read.csv("C:/Users/austi/Downloads/divydata/202407-divvy-tripdata.csv")
data08 <- read.csv("C:/Users/austi/Downloads/divydata/202408-divvy-tripdata.csv")
data09 <- read.csv("C:/Users/austi/Downloads/divydata/202409-divvy-tripdata.csv")
data10 <- read.csv("C:/Users/austi/Downloads/divydata/202410-divvy-tripdata.csv")
data11 <- read.csv("C:/Users/austi/Downloads/divydata/202411-divvy-tripdata.csv")
data12 <- read.csv("C:/Users/austi/Downloads/divydata/202412-divvy-tripdata.csv")

### Combine Data and Save Locally

data_all <- rbind(
  data01,
  data02,
  data03,
  data04,
  data04,
  data05,
  data06,
  data07,
  data08,
  data09,
  data10,
  data11,
  data12
)


write.csv(("C:/Users/austi/Downloads/divydata/data_all.csv"))

### Remove Unnecessary Data
rm(data01)
rm(data02)
rm(data03)
rm(data04)
rm(data05)
rm(data06)
rm(data07)
rm(data08)
rm(data09)
rm(data10)
rm(data11)
rm(data12)

### Clean, Validate, and Consolidate Data

clean_data_all <- drop_na(data_all)
clean_data_all <- remove_missing(data_all)

clean_data_all <- clean_data_all %>%
  filter(started_at < ended_at)
clean_data_all <- rename(clean_data_all,
                         bike_type = rideable_type,
                         customer_type = member_casual)

summary(clean_data_all)
head(clean_data_all)
colnames(clean_data_all)
str(clean_data_all)
colSums(is.na(clean_data_all))

### Add some Date Columns for Pattern Recognition

clean_data_all$date <- as.Date(clean_data_all$started_at)
clean_data_all$week_day <- format(as.Date(clean_data_all$date), "%A")
clean_data_all$month <- format(as.Date(clean_data_all$date), "%b")
clean_data_all$time_start <- as.POSIXct(clean_data_all$started_at, format("%Y-%m-%d %H:%M:%S"))
clean_data_all$time_start <- format(clean_data_all$time_start, "%H:%M")
clean_data_all$time_rode <- difftime(clean_data_all$ended_at, clean_data_all$started_at, units = "mins", 2)

### New Dataframe for data I want to anaylize
analysis <- clean_data_all %>%
  select(
    bike_type,
    start_station_name,
    end_station_name,
    customer_type,
    date,
    week_day,
    month,
    time_start,
    time_rode
  )
### Round to two decimals
analysis$time_rode <- round(analysis$time_rode, 2)
### Time to filter out rides too long/short
analysis <- analysis[!analysis$time_rode > 1440, ]
analysis <- analysis[!analysis$time_rode < 3, ]
### I missed some drops on Start/ end station names, probably from reloading R. same code
analysis <- drop_na(analysis)
analysis <- remove_missing(analysis)
### Save to PC
write.csv(analysis, file = "C:/Users/austi/Downloads/divydata/analysis.csv", row.names = FALSE)

### Total time rode by customer type
View(setNames(
  aggregate(time_rode ~ customer_type, analysis, sum),
  c("customer_type", "time_rode(mins)")
))
### Mean/Median time rode by customer type
View(
  analysis %>%
    group_by(customer_type) %>%
    summarise(
      mean_length = mean(time_rode),
      median_length = median(time_rode)
    )
)

### Time rode on average per day of the week by Customer Type
view(analysis %>%
       group_by(week_day, customer_type) %>%
       summarise(avg_length = mean(time_rode)))

### Validation of Tableau
count(analysis, customer_type, bike_type)

#TIME FOR VIZ/Tableua
