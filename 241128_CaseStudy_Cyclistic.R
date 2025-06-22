## Google Data Analytics Career Certificate - Cyclistic Case Study
## Analyzing Cyclistic/Divvy bike sharing data to understand usage and try to get more subscribers from casual users

## Import & Installation of Libraries
install.packages("tidyverse")
library(tidyverse)
install.packages("hydroTSM")
library("hydroTSM")
install.packages("readr")
library(readr)
install.packages("dplyr")
library(dplyr)
install.packages("forcats")
library(forcats)

## Building the main dataframe from .csv files in the working directory
df2023 <- list.files(pattern = "*.csv", full.names = T) %>% 
  map_df(~read_csv(.))

## Alt-building the main dataframe from a merged .csv files containing the full year
df2023 <- read_delim("2023-divvy-tripdata-fullyear.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

## Getting rid of null values in the dataframe
df2023 <- drop_na(df2023)

## Time conversions and creation of a new column with calculated travel time for each each trip
df2023$started_at <- ymd_hms(df2023$started_at)
df2023$ended_at <- ymd_hms(df2023$ended_at)
df2023$travel_time <- df2023$ended_at - df2023$started_at
df2023$travel_time <- as.numeric(df2023$travel_time, units="mins") 

## Removing irrelevant/falty travel times
df2023 <- subset(df2023, travel_time >= 1)

## Creation of new columns for day of the week and season
df2023$day_of_week <- strftime(df2023$started_at, "%A")
df2023$season <- time2season(df2023$started_at, out.fmt = "seasons")
df2023 <- df2023 %>%
  mutate(season = recode(season,
                         "spring" = "Spring",
                         "summer" = "Summer",
                         "autumn" = "Autumn",
                         "winter" = "Winter"),
         season = factor(season, levels = c("Spring", "Summer", "Autumn", "Winter")))

## Refactoring the day_of_week column (days of week are in French)
df2023 <- df2023 %>%
  mutate(day_of_week = recode(day_of_week,
                              "lundi" = "Monday",
                              "mardi" = "Tuesday",
                              "mercredi" = "Wednesday",
                              "jeudi" = "Thursday",
                              "vendredi" = "Friday",
                              "samedi" = "Saturday",
                              "dimanche" = "Sunday"),
         day_of_week = factor(day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))

## Renaming columns names for columns with French names
colnames(df2023)[colnames(df2023) == "station_de_depart"] <- "start_station_name"
colnames(df2023)[colnames(df2023) == "station_d_arrivee"] <- "end_station_name"
colnames(df2023)[colnames(df2023) == "type_utilisateur"] <- "member_casual"

## Counting the number of trips by type of user
df2023 %>% 
  count(member_casual) %>% 
  ggplot() +
  geom_col(mapping = aes(x = member_casual, y = n, fill = member_casual)) +
  geom_text(mapping = aes(x = member_casual, y = n, label = n, fontface = "bold"), nudge_y = -50000) +
  theme(axis.text.x = element_text(face = "bold"), axis.title = element_text(face = "bold"), title = element_text(face = "bold")) +
  labs(x = "User Type", y = "Number", title = "Number of service uses by user type")

## Average travel times by type of user
# Calculating Averages
averages <- df2023 %>%
  group_by(member_casual) %>%
  summarise(mean_time = mean(travel_time))
# Graph with Labels
ggplot(df2023, aes(x = member_casual, y = travel_time, fill = member_casual)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  geom_text(data = averages, aes(x = member_casual, y = mean_time, label = round(mean_time, 1)), 
            vjust = -0.5, fontface = "bold") +
  theme(axis.text.x = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        title = element_text(face = "bold")) +
  labs(x = "User Type",
       y = "Average travel time (min)",
       title = "Average travel time by user type")

## Trips by Season and User Type
# Data preparation with percentages
seasonal_props <- df2023 %>%
  count(season, member_casual) %>%
  group_by(season) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(season = factor(season, levels = c("Spring", "Summer", "Autumn", "Winter")))
# Graphs with percentages as labels
ggplot(seasonal_props, aes(x = season, y = n, fill = member_casual)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_dodge(width = 0.9),
            vjust = -0.5, fontface = "bold", size = 3.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        title = element_text(face = "bold")) +
  labs(x = "Season",
       y = "Number of Trips",
       fill = "User Type",
       title = "Share of Trips by Season and User Type")

## Creating a dataframe for casual users
df2023_casuals <- filter(df2023, member_casual == "casual")

## Creating a dataframe for subscribed members
df2023_members <- filter(df2023, member_casual == "member")

## Plotting the type of bike used by casual users
ggplot(df2023_casuals) + 
  geom_bar(mapping = aes(x = rideable_type, fill = rideable_type)) +
  theme(axis.text.x = element_text(face = "bold"), axis.title = element_text(face = "bold"), title = element_text(face = "bold")) +
  labs(x = "Type of bike", y = "Number of trips", title = "Share of types of bikes used by casual users")

## Plotting the type of bike used by subscribed members
ggplot(df2023_members) +  
  geom_bar(mapping = aes(x = rideable_type, fill = rideable_type)) +
  theme(axis.text.x = element_text(face = "bold"), axis.title = element_text(face = "bold"), title = element_text(face = "bold")) +
  labs(x = "Type of bike", y = "Number of trips", title = "Share of types of bikes used by subscribed members")

## Extracting and plotting the top 10 start stations of casual users
df2023_casuals_shortlist_startstation <- select(df2023_casuals, start_station_name)
df2023_casuals_shortlist_startstation %>%
  count(start_station_name) %>% 
  arrange(desc(n)) %>% 
  top_n(10, n) %>% 
  ggplot() + 
  geom_col(mapping = aes(x = reorder(start_station_name, -n), y = n, fill = start_station_name)) + 
  geom_text(aes(x = reorder(start_station_name, -n), y = n, label = n, fontface = "bold"), nudge_y = -500) + 
  theme(axis.text.x = element_text(angle = 90, face = "bold", hjust = 1, vjust = 0.2), axis.title = element_text(face = "bold"), title = element_text(face = "bold")) + 
  labs(x = "Start station", y = "Number of trips", title = "Top 10 start stations of casual users")

## Extracting and plotting the top 10 end stations of casual users
df2023_casuals_shortlist_endstation <- select(df2023_casuals, end_station_name)
df2023_casuals_shortlist_endstation %>% 
  count(end_station_name) %>% 
  arrange(desc(n)) %>% 
  top_n(10, n) %>% 
  ggplot() +
  geom_col(mapping = aes(x = reorder(end_station_name, -n), y = n, fill = end_station_name)) +
  geom_text(aes(x = reorder(end_station_name, -n), y = n, label = n, fontface = "bold"), nudge_y = -500) +
  theme(axis.text.x = element_text(angle = 90, face = "bold", hjust = 1, vjust = 0.2), axis.title = element_text(face = "bold"), title = element_text(face = "bold")) +
  labs(x = "End station", y = "Number of trips", title = "Top 10 end stations of casual users")

## Extracting and plotting the top 10 start stations of subscribed members
df2023_members_shortlist_startstation <- select(df2023_members, start_station_name)
df2023_members_shortlist_startstation %>%
  drop_na() %>% 
  count(start_station_name) %>% 
  arrange(desc(n)) %>% 
  top_n(10, n) %>% 
  ggplot() + 
  geom_col(mapping = aes(x = reorder(start_station_name, -n), y = n, fill = start_station_name)) + 
  geom_text(aes(x = reorder(start_station_name, -n), y = n, label = n, fontface = "bold"), nudge_y = -500) + 
  theme(axis.text.x = element_text(angle = 90, face = "bold", hjust = 1, vjust = 0.2), axis.title = element_text(face = "bold"), title = element_text(face = "bold")) + 
  labs(x = "Start station", y = "Number of trips", title = "Top 10 start stations of subscribed members")

## Extracting and plotting the top 10 end stations of subscribed members
df2023_members_shortlist_endstation <- select(df2023_members, end_station_name)
df2023_members_shortlist_endstation %>% 
  drop_na() %>% 
  count(end_station_name) %>% 
  arrange(desc(n)) %>% 
  top_n(10, n) %>% 
  ggplot() +
  geom_col(mapping = aes(x = reorder(end_station_name, -n), y = n, fill = end_station_name)) +
  geom_text(aes(x = reorder(end_station_name, -n), y = n, label = n, fontface = "bold"), nudge_y = -500) +
  theme(axis.text.x = element_text(angle = 90, face = "bold", hjust = 1, vjust = 0.2), axis.title = element_text(face = "bold"), title = element_text(face = "bold")) +
  labs(x = "End station", y = "Number of trips", title = "Top 10 end stations of subscribed members")

## Plotting the number of trips by day of the week of casual users 
df2023_casuals %>% 
  mutate(day_of_week = fct_relevel(day_of_week, "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) %>% 
  ggplot(aes(x = day_of_week)) + 
  geom_bar(aes(fill = day_of_week)) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  theme(axis.text.x = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        title = element_text(face = "bold")) +
  labs(x = "Week",
       y = "Number of trips",
       title = "Distribution of trips according to days of the week for casual users")

## Plotting the number of trips by day of the week of subscribed members 
df2023_members %>% 
  mutate(day_of_week = fct_relevel(day_of_week, "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) %>%
  ggplot(aes(x = day_of_week)) +  
  geom_bar(aes(fill = day_of_week)) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  theme(axis.text.x = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        title = element_text(face = "bold")) +
  labs(x = "Week",
       y = "Number of trips",
       title = "Distribution of trips according to days of the week for subscribed members")

