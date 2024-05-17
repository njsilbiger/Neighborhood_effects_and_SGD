### Temperature data from CT loggers #########################################
# Getting temperature data from the loggers on the dates that my exp was out
# Created by Jamie Kerlin
# Created on 2022_06_06
##############################################################################

### Load libraries ###########################################################
library(tidyverse)
library(here)
library(lubridate)

### Load data ################################################################
temp_thru811 <- read_csv(here("Data_Raw",
                              "Site",
                              "Biogeochemistry",
                              "CT_Loggers",
                              "Full_CT_08112021.csv"))

temp_metadata <- read_csv(here("Data_Raw",
                               "Site",
                               "Biogeochemistry",
                               "CT_Loggers",
                               "CTLoggerIDMetaData_Plates.csv"))

# There will be a separate file for the rest of the dates of my experiment, 
# the CTs were pulled halfway through and relaunched.
# Currently, this data hasn't been processed, so I need to pull raw csv files.

### Get data from raw csv files ##############################################
path.p <- here("Data_Raw",
               "Site",
               "Biogeochemistry",
               "CT_Loggers",
             "CT_post08112021") #the location of the files

files <- dir(path.p, pattern = ".csv") #set the file path

data_CTtemp <- tibble(filename = files) %>% #bring in each of the files and keep the file name bc it has logger info
  mutate(file_contents = map(filename,      
                             ~ read_csv(file.path(path.p, .))) 
  ) %>%
  unnest()

temp_thru817 <- data_CTtemp %>% # separate the filename into columns so we can keep logger info
  separate(filename, into = c("CT", "LoggerID", "Site"), sep = "_") %>%
  select(!c(CT, Site))


### Clean data ###############################################################

# Ok, now I have 2 datasets for temp and date- one covers the first half of 
# my experiment, 8/3 - 8/11. The other dataset covers the rest (experiment 
# collected on 8/17).

# I want to bind the two datasets, so I'll first change the columns to match

temp_thru811_new <- temp_thru811 %>%
  rename(temp = TempInSitu) %>% 
  select(date, LoggerID, temp)

temp_data <- rbind(temp_thru811_new, temp_thru817) #bind the data

# Then, I'll want to separate the date columns into date and time

temp_data$date_time <- mdy_hm(temp_data$date)

# I want to only select the dates that my experiment was out there

temp_data1 <- temp_data %>%
  select(!date) %>%
  separate(date_time, into = c("date", "time"), sep = " ") %>%
  filter(as.Date(date) <= "2021-08-17" & as.Date(date) >= "2021-08-03") %>%
  unite(date, date, time, sep = " ")

# Now want to clean the metadata so we can join

temp_metadata$date_launched <- mdy(temp_metadata$Date_launched)

temp_metadata1 <- temp_metadata %>%
  filter(Site == "Varari") %>% #only at one site
  filter(as.Date(date_launched) <= "2021-08-17" & as.Date(date_launched) >= "2021-08-03") 

temp_metadata2 <- temp_metadata1 %>%
  unite(Date_launched, Date_launched, Time_launched, sep = " ") %>%
  unite(Date_retrieved, Date_retrieved, Time_retrieved, sep = " ")

temp_metadata2$Date_launched <- mdy_hms(temp_metadata2$Date_launched)
temp_metadata2$Date_retrieved <- mdy_hms(temp_metadata2$Date_retrieved)
temp_data1$date <- ymd_hms(temp_data1$date)

temp_metadata2$LoggerID <- as.character(temp_metadata2$LoggerID) #change to character so they are the same

temp_final <- right_join(temp_metadata2, temp_data1) %>% # join data
  unique() %>%
  mutate(logging = ifelse(date > Date_launched & date < Date_retrieved, 1, 0)) %>%
  filter(logging == 1) %>%
  separate(date, into = c("date", "time"), sep = " ")

#double check these all look clean
temp_v <- temp_final %>%
  filter(CowTagID == "V20")

ggplot(temp_v, mapping = aes(x = time, y = temp)) +
  geom_point() +
  facet_wrap(~date) +
  labs(title = "") +
  scale_x_discrete(breaks=c("00:00:00","08:00:00","16:00:00", "23:00:00"))

ggsave(here("Output",
            "Site",
            "Biogeochemical",
            "v20_temp.png"),
       width = 12, height = 7, device = "png")


temp_final1 <- temp_final %>%
  group_by(CowTagID) %>%
  mutate(minTemp = min(temp),
         maxTemp = max(temp),
         meanTemp = mean(temp),
         rangeTemp = maxTemp - minTemp,
         cvTemp = sd(temp)/mean(temp)) %>%
  write_csv(here("Data",
                 "Site",
                 "CT_temp.csv"))

temp_final <- read_csv(here("Data",
                            "Site",
                 "CT_temp.csv"))

temp_final$CowTagID <- as.character(temp_final$CowTagID)

temp_final1 <- temp_final %>% #then save another file with only this info for simplicity
  select(CowTagID, minTemp, maxTemp, meanTemp, rangeTemp, cvTemp) %>%
  mutate(CowTagID = sub('V', '', CowTagID)) %>% #change cowtagid column to match other datasets
  unique() %>%
  write_csv(here("Data",
                 "Site",
                 "CT_temp_summ.csv"))


# temp_metadata
