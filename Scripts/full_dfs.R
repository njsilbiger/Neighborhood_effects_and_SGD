### Creating two main data frames for analysis Markdown file #################
# one for coral responses, the other for biogeochemistry
##############################################################################

 
### load libraries ###########################################################
library(tidyverse)
library(here)


### load data ################################################################

### first, coral trait data #####
data <- read_csv(here("Data",
                      "Coral",
                             "data_wide_2022_10_25.csv"))

### then, biogeochemical data ####
bgc <- read_csv(here("Data",
                     "Site",
                          "bgc_stats_2022_10_25.csv")) %>%
  filter(!grepl("cv", Predictor_measurement)) %>%
  filter(!grepl("Silicate", Predictor_measurement)) %>%
  filter(!grepl("Ammonia", Predictor_measurement)) %>%
  filter(Predictor_measurement != "del15N" & Predictor_measurement != "N_percent")

bgc_scaled <- read_csv(here("Data",
                            "Site",
                     "bgc_stats_scaled_2022_10_25.csv")) %>%
  filter(!grepl("cv", Predictor_measurement)) %>%
  filter(!grepl("Silicate", Predictor_measurement)) %>%
  filter(!grepl("Ammonia", Predictor_measurement)) %>%
  filter(Predictor_measurement != "del15N" & Predictor_measurement != "N_percent")

### coral trait data ####
data1 <- data %>%
  select(CowTagID, FragmentID, Treatment, Response_measurement, Initial, Final) %>%
  filter(Response_measurement != "CCED") %>%
  mutate(Treatment = recode(Treatment, 'Monoculture' = 'No_neighbor', 'Space' = 'Dead_skeletal')) %>%
  write_csv(here("Data",
                 "Coral",
                 "coral_response.csv"))

### site characteristics data ###

bgc1 <- left_join(bgc, bgc_scaled) %>%
  write_csv(here("Data",
                 "Site",
                 "site_biogeochemistry_summstats.csv"))
  

