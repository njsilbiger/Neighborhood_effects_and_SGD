### Turbinaria Nutrient Data August 2021 ####################
# Created by Jamie Kerlin
# Created on 2021-10-06
#############################################################

### Load libraries ##########################################
library(tidyverse)
library(here)

### Load data ###############################################
turb <- read_csv(here("Data_Raw", "Site", "Biogeochemistry", "Turb_NC.csv"))

### Manipulate and clean data ###############################
turb1 <- turb %>%
  filter(stringr::str_detect(CowTagID, 'V')) %>%
  filter(CowTagID != "Vseep")
  
  
turb1$CowTagID  <- sub('.', '', turb1$CowTagID)

turb1 %>% write_csv(here("Data", "Site", "turb_nutrients.csv"))
