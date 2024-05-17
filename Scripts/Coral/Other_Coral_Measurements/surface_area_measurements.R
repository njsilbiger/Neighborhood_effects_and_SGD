### Surface Area measurements ############################################
# Calculating surface area from wax dipping coral fragments
# Created by Jamie Kerlin
# Created on 2022_03_20
##########################################################################

### Load libraries #######################################################
library(tidyverse)
library(here)

### Load data ############################################################
sa <-  read_csv(here("Data_Raw",
                     "Coral",
                     "Other_Coral_Measurements",
                     "surface_area_2022_03_20.csv"))
dc <- read_csv(here("Data_Raw",
                    "Site",
                    "donor_colony_assignment.csv")) %>%
  select(!CowTagID) %>%
  rename(FragmentID = Donor_Colony, CowTagID = PlateID)
  
jamie_m <- .0208 #slope of jamie's calibration curve
jamie_c <- -0.0038 #intercept of jamie's calibration curve
sabrina_m <- .0293 #slope of sabrina's calibration curve
sabrina_c <- -0.0887 #intercept of sabrina's calibration curve

# wax dipping found y = mx + c
# to estimate surface area of coral, using x = (y-c)/m
# where y is the weight of the wax
# sabrina and I each did a calibration curve, so calculating each person's coral fragments
# with their own calibration curve

### Calculating surface area ###############################################
sa1 <- sa %>%
  mutate(Weight_wax_g = (Weight_after_g - Weight_before_g)) #calculate weight of wax

sa1_jamie <- sa1 %>%
  filter(Person_measuring == "Jamie") %>%
  mutate(Surface_area_cm2 = ((Weight_wax_g-jamie_c)/jamie_m)) #calculate surface area of fragments jamie measured

sa1_sabrina <- sa1 %>%
  filter(Person_measuring == "Sabrina") %>%
  mutate(Surface_area_cm2 = ((Weight_wax_g-sabrina_c)/sabrina_m)) #calculate surface area of fragments sabrina measured

sa1_combined <- rbind(sa1_jamie, sa1_sabrina) 

sa1_nopost <- sa1_combined %>%
  filter(Fragment_type != "Post_exp") %>%
  group_by(FragmentID, Fragment_type) %>%
  mutate(totalSA_cm2 = sum(Surface_area_cm2))

sa1_post <- sa1_combined %>%
  filter(Fragment_type == "Post_exp") %>%
  separate(FragmentID, into = c("FragmentID", "Extra", "ExtraID"), sep = "_") %>%
  select(!Extra) %>%
  group_by(FragmentID) %>%
  mutate(totalSA_cm2 = sum(Surface_area_cm2)) %>%
  filter(is.na(ExtraID)) %>%
  select(!ExtraID) %>%
  mutate(CowTagID1 = as.numeric(gsub("([0-9]+).*$", "\\1", FragmentID)))

#changed this code from original for new repo, but same data

final <- full_join(sa1_post, sa1_nopost) %>%
  select(CowTagID1, Fragment_type, FragmentID, totalSA_cm2) %>%
  left_join(dc) %>%
  unite("CowTagID",c(CowTagID, CowTagID1), na.rm = TRUE) %>%
  write_csv(here("Data",
                 "Coral",
                 "coral_surface_area.csv"))

