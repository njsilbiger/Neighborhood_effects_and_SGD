### Symbiodinaceae chlorophyll a content #####################################
# Measured on 2022_03_27 at Gump Station
# Created by Jamie Kerlin
# Created on 2022_03_28
##############################################################################

### Load libraries ###########################################################
library(tidyverse)
library(here)

### Load data ################################################################
plate_map <- read_csv(here("Data_Raw",
                           "Coral",
                           "Chlorophyll_Content",
                           "spec_plate_maps_2022_03_27.csv"))


surface_area_all <- read_csv(here("Data",
                                  "Coral",
                                  "coral_surface_area.csv")) %>%
  filter(Fragment_type == "Post_exp" | Fragment_type == "Physio_initial")

dc <- read_csv(here("Data_Raw",
                    "Site",
                    "donor_colony_assignment.csv"))

blastate_volume <- read_csv(here("Data_Raw",
                                 "Coral",
                                 "Other_Coral_Measurements",
                                 "volume_blastate.csv")) %>%
  select(Fragment_ID, Blastate_volume_mL)

ed <- read_csv(here("Data",
                    "Coral",
                    "Endosymbiont_Physiology",
                    "endosymbiont_density.csv"))

#Run 1
run1_630 <- read_csv(here("Data_Raw/Coral/Chlorophyll_content/Spec_results/plate_run1_2022_03_27_stats630.csv"))
run1_663 <- read_csv(here("Data_Raw/Coral/Chlorophyll_content/Spec_results/plate_run1_2022_03_27_stats663.csv"))  
run1_750 <- read_csv(here("Data_Raw/Coral/Chlorophyll_content/Spec_results/plate_run1_2022_03_27_stats750.csv"))

#Run 2
run2_630 <- read_csv(here("Data_Raw/Coral/Chlorophyll_content/Spec_results/plate_run2_2022_03_27_stats630.csv"))
run2_663 <- read_csv(here("Data_Raw/Coral/Chlorophyll_content/Spec_results/plate_run2_2022_03_27_stats663.csv"))  
run2_750 <- read_csv(here("Data_Raw/Coral/Chlorophyll_content/Spec_results/plate_run2_2022_03_27_stats750.csv"))

#Run 3
run3_630 <- read_csv(here("Data_Raw/Coral/Chlorophyll_content/Spec_results/plate_run3_2022_03_27_stats630.csv"))
run3_663 <- read_csv(here("Data_Raw/Coral/Chlorophyll_content/Spec_results/plate_run3_2022_03_27_stats663.csv"))  
run3_750 <- read_csv(here("Data_Raw/Coral/Chlorophyll_content/Spec_results/plate_run3_2022_03_27_stats750.csv"))

### Join data ###############################################################

#need to join the data for each run first and then join all runs
run1 <- left_join(run1_630, run1_663)
run1_full <- left_join(run1, run1_750) %>%
  mutate(Run = 1)
view(run1_full)

run2 <- left_join(run2_630, run2_663)
run2_full <- left_join(run2, run2_750)%>%
  mutate(Run = 2)
view(run2_full)

run3 <- left_join(run3_630, run3_663)
run3_full <- left_join(run3, run3_750)%>%
  mutate(Run = 3) %>%
  unique()
view(run3_full)

run_full <- rbind(run1_full, run2_full)
run_final <- rbind(run_full, run3_full)
view(run_final)

# we then join these together with the plate map so we have fragment ID
final_data <- left_join(run_final, plate_map) %>%
  na.omit()
final_data1 <- full_join(final_data, blastate_volume)

### Chlorophyll content calculations ########################################
# Based on equations from Jeffery and Humphrey 1975
# units are ug/mL
# the path length is different for the 96-well plate used here. 
# the path length adjustment I received from Danielle Becker & is used in the Putnam lab.
# first, we need to subtract the 750 nm value from all measurements

final_data2 <- final_data1 %>%
  mutate(adj_663 = a_663-a_750,
         adj_630 = a_630-a_750) %>%
  mutate(chla_ug_ml = (11.43*adj_663)/0.6-(0.64*adj_630)/0.6, #chlorophyll a standardized to path length
         chlc2_ug_ml = (27.09*adj_630)/0.6-(3.63*adj_663)/0.6) %>% #chlorophyll c2 standardized to path length
  mutate(chla_ug = chla_ug_ml * Blastate_volume_mL,
         chlc2_ug = chlc2_ug_ml * Blastate_volume_mL) %>%
  rename(FragmentID = Fragment_ID) %>%
  filter(FragmentID != "BLANK") %>%
  mutate(Fragment_type = recode(Fragment_type, "ST_PRE" ="Physio_initial", "ST_POST" = "Post_exp"))
  # write_csv(here("Short_term",
  #                "Data",
  #                "Endosymbiont_physiology",
  #                "chlorophyll_content.csv"))

# final_datatest <- final_data2 %>% #looking at error
#   select(FragmentID, chla_ug) %>%
#   group_by(FragmentID) %>%
#   mutate(mean = mean(chla_ug),
#             sd = sd(chla_ug),
#          high = (sd*3) + mean,
#          low = mean - (sd*3)) %>%
#   filter(chla_ug > high & chla_ug < low)

### This is not the final dataset- I will need to normalize it to endosymbiont density (also normalized to surface area)
# Maybe for now, just normalize these to surface area

# My surface area datasets for initial vs. post-exp fragments have different columns because
# for some of the post-exp fragments, I had multiple pieces that I had to add the SAs of together.
# So, I'll remove some columns I don't need and then rename the columns to be the same.



surface_area_all2 <- surface_area_all %>%
  select(FragmentID, Fragment_type, totalSA_cm2)

# now join this with the chlorophyll data

chl_sanorm <- left_join(final_data2, surface_area_all2) %>%
  mutate(chla_ug_cm2 = chla_ug/totalSA_cm2, 
         chlc2_ug_cm2= chlc2_ug/totalSA_cm2) %>%
  group_by(FragmentID) %>%
  mutate(mean_chla_cm2 = mean(chla_ug_cm2, na.rm = TRUE), #calculate means and se for each fragment to see if any replicates look off
         mean_chlc2_cm2 = mean(chlc2_ug_cm2, na.rm = TRUE),
         mean_chla = mean(chla_ug, na.rm = TRUE),
         mean_chlc2 = mean(chlc2_ug, na.rm = TRUE))


#now I want to calculate the change in chlorophyll content

chl_sanorm_pre <- chl_sanorm %>%
  filter(Fragment_type == "Physio_initial") %>%
  select(FragmentID, mean_chla, mean_chla_cm2) %>%
  rename(CC_initial = mean_chla,
         CC_cm2_initial = mean_chla_cm2,
         Donor_Colony = FragmentID) %>%
  left_join(dc) %>%
  select(!CowTagID) %>%
  rename(CowTagID = PlateID) %>%
  unique()

chl_sanorm_pre$CowTagID <- as.character(chl_sanorm_pre$CowTagID)

chl_sa_norm2 <- chl_sanorm %>%
  filter(Fragment_type == "Post_exp") %>%
  mutate(FragmentID2 = FragmentID) %>%
  separate(FragmentID, into = c("CowTagID", "Treatment"), "(?<=[0-9])(?=[a-z])") %>%
  rename(FragmentID = FragmentID2,
         CC_final = mean_chla,
         CC_cm2_final = mean_chla_cm2) %>%
  select(CowTagID, FragmentID, CC_final, CC_cm2_final) %>%
  filter(CowTagID != "BLANK")%>%
  unique() %>%
  left_join(chl_sanorm_pre) %>%
  write_csv(here("Data",
                 "Coral",
                 "Endosymbiont_Physiology", 
                 "chlorophyll_content_sanorm.csv"))
  
