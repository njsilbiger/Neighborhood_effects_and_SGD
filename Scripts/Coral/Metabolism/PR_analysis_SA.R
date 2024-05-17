### PR Short-term Data Visualization #####################################
# Analysis and visualization of Photosynthesis & Respiration data
# USING SNAPSHOT OF NUTRIENTS
# Created by Jamie Kerlin
# Created on 2021-09-14
##########################################################################

### Load libraries #######################################################
library(tidyverse)
library(here)

### Load data ############################################################
PRfinal <- read_csv(here("Data",
                         "Coral",
                         "Metabolism",
                         "PR_final_normalized_2022_06_28.csv")) #PR post-deployment fragments

PRinitial <- read_csv(here("Data",
                           "Coral",
                           "Metabolism",
                         "PR_initial_normalized_2022_06_28.csv")) #PR of fragment from donor colonies (no manipulation)



### Manipulate data ######################################################
PRfinal1 <- PRfinal %>%
  select(PlateID, CowTagID, FragmentID, totalSA_cm2, NetPhoto, GrossPhoto, Respiration) %>% #select only columns needed
  rename("NetPhotoFinal" = NetPhoto, #rename columns with "final" so can combine dataframes
         "SurfaceAreaFinal" = totalSA_cm2, 
         "GrossPhotoFinal" = GrossPhoto,
         "RespirationFinal" = Respiration)

PRinitial1 <- PRinitial %>%
  select(PlateID, CowTagID, Surface_area_cm2, NetPhoto, NetPhoto, GrossPhoto, Respiration) %>% #select only columns needed
  rename("NetPhotoInitial" = NetPhoto, #rename columns with "initial" so can combine dataframes
         "SurfaceAreaInitial" = Surface_area_cm2,
         "GrossPhotoInitial" = GrossPhoto,
         "RespirationInitial" = Respiration)

PRfragmentID <- PRfinal %>%
  select(PlateID, FragmentID) #take plateID and fragmentID from final PR so can add to PRinitial

PRcombined <- right_join(PRinitial1, PRfragmentID) #add to PR initial so can combine dataframes

PRcombined1 <- left_join(PRcombined, PRfinal1) %>% #join dataframes
  mutate(Treatment = case_when( #create new column for treatment based on letter in FragmentID column
    endsWith(FragmentID, "m") ~ "Monoculture",
    endsWith(FragmentID, "s") ~ "Space",
    endsWith(FragmentID, "c") ~ "Conspecific",
    endsWith(FragmentID, "h") ~ "Heterospecific",
  )) %>%
  mutate(pc_change_NP = ((NetPhotoFinal - NetPhotoInitial)/NetPhotoInitial) * 100, #change all PR variables to percent change
         pc_change_GP = ((GrossPhotoFinal - GrossPhotoInitial)/GrossPhotoInitial) * 100,
         pc_change_R = ((RespirationFinal - RespirationInitial)/RespirationInitial) * 100) %>%
  write_csv(here("Data",
                 "Coral",
                 "Metabolism",
                 "PR_wider_2022_06_28.csv"))


