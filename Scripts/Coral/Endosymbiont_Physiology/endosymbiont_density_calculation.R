### Calculating endosymbiont density from counts ##############################
# For SGD Field Experiment
# Created by Jamie Kerlin
# Created on 2021_07_12
###############################################################################

### Load libraries ############################################################
library(tidyverse)
library(here)

### Load data #################################################################
endosymbiont_counts <- read_csv(here("Data_Raw",
                                     "Coral",
                                     "Endosymbiont_Density",
                                     "kerlin_sym_plateII_results_06242022.csv"))

rerun <- read_csv(here("Data_Raw",
                       "Coral",
                       "Endosymbiont_Density",
                       "kerlin_sym_plateII_results_06292022.csv"))
#Wes re-ran some of the samples that gave weird errors/results

blastate_volume <- read_csv(here("Data_Raw",
                                 "Coral",
                                 "Other_Coral_Measurements",
                                 "volume_blastate.csv"))


surf_area2 <- read_csv(here("Data",
                            "Coral",
                            "coral_surface_area.csv"))

dc <- read_csv(here("Data_Raw",
                    "Site",
                    "donor_colony_assignment.csv"))

### Formula ###################################################################
# From our counts, we want to determine the volume of what we counted
# We should have detailed notes on the number and areas of cells we counted
# Cells counted should be consistent throughout all counts


# From flow cytometry, have # cells/uL... so want to scale this up to the blastate volume 
# then normalize to surface area of the coral


### Clean data ################################################################

### combine the original runs and the re-runs

endosymbiont_counts1 <- endosymbiont_counts %>%
  filter(FragmentID != "16c" & FragmentID != "16s" & FragmentID != "V" & FragmentID != "B" & FragmentID != "18s" & FragmentID != "20m" & FragmentID != "8c")

ec <- endosymbiont_counts1 %>%
  rbind(rerun)

#first, we'll clean the endosymbiont density data

ec1 <- ec %>%
  rename(fsc_uL = `sym_FSC Events/?L(V)`,  #rename columnd to ones easier to use in R
         ssc_uL = `sym_SSC Events/?L(V)`,
         fcm_vol_uL = `Volume(?L):`) %>%
  drop_na(Fragment_type) %>%  #drop NAs in this columns
  mutate(Fragment_type = recode(Fragment_type, 
                                "ST_PRE" = "Physiology_initial",
                                "ST_POST" = "Physiology_final"))
  

#then, we'll clean the blastate volume data
blv <- blastate_volume %>%
  rename(FragmentID = Fragment_ID) %>% #match the column names
  select(Fragment_type, FragmentID, Blastate_volume_mL)

#then, surface area 
sa <- surf_area2 %>%
  filter(Fragment_type == "Post_exp" | Fragment_type == "Physio_initial") %>% #filter for only the fragment types we need
  select(FragmentID, totalSA_cm2)



comb1 <- left_join(ec1, blv) #combine ed and blastate volume
comb2 <- left_join(comb1, sa) #combine ed and surface area

dc <- dc %>% #donor colony dataset with only fragmentID and cowtagID
  select(!CowTagID) %>%
  rename(FragmentID = Donor_Colony,
         CowTagID = PlateID)

dc$CowTagID <- as.character(dc$CowTagID)


final <- comb2 %>% #just final data
  filter(Fragment_type == "Physiology_final") %>% 
  mutate(CowTagID = str_remove_all(FragmentID, "[cmhs]")) #CowTagID column so can match donor colony to cowtag

int_step <- final %>% #only select columns needed
  select(CowTagID, FragmentID)

initial <- comb2 %>% #just initial data
  filter(Fragment_type == "Physiology_initial") %>%
  full_join(dc) %>% #combine with initial physio data
  select(!FragmentID)

comb3 <- left_join(initial, int_step)

comb4 <- full_join(final, comb3)

ssc_data <- comb4 %>% #want to save ssc data and fsc data as different dataframes
  filter(Notes == "use_ssc")

fsc_data <- comb4 %>%
  filter(is.na(Notes))

fsc_data_diluted <- comb4 %>% #want to also save the ones diluted separately too
  filter(Notes == "10X dilution")

# ### Now calculate the endosymbiont density per coral ##########################
# 
# # we have counts per uL, so need to scale that up to blastate volume
# # then normalize to surface area


ec_calcs <- fsc_data %>% #calculate # of  endosymbionts per blastate volume (uL to mL)
  mutate(ec_blastate_mL = fsc_uL * 1000 * Blastate_volume_mL) %>% 
  mutate(ec_mL_cm2 = ec_blastate_mL / totalSA_cm2) 

ec_calcs_ssc <- ssc_data %>% #calculate # of  endosymbionts per blastate volume (uL to mL)
  mutate(ec_blastate_mL = ssc_uL * 1000 * Blastate_volume_mL) %>%
  mutate(ec_mL_cm2 = ec_blastate_mL / totalSA_cm2)

ec_calcs_diluted <- fsc_data_diluted %>% #calculate # of  endosymbionts per blastate volume (uL to mL) & 10X diltion
  mutate(ec_blastate_mL = fsc_uL * 1000 * Blastate_volume_mL * 10) %>% #multiply by 10 for dilution
  mutate(ec_mL_cm2 = ec_blastate_mL / totalSA_cm2) 

ec_calcs_full <- rbind(ec_calcs, ec_calcs_ssc, ec_calcs_diluted) %>% #combine all files
  unique() %>% #keep only unique rows, get rid of duplicates
  select(CowTagID, Fragment_type, FragmentID, ec_mL_cm2) %>% #select columns we need
  pivot_wider(names_from = "Fragment_type", #pivot wider 
              values_from = "ec_mL_cm2") %>%
  filter(complete.cases(.)) %>%
  rename(ED_initial = Physiology_initial, ED_final = Physiology_final) %>%
  mutate(ED_final = ED_final*0.000001, #convert to cells x e-16
         ED_initial = ED_initial*0.000001) %>% 
  write_csv(here("Data",
                 "Coral",
               "Endosymbiont_Physiology",
               "endosymbiont_density.csv"))
