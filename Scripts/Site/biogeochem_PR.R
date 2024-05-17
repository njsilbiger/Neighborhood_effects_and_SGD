### Redoing biogeochemical + PR data
# Created by Jamie Kerlin
# Created on 2022_2_17

library(tidyverse)
library(here)
library(vtable)

### Load data ################################################################
bgc <- read_csv(here("Data",
                     "Site",
                     "site_biogeochemistry_qc.csv"))

PRw <- read_csv(here("Data",
                     "Coral",
                     "Metabolism", 
                     "PR_wider_2022_06_28.csv"))

turb <- read_csv(here("Data",
                      "Site",
                      "turb_nutrients.csv"))

temp_data <- read_csv(here("Data",
                           "Site",
                           "CT_temp_summ.csv"))

ed <- read_csv(here("Data",
                    "Endosymbiont_Physiology",
                    "endosymbiont_density.csv"))

cc <- read_csv(here("Data",
                    "Endosymbiont_Physiology",
                    "chlorophyll_content_sanorm.csv"))

### Clean data ##########################################################
bgc <- bgc %>%
  filter(Location == "Varari" & Plate_Seep == "Plate") #only Varari & plates
bgc$CowTagID  <- sub('V', '', bgc$CowTagID) #change cowtagid column to match other datasets
PRw$CowTagID <- as.character(PRw$CowTagID) #change to character
temp_data$CowTagID <- as.character(temp_data$CowTagID) 

bgc1 <- bgc %>%
  filter(DateTime != as.POSIXct("2021-08-06 06:40:00", tz = "UTC")) #filter out the bad sampling


### Clean turb data ##########
turb1 <- turb %>%
  select(!c(Sample_Weight, C_ug, N_ug)) #remove columns we don't need

turb1$CowTagID <- as.character(turb1$CowTagID) #change it so we can join

N_data_w <- turb1 %>%
  select(CowTagID, del15N, N_percent) %>%
  full_join(bgc1) %>%
  select(!c(Top_Plate_ID, Bottom_Plate_ID, Jamie_Plate_ID, Plate_Seep,
            M_C:Lignin_Like)) %>%
  write_csv(here("Data",
                 "Site",
                 "Biogeochem_clean.csv"))

sumtable <- N_data_w %>%
  select(pH, Salinity, NN_umolL, Phosphate_umolL)

sumtable(sumtable)

# ggplot(N_data_w, mapping = aes(x = Silicate_umolL, y = NN_umolL)) +
#   geom_point(color = "#5fad6a") +
#   geom_smooth(method = "lm", color = "#5fad6a") +
#   theme_bw() +
#   labs(x = "Silicate (umol/L)", y = "Nitrite + Nitrate (umol/L)")
# 
# N_data_w1 <- N_data_w %>%
#   filter(pH > 8)
# 
# ggplot(N_data_w1, mapping = aes(x = Silicate_umolL, y = pH)) +
#   geom_point(color = "#5d74a5") +
#   geom_smooth(method = "lm", color = "#5d74a5") +
#   theme_bw() +
#   labs(x = "Silicate (umol/L)", y = "pH")
# 
# summary(modelsilpH <- lm(pH ~ Silicate_umolL, data = N_data_w))
# summary(modelsilNN <- lm(NN_umolL ~ Silicate_umolL, data = N_data_w1))
#   
N_data <- turb1 %>%
  select(CowTagID, del15N, N_percent) %>%
  pivot_longer(cols = c(del15N, N_percent),
               names_to = "Predictor_measurement",
               values_to = "Predictor_value")

### Combine turb and biogeochemical data ###################################
combined <- full_join(bgc1, turb1) %>%
  select(!c(TA, del15N, N_percent, Temperature, C_N, C_percent, del13C, Top_Plate_ID:Plate_Seep, M_C:Lignin_Like)) %>%
  pivot_longer(cols = Salinity:Ammonia_umolL, #pivot longer
               names_to = "Predictor_measurement", 
               values_to = "Predictor_value") %>%
  group_by(CowTagID, Predictor_measurement) %>% #find summary stats for predictors
  summarise(mean = mean(Predictor_value, na.rm = TRUE),
         min = min(Predictor_value),
         max = max(Predictor_value),
         cv = (sd(Predictor_value)/mean(Predictor_value))) %>%
  mutate(range = max - min)

# test <- combined %>%
#   select(!cv) %>%
#   pivot_wider(names_from = Predictor_measurement,
#               values_from = c(min, max, range, mean))
# 
# ggplot(test, mapping = aes(max_Silicate_umolL, range_pH)) +
#   geom_point() +
#   geom_smooth(method = "lm")
# 
# ggplot(test, mapping = aes(min_Silicate_umolL, min_NN_umolL)) +
#   geom_point() +
#   geom_smooth(method = "lm")
# 
# test1 <- lm(range_Salinity ~ mean_Silicate_umolL, data = test)
# summary(test1)
# ggplot(test, mapping = aes(min_NN_umolL, range_pH)) +
#   geom_point()+
#   geom_smooth(method = "lm")

temp_data1 <- temp_data %>% #pivot to match other dataframe
  pivot_longer(cols = minTemp:cvTemp,
               names_to = "Temperature_C",
               values_to = "Value")
temp_data1$Temperature_C <- sub('Temp', '', temp_data1$Temperature_C) #remove temp from name
temp_data1$CowTagID <- as.character(temp_data1$CowTagID)

combined_long <- temp_data1 %>% #pivot & combine with other predictors
  pivot_wider(names_from = Temperature_C,
              values_from = Value) %>%
  mutate(Predictor_measurement = "Temperature_C") %>%
  full_join(combined) %>%
  pivot_longer(cols = c(min, max, mean, range, cv),
               names_to = "Predictor_type",
               values_to = "Predictor_value") %>%
  unite("Predictor_measurement", c(Predictor_type, Predictor_measurement), sep = "_") %>%
  filter(Predictor_measurement != "median_Temperature_C") %>%
  rbind(N_data) %>%
  write_csv(here("Data",
                 "Site",
                 "bgc_stats_2022_10_25.csv"))

### Scale data ############################################################
combined_wide <- combined_long %>%
  pivot_wider(names_from = Predictor_measurement,
              values_from = Predictor_value) 
  # write_csv(here("Data",
  #                "Site",
  #                "bgc_stats_wide_2022_10_25.csv"))
  

combined_scale <- combined_wide %>%
  select(!CowTagID) %>%
  scale(center = TRUE, scale = TRUE)

combined_scale1 <- as.data.frame(combined_scale) #make data frame
combined_scale1$CowTagID <- combined_wide$CowTagID #add first column back

combined_scale1 <- combined_scale1 %>%
  pivot_longer(cols = min_Temperature_C:N_percent, 
               names_to = "Predictor_measurement",
               values_to = "Predictor_scaled") %>%
  write_csv(here("Data",
                 "Site",
                 "bgc_stats_scaled_2022_10_25.csv"))

### Clean PR data ############
PR <- PRw %>%
  select(!c(SurfaceAreaInitial, SurfaceAreaFinal, CowTagID)) %>% #select columns we don't need
  rename(GP_Initial = GrossPhotoInitial, #want to rename these columns so they are easier to work with
         NP_Initial = NetPhotoInitial,
         R_Initial = RespirationInitial,
         GP_Final = GrossPhotoFinal,
         NP_Final = NetPhotoFinal,
         R_Final = RespirationFinal,
         GP_PercentChange = pc_change_GP,
         NP_PercentChange = pc_change_NP,
         R_PercentChange = pc_change_R) %>%
  pivot_longer(cols = c(contains("Initial"), contains("Final"), contains("Percent")), #pivot all rates longer
               names_to = c("Response_measurement", "Response_type"),
               names_sep = "_",
               values_to = "Response_value") %>%
  pivot_wider(names_from = "Response_type", #want to pivot these back wider so easier to work with
              values_from = "Response_value") %>%
  rename(CowTagID = PlateID) %>% #rename columns so more information included on what it is
  mutate(Initial = Initial*1000, Final = Final*1000) %>% #convert back to umol cm-2 hr-1
  select(!PercentChange)


### Clean endosymbiont data ##################################################
treatment <- PR %>%
  select(FragmentID, Treatment) %>%
  unique()

response <- left_join(cc, ed) %>% #combine endosymbiont data & normalize CC to ED
  mutate(CCED_final = CC_final/ED_final,
         CCED_initial = CC_initial/ED_initial) %>%
  rename(CCcm2_initial = CC_cm2_initial, 
         CCcm2_final = CC_cm2_final) %>%
  select(!c(CC_initial, CC_final, Donor_Colony)) %>% #deselect rows we dont need
  pivot_longer(CCcm2_final:CCED_initial, 
               names_to = c("Response_measurement", "Response_type"),
               names_sep = "_",
               values_to = "Response_value") %>%
  pivot_wider(names_from = "Response_type",
              values_from = "Response_value") %>%
  rename(Initial = initial, Final = final) %>%
  left_join(treatment) %>%
  rbind(PR)

### Now want to join predictors with responses ###############################
response$CowTagID <- as.character(response$CowTagID)

#combine
final_data_long <- full_join(response, combined_long)
final_data_scaled <- full_join(response, combined_scale1)
final_data_wide <- full_join(response, combined_wide)


# final_data_long %>% write_csv(here("Short_term",
#                               "Data",
#                               "PR",
#                               "data_long_2022_10_25.csv"))
# 
# final_data_scaled %>% write_csv(here("Short_term",
#                               "Data",
#                               "PR",
#                               "data_scaled_2022_10_25.csv"))

final_data_wide %>% write_csv(here("Data",
                                   "Coral",
                                     "data_wide_2022_10_25.csv"))
