### Descriptive figure for environmental data ####


library(tidyverse)
library(here)
library(patchwork)
library(ggridges)
library(ggtext)


## read in data #####

data<-read_csv(here("Data","Site","site_biogeochemistry_summstats.csv"))%>%
  separate(Predictor_measurement, into = c("stat","measurement","unit"),sep = "_") %>%
  mutate(nicenames = case_when(measurement == "NN" ~ "b. Nitrate+Nitrite<br> (&mu;mol L<sup>-1</sup>)",
                               measurement == "Salinity" ~ "a. Salinity <br> (psu)",
                               measurement == "Phosphate" ~ "c. Phosphate <br> (&mu;mol L<sup>-1</sup>)",
                               measurement == "pH" ~ "d. pH<sub>T</sub> <br>",
                               measurement == "Temperature" ~ "e. Temperature <br> (\u00b0C)"
  ))%>%
  mutate(group = ifelse(stat == "range",2,1)) %>% # for easier plotting
  mutate(names = ifelse(group == 2, paste(nicenames, " "), nicenames),
         stat = str_to_title(stat))

data %>%
 # filter(stat != "range") %>%
  ggplot(aes(x = fct_rev(stat), y = Predictor_value))+
  geom_violin(aes(fill = stat))+
  labs(fill = "",
       y = "",
       x = "")+
  scale_fill_grey()+
  facet_wrap(~names, scales = "free", ncol = 2, strip.position = "left")+
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        strip.placement = "outside",
        strip.text.y.left  = element_markdown(angle = 90, size = 16),
        strip.background = element_blank()
        )

ggsave(here("Output","Site","envirofig.pdf"), width = 8, height = 12)
