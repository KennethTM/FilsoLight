source("rawdata.R")

#Calculation of stats for manuscript

#test

#Nutrients stats used in manuscript
filso_chem %>%
  filter(chem_site != "indløb", year(date) %in% 2013:2017) %>%
  group_by(variable) %>%
  summarise(mean = mean(value_mean), sd=sd(value_mean),n =n())

#CDOM
cdom_stats <- filso_chem %>% 
  mutate(year = year(date),
         doy = yday(date),
         month = month(date),
         chem_site_label = case_when(chem_site == "indløb" ~ "Inlet",
                                     chem_site == "udløb" ~ "Outlet",
                                     chem_site == "søndersø" ~ "Dam",
                                     TRUE ~ "other"),
         chem_site_label = factor(chem_site_label, levels = c("Inlet", "Dam", "Outlet"))) %>%
  filter(variable == "abs_coef_440",
         year %in% 2013:2017,
         !(is.na(chem_site_label)))

#CDOM 2013
cdom_stats %>% 
  filter(year == 2013) %>% 
  group_by(chem_site_label) %>% 
  summarise(mean=mean(value_mean))

#CDOM season
cdom_stats %>% 
  mutate(season = case_when(month %in% c(6, 7, 8) ~ "summer",
                            month %in% c(12, 1, 2) ~ "winter",
                            month %in% c(3, 4, 5) ~ "spring",
                            month %in% c(9, 10, 11) ~ "autumn"),
         summer = ifelse(season == "summer", "summer", "not_summer")) %>% 
  group_by(summer) %>% 
  summarise(mean=mean(value_mean))

#Chlorophyll a
summary(chla)

#Wind 
summary(wnd_dmi)

#Average Kd (all values)
summary(light_kz)
kd_mean <- mean(light_kz$kz)

#Average Kd (for each station)
light_kz %>% 
  group_by(station) %>% 
  summarise(kz = mean(kz))

#Average 10 % light depth
light_kz %>% 
  mutate(z_ten_perc = 2.3/kz) %>% 
  summary()

#Percent light left at maximum colonization depth for each year
light_kz %>% 
  group_by(year(date)) %>% 
  summarise(kz = mean(kz)) %>% 
  add_column(max_depth = c(0.49, 0.78, 0.94, 1.19, 1.04)) %>% 
  mutate(perc_light_at_max = exp(-1*kz*max_depth))

#kz_part from figures.R script
kz_part %>% 
  gather(variable, value, -date) %>% 
  group_by(variable) %>% 
  summarise(mean = mean(value, na.rm = TRUE))

