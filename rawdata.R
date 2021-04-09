source("libs_and_funcs.R")

#Filsø chlorophyll a
chla <- read_xlsx(paste0(rawdata_path, "filso_chla.xlsx"), skip = 1, col_names = FALSE) %>%
  set_names(c("datetime", "chla_ug_l")) %>%
  mutate(date = as_date(datetime)) %>%
  group_by(date) %>%
  summarise(chla_ug_l = mean(chla_ug_l, na.rm = TRUE))

#Filsø inorganic and organic nutrients
data_years <- as.character(seq(2013, 2019, 1))
path_inorg_nutrients <- paste0(rawdata_path, "uorganisk_N-P_JSS.xlsx")
list_inorg_nutrients <- lapply(data_years, function(sheet){read_xlsx(path_inorg_nutrients, sheet = sheet)})
df_inorg_nutrients <- bind_rows(list_inorg_nutrients) %>%
  mutate(date = as_date(Date),
         location = str_to_lower(Location)) %>%
  select(date, location, NO3_ug_L, PO4_ug_L, NH4_ug_L) %>%
  gather(variable, value, -date, -location)

path_tot_nutrients <- paste0(rawdata_path, "TN_TP_JSS.xlsx")
list_tot_nutrients <- lapply(data_years, function(sheet){read_xlsx(path_tot_nutrients, sheet = sheet)})
df_tot_nutrients <- bind_rows(list_tot_nutrients) %>%
  mutate(date = as_date(Date),
         location = str_to_lower(Location)) %>%
  select(date, location, TN_ug_L = N_ug_L, TP_ug_L = P_ug_L) %>%
  gather(variable, value, -date, -location)

df_all_nutrients <- bind_rows(df_inorg_nutrients, df_tot_nutrients)

#Filsø cdom
path_cdom <- paste0(rawdata_path, "CDOM_JSS.xlsx")
list_cdom <- lapply(data_years, function(sheet){read_xlsx(path_cdom, sheet = sheet)})
df_cdom <- bind_rows(list_cdom) %>%
  mutate(date = as_date(Dato),
         location = str_to_lower(Lokalitet)) %>%
  select(date, location, cdom_400 = `400`) %>%
  gather(variable, value, -date, -location)

#Filsø combined chemistry (nutrients and cdom)
#Fix location names (performed manually in excel sheet)'
locations_translated <- read_xlsx(paste0(rawdata_path, "location_names_translated.xlsx")) %>%
  select(location = location...1, site = name)

filso_chem <- bind_rows(df_cdom, df_all_nutrients) %>%
  left_join(locations_translated) %>%
  na.omit() %>%
  mutate(value_pos = ifelse(value < 0, 0, value)) %>%
  group_by(site, date, variable) %>%
  summarise(value_mean = mean(value_pos)) %>% 
  ungroup() %>% 
  rename(chem_site = site)

# cdom <- read_csv(paste0(rawdata_path, "data_JSS/CDOM_JSS.csv")) %>%
#   select(date = dato, cdom = value.est) %>% 
#   group_by(date)

wnd <- read.csv(paste0(rawdata_path, "data_JSS/filso_vind_JSS.csv")) %>% 
  tbl_df() %>% 
  mutate(date = as_date(dag)) %>% 
  select(date, wnd_mean = vind_speed_middel, wnd_max = vind_speed_max, wnd_dir = vind_dir, 
         -interpolated) %>% 
  group_by(date) %>% 
  summarise_at(vars(wnd_mean, wnd_max, wnd_dir), list(mean), na.rm = TRUE) %>% 
  mutate(wnd_mean_lag1 = lag(wnd_mean, 1), 
         wnd_mean_lag2 = lag(wnd_mean, 2),
         wnd_mean_lag3 = lag(wnd_mean, 3),
         wnd_max_lag1 = lag(wnd_max, 1),
         wnd_max_lag2 = lag(wnd_max, 2),
         wnd_max_lag3 = lag(wnd_max, 3))

chl <- read_csv(paste0(rawdata_path, "data_JSS/klorofyl_data_JSS.csv")) %>% 
  select(date = dato, chl = mean.chl)

kz <- read_csv(paste0(rawdata_path, "data_JSS/kz_filso_JSS.csv")) %>%
  spread(logger_type, kz) %>% 
  select(date = dag, logger_site = logger_nr, kz_hobo = hobo, kz_ody = ody)

filso_kz <- kz %>% 
  left_join(wnd) %>% 
  left_join(chl)

wnd_dmi <- readRDS(paste0(rawdata_path, "dmi_wind_data.rds")) %>% 
  select(date, wnd_mean, wnd_dir = dir_mean) %>% 
  mutate(wnd_mean_lag1 = lag(wnd_mean, 1), 
         wnd_mean_lag2 = lag(wnd_mean, 2),
         wnd_mean_lag3 = lag(wnd_mean, 3))

kz_dmi_wnd <- kz %>% 
  select(date, logger_site, kz_hobo) %>% 
  left_join(wnd_dmi) %>%
  na.omit() %>%  
  filter(kz_hobo > 0)

#prepare model data
model_df <- kz_dmi_wnd %>% 
  mutate(year = year(date),
         year_fact = factor(year),
         kz_hobo = log(kz_hobo),
         doy = yday(date),
         month = month(date),
         site = factor(logger_site)) %>%
  filter(between(doy, 80, 295)) %>% 
  select(site, date, year, year_fact, doy, kz_hobo, contains("wnd_")) %>% 
  arrange(site, date) %>% 
  na.omit()
