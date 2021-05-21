source("libs_and_funcs.R")

#Filsø chlorophyll a
chla <- read_xlsx(paste0(rawdata_path, "filso_chla.xlsx"), skip = 1, col_names = FALSE) %>%
  set_names(c("datetime", "chla_ug_l")) %>%
  mutate(date = as_date(datetime)) %>%
  group_by(date) %>%
  summarise(chla_ug_l = mean(chla_ug_l, na.rm = TRUE)) %>% 
  mutate(kz_chla = 0.0149*chla_ug_l) #sand-jensen og krause-jensen LO

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
  # select(date, location, `400`:`700`, background = `750`) %>% 
  # gather(wavelengt, value, -date, -location, -background) %>% 
  # mutate(abs_coef = 2.303*(value-background)/0.01) %>% 
  # group_by(date, location) %>% 
  # summarise(mean_abs_coef = mean(abs_coef)) %>% 
  select(date, location, A_440 = `440`, A_750 = `750`) %>%
  mutate(abs_coef_440 = 2.303*(A_440 - A_750)/0.01) %>%
  mutate(kz_cdom_pfannkuche = abs_coef_440 * 0.221, #
         kz_cdom_balogh = 0.0172*(18.216*abs_coef_440-0.209)) %>%
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

# #Chem stats
# filso_chem %>% 
#   filter(chem_site != "indløb", year(date) %in% 2013:2017) %>% 
#   group_by(variable) %>%
#   summarise(mean = mean(value_mean), sd=sd(value_mean),n =n())

#Read light data
light_kz <- readRDS(paste0(rawdata_path, "light_kz.rds"))

#Read wind data
wnd_dmi <- readRDS(paste0(rawdata_path, "dmi_wind_data.rds")) %>% 
  select(date, wnd_mean, wnd_dir = dir_mean) %>% 
  mutate(wnd_mean_lag1 = lag(wnd_mean, 1), 
         wnd_mean_lag2 = lag(wnd_mean, 2),
         wnd_mean_lag3 = lag(wnd_mean, 3))

kz_wnd <- light_kz %>% 
  left_join(wnd_dmi) %>%
  na.omit()

#prepare model data
model_df <- kz_wnd %>% 
  mutate(year = year(date),
         year_fact = factor(year),
         kz_log = log(kz),
         doy = yday(date),
         month = month(date),
         station = factor(station),
         date_num = as.numeric(date)) %>%
  filter(between(doy, 80, 295)) %>% 
  filter(!(station == 4 & doy > 210 & year == 2016)) %>% #bad period
  select(station, date, date_num, year, year_fact, doy, kz_log, kz, contains("wnd_")) %>% 
  arrange(station, date) %>% 
  na.omit()
