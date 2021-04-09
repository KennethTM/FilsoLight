source("libs_and_funcs.R")

#dmi data
#metObs v1 API

#api key: 
#blåvandshuk fyr: stationId=06081
#params: parameterId=wind_dir, parameterId=wind_speed
#fra 2013-01-01: from=1356998400000000

key <- ""
stat <- "06081"
par_dir <- "wind_dir"
par_wnd <- "wind_speed"
time_from <- "1370048400000000" #Jun. 2013
time_to <- "1511139600000000" #Nov 2017
limit <- "300000"
base <- paste0("https://dmigw.govcloud.dk/metObs/v1/observation?limit=", limit)

get_dmi_data <- function(var){
  request <- paste0(base, "&parameterId=", var, "&stationId=", stat, "&from=", time_from, "&to=", time_to, "&api-key=", key)
  response <- GET(request)
  json <- content(response, as="text") 
  df <- fromJSON(json)
  df_clean <- df %>% 
    mutate(datetime = ymd_hms(as.POSIXct(timeObserved*10^-6, origin = "1970-01-01", tz = "GMT"))) %>% 
    select(datetime, variable = parameterId, value)
  
  return(df_clean)
}

var_list <- lapply(c(par_dir, par_wnd), get_dmi_data)

dmi_wnd_df <- bind_rows(var_list) %>% 
  spread(variable, value) %>% 
  mutate(date = as_date(datetime),
         wind_dir = ifelse(wind_dir == 0, NA, wind_dir)) %>% 
  group_by(date) %>% 
  summarise(wnd_mean = mean(wind_speed), wnd_max = max(wind_speed), wnd_min = min(wind_speed),
            dir_mean = mean(wind_dir, na.rm = TRUE), dir_max = max(wind_dir, na.rm = TRUE), dir_min = min(wind_dir, na.rm = TRUE))

saveRDS(dmi_wnd_df, paste0(rawdata_path, "dmi_wind_data.rds"))
