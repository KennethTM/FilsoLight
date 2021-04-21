source("libs_and_funcs.R")

light_files <- list.files(paste0(rawdata_path, "light"), full.names = TRUE, pattern = "st\\d")
light_land_files <- list.files(paste0(rawdata_path, "light"), full.names = TRUE, pattern = "vejrstation*")

#Land light data, establish lux to par conversion
land_par <- read.csv(light_land_files[1], skip = 1) %>% 
  tbl_df() %>% 
  select(Date.Time..GMT.02.00, PAR..uE..LGR.S.N..10305677_1115833_duplicate_duplicate_duplicate_duplicate_duplicate_duplicate_duplicate_duplicate_2404829..SEN.S.N..10283826.) %>% 
  set_names(c("datetime", "par")) %>% 
  mutate(datetime_utc = round_date(dmy_hms(datetime) -2*60*60, "10 mins"),
         par = par - 1.2) %>% 
  select(datetime_utc, par)

land_lux <- read.csv(light_land_files[2], skip = 1) %>% 
  tbl_df() %>% 
  select(Date.Time..GMT.01.00, Intensity..Lux..LGR.S.N..10303010_duplicate_duplicate_duplicate_duplicate..SEN.S.N..10303010.) %>% 
  set_names(c("datetime", "lux")) %>% 
  mutate(datetime_utc = round_date(dmy_hms(datetime) -1*60*60, "10 mins")) %>% 
  select(datetime_utc, lux)

land_light_day <- inner_join(land_par, land_lux) %>% 
  mutate(date = as_date(datetime_utc)) %>% 
  group_by(date) %>% 
  summarise(lux = sum(lux), par = sum(par), n = n()) %>% 
  filter(n == 144)

land_light_model <- lm(par ~ poly(lux, 2), data = land_light_day)
plot(resid(land_light_model))
summary(land_light_model)

land_light_day %>% 
  ggplot(aes(lux, par))+
  geom_point()+
  geom_smooth(method = "lm", formula = "y ~ poly(x, 2)")

#Read hobo light data
read_hobo_csv <- function(file){
  df <- read.csv(file, skip = 1)
  names(df) <- c("rowid", "datetime", "wtr", "lux")
  
  stat_depth <- str_split(str_sub(basename(file), end=-5), "_")[[1]]
  
  df_clean <- df %>% 
    tbl_df() %>% 
    mutate(datetime_utc = round_date(dmy_hms(datetime) -2*60*60, "10 mins"),
           station = parse_number(stat_depth[1]),
           hob = parse_number(stat_depth[2]),
           date = as_date(datetime_utc)) %>% 
    select(datetime_utc, date, station, hob, wtr, lux)
  
  return(df_clean)
}

light_list <- lapply(light_files, read_hobo_csv)

light_df <- bind_rows(light_list)

#Filter out invalid dates, e.g. close to zero light or incomplete sampling day
light_day <- light_df %>% 
  group_by(date, station, hob) %>% 
  summarise(lux_sum = sum(lux), n = n()) %>% 
  filter(n == 144, 
         lux_sum > 500) %>% 
  ungroup() %>% 
  mutate(par = predict(land_light_model, newdata = data.frame(lux = lux_sum))) %>% 
  select(-n, -lux_sum)

hobo_kz <- function(df){
  obs <- nrow(df)
  df$hob_m <- df$hob/100
  df$log_par <- log(df$par)
  
  if(obs <= 1){
    kz <- NA
  }else if(obs == 2){
    kz <- (df$log_par[2] - df$log_par[1]) / (df$hob_m[2] - df$hob_m[1])
  }else{
    kz_lm <- lm(df$log_par ~ df$hob_m)
    kz <- coef(kz_lm)[2]
  }
  return(kz)
  
}

light_kz <- light_day %>% 
  arrange(date, station, hob) %>% 
  nest(data = c(hob, par)) %>% 
  mutate(kz = map_dbl(data, hobo_kz)) %>% 
  filter(kz > 0) %>% 
  select(-data)

light_kz %>% 
  mutate(doy = yday(date),
         year = year(date)) %>% 
  ggplot(aes(doy, kz, col = factor(station)))+
  geom_line()+
  facet_grid(year~.)

saveRDS(light_kz, paste0(rawdata_path, "light/light_kz.rds"))
