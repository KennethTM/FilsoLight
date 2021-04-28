source("rawdata.R")

#Figure 1
boundary <- st_read(paste0(getwd(), "/data/boundary.sqlite")) %>% 
  st_transform(25832) %>% 
  st_zm()

boundary_centroid <- boundary %>% 
  st_centroid()

st <- st_read(paste0(getwd(), "/data/st_filso_basins_2.kmz")) %>% 
  st_transform(25832)

eu_poly <- st_read(paste0(getwd(), "/data/eu_poly.kmz")) %>% 
  st_transform(25832)

eu <- ne_countries(continent = "Europe", scale = 50) %>% 
  st_as_sf() %>% 
  st_transform(25832) %>% 
  st_crop(st_bbox(eu_poly))

lake_map <- ggplot()+
  geom_sf(data = boundary, fill = NA, col = "gray")+
  geom_sf_text(data = filter(st, str_detect(Name, "St.")), aes(label = Name), size = 2.5, nudge_x = -350, nudge_y = 100)+
  geom_sf_text(data = filter(st, !str_detect(Name, "St.")), aes(label = Name), size = 2.5)+
  geom_sf(data = filter(st, str_detect(Name, "St.")), col = "black")+
  scale_color_manual(values = c("Light" = "deepskyblue", "Chemistry" = "coral", "basin" = "white"), name = "")+
  xlab(NULL)+
  scale_x_continuous(breaks = c(8.21, 8.235, 8.26))+
  ylab("Latitude")

eu_map <- ggplot()+
  geom_sf(data = eu, fill = NA, col = "black")+
  geom_sf(data = boundary_centroid, size = 2, col = "red")

#Station fetch stats
wnd_dir_freq <- wnd_dmi %>% 
  mutate(wnd_dir_cut = cut(wnd_dir, breaks=seq(0, 360, 10))) %>% 
  group_by(wnd_dir_cut) %>% 
  summarise(freq=n()/nrow(.)) %>% 
  add_row(wnd_dir_cut="(350,360]", freq=0) %>% 
  add_row(wnd_dir_cut="(0,10]", freq=0) %>% 
  add_row(wnd_dir_cut="(10,20]", freq=0) %>% 
  arrange(wnd_dir_cut) %>% 
  pull(freq)

rast_tmp <- raster(boundary, res = 5)
st_stations <- st %>% filter(Name %in% paste0("St. ", 1:4))
rast <- rasterize(as(boundary, "Spatial"), rast_tmp, field = 1)
rast_islake <- (rast ==  1)
rast_fetch <- fetch(rast_islake, angle = seq(5, 355, 10))
rast_fetch_weight <- rast_fetch*wnd_dir_freq

rast_fetch_weight_mean <- calc(rast_fetch_weight, fun=sum)
rast_fetch_mean <- calc(rast_fetch, fun=mean)
rast_fetch_min <- calc(rast_fetch, fun=min)
rast_fetch_max <- calc(rast_fetch, fun=max)
fetch_stack <- stack(rast_fetch_mean, rast_fetch_weight_mean, rast_fetch_min, rast_fetch_max)

st_fetch_stats <- extract(fetch_stack, as(st_stations, "Spatial")) %>% 
  as.data.frame() %>% 
  set_names(c("mean", "weighted_mean", "min", "max")) %>% 
  cbind(st_stations["Name"], .)

#plot mean and weighted mean fetch and combine in figure 1
rast_fetch_mean_latlon <- projectRaster(rast_fetch_mean, crs=4326)
rast_fetch_weight_mean_latlon <- projectRaster(rast_fetch_weight_mean, crs=4326)

fetch_mean_df <- as.data.frame(rast_fetch_mean_latlon, xy=TRUE, na.rm = TRUE)
fetch_mean_weight_df <- as.data.frame(rast_fetch_weight_mean_latlon, xy=TRUE, na.rm = TRUE)

mean_fetch_plot <- fetch_mean_df %>% 
  ggplot(aes(x, y, fill=layer))+
  geom_raster()+
  scale_fill_viridis_c(option = "D", name = "Fetch (m)", limits = c(0, 1340), breaks = c(0, 400, 800, 1200))+
  xlab(NULL)+
  ylab("Latitude")+
  scale_x_continuous(breaks = c(8.21, 8.235, 8.26))+
  geom_sf(data = st_transform(boundary, 4326), inherit.aes = FALSE, fill = NA, col = "black")

weighted_mean_fetch_plot <- fetch_mean_weight_df %>% 
  ggplot(aes(x, y, fill=layer))+
  geom_raster()+
  scale_fill_viridis_c(option = "D", name = "Fetch (m)", limits = c(0, 1340), breaks = c(0, 400, 800, 1200))+
  xlab("Longitude")+
  ylab("Latitude")+
  scale_x_continuous(breaks = c(8.21, 8.235, 8.26))+
  geom_sf(data = st_transform(boundary, 4326), inherit.aes = FALSE, fill = NA, col = "black")

map_fig <- lake_map/mean_fetch_plot/weighted_mean_fetch_plot + plot_layout(guides="collect") + plot_annotation(tag_levels = "A") #:inset_element(, -0.4, 0.6, 0.1, 1, align_to = "full")

ggsave(paste0(figures_path, "map_fig.png"), map_fig, width = 100, height = 234, units = "mm")

#Figure 2
kz_fig_data <- kz_wnd %>% 
  mutate(site_label = paste0("St. ", station),
         year = year(date),
         doy = yday(date)) %>% 
  filter(!(station == 4 & doy > 210 & year == 2016)) #bad period

kz_fig <- kz_fig_data %>% 
  ggplot(aes(doy, kz, col = site_label))+
  geom_vline(xintercept = 80, linetype = 2)+
  geom_vline(xintercept = 295, linetype = 2)+
  geom_rect(inherit.aes=FALSE, aes(xmin = doy, xmax = doy+1, ymin=9, ymax=10.5, fill = wnd_mean))+
  geom_line()+
  scale_color_viridis_d(name = "Station")+
  scale_fill_gradient(low = grey(0.9), high = grey(0), name = expression("Wind speed (m s"^{-1}*")"))+
  facet_grid(year~.)+
  guides(fill=guide_colorbar(title.position = "top", barwidth = 10), color = guide_legend(title.position = "top"))+
  ylab(expression(K[d]~"(m"^{-1}*")"))+
  xlab("Day of year")+
  theme(legend.position = "bottom")

ggsave(paste0(figures_path, "kz_fig.png"), kz_fig, width = 174, height = 234, units = "mm")

#Figure 3
kz_st_med <- kz_fig_data %>%
  group_by(site_label) %>%
  summarise(kz_med = median(kz))

kz_site_fig <- kz_fig_data %>%
  left_join(kz_st_med) %>%
  ggplot(aes(kz, fill=site_label))+
  geom_density(alpha = 0.5, col = "white")+
  geom_vline(aes(xintercept=kz_med, col = site_label), linetype=2, show.legend = FALSE)+
  scale_fill_viridis_d(name = "Station")+
  scale_color_viridis_d(name = "Station")+
  coord_cartesian(xlim=c(0, 8))+
  ylab("Density")+
  xlab(expression(K[d]~"(m"^{-1}*")"))+
  theme(legend.position = c(0.8, 0.7))

ggsave(paste0(figures_path, "kz_site_fig.png"), kz_site_fig, width = 129, height = 100, units = "mm")

#Figure 4
cdom_fig <- filso_chem %>% 
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
         !(is.na(chem_site_label))) %>%
  ggplot(aes(doy, value_mean, col = chem_site_label))+
  geom_vline(xintercept = 80, linetype = 2)+
  geom_vline(xintercept = 295, linetype = 2)+
  geom_line()+
  geom_point()+
  scale_color_brewer(name = "Year", palette="Dark2")+
  xlab("Month")+
  facet_grid(year~.)+
  ylab(expression("CDOM (a"[440]*", m"^{-1}*")"))+
  xlab("Day of year")+
  theme(legend.position = "bottom")

ggsave(paste0(figures_path, "cdom_fig.png"), cdom_fig, width = 174, height = 234, units = "mm")

#Figure 5
chl_fig <- chla %>%
  mutate(year = year(date),
         month = month(date),
         doy = yday(date),
         week = week(date)) %>%
  #group_by(year, month) %>% 
  #summarise(chl = mean(chla_mg_l)) %>% 
  filter(year %in% 2013:2017) %>% 
  ggplot(aes(doy, chla_ug_l, col = factor(year)))+
  geom_vline(xintercept = 80, linetype = 2)+
  geom_vline(xintercept = 295, linetype = 2)+
  geom_line()+
  #scale_x_continuous(breaks = 1:12)+
  scale_color_brewer(name = "Year", palette="Dark2")+
  xlab("Day of the year")+
  ylab(expression("Chlorophyll"~italic(a)~"("*mu*g~L^{-1}*")"))

wnd_speed_fig <- wnd_dmi %>% 
  select(date, wnd_mean, wnd_dir) %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  ggplot(aes(factor(month), wnd_mean))+
  geom_boxplot()+
  scale_x_discrete(breaks = 1:12)+
  scale_color_brewer(name = "Year", palette="Dark2")+
  xlab("Month")+
  ylab(expression("Wind speed (m s"^{-1}*")"))

wnd_dir_fig <- wnd_dmi %>% 
  select(date, wnd_mean, wnd_dir) %>% 
  mutate(year = year(date)) %>% 
  ggplot(aes(wnd_dir, fill = factor(year)))+
  geom_density(alpha = 0.5, col = "white")+
  scale_fill_brewer(name = "Year", palette="Dark2")+
  xlab("Wind direction (degrees)")+
  ylab("Density")+
  scale_x_continuous(breaks = seq(0, 315, 45))

all_vars_fig <- chl_fig+wnd_speed_fig+wnd_dir_fig+plot_layout(ncol=1, guides = "collect")+plot_annotation(tag_levels = "A")

ggsave(paste0(figures_path, "all_vars_fig.png"), all_vars_fig, width = 129, height = 234, units = "mm")

#Figure 5
gam_best <- readRDS(paste0(modeling_path, "gam_best.rds"))

viz <- getViz(gam_best$gam)

fig_ylims <- c(-1.1, 2.5)

add_model_int <- function(x){round(x + as.numeric(coef(gam_best$gam)[1]), 1)} 

gam_wnd_fig <- plot(sm(viz, 1))+
  l_ciPoly()+
  l_fitLine()+
  l_rug()+
  coord_cartesian(ylim = fig_ylims)+
  scale_y_continuous(labels = add_model_int)+
  xlab(expression("Wind speed (t"[0]*", m s"^{-1}*")"))+
  ylab(expression(K[d]~"(m"^{-1}*")"))+
  theme_pub

gam_wnd_lag1_fig <- plot(pterm(viz, 1))+
  l_ciPoly()+
  l_fitLine()+
  l_rug()+
  coord_cartesian(ylim = fig_ylims)+
  scale_y_continuous(labels = add_model_int)+
  xlab(expression("Wind speed (t"[-1]*", m s"^{-1}*")"))+
  ylab(expression(K[d]~"(m"^{-1}*")"))+
  theme_pub

gam_dir_fig <- plot(sm(viz, 2))+
  l_ciPoly()+
  l_fitLine()+
  l_rug()+
  coord_cartesian(ylim = fig_ylims)+
  scale_y_continuous(labels = add_model_int)+
  xlab("Wind direction (degrees)")+
  ylab(expression(K[d]~"(m"^{-1}*")"))+
  theme_pub

#plotRGL(sm(viz, 3))
gam_inter_fig <- plot(sm(viz, 3))+
  l_fitRaster()+
  scale_fill_gradient2(low = "red", high = "blue", mid = "grey", na.value = "white")+
  ggtitle(NULL)+
  guides(fill = guide_colorbar(title = expression(K[d]~"(m"^{-1}*")")))+ 
  xlab(expression("Wind speed (t"[0]*", m s"^{-1}*")"))+
  ylab("Wind direction (degrees)")+
  theme_pub

# year_smooth_df <- expand.grid(year = 2013:2017, doy=80:295, site = 1) %>% 
#   mutate(wnd_mean = mean(model_df$wnd_mean),
#          wnd_mean_lag1 = mean(model_df$wnd_mean_lag1),
#          wnd_mean_lag2 = mean(model_df$wnd_mean_lag2),
#          wnd_dir = mean(model_df$wnd_dir),
#          wnd_mean = mean(model_df$wnd_mean),
#          year_fact = factor(year))
# 
# year_smooth_preds <- predict(gam_best$gam, newdata = year_smooth_df)
# 
# gam_year_fig <- cbind(year_smooth_df, year_smooth_preds) %>%
#   ggplot(aes(doy, year_smooth_preds, col = year_fact))+
#   geom_line()+
#   coord_cartesian(ylim=c(-2, 6))+
#   scale_color_brewer(name = "Year", palette="Dark2")+
#   ylab(expression("k"[z]))+
#   xlab("Day of year")

gam_doy_fig <- plot(sm(viz, 4))+
  l_ciPoly()+
  l_fitLine()+
  l_rug()+
  coord_cartesian(ylim = fig_ylims)+
  scale_y_continuous(labels = add_model_int)+
  xlab(expression("Day of year"))+
  ylab(expression(K[d]~"(m"^{-1}*")"))+
  theme_pub

gam_year_fig <- plot(sm(viz, 5))+
  l_ciPoly()+
  l_fitLine()+
  coord_cartesian(ylim = fig_ylims)+
  scale_y_continuous(labels = add_model_int)+
  xlab(expression("Year"))+
  ylab(expression(K[d]~"(m"^{-1}*")"))+
  theme_pub

all_model_fig <- gam_wnd_fig$ggObj + gam_wnd_lag1_fig$ggObj + gam_dir_fig$ggObj +gam_inter_fig$ggObj + 
  gam_doy_fig$ggObj + gam_year_fig$ggObj + 
  plot_layout(guides = "collect", ncol=2)+plot_annotation(tag_levels = "A") &
  theme(legend.position='bottom') &
  guides(fill=guide_colorbar(title.position = "top", barwidth = 10, title = expression(K[d]~"(m"^{-1}*")")), color = guide_legend(title.position = "top"))

ggsave(paste0(figures_path, "all_model_fig.png"), all_model_fig, width = 174, height = 234, units = "mm")

#obs-pred figure
obs_pred_data <- model_df %>% 
  mutate(prediction = predict(gam_best$gam),
         rmse = sqrt(mean((kz-prediction)^2)),
         mae = mean(abs(kz-prediction))) 

obs_pred_fig <- obs_pred_data %>% 
  ggplot(aes(kz, prediction))+
  geom_point(alpha=0.1)+
  geom_abline(intercept = 0, slope=1, linetype = 2)+
  ylim(0, 8.5)+
  xlim(0, 8.5)+
  ylab(expression(Predicted~K[d]~"(m"^{-1}*")"))+
  xlab(expression(Observed~K[d]~"(m"^{-1}*")"))+
  annotate("text", x = -Inf, y = Inf, label = paste0("RMSE == ", round(obs_pred_data$rmse[1], 2), '~m^{-1}'), 
           parse = TRUE, hjust = -0.1, vjust = 1.5)+
  annotate("text", x = -Inf, y = Inf, label = paste0("MAE == ", round(obs_pred_data$mae[1], 2), '~m^{-1}'), 
           parse = TRUE, hjust = -0.1, vjust = 3)

ggsave(paste0(figures_path, "obs_pred_fig.png"), obs_pred_fig, width = 84, height = 84, units = "mm")

#Partitioning figure
kz_southern <- light_kz %>% 
  filter(station %in% c(1, 2)) %>% 
  group_by(date) %>% 
  summarise(kz = mean(kz, na.rm = TRUE))

cdom_southern <- filso_chem %>% 
  filter(variable == "kz_cdom", 
         chem_site %in% c("indløb", "søndersø")) %>% 
  group_by(date) %>% 
  summarise(kz_cdom = mean(value_mean, na.rm=TRUE)) %>% 
  right_join(data.frame(date = seq(ymd("2013-01-01"), ymd("2017-12-31"), by = "day"))) %>% 
  arrange(date) %>% 
  mutate(kz_cdom = na.approx(kz_cdom, na.rm = FALSE)) %>% 
  na.omit()

chl_southern <- chla %>% 
  select(date, kz_chla) %>% 
  right_join(data.frame(date = seq(ymd("2013-01-01"), ymd("2017-12-31"), by = "day"))) %>% 
  arrange(date) %>% 
  mutate(kz_chla = na.approx(kz_chla, na.rm = FALSE)) %>% 
  na.omit()

kz_water <- 0.027

kz_part <- kz_southern %>% 
  left_join(cdom_southern) %>% 
  left_join(chl_southern) %>% 
  add_column(kz_water = kz_water) %>% 
  mutate(kz_particle = kz - kz_cdom - kz_chla - kz_water,
         kz_cdom_perc = kz_cdom/kz*100,
         kz_chla_perc = kz_chla/kz*100,
         kz_particle_perc = kz_particle/kz*100,
         kz_water_perc = kz_water/kz*100)

kd_comps <- kz_part %>% 
  select(date, kz_cdom, kz_chla, kz_particle, kz_water) %>% 
  na.omit() %>% 
  gather(variable, value, -date) %>% 
  mutate(year = year(date),
         month = month(date),
         doy = yday(date),
         kz_comp = case_when(variable == "kz_chla" ~ "Chl. a",
                             variable == "kz_cdom" ~ "CDOM",
                             variable == "kz_particle" ~ "Particles",
                             variable == "kz_water" ~ "Water"),
         kz_comp = factor(kz_comp, levels = c("Chl. a", "CDOM", "Particles", "Water"))) %>% 
  filter(year == 2014 & doy >= 80)  %>% 
  ggplot(aes(doy, value, fill=kz_comp))+
  geom_area()+
  scale_fill_brewer(name = "Component", palette="Dark2")+
  ylab(expression(K[d]~"(m"^{-1}*")"))+
  scale_y_continuous(expand = c(0, 0))+
  scale_x_continuous(expand = c(0, 0))+
  xlab("Day of year")

kd_comps_perc <- kz_part %>% 
  select(date, contains("_perc")) %>% 
  na.omit() %>% 
  gather(variable, value, -date) %>% 
  mutate(year = year(date),
         month = month(date),
         doy = yday(date),
         kz_comp = case_when(variable == "kz_chla_perc" ~ "Chl. a",
                             variable == "kz_cdom_perc" ~ "CDOM",
                             variable == "kz_particle_perc" ~ "Particles",
                             variable == "kz_water_perc" ~ "Water"),
         kz_comp = factor(kz_comp, levels = c("Chl. a", "CDOM", "Particles", "Water"))) %>% 
  filter(year == 2014 & doy >= 80) %>% 
  ggplot(aes(doy, value, fill=kz_comp))+
  geom_area()+
  scale_fill_brewer(name = "Component", palette="Dark2")+
  ylab(expression("Contribution to "*K[d]~"(%)"))+
  scale_y_continuous(expand = c(0, 0))+
  scale_x_continuous(expand = c(0, 0))+
  xlab("Day of year")

fig_partitioning <- kd_comps+kd_comps_perc+plot_layout(ncol=1, guides = "collect")+plot_annotation(tag_levels = "A")

ggsave(paste0(figures_path, "fig_partitioning.png"), fig_partitioning, width = 174, height = 129, units = "mm")
