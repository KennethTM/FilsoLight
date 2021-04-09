source("rawdata.R")

#Figure 1
boundary <- st_read(paste0(getwd(), "/data/boundary.sqlite")) %>% 
  st_transform(25832)

boundary_centroid <- boundary %>% 
  st_centroid()

st <- st_read(paste0(getwd(), "/data/st_filso.kmz")) %>% 
  st_transform(25832) %>% 
  filter(Name != "Weather")

eu_poly <- st_read(paste0(getwd(), "/data/eu_poly.kmz")) %>% 
  st_transform(25832)

eu <- ne_countries(continent = "Europe", scale = 50) %>% 
  st_as_sf() %>% 
  st_transform(25832) %>% 
  st_crop(st_bbox(eu_poly))

lake_map <- ggplot()+
  geom_sf(data = boundary, fill = NA, col = "black")+
  geom_sf_label(data = st, aes(label = Name), size = 2.5)+
  xlab("Longitude")+
  ylab("Latitude")

eu_map <- ggplot()+
  geom_sf(data = eu, fill = NA, col = "black")+
  geom_sf(data = boundary_centroid, size = 2, col = "red")

map_fig <- lake_map + inset_element(eu_map, -0.4, 0.6, 0.1, 1, align_to = "full")

ggsave(paste0(figures_path, "map_fig.png"), map_fig, width = 174, height = 130, units = "mm")

#Figure 2
kz_fig_data <- kz_dmi_wnd %>% 
  mutate(site_label = paste0("St. ", logger_site),
         year = year(date),
         doy = yday(date)) 

kz_fig <- kz_fig_data %>% 
  ggplot(aes(doy, kz_hobo, col = site_label))+
  geom_vline(xintercept = 80, linetype = 2)+
  geom_vline(xintercept = 295, linetype = 2)+
  geom_rect(inherit.aes=FALSE, aes(xmin = doy, xmax = doy+1, ymin=18, ymax=20, fill = wnd_mean))+
  geom_line()+
  scale_color_viridis_d(name = "Station")+
  scale_fill_gradient(low = grey(0.9), high = grey(0), name = expression("Wind speed (m s"^{-1}*")"))+
  facet_grid(year~.)+
  guides(fill=guide_colorbar(title.position = "top", barwidth = 10), color = guide_legend(title.position = "top"))+
  ylab(expression("Light attenuation (k"[z]*", m"^{-1}*")"))+
  xlab("Day of year")+
  theme(legend.position = "bottom")

ggsave(paste0(figures_path, "kz_fig.png"), kz_fig, width = 174, height = 234, units = "mm")

#Figure 3
kz_st_means <- kz_fig_data %>% 
  group_by(site_label) %>% 
  summarise(kz_mean = median(kz_hobo))

kz_site_fig <- kz_fig_data %>% 
  left_join(kz_st_means) %>% 
  ggplot(aes(kz_hobo, fill=site_label))+
  geom_density(alpha = 0.5, col = "white")+
  geom_vline(aes(xintercept=kz_mean, col = site_label), linetype=2, show.legend = FALSE)+
  scale_fill_viridis_d(name = "Station")+
  scale_color_viridis_d(name = "Station")+
  coord_cartesian(xlim=c(0, 10))+
  ylab("Density")+
  xlab(expression("Light attenuation (k"[z]*", m"^{-1}*")"))+
  theme(legend.position = c(0.8, 0.7))

ggsave(paste0(figures_path, "kz_site_fig.png"), kz_site_fig, width = 129, height = 100, units = "mm")

#Figure 4
cdom_fig <- filso_chem %>% 
  filter(chem_site == "søndersø", 
         variable == "cdom_400") %>%
  mutate(year = year(date),
         month = month(date),
         cdom_abs_coef = 2.303*value_mean/0.01) %>%
  group_by(year, month) %>% 
  summarise(cdom = mean(cdom_abs_coef)) %>% 
  filter(year %in% 2013:2017) %>% 
  ggplot(aes(month, cdom, col = factor(year)))+
  geom_line()+
  scale_x_continuous(breaks = 1:12)+
  scale_color_brewer(name = "Year", palette="Dark2")+
  xlab("Month")+
  ylab(expression("CDOM"[400]~"abs. coef. (m"^{-1}*")"))
  
chl_fig <- chl %>%
  mutate(year = year(date),
         month = month(date),
         doy = yday(date),
         week = week(date)) %>%
  group_by(year, month) %>% 
  summarise(chl = mean(chl)) %>% 
  filter(year %in% 2013:2017) %>% 
  ggplot(aes(month, chl, col = factor(year)))+
  geom_line()+
  scale_x_continuous(breaks = 1:12)+
  scale_color_brewer(name = "Year", palette="Dark2")+
  xlab("Month")+
  ylab(expression("Chlorophyll"~italic(a)~"("*mu*"g L"^{-1}*")"))

wnd_fig_data <- wnd_dmi %>% 
  select(date, wnd_mean, wnd_dir) %>% 
  mutate(year = year(date),
         month = month(date),
         doy = yday(date),
         week = week(date)) %>%
  group_by(year, month) %>% 
  summarise(wnd_mean = mean(wnd_mean),
            wnd_dir = mean(wnd_dir)) %>% 
  filter(year %in% 2013:2017)

wnd_speed_fig <- wnd_fig_data %>% 
  ggplot(aes(month, wnd_mean, col = factor(year)))+
  geom_line()+
  scale_x_continuous(breaks = 1:12)+
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

all_vars_fig <- cdom_fig+chl_fig+wnd_speed_fig+wnd_dir_fig+plot_layout(ncol=1, guides = "collect")+plot_annotation(tag_levels = "A")

ggsave(paste0(figures_path, "all_vars_fig.png"), all_vars_fig, width = 129, height = 234, units = "mm")


####

#Figure 5

model_df %>% 
  mutate(prediction = predict(gam_car_5$gam)) %>% 
  ggplot(aes(kz_hobo, prediction))+
  geom_point(alpha=0.2)+
  geom_abline(intercept = 0, slope=1, linetype = 2)+
  ylim(-1, 2.5)+
  xlim(-1, 2.5)+
  ylab("Predicted log(kz)")+
  xlab("Observed log(kz)")


viz <- getViz(gam_best$gam)
print(plot(viz), pages = 1)
