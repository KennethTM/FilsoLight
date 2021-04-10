source("rawdata.R")

#Figure 1
boundary <- st_read(paste0(getwd(), "/data/boundary.sqlite")) %>% 
  st_transform(25832) %>% 
  st_zm()

boundary_centroid <- boundary %>% 
  st_centroid()

st <- st_read(paste0(getwd(), "/data/st_filso.kmz")) %>% 
  st_transform(25832) %>% 
  filter(Name != "Weather")

#Station fetch stats
# library(windfetchR);library(raster)
# rast_tmp <- raster(boundary, res = 5)
# rast <- rasterize(as(boundary, "Spatial"), rast_tmp, field = 1)
# rast_islake <- (rast ==  1)
# rast_fetch_many <- fetch(rast_islake, angle = seq(0, 360-22.5,22.5 ))
# fetch_mean <- calc(rast_fetch_many, fun=mean)
# stat_fetch <- st %>% 
#   mutate(fetch=extract(fetch_mean, as(st, "Spatial")))
# 
# gam_best$gam %>% coef() -> gam_re
# gam_re[grep("site", names(gam_re))]
# 
# plot(c(852.1875, 1056.2500, 923.1250, 851.2500), c(0.213230858,  0.003486896, -0.136628694, -0.080089061 ))

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

#Figure 5
gam_best <- readRDS(paste0(modeling_path, "gam_best.rds"))

viz <- getViz(gam_best$gam)

fig_ylims <- c(-0.1, 0.6)

gam_wnd_fig <- plot(sm(viz, 2))+
  l_ciPoly()+
  l_fitLine()+
  l_rug()+
  coord_cartesian(ylim = fig_ylims)+
  xlab(expression("Wind speed, t"[0]~"(m s"^{-1}*")"))+
  ylab(expression("k"[z]*" (m"^{-1}*")"))+
  theme_pub

gam_wnd_lag1_fig <- plot(pterm(viz, 1))+
  l_ciPoly()+
  l_fitLine()+
  l_rug()+
  coord_cartesian(ylim = fig_ylims)+
  xlab(expression("Wind speed, t"[-1]~"(m s"^{-1}*")"))+
  ylab(expression("k"[z]*" (m"^{-1}*")"))+
  theme_pub

gam_wnd_lag2_fig <- plot(pterm(viz, 2))+
  l_ciPoly()+
  l_fitLine()+
  l_rug()+
  coord_cartesian(ylim = fig_ylims)+
  xlab(expression("Wind speed, t"[-2]~"(m s"^{-1}*")"))+
  ylab(expression("k"[z]*" (m"^{-1}*")"))+
  theme_pub

gam_dir_fig <- plot(sm(viz, 3))+
  l_ciPoly()+
  l_fitLine()+
  l_rug()+
  coord_cartesian(ylim = fig_ylims)+
  xlab("Wind direction (degrees)")+
  ylab(expression("k"[z]*" (m"^{-1}*")"))+
  theme_pub

gam_inter_fig <- plot(sm(viz, 4))+
  l_fitRaster()+
  scale_fill_gradient2(low = "red", high = "blue", mid = "grey", na.value = "white")+
  ggtitle(NULL)+
  guides(fill = guide_colorbar(title = expression("k"[z]*" (m"^{-1}*")")))+ 
  #scale_y_continuous(limits = c(-1.3, 1.3))+
  xlab(expression("Wind speed, t"[0]~"(m s"^{-1}*")"))+
  ylab("Wind direction (degrees)")+
  theme_pub

year_smooth_df <- expand.grid(year = 2013:2017, doy=80:295, site = 1) %>% 
  mutate(wnd_mean = mean(model_df$wnd_mean),
         wnd_mean_lag1 = mean(model_df$wnd_mean_lag1),
         wnd_mean_lag2 = mean(model_df$wnd_mean_lag2),
         wnd_dir = mean(model_df$wnd_dir),
         wnd_mean = mean(model_df$wnd_mean),
         year_fact = factor(year))

year_smooth_preds <- predict(gam_best$gam, newdata = year_smooth_df)

gam_year_fig <- cbind(year_smooth_df, year_smooth_preds) %>%
  ggplot(aes(doy, year_smooth_preds, col = year_fact))+
  geom_line()+
  coord_cartesian(ylim=c(0, 2))+
  scale_color_brewer(name = "Year", palette="Dark2")+
  ylab(expression("k"[z]*" (m"^{-1}*")"))+
  xlab("Day of year")

all_model_fig <- gam_wnd_fig$ggObj + gam_wnd_lag1_fig$ggObj + gam_wnd_lag2_fig$ggObj + gam_dir_fig$ggObj +
  gam_inter_fig$ggObj + gam_year_fig +
  plot_layout(guides = "collect", ncol=2)+plot_annotation(tag_levels = "A") &
  theme(legend.position='bottom') &
  guides(fill=guide_colorbar(title.position = "top", barwidth = 10, title = expression("k"[z]*" (m"^{-1}*")")), color = guide_legend(title.position = "top"))

ggsave(paste0(figures_path, "all_model_fig.png"), all_model_fig, width = 174, height = 234, units = "mm")

#obs-pred figure
obs_pred_fig <- model_df %>% 
  mutate(prediction = predict(gam_best$gam)) %>% 
  ggplot(aes(kz_hobo, prediction))+
  geom_point(alpha=0.1)+
  geom_abline(intercept = 0, slope=1, linetype = 2)+
  ylim(-1, 2.5)+
  xlim(-1, 2.5)+
  ylab(expression("Predicted log(k"[z]*")"))+
  xlab(expression("Observed log(k"[z]*")"))

ggsave(paste0(figures_path, "obs_pred_fig.png"), obs_pred_fig, width = 84, height = 84, units = "mm")
