source("rawdata.R")

#Replace station names by numbering from S-N
st_names <- data.frame(station = c(1, 2, 3, 4),
                       name_new = c(1, 2, 4, 3))

#Figures for manuscript

#Figure 1
boundary <- st_read(paste0(getwd(), "/data/boundary.sqlite")) %>% 
  st_transform(25832) %>% 
  st_zm()

boundary_centroid <- boundary %>% 
  st_centroid()

st <- st_read(paste0(getwd(), "/data/st_filso_basins_2.kmz")) %>% 
  st_transform(25832) %>% 
  mutate(station = parse_number(str_sub(Name, start=-1))) %>% 
  left_join(st_names)

eu_poly <- st_read(paste0(getwd(), "/data/eu_poly.kmz")) %>% 
  st_transform(25832)

eu <- ne_countries(continent = "Europe", scale = 50) %>% 
  st_as_sf() %>% 
  st_transform(25832) %>% 
  filter(admin %in% c("Denmark", "Sweden", "Norway")) %>% 
  st_crop(st_bbox(eu_poly))

filso_contour <- st_read(paste0(getwd(), "/data/filso_bathy/samlet_Major Contours.shp")) %>% 
  st_transform(25832) %>% 
  st_intersection(boundary) %>% 
  st_cast("MULTILINESTRING")

filso_contour_col <- brewer.pal(9, "Blues")[3:9]

lake_map <- ggplot()+
  geom_sf(data = filso_contour, aes(col=factor(Dyb)), size = 0.5)+
  annotation_scale()+
  geom_sf(data = boundary, fill = NA, col = "black")+
  scale_color_manual(values = filso_contour_col)+
  geom_sf_text(data = filter(st, str_detect(Name, "St.")), aes(label = paste0("St. ", name_new)), size = 2.5, nudge_x = -350, nudge_y = 100)+
  geom_sf_text(data = filter(st, !str_detect(Name, "St.")), aes(label = Name), size = 2.5)+
  geom_sf(data = filter(st, str_detect(Name, "St.")), col = "black")+
  xlab(NULL)+
  guides(colour = guide_legend(title = "Depth (m)"))+
  scale_x_continuous(breaks = c(8.21, 8.235, 8.26))+
  ylab("Latitude")+
  xlab("Longitude")

eu_map <- ggplot()+
  geom_sf(data = eu, fill = NA, col = "black")+
  geom_sf(data = boundary_centroid, size = 2, col = "red")

#Export figures separately and combine in Inkscape
ggsave(paste0(figures_path, "map_1_fig.svg"), lake_map, width = 129, height = 129, units = "mm")
ggsave(paste0(figures_path, "map_2_fig.svg"), eu_map, width = 100, height = 100, units = "mm")

#Figure 2
kz_fig_data <- kz_wnd %>% 
  left_join(st_names) %>% 
  mutate(site_label = paste0("St. ", name_new),
         year = year(date),
         doy = yday(date),
         z_ten_perc = 2.3/kz)

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

ggsave(paste0(figures_path, "Figure2.png"), kz_fig, width = 174, height = 234, units = "mm")

#Figure 3
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
  scale_color_brewer(name = "Site", palette="Dark2")+
  xlab("Month")+
  facet_grid(year~.)+
  ylab(expression("CDOM (a"[440]*", m"^{-1}*")"))+
  xlab("Day of year")+
  theme(legend.position = "bottom")

ggsave(paste0(figures_path, "Figure3.png"), cdom_fig, width = 174, height = 234, units = "mm")

#Figure 4
chl_fig <- chla %>%
  mutate(year = year(date),
         month = month(date),
         doy = yday(date),
         week = week(date)) %>%
  filter(year %in% 2013:2017) %>% 
  ggplot(aes(doy, chla_ug_l, col = factor(year)))+
  geom_vline(xintercept = 80, linetype = 2)+
  geom_vline(xintercept = 295, linetype = 2)+
  geom_line()+
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
  scale_x_continuous(breaks = seq(0, 360, 45), limits = c(0, 360))

all_vars_fig <- chl_fig+wnd_speed_fig+wnd_dir_fig+plot_layout(ncol=1, guides = "collect")+plot_annotation(tag_levels = "A")

ggsave(paste0(figures_path, "Figure4.png"), all_vars_fig, width = 129, height = 234, units = "mm")

#Figure 6
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

ggsave(paste0(figures_path, "Figure6.png"), all_model_fig, width = 174, height = 234, units = "mm")

#Figure 5
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

ggsave(paste0(figures_path, "Figure5.png"), obs_pred_fig, width = 84, height = 84, units = "mm")

#Figure S3
kz_southern <- light_kz %>% 
  filter(station %in% c(1, 2)) %>% 
  group_by(date) %>% 
  summarise(kz = mean(kz, na.rm = TRUE))

cdom_southern <- filso_chem %>% 
  filter(variable == "kz_cdom_balogh", 
         chem_site == "søndersø") %>% 
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
  filter(year == 2014)  %>% 
  filter(between(doy, 80, 295)) %>% 
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
  filter(year == 2014)  %>% 
  filter(between(doy, 80, 295)) %>% 
  ggplot(aes(doy, value, fill=kz_comp))+
  geom_area()+
  scale_fill_brewer(name = "Component", palette="Dark2")+
  ylab(expression("Contribution to "*K[d]~"(%)"))+
  scale_y_continuous(expand = c(0, 0))+
  scale_x_continuous(expand = c(0, 0))+
  xlab("Day of year")

fig_partitioning <- kd_comps+kd_comps_perc+plot_layout(ncol=1, guides = "collect")+plot_annotation(tag_levels = "A")

ggsave(paste0(figures_path, "FigureS3.png"), fig_partitioning, width = 174, height = 129, units = "mm")

#Figure 7
#Calculate bootstrapped percentile confidence intervals of z10%
#https://gist.github.com/roualdes/1de1c9a4a26581ba18a7ae9b96019970
boot_fn <- function(d, i) {
  mean(d[i])
}

boot_perc <- function(b, probs=c(0.025, 0.5, 0.975),
                       nms=c("lower_bound", "median", "upper_bound")) {
  b$t %>%
    quantile(probs=probs) %>%
    as.list() %>%
    setNames(nm=nms) %>%
    data.frame()
}

depth_data <- kz_fig_data %>%
  group_by(year) %>%
  summarise(bsamples = list(boot(z_ten_perc, boot_fn, R=1000))) %>%
  mutate(bs = lapply(bsamples, boot_perc)) %>%
  select(-bsamples) %>%
  unnest(bs) %>% 
  add_column(max_depth = c(0.49, 0.78, 0.94, 1.19, 1.04))

depth_plot <- depth_data %>% 
  ggplot()+
  geom_linerange(aes(year, median, ymin = lower_bound, ymax=upper_bound))+
  geom_line(aes(year, median), linetype = 1)+
  geom_point(aes(year, median, shape = "z[10%] (m)"), size = 2.5)+
  geom_line(aes(year, max_depth), linetype = 2)+
  geom_point(aes(year, max_depth, shape = "Max. colonization depth (m)"), size = 2.5, fill="white")+
  ylab("Depth (m)")+
  xlab("Year")+
  scale_shape_manual(values = c(21, 19), labels = c("Max. colonization depth (m)", expression(z["10%"]~"(m)")))+
  theme(legend.position = c(0.25, 0.87), legend.title = element_blank(), legend.text.align = 0)

ggsave(paste0(figures_path, "Figure7.png"), depth_plot, width = 129, height = 84, units = "mm")

#Figure S2 - chla vs wtr
wtr_mean <- readRDS(paste0(rawdata_path, "wtr_station_1.rds"))

figure_s2 <- chla %>% 
  left_join(wtr_mean) %>% 
  na.omit() %>% 
  mutate(Year = factor(year(date))) %>% 
  ggplot(aes(wtr_mean, chla_ug_l, col = Year))+
  geom_point(shape=1)+
  ylab(expression("Chlorophyll"~italic(a)~"("*mu*g~L^{-1}*")"))+
  xlab("Water temperature (°C)")+
  scale_color_brewer(name = "Year", palette="Dark2")+
  theme(legend.position = "bottom")

ggsave(paste0(figures_path, "FigureS2.png"), figure_s3, width = 129, height = 129, units = "mm")

