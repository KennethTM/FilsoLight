source("rawdata.R")

library(mgcv)

filso_kz_filt <- filso_kz %>% 
  select(-kz_ody) %>% 
  filter(kz_hobo > 0) %>% 
  mutate(kz_hobo = log(kz_hobo),
         year = year(date),
         doy = yday(date),
         month = month(date),
         site = factor(logger_site))

#Model data
model_df <- filso_kz_filt %>% 
  select(kz_hobo, wnd_max, contains("max_lag"), site, year, doy, date, month, wnd_dir) %>% 
  filter(month %in% c(3:11)) %>% 
  arrange(site, date) %>% 
  na.omit()

summary(model_df)

#Fit initial models and compare correlation structures
form <- formula(kz_hobo ~ 
                  s(site, bs="re")+
                  s(wnd_max)+
                  s(wnd_max_lag1)+
                  s(wnd_max_lag2)+
                  s(wnd_max_lag3)+
                  s(wnd_dir, bs = "cc")+
                  s(doy, bs = "cc", by = ordered(year))+
                  year)

#Fit models using different correlations structures
gam_nocorr <- gamm(form, 
                   data = model_df, 
                   method = "REML")

gam_car <- gamm(form,
                correlation = corCAR1(form = ~ as.numeric(date)|site), 
                data = model_df, 
                method = "REML")

gam_gaus <- gamm(form, 
                 correlation = corGaus(form = ~ as.numeric(date)|site), 
                 data = model_df, 
                 method = "REML")

saveRDS(list("nocorr"=gam_nocorr, "gam_car"=gam_car, "gam_gaus"=gam_gaus), "gam_list.rds")

#Compare models
anova(gam_nocorr$lme, gam_car$lme, gam_gaus$lme)

#Inspect best model
summary(gam_car$gam)
acf(resid(gam_car$lme, type = "normalized"))
pacf(resid(gam_car$lme, type = "normalized"))
plot(Variogram(gam_car$lme, form = ~ as.numeric(date)|site, data = model_df))
gam.check(gam_car$gam)

#Refit model without non-significant smooths
gam_car_2 <- gamm(kz_hobo ~ 
                  s(site, bs="re")+
                  s(wnd_max)+
                  s(wnd_max_lag1)+
                  wnd_max_lag2+
                  wnd_max_lag3+
                  s(wnd_dir, bs = "cc")+
                  s(doy, bs = "cc", by = ordered(year))+
                  year,
                correlation = corCAR1(form = ~ as.numeric(date)|site), 
                data = model_df, 
                method = "REML")

#Stepwise drop non-significant terms
gam_car_3 <- gamm(kz_hobo ~ 
                    s(site, bs="re")+
                    s(wnd_max)+
                    s(wnd_max_lag1)+
                    wnd_max_lag2+
                    s(wnd_dir, bs = "cc")+
                    s(doy, bs = "cc", by = ordered(year))+
                    year,
                  correlation = corCAR1(form = ~ as.numeric(date)|site), 
                  data = model_df, 
                  method = "REML")

gam_car_4 <- gamm(kz_hobo ~ 
                    s(site, bs="re")+
                    s(wnd_max)+
                    s(wnd_max_lag1)+
                    wnd_max_lag2+
                    s(wnd_dir, bs = "cc")+
                    s(doy, bs = "cc", by = ordered(year)),
                  correlation = corCAR1(form = ~ as.numeric(date)|site), 
                  data = model_df, 
                  method = "REML")

gam_car_5 <- gamm(kz_hobo ~ 
                    s(site, bs="re")+
                    s(wnd_max)+
                    s(wnd_max_lag1)+
                    s(wnd_dir, bs = "cc")+
                    s(doy, bs = "cc", by = ordered(year)),
                  correlation = corCAR1(form = ~ as.numeric(date)|site), 
                  data = model_df, 
                  method = "REML")

#Inspect and save final model
saveRDS(gam_car_5, "gam_final.rds")

summary(gam_car_5$gam)
acf(resid(gam_car_5$lme, type = "normalized"))
pacf(resid(gam_car_5$lme, type = "normalized"))
plot(Variogram(gam_car_6$lme, form = ~ as.numeric(date)|site, data = model_df))
gam.check(gam_car_6$gam)

model_df %>% 
  mutate(prediction = predict(gam_car_5$gam)) %>% 
  ggplot(aes(kz_hobo, prediction))+
  geom_point(alpha=0.2)+
  geom_abline(intercept = 0, slope=1, linetype = 2)+
  ylim(-1, 2.5)+
  xlim(-1, 2.5)+
  ylab("Predicted log(kz)")+
  xlab("Observed log(kz)")

library(mgcViz)
viz <- getViz(gam_car_5$gam)
print(plot(viz), pages = 1)

