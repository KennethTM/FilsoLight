source("rawdata.R")

#summary of model data
summary(model_df)

#initial gam - check smoothing of predictor terms
gam_1 <- gam(kz_hobo ~
               s(site, bs = "re")+
               s(wnd_mean)+
               s(wnd_mean_lag1)+
               s(wnd_mean_lag2)+
               s(wnd_mean_lag3)+
               s(wnd_dir, bs = "cc")+
               ti(wnd_mean, wnd_dir, bs=c("tp", "cc"))+
               s(doy, by=year_fact)+
               year,
             data = model_df)
summary(gam_1)
plot(gam_1, pages=1, residuals=TRUE)

#make non signicant smooth terms parametric terms instead
gam_2 <- gam(kz_hobo ~
               s(site, bs = "re")+
               s(wnd_mean)+
               wnd_mean_lag1+
               wnd_mean_lag2+
               wnd_mean_lag3+
               s(wnd_dir, bs = "cc", k=15)+
               ti(wnd_mean, wnd_dir, bs=c("tp", "cc"))+
               s(doy, by=year_fact, k=15)+
               year,
             data = model_df)
summary(gam_2)
plot(gam_2, pages=1, residuals=TRUE)

#check k values by plotting residuals vs each smoothed predictor and change gam_2 model
rsd <- residuals(gam_2)
gam(rsd~s(wnd_mean, bs = "cs"), gamma=1.4, data=model_df)
gam(rsd~s(wnd_dir, bs = "cs", k = 15), gamma=1.4, data=model_df) 
gam(rsd~ti(wnd_mean, wnd_dir, bs="cs"), gamma=1.4, data=model_df) 
gam(rsd~s(doy, by=year_fact, k=15, bs="cs"), gamma=1.4, data=model_df) #increase k
gam.check(gam_2)

#Fit models using different correlations structures
form <- formula(kz_hobo ~
                  s(site, bs = "re")+
                  s(wnd_mean)+
                  wnd_mean_lag1+
                  wnd_mean_lag2+
                  wnd_mean_lag3+
                  s(wnd_dir, bs = "cc", k=15)+
                  ti(wnd_mean, wnd_dir, bs=c("tp", "cc"))+
                  s(doy, by=year_fact, k=15)+
                  year)

gam_nocorr <- gamm(form, 
                   data = model_df)

gam_car <- gamm(form,
                correlation = corCAR1(form = ~ as.numeric(date)|site), 
                data = model_df)

gam_gaus <- gamm(form, 
                 correlation = corGaus(form = ~ as.numeric(date)|site), 
                 data = model_df)

#Compare models
anova(gam_nocorr$lme, gam_car$lme, gam_gaus$lme)

#Save models
saveRDS(list("nocorr"=gam_nocorr, "gam_car"=gam_car, "gam_gaus"=gam_gaus), 
        paste0(modeling_path, "gam_list.rds"))

#Inspect model
summary(gam_car$gam)
acf(resid(gam_car$lme, type = "normalized"))
pacf(resid(gam_car$lme, type = "normalized"))
plot(Variogram(gam_car$lme, data = model_df))

#Compare candidate models by AIC
gam_car_wrap <- uGamm(form,
                      correlation = corCAR1(form = ~ as.numeric(date)|site),
                      data = model_df)

cl <- makeCluster(4, type = "SOCK")
clusterExport(cl, "model_df")

mod_sel <- pdredge(gam_car_wrap,
                   cluster = cl,
                   rank = "AIC",
                   fixed = c('s(site, bs = "re")',
                             's(wnd_mean)',
                             's(wnd_dir, bs = "cc", k = 15)',
                             's(doy, by = year_fact, k = 15)'))

saveRDS(mod_sel, paste0(modeling_path, "gam_model_selection.rds"))

stopCluster(cl)

#Fit best model, inspect and save
form_best <- formula(kz_hobo ~
                       s(site, bs = "re")+
                       s(wnd_mean)+
                       wnd_mean_lag1+
                       wnd_mean_lag2+
                       s(wnd_dir, bs = "cc", k=15)+
                       ti(wnd_mean, wnd_dir, bs=c("tp", "cc"))+
                       s(doy, by=year_fact, k=15)+
                       year)

gam_best <- gamm(form_best, 
                 correlation = corCAR1(form = ~ as.numeric(date)|site),
                 data = model_df)

summary(gam_best$gam)
acf(resid(gam_best$lme, type = "normalized"))
pacf(resid(gam_best$lme, type = "normalized"))
plot(Variogram(gam_best$lme, form = ~ as.numeric(date)|site, data = model_df))
par(mfrow=c(2,2))  
gam.check(gam_best$gam)  

saveRDS(gam_best, paste0(modeling_path, "gam_best.rds"))
