source("rawdata.R")

#Summary of model data
summary(model_df)

#Initial gam to check smoothing of predictor terms
gam_1 <- gam(kz ~
               s(wnd_mean)+
               s(wnd_mean_lag1)+
               s(wnd_mean_lag2)+
               s(wnd_mean_lag3)+
               s(wnd_dir, bs = "cc")+
               ti(wnd_mean, wnd_dir, bs=c("tp", "cc"))+
               s(doy)+
               s(year, k=4)+
               ti(doy, year),
             data = model_df)
summary(gam_1)
plot(gam_1, pages=1, residuals=TRUE)

#Make non-signicant smooth terms parametric (linear) terms instead
gam_2 <- gam(kz ~
               s(station, bs = "re")+
               s(wnd_mean, k = 15)+
               wnd_mean_lag1+
               wnd_mean_lag2+
               wnd_mean_lag3+
               s(wnd_dir, bs = "cc", k = 15)+
               ti(wnd_mean, wnd_dir, bs=c("tp", "cc"))+
               s(doy)+
               s(year, k=4)+
               ti(doy, year),
             data = model_df)
summary(gam_2)
plot(gam_2, pages=1, residuals=TRUE)

#Check k values by plotting residuals vs smoothed predictors (with penalizing cubic splice) and 
#change k values in gam_2 model accordingly
rsd <- residuals(gam_2)
gam(rsd~s(wnd_mean, bs = "cs"), gamma=1.4, data=model_df)
gam(rsd~s(wnd_dir, bs = "cs"), gamma=1.4, data=model_df) 
gam(rsd~ti(wnd_mean, wnd_dir, bs="cs"), gamma=1.4, data=model_df) 
gam(rsd~s(doy, bs="cs"), gamma=1.4, data=model_df) #increase k
gam.check(gam_2)

#Fit models using different correlations structures
form <- formula(kz ~
                  s(wnd_mean, k = 15)+
                  wnd_mean_lag1+
                  wnd_mean_lag2+
                  wnd_mean_lag3+
                  s(wnd_dir, bs = "cc", k = 15)+
                  ti(wnd_mean, wnd_dir, bs=c("tp", "cc"))+
                  s(doy)+
                  s(year, k=4)+
                  ti(doy, year))

gam_nocorr <- gamm(form, 
                   random = list(station = ~1),
                   data = model_df)

gam_car <- gamm(form,
                correlation = corCAR1(form = ~doy|year), 
                random = list(station = ~1),
                data = model_df)

gam_gaus <- gamm(form, 
                 correlation = corGaus(form = ~doy|year), 
                 random = list(station = ~1),
                 data = model_df)

#Compare models
anova(gam_nocorr$lme, gam_car$lme, gam_gaus$lme)

#Save models
saveRDS(list("nocorr"=gam_nocorr, "gam_car"=gam_car, "gam_gaus"=gam_gaus), 
        paste0(modeling_path, "gam_list.rds"))

#Inspect model
summary(gam_car$gam)

#Autocorrelation reveal few slightly, significant lags but appears random from visual inspection,
#keep model and correlation structure
acf(resid(gam_car$lme, type = "normalized"))
pacf(resid(gam_car$lme, type = "normalized")) 
plot(Variogram(gam_car$lme, data = model_df))

#Compare candidate models by AIC
gam_car_wrap <- uGamm(form,
                      correlation = corCAR1(form = ~doy|year), 
                      random = list(station = ~1),
                      data = model_df)

#Fit in parallel
cl <- makeCluster(4, type = "SOCK")
clusterExport(cl, "model_df")

mod_sel <- pdredge(gam_car_wrap,
                   cluster = cl,
                   rank = "AIC",
                   fixed = c('s(wnd_mean, k = 15)',
                             's(wnd_dir, bs = "cc", k = 15)',
                             's(doy)'))

saveRDS(mod_sel, paste0(modeling_path, "gam_model_selection.rds"))

stopCluster(cl)

#Fit, inspect, and save best model
form_best <- formula(kz ~
                       s(wnd_mean, k = 15)+
                       wnd_mean_lag1+
                       s(wnd_dir, bs = "cc", k = 15)+
                       ti(wnd_mean, wnd_dir, bs=c("tp", "cc"))+
                       s(doy)+
                       s(year, k=4)+
                       ti(doy, year))

gam_best <- gamm(form_best, 
                 correlation = corCAR1(form = ~doy|year), 
                 random = list(station = ~1),
                 data = model_df)

summary(gam_best$gam)
acf(resid(gam_best$lme, type = "normalized"))
pacf(resid(gam_best$lme, type = "normalized"))
plot(Variogram(gam_best$lme, data = model_df))
par(mfrow=c(2,2))  
gam.check(gam_best$gam)  

saveRDS(gam_best, paste0(modeling_path, "gam_best.rds"))
