#Libraries and functions

library(raster);library(tidyverse);library(lubridate);library(readxl);library(httr);library(jsonlite);library(mgcv)
library(patchwork);library(mgcViz);library(MuMIn);library(snow)
library(sf);library(rnaturalearth);library(zoo);library(boot)

#Figure sizing. For most journals the figures should be 39 mm, 84 mm, 129 mm, or 174 mm wide and not higher than 234 mm.

rawdata_path <- paste0(getwd(), "/data/raw/")
figures_path <- paste0(getwd(), "/figures/")
modeling_path <- paste0(getwd(), "/modeling/")

#ggplot theme used for figures
theme_pub <- theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text = element_text(colour = "black"), 
        strip.background = element_rect(fill = "white"))
theme_set(theme_pub)