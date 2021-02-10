#libraries and functions

library(tidyverse);library(lubridate);library(readxl)

rawdata_path <- paste0(getwd(), "/data/raw/")

#ggplot theme used for figures
theme_pub <- theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text = element_text(colour = "black"), 
        strip.background = element_rect(fill = "white"))
theme_set(theme_pub)