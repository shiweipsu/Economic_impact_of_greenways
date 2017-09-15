library(tidyverse)
library(foreign)
library(ggplot2)
library(reshape2)
library(sf)

block <- read.dbf("Data/Portland_blocks.dbf")
colnames(block)[23] <- "dis_bk_16"

block <- block %>% 
  mutate(dis_bk_16=ifelse(dis_bk_16==999999,NA,dis_bk_16) )%>% 
  mutate(dis_bk_grw=ifelse(dis_bk_grw==999999,NA,dis_bk_grw) ) 

blockgrp <- st_read("Data/PDX_blockgrp_point.shp")

acc2_blockgrp <- block %>% 
  group_by(substr(GEOID10,1,12)) %>%
  summarise(emp_16=sum(C000*Empl_cent*wth_buf_16),
            ret_16=sum(Ret_emply*Ret_cent*wth_buf_16),
            sev_16=sum(Sev_emply*Sev_cent*wth_buf_16),
            emp_grw=sum(C000*Empl_cent*wth_buf_gw),
            ret_grw=sum(Ret_emply*Ret_cent*wth_buf_gw),
            sev_grw=sum(Sev_emply*Sev_cent*wth_buf_gw)) 

pdx
  