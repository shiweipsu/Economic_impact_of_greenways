library(foreign)
library(tidyverse)

block <- read.dbf("Data/Portland_blocks.dbf")
colnames(block)[23] <- "dis_bk_16"
block <- block[block$dis_bk_16<999998 & block$dis_bk_grw<999998 & block$emp_Wl_16<999998 & block$emp_Wl_gw<999998
               & block$ret_pct_gw>=0.79,]
block$TRBG <- substring(block$GEOID10,6,12)

block <- block %>%
  mutate(acc1_base = exp(0-dis_bk_16/5280)*100,
         acc3_emp_base = exp(0-emp_Wl_16/5280)*100,
         acc3_emp_pct_base = 1/emp_pct_16,
         acc3_ret_base = exp(0-ret_Wl_16/5280)*100,
         acc3_ret_pct_base = 1/ret_pct_16,
         acc3_svc_base = exp(0-svc_Wl_16/5280)*100,
         acc3_svc_pct_base = 1/svc_pct_16,
         acc3_pak_base = exp(0-pak_Wl_16/5280)*100,
         acc3_pak_pct_base = 1/pak_pct_16,
         acc1_gw = exp(0-dis_bk_grw/5280)*100,
         acc3_emp_gw = exp(0-emp_Wl_gw/5280)*100,
         acc3_emp_pct_gw = 1/emp_pct_gw,
         acc3_ret_gw = exp(0-ret_Wl_gw/5280)*100,
         acc3_ret_pct_gw = 1/ret_pct_gw,
         acc3_svc_gw = exp(0-svc_Wl_gw/5280)*100,
         acc3_svc_pct_gw = 1/svc_pct_gw,
         acc3_pak_gw = exp(0-pak_Wl_gw/5280)*100,
         acc3_pak_pct_gw = 1/pak_pct_gw)
         
summary(block)
        
# compute UGM
# 
block_grp <- block %>%
  group_by(TRBG) %>%
  select(TRBG:acc3_pak_pct_base,acc1_gw,acc3_emp_gw,acc3_emp_pct_gw,acc3_ret_gw,acc3_ret_pct_gw,acc3_svc_gw,acc3_svc_pct_gw,
         acc3_pak_gw,acc3_pak_pct_gw) %>%
  summarise_all(mean)

