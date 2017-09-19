library(sf)
library(tidyverse)


# Preparing network dataset -----------------------------------------------


bkenet_16 <- st_read("Data/bkenet_16.shp")
bkenet_16 <- read.dbf("Data/bkenet_16.dbf")

bkenet_16 <- bkenet_16 %>% 
  mutate(
    F_stress=case_when(
    est_vol<10000 & BIKETYP=="BKE-LANE" ~ -0.05,
    est_vol<10000 & BIKETYP=="BKE-BLVD" ~ -0.14,
    est_vol<10000 & BIKETYP %in% c("BKE-TRAK","BKE-BUFF") ~ -0.18,
    est_vol<10000 & BIKETYP=="PTH-REMU" ~ -0.21,
    est_vol>10000 & est_vol<20000 & is.na(BIKETYP) ~ 0.295,
    est_vol>10000 & est_vol<20000 & BIKETYP=="BKE-BLVD" ~ -0.1,
    est_vol>10000 & est_vol<20000 & BIKETYP %in% c("BKE-TRAK","BKE-BUFF") ~ -0.15,
    est_vol>20000 & est_vol<30000 & is.na(BIKETYP) ~ 1.387,
    est_vol>20000 & est_vol<30000 & BIKETYP %in% c("BKE-TRAK","BKE-BUFF") ~ -0.1,
    est_vol>30000 & is.na(BIKETYP) ~ 6.675,
    TRUE ~ 0
  ),
  W_link = LENGTH*(1+F_stress+F_slope),
  L_W_slp = LENGTH*(1+F_slope)) 

st_write(bkenet_16,"Data/bkenet_16.shp")
# if st_write doesn't work, use as(object,"Spatial"), writeOGR


bkenet_35 <- st_read("Data/bkenet_35_NEWnew.shp")
# bkenet_35<- bkenet_35 %>% 
  select(c(1:15,47:48)) %>%
  left_join(bkenet_16[,c("BIKEID","BIKETYP")],by="BIKEID",suffix=c("_35","_16")) 

# levels(bkenet_35$BIKETYP_35)[levels(bkenet_35$BIKETYP_35)=='100'] <- "Greenway"
# levels(bkenet_35$BIKETYP_35)[levels(bkenet_35$BIKETYP_35)=='0'] <- bkenet_35$BIKETYP_16
# levels(bkenet_35$BIKETYP_35)[levels(bkenet_35$BIKETYP_16)=='PTH_REMU'] <- "PTH_REMU"
# levels(bkenet_35$BIKETYP_35)[FID_1=27568] <- 

bkenet_35 <- bkenet_35 %>%
  mutate(F_stress_35=case_when(est_vol<10000 & BIKETYP_35=="BKE-LANE" ~ -0.05,
                            est_vol<10000 & BIKETYP_35=="BKE-BLVD" ~ -0.14,
                            est_vol<10000 & BIKETYP_35 %in% c("BKE-TRAK","BKE-BUFF","Greenway") ~ -0.18,
                            est_vol<10000 & BIKETYP_35=="PTH-REMU" ~ -0.21,
                            est_vol>10000 & est_vol<20000 & is.na(BIKETYP_35) ~ 0.295,
                            est_vol>10000 & est_vol<20000 & BIKETYP_35=="BKE-BLVD" ~ -0.1,
                            est_vol>10000 & est_vol<20000 & BIKETYP_35 %in% c("BKE-TRAK","BKE-BUFF","Greenway") ~ -0.15,
                            est_vol>20000 & est_vol<30000 & is.na(BIKETYP_35) ~ 1.387,
                            est_vol>20000 & est_vol<30000 & BIKETYP_35 %in% c("BKE-TRAK","BKE-BUFF","Greenway") ~ -0.1,
                            est_vol>30000 & is.na(BIKETYP_35) ~ 6.675,
                            TRUE ~ 0),
         slope=(dzp+dzn)/LENGTH,
         F_Slope=case_when(slope>0.06 ~ 7.15,
                           slope>0.04 ~ 2.05,
                           slope>0.02 ~0.55,
                           TRUE ~0),
         W_link = LENGTH*(1+F_stress_35+F_slope),
         L_link = LENGTH*(1+F_slope))
# There are 143 greenway links with est_vol above 30,000, we transform them as 0 F_stress

st_write(bkenet_35,"Data/bkenet_35.shp")

# Calculate accessibility -------------------------------------------------
library(tidyverse)
# rt_emp_16 <- st_read("Data/route_emply_base.shp")
rt_emp_16 <- read.dbf("Data/route_emply_base.dbf")
sum_emp_16 <- rt_emp_16 %>% 
  group_by(IncidentID) %>%
  summarise(count=n(),
            W_link=mean(Total_W_li+Total_Turn),
            L_W_Slp=mean(Total_L_W_+Total_Turn), 
            Length=mean(Total_Leng))

rt_emp_grwy <- read.dbf("Data/route_emply_grwy.dbf")
sum_emp_grwy <- rt_emp_grwy %>% 
  group_by(IncidentID) %>%
  summarise(count=n(),
            W_link=mean(Total_W_li+Total_Turn),
            L_W_Slp=mean(Total_W_L_+Total_Turn), 
            Length=mean(Total_Leng))

files_to_load <- list.files(path="Data/",pattern = "^route.*.dbf$",full.names = TRUE)
# meta character expression reference: https://linuxconfig.org/match-beginning-and-end-of-the-filename-using-meta-characters-and-regex
route <- lapply(files_to_load, read.dbf)
