library(sf)
library(tidyverse)


# Preparing network dataset -----------------------------------------------


bkenet_16 <- st_read("Data/bike_route_2016.shp")

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


bkenet_35 <- st_read("Data/2030_bik_fac.shp")
library(readxl)
aadt_elev <- read_excel("Data/bikenet2012_aadt_elev.xlsx",sheet="bikenet2012_aadt_elev")

a <- bkenet_35 %>%
  left_join(aadt_elev,est_vol,by=c("BIKEID","localid")) %>%
  left_join(aadt_elev,dzp, by=c("BIKEID","localid")) %>%
  left_join(aadt_elev,dzn, by=c("BIKEID","localid")) %>%
  mutate(F_stress=case_when(est_vol<10000 & BIKETYP=="BKE-LANE" ~ -0.05,
                            est_vol<10000 & BIKETYP=="BKE-BLVD" ~ -0.14,
                            est_vol<10000 & BIKETYP %in% c("BKE-TRAK","BKE-BUFF") ~ -0.18,
                            est_vol<10000 & BIKETYP=="PTH-REMU" ~ -0.21,
                            est_vol>10000 & est_vol<20000 & is.na(BIKETYP) ~ 0.295,
                            est_vol>10000 & est_vol<20000 & BIKETYP=="BKE-BLVD" ~ -0.1,
                            est_vol>10000 & est_vol<20000 & BIKETYP %in% c("BKE-TRAK","BKE-BUFF") ~ -0.15,
                            est_vol>20000 & est_vol<30000 & is.na(BIKETYP) ~ 1.387,
                            est_vol>20000 & est_vol<30000 & BIKETYP %in% c("BKE-TRAK","BKE-BUFF") ~ -0.1,
                            est_vol>30000 & is.na(BIKETYP) ~ 6.675,
                            TRUE ~ 0),
         slope=(dzp+dzn)/LENGTH,
         F_Slope=case_when(slope>0.06 ~ 7.15,
                           slope>0.04 ~ 2.05,
                           slope>0.02 ~0.55,
                           TRUE ~0),
         W_link = LENGTH*(1+F_stress+F_slope),
         L_link = LENGTH*(1+F_slope))


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

files_to_load <- list.files(path="Data/",pattern = "^route.*.dbf$",full.names = TRUE)
# meta character expression reference: chttps://linuxconfig.org/match-beginning-and-end-of-the-filename-using-meta-characters-and-regex
route <- lapply(files_to_load, read.dbf)
