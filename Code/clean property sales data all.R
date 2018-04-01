library(dplyr)

sale <- read.csv("Data/Monthly Sales 2007-2017.csv",header = T)
prpty <- read.csv("Data/Real Property Report.csv",header = T)

sale <- sale[sale$Prop.Class%in%c(101,102,121,122,131,132)&sale$Sale.Price>2000,]
sale$addr <- with(sale,paste0(Situs.Street, ", ",Situs.CSZ))
sale$valid.addr <- ifelse(substring(sale$addr,1,1)%in%c(1,2,3,4,5,6,7,8,9),0,1)

sale_addr <- sale[sale$valid.addr==0,][,c(1,21)]

sale_addr <- sale_addr[!duplicated(sale_addr$PropId), ]


# convert address to long, lat
if (!require("pacman")) install.packages("pacman")

p_load(gdata, dplyr, ggmap)

# see how many requests we have left with google
geocodeQueryCheck()

# Use google map geocoder to get standardized addresses
#This requires to run in chuncks, as Google limits queries/day to 2,500
geocode_adress <- function(df, chunk_size, address_col, prefix=""){
  lat_col <- paste0(prefix, "lat")
  lon_col <- paste0(prefix, "lon")
  if (! lat_col %in% names(df)) df[, lat_col] <- ""
  if (! lon_col %in% names(df)) df[, lon_col] <- ""
  
  for (i.start in seq(1, nrow(df), chunk_size)) {
    
    i.end <- ifelse((i.start+chunk_size) > nrow(df), nrow(df), i.start+chunk_size-1)
    
    latlon <- geocode(df[i.start:i.end, address_col], output = "latlon")
    df[i.start:i.end, lat_col] <- latlon$lat
    df[i.start:i.end, lon_col] <- latlon$lon
    
    if (i.end != nrow(df)) Sys.sleep(24*60*60) #wait for 24 hours
  }
  df
}


# Test cases

# df <- data.frame(HADDR=c("1600 pennsylvania avenue, washington dc", 
"506 SW Mill St, Portland, OR"),
BFHADDR=c("1539 Pennsylvania Ave SE, Washington, DC", 
          "1720 SW 4th Ave, Portland, OR 97201"), 
stringsAsFactors =F)

sale_addr1 <- sale_addr[1:10000,]
sale_addr1 <- geocode_adress(sale_addr1,2500,'addr')

sale_addr2 <- sale_addr[10001:20000,]
sale_addr2 <- geocode_adress(sale_addr2,2500,'addr')

sale_addr3 <- sale_addr[20001:30000,]
sale_addr3 <- geocode_adress(sale_addr3,2500,'addr')

# sale_addr <- geocode_adress(sale_addr, 2500, 'addr')
