library(foreign)
library(raster)
library(dplyr)
library(rgdal)

rm(list=ls()) 

db <- read.dta("./UMSP database updated Jan 17th 2020.dta")
colnames(db)
db$date <- ymd(db$date)
db$monthyear_date <- (paste(year(db$date), month(db$date), "01", sep = "-"))
db$monthyear_date <- ymd(db$monthyear_date)
range(db$monthyear_date)

db <- db %>% filter(monthyear_date >= "2013-01-01")
db <- db %>% filter(site == "Aduku" |
                      site == "Anyeke" |
                      site == "Aboke" |
                      site == "Awach" |
                      site == "Lalogi" |
                      site == "Patongo" |
                      site == "Atiak" |
                      site == "Padibe" |
                      site == "Namokora" |
                      site == "Nagongera" |
                      site == "Amolatar" |
                      site == "Dokolo" |
                      site == "Orum" |
                      site == "Alebtong")

#Load in CHIRPS data (precipitation)
file_loc <- "./CHIRPS/Monthly CHIRPS data"
month_year_rasters <- c()
for(year in 2012:2019) { 
  for(month in 1:12) {
    file <- paste("chirps-v2.0", year, sprintf("%02d", month), "tif", sep = ".")
    filename <- paste(file_loc, file, sep = "/")
    rainfall <- raster(filename)
    month_year_rasters <- append(month_year_rasters, rainfall)
  }
  year
}

length(month_year_rasters)

#Define function to put percentile of rainfall into data file
count_months <- function(start_date, sample_date) { #Need to index the sample months to correspond to the correct pxtile raster
  date_of_sample <- as.POSIXlt(sample_date)
  date_of_start <- as.POSIXlt(start_date)
  12 * (date_of_sample$year - date_of_start$year) + (date_of_sample$mon - date_of_start$mon)
}

count_months_jan12 <- function(sample_date) { #Index sample dates as month # with 1 = January 2013
  count_months("2011-12-01", sample_date)
}

db <- db %>% dplyr::select(site, monthyear_date)
db <- unique(db)

db$monthid <- sapply(db$monthyear_date, count_months_jan12)
range(db$monthid)

db$monthid_lag <- db$monthid-1

#lat/long
latlong <- read.csv("./Shapefiles/Health facilities/List of MRCs and codes_latlong.csv")
latlong <- latlong %>% dplyr::select(Health.facility, Latitude, Longitude)
latlong <- latlong %>% rename(site = Health.facility)

db <- merge(db, latlong, by = "site")
coordinates(db) <- ~Latitude + Longitude
plot(db)

#Loop over sample month to extract exposure data from proper raster
for(i in 1:length(db)) {
  month <- db$monthid_lag[i]
  coordinates <- db[i,]
  db$precip_lag[i] <- raster::extract(month_year_rasters[[month]], coordinates)
  print(i)
}

for(i in 1:length(db)) {
  month <- db$monthid[i]
  coordinates <- db[i,]
  db$precip[i] <- raster::extract(month_year_rasters[[month]], coordinates)
  print(i)
}

summary(db$precip)
hist(db$precip)
write.csv(db, "./Data/precipitation for irs project.csv")
