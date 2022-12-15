################################################################################
##
## [ PROJ ] Final Project: Data Cleaning
## [ FILE ] final_clean
## [ AUTH ] Agastia Cestyakara & Muhammad Rifqi Febrian
## [ INIT ] Nov 29, 2022
## [ DESC ] Data Cleaning R Code
##

################################################################################
#LOADING LIBRARY
library(tidyverse)
library(sf)
library(tmap)

 
################################################################################
#DATA UPLOADING

#uhi data
uhi_us <- read_csv("raw_data/Census_UHI_US_Urbanized_recalculated.csv")

#income data
tract_income <- read_csv("raw_data/ACSST5Y2017.S1902-Data.csv") #real data
meta_income <- read_csv("raw_data/ACSST5Y2017.S1902-Column-Metadata.csv") #meta data

#demographic data
tract_demog <- read_csv("raw_data/ACSST5Y2017.S0101-Data.csv") #real data
meta_demog <- read_csv("raw_data/ACSST5Y2017.S0101-Column-Metadata.csv") #meta data

#race data
tract_race <- read_csv("raw_data/ACSDT5Y2017.B02001-Data.csv") #real data
meta_race <- read_csv("raw_data/ACSDT5Y2017.B02001-Column-Metadata.csv") #meta data

#poverty data
tract_poverty <- read_csv("raw_data/ACSST5Y2017.S1701-Data.csv") #real data
meta_poverty <- read_csv("raw_data/ACSST5Y2017.S1701-Column-Metadata.csv") #meta data

################################################################################
#DATA STRUCTURE
#US major cities UHI
str(uhi_us, give.attr = FALSE)

#tract income
str(tract_income, give.attr = FALSE)

#tract demographic
str(tract_demog, give.attr = FALSE)

#tract races
str(tract_race, give.attr = FALSE)

#tract poverty
str(tract_poverty, give.attr = FALSE)


################################################################################
#DATA CLEANSING
#CLEANSIN FOR SEX AND RACE
tract_demog <- tract_demog %>% 
  select(GEO_ID, NAME, S0101_C01_001E, S0101_C03_001E, S0101_C05_001E, 
         S0101_C01_032E, S0101_C01_022E, S0101_C01_002E, S0101_C01_003E,
         S0101_C01_004E, S0101_C01_005E, S0101_C01_006E, S0101_C01_007E,
         S0101_C01_008E, S0101_C01_009E, S0101_C01_010E, S0101_C01_011E,
         S0101_C01_012E, S0101_C01_013E, S0101_C01_014E, S0101_C01_030E) #selecting important information

colnames(tract_demog) <- c("GEO_ID", "NAME", "total", "male", "female", "median_age", "under18",
                           "under5", "age5to9", "age10to14", "age15to19", "age20to24", "age25to29", 
                           "age30to34", "age35to39", "age40to44", "age45to49", "age50to54", "age55to59", 
                           "age60to64", "over64")

tract_demog <- tract_demog[-1, ] #removing first row

#creating new column for age 18 to 64 years
sapply(tract_demog, mode)
tract_demog[, c(3:21)] <- sapply(tract_demog[, c(3:21)], as.numeric)
str(tract_demog, give.attr = FALSE)

tract_demog <- tract_demog %>%
  mutate(age_0to64 = select(., under5:age60to64) %>% rowSums(na.rm = TRUE)) #calculating sum of age 0 to 64

age_0to64 <- c("age_0to64")
age_under18 <- c("under18")

tract_demog <- tract_demog %>%
  mutate(age_18to64 = .data[[age_0to64]] - .data[[age_under18]]) #subtracting to get sum of age 18-64

tract_demog <- tract_demog %>%
  select(GEO_ID, NAME, total, male, female, median_age, under18, age_18to64, over64)

save(list = "tract_demog", file = "clean_data/01_tract_demog_clean.RData")

rm(age_0to64, age_under18)

#CLEANSING FOR INCOME
tract_income <- tract_income %>% 
  select(GEO_ID, NAME, S1902_C01_019E, S1902_C01_001E) #selecting important information

colnames(tract_income) <- c("GEO_ID", "NAME", "mean_inc_percap", "mean_inc_fam")

tract_income <- tract_income[-1, ] #removing first row

sapply(tract_income, mode)
tract_income[, c(3:4)] <- sapply(tract_income[, c(3:4)], as.numeric) #converting as numeric
#str(tract_income, give.attr = FALSE)

tract_income <- tract_income %>% 
  mutate(census_geoid = substr(GEO_ID, 10, 20)) #converting the number to 11 digit only

save(list = "tract_income", file = "clean_data/02_tract_income_clean.RData")

#CLEANSING FOR POVERTY
tract_poverty <- tract_poverty %>% 
  select(GEO_ID, NAME, S1701_C02_001E, S1701_C02_011E, S1701_C02_012E, 
         S1701_C03_001E, S1701_C03_011E, S1701_C03_012E) #selecting important information

colnames(tract_poverty) <- c("GEO_ID", "NAME", "povt", "povt_male", "povt_female",
                             "povt_rate", "povt_rate_male", "povt_rate_female")

tract_poverty <- tract_poverty[-1, ] #removing first row

sapply(tract_poverty, mode)
tract_poverty[, c(3:8)] <- sapply(tract_poverty[, c(3:8)], as.numeric) #converting as numeric
str(tract_poverty, give.attr = FALSE)

tract_poverty <- tract_poverty %>% 
  mutate(census_geoid = substr(GEO_ID, 10, 20)) #converting the number to 11 digit only

save(list = "tract_poverty", file = "clean_data/03_tract_poverty_clean.RData")

#CLEANSING FOR RACE
tract_race <- tract_race %>%
  select(GEO_ID, NAME, B02001_001E, B02001_002E, B02001_003E, B02001_004E, B02001_005E, B02001_006E,
         B02001_007E, B02001_008E, B02001_009E, B02001_010E) #selecting important information

colnames(tract_race) <- c("GEO_ID", "NAME", "total", "white", "black_or_african_american", "american_indian_and_alaska_native",
                          "asian", "native_hawaiian_and_other_pacific_islander", "some_other_race",
                          "two_or_more_races", "two_races_including_some_other_race",
                          "two_races_excluding_some_other_race_and_three_or_more_races")

tract_race <- tract_race[-1, ] #removing first row

tract_race <- tract_race %>% 
  mutate(census_geoid = substr(GEO_ID, 10, 20)) #converting the number to 11 digit only

save(list = "tract_race", file = "clean_data/04_tract_race_clean.RData")

#LOADING NEWEST CLEAN DATA
load("clean_data/01_tract_demog_clean.RData")
load("clean_data/02_tract_income_clean.RData")
load("clean_data/03_tract_poverty_clean.RData")
load("clean_data/04_tract_race_clean.RData")

#CLEANSING FOR UHI DATA
uhi_us <- uhi_us[, -1] #removing first column

uhi_us <- uhi_us %>% #calculating the number of digit
  mutate(census_geoid = as.character(Census_geoid)) #converting to a character

str(uhi_us, give.attr = FALSE)

uhi_us <- uhi_us %>%
  mutate(census_geoid = ifelse(nchar(census_geoid) == 10,
                               gsub("^(.{0})(.*)$","\\10\\2",census_geoid),
                               census_geoid)) #adding 0 to the census_geoid with 10 digit

nchar(uhi_us$census_geoid) #checking the digit

class(uhi_us$census_geoid)

save(list = "uhi_us", file = "clean_data/05_uhi_us_clean.RData")

uhi_us_census <- uhi_us %>%
  select(census_geoid) #checking the column of census_geoid

rm(uhi_us_census)

#JOINING tract_demof and tract_income (age, sex, and income)
#str(tract_demog, give.attr = FALSE)
#str(tract_income, give.attr = FALSE)
tract_income <- tract_income %>%
  select(-NAME) #excluding name
tract_demog_income <- full_join(tract_demog,tract_income, by = "GEO_ID")

#JOINING tract_demog_income and tract_poverty (age, sex, income, and poverty)
#str(tract_demog_income, give.attr = FALSE)
tract_demog_income <- tract_demog_income %>%
  select(-census_geoid) #excluding census_geoid
str(tract_poverty, give.attr = FALSE)
tract_poverty <- tract_poverty %>%
  select(-NAME) #excluding name
tract_demog_sec <- full_join(tract_demog_income,tract_poverty, by = "GEO_ID")
tract_demog_sec <- tract_demog_sec %>%
  select(-mean_inc_percap, -mean_inc_fam)

#JOINING tract_demog_income and tract_race
tract_race <- tract_race %>%
  mutate(total = as.numeric(total),
         white = as.numeric(white),
         black_or_african_american = as.numeric(black_or_african_american),
         american_indian_and_alaska_native = as.numeric(american_indian_and_alaska_native),
         asian = as.numeric(asian),
         native_hawaiian_and_other_pacific_islander = as.numeric(native_hawaiian_and_other_pacific_islander),
         some_other_race = as.numeric(some_other_race),
         two_or_more_races = as.numeric(two_or_more_races),
         two_races_including_some_other_race = as.numeric(two_races_including_some_other_race),
         two_races_excluding_some_other_race_and_three_or_more_races = as.numeric(two_races_excluding_some_other_race_and_three_or_more_races))

tract_demog_sec_race <- full_join(tract_demog_sec, tract_race, by = c("census_geoid", "GEO_ID", "NAME", "total"))

save(list = "tract_race", file = "clean_data/06_tract_race_clean.RData")
save(list = "tract_demog_sec", file = "clean_data/07_tract_demog_sec_clean.RData")
save(list = "tract_demog_sec_race", file = "clean_data/08_tract_demog_sec_race_clean.RData")

#SELECTING COLUMN IN uhi_us data
str(uhi_us, give.attr = FALSE)
uhi_ndvi <- uhi_us %>%
  select(NDVI_rur, NDVI_rur_summer, NDVI_rur_winter, NDVI_urb_CT, NDVI_urb_CT_act, NDVI_urb_CT_act_summer,
         NDVI_urb_CT_act_winter, NDVI_urb_CT_summer, NDVI_urb_CT_winter, NDVI_urb_all, NDVI_urb_all_summer,
         NDVI_urb_all_winter, Total_Count, Urban_Count, Urban_geoid, Urban_name, Urban_name_extended, UHI_annual_day,
         UHI_annual_night, UHI_summer_day, UHI_summer_night, UHI_winter_day, UHI_winter_night, UHI_annual_day_city,
         UHI_annual_night_city, UHI_summer_day_city, UHI_summer_night_city, UHI_winter_day_city, UHI_winter_night_city,
         census_geoid, DEM_rur, DEM_urb_CT, DEM_urb_CT_act, DEM_urb_all)

#joining uhi_ndvi with tract_demog_income_race
uhi_ndvi_tract <- inner_join(uhi_ndvi, tract_demog_sec_race, by = "census_geoid")

#save uhi_ndvi_tract
save(list = "uhi_ndvi_tract", file = "clean_data/09_uhi_ndvi_tract_clean.RData")

#Last loading all data
load("clean_data/09_uhi_ndvi_tract_clean.RData")

################################################################################
#Last Inspection
#checking the NA
summary(uhi_ndvi_tract) #checking whether we has weird data

table(sign(uhi_ndvi_tract$UHI_summer_day))
# There are 42510 positive UHI
# There are 13130 census tract that has negative value which is interesting
# QUESTION: How some census tract could has cooler areas compared to its rural surroundings?

#removing negative values
uhindvi_tract_duplicate <- uhi_ndvi_tract
uhindvi_tract_duplicate <- uhindvi_tract_duplicate %>%
  mutate(UHI_summer_day_clean = ifelse(UHI_summer_day <= 0, NA, UHI_summer_day))
summary(uhindvi_tract_duplicate$UHI_summer_day_clean) #no negative values for now
uhindvi_tract_omit <- na.omit(uhindvi_tract_duplicate)
save(list = "uhindvi_tract_omit", file = "clean_data/10_uhindvi_tract_omit.RData")

#Categorizing data and making it final
uhindvi_tract <- uhindvi_tract_omit %>%
  mutate(other = american_indian_and_alaska_native + some_other_race + two_or_more_races + two_races_including_some_other_race +
           two_races_excluding_some_other_race_and_three_or_more_races) %>%
  mutate(asian_nativehawaiaan_pacific_islander = asian + native_hawaiian_and_other_pacific_islander)
uhindvi_tract <- uhindvi_tract %>%
  select(census_geoid, GEO_ID, NAME, total, Total_Count, Urban_Count, Urban_geoid, Urban_name, Urban_name_extended,
         male, female, median_age, under18, age_18to64, over64,povt, povt_male, povt_female, povt_rate, 
         povt_rate_male,povt_rate_female, white, black_or_african_american, asian_nativehawaiaan_pacific_islander,
         other, NDVI_urb_CT_summer, UHI_summer_day_clean, DEM_urb_CT) %>%
  mutate(UHI_summer_day = UHI_summer_day_clean,
         NDVI_urb_summer = NDVI_urb_CT_summer,
         DEM_urb = DEM_urb_CT) 

uhindvi_tract <- uhindvi_tract %>%
  select(census_geoid, GEO_ID, NAME, total, Total_Count, Urban_Count, Urban_geoid, Urban_name, Urban_name_extended,
         male, female, median_age, under18, age_18to64, over64, povt, povt_male, povt_female, povt_rate,
         povt_rate_male,povt_rate_female, white, black_or_african_american, asian_nativehawaiaan_pacific_islander,
         other, NDVI_urb_summer, UHI_summer_day, DEM_urb)

uhindvi_tract <- uhindvi_tract %>%
  mutate(nblack = black_or_african_american/total,
         nfemale = female/total,
         nover64 = over64/total)

#Creating dummy variables for necessary data
uhindvitract <- uhindvi_tract %>%
  mutate(highpov = as.numeric(povt_rate > median(uhindvi_tract$povt_rate)),
         shareblack = as.numeric(nblack > 0.5),
         sharefemale = as.numeric(nfemale > median(nfemale)),
         shareover64 = as.numeric(nover64 > 0.5)) %>%
  mutate(highpov = factor(highpov, levels = c(0,1),
                          labels = c("Not high poverty", "High poverty")),
         shareblack = factor(shareblack, levels=c(0,1),
                             labels = c("Majority non-Black", "Majority Black")),
         shareover64 = factor(shareover64, levels=c(0,1),
                              labels = c("Majority age under 64", "Majority age over 64")),
         sharefemale = factor(sharefemale, levels=c(0,1),
                              labels = c("Majority male", "Majority female")))

sum(uhindvitract$nblack > 0.5)
sum(uhindvitract$nfemale > median(uhindvitract$nfemale))
sum(uhindvitract$nover64 > 0.5)

save(list = "uhindvi_tract", file = "clean_data/11_uhindvi_tract.RData")
save(list = "uhindvitract", file = "clean_data/00_uhindvitract.RData")

str(uhindvitract, give.attr = FALSE)

#Creating dummy variables for necessary data
uhindvitract_nolabel <- uhindvi_tract %>%
  mutate(highpov = as.numeric(povt_rate > median(uhindvi_tract$povt_rate)),
         shareblack = as.numeric(nblack > 0.5),
         sharefemale = as.numeric(nfemale > median(nfemale)),
         shareover64 = as.numeric(nover64 > 0.5)) %>%
  mutate(highpov = factor(highpov, levels = c(0,1)),
         shareblack = factor(shareblack, levels=c(0,1)),
         shareover64 = factor(shareover64, levels=c(0,1)),
         sharefemale = factor(sharefemale, levels=c(0,1)))

save(list = "uhindvitract_nolabel", file = "clean_data/00_uhindvitract_nolabel.RData")

rm(meta_demog, meta_income, meta_poverty, meta_race, tract_demog, tract_demog_income, tract_demog_sec, tract_demog_sec_race,
   tract_income, tract_poverty, tract_race, uhi_ndvi, uhi_ndvi_tract, uhi_us, uhindvi_tract, uhindvi_tract_duplicate, uhindvi_tract_omit,
   uhindvitract, uhindvitract_nolabel)

#DATA MAPPING
#Latest data
load("clean_data/00_uhindvitract.RData")

#loading US urban data
usa_urban <- st_read(dsn = "raw_data/shpfile/USA_urban/tl_2017_us_uac10.shp")

uhindvitract <- uhindvitract %>%
  mutate(GEOID10 = Urban_geoid) %>%
  mutate(GEOID10 = as.character(GEOID10)) %>%
  mutate(GEOID10 = ifelse(nchar(GEOID10) == 3,
                          gsub("^(.{0})(.*)$","\\100\\2", GEOID10),
                          GEOID10)) %>%
  mutate(GEOID10 = ifelse(nchar(GEOID10) == 4,
                          gsub("^(.{0})(.*)$","\\10\\2", GEOID10),
                          GEOID10))

save(list = "uhindvitract", file = "clean_data/00_final_uhindvitract.RData")

#creating map data
uhindvitract_map <- uhindvitract %>%
  select(Urban_name, Urban_name_extended, NDVI_urb_summer, total,
         UHI_summer_day, DEM_urb, povt_rate, nblack, nfemale, nover64, GEOID10)

uhindvitract_map <- uhindvitract_map %>%
  group_by(GEOID10, Urban_name, Urban_name_extended) %>%
  summarise(total = sum(total),
            UHI_mean = mean(UHI_summer_day),
            NDVI_mean = mean(NDVI_urb_summer),
            DEM_mean = mean(DEM_urb),
            povt_rate = mean(povt_rate),
            shareblack = mean(nblack),
            sharefemale = mean(nfemale))

usa_urban_map <- usa_urban %>%
  select(GEOID10, geometry)

uhindvi_urban_map <- inner_join(uhindvitract_map, usa_urban_map, by = "GEOID10")

#save new dataframe
save(list = "uhindvitract_map", file = "clean_data/12_uhindvitract_map.RData")
save(list = "uhindvi_urban_map", file = "clean_data/00_final_uhindvi_urbanmap.RData")

#Taking New Orleans
neworleans_uhi <- uhindvitract %>%
  filter(GEOID10 == "62677") %>%
  select(census_geoid, Urban_name, Urban_name_extended, NDVI_urb_summer, total,
         UHI_summer_day, DEM_urb, povt_rate, nblack, nfemale, nover64, GEOID10)

louisiana_ct <- st_read(dsn = "USA_louisiana_2017/tl_2017_22_tract.shp")

louisiana_ct <- louisiana_ct %>%
  select(GEOID, geometry) %>%
  mutate(census_geoid = GEOID) %>%
  select(-GEOID)

neworleansuhi <- inner_join(neworleans_uhi, louisiana_ct, by = "census_geoid")

save(list = "neworleansuhi", file = "neworleansuhi.RData")

#Taking New York--Newark Data
NY_uhi <- uhindvitract %>%
  filter(GEOID10 == "63217") %>%
  select(census_geoid, Urban_name, Urban_name_extended, NDVI_urb_summer, total,
         UHI_summer_day, DEM_urb, povt_rate, nblack, nfemale, nover64, GEOID10)

ny_ct <- st_read(dsn = "raw_data/shpfile/USA_newyork/tl_2017_36_tract.shp")
nj_ct <- st_read(dsn = "raw_data/shpfile/USA_newjersey/tl_2017_34_tract.shp")
ct_ct <- st_read(dsn = "taw_data/shpfile/USA_connecticut/tl_2017_09_tract.shp")

ny_ct <- ny_ct %>%
  select(GEOID, geometry) %>%
  mutate(census_geoid = GEOID) %>%
  select(-GEOID)

nj_ct <- nj_ct %>%
  select(GEOID, geometry) %>%
  mutate(census_geoid = GEOID) %>%
  select(-GEOID)

ct_ct <- ct_ct %>%
  select(GEOID, geometry) %>%
  mutate(census_geoid = GEOID) %>%
  select(-GEOID)

nynjct <- rbind(ny_ct, nj_ct, ct_ct)

nynjctuhi <- inner_join(NY_uhi, nynjct, by = "census_geoid")

str(nynjctuhi, give.attr = FALSE)

nynjctuhi = st_as_sf(nynjctuhi)

#New Hampsire
NH_uhi <- uhindvitract %>%
  filter(GEOID10 == "53740") %>%
  select(census_geoid, Urban_name, Urban_name_extended, NDVI_urb_summer, total,
         UHI_summer_day, DEM_urb, povt_rate, nblack, nfemale, nover64, GEOID10)

nh_ct <- st_read(dsn = "USA_newhampsire_2017/tl_2017_33_tract.shp")

nh_ct <- nh_ct %>%
  select(GEOID, geometry) %>%
  mutate(census_geoid = GEOID) %>%
  select(-GEOID)

nhct_uhi <- inner_join(NH_uhi, nh_ct, by = "census_geoid")

str(nhct_uhi, give.attr = FALSE)

nhct_uhi = st_as_sf(nhct_uhi)

t1_nh <- tm_shape(nhct_uhi) + 
  tm_polygons("UHI_summer_day")

t1_nh
save(list = "nhct_uhi", file = "nhct_uhi_map.RData")

###############################################################################
#REMOVAL
rm(uhindvitract, uhindvi_urban_map, uhindvitract_map, usa_urban, usa_urban_map, ct_ct,
   nj_ct, ny_ct)

###############################################################################
#EXPLORING NEW YORK TREE DATASET
tree_1995 <- read_csv("raw_data/1995_Street_Tree_Census.csv")
tree_2005 <- read_csv("raw_data/2005_Street_Tree_Census.csv")
tree_2015 <- read_csv("raw_data/2015_Street_Tree_Census.csv")
census_area <- read_csv("raw_Data/2020_Census_Tracts_Tabular.csv")

#CLEANING NEW YORK DATASET
#Tree data 1995
tree_1995_clean <- tree_1995 %>%
  select(RecordId, CensusTract_2010) %>%
  mutate(census_tract = as.character(CensusTract_2010)) %>%
  mutate(census_tract = ifelse(nchar(census_tract) == 1,
                               gsub("^(.{0})(.*)$","\\1000\\2", census_tract),
                               census_tract)) %>%
  mutate(census_tract = ifelse(nchar(census_tract) == 2,
                               gsub("^(.{0})(.*)$","\\100\\2", census_tract),
                               census_tract)) %>%
  mutate(census_tract = ifelse(nchar(census_tract) == 3,
                               gsub("^(.{0})(.*)$","\\10\\2", census_tract),
                               census_tract))

tree_1995_clean <- tree_1995_clean %>%
  mutate(census_tract = ifelse(nchar(census_tract) == 4,
                               paste0(census_tract, "00"), census_tract)) %>%
  mutate(census_tract = ifelse(nchar(census_tract) == 5,
                               paste0(census_tract, "0"), census_tract))

#Tree data 2015
tree_2015_clean <- tree_2015 %>%
  mutate(census_tract = `census tract`) %>%
  select(tree_id, census_tract) %>%
  mutate(census_tract = as.character(census_tract)) %>%
  mutate(census_tract = ifelse(nchar(census_tract) == 1,
                               gsub("^(.{0})(.*)$","\\1000\\2", census_tract),
                               census_tract)) %>%
  mutate(census_tract = ifelse(nchar(census_tract) == 2,
                               gsub("^(.{0})(.*)$","\\100\\2", census_tract),
                               census_tract)) %>%
  mutate(census_tract = ifelse(nchar(census_tract) == 3,
                               gsub("^(.{0})(.*)$","\\10\\2", census_tract),
                               census_tract)) %>%
  mutate(census_tract = ifelse(nchar(census_tract) == 4,
                               paste0(census_tract, "00"), census_tract)) %>%
  mutate(census_tract = ifelse(nchar(census_tract) == 5,
                               paste0(census_tract, "0"), census_tract))

#NYC Data
#Cutting NJ Data, which is row 1 - 1237
nynjctuhi_clean <- nynjctuhi %>%
  mutate(census_geoid2 = as.numeric(census_geoid)) %>%
  filter(census_geoid2 > 35999999999)

ny_uhi_clean <- nynjctuhi_clean
save(list = "ny_uhi_clean", file = "clean_data/00_ny_uhi.RData")

ny_uhi_clean <- ny_uhi_clean %>%
  mutate(highpov = as.numeric(povt_rate > median(ny_uhi_clean$povt_rate)),
         shareblack = as.numeric(nblack > 0.5),
         sharefemale = as.numeric(nfemale > median(nfemale)),
         shareover64 = as.numeric(nover64 > 0.5)) %>%
  mutate(highpov = factor(highpov, levels = c(0,1),
                          labels = c("Not high poverty", "High poverty")),
         shareblack = factor(shareblack, levels=c(0,1),
                             labels = c("Majority non-Black", "Majority Black")),
         shareover64 = factor(shareover64, levels=c(0,1),
                              labels = c("Majority age under 64", "Majority age over 64")),
         sharefemale = factor(sharefemale, levels=c(0,1),
                              labels = c("Majority male", "Majority female")))

save(list = "ny_uhi_clean", file = "clean_data/00_ny_uhi.RData")


ny_uhi_clean <- nynjctuhi_clean %>%
  mutate(census_tract = substring(census_geoid, first = 6, last = 11))

#Summarizing number of tree in 1995 and 2015
tree_1995_sum <- tree_1995_clean %>%
  group_by(census_tract) %>%
  summarise(num_tree = n())

tree_2015_sum <- tree_2015_clean %>%
  group_by(census_tract) %>%
  summarise(num_tree = n())

ny_uhi_clean <- ny_uhi_clean %>%
  select(-highpov, -sharefemale, -shareover64, -shareblack)

ny_uhi_clean2 <- ny_uhi_clean %>%
  select(-census_geoid, -Urban_name, -Urban_name_extended, -GEOID10, -geometry, -census_geoid2) %>%
  group_by(census_tract) %>%
  summarise(NDVI_urb_summer = mean(NDVI_urb_summer),
            total = sum(total),
            UHI_summer_day = mean(UHI_summer_day),
            DEM_urb = mean(DEM_urb),
            povt_rate = mean(povt_rate),
            nblack = mean(nblack),
            nfemale = mean(nfemale),
            nover64 = mean(nover64))

#Combining tree and NY UHI
tree_ny <- inner_join(tree_1995_sum, tree_2015_sum, by ="census_tract")

tree_ny <- tree_ny %>%
  mutate(tree_1995 = num_tree.x,
         tree_2015 = num_tree.y) %>%
  select(-num_tree.x, -num_tree.y)

ny_uhi_tree <- inner_join(ny_uhi_clean2, tree_ny, by = "census_tract")
save(list = "ny_uhi_tree", file = "clean_data/00_nyuhitree.RData")

#Cleaning census tract data to get the area
census_area_clean <- census_area %>%
  select(CT2020, Shape_Area) %>%
  mutate(census_tract = CT2020) %>%
  select(-CT2020)

ny_uhi_tree <- inner_join(ny_uhi_tree, census_area_clean, by ="census_tract")
save(list = "ny_uhi_tree", file = "clean_data/00_nyuhitree.RData")

rm(census_area, census_area_clean, tree_1995, tree_1995_clean, tree_2005, tree_2015, tree_2015_clean, tree_2015_sum,
   tree_1995_sum)
rm(NY_uhi, ny_uhi_clean, ny_uhi_clean2, nynjct, nynjctuhi, t1_ny, t1_ny_black, t1_ny_pvt)

load("clean_data/00_nyuhitree.RData")

ny_uhi_tree <- ny_uhi_tree %>%
  mutate(highpov = as.numeric(povt_rate > median(ny_uhi_tree$povt_rate)),
         shareblack = as.numeric(nblack > 0.5),
         sharefemale = as.numeric(nfemale > median(nfemale)),
         shareover64 = as.numeric(nover64 > median(nover64))) %>%
  mutate(highpov = factor(highpov, levels = c(0,1),
                          labels = c("Not high poverty", "High poverty")),
         shareblack = factor(shareblack, levels=c(0,1),
                             labels = c("Majority non-Black", "Majority Black")),
         shareover64 = factor(shareover64, levels=c(0,1),
                              labels = c("Majority younger population", "Majority older population")),
         sharefemale = factor(sharefemale, levels=c(0,1),
                              labels = c("Majority male", "Majority female")))

save(list = "ny_uhi_tree", file = "clean_data/00_nyuhitree.RData")
