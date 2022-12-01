################################################################################
##
## [ PROJ ] Final Project
## [ FILE ] main_final
## [ AUTH ] Muhammad Rifqi Febrian & Agastia Cestyakara
## [ INIT ] Nov 29, 2022
##
################################################################################

#load libraries
library(tidyverse)

#get directory
getwd()

################################################################################
#DATA UPLOADING

#uhi data
uhi_us <- read_csv("Census_UHI_US_Urbanized_recalculated.csv")

#income data
tract_income <- read_csv("ACSST5Y2017.S1902-Data.csv") #real data
meta_income <- read_csv("ACSST5Y2017.S1902-Column-Metadata.csv") #meta data

#demographic data
tract_demog <- read_csv("ACSST5Y2017.S0101-Data.csv") #real data
meta_demog <- read_csv("ACSST5Y2017.S0101-Column-Metadata.csv") #meta data

#race data
tract_race <- read_csv("ACSDT5Y2017.B02001-Data.csv") #real data
meta_race <- read_csv("ACSDT5Y2017.B02001-Column-Metadata.csv") #meta data

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


################################################################################
#DATA CLEANSING
#cleansing for sex and age
tract_demog <- tract_demog %>% 
  select(GEO_ID, NAME, S0101_C01_001E, S0101_C03_001E, S0101_C05_001E, 
         S0101_C01_032E, S0101_C02_022E, S0101_C01_002E, S0101_C01_003E,
         S0101_C01_004E, S0101_C01_005E, S0101_C01_006E, S0101_C01_007E,
         S0101_C01_008E, S0101_C01_009E, S0101_C01_010E, S0101_C01_011E,
         S0101_C01_012E, S0101_C01_013E, S0101_C01_014E, S0101_C01_030E) #selecting important information

colnames(tract_demog) <- c("GEO_ID", "NAME", "total", "male", "female", "median_age", "under18",
                           "under5", "age5to9", "age10to14", "age15to19", "age20to24", "age25to29", 
                           "age30to34", "age35to39", "age40to44", "age45to49", "age50to54", "age55to59", 
                           "age60to64", "over64")
tract_demog <- tract_demog[-1, ] #removing first row

  # creating new column for age 18 to 64 years
    sapply(tract_demog, mode)
    tract_demog[, c(3:21)] <- sapply(tract_demog[, c(3:21)], as.numeric)
    str(tract_demog, give.attr = FALSE)
    
    tract_demog <- tract_demog %>%
      mutate(age_0to64 = select(., under5:age60to64) %>% rowSums(na.rm = TRUE))
    
    age_0to64 <- c("age_0to64")
    age_under18 <- c("under18")
    
    tract_demog <- tract_demog %>%
      mutate(age_18to64 = .data[[age_0to64]] - .data[[age_under18]])
    
    tract_demog <- tract_demog %>%
      select(GEO_ID, NAME, total, male, female, median_age, under18, age_18to64, over64)
    
    save(list = "tract_demog", file = "tract_demog_clean.RData")

rm(age_0to64, age_under18)
    

#cleansing for income
tract_income <- tract_income %>% 
  select(GEO_ID, NAME, S1902_C01_019E, S1902_C01_001E) #selecting important information

colnames(tract_income) <- c("GEO_ID", "NAME", "mean_inc_percap", "mean_inc_fam")

tract_income <- tract_income[-1, ] #removing first row

tract_income <- tract_income %>% 
  mutate(census_geoid = substr(GEO_ID, 10, 20)) #converting the number to 11 digit only

save(list = "tract_income", file = "tract_income_clean.RData")


#cleansing for race
tract_race <- tract_race %>%
  select(GEO_ID, NAME, B02001_001E, B02001_002E, B02001_003E, B02001_004E, B02001_005E, B02001_006E,
         B02001_007E, B02001_008E, B02001_009E, B02001_010E) #selecting important information

colnames(tract_race) <- c("GEO_ID", "NAME", "total", "white", "black or african american", "american indian and alaska native",
                          "asian", "native hawaiian and other pacific islander", "some other race",
                          "two or more races", "two races including some other race",
                          "two races excluding some other race, and three or more races")

tract_race <- tract_race[-1, ] #removing first row

tract_race <- tract_race %>% 
  mutate(census_geoid = substr(GEO_ID, 10, 20)) #converting the number to 11 digit only

save(list = "tract_race", file = "tract_race_clean.RData")

#joining age, sex, income, and race


#cleansing for UHI data
uhi_us <- uhi_us[, -1] #removing first column

#calculating the number of digit
uhi_us <- uhi_us %>%
  mutate(census_geoid = as.character(Census_geoid)) #converting to a character

str(uhi_us, give.attr = FALSE)

uhi_us <- uhi_us %>%
  mutate(census_geoid = ifelse(nchar(census_geoid) == 10,
                               gsub("^(.{0})(.*)$","\\10\\2",census_geoid),
                               census_geoid)) #adding 0 to the census_geoid with 10 digit

nchar(uhi_us$census_geoid) #checking the digit

uhi_us <- uhi_us %>%
  mutate(census_geoid = as.numeric(census_geoid))

class(uhi_us$census_geoid)

save(list = "uhi_us", file = "uhi_us_clean.RData")

#joining UHI with age, sex, income, and race


################################################################################
#DATA EXPLORATORY

#Creating a table between UHI and demographic + socioeconomic

################################################################################
#DATA ANALYSIS

#grouping to lower income and higher income

#understanding the differences in races

#understanding the differences in age

#understanding the differences in sex

################################################################################
#DATA MAPPING



################################################################################
#DATA CONCLUSION
