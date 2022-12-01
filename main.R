################################################################################
##
## [ PROJ ] Final Project
## [ FILE ] main_final
## [ AUTH ] Muhammad Rifqi Febrian & Agastya Cestyakara
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


#tract demographic
str(tract_demog, give.attr = FALSE)

#tract races
str(tract_race, give.attr = FALSE)


################################################################################
#DATA CLEANSING
#cleansing for sex and age


#cleansing for income


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
