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

#poverty data
tract_poverty <- read_csv("ACSST5Y2017.S1701-Data.csv") #real data
meta_poverty <- read_csv("ACSST5Y2017.S1701-Column-Metadata.csv") #meta data

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

#tract poverty
str(tract_poverty, give.attr = FALSE)

#tract demographic
str(tract_demog, give.attr = FALSE)

#tract races
str(tract_race, give.attr = FALSE)


################################################################################
#DATA CLEANSING
#cleansing for sex and age
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

  # creating new column for age 18 to 64 years
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
    
    save(list = "tract_demog", file = "tract_demog_clean.RData")

rm(age_0to64, age_under18)
        

#cleansing for income
tract_income <- tract_income %>% 
  select(GEO_ID, NAME, S1902_C01_019E, S1902_C01_001E) #selecting important information

colnames(tract_income) <- c("GEO_ID", "NAME", "mean_inc_percap", "mean_inc_fam")

tract_income <- tract_income[-1, ] #removing first row

sapply(tract_income, mode)
tract_income[, c(3:4)] <- sapply(tract_income[, c(3:4)], as.numeric) #converting as numeric
str(tract_income, give.attr = FALSE)

tract_income <- tract_income %>% 
  mutate(census_geoid = substr(GEO_ID, 10, 20)) #converting the number to 11 digit only

save(list = "tract_income", file = "tract_income_clean.RData")


#cleansing for poverty
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

save(list = "tract_poverty", file = "tract_poverty_clean.RData")


#cleansing for race
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

class(uhi_us$census_geoid)

save(list = "uhi_us", file = "uhi_us_clean.RData")

uhi_us_census <- uhi_us %>%
  select(census_geoid)

#removing previous data
#rm(meta_demog)
#rm(meta_income)
#rm(meta_race)
#rm(tract_demog)
#rm(tract_income)
#rm(tract_poverty)
#rm(tract_race)
#rm(uhi_us)
#rm(uhi_us_census)
#rm(tract_demog_income)
#rm(tract_demog_income_race)

#loading data
load("tract_demog_clean.RData")
load("tract_income_clean.RData")
load("tract_poverty_clean.RData")
load("tract_race_clean.RData")
load("uhi_us_clean.RData")

#joining tract_demog and tract_income (age, sex, and income)
str(tract_demog, give.attr = FALSE)
str(tract_income, give.attr = FALSE)
tract_income <- tract_income %>%
  select(-NAME) #excluding name
tract_demog_income <- full_join(tract_demog,tract_income, by = "GEO_ID")

#joining tract_demog_income and tract_race
str(tract_demog_income, give.attr = FALSE)
str(tract_race, give.attr = FALSE)

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

tract_demog_income_race <- full_join(tract_demog_income, tract_race, by = c("census_geoid", "GEO_ID", "NAME", "total"))

save(list = "tract_demog_income", file = "tract_democ_income_clean.RData")
save(list = "tract_demog_income_race", file = "tract_democ_income_race_clean.RData")

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



#write down the regression function
#likely we will have several groups

#scatterplot each of this thing, and make it in one slide
#put it in a grid, look at how to arrange plot

#think how to put in my model
#Splitting th income, race, and sex
