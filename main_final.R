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
library(forcats)
library(fastDummies)
library(SciViews)
library(weights)
library(lmtest)
library(sandwich)
library(Weighted.Desc.Stat)
library(kableExtra)
library(stargazer)
library(sf)
library(ggplot2)
library(tidyverse)
library(tmap)
library("spData")
library("rgdal")

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

#poverty data
tract_poverty <- read_csv("ACSST5Y2017.S1701-Data.csv") #real data
meta_poverty <- read_csv("ACSST5Y2017.S1701-Column-Metadata.csv") #meta data

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
tract_income[, c(3:4)] <- sapply(tract_demog[, c(3:4)], as.numeric) #converting as numeric
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

#cleansing for UHI data
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

save(list = "uhi_us", file = "uhi_us_clean.RData")

uhi_us_census <- uhi_us %>%
  select(census_geoid)

#loading data
load("tract_demog_clean.RData")
load("tract_income_clean.RData")
load("tract_race_clean.RData")
load("tract_poverty_clean.RData")
load("uhi_us_clean.RData")

#joining tract_demog and tract_income (age, sex, and income)
str(tract_demog, give.attr = FALSE)
str(tract_income, give.attr = FALSE)
tract_income <- tract_income %>%
  select(-NAME) #excluding name
tract_demog_income <- full_join(tract_demog,tract_income, by = "GEO_ID")

#joining tract_demog_income and tract_poverty (age, sex, income, and poverty)
str(tract_demog_income, give.attr = FALSE)
tract_demog_income <- tract_demog_income %>%
  select(-census_geoid) #excluding census_geoid
str(tract_poverty, give.attr = FALSE)
tract_poverty <- tract_poverty %>%
  select(-NAME) #excluding name
tract_demog_sec <- full_join(tract_demog_income,tract_poverty, by = "GEO_ID")
tract_demog_sec <- tract_demog_sec %>%
  select(-mean_inc_percap, -mean_inc_fam)

#joining tract_demog_income and tract_race
#str(tract_demog_income, give.attr = FALSE)
#str(tract_race, give.attr = FALSE)

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

save(list = "tract_race", file = "tract_race_clean.RData")
save(list = "tract_demog_sec", file = "tract_demog_sec_clean.RData")
save(list = "tract_demog_sec_race", file = "tract_demog_sec_race_clean.RData")

# load data
load("tract_demog_clean.RData")
#load("tract_income_clean.RData")
load("tract_race_clean.RData")
load("uhi_us_clean.RData")
load("tract_demog_sec_race_clean.RData")
load("tract_demog_sec_clean.RData")

#selecting column in uhi_us data
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
save(list = "uhi_ndvi_tract", file = "uhi_ndvi_tract_clean.RData")

#Last loading all data
load("uhi_ndvi_tract_clean.RData")

#Last join
#joining tract_demog_sec with tract_race
#tract_demog_final <- full_join(tract_demog_sec, tract_race, by = c("GEO_ID", "NAME", "census_geoid", "total"))
#save(list = "tract_demog_final", file = "tract_demog_final.RData")

#joining tract_demog_final and uhi_ndvi
#uhi_ndvi_tract_final <- inner_join(uhi_ndvi, tract_demog_final, by = "census_geoid")

################################################################################
#Last Inspection
#checking the NA
summary(uhi_ndvi_tract) #checking whether we has weird data
#We find several questions and doubts
#1. Why is there minus NDVI?
#2. What is total and urban count?
#3. Why is there minus UHI?

table(sign(uhi_ndvi_tract$UHI_summer_day))
# There are 42510 positive UHI
# There are 13130 census tract that has negative value which is interesting
# QUESTION: How some census tract could has cooler areas compared to its rural surroundings?

#removing negative values
uhindvi_tract_duplicate <- uhi_ndvi_tract
uhindvi_tract_duplicate <- uhindvi_tract_duplicate %>%
  mutate(UHI_summer_day_clean = ifelse(UHI_summer_day <= 0, NA, UHI_summer_day))
summary(uhindvi_tract_duplicate$UHI_summer_day_clean) #no negative values for now
uhindvi_tract_final <- na.omit(uhindvi_tract_duplicate)
save(list = "uhindvi_tract_final", file = "uhindvi_tract_final.RData")

#Categorizing data and making it final
uhindvi_tract <- uhindvi_tract_final %>%
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

save(list = "uhindvi_tract", file = "uhindvi_tract.RData")
save(list = "uhindvitract", file = "uhindvitract.RData")

str(uhindvitract, give.attr = FALSE)

library(stargazer)

stargazer(as.data.frame(uhindvitract), 
          type = "text",
          title= ("Table Summary"))

rm(meta_demog)
rm(meta_income)
rm(meta_poverty)
rm(meta_race)

rm(tract_demog)
rm(tract_demog_final)
rm(tract_demog_income)
rm(tract_demog_sec)
rm(tract_demog_sec_race)
rm(tract_income)
rm(tract_poverty)
rm(tract_race)

rm(uhi_us)
rm(uhi_us_census)
rm(uhindvi_tract)
rm(uhi_ndvi_tract_final)
rm(uhindvi_tract_duplicate)
rm(uhi_ndvi)
rm(uhindvi_tract_final)
rm(uhindvi_tract_final)
rm(uhi_ndvi_tract)

################################################################################
#DATA EXPLORATORY

#Visualizing distribution of UHI with histogram
#ggplot(data = uhi_ndvi_tract, aes(x = UHI_annual_day)) + geom_histogram()
#ggplot(data = uhi_ndvi_tract, aes(x = UHI_summer_night)) + geom_histogram()

ggplot(data = uhindvitract, aes(x = UHI_summer_day)) + geom_histogram()
ggplot(data = uhindvitract, aes(x = NDVI_urb_summer)) + geom_histogram()
#NOTES: we are using the data of summer (June, July, August) and day because it is the data which shows most 
#pronounced. Moreover, the mean temperatures are generally higher than other periods through the year.

#boxplot
boxplot_uhi_highpov <- uhindvitract %>%
  select(UHI_summer_day, highpov) %>%
  mutate(type = highpov) %>%
  select(-highpov)
boxplot_uhi_shareblack <- uhindvitract %>%
  select(UHI_summer_day, shareblack) %>%
  mutate(type = shareblack) %>%
  select(-shareblack)
boxplot_uhi_sharefemale <- uhindvitract %>%
  select(UHI_summer_day, sharefemale) %>%
  mutate(type = sharefemale) %>%
  select(-sharefemale)
boxplot_uhi_over64 <- uhindvitract %>%
  select(UHI_summer_day, shareover64) %>%
  mutate(type = shareover64) %>%
  select(-shareover64)
boxplot_uhi <- rbind(boxplot_uhi_highpov, boxplot_uhi_shareblack, boxplot_uhi_sharefemale, boxplot_uhi_over64)

ggplot(boxplot_uhi, aes(x=type, y = UHI_summer_day)) +
  geom_boxplot()

#boxplotndvi
boxplot_ndvi_highpov <- uhindvitract %>%
  select(NDVI_urb_summer, highpov) %>%
  mutate(type = highpov) %>%
  select(-highpov)
boxplot_ndvi_shareblack <- uhindvitract %>%
  select(NDVI_urb_summer, shareblack) %>%
  mutate(type = shareblack) %>%
  select(-shareblack)
boxplot_ndvi_sharefemale <- uhindvitract %>%
  select(NDVI_urb_summer, sharefemale) %>%
  mutate(type = sharefemale) %>%
  select(-sharefemale)
boxplot_ndvi_over64 <- uhindvitract %>%
  select(NDVI_urb_summer, shareover64) %>%
  mutate(type = shareover64) %>%
  select(-shareover64)
boxplot_ndvi <- rbind(boxplot_ndvi_highpov, boxplot_ndvi_shareblack, boxplot_ndvi_sharefemale, boxplot_ndvi_over64)

rm(boxplot_ndvi_highpov)
rm(boxplot_ndvi_over64)
rm(boxplot_ndvi_shareblack)
rm(boxplot_ndvi_sharefemale)

ggplot(boxplot_ndvi, aes(x=type, y = NDVI_urb_summer)) +
  geom_boxplot()

#top 20 cities in terms of urban heat island intensity
#cleanig urban data
uhindviurban <- uhindvitract %>%
  mutate(GEOID10 = Urban_geoid) %>%
  mutate(GEOID10 = as.character(GEOID10)) %>%
  mutate(GEOID10 = ifelse(nchar(GEOID10) == 3,
                          gsub("^(.{0})(.*)$","\\100\\2", GEOID10),
                          GEOID10)) %>%
  mutate(GEOID10 = ifelse(nchar(GEOID10) == 4,
                          gsub("^(.{0})(.*)$","\\10\\2", GEOID10),
                          GEOID10))

uhindviurban_picked <- uhindviurban %>%
  select(Urban_name, Urban_name_extended, NDVI_urb_summer, total,
         UHI_summer_day, DEM_urb, povt_rate, nblack, nfemale, nover64, GEOID10,
         white, black_or_african_american, asian_nativehawaiaan_pacific_islander, other)

urban_data <- uhindviurban_picked %>%
  group_by(GEOID10, Urban_name, Urban_name_extended) %>%
  summarise(total = sum(total),
            white = sum(white),
            black_or_african_american = sum(black_or_african_american),
            asian_nativehawaiaan_pacific_islander = sum(asian_nativehawaiaan_pacific_islander),
            other = sum(other),
            UHI_mean = mean(UHI_summer_day),
            NDVI_mean = mean(NDVI_urb_summer),
            DEM_mean = mean(DEM_urb),
            povt_rate = mean(povt_rate),
            shareblack = mean(nblack),
            sharefemale = mean(nfemale)) %>%
  arrange(desc(UHI_mean))

urban_data_20 <- urban_data %>%
  ungroup() %>%
  slice(1:20)

uhi_race_top_long <- gather(urban_data_20, race, n_race, white:other,factor_key=TRUE) # Converting data from wide to long format

ggplot(uhi_race_top_long,
       aes(x=GEOID10, y= n_race, fill = race))+
  geom_bar(stat = "identity") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1))  +
  xlab("Top 20 tracts with highest UHI") +
  ylab("Total population") +
  scale_fill_discrete(name = "Race") +
  ggtitle("Distribution of UHI by race",
          subtitle = "At top 20 tracts accross the US")

################################################################################
#DATA ANALYSIS - UHI exposure with type of demographics

#Understanding the association between exposure to UHI and poverty rate
#Scatterplot between exposure to UHI and Poverty rate
ggplot(data = uhindvitract,
       aes(x=povt_rate, y=UHI_summer_day)) +
  geom_point(aes(size = total)) + #aes(size=total) if we want to weight it
  geom_smooth(method = "lm", formula = y ~ x + x^2) +
  ggtitle('Scatterplot between exposure to UHI and poverty rate') +
  labs(x="poverty rate", y="UHI intensity")
#NOTE: Increase in poverty rate is associated with an increase in exposure of UHI

#Fit linear OLS model
ols1l <- lm(UHI_summer_day ~ povt_rate,
            data = uhindvitract, weight = total)
summary(ols1l)
coeftest(ols1l, vcov = vcovHC(ols1l, type = "HC1"))

#Fit quadratic OLS model
ols1q <- lm(UHI_summer_day ~ povt_rate + I(povt_rate^2),
            data = uhindvitract, weight = total)
summary(ols1q) 
coeftest(ols1q, vcov = vcovHC(ols1q, type="HC1"))

#understanding the differences of exposure to heat between high and low poverty area
#calculating and testing difference in means between high/low poverty census tract
uhindvitract %>%
  group_by(highpov) %>%
  summarise(n=n(),
            mean_pov=mean(povt_rate),
            mean_uhi=mean(UHI_summer_day))
t.test(UHI_summer_day ~ highpov, data = uhindvitract, var.equal = FALSE)
#using bivariate regression
diff1 <- lm(UHI_summer_day ~ highpov, data = uhindvitract, weights = total)
summary(diff1) #get summary of the model
coeftest(diff1, vcov = vcovHC(diff1, type="HC1")) #get robust SEs

#Association between high poverty and UHI
ggplot(data = uhindvitract,
       aes(x=highpov, y=UHI_summer_day)) +
  geom_point(aes(size = total)) + #aes(size=total) if we want to weight it
  geom_smooth(method = "lm", formula = y ~ x) +
  ggtitle('Scatterplot between exposure to UHI and high poverty') +
  labs(x="high poverty", y="UHI intensity")


###############################################################################
#DATA ANALYSIS - Understanding races and exposure to UHI
#understanding the differences in races
ggplot(data = uhindvitract,
       aes(x=nblack, y=UHI_summer_day)) +
  geom_point(aes(size=total)) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2)) +
  ggtitle('Scatterplot between exposure to UHI and black share') +
  labs(x="black share", y="UHI intensity")

#Weighted difference in mean uhi exposure of highpov and nblack
t1_uhi_wtd <-
  tapply(uhindvitract$UHI_summer_day * uhindvitract$total,
         list(uhindvitract$highpov, uhindvitract$shareblack),
         sum) / 
  tapply(uhindvitract$total,
         list(uhindvitract$highpov, uhindvitract$shareblack),
         sum)
t1_uhi_wtd

t1_pvt_wtd <-
  tapply(uhindvitract$povt_rate * uhindvitract$total,
         list(uhindvitract$highpov, uhindvitract$shareblack),
         sum) / 
  tapply(uhindvitract$total,
         list(uhindvitract$highpov, uhindvitract$shareblack),
         sum)
t1_pvt_wtd

#get separate data frames by predominantly Black stations to estimate separate models
ct_black <- uhindvitract %>% filter(shareblack == "Majority Black")
ct_nonblack <- uhindvitract %>% filter(shareblack == "Majority non-Black")

#shareblack == 1: linear model with station observations
ols_b_l <- lm(UHI_summer_day ~ povt_rate, 
              data = ct_black, weights = total)
summary(ols_b_l)
coeftest(ols_b_l, vcov = vcovHC(ols_b_l, type = "HC1"))

#shareblack == 0: quadratic model with station observations
ols_nb_l <- lm(UHI_summer_day ~ povt_rate, 
               data = ct_nonblack, weights = total)
summary(ols_nb_l)
coeftest(ols_nb_l, vcov = vcovHC(ols_nb_l, type = "HC1"))

#shareblack == 1: quadratic model with station observations
ols_b_q <- lm(UHI_summer_day ~ povt_rate + I(povt_rate^2), 
              data = ct_black, weights = total)
summary(ols_b_q)
coeftest(ols_b_q, vcov = vcovHC(ols_b_q, type = "HC1"))

#shareblack == 0: quadratic model with station observations
ols_nb_q <- lm(UHI_summer_day ~ povt_rate + I(povt_rate^2), 
               data = ct_nonblack, weights = total)
summary(ols_nb_q)
coeftest(ols_nb_q, vcov = vcovHC(ols_nb_q, type = "HC1"))

#scatterplot quadratic
ggplot(uhindvitract, aes(x = povt_rate, y = UHI_summer_day, color = shareblack)) +
  geom_point(aes(size = total))  +
  geom_smooth(method = 'lm', formula = y ~ x + I(x^2)) + 
  ylab("Exposure to UHI intensity") + xlab("poverty rate") +
  ggtitle("Exposure to UHI Intensity vs poverty by race", 
          subtitle = "census tract (2017)") +
  scale_color_discrete(name = "Predominantly Black Census Tract",
                       labels=c("No", "Yes"),
                       guide = guide_legend(reverse=TRUE)) +
  theme(legend.position = "bottom", 
        legend.background = element_rect(color = "black", fill = "grey90", 
                                         size = .2, linetype = "solid"), 
        legend.direction = "horizontal",
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 8))

################################################################################
#DATA ANALYSIS - Understanding age and exposure to UHI
#understanding the differences in age
ggplot(data = uhindvitract,
       aes(x=nover64, y=UHI_summer_day)) +
  geom_point(aes(size=total)) +
  geom_smooth(method = "lm", formula = y ~ x) +
  ggtitle('Scatterplot between exposure to UHI and age over 64') +
  labs(x="age group over 64", y="UHI intensity")

#Weighted difference in mean uhi exposure of highpov and nover64
t1_uhiage_wtd <-
  tapply(uhindvitract$UHI_summer_day * uhindvitract$total,
         list(uhindvitract$highpov, uhindvitract$shareover64),
         sum) / 
  tapply(uhindvitract$total,
         list(uhindvitract$highpov, uhindvitract$shareover64),
         sum)
t1_uhiage_wtd

#Weighted difference in mean uhi exposure of nover64 and nblack
t1_uhiagerace_wtd <-
  tapply(uhindvitract$UHI_summer_day * uhindvitract$total,
         list(uhindvitract$shareblack, uhindvitract$shareover64),
         sum) / 
  tapply(uhindvitract$total,
         list(uhindvitract$shareblack, uhindvitract$shareover64),
         sum)
t1_uhiagerace_wtd

#get separate data frames by predominantly Black stations to estimate separate models
#shareblack == 1: quadratic model with station observations
ols_b64_q <- lm(UHI_summer_day ~ nover64, 
                data = ct_black, weights = total)
summary(ols_b64_q)
coeftest(ols_b64_q, vcov = vcovHC(ols_b64_q, type = "HC1"))
#nblack == 0: quadratic model with station observations
ols_nb64_q <- lm(UHI_summer_day ~ nover64, 
                 data = ct_nonblack, weights = total)
summary(ols_nb64_q)
coeftest(ols_nb64_q, vcov = vcovHC(ols_nb64_q, type = "HC1"))

################################################################################
#DATA ANALYSIS - Understanding sex and exposure to UHI
#understanding the differences in sex
ggplot(data = uhindvitract,
       aes(x=nfemale, y=UHI_summer_day)) +
  geom_point(aes(size=total)) +
  geom_smooth(method = "lm", formula = y ~ x) +
  ggtitle('Scatterplot between exposure to UHI and census tract concentrated female') +
  labs(x="female proportion", y="UHI intensity")

#Weighted difference in mean uhi exposure of highpov and nfemale
t1_uhifemale_wtd <-
  tapply(uhindvitract$UHI_summer_day * uhindvitract$total,
         list(uhindvitract$highpov, uhindvitract$sharefemale),
         sum) / 
  tapply(uhindvitract$total,
         list(uhindvitract$highpov, uhindvitract$sharefemale),
         sum)
t1_uhifemale_wtd

#Weighted difference in mean uhi exposure of nfemale and nblack
t1_uhifemalerace_wtd <-
  tapply(uhindvitract$UHI_summer_day * uhindvitract$total,
         list(uhindvitract$shareblack, uhindvitract$sharefemale),
         sum) / 
  tapply(uhindvitract$total,
         list(uhindvitract$shareblack, uhindvitract$sharefemale),
         sum)
t1_uhifemalerace_wtd

#shareblack == 1: quadratic model with station observations
ols_bf_q <- lm(UHI_summer_day ~ nfemale, 
               data = ct_black, weights = total)
summary(ols_bf_q)
coeftest(ols_bf_q, vcov = vcovHC(ols_bf_q, type = "HC1"))
#nblack == 0: quadratic model with station observations
ols_nbf_q <- lm(UHI_summer_day ~ nfemale, 
                data = ct_nonblack, weights = total)
summary(ols_nbf_q)
coeftest(ols_nbf_q, vcov = vcovHC(ols_nbf_q, type = "HC1"))


################################################################################
#Data Analysis - Multivariate Regression
uhi_mult <- lm(UHI_summer_day ~ highpov + shareblack + sharefemale + shareover64 +
                 highpov*shareblack + highpov*sharefemale + highpov*shareover64 +
                 shareblack*sharefemale + shareblack*shareover64 + sharefemale*shareover64 +
                 highpov*shareblack*sharefemale + highpov*shareblack*shareover64 +
                 shareblack*sharefemale*shareover64 + highpov*shareblack*sharefemale*shareover64,
               data = uhindvitract, weights = total)
summary(uhi_mult) #get summary of the model
coeftest(uhi_mult, vcov = vcovHC(uhi_mult, type="HC1")) #get robust SEs

#Using stargazer OLS Model
linear.1 <- lm(UHI_summer_day ~ highpov, data = uhindvitract, weights = total)
linear.2 <- lm(UHI_summer_day ~ highpov + shareblack + highpov*shareblack, data = uhindvitract, 
               weights = total)
linear.3 <- lm(UHI_summer_day ~ highpov + shareblack + sharefemale + highpov*shareblack + highpov*sharefemale +
                 shareblack*sharefemale + highpov*shareblack*sharefemale,
               data = uhindvitract, weights = total)
linear.4 <- lm(UHI_summer_day ~ highpov + shareblack + sharefemale + shareover64 +
                 highpov*shareblack + highpov*sharefemale + highpov*shareover64 +
                 shareblack*sharefemale + shareblack*shareover64 + sharefemale*shareover64 +
                 highpov*shareblack*sharefemale + highpov*shareblack*shareover64 +
                 shareblack*sharefemale*shareover64 + highpov*shareblack*sharefemale*shareover64,
               data = uhindvitract, weights = total)
my_models <- list(linear.1, linear.2, linear.3, linear.4)

stargazer(my_models,
          title = ("Results"),
          type = "text")

################################################################################
#DATA ANALYSIS - UHI exposure and NDVI
#scatterplot between the UHI and NDVI
ggplot(uhindvitract, aes(x = NDVI_urb_summer, y = UHI_summer_day)) +
  geom_point(aes(size = total))  +
  geom_smooth(method = 'lm', formula = y ~ x) + 
  ylab("Exposure to UHI intensity") + xlab("Vegetation index") +
  ggtitle("Exposure to UHI Intensity vs vegatation index", 
          subtitle = "census tract (2017)") +
  theme(legend.position = "bottom", 
        legend.background = element_rect(color = "black", fill = "grey90", 
                                         size = .2, linetype = "solid"), 
        legend.direction = "horizontal",
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 8))

#Fit OLS model
olsuhi_ndvi <- lm(UHI_summer_day ~ NDVI_urb_summer,
                 data = uhindvitract, weight = total)
summary(olsuhi_ndvi) 
coeftest(olsuhi_ndvi, vcov = vcovHC(olsuhi_ndvi, type="HC1"))

#Adding DEM to ols model
olsuhi_ndvidem <- lm(UHI_summer_day ~ NDVI_urb_summer + DEM_urb,
                 data = uhindvitract, weight = total)
summary(olsuhi_ndvidem) 
coeftest(olsuhi_ndvidem, vcov = vcovHC(olsuhi_ndvidem, type="HC1"))

################################################################################
#DATA MAPPING
#important load
loading("uhindvitract.RData")

usa <- st_read(dsn = 'USA_shp/gadm36_USA_0.shp')
usa_1 <- st_read(dsn = "USA_shp/gadm36_USA_1.shp")
usa_2 <- st_read(dsn = 'USA_shp/gadm36_USA_2.shp')
usa_urban <- st_read(dsn = "USA_urban/tl_2017_us_uac10.shp")

uhindvitract <- uhindvitract %>%
  mutate(GEOID10 = Urban_geoid) %>%
  mutate(GEOID10 = as.character(GEOID10)) %>%
  mutate(GEOID10 = ifelse(nchar(GEOID10) == 3,
                          gsub("^(.{0})(.*)$","\\100\\2", GEOID10),
                          GEOID10)) %>%
  mutate(GEOID10 = ifelse(nchar(GEOID10) == 4,
                          gsub("^(.{0})(.*)$","\\10\\2", GEOID10),
                          GEOID10))

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
save(list = "uhindvitract", file = "uhindvitract.RData")
save(list = "uhindvitract_map", file = "uhindvitract_map.RData")
save(list = "uhindvi_urban_map", file = "uhindvi_urban_map.RData")


#Creating the map (TOO LARGE)
t2 <- tm_shape(uhindvi_urban_map) + 
  tm_fill('UHI_mean') +  #tm_polygon is tm_fill and tm_borders together
  tm_borders() + 
  tm_legend(outside = TRUE)

t2

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

str(neworleansuhi, give.attr = FALSE)

neworleansuhi = st_as_sf(neworleansuhi)

t1_no <- tm_shape(neworleansuhi) + 
  tm_polygons("UHI_summer_day")

t1_no

t1_no_pvt <- tm_shape(neworleansuhi) +
  tm_polygons("povt_rate")

t1_no_pvt

t1_no_black <- tm_shape(neworleansuhi) +
  tm_polygons("nblack")

t1_no_black

#Taking New York--Newark Data
NY_uhi <- uhindvitract %>%
  filter(GEOID10 == "63217") %>%
  select(census_geoid, Urban_name, Urban_name_extended, NDVI_urb_summer, total,
         UHI_summer_day, DEM_urb, povt_rate, nblack, nfemale, nover64, GEOID10)

ny_ct <- st_read(dsn = "USA_newyork/tl_2017_36_tract.shp")
nj_ct <- st_read(dsn = "USA_newjersey/tl_2017_34_tract.shp")
ct_ct <- st_read(dsn = "USA_connecticut/tl_2017_09_tract.shp")

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

t1_ny <- tm_shape(nynjctuhi) + 
  tm_polygons("UHI_summer_day")

t1_ny

t1_ny_pvt <- tm_shape(nynjctuhi) +
  tm_polygons("povt_rate")

t1_ny_pvt

t1_ny_black <- tm_shape(nynjctuhi) +
  tm_polygons("nblack")

t1_ny_black



################################################################################
#DATA CONCLUSION



#write down the regression function
#likely we will have several groups

#scatterplot each of this thing, and make it in one slide
#put it in a grid, look at how to arrange plot

#think how to put in my model
#Splitting th income, race, and sex

