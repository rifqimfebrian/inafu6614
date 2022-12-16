################################################################################
##
## [ PROJ ] Final Project: Disproportionate Exposure of UHI to US Urban Major Cities
## [ FILE ] final_uscities
## [ AUTH ] Agastia Cestyakara & Muhammad Rifqi Febrian
## [ INIT ] Nov 29, 2022
## [ DESC ] Understanding the exposure of UHI to different vulnerbale communities in US major cities
##

################################################################################
#Loading library
library(tidyverse)
library(sf)
library(stargazer)
library(tmap)
library(lmtest)

#DATA LOADING

load("clean_data/00_final_uhindvitract.RData")
load("clean_data/00_final_uhindvi_urbanmap.RData")

################################################################################
#DISPROPORTIONATE EXPOSURE OF UHI 
#DATA EXPLORATORY
#summary(uhindvitract$DEM_urb)

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

boxplot_uhi_highpov
boxplot_uhi_shareblack
boxplot_uhi_sharefemale
boxplot_uhi_over64

ggplot(boxplot_uhi_highpov, aes(x=type, y = UHI_summer_day)) +
  geom_boxplot()

ggplot(boxplot_uhi_shareblack, aes(x=type, y = UHI_summer_day)) +
  geom_boxplot()

ggplot(boxplot_uhi_over64, aes(x=type, y = UHI_summer_day)) +
  geom_boxplot()

ggplot(boxplot_uhi_sharefemale, aes(x=type, y = UHI_summer_day)) +
  geom_boxplot()

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
            black = sum(black_or_african_american),
            asian_pacificislander = sum(asian_nativehawaiaan_pacific_islander),
            black_or_african_american = sum(black_or_african_american),
            asian_nativehawaiaan_pacific_islander = sum(asian_nativehawaiaan_pacific_islander),
            other = sum(other),
            UHI_mean = mean(UHI_summer_day),
            NDVI_mean = mean(NDVI_urb_summer),
            DEM_mean = mean(DEM_urb),
            povt_rate = mean(povt_rate),
            shareblack = mean(nblack),
            sharefemale = mean(nfemale),
            shareover64 = mean(nover64)) %>%
##  sharefemale = mean(nfemale) %>%
  arrange(desc(UHI_mean))

urban_data_20 <- urban_data %>%
  ungroup() %>%
  slice(1:20) %>%
  select(GEOID10, Urban_name, total, white, black, asian_pacificislander, other, UHI_mean, NDVI_mean,
         DEM_mean, povt_rate, shareblack, sharefemale, shareover64)

urban_data_20 %>%
  kbl(caption = "20 cities with highest UHI intensity") %>%
  kable_classic_2(full_width = F)

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
  geom_point(aes(size = total, color = "green")) + #aes(size=total) if we want to weight it
  geom_smooth(method = "lm", formula = y ~ x + x^2) +
  ggtitle('Scatterplot between exposure to UHI and poverty rate') +
  labs(x="poverty rate", y="UHI intensity")
#NOTE: Increase in poverty rate is associated with an increase in exposure of UHI

ggplot(data = uhindvitract,
       aes(x=povt_rate, y=UHI_summer_day)) +
  geom_point(aes(size = total)) + #aes(size=total) if we want to weight it
  geom_smooth(method = "lm", formula = y ~ x) +
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
summary_diff1 <- summary(diff1)
summary_diff1$coefficient[2,1]


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
  geom_smooth(method = "lm", formula = y ~ x) +
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

t1_uhi_wtd_df <- as.data.frame(t1_uhi_wtd)

t1_uhi_wtd_df <- t1_uhi_wtd_df %>%
  mutate(diff = t1_uhi_wtd_df[,2] - t1_uhi_wtd_df[,1])

new_row_t1 <- c(t1_uhi_wtd_df[2,1] - t1_uhi_wtd_df[1,1], t1_uhi_wtd_df[2,2] - t1_uhi_wtd_df[1,2], t1_uhi_wtd_df[2,3] - t1_uhi_wtd_df[1,3])
t1_uhi_wtd_df <- rbind(t1_uhi_wtd_df, new_row_t1)
rownames(t1_uhi_wtd_df) <- c("Not high poverty", "high poverty", "diff")
t1_uhi_wtd_df

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

#shareblack == 0: linear model with station observations
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

t1_uhiagerace_df <- as.data.frame(t1_uhiagerace_wtd)

t1_uhiagerace_df <- t1_uhiagerace_df %>%
  mutate(diff = t1_uhiagerace_df[,2] - t1_uhiagerace_df[,1])

new_row_t1uhiagerace <- c(t1_uhiagerace_df[2,1] - t1_uhiagerace_df[1,1], t1_uhiagerace_df[2,2] - t1_uhiagerace_df[1,2], t1_uhiagerace_df[2,3] - t1_uhiagerace_df[1,3])
t1_uhiagerace_df <- rbind(t1_uhiagerace_df, new_row_t1uhiagerace)
rownames(t1_uhiagerace_df) <- c("Majority non-Black", "Majority Black", "diff")
t1_uhiagerace_df


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

#shareblack == 1: linear model with station observations
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

t1_uhifemalerace_df <- as.data.frame(t1_uhifemalerace_wtd)

t1_uhifemalerace_df <- t1_uhifemalerace_df %>%
  mutate(diff = t1_uhifemalerace_df[,2] - t1_uhifemalerace_df[,1])

new_row_t1femalerace <- c(t1_uhifemalerace_df[2,1] - t1_uhifemalerace_df[1,1], t1_uhifemalerace_df[2,2] - t1_uhifemalerace_df[1,2], t1_uhifemalerace_df[2,3] - t1_uhifemalerace_df[1,3])
t1_uhifemalerace_df <- rbind(t1_uhifemalerace_df, new_row_t1femalerace)
rownames(t1_uhifemalerace_df) <- c("Majority non-Black", "Majority Black", "diff")
t1_uhifemalerace_df

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
#Understanding disproportionate exposure in details
#Taking New York--Newark Data
NY_uhi <- uhindvitract %>%
  filter(GEOID10 == "63217") %>%
  select(census_geoid, Urban_name, Urban_name_extended, NDVI_urb_summer, total,
         UHI_summer_day, DEM_urb, povt_rate, nblack, nfemale, nover64, GEOID10)

#loading data about NY NJ and CT
ny_ct <- st_read(dsn = "raw_data/shpfile/USA_newyork/tl_2017_36_tract.shp")
nj_ct <- st_read(dsn = "raw_data/shpfile/USA_newjersey/tl_2017_34_tract.shp")
ct_ct <- st_read(dsn = "raw_data/shpfile/USA_connecticut/tl_2017_09_tract.shp")

#Binding NY NJ and CT
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

nynjctuhi <- nynjctuhi %>%
  mutate(highpov = as.numeric(povt_rate > median(nynjctuhi$povt_rate)),
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

save(list = "nynjctuhi", file = "clean_data/00_nynjctuhi.Rdata")

#Mapping NY and NJ
t1_ny <- tm_shape(nynjctuhi) + 
  tm_polygons("UHI_summer_day")
t1_ny
t1_ny_pvt <- tm_shape(nynjctuhi) +
  tm_polygons("povt_rate")
t1_ny_pvt
t1_ny_black <- tm_shape(nynjctuhi) +
  tm_polygons("nblack")
t1_ny_black

#Exposure to UHI for different communities in NYC
ggplot(data = NY_uhi,
       aes(x=povt_rate, y=UHI_summer_day)) +
  geom_point(aes(size = total, color = "green")) + #aes(size=total) if we want to weight it
  geom_smooth(method = "lm", formula = y ~ x + x^2) +
  ggtitle('Scatterplot between exposure to UHI and poverty rate') +
  labs(x="poverty rate", y="UHI intensity")

help("ggplot")

NY_uhi %>%
  group_by(highpov) %>%
  summarise(n=n(),
            mean_pov=mean(povt_rate),
            mean_uhi=mean(UHI_summer_day))

t.test(UHI_summer_day ~ highpov, data = uhindvitract, var.equal = FALSE)


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

linear.ndvi <- lm(UHI_summer_day ~ NDVI_urb_summer, data = uhindvitract, weights = total)
linear.ndvi2 <- lm(UHI_summer_day ~ NDVI_urb_summer + DEM_urb, data = uhindvitract, 
                   weights = total)
my_models_2 <- list(linear.ndvi, linear.ndvi2)

stargazer(my_models_2,
          title = ("Results"),
          type = "text")


################################################################################
#DATA MAPPING
#important load
load("uhindvitract.RData")

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
#t2 <- tm_shape(uhindvi_urban_map) + 
#  tm_fill('UHI_mean') +  #tm_polygon is tm_fill and tm_borders together
#  tm_borders() + 
#  tm_legend(outside = TRUE)

#t2
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


save(list = "neworleansuhi", file = "neworleansuhi.RData")

str(neworleansuhi, give.attr = FALSE)

neworleansuhi = st_as_sf(neworleansuhi)

tmaptools::palette_explorer()

install.packages("shinyjs")
library(shinyjs)

t1_no <- tm_shape(neworleansuhi) + 
  tm_polygons("UHI_summer_day", palette = "OrRd") # +
#tm_symbols(col = "blue", size = "nblack", scale = 0.5)

t1_no

t1_no_2 <- tm_shape(neworleansuhi) + 
  tm_polygons(c("UHI_summer_day", "nblack", "povt_rate", "nover64")) +
  tm_facets(sync = TRUE, ncol = 2) # +
#tm_symbols(col = "blue", size = "nblack", scale = 0.5)

t1_no_2

t1_no <- tm_shape(neworleansuhi) + tm_polygons("UHI_summer_day")

t1_no
t1_no_pvt <- tm_shape(neworleansuhi) +
  tm_polygons("povt_rate")

t1_no_pvt

t1_no_black <- tm_shape(neworleansuhi) +
  tm_polygons(col = "nblack")

tm_polygons("nblack")

t1_no_black


#Taking New Hampsire
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

t1_nh_2 <- tm_shape(nhct_uhi) + 
  tm_polygons(c("UHI_summer_day", "nblack", "povt_rate", "nover64"))

t1_nh_2

t1_ny_pvt <- tm_shape(nynjctuhi) +
  tm_polygons("povt_rate")

t1_ny_pvt

t1_ny_black <- tm_shape(nynjctuhi) +
  tm_polygons("nblack")

t1_ny_black