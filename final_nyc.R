################################################################################
##
## [ PROJ ] Final Project: Disproportionate exposure of UHI in NYC and the effect of NYC cooling policies
## [ FILE ] final_clean
## [ AUTH ] Agastia Cestyakara & Muhammad Rifqi Febrian
## [ INIT ] Nov 29, 2022
## [ DESC ] Understanding the exposure of UHI in NYC and the NYC cooling policies impact
##

################################################################################
#LOADING LIBRARY
library(tidyverse)
library(lmtest)
library(stargazer)
library(sandwich)

################################################################################
#LOADING DATASET
load("clean_data/00_final_uhindvitract.RData")
load("clean_data/00_final_uhindvi_urbanmap.RData")
load("clean_data/00_ny_uhi.RData")
load("clean_data/00_nyuhitree.RData")

################################################################################
#EXPLORATORY DATA
#Dropping the geometry
ny_uhi_exp <- st_set_geometry(ny_uhi_tree, NULL)
save(list = "ny_uhi_exp", file = "clean_data/00_nyuhiexp.RData")
rm(ny_uhi_tree)

summary(ny_uhi_exp$nover64)

ny_uhi_exp %>%
  group_by(highpov) %>%
  summarise(n=n(),
            mean_pov=mean(povt_rate),
            mean_uhi=mean(UHI_summer_day))

ny_uhi_exp %>%
  group_by(shareblack) %>%
  summarise(n=n(),
            mean_pov=mean(povt_rate),
            mean_uhi=mean(UHI_summer_day))

ny_uhi_exp %>%
  group_by(shareover64) %>%
  summarise(n=n(),
            mean_pov=mean(povt_rate),
            mean_uhi=mean(UHI_summer_day))

#Understanding the exposure of UHI in each NYC census tract for specific communities
#High Poverty, Black Population, and Exposure to UHI
#weighted by population
nyc_uhi_1_wtd <-
  tapply(ny_uhi_exp$UHI_summer_day * ny_uhi_exp$total,
         list(ny_uhi_exp$highpov, ny_uhi_exp$shareblack),
         mean) / 
  tapply(ny_uhi_exp$total,
         list(ny_uhi_exp$highpov, ny_uhi_exp$shareblack),
         mean)
nyc_uhi_1_wtd
## Majority Black both in non-high poverty and high-poverty area experience higher UHI
## High poverty area experience higher UHI compared to non-high poverty
## The exposure of UHI in NYC is different with the exposure in US major cities which is higher
## in majority black with higher poverty
## Is that statistically significant? Let's test that
## Using bivariate regression
diff1 <- lm(UHI_summer_day ~ highpov, data = ny_uhi_exp, weights = total)
#diff2 <- lm(UHI_summer_day ~ highpov, data = ny_uhi_exp)
#summary(diff1) #get summary of the model
coeftest(diff1, vcov = vcovHC(diff1, type="HC1")) #get robust SEs
#coeftest(diff2, vcov = vcovHC(diff2, type="HC1")) #get robust SEs
## higher poverty is 0.252 higher than lower poverty in terms of UHI exposure and is statistically significant
## On the other hand, when we did not put weighting, or making all census tract the same, the result is not statistically significant
## We would think that each census tract has difference influence in to the exposure of UHI. Higher population menas higher density
## which could create hgiher UHI.
diff_black <- lm(UHI_summer_day ~ shareblack, data = ny_uhi_exp, weights = total)
summary(diff_black)
coeftest(diff_black, vcov = vcovHC(diff_black, type = "HC1"))

diff_black_q <- lm(UHI_summer_day ~ nblack + I(nblack^2), data = ny_uhi_exp, weights = total)
summary(diff_black_q)
coeftest(diff_black_q, vcov = vcovHC(diff_black_q, type = "HC1"))
#the quadratic form has higher correlation, so we will use that.


#Weighted difference in mean uhi exposure of highpov and nover64
nyc_uhi_2_wtd <-
  tapply(ny_uhi_exp$UHI_summer_day * ny_uhi_exp$total,
         list(ny_uhi_exp$highpov, ny_uhi_exp$shareover64),
         mean) / 
  tapply(ny_uhi_exp$total,
         list(ny_uhi_exp$highpov, ny_uhi_exp$shareover64),
         mean)
nyc_uhi_2_wtd

diff_old_q <- lm(UHI_summer_day ~ nover64 + I(nover64^2), data = ny_uhi_exp, weights = total)
summary(diff_old_q)
coeftest(diff_old_q, vcov = vcovHC(diff_old_q, type = "HC1"))

diff_old <- lm(UHI_summer_day ~ shareover64, data = ny_uhi_exp, weights = total)
summary(diff_old)
coeftest(diff_old, vcov = vcovHC(diff_old, type = "HC1"))

#Weighted difference in mean uhi exposure of nover64 and nblack
nyc_uhi_3_wtd <-
  tapply(ny_uhi_exp$UHI_summer_day * ny_uhi_exp$total,
         list(ny_uhi_exp$shareblack, ny_uhi_exp$shareover64),
         sum) / 
  tapply(ny_uhi_exp$total,
         list(ny_uhi_exp$shareblack, ny_uhi_exp$shareover64),
         sum)
nyc_uhi_3_wtd

#Scatterplot between UHI exposure and poverty
ggplot(data = ny_uhi_tree,
       aes(x=povt_rate, y=UHI_summer_day)) +
  geom_point(alpha = 0.3, color = "blue") + 
  aes(size=total) +
  geom_smooth(method = "lm", formula = y ~ x + x^2) +
  ggtitle('Scatterplot between exposure to UHI and poverty rate') +
  labs(x="poverty rate", y="UHI intensity")

################################################################################
#ANALYSIS DATA
ny_uhi_exp_analysis <- ny_uhi_exp %>%
  mutate(tree_prog = tree_2015 - tree_1995)

summary(ny_uhi_exp_analysis$tree_prog)

ny_uhi_exp_analysis <- ny_uhi_exp_analysis %>%
  mutate(tree_intervention = as.numeric(tree_prog>median(ny_uhi_exp_analysis$tree_prog))) %>%
  mutate(tree_intervention = factor(tree_intervention, levels = c(0,1))) %>%
  mutate(density = total/Shape_Area)

#Exploratory Analysis
ny_uhi_exp_analysis %>%
  group_by(highpov, tree_intervention) %>%
  summarise(n=n(),
            mean_pov=mean(povt_rate),
            mean_uhi=mean(UHI_summer_day))

#Difference in difference of UHI and intervention
nyc_uhi_4_wtd <-
  tapply(ny_uhi_exp_analysis$UHI_summer_day * ny_uhi_exp_analysis$total,
         list(ny_uhi_exp_analysis$tree_intervention, ny_uhi_exp_analysis$highpov),
         mean) / 
  tapply(ny_uhi_exp_analysis$total,
         list(ny_uhi_exp_analysis$tree_intervention, ny_uhi_exp_analysis$highpov),
         mean)
nyc_uhi_4_wtd

nyc_uhi_5_wtd <-
  tapply(ny_uhi_exp_analysis$UHI_summer_day * ny_uhi_exp_analysis$total,
         list(ny_uhi_exp_analysis$tree_intervention, ny_uhi_exp_analysis$shareblack),
         mean) / 
  tapply(ny_uhi_exp_analysis$total,
         list(ny_uhi_exp_analysis$tree_intervention, ny_uhi_exp_analysis$shareblack),
         mean)
nyc_uhi_5_wtd

#Is the intervention statistically significant in high poverty area and share of black?
