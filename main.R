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
library(readxl)

#get directory
getwd()

################################################################################
#DATA UPLOADING
#open census data
uhi_us <- read_csv("Census_UHI_US_Urbanized_recalculated.csv")
tract_income <- read_csv("ACSST5Y2017.S1902-Data.csv")
tract_demog <- read_csv("ACSST5Y2017.S0101-Data.csv")

################################################################################
#DATA CLEANSING
#renaming column for age and sex


#renaming column for income


#renaming column for race


#joining age, sex, income, and race


#filter necessary UHI variable


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
