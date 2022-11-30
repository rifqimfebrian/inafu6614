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

#open census data
uhi_us <- read_csv("Census_UHI_US_Urbanized_recalculated.csv")
tract_income <- read_csv("ACSST5Y2017.S1902-Data.csv")
tract_demog <- read_csv("ACSST5Y2017.S0101-Data.csv")



#