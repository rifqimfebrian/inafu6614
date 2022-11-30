################################################################################
##
## [ PROJ ] Final Project
## [ FILE ] main_final
## [ AUTH ] Muhammad Rifqi Febrian & Agastya Cestyakara
## [ INIT ] Nov 29, 2022
##
################################################################################

#install packages
install.packages("readxl")

#load libraries
library(tidyverse)
library(readxl)

#get directory
getwd()

#open census data
uhi_us <- read_csv("data/Census_UHI_US_Urbanized_recalculated.csv")


#