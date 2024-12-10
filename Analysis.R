library(tidyverse)
library(haven)
library(ggplot2)
library(readxl)
library(tidycensus)
library(sf) 
library(viridis)
library(dplyr)

#cleaning data

#removing rows with missing values


source(
  "scripts/load_and_clean_data.R",
  echo = FALSE 
)