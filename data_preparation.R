# Setup packages
library(readr)
library(janitor)
library(dplyr)

# Data downloaded from https://covid.saude.gov.br/ on 29/07/2022
# Using read_csv2 beacause the values are separated with ";"
df <- read_csv2("dados/HIST_PAINEL_COVIDBR_2020_Parte1_26jul2022.csv")

# Making the column names easier to work with
df <- clean_names(df)

# tail(df)

# Checking to see why there are values without state
df %>%
  filter(is.na(estado)) %>%
  distinct(coduf)

# Maybe it is because is a consolidate data from all of Brazil
df %>%
  filter(coduf == 76) %>%
  distinct(estado, populacao_tcu2019)

# Getting only states data about new COVID cases
df_filtered <- df %>%
  filter(!is.na(estado)) %>%
  select(estado, data, casos_novos)

write.csv(df_filtered, "dados/covid_df.csv", row.names = FALSE)
