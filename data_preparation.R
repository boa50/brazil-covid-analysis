# Setup packages
library(readr)
library(janitor)
library(dplyr)
library(zoo)

# Data downloaded from https://covid.saude.gov.br/ on 29/07/2022
# Using read_csv2 beacause the values are separated with ";"
df_covid <- read_csv2("dados/HIST_PAINEL_COVIDBR_2020_Parte1_26jul2022.csv")

# Population data downloaded from https://www.ibge.gov.br/cidades-e-estados on 31/07/2022
# We needed to remove the first line from the file and the last one before
# loading the .csv file
df_populacao <- read_csv("dados/populacao.csv")

# Making the column names easier to work with
df_covid <- clean_names(df_covid)
df_populacao <- clean_names(df_populacao)

# Creating a new dataframe to do the mapping between data
df_uf_estado <- data.frame(
  uf = c("Acre","Alagoas","Amap&aacute","Amazonas","Bahia","Cear&aacute","Distrito Federal","Esp&iacute","Goi&aacute","Maranh&atilde","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Par&aacute","Para&iacute","Paran&aacute","Pernambuco","Piau&iacute","Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rond&ocirc","Roraima","Santa Catarina","S&atilde","Sergipe","Tocantins"),
  estado = c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA","MT","MS","MG","PA","PB","PR","PE","PI","RJ","RN","RS","RO","RR","SC","SP","SE","TO")
)

# Checking to see why there are values without state
df_covid %>%
  filter(is.na(estado)) %>%
  distinct(coduf)

# Maybe it is because is a consolidate data from all of Brazil
df_covid %>%
  filter(coduf == 76) %>%
  distinct(estado, populacao_tcu2019)

# Getting only states data about new COVID cases
df_covid_filtered <- df_covid %>%
  # Filtering of data distributed by city so we don't double the values
  filter(!is.na(estado) & is.na(codmun)) %>%
  select(estado, data, casos_novos, obitos_novos) %>%
  arrange(estado, data) %>%
  group_by(estado) %>%
  # Calculating the rolling mean from 07 and 14 days to check the differences
  # Calculations based only on previous values as it would be unexpected
  # to have always the future values
  mutate(casos_media_07 = rollmean(casos_novos, k = 7, fill = 0, align = "right"),
         casos_media_14 = rollmean(casos_novos, k = 14, fill = 0, align = "right"),
         obitos_media_07 = rollmean(obitos_novos, k = 7, fill = 0, align = "right"),
         obitos_media_14 = rollmean(obitos_novos, k = 14, fill = 0, align = "right")) %>%
  ungroup()

write.csv(df_covid_filtered, "dados/covid_df.csv", row.names = FALSE)
