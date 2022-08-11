# Setup packages
library(readr)
library(janitor)
library(dplyr)
library(zoo)

# Data downloaded from https://covid.saude.gov.br/ on 29/07/2022
# Using read_csv2 beacause the values are separated with ";"
# df_covid <- read_csv2("dados/HIST_PAINEL_COVIDBR_2020_Parte1_26jul2022.csv")
# Reading data shared between multiple files
df_covid <- list.files(path = "dados",
           pattern = "HIST_PAINEL_COVIDBR*", full.names = TRUE) %>%
  lapply(read_csv2) %>%
  bind_rows

# Population data downloaded from https://www.ibge.gov.br/cidades-e-estados on 31/07/2022
# We needed to remove the first line from the file and the last one before
# loading the .csv file
df_populacao <- read_csv("dados/populacao.csv")

# Making the column names easier to work with
df_covid <- clean_names(df_covid)
df_populacao <- clean_names(df_populacao)

# Creating a new dataframe to do the mapping between data
df_uf_estado <- data.frame(
  uf = c("Acre","Alagoas","Amap&aacute;","Amazonas","Bahia","Cear&aacute;","Distrito Federal","Esp&iacute;rito Santo","Goi&aacute;s","Maranh&atilde;o","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Par&aacute;","Para&iacute;ba","Paran&aacute;","Pernambuco","Piau&iacute;","Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rond&ocirc;nia","Roraima","Santa Catarina","S&atilde;o Paulo","Sergipe","Tocantins"),
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
  select(estado, data, casos_novos, obitos_novos) 
# %>%
#   arrange(estado, data) %>%
#   group_by(estado) %>%
#   # Calculating the rolling mean from 07 and 14 days to check the differences
#   # Calculations based only on previous values as it would be unexpected
#   # to have always the future values
#   mutate(casos_media_07 = rollmean(casos_novos, k = 7, fill = 0, align = "right"),
#          casos_media_14 = rollmean(casos_novos, k = 14, fill = 0, align = "right"),
#          obitos_media_07 = rollmean(obitos_novos, k = 7, fill = 0, align = "right"),
#          obitos_media_14 = rollmean(obitos_novos, k = 14, fill = 0, align = "right")) %>%
#   ungroup()

df_covid_filtered <- merge(df_populacao, df_uf_estado, by = "uf") %>%
  rename(
    populacao_estimada_2021 = popula_ccedil_atilde_o_estimada_pessoas_2021,
    area_territorial_2021 = aacute_rea_territorial_km_sup2_2021
  ) %>%
  select(estado, populacao_estimada_2021, area_territorial_2021) %>%
  merge(df_covid_filtered, by = "estado") %>%
  mutate(percentual_casos_populacao = casos_novos / populacao_estimada_2021,
         casos_por_area = casos_novos / area_territorial_2021)


df_covid_filtered %>%
  filter(casos_novos < 0)

write.csv(df_covid_filtered, "dados/covid_df.csv", row.names = FALSE)
