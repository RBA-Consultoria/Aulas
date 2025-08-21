if(!require("tidygeocoder")) install.packages("tidygeocoder");library(tidygeocoder) 
if(!require("cepR")) install.packages("cepR");library(cepR) 
if(!require("dplyr")) install.packages("dplyr");library(dplyr) 
if(!require("readr")) install.packages("readr");library(readr) 
if(!require("jsonlite")) install.packages("jsonlite");library(jsonlite) 
if(!require("arrow")) install.packages("arrow");library(arrow)
if(!require("stringr")) install.packages("stringr");library(stringr)
if(!require('haven'))install.packages("haven");library(haven)


RAIS_est <- read_dta("~/GitHub/AULAS/Dados/Estb2023_MSP.dta")
RAIS_est <- data.frame(RAIS_est)

# remove space front all chars
RAIS_est$endereco = gsub("^\\s+", "", RAIS_est$endereco)

# standard cep
RAIS_est$cep = gsub(" ", "", RAIS_est$cep)
# if str len is 7, add 0 at the beginning
RAIS_est$cep = ifelse(nchar(RAIS_est$cep) == 7, 
                      paste0("0", RAIS_est$cep), 
                      RAIS_est$cep)
#add - to cep
RAIS_est$cep = gsub("(\\d{5})(\\d{3})", "\\1-\\2", RAIS_est$cep)

RAIS_est$full_endereco = paste0((RAIS_est$endereco),", ",
                                (RAIS_est$cep),", São Paulo, SP - Brasil")   

# --- 4. DIVIDIR OS DADOS EM 4 PARTES ---
# Define o número de partes
num_partes <- num_trabalhadores <- 100

# Cria uma coluna de grupo, dividindo as linhas em 4 grupos
RAIS_est <- RAIS_est %>%
  mutate(grupo = rep(1:num_partes, length.out = n()))

# Divide o dataframe em uma lista de 4 dataframes, um para cada grupo
lista_dfs <- RAIS_est %>%
  group_split(grupo, .keep = FALSE) # .keep = FALSE remove a coluna 'grupo'

funcao <- function(x, temp) {
  
  temp <- temp[[x]]
  gc()
  # savee lista df
  
  out <- as.data.frame(temp) |>
    geocode(address = full_endereco, method = "arcgis", full_results = TRUE)
  
  write_csv(out, paste0("RAIS_est_geocoded",x,".csv"))
  
  gc()
}

for (i in 1:100) {
  funcao(i,lista_dfs)
}

