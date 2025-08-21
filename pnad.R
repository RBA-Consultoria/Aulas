if (!require('plotly'))install.packages("plotly");library(plotly) 
if (!require("survey")) install.packages("survey");library(survey) 
if (!require("srvyr")) install.packages("srvyr");library(srvyr) 
if (!require("convey")) install.packages("convey");library(convey)
if (!require("PNADcIBGE")) install.packages("PNADcIBGE");library(PNADcIBGE)
if (!require("forecast")) install.packages("forecast");library(forecast)
if (!require("readstata13")) install.packages("readstata13");library(readstata13) 
if (!require("tidyverse")) install.packages("tidyverse");library(tidyverse) 
if (!require("dplyr")) install.packages("dplyr");library(dplyr) 
if (!require('reshape2'))install.packages("reshape2");library(reshape2)
if (!require('stringi'))install.packages("stringi");library(stringi)
if (!require('htmlwidgets'))install.packages("htmlwidgets");library(htmlwidgets)
if (!require('randomcoloR'))install.packages("randomcoloR");library(randomcoloR)

rm_accent <- function(x) iconv(x, to = "ASCII//TRANSLIT")
setwd("~/GitHub/Dashes/HTMLs")
options(scipen=999)
ay <- list(
  tickfont = list(color = "red"),
  overlaying = "y",
  side = "right",
  title = "Porcentagem",
  color = "red")



df_capitais = data.frame()


for (i in 2012:2022){
  for (q in 1:4){
    
    # trabalhadores -----------------------------------------------------------
    
    options(survey.lonely.psu="certainty")
    dadosPNADc <- get_pnadc(year= i, quarter= q)
    
    dadosPNADc <- update(dadosPNADc, D_Idade = ifelse(coalesce((V2009 >= 14 & V2009 <= 65), FALSE), "Ativa", 'Inativa'))
    
    
    dadosPNADc$variables$Capital <- ifelse(dadosPNADc$variables$Capital=="Município de São Paulo (SP)","São Paulo", "Outros")
    
    
    # capital -----------------------------------------------------------------
    
    df_pnad <- as.data.frame(
      svytotal(~interaction(Ano, Trimestre, Capital, D_Idade == "Ativa" & VD4002 == "Pessoas desocupadas"),
               design=dadosPNADc, na.rm=TRUE))
    df_pnad2 <- as.data.frame(
      svytotal(~interaction(Ano, Trimestre, Capital, D_Idade == "Ativa"),
               design=dadosPNADc, na.rm=TRUE))
    
    
    
    df_pnad$SE <- NULL
    
    df_pnad <- cbind(index = rownames(df_pnad), df_pnad)
    rownames(df_pnad) <- 1:nrow(df_pnad)
    
    df_pnad$index <- str_sub(df_pnad$index, 91, str_length(df_pnad$index))
    
    df_pnad2$SE <- NULL
    
    df_pnad2 <- cbind(index = rownames(df_pnad2), df_pnad2)
    rownames(df_pnad2) <- 1:nrow(df_pnad2)
    
    df_pnad2$index <- str_sub(df_pnad2$index, 57, str_length(df_pnad2$index))
    
    df_pnad$total = df_pnad$total/df_pnad2$total
    
    
    out <- strsplit(df_pnad$index, '[.]')
    c <- as.data.frame(do.call(rbind, out))
    names(c) <- c('ano','quater','Capital','bool_cod')
    df_pnad <- cbind(df_pnad, c)
    
    df_pnad <- df_pnad[df_pnad$bool_cod=='TRUE',]
    df_pnad <- df_pnad[df_pnad$Capital=='São Paulo',]
    
    df_capitais <- rbind(df_capitais, df_pnad)
    
    gc()
  }
}

write.csv(df_capitais, "df_capitais_tx_desoc.csv", row.names = FALSE)
