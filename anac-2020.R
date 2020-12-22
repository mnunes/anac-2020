# obtencao dos dados

base <- "https://www.anac.gov.br/assuntos/dados-e-estatisticas/dados-estatisticos/arquivos/resumo_anual_"

anos <- 2019:2020

for (j in anos){
  download.file(url = paste0(base, j, ".csv"),
                destfile = paste0("dados/resumo_anual_", j, ".csv"))
}



# preparacao dos dados

library(tidyverse)
library(janitor)

preparacao <- function(arquivo){
  
  # leitura
  dados <- read.csv(file = arquivo,
                    sep = ";",
                    fileEncoding="latin1",
                    dec = ",")
  
  # nomes das colunas
  dados <- clean_names(dados)
  
  return(dados)
}

resumo_anual_2019 <- preparacao("dados/resumo_anual_2019.csv")
# retirar dezembro
resumo_anual_2019 <-
  resumo_anual_2019 %>%
  filter(mes != 12)

resumo_anual_2020 <- preparacao("dados/resumo_anual_2020.csv")

dados <- 
  rbind(resumo_anual_2019, resumo_anual_2020) %>%
  mutate(passageiros = passageiros_pagos + passageiros_gratis) %>%
  mutate(ano = as.factor(ano)) %>%
  select(ano, mes, passageiros) %>%
  group_by(ano, mes) %>%
  summarise(passageiros = sum(passageiros, na.rm = TRUE))



# visualizacao

ggplot(dados, aes(x = mes, y = passageiros, colour = ano)) +
  geom_line() +
  labs(x = "Mês", y = "Passageiros", colour = "Ano") +
  scale_x_continuous(breaks = 1:12,
                     labels = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
                                "Jul", "Ago", "Set", "Out", "Nov", "Dez"),
                     minor_breaks = NULL) +
  scale_y_continuous(limits = c(0, 1.2e7), 
                     labels = scales::comma_format()) +
  theme_bw() +
  scale_colour_viridis_d()


