library(tidyverse)
library(tidytext)
library(here)
library(readxl)
library(hrbrthemes)
library(gt)


valdip <- read_excel("valutazioniD.xls", 
                     col_types = c("text", "text", "text", 
                                   "numeric", "text", "numeric", "text"))


valdip %>% 
  filter(escludere=="no") %>% 
  mutate(ruolo= ifelse(`DESCRIZIONE QUALIFICA` == "Dirigente veterinario", "VET", 
                       ifelse(`DESCRIZIONE QUALIFICA` == "Dirigente amministrativo", "AMM", 
                              ifelse(`DESCRIZIONE QUALIFICA` == "Dirigente veterinario a rapp.esclusivo", "VET", "SAN(C/B)")))) %>% 
  filter(ruolo != "AMM") %>%  
 # mutate(TOTf = factor(TOT)) %>%  
  group_by(anno, ruolo, TOT) %>% 
  count() %>%   
  filter(anno == 2021) %>% ungroup() %>% 
  select(- anno) %>% 
  pivot_wider(names_from = "ruolo", values_from = "n", values_fill = 0 ) %>%   View()
  # gt() %>% 
  # gtsave("2020ruolo.rtf")


valdip %>% 
  filter(escludere=="no") %>% 
  mutate(ruolo= ifelse(`DESCRIZIONE QUALIFICA` == "Dirigente veterinario", "VET", 
                       ifelse(`DESCRIZIONE QUALIFICA` == "Dirigente amministrativo", "AMM", 
                              ifelse(`DESCRIZIONE QUALIFICA` == "Dirigente veterinario a rapp.esclusivo", "VET", "SAN(C/B)")))) %>% 
  filter(ruolo != "AMM") %>%  
  group_by(anno, ruolo, Incarico) %>%
  summarise(n = n(), 
            totale = sum(TOT, na.rm = TRUE)) %>%  
  filter(anno == 2020) %>% View()
  select(- anno) %>% 
  pivot_wider(names_from = "ruolo", values_from = "n", values_fill = 0 ) %>%   View()
  gt() %>% 
  gtsave("2021ruolo.rtf")