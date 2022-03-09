library(tidyverse)
library(tidytext)
library(here)
library(readxl)
library(hrbrthemes)
library(gt)

 



































# valdip <- read_excel("PROGRAMMAZIONE/Relazione Performances 2020/VALUTAZIONE DIPENDENTI ANNO 2020 - NVP  SEDUTA DEL 12.03.2021.xls", 
#                      col_types = c("text", "text", "text", 
#                                    "text", "numeric", "numeric"))
#    
#    
   
   
# valdip %>% 
#    mutate(categ = cut(TOT, breaks = c(quantile(TOT, probs = c(0,0.10, 0.25, 0.5, 0.75, 1))), include.lowest = TRUE)) %>% 
#    ggplot(aes(x=categ))+
#    geom_bar()
 

valdip <- read_excel("personale.xls", 
                        col_types = c("text", "text", "text", 
                                      "text", "numeric", "numeric"))


strutture <- read_excel("strutture.xlsx")



dati21 <- valdip %>% 
  filter(ANNO == 2021)


valdip <- dati21 %>% 
  mutate(REPARTO = recode(REPARTO,
                          "S.T. PIACENZA E PARMA -" = "SEDE TERRITORIALE DI PIACENZA - PARMA",
                          "S.T. PIACENZA E PARMA - - S.T. PARMA" = "SEDE TERRITORIALE DI PIACENZA - PARMA", 
                          "REP. CHIM. DEGLI ALIMENTI E MANGIMI -" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI",
                          "REP. CHIM. DEGLI ALIMENTI E MANGIMI - LABORATORIO CHIMICA APPLICATA ALLE TECNOLOGIE ALIMENTARI" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI", 
                          "REP. CHIM. DEGLI ALIMENTI E MANGIMI - LABORATORIO CONTAMINATI AMBIENTALI BS" = "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI", 
                          "REP. CHIMICO ALIMENTI BOLOGNA -" = "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)",
                          "REP. CHIMICO ALIMENTI BOLOGNA - LABORATORIO CONTAMINANTI AMBIENTALI BOLOGNA" = "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)", 
                          "REP. PRODUZIONE PRIMARIA -" = "REPARTO PRODUZIONE PRIMARIA", 
                          "REP. PRODUZIONE PRIMARIA - LABORATORIO BATTERIOLOGIA SPECIALIZZATA" = "REPARTO PRODUZIONE PRIMARIA", 
                          "REP. PRODUZIONE PRIMARIA - LABORATORIO DI VIROLOGIA E SIEROLOGIA SPECIALIZZATA E MICROSCOPIA ELETTRONICA" = "REPARTO PRODUZIONE PRIMARIA",
                          "S.T. BOLOGNA, FERRARA E MODENA -" = "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA",
                          "S.T. BOLOGNA, FERRARA E MODENA - - S.T. FERRARA" = "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA", 
                          "S.T. BOLOGNA, FERRARA E MODENA - - S.T. MODENA" = "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA", 
                          "S.T. REGGIO EMILIA -" = "SEDE TERRITORIALE DI REGGIO EMILIA",
                          "REP. VIROLOGIA -" = "REPARTO VIROLOGIA",
                          "REP. VIROLOGIA - LABORATORIO DI VIROLOGIA E SIEROLOGIA SPECIALIZZATA E MICROSCOPIA ELETTRONICA" = "REPARTO VIROLOGIA",
                          "REP. VIROLOGIA - LABORATORIO PROTEOMICA E DIAGNOSTICA TSE" = "REPARTO VIROLOGIA",
                          "REP. VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE -" = "REPARTO VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE",
                          "REPARTO CONTROLLO ALIMENTI - LABORATORIO DIAGNOSTICA MOLECOLARE OGM" = "REPARTO VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE",
                          "S.T. BERGAMO, SONDRIO E BINAGO -" = "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO",
                          "S.T. BERGAMO, SONDRIO E BINAGO - - S.T. BINAGO" = "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO", 
                          "S.T. BERGAMO, SONDRIO E BINAGO - - S.T. SONDRIO" = "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO", 
                          "S.T. BRESCIA -" = "SEDE TERRITORIALE DI BRESCIA",
                          "S.T. CREMONA, MANTOVA -" = "SEDE TERRITORIALE DI CREMONA - MANTOVA",
                          "S.T. CREMONA, MANTOVA - - S.T. MANTOVA" = "SEDE TERRITORIALE DI CREMONA - MANTOVA",
                          "S.T. FORLI' E RAVENNA -" = "SEDE TERRITORIALE DI FORLÌ - RAVENNA",
                          "S.T. FORLI' E RAVENNA - - S.T. FORLI'" = "SEDE TERRITORIALE DI FORLÌ - RAVENNA", 
                          "S.T. FORLI' E RAVENNA - - S.T. RAVENNA" = "SEDE TERRITORIALE DI FORLÌ - RAVENNA",
                          "S.T. LODI E MILANO -" = "SEDE TERRITORIALE DI LODI - MILANO",
                          "S.T. LODI E MILANO - - S.T. LODI" = "SEDE TERRITORIALE DI LODI - MILANO",
                          "S.T. LODI E MILANO - - S.T. MILANO" = "SEDE TERRITORIALE DI LODI - MILANO",
                          "REPARTO CONTROLLO ALIMENTI -" = "REPARTO CONTROLLO ALIMENTI", 
                          "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO -" = "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO", 
                          "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO - LABORATORIO PRODUZIONE TERRENI" = "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO",
                          "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO - REPARTO SUBSTRATI CELLULARI E IMMUNOLOGIA CELLULARE" = "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO",
                          "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE -" = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE", 
                          "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE - LABORATORIO BATTERIOLOGIA SPECIALIZZATA" = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE",
                          "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE - LABORATORIO DIAGNOSTICA MOLECOLARE OGM" = "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE",
                          "S.T. PAVIA -" = "SEDE TERRITORIALE DI PAVIA",
                          "S.T. PAVIA - - S.T. MILANO" = "SEDE TERRITORIALE DI PAVIA", 
                          "ANALISI DEL RISCHIO ED EPIDEMIOLOGIA GENOMICA -" = "ANALISI DEL RISCHIO ED EPIDEMIOLOGIA GENOMICA", 
                          "SORVEGLIANZA EPIDEMIOLOGICA -" = "SORVEGLIANZA EPIDEMIOLOGICA",
                          "SORVEGLIANZA EPIDEMIOLOGICA - SORVEGLIANZA EPIDEMIOLOGICA" = "SORVEGLIANZA EPIDEMIOLOGICA",
                          "U.O. PROVV. ECONOMATO E VENDITE -" = "UO PROVVEDITORATO ECONOMATO E VENDITE",
                          "U.O. PROVV. ECONOMATO E VENDITE - U.O. PROVV. ECONOMATO E VENDITE" = "UO PROVVEDITORATO ECONOMATO E VENDITE",
                          "SERVIZIO ASSICURAZIONE QUALITA -" = "SERVIZIO ASSICURAZIONE QUALITA'",
                          "U.O. AFFARI GENERALI E LEGALI -" = "U.O. AFFARI GENERALI E LEGALI",
                          "U.O. TECNICO PATRIMONIALE -" = "UO TECNICO PATRIMONIALE",
                          "U.O. TECNICO PATRIMONIALE - U.O. TECNICO PATRIMONIALE" = "UO TECNICO PATRIMONIALE", 
                          "U.O. GESTIONE RISORSE UMANE E SVILUPPO COMPETENZE -" = "U.O. GESTIONE RISORSE UMANE E SVILUPPO COMPETENZE",
                          "U.O. GESTIONE SERVIZI CONTABILI" = "U.O. GESTIONE SERVIZI CONTABILI",
                          "U.O. GESTIONE SERVIZI CONTABILI - U.O. GESTIONE SERVIZI CONTABILI" = "U.O. GESTIONE SERVIZI CONTABILI",
                          "PROGRAMMAZIONE DEI SERVIZI TECNICI E CONTROLLO DI GESTIONE -" = "Programmazione dei servizi tecnici e controllo di gestione",
                          "FORMAZIONE -" =  "FORMAZIONE E BIBLIOTECA",
                          "SISTEMI INFORMATIVI - SISTEMI INFORMATIVI" = "Programmazione dei servizi tecnici e controllo di gestione",
                          "CONTROLLO DI GESTIONE -" = "Programmazione dei servizi tecnici e controllo di gestione", 
                          "SEGRETERIA DIREZIONALE -" = "DIREZIONE GENERALE",
                          "GESTIONE CENTRALIZZATA DELLE RICHIESTE DELL'UTENZA -" = "GESTIONE CENTRALIZZATA DELLE RICHIESTE", 
                          "GESTIONE CENTRALIZZATA DELLE RICHIESTE DELL'UTENZA - U.O. PROVV. ECONOMATO E VENDITE" = "GESTIONE CENTRALIZZATA DELLE RICHIESTE"
  )
  
  )  %>% 
  left_join(
    
    (strutture %>% 
       select(Dipartimento, Reparto) %>%
       unique()),  by = c("REPARTO" = "Reparto"))  



valdip %>% 
   mutate(dirigenza = ifelse(is.na(CATEGORIA), "DIRIGENZA", "COMPARTO")) %>%  
   filter(dirigenza == "COMPARTO") %>% 
   group_by(ANNO) %>% 
   summarise(min = min(TOT,na.rm=T), 
                       "25°percentile" = quantile(TOT, 0.25, na.rm = T), 
                       "mediana" = median(TOT,na.rm=T), 
                       "75°percentile" = quantile(TOT, 0.75, na.rm=T),
                       max = max(TOT,na.rm=T)) %>% 
  gt() %>% 
  gtsave("tc.rtf")



   kbl(booktabs = TRUE, caption = "Distribuzione dei punteggi di valutazione del personale IZSLER nel triennio 2019-2021") %>% 
   kable_styling()


   
   
   



ptot <- valdip %>% 
   filter(ANNO == 2021) %>% 
   ggplot(aes(x = TOT))+
   geom_histogram(bins = 10, col= "grey", fill= "steelblue")+
   labs(x= "valutazione", y= "n.personale IZSLER", 
        subtitle = "Valutazione del personale IZSLER anno 2021 ")+
   theme_ipsum_rc()+
   geom_vline(aes(xintercept = median(TOT, na.rm = TRUE)), color = "red")+
   geom_text(aes(x = median(TOT, na.rm =TRUE ), y = 200, label = paste("Mediana = ", round(median(TOT, na.rm = TRUE),2))))
   
pdir <-valdip %>% 
   filter(ANNO == 2021) %>% 
   mutate(dirigenza = ifelse(is.na(CATEGORIA), "DIRIGENZA", "COMPARTO")) %>%  
   filter(dirigenza == "DIRIGENZA") %>% 
   ggplot(aes(x = TOT))+
   geom_histogram(bins = 10, col= "grey", fill= "steelblue")+
   geom_vline(aes(xintercept = median(TOT)), color = "red")+
   geom_text(aes(x = 97, y = 28, label = paste("Mediana = ", 97)))+
   labs(x= "valutazione", y= "n. personale dirigente", 
        subtitle = "Valutazione personale della dirigenza")+
   theme_ipsum_rc()

pcomp <- valdip %>% 
   filter(ANNO== 2021) %>% 
   mutate(dirigenza = ifelse(is.na(CATEGORIA), "DIRIGENZA", "COMPARTO")) %>%  
   filter(dirigenza == "COMPARTO") %>% 
   ggplot(aes(x = TOT))+
   geom_histogram(bins = 11, col= "grey", fill= "steelblue")+
   geom_text(aes(x = 98, y = 100, label = paste("Mediana = ", 98)))+
   geom_vline(aes(xintercept = median(TOT, na.rm = TRUE)), color = "red")+
   labs(x= "valutazione", y= "n. personale di comparto", 
        subtitle = "Valutazione personale del comparto")+
   theme_ipsum_rc()

library(patchwork)
ptot/
  (pdir|pcomp)



valdip %>% 
  filter(ANNO== 2021) %>% 
  mutate(dirigenza = ifelse(is.na(CATEGORIA), "DIRIGENZA", "COMPARTO")) %>% 
  group_by(dirigenza) %>% 
  count()



valdip %>% 
  mutate(dirigenza = ifelse(is.na(CATEGORIA), "DIRIGENZA", "COMPARTO")) %>%  
  filter(dirigenza == "DIRIGENZA"  & ANNO %in% c(2019, 2020)) %>% 
  mutate(score = ifelse(TOT >= 95, " da >= 95 a 100", 
                        ifelse(TOT > 70 & TOT < 95, " da > 70 a <95", "fino a 70"))) %>% 
  group_by(ANNO, score) %>% 
  count() %>% View()
  


###score by dipartimento 2021---
library(openxlsx)
valdip %>% 
  mutate(dirigenza = ifelse(is.na(CATEGORIA), "DIRIGENZA", "COMPARTO")) %>%  
  filter(dirigenza == "DIRIGENZA"  & ANNO %in% c(2021)) %>% 
  mutate(score = ifelse(TOT >90, " da >90 a 100", 
                        ifelse(TOT > 80 & TOT <= 90, " da > 80 a 90",
                               ifelse(TOT > 70 & TOT <= 80, " da >70 a 80", 
                                      ifelse(TOT > 60 & TOT <= 70, "da >60 a 70", 
                                             ifelse(TOT > 50 & TOT <= 60, "da > 50 a 60", "da 0 a 50")))))) %>% 
  group_by(Dipartimento, score) %>% 
  count() %>%  
  pivot_wider(names_from = score, values_from = n, values_fill = 0) %>% 
  rowwise() %>% 
  mutate(total = rowSums(across(where(is.numeric)))) %>%  
  write.xlsx("scorebydip.xls")
  






valdip %>% 
  mutate(dirigenza = ifelse(is.na(CATEGORIA), "DIRIGENZA", "COMPARTO")) %>%  
  filter(dirigenza == "DIRIGENZA"  & ANNO %in% c(2021)) %>% 
  mutate(TOTf = factor(TOT)) %>% 
  group_by(TOTf) %>% 
  count() %>% View()
  

##COMPARTO



valdip %>% 
  mutate(dirigenza = ifelse(is.na(CATEGORIA), "DIRIGENZA", "COMPARTO")) %>%  
  filter(dirigenza == "COMPARTO"  & ANNO %in% c(2021)) %>% 
  mutate(score = ifelse(TOT >95, " da >95 a 100", 
                        ifelse(TOT > 85 & TOT <= 95, " da > 85 a 95",
                               ifelse(TOT > 75 & TOT <= 85, " da >75 a 85", 
                                      ifelse(TOT > 65 & TOT <= 75, "da >65 a 75", 
                                             ifelse(TOT >= 60 & TOT <= 65, "da >= 60 a 65", 
                                                    ifelse(TOT < 60, "<60",  "da 0 a < 50"))))))) %>% 
  group_by(Dipartimento, score) %>% 
  count() %>% 
  pivot_wider(names_from = score, values_from = n, values_fill = 0) %>% 
  rowwise() %>% 
  mutate(total = rowSums(across(where(is.numeric)))) %>%  
  write.xlsx("scorebydipc.xls")








valdip %>% 
  mutate(dirigenza = ifelse(is.na(CATEGORIA), "DIRIGENZA", "COMPARTO")) %>%  
  filter(dirigenza == "COMPARTO"  & ANNO %in% c(2021)) %>% 
  mutate(TOTf = factor(TOT)) %>% 
  group_by(TOTf) %>% 
  count() %>%
  gt() %>% 
  gtsave("comp.rtf")



valdip %>% 
  mutate(dirigenza = ifelse(is.na(CATEGORIA), "DIRIGENZA", "COMPARTO")) %>%  
  filter(dirigenza == "COMPARTO"  & ANNO %in% c(2019, 2020)) %>% 
  mutate(score = ifelse(TOT >= 95, " da >= 95 a 100", 
                        ifelse(TOT > 50 & TOT < 95, " da > 50 a <95", "fino a 50"))) %>% 
  group_by(ANNO, score) %>% 
  count() %>% View()
