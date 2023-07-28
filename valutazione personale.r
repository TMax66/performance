library(tidyverse)
library(tidytext)
library(here)
library(readxl)
library(hrbrthemes)
library(gt)
library(ggcorset)
library(readr)

 


# 1. carico i dati dal file excel estratto dall'applicativo obiettivistrategici....

dt <- read_excel("valutazioni.xlsx", 
                          col_types = c("text", "text", "text", 
                                        "text", "numeric", "numeric"))

valutazioni <- dt %>% 
  rename("Anno" = ANNO ) %>% 
  mutate(categoria = ifelse(is.na(CATEGORIA), "DIRIGENZA", "COMPARTO" ))
         
  



valutazioni %>% 
  filter(Anno == 2022) %>%  
  mutate(VALUTATOMtr = parse_number(VALUTATO)) %>%  
  select( VALUTATOMtr, categoria) %>%  
  unique() %>% 
  group_by(categoria) %>% 
  count()


#2. tabella complessiva della distribuzione dei punteggi di tutto il personale in diversi anni
valutazioni %>%
   group_by(Anno) %>% 
   summarise(min = min(TOT,na.rm=T), 
                       "25°percentile" = quantile(TOT, 0.25, na.rm = T), 
                       "mediana" = median(TOT,na.rm=T), 
                       "75°percentile" = quantile(TOT, 0.75, na.rm=T),
                       max = max(TOT,na.rm=T), 
             media = mean(TOT,na.rm = T), 
             ds = sd(TOT, na.rm = T), 
             n = n()) %>%   
  gt() %>% 
  fmt_number(columns = 3:8, 
             decimals = 1) 

#2.1 tabella dirigenti

valutazioni %>%
  filter(categoria == "DIRIGENZA") %>% 
  group_by(Anno) %>% 
  summarise(min = min(TOT,na.rm=T), 
            "25°percentile" = quantile(TOT, 0.25, na.rm = T), 
            "mediana" = median(TOT,na.rm=T), 
            "75°percentile" = quantile(TOT, 0.75, na.rm=T),
            max = max(TOT,na.rm=T), 
            media = mean(TOT,na.rm = T), 
            ds = sd(TOT, na.rm = T), 
            n = n()) %>%   
  gt() %>% 
  fmt_number(columns = 3:8, 
             decimals = 1) 



#2.2 tabella comparto

valutazioni %>%
  filter(categoria == "COMPARTO") %>% 
  group_by(Anno) %>% 
  summarise(min = min(TOT,na.rm=T), 
            "25°percentile" = quantile(TOT, 0.25, na.rm = T), 
            "mediana" = median(TOT,na.rm=T), 
            "75°percentile" = quantile(TOT, 0.75, na.rm=T),
            max = max(TOT,na.rm=T), 
            media = mean(TOT,na.rm = T), 
            ds = sd(TOT, na.rm = T), 
            n = n()) %>%   
  gt() %>% 
  fmt_number(columns = 3:8, 
             decimals = 1) 


  
# 3. grafico complessivo 
ptot <- valutazioni %>% 
   filter(Anno == 2022) %>% 
   ggplot(aes(x = TOT))+
   geom_histogram(bins = 50, col= "grey", fill= "steelblue")+
  # geom_density()+
  xlim(50,100)+
   labs(x= "valutazione", y= "n.personale IZSLER", 
        subtitle = "Distribuzione della valutazione del personale IZSLER anno 2022 ")+
   theme_ipsum_rc()+
   geom_vline(aes(xintercept = median(TOT, na.rm = TRUE)), color = "red")+
   geom_text(aes(x = 80, y = 100, label = paste("Mediana = ", round(median(TOT, na.rm = TRUE),2))))



#4. grafico dirigenti
   
pdir <-valutazioni %>% 
   filter(Anno == 2022) %>% 
   filter(categoria == "DIRIGENZA") %>% 
   ggplot(aes(x = TOT))+
   geom_histogram(bins = 50, col= "grey", fill= "steelblue")+
   geom_vline(aes(xintercept = median(TOT)), color = "red")+
  xlim(50,100)+
   geom_text(aes(x = 80, y = 15, label = paste("Mediana = ", 97)))+
   labs(x= "valutazione", y= "n. personale dirigente", 
        subtitle = "Distribuzione della valutazione del personale dirigente anno 2022")+
   theme_ipsum_rc()

#5. grafico comparto
pcomp <- valutazioni %>% 
   filter(Anno== 2022,  
          categoria == "COMPARTO") %>%   
   ggplot(aes(x = TOT))+
   geom_histogram(bins = 50, col= "grey", fill= "steelblue")+
   geom_text(aes(x = 80, y = 100, label = paste("Mediana = ", 98)))+
   geom_vline(aes(xintercept = median(TOT, na.rm = TRUE)), color = "red")+
  xlim(50, 100)+
   labs(x= "valutazione", y= "n. personale di comparto", 
        subtitle = "Distribuzione della valutazione del personale di comparto anno 2022")+
   theme_ipsum_rc()

library(patchwork)
ptot/
  (pdir|pcomp)


valutazioni %>% 
  ggplot(
    aes(x = Anno,
        y = TOT, 
        group = Anno)
  )+
  geom_violin()+
  geom_jitter(alpha = 0.1)+
  stat_summary(fun = "median", color = "red")+
  facet_wrap(~ categoria, scales = "free")+
  theme_ipsum_rc()+
  labs(x= "Anno", y= "valutazione in punti %")
  
  






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
  mutate(total = rowSums(across(where(is.numeric)))) %>%  View()
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
  mutate(total = rowSums(across(where(is.numeric)))) %>%  View()
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



######grafico variazione punteggio per individuo######
valdip <- read_excel("personale.xls", 
                    col_types = c("text", "text", "text", 
                                  "text", "numeric", "numeric"))


strutture <- read_excel("strutture.xlsx")



dati20_21 <- valdip %>% 
  filter(ANNO > 2019)


dtwide <- dati20_21 %>% 
mutate(dirigenza = ifelse(is.na(CATEGORIA), "DIRIGENZA", "COMPARTO")) %>%   
  pivot_wider(names_from = "ANNO", values_from = "TOT") %>% 
  mutate(change = `2021`-`2020`,  
  direction  = ifelse( change<0,"Diminuzione",
                                ifelse( change>0,"Aumento","Invariato")
                       ),
 direction = factor(direction, 
                              levels = c("Aumento","Invariato","Diminuzione"))) %>%   
  filter(!is.na(direction) & dirigenza == "DIRIGENZA" ) %>% View()
  # group_by(direction) %>% 
  # count()
 
  

  gg_corset(., y_var1 = "2020", y_var2 = "2021", c_var = "direction", group="VALUTATO", faceted = T)+
  theme_ggcorset()+
  scale_x_discrete(labels = c("2020","2021"))+ labs(x = "", y = "valutazione")
  

  


# group_by(direction) %>% 
# count()
#  

