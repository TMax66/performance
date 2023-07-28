library(showtext)
font_paths() 
font_files()
font_add("Calibri", "calibri.ttf")
font_families()
showtext_auto() 

library(tidyverse)
library(stringr)

getwd()
#setwd("C:/Users/andrea.boscarino/Desktop/docs IZSLER/PERFORMANCE 2021")

perf <- readRDS("performance.RDS")

#MACROAREA PER DIPARTIMENTO

#periodo 4
#forzare avanzamento=1
#(cambiare avanzamento=0.5)
#se valore=0 forzare valore=target

dt <- perf %>%
  filter(Periodo == 4) %>%
  mutate(Avanzamento = ifelse(Avanzamento == 0 | Avanzamento == 0.5, 1, Avanzamento), Valore = ifelse(Valore == 0, Target, Valore))

#togliere indicatori OBIETTIVI RICERCATORI CAT. D
dt <- dt %>% 
  filter(!str_detect(Indicatore,"OBIETTIVI RICERCATORI"))

dt<-tibble::rowid_to_column(dt, "index") #inserita colonna id
#View(dt)

dt %>%
  mutate(var_percen=(Valore-Target)/Target*100) %>%
  select(index,var_percen,Valore,Target,Reparto, Indicatore, everything()) %>%
  View()

#modifiche Elena a valore e target di indicatori con var_percen anomala
dt[126,3]<-1
dt[143,3]<-15 # =15 outlier
dt[174,3]<-1
dt[175,3]<-3
dt[112,3]<-3
dt[145,3]<-6
dt[172,3]<-8
dt[2,3]<-7
dt[8,3]<-66 
dt[149,3]<-5
dt[149,4]<-5
dt[150,3]<-7
dt[150,4]<-5
dt[157,3]<-22
dt[157,4]<-4
dt[160,3]<-3
dt[160,4]<-1
dt[161,3]<-3
dt[161,4]<-1
dt[167,3]<-17
dt[206,3]<-25
#SISTEMI INFORMATIVI - modifica valore obiettivi_vedi mail elena
dt[179,3]<-70
dt[180,3]<-21

dt %>%
  mutate(var_percen=(Valore-Target)/Target*100) %>%
  select(index,var_percen,Valore,Target,Reparto, Indicatore, everything()) %>%
  View()

aggdata <- dt %>%
  mutate(MacroArea=as.factor(MacroArea), Dipartimento=as.factor(Dipartimento)) %>%
  group_by(MacroArea, Dipartimento) %>%
  summarise(var_media = round(mean((Valore-Target)/Target*100,na.rm = T),0)) %>% 
  ungroup()
View(aggdata)


levels(aggdata$MacroArea)
aggdata$MacroArea <- recode_factor(aggdata$MacroArea,
"1 GARANTIRE L' ATTIVITA' ISTITUZIONALE IN MODO EFFICACE ED APPROPRIATO " = "MacroArea 1", 
"2 POTENZIAMENTO DELLE ATTIVITA' RELATIVE ALLA RICERCA NAZIONALE E INTERNAZIONALE" = "MacroArea 2",
"3 RIDEFINIZIONE DELL'ORGANIZZAZIONE E DELLE ATTIVITA'"= "MacroArea 3",
"4 MANTENIMENTO DI ELEVATI LIVELLI DI STANDARD QUALITATIVI"= "MacroArea 4",                                                                  
"5 MIGLIORAMENTO SVILUPPO E INNOVAZIONE DELLA RETE INFRASTRUTTURALE" = "MacroArea 5",                                                        
"6 GARANTIRE LA SOSTENIBILITA' ECONOMICA E FINANZIARIA CON SVILUPPO DEL PATRIMONIO DELL' ISTITUTO E DELLE RISORSE UMANE"  = "MacroArea 6",   
"7 FORMAZIONE CONTINUA, COMUNICAZIONE E VALORIZZAZIONE DELLE RISORSE UMANE" = "MacroArea 7",                                 
"8 DIFFUSIONE DELLA CULTURA DELLA LEGALITA', SVILUPPO DI UN SISTEMA DI PREVENZIONE, RAFFORZAMENTO DEL LIVELLO DI TRASPARENZA"= "MacroArea 8")

levels(aggdata$Dipartimento)
aggdata$Dipartimento <- recode_factor(aggdata$Dipartimento,
                                      "Dipartimento amministrativo" = "Amministrativo", 
                                      "Dipartimento area territoriale Emilia Romagna" = " A.T.Emilia Romagna", 
                                      "Dipartimento area territoriale Lombardia" = "A.T.Lombardia", 
                                      "Dipartimento sicurezza alimentare" = "Sicurezza alimentare", 
                                      "Dipartimento tutela e salute animale" = "Tutela e salute animale", 
                                      "Direzione Amministrativa" = "Direzione Amministrativa",
                                      "Direzione Generale" = "Direzione Generale",
                                      "Direzione Sanitaria" = "Direzione Sanitaria")                                  

####funzioni per ordinare i segmenti----
#https://stackoverflow.com/questions/52214071/how-to-order-data-by-value-within-ggplot-facets
#https://drsimonj.svbtle.com/ordering-categories-within-ggplot2-facets

reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}

scale_y_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_y_discrete(labels = function(y) gsub(reg, "", y), ...)
}


macroarea_names <- c(
  `MacroArea 1` = "MacroArea 1 \n Attivit? istituzionale", 
  `MacroArea 2` = "MacroArea 2 \n Ricerca nazionale/internazionale",
  `MacroArea 3` = "MacroArea 3 \n Organizzazione efficiente",
  `MacroArea 4` = "MacroArea 4 \n Qualit?",                                                                  
  `MacroArea 5` = "MacroArea 5 \n Sviluppo e Innovazione",                                                        
  `MacroArea 6` = "MacroArea 6 \n Sostenibilit? economico-finanziaria",   
  `MacroArea 7` = "MacroArea 7 \n Formazione",                                 
  `MacroArea 8` = "MacroArea 8 \n Trasparenza e anticorruzione")


####PLOT TP4----
tp4 <- ggplot(aggdata, aes(y = reorder_within(Dipartimento,var_media,MacroArea), x = var_media)) + 
  geom_segment(aes(yend = reorder_within(Dipartimento,var_media,MacroArea), xend = 0), colour = "lightsteelblue3",lwd = 1) + 
  geom_point(size = 7.5, pch = 21, bg = "royalblue3", col = 1) +
  geom_text(aes(label = ifelse(var_media > 0, var_media, intToUtf8(10004))), color = ifelse(aggdata$var_media > 0, "white","green"), size = 2.5, fontface='bold') + 
  theme_bw() +
  scale_y_reordered() +
  facet_grid(MacroArea ~ ., scales = "free", space = "free", switch="y") +  #switch per cambiare posizione delle strip  
  labs(title = "Superamento obiettivi declinati dalle MacroAree nei differenti Dipartimenti assegnatari",
       y = "",
       x = "incremento medio %",
       subtitle = "anno 2021")

tp4 <- tp4 + scale_x_continuous(trans=scales::pseudo_log_trans(base = 10)) #per trattare i valori anomali e diminuire lunghezza segmenti
tp4 <- tp4 + theme(text = element_text(family="Calibri"),
                   plot.title = element_text(size=13),
                   plot.subtitle = element_text(size=9),
                   strip.text.y.left = element_text(angle = 0), #per ruotare strip #https://stackoverflow.com/questions/40484090/rotate-switched-facet-labels-in-ggplot2-facet-grid
                   strip.placement = "outside", #per mettere strip a sinistra delle labels
                   strip.text = element_text(size=11, margin=margin(15, 15, 15, 15)),
                   strip.background = element_rect(fill="lightblue", colour="royalblue3", size=1.5),
                   axis.text.x=element_blank(),
                   axis.ticks.x=element_blank(),
                   axis.title.x = element_text(size=10),
                   axis.text.y = element_text(size=10),
                   panel.grid.major.x = element_blank(),
                   panel.grid.minor.x = element_blank(),
                   panel.grid.major.y = element_line(size = 0.1, linetype = 'solid',colour = "lightsteelblue1")
)
tp4


####PLOT TP5----
tp5 <- ggplot(aggdata, aes(y = reorder_within(Dipartimento,var_media,MacroArea), x = var_media)) + 
  geom_segment(aes(yend = reorder_within(Dipartimento,var_media,MacroArea), xend = 0), colour = "lightsteelblue3",lwd = 1) + 
  geom_point(size = 2.5, pch = 21, bg = "royalblue3", col = 1) +
  geom_text(aes(label = ifelse(var_media > 0, var_media, intToUtf8(10004))), color = ifelse(aggdata$var_media > 0, "white","green"), size = 4, fontface='bold') + 
  theme_bw() +
  scale_y_reordered() +
  facet_grid(MacroArea ~ ., scales = "free", space = "free", switch="y", labeller = as_labeller(macroarea_names)) +  #switch per cambiare posizione delle strip  
  labs(title ="    Superamento obiettivi declinati dalle MacroAree nei differenti dipartimenti assegnatari",
       subtitle = "     anno 2021 (%)",
       y = "",
       x = "aumento medio (%)") +
  scale_x_continuous(trans=scales::pseudo_log_trans(base = 10)) + #per cambiare scala asse x
  theme(text = element_text(family="Calibri"),
        plot.title.position = "plot",
        plot.caption.position =  "plot",
        plot.title=element_text(size = 20),
        plot.subtitle=element_text(size = 16),
        strip.text.y.left = element_text(angle = 0), #per ruotare strip
        strip.placement = "outside", #per mettere strip a sinistra delle labels
        strip.text = element_text(size=17, lineheight = 0.3), #,margin=margin(15, 10, 15, 10) per dimensioni box strip
        strip.background = element_rect(fill="lightblue", colour="royalblue3", size=1),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x = element_text(size=12),
        axis.text.y = element_text(size=15),
        panel.grid.major.x = element_blank(),
                   panel.grid.minor.x = element_blank(),
                   panel.grid.major.y = element_line(size = 0.1, linetype = 'solid',colour = "lightsteelblue1"),
                   panel.spacing.y = unit(0.2,"line")
)

tp5

ggsave(filename = "plot_dip_macroarea.png", plot = tp5, width = 4, height = 4, dpi = 300, units = "in", device='png')
