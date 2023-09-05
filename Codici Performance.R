library(tidyverse)
library(here)
library(flexdashboard)
library(knitr)
library(kableExtra)
library(formattable)
library(fmsb)
library(gt)
library(DBI)
library(odbc)

conSB <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "CED-IIS2",
                        Database = "ObiettiviStrategici2022", Port = 1433)

Query<-function(fixed="SELECT COLUMN_NAME, DATA_TYPE, CHARACTER_MAXIMUM_LENGTH FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME=", tabella="'nometab'"){
  paste(fixed, tabella)
}

q<-Query(tabella = "'vSchedaBudget'")

myfun <- function(con, q, tabella)
{   
  
  column.types <- dbGetQuery(con, q)
  
  ct <- column.types %>%
    mutate(cml = case_when(
      is.na(CHARACTER_MAXIMUM_LENGTH) ~ 10,
      CHARACTER_MAXIMUM_LENGTH == -1 ~ 100000,
      TRUE ~ as.double(CHARACTER_MAXIMUM_LENGTH)
    )
    ) %>%
    arrange(cml) %>%
    pull(COLUMN_NAME)
  fields <- paste(ct, collapse=", ")
  query <- paste("SELECT", fields, paste("FROM", tabella))
  return(query)
}

query <- myfun(con=conSB, q=q, tabella = "vSchedaBudget")
 
dati <- conSB %>% tbl(sql(query)) %>% as_tibble()

saveRDS(dati, file = "datiperformance.RDS")



dati <- readRDS("datiperformance.RDS")

# perf <- dati %>% 
#   filter(!str_detect(ObiettivoOperativo,"2.1.9."), 
#          Anno == "2022", 
#          !Indicatore %in% c("% di attività realizzata nell'anno 2022 per l'incarico di Direttore di Dipartimento",
#                             "% incremento indicatori griglia di valutazione PRC", 
#                             "% utilizzo budget PRF"), 
#          ValoreInRendiconto != -1)



 

perf <-  dati %>% 
  filter(Anno == "2023", 
    Periodo == 1) %>% 
  mutate(Dipartimento = ifelse(is.na(Dipartimento), "DIREZIONE AMMINISTRATIVA", Dipartimento), 
    AreaStrategica = recode(AreaStrategica, 
                                 "AS1 ATTIVITA' ISITUZIONALE - GARANTIRE L'ATTIVITÀ ISTITUZIONALE IN MODO EFFICACE ED APPROPRIATO " = 
                                   "AS1 ATTIVITA' ISITUZIONALE - GARANTIRE L'ATTIVITÀ ISTITUZIONALE IN MODO EFFICACE ED APPROPRIATO", 
                                 "AS1 GARANTIRE L'ATTIVITA' ISTITUZIONALE IN MODO EFFICACE ED APPROPRIATO " =
                                   "AS1 ATTIVITA' ISITUZIONALE - GARANTIRE L'ATTIVITÀ ISTITUZIONALE IN MODO EFFICACE ED APPROPRIATO", 
                                 "AS1 GARANTIRE L'ATTIVITA' ISTITUZIONALE IN MODO EFFICACE ED APPROPRIATO" = 
                                   "AS1 ATTIVITA' ISITUZIONALE - GARANTIRE L'ATTIVITÀ ISTITUZIONALE IN MODO EFFICACE ED APPROPRIATO",
                                 
                                 "AS2 POTENZIARE LE ATTIVITA' RELATIVE ALLA RICERCA NAZIONALE ED INTERNAZIONALE  " =
                                   "AS2 POTENZIARE LE ATTIVITA' RELATIVE ALLA RICERCA NAZIONALE ED INTERNAZIONALE", 
                                 
                                 "AS3 SISTEMA GESTIONALE - GARANTIRE L’EFFICIENZA DEI SISTEMI GESTIONALI ATTRAVERSO LA SOSTENIBILITÀ DEI PROCESSI IN UN’OTTICA MULTIDIMENSIONALE "=
                                   "AS3 SISTEMA GESTIONALE - GARANTIRE L’EFFICIENZA DEI SISTEMI GESTIONALI ATTRAVERSO LA SOSTENIBILITÀ DEI PROCESSI IN UN’OTTICA MULTIDIMENSIONALE", 
                                 "AS3 GARANTIRE L’EFFICIENZA DEI SISTEMI GESTIONALI ATTRAVERSO LA SOSTENIBILITÀ DEI PROCESSI IN UN’OTTICA MULTIDIMENSIONALE  " =
                                   "AS3 SISTEMA GESTIONALE - GARANTIRE L’EFFICIENZA DEI SISTEMI GESTIONALI ATTRAVERSO LA SOSTENIBILITÀ DEI PROCESSI IN UN’OTTICA MULTIDIMENSIONALE",
                                 "AS3 GARANTIRE L’EFFICIENZA DEI SISTEMI GESTIONALI ATTRAVERSO LA SOSTENIBILITÀ DEI PROCESSI IN UN’OTTICA MULTIDIMENSIONALE " =
                                   "AS3 SISTEMA GESTIONALE - GARANTIRE L’EFFICIENZA DEI SISTEMI GESTIONALI ATTRAVERSO LA SOSTENIBILITÀ DEI PROCESSI IN UN’OTTICA MULTIDIMENSIONALE",
                                 
                                 "AS4 FORMAZIONE E COMUNICAZIONE - COMUNICAZIONE\r\nPROMUOVERE LA FORMAZIONE CONTINUA, SVILUPPARE LA COMUNICAZIONE E VALORIZZAZIONE DELLE RISORSE UMANE\r\n" =
                                   "AS4 FORMAZIONE E COMUNICAZIONE - COMUNICAZIONE PROMUOVERE LA FORMAZIONE CONTINUA, SVILUPPARE LA COMUNICAZIONE E VALORIZZAZIONE DELLE RISORSE UMANE", 
                                 "AS4 PROMUOVERE LA FORMAZIONE CONTINUA, IL DIALOGO CON GLI STAKEHOLDER E  LA VALORIZZAZIONE DELLE RISORSE UMANE " =
                                   "AS4 FORMAZIONE E COMUNICAZIONE - COMUNICAZIONE PROMUOVERE LA FORMAZIONE CONTINUA, SVILUPPARE LA COMUNICAZIONE E VALORIZZAZIONE DELLE RISORSE UMANE"
                                 ), 
    AS = substr(AreaStrategica, start = 1, stop = 3), 
          AreaStrategica = recode( AS, 
                                   AS1 = "AS1-ATTIVITA' ISTITUZIONALE", 
                                   AS2 = "AS2-POTENZIAMENTO DELLA RICERCA", 
                                   AS3 = "AS3-SISTEMA GESTIONALE", 
                                   AS4 = "AS4-FORMAZIONE E COMUNICAZIONE"), 
                                   
          Dipartimento = factor(Dipartimento, levels = c("DIREZIONE GENERALE", 
                                                         "DIREZIONE SANITARIA", 
                                                         "DIREZIONE AMMINISTRATIVA", 
                                                         "DIPARTIMENTO AMMINISTRATIVO",
                                                         "DIPARTIMENTO TUTELA SALUTE ANIMALE", 
                                                         "DIPARTIMENTO SICUREZZA ALIMENTARE", 
                                                         "DIPARTIMENTO AREA TERRITORIALE LOMBARDIA",
                                                         "DIPARTIMENTO AREA TERRITORIALE EMILIA ROMAGNA"))) 

perf %>% 
  select(AreaStrategica,Dipartimento, Reparto, Struttura, Indicatore,  Periodo, Target, ValoreInRendiconto, Avanzamento) %>%  
  group_by(AreaStrategica) %>% 
  summarise(media = 100*round(mean(Avanzamento,na.rm  = T),2))  %>% 
  ungroup %>% 
  add_row(AreaStrategica = 'Livello Sintetico di Ente', !!! colMeans(.[-1]))


 

# perf %>% rename( "Area Strategica" = AreaStrategica ) %>%   
#   #write.xlsx(file= "LSE.xlsx")


library(flexdashboard)
gauge(46, min= 0, max = 100, symbol = '%',
      gaugeSectors(success = c(0,100),   colors = "steelblue"))

perf %>% 
  group_by(AreaStrategica) %>% 
  summarise(media = 100*mean(Avanzamento,na.rm  = T)) %>%
  ungroup() %>%  
  #pivot_wider(names_from = Dipartimento, values_from = media) %>%  
  gt() %>% 
  fmt_number(columns = 2,decimals = 2) %>% 
  sub_missing(
    columns = 2,
    missing_text = "")  




perf %>% 
  group_by(AreaStrategica, Dipartimento)  %>% 
  summarise(media = 100*mean(Avanzamento,na.rm  = T)) %>%
  ungroup() %>% 
  pivot_wider(names_from = AreaStrategica, values_from = media) %>%   
  gt() %>% 
  fmt_number(columns = 2:5,decimals = 2) %>% 
  sub_missing(
    columns = 2:5,
    missing_text = "") 
 
  
  
  
  
  

  

Area <-  perf %>%
  filter(Periodo == 1  ) %>%
  mutate(MacroArea = factor(MacroArea)) %>%
  group_by(MacroArea) %>%
  summarise(mediana =  round(median(Avanzamento, na.rm = T),2),
            media = round(mean(Avanzamento,na.rm  = T),2),
            n = n()) %>%
  mutate(mediana = percent(mediana),
         mediana = as.character(mediana),
         media = percent(media),
         media = as.character(media)) %>%
  #pivot_wider(names_from = "Dipartimento", values_from = "mediana", values_fill = " ") %>%
  arrange(MacroArea) %>%
  mutate(MacroArea = as.character(MacroArea)) %>%
  mutate(MacroArea = gsub("\\d+", "", MacroArea),
         MacroArea = gsub("\"", "", MacroArea))  %>%
  kbl( ) %>%
  kable_styling() %>%
  kable_paper(bootstrap_options = "striped", full_width = F)
  # 
  # 
  # #Polar plot avanzamento per Area----
  # 
  plot_dt <- perf %>%
      filter(Periodo == 1  ) %>%
    # mutate(MacroArea = factor(MacroArea)) %>%
    group_by(AreaStrategica) %>%
    summarise(mediana =  100*round(median(Avanzamento, na.rm = T),2),
              media = 100*round(mean(Avanzamento, na.rm = T),2),
              n = n()) %>%
    mutate(target = 100)  
    # mutate(MacroArea = as.character(MacroArea)) %>%
    # mutate(MacroArea = gsub("\\d+", "", MacroArea),
    #        MacroArea = gsub("\"", "", MacroArea))

  # #Polar plot avanzamento per Area/Dip---
  # plot_dt2 <- dt %>%
  #   mutate(MacroArea = factor(MacroArea)) %>%
  #   group_by(MacroArea, Dipartimento) %>%
  #   summarise(mediana =  100*round(median(Avanzamento, na.rm = T),2),
  #             media = 100*round(mean(Avanzamento, na.rm = T),2),
  #             n = n()) %>%
  #   mutate(target = 100) %>%
  #   mutate(MacroArea = as.character(MacroArea)) %>%
  #   mutate(MacroArea = gsub("\\d+", "", MacroArea),
  #          MacroArea = gsub("\"", "", MacroArea))
  # 
  # 
  # 
  # 
  # 
  # ##Plot----
  plt <- ggplot(plot_dt)+
    geom_hline(
      aes(yintercept = y),
      data.frame(y = c(0, 25, 50, 75, 90, 100)),
      color = "lightgrey"
    )+
    geom_col(
      aes(x = reorder(str_wrap(AreaStrategica, 1), media),
          y = media,
          fill = media
          ),
      position = "dodge2",
      show.legend = TRUE,
      alpha = .9
    )+

    geom_point(
      aes(
        x = reorder(str_wrap(AreaStrategica, 1), media),
        y = media
      ),
      size = 3, color = "gray12"
    )+

    geom_segment(
      aes(
        x =  reorder(str_wrap(AreaStrategica, 1), media),
        y = 0,
        xend = reorder(str_wrap(AreaStrategica, 1), media),
        yend = 100
      ),
      linetype = "dashed",
      color = "gray12"
    )+
    coord_polar()+

    scale_y_continuous(
      limits = c(-20,110),
      expand = c(0, 0)

    ) +
    geom_text(
      aes(
        x = reorder(str_wrap(AreaStrategica, 1), media),
        y = media-10,
        label = paste0(media, "%")),
        color = "black",
        size=5)+

    annotate(
      x = 0.5,
      y = 30,
      label = "25%",
      geom = "text",
      color = "red",
      family = "Bell MT"
    )  +
    annotate(
      x = 0.5,
      y = 55,
      label = "50%",
      geom = "text",
      color = "red",
      family = "Bell MT"
    )  +

    annotate(
      x = 0.5,
      y = 80,
      label = "75%",
      geom = "text",
      color = "red",
      family = "Bell MT"
    )  +

    annotate(
      x = 0.5,
      y = 110,
      label = "100%",
      geom = "text",
      color = "red",
      family = "Bell MT"
    )  +

   # scale_fill_gradientn(colours = gray.colors(7))+

    theme(
      # Remove axis ticks and text
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      # Use gray text for the region names
      axis.text.x = element_text(color = "gray12", size = 8),
      # Move the legend to the bottom
      legend.position = "blank",
    )+

    # Customize general theme
    theme(

      # Set default color and font family for the text
      text = element_text(color = "gray12", family = "Bell MT"),

      # Customize the text in the title, subtitle, and caption
      plot.title = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(size = 14, hjust = 0.05),
      plot.caption = element_text(size = 10, hjust = .5),

      # Make the background white and remove extra grid lines
      panel.background = element_rect(fill = "white", color = "white"),
      panel.grid = element_blank(),
      panel.grid.major.x = element_blank()
    )
  # 
  # 
  # 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

library(strex)

perf %>% 
  mutate(OG = sub(" - ", "-", ObiettivoGenerale ), 
         OG = sub("........", "", OG), 
         OG = sub("1", "", OG), 
         ObGen =sub(" - ", "-", ObiettivoGenerale ), 
         ObGen = substr(ObGen, start=5, stop=7),
         ObOp = str_after_nth( ObiettivoOperativo, "\\.", 3),
         ObOp = str_remove(ObOp, "1.")) %>% 
  select(AS,  ObGen,OG, ObOp, Struttura) %>% 
  
 # pivot_wider(names_from = "Struttura", values_from ="Struttura" ) %>% 
  unique() 

 # openxlsx::write.xlsx(file = "obiettiviop.xlsx")
 


  #select(AS, ObiettivoGenerale, ObiettivoOperativo, Indicatore, Dipartimento, Reparto, Struttura) %>% 
  
  #filter(str_detect(ObiettivoGenerale,"AS1-OG2")) %>% 


  
  





# perf <- readRDS("performance.RDS")
# perf <-  perf %>%
#   mutate(Dipartimento = factor(Dipartimento, levels = c("Direzione Generale", 
#                                                         "Direzione Sanitaria", 
#                                                         "Direzione Amministrativa", 
#                                                         "Dipartimento amministrativo", 
#                                                         "Dipartimento tutela e salute animale", 
#                                                         "Dipartimento sicurezza alimentare", 
#                                                         "Dipartimento area territoriale Lombardia",
#                                                         "Dipartimento area territoriale Emilia Romagna")), 
#          Dipartimento = toupper(Dipartimento))
# 
# perf <- perf %>% 
#   filter(Periodo == 4) %>%  
#   mutate(Avanzamento = ifelse(Periodo == 4 & Avanzamento == 0, 1, Avanzamento) ) %>% 
# mutate(Avanzamento = ifelse(Avanzamento == 0.5, 1, Avanzamento))  %>% 
#   mutate(flag = str_detect(Indicatore, "VOIP"), 
#          flag2 = str_detect(Indicatore, "Attivazione nuovo sistema di gestione dei laboratori IZSLER")) %>%  
#   mutate(Avanzamento = ifelse(flag == TRUE,  0.7, Avanzamento), 
#          Avanzamento = ifelse(flag2 == TRUE, 0.7, Avanzamento)) 




# 
# 
# library(openxlsx)
# perf %>% 
#   filter(MacroArea== "1 GARANTIRE L' ATTIVITA' ISTITUZIONALE IN MODO EFFICACE ED APPROPRIATO ") %>% 
#   select(Obiettivo, Azione, Indicatore) %>% 
#   distinct() %>%  
#   write.xlsx("mA1.xlsx")







# perf %>% 
#   filter(Periodo == 2) %>% 
#   group_by(MacroArea, Dipartimento) %>% 
#   summarise(media = 100*mean(Avanzamento,na.rm  = T)) %>%
#   ungroup() %>% 
#   pivot_wider(names_from = Dipartimento, values_from = media) %>% 
#   gt() %>%
#   fmt_number(columns = 2:9,decimals = 2) %>% 
#   fmt_missing(
#     columns = 2:9,
#     missing_text = "") %>% 
# gtsave("t2.rtf")
# 
# 
# perf %>% 
#   filter(Periodo == 4) %>%
#   summarise(media = 100*mean(Avanzamento,na.rm  = T))
# 
# 
# 
# 
# 
# 
# 
# 
#  
#       
#   filter(Periodo == 4) %>%  
#   mutate(Avanzamento = ifelse(Periodo == 4 & Avanzamento == 0, 1, Avanzamento) ) 
# mutate(Avanzamento = ifelse(Avanzamento == 0.5, 1, Avanzamento))  
# 
# 
# perf %>% 
#   filter(Periodo == 4) %>% 
#   group_by(MacroArea, Dipartimento) %>% 
#   summarise(media = 100*mean(Avanzamento,na.rm  = T)) %>% View()
#   ungroup() %>% 
#   pivot_wider(names_from = Dipartimento, values_from = media, value) %>% View()
# 
# perf %>% 
#   filter(Periodo == 4) %>% 
#   group_by(MacroArea, Dipartimento) %>% 
#   summarise(media = 100*mean(Avanzamento,na.rm  = T)) %>% 
#   ungroup() %>% 
#   pivot_wider(names_from = Dipartimento, values_from = media) %>% View()
# 
# perf %>% 
#   filter(Periodo == 4) %>%  
#   group_by(Dipartimento, Indicatore) %>%  
#   summarise(mediaV = mean(Valore, na.rm = T)) %>% 
#   
#   perf %>% 
#   filter(Periodo == 4) %>% 
#   group_by(MacroArea, Dipartimento) %>% 
#   summarise(media = 100*mean(Avanzamento,na.rm  = T)) %>% 
#   ungroup %>% 
#   pivot_wider(names_from = Dipartimento, values_from = media, value) %>% 
#    gt() %>% 
#   fmt_number(columns = media,decimals = 2)
#   
#   
# 
# perf %>% 
#   filter(Periodo == 4) %>% 
#   group_by(Dipartimento, MacroArea) %>% 
#   summarise(media = 100*mean(Avanzamento,na.rm  = T)) %>% 
#   ungroup %>% 
#   gt() %>% 
#   fmt_number(columns = media,decimals = 2)
# 
# 
# 
# 
# perf %>% 
#   summarise(media = 100*(mean(Avanzamento, na.rm= T)))
# 
# 
# 
# 
# 
# 
# 
# 
# perf <-  perf %>%
# #  select(-StrutturaAssegnataria, -MacroArea, -TipoObiettivo, -Azione) %>% 
#   mutate(Avanzamento = ifelse(Periodo == 4 & Avanzamento == 0, 1, Avanzamento) ) 
#   
# 
# perf %>% 
#   filter(Periodo == 4) %>%  
#   mutate(Avanzamento = ifelse(Avanzamento == 0.5, 1, Avanzamento))  %>% 
#   group_by(MacroArea) %>% 
#   summarise(media = 100*(mean(Avanzamento,na.rm  = T))) %>%  View()
#  
# dt <- perf %>% 
#   filter(Periodo == 4 | Periodo == 2) %>% View()
#   
#   
#   # # pivot_wider(names_from = Periodo, values_from = c("Valore", "Avanzamento")) %>% 
#   # dplyr::group_by(Anno, TipoObiettivo, MacroArea, Obiettivo, Azione, Indicatore, Reparto, Dipartimento, Periodo) %>%
#   # dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
#   # dplyr::filter(n > 1L) %>% View()
#   
# library(kableExtra)
# library(knitr)
# library(stringr)
# 
# perf %>% 
#   # mutate( #MacroArea = gsub('[0-9]+', '', MacroArea),
#   #        MacroArea = str_to_sentence(MacroArea))  %>% 
#   filter(Periodo == 4) %>% 
#   group_by(MacroArea, Dipartimento) %>% 
#   summarise("% Ragiungimento obiettivo" = mean(Avanzamento,na.rm  = T)) %>% 
#   # ungroup %>%  
#   # add_row(MacroArea = 'Livello Sintetico di Ente', !!! colMeans(.[-1], na.rm = TRUE)) %>% 
#   gt() %>% 
#   # fmt_number(columns = "% Ragiungimento obiettivo",
#   #            decimals = 2) %>% 
#   # fmt_percent(
#   #   columns = "% Ragiungimento obiettivo",
#   #   decimals = 2
#   # ) %>% 
#  gtsave("t2.rtf")
#   
#   # kbl("latex", digits = 2,
#   #     col.names = c("Macro Area", "% di raggiungimento degli obiettivi
#   #                    (media complessiva tra strutture, obiettivi e indicatori)")) %>% 
#   # kable_styling(latex_options="scale_down", font_size = 3)  
#    
#  
# 
# 
# # (perf %>% 
# #     filter(Periodo == 4) %>% 
# #     summarise(media = 100*round(mean(Avanzamento, na.rm = T),2)))
# 
# x <- gauge(99.8, min= 0, max = 100, symbol = '%',
#            gaugeSectors(success = c(0,100),   colors = "steelblue"))
# 
# 
# # pPerf <- perf %>% 
# #                     filter(Periodo == 4 & Avanzamento != 0 ) %>%
# #                     mutate(MacroArea = factor(MacroArea)) %>%
# #                     group_by(MacroArea) %>%
# #                     summarise(media = 100*round(mean(Avanzamento, na.rm = T),2),
# #                               n = n()) %>%
# #                     mutate(target = 100) %>%
# #                     mutate(MacroArea = as.character(MacroArea)) %>%
# #                     mutate(MacroArea = gsub("\\d+", "", MacroArea),
# #                            MacroArea = gsub("\"", "", MacroArea))
# 
# 
# 
# 
# 
# 
# library(gt)
#  
# 
#   
# gtsave("LSE.pdf")
#   
#   
#   
#   library(tibble)
# df2 %>% 
#   ungroup %>% 
#   add_row(year = 'mean', !!! colMeans(.[-1]))
# 
# 
# 
# # AV2 <- dt %>% 
# #   filter(Periodo == 4) %>% 
# #   mutate(Avanzamento = ifelse(Avanzamento == 0, 1, Avanzamento) )
# 
# 
# 
# 
# 
# 
# library(flexdashboard)
# 
# x <- gauge(74, min= 0, max = 100, symbol = '%',
#       gaugeSectors(success = c(0,100),   colors = "steelblue"))
# 
# x %>%
#   knit_print()
# 
# 
# 
# 
# 
# #Dipartimenti----
# 
# 
# 
# ##AreaDip-----
# AreaDip <-  perf %>%  
#   filter(Periodo == 4  ) %>% 
#   mutate(MacroArea = factor(MacroArea)) %>% 
#   group_by(Dipartimento,  MacroArea) %>% 
#   summarise(media =  round(mean(Avanzamento, na.rm = T),2)) %>%  
#   mutate(media = percent(media), 
#          media = as.character(media)) %>%   
#   pivot_wider(names_from = "Dipartimento", values_from = "media", values_fill = " ") %>%  
#   select("MacroArea","Direzione Generale", "Direzione Sanitaria", "Dipartimento tutela e salute animale", 
#          "Dipartimento sicurezza alimentare","Dipartimento area territoriale Lombardia",
#          "Dipartimento area territoriale Emilia Romagna",
#          "Dipartimento amministrativo") %>% 
#   arrange(MacroArea) %>% 
#   mutate(MacroArea = as.character(MacroArea)) %>% 
#   mutate(MacroArea = gsub("\\d+", "", MacroArea), 
#          MacroArea = gsub("\"", "", MacroArea))  %>% 
#   rename("Macro Area" = "MacroArea") %>% 
#   kbl( ) %>% 
#   kable_styling() %>% 
#   kable_paper(bootstrap_options = "striped", full_width = F)# %>% 
#   save_kable(file = "tab1.png")
# 
# 
# 
# #plot aredip----
# 
# plt2 <- ggplot(plot_dt2)+
#   geom_hline(
#     aes(yintercept = y),
#     data.frame(y = c(0, 25, 50, 75, 90, 100)),
#     color = "lightgrey"
#   )+
#   geom_col(
#     aes(x = reorder(str_wrap(MacroArea, 1), media),
#         y = media,
#         fill = media
#     ),
#     position = "dodge2",
#     show.legend = TRUE,
#     alpha = .9
#   )+
# 
#   geom_point(
#     aes(
#       x = reorder(str_wrap(MacroArea, 1), media),
#       y = media
#     ),
#     size = 3, color = "gray12"
#   )+
# 
#   geom_segment(
#     aes(
#       x =  reorder(str_wrap(MacroArea, 1), media),
#       y = 0,
#       xend = reorder(str_wrap(MacroArea, 1), media),
#       yend = 100
#     ),
#     linetype = "dashed",
#     color = "gray12"
#   )+
#   coord_polar()+
# 
#   scale_y_continuous(
#     limits = c(-20,110),
#     expand = c(0, 0)
# 
#   ) +
#   geom_text(
#     aes(
#       x = reorder(str_wrap(MacroArea, 1), media),
#       y = media-10,
#       label = paste0(media, "%")),
#     color = "black",
#     size=5)+
# 
#   annotate(
#     x = 0.5,
#     y = 30,
#     label = "25%",
#     geom = "text",
#     color = "red",
#     family = "Bell MT"
#   )  +
#   annotate(
#     x = 0.5,
#     y = 55,
#     label = "50%",
#     geom = "text",
#     color = "red",
#     family = "Bell MT"
#   )  +
# 
#   annotate(
#     x = 0.5,
#     y = 80,
#     label = "75%",
#     geom = "text",
#     color = "red",
#     family = "Bell MT"
#   )  +
# 
#   annotate(
#     x = 0.5,
#     y = 110,
#     label = "100%",
#     geom = "text",
#     color = "red",
#     family = "Bell MT"
#   )  +
# 
#   scale_fill_gradientn(colours = gray.colors(7))+
# 
#   theme(
#     # Remove axis ticks and text
#     axis.title = element_blank(),
#     axis.ticks = element_blank(),
#     axis.text.y = element_blank(),
#     # Use gray text for the region names
#     axis.text.x = element_text(color = "gray12", size = 8),
#     # Move the legend to the bottom
#     legend.position = "blank",
#   )+
# 
# 
# 
#   facet_wrap(~Dipartimento, ncol = 5)
# 
# plt2 <- plt2+
#   labs(
#     title = paste("\nGrado di raggiungimento obiettivi di performance:Valutazione Intermedia al 30/06/2021.\n"),
#     caption = "U.O. Controllo di Gestione e Performances")+
# 
#   # Customize general theme
#   theme(
# 
#     # Set default color and font family for the text
#     text = element_text(color = "gray12", family = "Bell MT"),
# 
#     # Customize the text in the title, subtitle, and caption
#     plot.title = element_text(face = "bold", size = 18),
#     plot.subtitle = element_text(size = 14, hjust = 0.05),
#     plot.caption = element_text(size = 10, hjust = .5),
# 
#     # Make the background white and remove extra grid lines
#     panel.background = element_rect(fill = "white", color = "white"),
#     panel.grid = element_blank(),
#     panel.grid.major.x = element_blank()
#   )
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ###OBIETTIVI ----
# 
# Obiettivi <-  dt %>%  
#   mutate(Obiettivo = factor(Obiettivo)) %>% 
#   group_by(Dipartimento,  Obiettivo) %>% 
#   summarise(media =  round(mean(Avanzamento, na.rm = T),2)) %>% 
#   mutate(media = percent(media), 
#          media = as.character(media)) %>%
#   pivot_wider(names_from = "Dipartimento", values_from = "media", values_fill = " ") %>%  
#   select("Obiettivo","Direzione Generale", "Direzione Sanitaria", "Dipartimento tutela e salute animale", 
#          "Dipartimento sicurezza alimentare","Dipartimento area territoriale Lombardia",
#          "Dipartimento area territoriale Emilia Romagna",
#          "Dipartimento amministrativo") %>%  
#   arrange(Obiettivo) %>% 
#   mutate(Obiettivo = as.character(Obiettivo)) %>% 
#   mutate(Obiettivo = gsub("\\d+", "", Obiettivo), 
#          Obiettivo = gsub("\\.", "", Obiettivo), 
#          Obiettivo = gsub("\\)", "", Obiettivo),
#          Obiettivo = gsub("\"", "", Obiettivo)) %>% .[-5,] %>% 
#   kbl() %>% 
#   kable_styling() %>% 
#   kable_paper(bootstrap_options = "striped", full_width = F) %>% 
#   save_kable(file = "tab2.png")
# 
# 
# 
# ##plot obiettivi
# 
# plot_ob <-  dt %>%  
#   mutate(Obiettivo = factor(Obiettivo)) %>% 
#   group_by(Obiettivo) %>% 
#   summarise(media =  100*round(mean(Avanzamento, na.rm = T),2)) %>% 
#   arrange(Obiettivo) %>% 
#   mutate(Obiettivo = as.character(Obiettivo)) %>% 
#   mutate(Obiettivo = gsub("\\d+", "", Obiettivo), 
#          Obiettivo = gsub("\\.", "", Obiettivo), 
#          Obiettivo = gsub("\\)", "", Obiettivo),
#          Obiettivo = gsub("\"", "", Obiettivo)) %>% .[-5,]
# 
# plt <- ggplot(plot_ob)+
#   aes(y = Obiettivo, x = media)+
#   geom_point()
#    
# 
# Indicatori <-   dt %>%  
#   mutate(Indicatore = factor(Indicatore)) %>% 
#   group_by(Dipartimento,  Indicatore) %>% 
#   summarise(mediana =  round(median(Avanzamento, na.rm = T),2)) %>% 
#   mutate(mediana = percent(mediana), 
#          mediana = as.character(mediana)) %>%
#   pivot_wider(names_from = "Dipartimento", values_from = "mediana", values_fill = " ") %>% 
#   select("Indicatore","Direzione Generale", "Direzione Sanitaria", "Dipartimento tutela e salute animale", 
#          "Dipartimento sicurezza alimentare","Dipartimento area territoriale Lombardia",
#          "Dipartimento area territoriale Emilia Romagna",
#          "Dipartimento amministrativo") %>% 
#   arrange(Indicatore) %>% 
#   mutate(Indicatore = as.character(Indicatore)) %>% 
#   mutate(Indicatore = gsub("\\d+", "", Indicatore), 
#          Indicatore = gsub("\\.", "", Indicatore), 
#          Indicatore = gsub("\\)", "", Indicatore),
#          Indicatore = gsub("\"", "", Indicatore)) %>% View()
# kbl() %>% 
#   kable_styling()
# 
# 
# dt %>% 
#   group_by(Azione) %>% 
#   unique() %>% 
#   count() %>% 
#   count() %>%  View()
# 
#   
#   
#   
#   # summarise(min= min(Avanzamento, na.rm = T), 
#   #                    mediana = median(Avanzamento, na.rm = T), 
#   #                                     media = mean(Avanzamento, na.rm = T),
#   #                                     max = max(Avanzamento, na.rm = T))
#   # 
# 
# 
# 
# 
# 
# ##PlotAreaDip
# 
# 
# newp <- subset(dt, dt$periodo== "B" & dt$punteggio == 0)
# 
# 
# & dt$punteggio =0)
# 
# 
# 
# 
# ifelse(dt$punteggio == 0 & dt$periodo==4, subset(dt, ))