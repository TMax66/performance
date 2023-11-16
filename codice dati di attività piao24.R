pkg()

dt <- read_excel("cantina/PIAO valutazione/datiesamipersezioneanagraficapiao24.xlsx")

dt %>% 
  select(- `2023` ) %>% 
  filter(settore != "Altri Controlli (cosmetici,ambientali..)") %>% 
  pivot_longer(cols = 4:7, names_to = "anno", values_to = "nesami") %>% 
  group_by(anno, settore) %>% 
  summarise(nesami = sum(nesami, na.rm = TRUE)) %>%
  pivot_wider(names_from = anno, values_from = nesami) %>%
  rowwise() %>% 
  mutate(mediaesami = mean(c(`2019`,`2020`,`2021`,`2022`))) %>% View()

 
  
  