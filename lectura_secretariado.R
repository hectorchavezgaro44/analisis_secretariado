pacman::p_load(tidyverse,readxl,ggrepel,lubridate, tidyquant,sf, cowplot, Hmisc)

# este archivo en específico tiene el acumulado de todos los delitos, de todos los años (2015-2025),
# de todos los estados. No conviene subirlo a git.
# Pero por la estructura de la base, la manera de procesar los datos es la misma para cualquier año.

tabla_25 <- read_csv("Downloads/Municipal-Delitos-2015-2025_jun2025/Municipal-Delitos - Junio 2025 (2015-2025).csv", 
                            locale=locale(encoding="latin1")) %>% 
  janitor::clean_names() %>% 
  filter(subtipo_de_delito=="Homicidio doloso") %>% 
  group_by(ano, subtipo_de_delito) %>% 
  summarise(across(enero:diciembre, ~sum(.x, na.rm = T))) %>% 
  ungroup() %>% 
  gather(key="mes", value="homicidios", enero:diciembre) %>%
  group_by(ano) %>% 
  summarise(total=sum(homicidios, na.rm=T)) %>% 
  ungroup()
  
  mutate(meses = case_when(mes == "enero" ~ "01",
                           mes == "febrero"  ~ "02",
                           mes=="marzo" ~ "03",
                           mes=="abril" ~ "04",
                           mes=="mayo" ~ "05",
                           mes=="junio" ~ "06",
                           mes=="julio" ~ "07",
                           mes=="agosto" ~ "08",
                           mes=="septiembre" ~ "09",
                           mes=="octubre" ~ "10",
                           mes=="noviembre" ~ "11", 
                           mes=="diciembre" ~ "12"),
         fecha=paste(ano, "01",meses,sep="-"), 
         fecha=ydm(fecha))
