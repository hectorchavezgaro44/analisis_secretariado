pacman::p_load(tidyverse,readxl,ggrepel,lubridate, tidyquant,sf, cowplot, Hmisc)
require(geofacet)
# este archivo en específico tiene el acumulado de todos los delitos, de todos los años (2015-2025),
# de todos los estados. No conviene subirlo a git.
# Pero por la estructura de la base, la manera de procesar los datos es la misma para cualquier año.

tabla_25 <- read_csv("~/Downloads/Municipal-Delitos-2015-2025_jun2025/Municipal-Delitos - Junio 2025 (2015-2025).csv", 
                            locale=locale(encoding="latin1")) %>% 
  janitor::clean_names() %>% 
  filter(subtipo_de_delito=="Homicidio doloso") %>% 
  group_by(ano, subtipo_de_delito) %>% 
  summarise(across(enero:diciembre, ~sum(.x, na.rm = T))) %>% 
  ungroup() %>% 
  gather(key="mes", value="homicidio", enero:diciembre) %>% 
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
         fecha=ydm(fecha)) %>% 
  group_by(ano) %>% 
  summarise(homicidio=sum(homicidio, na.rm = T)) %>% 
  ungroup()


# loop --------------------------------------------------------------------


# loop de todos los años para no depender de una base de 2 millones de obs

nombres <- c("2015","2016", "2017","2018", "2019", "2020", "2021", "2022",
             "2023", "2024", "2025")
tabla<- tibble()
pb = txtProgressBar(min=1, max=length(nombres), style=3)

for(x in 1:length(nombres)) {
  
  c_tabla <- read_excel(paste0("~/Downloads/Municipal-Delitos-2015-2025_jun2025/", nombres[x], ".xlsx")) %>% 
    janitor::clean_names() %>% 
    filter(clave_ent==19 & subtipo_de_delito=="Homicidio doloso") %>% 
    group_by(ano, clave_ent, subtipo_de_delito) %>% 
    summarise(across(enero:diciembre, ~sum(.x, na.rm = T))) %>% 
    ungroup() %>% 
    gather(key="mes", value="homicidio_doloso", enero:diciembre) %>% 
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
  
  tabla <- bind_rows(tabla, c_tabla)
  setTxtProgressBar(pb, x)
  rm(c_tabla)
}

tabla <- bind_rows(tabla, tabla_25)


# Graphs ------------------------------------------------------------------

# Tendencia básica

tabla %>% 
  filter(fecha<="2025-02-01") %>% 
  filter(fecha>="2015-01-01") %>% 
  ggplot( ) +
  geom_line(aes(x = fecha, y = robo_transportista), size = 1.1, alpha = .2) +
  geom_point(aes(x = fecha, y = robo_transportista),size = 1.1, alpha = .3) +
  geom_smooth(aes(x = fecha, y = robo_transportista),
              method = "loess", color = "red", se=F,linetype = "dashed")+
  # scale_color_manual(values=c("#FF2700"))+
  labs(title="Evolución del total de roboo a transportista, Nuevo León",
       subtitle = "Del 2015 al 2024", 
       caption = "Elaboración propia. Fuente: SESNSP.",
       color="")+
  scale_x_date(date_breaks= "3 months", date_labels = "%b/%y")+
  # scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5)) 


# Geofacet 

myg <- mx_state_grid2

fct_tabla <- tabla %>% 
  mutate(code=as.numeric(clave_ent), 
         fecha=paste(ano, "01","01", sep="-"), 
         fecha=lubridate::ymd(fecha), 
         year=year(fecha)) %>% 
  left_join(myg, by="code")


ggplot(fct_tabla) +
  geom_line(aes(x = fecha, y = robo_transportista), size = 1.1, alpha = .2) +
  geom_point(aes(x = fecha, y = robo_transportista),size = 1.1, alpha = .3) +
  geom_smooth(aes(x = fecha, y = robo_transportista),
              method = "loess", color = "red", se=F,linetype = "dashed")+ 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5)) +
  facet_geo(~name, grid = "mx_state_grid2", scales = "free_y")+
  scale_x_date(date_breaks= "2 years", date_labels = "%Y")+
  labs(title="Evolución del total de roboo a transportista, por estado",
       subtitle = "Del 2015 al 2024",
       caption = "Elaboración propia. Fuente: SESNSP.",
       color="", y="", x="")

