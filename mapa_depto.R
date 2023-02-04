library(tidyverse)
library(sf)
library(cowplot)
library(geoAr)
library(herramientas)

options(scipen = 99)

#Cargo capa de deptos IGN
arg <- read_sf("departamento.json")

#Cargo data censo y ajusto nombres para join
censo <- read_csv("Censo - Departamentos.csv") %>% 
  janitor::clean_names() %>% 
  select(provincia = pcia, 
         departamento_partido = comuna,
         poblacion = total_de_poblacion) %>% 
  mutate(poblacion = as.numeric(str_remove_all(poblacion, "\\.")),
       provincia = case_when(provincia == "CABA" ~ "Ciudad Autónoma de Buenos Aires",
                             provincia == "Tierra del Fuego" ~ "Tierra del Fuego, Antártida e Islas del Atlántico Sur",
                             TRUE ~ provincia),
       departamento_partido = case_when(departamento_partido == "San Blas de los Sauces" ~ "San Blas de Los Sauces",
                                        departamento_partido == "1º de Mayo" ~ "1° de Mayo",
                                        TRUE ~ departamento_partido))

#Joineo tablas
censo_data <- arg %>% 
  mutate(cod_prov = substr(in1, 1, 2)) %>% #Genero código de provincia
  herramientas::etiquetar_provincia("cod_prov") %>% #Traigo nombres de provincias
  left_join(censo, by = c("provincia_nombre" = "provincia",
                          "nam" = "departamento_partido")) %>% 
  filter(!nam %in% c("Islas del Atlántico Sur", "Antártida Argentina")) #Filtro Antártida para visualización


#Defino breaks para visualización
breaks <- c(500, 5000, 50000, 300000, 1800000)
limits <- c(min(censo_data$poblacion, na.rm = T), max(censo_data$poblacion, na.rm = T))

#Traigo capa por provincia para base del mapa
base <- get_geo("ARGENTINA", "provincia")


#Defino deptos para mapa miniatura de GBA
aglomerado_gba <- c("AVELLANEDA","BERAZATEGUI","ESCOBAR","ESTEBAN ECHEVERRIA",
                            "EZEIZA","FLORENCIO VARELA","GENERAL RODRIGUEZ","GENERAL SAN MARTIN",
                            "HURLINGHAM","ITUZAINGO","JOSE C. PAZ","LA MATANZA","LANUS",
                            "LOMAS DE ZAMORA","MALVINAS ARGENTINAS","MARCOS PAZ",
                            "MERLO","MORENO","MORON","PILAR","PRESIDENTE PERON","QUILMES",
                            "SAN FERNANDO","SAN ISIDRO","SAN MIGUEL","SAN VICENTE",
                            "TIGRE","TRES DE FEBRERO","VICENTE LOPEZ", "ALMIRANTE BROWN")

#Filtro deptos GBA
gba <- censo_data %>% 
  filter(cod_prov == "06" & toupper(herramientas::remover_tildes(nam)) %in% aglomerado_gba |
           cod_prov == "02")

#Armo mapa de población país
mapa_arg <- ggplot() +
  geom_sf(data = censo_data, aes(fill = poblacion)) +
  geom_sf(data = base, fill = NA, color = "grey") +
  scale_fill_viridis_c(trans = "log",
                      breaks = breaks, 
                      labels = breaks, 
                      direction = -1,
                      limits = limits) +
  labs(fill = "Población", title = "Censo 2022, resultados provisionales") +
  theme_void() +
  theme(legend.position = "left",
        text = element_text(size = 18))

#Armo mapa de población GBA
mapa_caba <- ggplot() +
  geom_sf(data = gba, aes(fill = poblacion)) +
  scale_fill_viridis_c(trans = "log",
                       breaks = breaks, 
                       labels = breaks, 
                       direction = -1, 
                       limits = limits) +
  theme_void() +
  theme(legend.position = "none")

#Junto mapas
ggdraw(mapa_arg) +
  draw_plot(mapa_caba, x = 0.7, y = 0.5,
             width = 0.24, height = 0.24)

#Guardo como imagen
ggsave("mapa_censo.png", dpi = 200, height = 12, width = 12,
       bg = "white")
