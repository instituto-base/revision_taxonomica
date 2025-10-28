#Trabajo con taxonomía de aves
#2025-10-28
#Catalina Marín Cruz

#settings####

#library
library(rgbif)
library(dplyr)
library(tidyverse)
library(ggplot2)

#datos----

estudio_aves <- read.csv("estudio_aves.csv", fileEncoding = "UTF-8", sep = ";",
                         na = "")


#marcar datos conflictivos-----

aves_marcado <- estudio_aves %>% 
  mutate(conflicto = 
           case_when(orden_sacc != orden_sag | 
                     nombre_cientifico_sacc != nombre_cientifico_sag ~ "conflicto_taxonomico",
                     orden_sacc == orden_sag ~ "sin_conflicto"
           ))
#conteo de datos conflictivos---
n_conflictos <- aves_marcado %>% 
                group_by(conflicto) %>% 
                summarise( conteo = n())

#validación taxonomica-----
#validación taxonómica sacc-----
revision_taxonomica_sacc <- name_backbone_checklist(aves_marcado$nombre_cientifico_sacc) #revision taxonómica según sacc

conteo_matchtype1 <- revision_taxonomica_sacc %>%
                      group_by(matchType) %>% 
                      summarise(conteo = n())
fuzzy_taxsacc <- revision_taxonomica_sacc %>% 
                 filter(matchType == "FUZZY")

#validación taxonomica sag----

revision_taxonomica_sag <- name_backbone_checklist(aves_marcado$nombre_cientifico_sag) #revision taxonómica según sag

conteo_matchtype2 <- revision_taxonomica_sag %>%
  group_by(matchType) %>% 
  summarise(conteo = n())

fuzzy_taxsag <- revision_taxonomica_sag %>% 
  filter(matchType == "FUZZY")

HR_taxsag <- revision_taxonomica_sag %>% 
  filter(matchType == "HIGHERRANK")

#gráfico comparativo-----
datos_grafico <- inner_join(conteo_matchtype1, conteo_matchtype2)

grafico_comparativo <- ggplot() +
  geom_col(data = conteo_matchtype1, 
           aes(x = matchType, y = conteo, fill = "SACC"),
           position = position_nudge(0.22),
           width = 0.4) +
  geom_col(data = conteo_matchtype2, 
           aes(x = matchType, y = conteo, fill = "SAG"),
           position = position_nudge(-0.22),
           width = 0.4) +
  scale_fill_manual(
    name = "Fuente",
    values = c("SACC" = "blue", "SAG" = "red")
  ) +
  theme_minimal()
                  
print(grafico_comparativo)

#reemplazar datos conflictivos con taxonomía GBIF----

aves_sag <- aves_marcado %>% 
            select(orden_sag, 
                   familia_sag, 
                   nombre_cientifico_sag)

aves_corregido <- aves_sag %>% 
                  mutate(orden = case_when( 
                    orden_sag != revision_taxonomica_sag$order 
                                ~ revision_taxonomica_sag$order,
                    orden_sag == revision_taxonomica_sag$order ~ orden_sag),
                    familia = case_when(
                      familia_sag != revision_taxonomica_sag$family ~ revision_taxonomica_sag$order,
                      familia_sag == revision_taxonomica_sag$family ~  familia_sag),
                    genero = revision_taxonomica_sag$genus,
                    nombre_cientifico = case_when(
                      nombre_cientifico_sag != revision_taxonomica_sag$canonicalName
                      ~ revision_taxonomica_sag$canonicalName, 
                      nombre_cientifico_sag == revision_taxonomica_sag$canonicalName
                      ~ nombre_cientifico_sag
                    ))



