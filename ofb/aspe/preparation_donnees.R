library(tidyverse)
library(sf)
library(mapview)
library(aspe)
library(tod)
library(readxl)

setwd("O:/02.TRAITEMENTS/traitement-r/ofb/aspe")

# ======================================================================================================
# Préparation d'un polygone de la région qui nous intéresse
# ======================================================================================================

# téléchargement du shapefile des contours des départements
url <-
  "https://www.data.gouv.fr/fr/datasets/r/3096e551-c68d-40ce-8972-a228c94c0ad1"
depts <-
  osm_depts_tod(url = url,
                repertoire = "raw_data",
                crs_sortie = 2154)

# Création du polygone (classe sf) de la région
region_bzh <- osm_creer_polygone_region(
  departements_sf = depts,
  departements_selectionnes = c("22", "29", "35", "56"),
  distance_buffer = 0.01,
  intitule_region = "Bretagne"
) %>%
  `st_crs<-`(2154)

mapview(region_bzh)

# couche des SAGEs
wms <-
  "https://geobretagne.fr/geoserver/dreal_b/sage_dreal/wms?SERVICE=WMS&REQUEST=GetCapabilities" #image
wfs <-
  "https://geobretagne.fr/geoserver/dreal_b/sage_dreal/wfs?SERVICE=WFS&REQUEST=GetCapabilities" #data

# m <- mapview(region_bzh)
# m@map <- m@map %>%
#   addWMSTiles(group = "sage_dreal",
#               baseUrl = wms,
#               layers = "sage_dreal")                                                                               )
#
# m

sages <- st_read(wfs)

# pb pas moyen de manipuler cet objet => solution sur
# https://gis.stackexchange.com/questions/389814/r-st-centroid-geos-error-unknown-wkb-type-12/389854#389854
ensure_multipolygons <- function(X) {
  tmp1 <- tempfile(fileext = ".gpkg")
  tmp2 <- tempfile(fileext = ".gpkg")
  st_write(X, tmp1)
  gdalUtilities::ogr2ogr(tmp1, tmp2, f = "GPKG", nlt = "MULTIPOLYGON")
  Y <- st_read(tmp2)
  st_sf(st_drop_geometry(X), geom = st_geometry(Y))
}

sages <- ensure_multipolygons(sages)

mapview::mapview(sages)

# ======================================================================================================
# Sélection des identifiants de la région
# ======================================================================================================
load(file = "raw_data/tables_sauf_mei_2021_09_10_14_35_58.RData")

pops <- point_prelevement %>%
  geo_ajouter_coords_pop() %>%
  geo_convertir_coords_df(
    var_x = "pop_coordonnees_x",
    var_y = "pop_coordonnees_y",
    var_id = "pop_id",
    var_crs_initial = "typ_code_epsg",
    crs_sortie = 2154
  ) %>%
  st_as_sf(coords = c("X", "Y"),
           crs = 2154)

pops_bzh <- pops %>%
  st_join(region_bzh) %>%
  filter(!is.na(region))

pops_sages <- pops %>%
  st_join(sages) %>%
  filter(!is.na(cd_sage))

pops_id <- c(pops_bzh$pop_id, pops_sages$pop_id) %>%
  unique()

# ======================================================================================================
# Mise en forme
# ======================================================================================================
passerelle <- mef_creer_passerelle() %>%
  filter(pop_id %in% pops_id) %>%
  mef_ajouter_ope_date() %>%
  mef_ajouter_libelle() %>%
  mef_ajouter_lots() %>%
  mef_ajouter_esp_code_alternatif() %>%
  left_join(y = passerelle_taxo %>%
              select(esp_code_alternatif, esp_nom_latin, esp_code_taxref))

# ======================================================================================================
# Référentiels des types de cours d'eau et des abondances attendues
# ======================================================================================================
referentiel_classes_abondance_bzh <-
  read_excel(
    "raw_data/CLASAB_BZH.xls",
    sheet = "CLASBRET_Inv2P",
    range = "B4:K27",
    col_types = c(
      "text",
      "numeric",
      "skip",
      "numeric",
      "skip",
      "numeric",
      "skip",
      "numeric",
      "skip",
      "numeric"
    )
  ) %>%
  setNames(c("Code_espece_onema", "inf1", "inf2", "inf3", "inf4", "inf5"))

referentiel_station_typologie_ce <-
  read.csv("raw_data/referentiel_station_typologie_ce.csv", sep = ";") %>%
  setNames(c("Code_station", "Ref_typologie_CE")) %>%
  # Les codes station SANDRE ont 8 caractères
  mutate(Code_station = sprintf("%08d", Code_station))

referentiel_espece_abondance_theorique <-
  read.csv("raw_data/referentiel_espece_abondance_theorique.csv", sep = ";") %>%
  setNames(c(
    "Code_espece_onema",
    "Ref_typologie_CE",
    "Classe_abondance_theorique"
  )) %>%
  mutate(
    Classe_abondance_theorique = ifelse(
      Classe_abondance_theorique == '0,5',
      'P',
      Classe_abondance_theorique
    )
  )


# table peuplement
# --------------------------
provisoire <- passerelle %>%
  left_join(station %>%
              select(sta_id, sta_code_sandre)) %>%
  left_join(point_prelevement %>%
              select(pop_id, pop_code_sandre)) %>%
  mutate(Code_station = as.character(sta_code_sandre))
#mutate(Code_station = ifelse(!is.na(sta_code_sandre), sta_code_sandre, pop_code_sandre)) # création d'un code point


peuplement_bzh <- provisoire %>%
  select(
    Annee = annee,
    Num_operation = ope_id,
    date_peche = ope_date,
    Code_station,
    Code_espece_onema = esp_code_alternatif,
    Code_INPN = esp_code_taxref,
    Effectif_peche = lop_effectif
  ) %>%
  group_by(across(c(-Effectif_peche))) %>%
  summarise(Effectif_peche = sum(Effectif_peche)) %>%
  ungroup() %>%
  distinct() %>%
  # Jointure des référentiels pour obtenir une classe d'abondance et une classe d'abondance attendue
  left_join(referentiel_classes_abondance_bzh, by = "Code_espece_onema") %>%
  mutate(
    Classe_abondance = case_when(
      Effectif_peche >= inf5 ~ '5',
      Effectif_peche < inf5 &
        Effectif_peche >= inf4 ~ '4',
      Effectif_peche < inf4 &
        Effectif_peche >= inf3 ~ '3',
      Effectif_peche < inf3 &
        Effectif_peche >= inf2 ~ '2',
      Effectif_peche < inf2 &
        Effectif_peche >= inf1 ~ '1',
      # La classe d'abondance 'P' n'est pas gérée dans le job Talend --> vide
      Effectif_peche < inf1 &
        Effectif_peche > 0 ~ ''
    )
  ) %>%
  left_join(referentiel_station_typologie_ce, by = "Code_station") %>%
  left_join(
    referentiel_espece_abondance_theorique,
    by = c("Code_espece_onema", "Ref_typologie_CE")
  ) %>%
  select(
    Annee,
    Num_operation,
    date_peche,
    Code_station,
    Code_espece_onema,
    Code_INPN,
    Effectif_peche,
    Classe_abondance,
    Classe_abondance_theorique
  )


# table operation
# --------------------------
operation_bzh <- provisoire %>%
  mef_ajouter_moyen_prospection() %>%
  mef_ajouter_surf_calc() %>%
  mef_ajouter_passage() %>%
  mef_ajouter_type_protocole() %>%
  geo_ajouter_coords_pop()

# passage des coordonnées en WGS84
coords <- operation_bzh %>%
  select(pop_id,
         pop_coordonnees_x,
         pop_coordonnees_y,
         typ_code_epsg) %>%
  distinct() %>%
  geo_convertir_coords_df(
    var_crs_initial = "typ_code_epsg",
    var_id = "pop_id",
    var_x = "pop_coordonnees_x",
    var_y = "pop_coordonnees_y"
  )

# assemblage
operation_bzh <- operation_bzh %>%
  left_join(coords) %>%
  mutate(Cd_Rdd = NA,
         Lb_Rdd = NA) %>%
  group_by(across(-pas_numero)) %>%
  summarise(Nombre_passage = max(pas_numero)) %>%
  ungroup() %>%
  mutate(date_peche = format(ope_date, "%d/%m/%Y %H:%m")) %>%
  left_join(referentiel_station_typologie_ce, by = "Code_station") %>%
  select(
    Annee = annee,
    Num_operation = ope_id,
    date_peche,
    Code_station,
    Lb_station = pop_libelle,
    Coord_X = X,
    Coord_Y = Y,
    Cd_Rdd,
    Lb_Rdd,
    Methode_prospection = pro_libelle,
    Mode_prospection = mop_libelle,
    Nombre_passage,
    Surface_prospectee = ope_surface_calculee,
    Ref_typologie_CE
  ) %>%
  distinct()



# table IPR
# --------------------------

ipr_bzh <- provisoire %>%
  select(annee, Code_station, ope_id) %>% 
  distinct() %>% 
  mef_ajouter_ipr() %>% 
  mef_ajouter_metriques() %>% 
  filter(!is.na(ipr)) %>% 
  select(-(cli_id:dti_observe)) %>% 
  pivot_longer(cols = ner:dti,
               names_to = "type_metrique",
               values_to = "valeur_metrique") %>% 
  # Attribution des classes IPR
  # TODO rattacher les classes de qualité IPR https://www.legifrance.gouv.fr/jorf/article_jo/JORFARTI000037347782 tableau 34
  mutate(classe_metrique = cut(valeur_metrique,
                               # La limite supérieure de la classe "Bon" passe de 7 à 5
                               breaks = c(-99, 5, 16, 25, 36, 1e6) / 7,
                               labels = c("Très bon", "Bon", "Médiocre", "Mauvais", "Très mauvais"))) %>%
  left_join(classe_ipr, by=character()) %>%
  filter(ipr > cli_borne_inf, ipr <= cli_borne_sup, is.na(cli_altitude_max)) %>%
  mutate(cli_libelle = case_when(cli_libelle == 'Excellent' ~ 'Très bon',
                                 TRUE ~ as.character(cli_libelle))) %>%
  select(Annee = annee,
         Code_station,
         note_IPR = ipr,
         classe_IPR = cli_libelle,
         type_metrique,
         valeur_metrique,
         classe_metrique)

write.table(
  peuplement_bzh,
  file = "processed_data/peuplement.csv",
  na = "",
  sep = ";",
  row.names = FALSE,
  dec = ".",
  fileEncoding = "UTF-8"
)
write.table(
  operation_bzh,
  file = "processed_data/operation.csv",
  na = "",
  sep = ";",
  row.names = FALSE,
  dec = ".",
  fileEncoding = "UTF-8"
)
write.table(
  ipr_bzh,
  file = "processed_data/ipr.csv",
  na = "",
  sep = ";",
  row.names = FALSE,
  dec = ".",
  fileEncoding = "UTF-8"
)

# export en Excel si ça a un intérêt
ma_liste <- list("peuplement" = peuplement_bzh,
                 "operation" = operation_bzh,
                 "ipr" = ipr_bzh)

openxlsx::write.xlsx(ma_liste, file = "processed_data/aspe_2021_10_20.xlsx",
                     overwrite = T)