library(tidyverse)
library(sf)
library(mapview)
library(aspe)
library(tod)

# ======================================================================================================
# Préparation d'un polygone de la région qui nous intéresse
# ======================================================================================================

# téléchargement du shapefile des contours des départements
url <- "https://www.data.gouv.fr/fr/datasets/r/3096e551-c68d-40ce-8972-a228c94c0ad1"
depts <- osm_depts_tod(url = url, repertoire = "raw_data", crs_sortie = 2154)

# Création du polygone (classe sf) de la région
region_bzh <- osm_creer_polygone_region(departements_sf = depts,
                                        departements_selectionnes = c("22", "29", "35", "56"),
                                        distance_buffer = 0.01,
                                        intitule_region = "Bretagne") %>% 
  `st_crs<-`(2154)

mapview(region_bzh)

# couche des SAGEs
wms <- "https://geobretagne.fr/geoserver/dreal_b/sage_dreal/wms?SERVICE=WMS&REQUEST=GetCapabilities" #image
wfs <- "https://geobretagne.fr/geoserver/dreal_b/sage_dreal/wfs?SERVICE=WFS&REQUEST=GetCapabilities" #data

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

mapview::mapview(sages2)

# ======================================================================================================
# Sélection des identifiants de la région
# ======================================================================================================
load(file = "raw_data/tables_sauf_mei_2021_09_10_14_35_58.RData")

pops <- point_prelevement %>% 
  geo_ajouter_coords_pop() %>% 
  geo_convertir_coords_df(var_x = "pop_coordonnees_x",
                          var_y = "pop_coordonnees_y",
                          var_id = "pop_id",
                          var_crs_initial = "typ_code_epsg",
                          crs_sortie = 2154) %>% 
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

# table peuplement
# --------------------------
provisoire <- passerelle %>% 
  left_join(station %>% 
              select(sta_id, sta_code_sandre)) %>% 
  left_join(point_prelevement %>% 
              select(pop_id, pop_code_sandre)) %>% 
  mutate(Code_station = ifelse(!is.na(sta_code_sandre), sta_code_sandre, pop_code_sandre)) # création d'un code point
 
peuplement_bzh <- provisoire %>% 
 select(Annee = annee,
         Num_operation = ope_id,
         date_peche = ope_date,
         Code_station,
         Code_espece_onema = esp_code_alternatif,
         Code_INPN = esp_code_taxref,
         Effectif_peche = lop_effectif) %>% 
  group_by(across(c(-Effectif_peche))) %>% 
    summarise(Effectif_peche = sum(Effectif_peche)) %>% 
  ungroup() %>% 
  distinct()


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
  geo_convertir_coords_df(var_crs_initial = "typ_code_epsg",
                          var_id = "pop_id",
                          var_x = "pop_coordonnees_x",
                          var_y = "pop_coordonnees_y")

# assemblage
operation_bzh <- operation_bzh %>% 
  left_join(coords) %>% 
  mutate(Cd_Rdd = NA,
         Lb_Rdd = NA) %>% 
  group_by(across(-pas_numero)) %>% 
    summarise(Nombre_passage = max(pas_numero)) %>% 
  ungroup() %>% 
  select(Annee = annee,
         Num_operation = ope_id,
         date_peche = ope_date,
         Code_station,
         Lb_station = pop_libelle,
         Coord_X = X,
         Coord_Y = Y,
         Cd_Rdd,
         Lb_Rdd,
         Methode_prospection = pro_libelle,
         Mode_prospection = mop_libelle,
         Nombre_passage,
         Surface_prospectee = ope_surface_calculee) %>% 
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
               values_to = "valeur_metrique")


ipr_bzh <- ipr_bzh %>% 
  mutate(classe_metrique = cut(valeur_metrique,
                               breaks = c(-99, 7, 16, 25, 36, 1e6) / 7,
                               labels = c("Excellent", "Bon", "Médiocre", "Mauvais", "Très mauvais")))



write.csv2(peuplement_bzh, file = "processed_data/peuplement.csv")
write.csv2(operation_bzh, file = "processed_data/operation.csv")
write.csv2(ipr_bzh, file = "processed_data/ipr.csv")

# export en Excel si ça a un intérêt
ma_liste <- list("peuplement" = peuplement_bzh,
                 "operation" = operation_bzh,
                 "ipr" = ipr_bzh)

openxlsx::write.xlsx(ma_liste, file = "processed_data/aspe_2021_10_20.xlsx",
                     overwrite = T)













load(file = "raw_data/tables_sauf_mei_2021_09_10_14_35_58.RData")

# dataframe "passerelle" de mise en correspondance des identifiants de station, point, opération etc.
passerelle <- mef_creer_passerelle()

# identifiants des points
pop_ids <- passerelle %>%
  mef_ajouter_dept() %>% 
  filter(dept %in% c("22", "29", "35", "56")) %>% 
  pull(pop_id) %>% 
  unique()

# filtrage de la passerelle sur la région
passerelle_bzh <- passerelle %>% 
  filter(pop_id %in% pop_ids) %>% 
  distinct()

# stations
# stations_bzh <- passerelle_bzh %>% 
#   mef_ajouter_libelle() %>% 
#   select(sta_id,
#          pop_id,
#          pop_libelle) %>% 
#   distinct() %>% 
#   left_join(y = station %>% 
#               select(sta_id, sta_code_sandre)) %>% 
#   left_join(y = point_prelevement %>% 
#               select(pop_id, pop_code_sandre))

# operations
# operations_bzh <- stations_bzh %>% 
#   left_join(passerelle_bzh) %>% 
#   mef_ajouter_ope_date() %>% 
#   select(sta_id,
#          pop_id,
#          ope_id,
#          ope_date,
#          annee) %>% 
#   distinct()

# captures
peuplement <- passerelle_bzh %>% 
  mef_ajouter_lots() %>% 
  mef_ajouter_esp_code_alternatif() %>% 
  mef_ajouter_ope_date() %>% 
  left_join(y = passerelle_taxo) %>% 
  group_by(across(c(-effectif))) %>% 
    summarise(lop_effectif = sum(lop_effectif)) %>% 
  ungroup() %>% 
  select(Annee = annee,
         Num_operation = ope_id,
         date_peche = ope_date,
         Code_station = sta_id,
         Code_espece_onema = esp_code_alternatif,
         Code_INPN = esp_code_taxref,
         Effectif_peche = lop_effectif) %>% 
  distinct() %>% 
  

# export en Excel si ça a un intérêt
ma_liste <- list("liste_stations" = stations_bzh,
                 "operations" = operations_49,
                 "captures" = captures_49,
                 "syntheses" = export_aspe_49,
                 "taxonomie" = passerelle_taxo)

openxlsx::write.xlsx(ma_liste, file = "processed_data/aspe_49.xlsx",
                     overwrite = T)