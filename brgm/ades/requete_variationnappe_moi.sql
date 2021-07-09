SELECT 
tmp.Code_national_BSS,
tmp.Date_de_la_mesure,
tmp.Profondeur_relative_rep_re_de_mesure,
tmp.C_te_NGF,
tmp.Code_qualification,
tmp.Qualification_de_la_mesure,
tmp.Continuit_,
tmp.Mode_obtention,
tmp.Statut_de_la_mesure,
tmp.Profondeur_d_but_site_de_mesure,
tmp.Profondeur_fin_site_de_mesure,
tmp.Producteur_de_donn_es,
tmp.Mois,
tmp.Annee,
pz.Denomination,
pz.Adresse,
pz.Mode_de_gisement,
pz.Artificiel_Naturel,
pz.Code_commune_actuelle,
pz.Commune_INSEE_actuelle,
tmp.X_WGS84,
tmp.Y_WGS84,
pz.Altitude,
pz.Profondeur_investigation_maximale,
pz.Masse_s__eau,
pz.Entite_hydro_BDRHFV1,
pz.Etat_du_point_d_eau
FROM oeb_piezo_variation_nappes.oeb_tbi_variationnappe_tmp tmp
join oeb_piezo_variation_nappes.oeb_tbi_variationnappe_pz pz on tmp.Code_national_BSS = pz.Code_national_BSS 
WHERE month(Date_de_la_mesure) = month((select max(Date_de_la_mesure) from oeb_piezo_variation_nappes.oeb_tbi_variationnappe_tmp))
AND year(Date_de_la_mesure) = year((select max(Date_de_la_mesure) from oeb_piezo_variation_nappes.oeb_tbi_variationnappe_tmp));