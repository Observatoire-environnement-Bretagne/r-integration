SELECT
tmp.Code_national_BSS,
tmp.Date_de_la_mesure,
tmp.Mois,
tmp.Annee, 
avg(tmp.C_te_NGF) as 'Moyenne_Resultat',
pz.Denomination,
pz.Adresse,
pz.Mode_de_gisement,
pz.Artificiel_Naturel,
pz.Code_commune_actuelle,
pz.Commune_INSEE_actuelle,
pz.X_WGS84,
pz.Y_WGS84,
pz.Altitude,
avg(tmp.Profondeur_relative_rep_re_de_mesure) as Profondeur_relative_rep_re_de_mesure,
pz.Masse_s__eau,
pz.Entite_hydro_BDRHFV1,
pz.Etat_du_point_d_eau
FROM oeb_piezo_variation_nappes.oeb_tbi_variationnappe_tmp tmp
join oeb_piezo_variation_nappes.oeb_tbi_variationnappe_pz pz on tmp.Code_national_BSS = pz.Code_national_BSS 
group by 
  tmp.Annee, tmp.Mois, tmp.Code_national_BSS