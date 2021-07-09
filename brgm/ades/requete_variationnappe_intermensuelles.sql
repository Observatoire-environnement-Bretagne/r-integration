SELECT 
   tmp.Date_de_la_mesure,
tmp.Mois, 
  avg(tmp.C_te_NGF) as 'Moyenne_Resultat', 
  tmp.Code_national_BSS,
  InterA.Max_Resultat_InterA,
  InterA.Min_Resultat_InterA,
  InterA.Moy_Resultat_InterA,
  pz.Denomination,
  pz.Adresse,
  pz.Mode_de_gisement,
  pz.Artificiel_Naturel,
  pz.Code_commune_actuelle,
  pz.Commune_INSEE_actuelle,
  pz.X_WGS84,
  pz.Y_WGS84,
  pz.Altitude,
  pz.Profondeur_investigation_maximale,
  pz.Masse_s__eau,
  pz.Entite_hydro_BDRHFV1,
  pz.Etat_du_point_d_eau
FROM oeb_piezo_variation_nappes.oeb_tbi_variationnappe_tmp tmp
join (SELECT 
  Mois, 
  Max(C_te_NGF) as 'Max_Resultat_InterA', 
  Min(C_te_NGF) as 'Min_Resultat_InterA', 
  Avg(C_te_NGF) as 'Moy_Resultat_InterA', 
  Code_national_BSS
FROM oeb_piezo_variation_nappes.oeb_tbi_variationnappe_tmp
group by  
  Mois, Code_national_BSS) as InterA on tmp.Code_national_BSS = InterA.Code_national_BSS and tmp.Mois = InterA.Mois
join oeb_piezo_variation_nappes.oeb_tbi_variationnappe_pz pz on tmp.Code_national_BSS = pz.Code_national_BSS
where Date_de_la_mesure between (select max(Date_de_la_mesure)- INTERVAL 12 MONTH from oeb_piezo_variation_nappes.oeb_tbi_variationnappe_tmp) and (select max(Date_de_la_mesure) from oeb_piezo_variation_nappes.oeb_tbi_variationnappe_tmp)
group by 
  Mois, Code_national_BSS
  order by Date_de_la_mesure , Code_national_BSS 