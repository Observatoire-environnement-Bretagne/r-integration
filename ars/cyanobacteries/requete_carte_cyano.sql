select YEAR(den.DatePrel) as "Annee",
sit.CdSiteSurv as "Cd Site Surv",
sit.Commune as "Commune",
sit.CoordX as "Coord X",
sit.CoordY as "Coord Y",
sit.LbSiteSurv as "Lb Site Surv",
      CASE tox.Presence_toxine
      WHEN 1 THEN 'Présence ponctuelle'
      WHEN 0 THEN 'Absence'
      ELSE 'Pas de recherche'
  end as "Legende toxine",
sit.TypSitSurv as "Typ Sit Surv",
sum(den.nbjours_Sup_100000)+3 as "SUM([NbJours100000])+3",
sum(den.nbjours_Sup_100000) as "NbJours100000"
from
        oeb_ars_cyano.ars_denombrement den
left join 
(select
        CdSiteSurv,
        year(DatePrel) as Annee,
        max(resultat) as Presence_toxine
    from
        oeb_ars_cyano.ars_toxines
    where
        Serie = 'Présence de toxine'
    group by
        CdSiteSurv, year(DatePrel)) tox on den.CdSiteSurv = tox.CdSiteSurv and year(den.DatePrel) = tox.Annee
left join oeb_ars_cyano.ars_cyano_site sit on sit.CdSiteSurv = den.CdSiteSurv
group by 1,2,3,4,5,6,7,8
;