-- data_infraj_prod : Ensemble des données valeurs/mesures
CREATE MATERIALIZED VIEW public.data_infraj_prod AS 
select *
from(
select date,time,cn.code as code_site_station,cn5.code as variable,value,dt.name as datatype
from 
(
select mms_date as date ,mms_time as time,v.id,value
from valeurs_meteo_sh_vms as v
INNER JOIN  mesures_meteo_sh_mms as m on m.mms_id=v.mesure_id  
)a
join realnode rv ON rv.id = a.id    
join realnode rd ON rd.id = rv.id_parent_node
join realnode rt ON rt.id = rd.id_parent_node
join realnode rs ON rs.id = rt.id_parent_node
join composite_nodeable cn ON cn.id = rs.id_nodeable 
join datatype_unite_variable_snot_vdt dvu ON dvu.sdvu_id = rv.id_nodeable
join composite_nodeable cn5 on cn5.id=dvu.id
join datatype as dt on dt.dty_id=dvu.dty_id
UNION
select date,time,cn.code as code_site_station,cn5.code as variable,value,dt.name as datatype
from 
(
select mfs_date as date ,mfs_time as time,v.id,value
from valeurs_flux_tour_vfs as v
INNER JOIN  mesures_flux_mfs as m on m.mfs_id=v.mesure_id  
)a
join realnode rv ON rv.id = a.id    
join realnode rd ON rd.id = rv.id_parent_node
join realnode rt ON rt.id = rd.id_parent_node
join realnode rs ON rs.id = rt.id_parent_node
join composite_nodeable cn ON cn.id = rs.id_nodeable 
join datatype_unite_variable_snot_vdt dvu ON dvu.sdvu_id = rv.id_nodeable
join composite_nodeable cn5 on cn5.id=dvu.id
join datatype as dt on dt.dty_id=dvu.dty_id
)a
WITH DATA;

CREATE INDEX ON public.data_infraj_prod (code_site_station);
CREATE INDEX ON public.data_infraj_prod (variable);
CREATE INDEX ON public.data_infraj_prod (date);

-- carac_data_sensor_method_prod : Caractérisation des données, des capteurs, des méthodes et des jeux de données (table de métadonnées pour construire et documenter l'appli web)
CREATE MATERIALIZED VIEW public.carac_data_sensor_method_prod AS 
select *
from(
select d[1] as code_site,d[2] as code_station,code_site_station,theme,datatype,variable,unite,minDate,maxDate,
definition_fr,l5.localestring as definition_en,l6.localestring as station_nom_fr,
l3.localestring as station_nom_en,station_description_fr,station_description_en,site_description_fr,
site_description_en,l4.localestring as site_nom,
code_jeu,fabricant,instrument,description_capteur_fr,description_capteur_en,zet_coordonnees_bbox,description_methode_fr,description_methode_en,
	titre,descriptionJeu,genealogie,doi,zet_altitude
from(
select regexp_split_to_array(c[1],'/'),c[1] as code_site_station,theme,c[2] as datatype,c[3] as variable,c[4] as unite,minDate,maxDate,
site_description_fr, definition as definition_fr,station_description_fr,station_description_en,site_description_en,
code_jeu,fabricant,instrument,description_capteur_fr,description_capteur_en,zet_coordonnees_bbox,description_methode_fr,description_methode_en,
	titre,descriptionJeu,genealogie,doi,zet_altitude
from(
select regexp_split_to_array(b[4],'-'),b[2] as theme,minDate,maxDate,site_description_fr,
station_description_fr,station_description_en,site_description_en,code_jeu,fabricant,instrument,description_capteur_fr
	,description_capteur_en,zet_coordonnees_bbox,description_methode_fr,description_methode_en,
	titre,descriptionJeu,genealogie,doi,zet_altitude
from(
select distinct regexp_split_to_array(path, ','),minDate,maxDate,s.description as station_description_fr,
si.description as site_description_fr,l2.localestring as station_description_en,l.localestring as site_description_en,
code_jeu,fabricant,i.code as instrument,loc.defaultstring as description_capteur_fr,
loc.localestring as description_capteur_en,zet_coordonnees_bbox,loc2.defaultstring as description_methode_fr,loc2.localestring as description_methode_en,
	jeu.titre,jeu.description as descriptionJeu,genealogie,doi,zet_altitude
from(
select min(m.mms_date) as minDate,max(m.mms_date) as maxDate,v.id,site_id
from valeurs_meteo_sh_vms as v
INNER JOIN mesures_meteo_sh_mms as m on m.mms_id=v.mesure_id
group by v.id,site_id
)a
INNER JOIN realnode as rn on rn.id=a.id
INNER JOIN site as s on s.site_id=a.site_id
INNER JOIN site as si on si.site_id=s.parent_site_id
INNER JOIN site_snot on site_snot.site_id=s.site_id
INNER JOIN localisation as l on l.defaultstring=si.description
INNER JOIN localisation as l2 on l2.defaultstring=s.description
LEFT JOIN datatype_unite_variable_snot_vdt dvu ON dvu.sdvu_id = rn.id_nodeable 
LEFT JOIN jeu on jeu.jeu_id = dvu.jeu_id
LEFT JOIN periode_utilisation_instrument as pui on pui.stdtvar_id=rn.id
LEFT JOIN instrument as i on i.instr_id=pui.instr_id
LEFT JOIN localisation as loc on loc.defaultstring=i.description
LEFT JOIN periode_application_methode as pam on pam.stdtvar_id=rn.id
LEFT JOIN methode_calcul as mth on mth.mcalc_id=pam.mcalc_id
LEFT JOIN localisation as loc2 on loc2.defaultstring=mth.description
WHERE loc.colonne like 'description' 
or loc.colonne is null
--or loc2.colonne like 'description' 
--or loc2.colonne is null
)as dt(b)
) as dt(c)
INNER join composite_nodeable as cn on cn.code=c[3]
INNER join variable as v on v.var_id=cn.id
)as dt(d)
INNER JOIN localisation as l3 on l3.defaultstring=d[2]
INNER JOIN localisation as l6 on l6.defaultstring=d[2]
INNER JOIN localisation as l4 on l4.defaultstring=d[1]
INNER JOIN localisation as l5 on l5.defaultstring=definition_fr
WHERE l3.colonne like 'nom'
AND l6.colonne like 'nom'
AND l3.localization like 'en'
AND l6.localization like 'fr'
AND l4.localization like 'fr'
AND l4.colonne like 'nom'
AND l5.colonne like 'definition'
UNION
select d[1] as code_site,d[2] as code_station,code_site_station,theme,datatype,variable,unite,minDate,maxDate,
definition_fr,l5.localestring as definition_en,l6.localestring as station_nom_fr,l3.localestring as station_nom_en,
station_description_fr,station_description_en,site_description_fr,site_description_en,l4.localestring as site_nom,
code_jeu,fabricant,instrument,description_capteur_fr,description_capteur_en,zet_coordonnees_bbox,description_methode_fr,description_methode_en,
	titre,descriptionJeu,genealogie,doi,zet_altitude
from(
select regexp_split_to_array(c[1],'/'),c[1] as code_site_station,theme,c[2] as datatype,c[3] as variable,c[4] as unite,
minDate,maxDate,site_description_fr, definition as definition_fr,station_description_fr,
station_description_en,site_description_en,
code_jeu,fabricant,instrument,description_capteur_fr,description_capteur_en,zet_coordonnees_bbox,description_methode_fr,description_methode_en,
	titre,descriptionJeu,genealogie,doi,zet_altitude
from(
select regexp_split_to_array(b[4],'-'),b[2] as theme,minDate,maxDate,site_description_fr,station_description_fr,
station_description_en,site_description_en,code_jeu,fabricant,instrument,description_capteur_fr,description_capteur_en,zet_coordonnees_bbox,description_methode_fr,description_methode_en,
	titre,descriptionJeu,genealogie,doi,zet_altitude
from(
select distinct regexp_split_to_array(path, ','),minDate,maxDate,s.description as station_description_fr,
si.description as site_description_fr,l2.localestring as station_description_en,l.localestring as site_description_en,
code_jeu,fabricant,i.code as instrument,loc.defaultstring as description_capteur_fr,
loc.localestring as description_capteur_en,zet_coordonnees_bbox,loc2.defaultstring as description_methode_fr,loc2.localestring as description_methode_en,
	jeu.titre,jeu.description as descriptionJeu,genealogie,doi,zet_altitude
from(
select min(m.mfs_date) as minDate,max(m.mfs_date) as maxDate,v.id,site_id
from valeurs_flux_tour_vfs as v
INNER JOIN mesures_flux_mfs as m on m.mfs_id=v.mesure_id
group by v.id,site_id
)a
INNER JOIN realnode as rn on rn.id=a.id
INNER JOIN site as s on s.site_id=a.site_id
INNER JOIN site as si on si.site_id=s.parent_site_id
INNER JOIN site_snot on site_snot.site_id=s.site_id
INNER JOIN localisation as l on l.defaultstring=si.description
INNER JOIN localisation as l2 on l2.defaultstring=s.description
LEFT JOIN datatype_unite_variable_snot_vdt dvu ON dvu.sdvu_id = rn.id_nodeable 
LEFT JOIN jeu on jeu.jeu_id = dvu.jeu_id
LEFT JOIN periode_utilisation_instrument as pui on pui.stdtvar_id=rn.id
LEFT JOIN instrument as i on i.instr_id=pui.instr_id
LEFT JOIN localisation as loc on loc.defaultstring=i.description
LEFT JOIN periode_application_methode as pam on pam.stdtvar_id=rn.id
LEFT JOIN methode_calcul as mth on mth.mcalc_id=pam.mcalc_id
LEFT JOIN localisation as loc2 on loc2.defaultstring=mth.description
WHERE loc.colonne like 'description' 
or loc.colonne is null
--or loc2.colonne like 'description' 
--or loc2.colonne is null
)as dt(b)
) as dt(c)
INNER join composite_nodeable as cn on cn.code=c[3]
INNER join variable as v on v.var_id=cn.id
)as dt(d)
INNER JOIN localisation as l3 on l3.defaultstring=d[2]
INNER JOIN localisation as l6 on l6.defaultstring=d[2]
INNER JOIN localisation as l4 on l4.defaultstring=d[1]
INNER JOIN localisation as l5 on l5.defaultstring=definition_fr
WHERE l3.colonne like 'nom'
AND l6.colonne like 'nom'
AND l3.localization like 'en'
AND l6.localization like 'fr'
AND l4.localization like 'fr'
AND l4.colonne like 'nom'
AND l5.colonne like 'definition'
)b
WITH DATA;