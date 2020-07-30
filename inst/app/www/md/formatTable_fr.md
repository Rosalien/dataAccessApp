Deux formats de table d'extraction sont proposés au choix avant le téléchargement : 

- **Vertical** : une table au format vertical propose un stockage des données par colonne. Ces colonnes sont les suivantes :
	- site : site/station code
	- Date : date de la valeur
	- value : valeur de la variable
	- variable : code de la variable

Par exemple pour les variables LW_IN_1_1_1 et LW_OUT_1_1_1 du site lgt et de la station bm2 :

```
site 	Date 	   value 	   variable
lgt/bm2 2019-02-04 296.0305625 LW_IN_1_1_1
lgt/bm2 2018-12-03 375.1695625 LW_OUT_1_1_1
```

- **Horizontal** : le format horizontal propose pour une date donnée, une distribution des valeurs par ligne. Par exemple pour les variables LW_IN_1_1_1 et LW_OUT_1_1_1 du site lgt et de la station bm2 : 

```
Date		lgt.bm2_LW_IN_1_1_1	lgt.bm2_LW_OUT_1_1_1
2017-10-02	396.249604166667	401.26275
2017-10-03	347.3494375			385.614375
```



