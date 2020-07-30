Two types of table are possible when you download data :

- **Long** : Each row is a unique id-variable combination :
	- site : site/station code
	- Date : date of the value
	- value : value of the variable
	- variable : variable code

Example for variables LW_IN_1_1_1 and LW_OUT_1_1_1 of "lgt" site and bm2 station :

```
site 	Date 	   value 	   variable
lgt/bm2 2019-02-04 296.0305625 LW_IN_1_1_1
lgt/bm2 2018-12-03 375.1695625 LW_OUT_1_1_1
```

- **Wide** : Format with date and value of a variable link to the combination of site/station. Example for variables LW_IN_1_1_1 and LW_OUT_1_1_1 of "lgt" site and bm2 station :

```
Date		lgt.bm2_LW_IN_1_1_1	lgt.bm2_LW_OUT_1_1_1
2017-10-02	396.249604166667	401.26275
2017-10-03	347.3494375			385.614375
```
