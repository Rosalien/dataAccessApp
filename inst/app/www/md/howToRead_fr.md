*Rappel du format du fichier contenant les données d'observation*

- Format du fichier : `csv`
- Séparateur de colonne : `,`
- Séparateur de décimal : `.`
- Encodage du fichier : `UTF-8`

**Tableur**

All spreadsheets can read csv files according to the input characteristics of the file.

**R**

```
SNOTData <- read.table("Data_SNOT_22_10_2019_15:24:06.csv",header=TRUE,sep=",",encoding="UTF-8") 
```

**python**

```
import pandas as pd
fichier=' ....'
obs=pd.read_csv(fichier,index_col=False)
obs_date=obs["Date"][:]
obs_var1=obs["lgt.pz_cbdv_amont_WTD"][:]
obs_var2=obs["lgt.pz_cbdv_aval_WTD"][:] 
```
