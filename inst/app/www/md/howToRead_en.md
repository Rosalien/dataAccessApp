*Reminder of the format file containing the observation data*

- File name : [Data_SNOT]_[dd_mm_yyyy_hh:MM:ss].csv
- File format : `csv`
- Column separator : `,`
- Decimal separator : `.`
- File encodage : `UTF-8`
- Null values : NA

**Spreadsheet**

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
