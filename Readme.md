# dataAccessApp

<!-- badges: start -->
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

Shiny app to visualize and extract data from SNO-Tourbières database : [https://data-snot.cnrs.fr/data-access/](https://data-snot.cnrs.fr/data-access/)

## Installation

You can install the released version of dataAccessApp with:

``` r
devtools::install_github("Rosalien/dataAccessApp")
```

## Deploy

### Depencies

### Deploy in local

``` r
library(toolboxApp)
dataAccessApp::run_app(language="en",pool="dbconfProd.yaml")
```

### Shiny-server

Copy/paste package folder into shiny-server folder

``` r
git clone https://github.com/Rosalien/dataAccessApp.git
cp -r dataAccessApp/* to/the/Shiny-server/folder/
```

Modify `app.R` for language and database configuration :

- language : 'en' or 'fr'
- pool : path of yaml database configuration. [Example]()

``` r
dataAccessApp::run_app(language,pool)
```

### Docker

```bash
docker build -t dataaccessapp
```

```bash
docker run --net=host dataaccessapp
```
