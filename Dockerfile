FROM rocker/r-ver:3.6.3
RUN apt-get update && apt-get install -y  git-core imagemagick libjpeg-dev libcurl4-openssl-dev libgit2-dev libpng-dev libssh2-1-dev libssl-dev libxml2-dev libudunits2-dev libpq-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_github("r-lib/remotes", ref = "97bbf81")'
RUN Rscript -e 'remotes::install_version("yaml",upgrade="never", version = "2.2.1")'
RUN Rscript -e 'remotes::install_version("stringr",upgrade="never", version = "1.4.0")'
RUN Rscript -e 'remotes::install_version("pkgload",upgrade="never", version = "1.1.0")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "2.3.2")'
RUN Rscript -e 'remotes::install_version("jsonlite",upgrade="never", version = "1.7.0")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.0")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.5.0")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.0.1")'
RUN Rscript -e 'remotes::install_version("curl",upgrade="never", version = "4.3")'
RUN Rscript -e 'remotes::install_version("RColorBrewer",upgrade="never", version = "1.1-2")'
RUN Rscript -e 'remotes::install_version("gridExtra",upgrade="never", version = "2.3")'
RUN Rscript -e 'remotes::install_version("ggplot2",upgrade="never", version = "3.3.2")'
RUN Rscript -e 'remotes::install_version("sp",upgrade="never", version = "1.4-2")'
RUN Rscript -e 'remotes::install_version("DBI",upgrade="never", version = "1.1.0")'
RUN Rscript -e 'remotes::install_version("httr",upgrade="never", version = "1.4.2")'
RUN Rscript -e 'remotes::install_version("knitr",upgrade="never", version = "1.29")'
RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.5.3")'
RUN Rscript -e 'remotes::install_version("data.table",upgrade="never", version = "1.13.0")'
RUN Rscript -e 'remotes::install_version("xts",upgrade="never", version = "0.12-0")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3")'
RUN Rscript -e 'remotes::install_version("rmarkdown",upgrade="never", version = "2.3")'
RUN Rscript -e 'remotes::install_version("ggtern",upgrade="never", version = "3.3.0")'
RUN Rscript -e 'remotes::install_version("shinydashboard",upgrade="never", version = "0.7.1")'
RUN Rscript -e 'remotes::install_version("esquisse",upgrade="never", version = "0.3.0")'
RUN Rscript -e 'remotes::install_version("rintrojs",upgrade="never", version = "0.2.2")'
RUN Rscript -e 'remotes::install_version("suncalc",upgrade="never", version = "0.5.0")'
RUN Rscript -e 'remotes::install_version("gsubfn",upgrade="never", version = "0.7")'
RUN Rscript -e 'remotes::install_version("shinyalert",upgrade="never", version = "1.1")'
RUN Rscript -e 'remotes::install_version("shiny.i18n",upgrade="never", version = "0.1.0")'
RUN Rscript -e 'remotes::install_version("tableHTML",upgrade="never", version = "2.0.0")'
RUN Rscript -e 'remotes::install_version("shinythemes",upgrade="never", version = "1.1.2")'
RUN Rscript -e 'remotes::install_version("reshape",upgrade="never", version = "0.8.8")'
RUN Rscript -e 'remotes::install_version("plotly",upgrade="never", version = "4.9.2.1")'
RUN Rscript -e 'remotes::install_version("shinycssloaders",upgrade="never", version = "1.0.0")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "1.1")'
RUN Rscript -e 'remotes::install_version("wesanderson",upgrade="never", version = "0.3.6")'
RUN Rscript -e 'remotes::install_version("anytime",upgrade="never", version = "0.3.8")'
RUN Rscript -e 'remotes::install_version("dygraphs",upgrade="never", version = "1.1.1.6")'
RUN Rscript -e 'remotes::install_version("leaflet",upgrade="never", version = "2.0.3")'
RUN Rscript -e 'remotes::install_version("pool",upgrade="never", version = "0.1.4.3")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.14")'
RUN Rscript -e 'remotes::install_version("RPostgreSQL",upgrade="never", version = "0.6-2")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.2.1")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');dataAccessApp::run_app(language='en',pool='~/SI_SNOT/dbconfProd.yaml')"
