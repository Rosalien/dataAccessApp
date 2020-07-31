

#' welcome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom golem get_golem_options
#' @importFrom shiny NS tagList 
#' @importFrom shiny.i18n Translator
#' @importFrom shinycssloaders withSpinner
#' @importFrom plotly plotlyOutput renderPlotly
#' @importFrom DT dataTableOutput datatable renderDataTable
#' @importFrom leaflet leafletOutput renderLeaflet
#' @import shinyWidgets
#' @import shinydashboard
#' 
mod_welcomeUI <- function(id,translationVariable){
  ns <- NS(id)
  language <- get_golem_options("language")
  pool <- get_golem_options("pool")
  translator <- shiny.i18n::Translator$new(translation_csvs_path = "inst/app/www/translation")
  translator$set_translation_language(language)

  tabPanel(translator$t("Bienvenue"),
  dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(disable=TRUE),
    dashboardBody(
      fluidRow(
        column(2,
          fluidRow(
            box(width=12,
              uiOutput(ns('UIoutputsiteInstru')),
              conditionalPanel(condition=paste0("input['",ns("siteInstru"),"']!='All peatlands'"),uiOutput(ns("listesite")))
            )
          )
        ),
      column(10,
        fluidRow(
          #valueBoxOutput(ns("progressBox")),
          box(width=12,title =p(icon("map"),translator$t("Carte des tourbières et des instruments")), collapsed=FALSE,status = "primary", solidHeader = TRUE,collapsible = TRUE
              ,withSpinner(leafletOutput(ns("snot_map")),type=5)
             )
            )
          )
        ),
      fluidRow(
            box(width=7,
              title = tagList(shiny::icon("table"), translator$t("Données disponibles")),status = "warning",collapsed=TRUE, solidHeader = TRUE,collapsible = TRUE
              ,withSpinner(dataTableOutput(ns("tableDataDispo")),type=5)
            ),
            box(width=5,title = tagList(shiny::icon("chart-bar"), translator$t("Données disponibles")),solidHeader = TRUE,collapsible = TRUE,status = "warning",collapsed=TRUE,
              materialSwitch(inputId = ns("facetWrapOption"),label = translator$t("Détail par site ou station"),value = FALSE,status = "warning")
              ,withSpinner(plotlyOutput(ns("figureDataDispo"),height = 600),type=5)
              )
            ),
      fluidRow(
            box(width=12,
              title = translator$t("Liste des instruments"),solidHeader = TRUE,status = "danger",collapsed=TRUE,collapsible = TRUE
              ,withSpinner(dataTableOutput(ns("tableInstu")),type=5)
          )
        )
      )       
    )
  )
}
   
#' welcome Server Function
#'
#' @noRd 
mod_welcome <- function(input, output, session,translationVariable){
  ns <- session$ns
  language <- get_golem_options("language")
  pool <- get_golem_options("pool")
  translator <- shiny.i18n::Translator$new(translation_csvs_path = "inst/app/www/translation")
  translator$set_translation_language(language)

  # Internationalization des variables métadonnées
  metadataVariable <- c("type")

  # On assigne aux variables metadataVariable le nom de colonne internationalisé (utile??)
  for(i in metadataVariable){
    assign(i,paste0(i,"_",language))
  }

  # Caractéristique de l'ensemble des données
  # Mettre dans une fonction
  caracDataSensor <- caracdata(pool,language)[order(variable)]
  
  variableCaracData <- c("code_jeu","code_site","code_station","code_site_station","site_nom","theme","datatype","variable","unite","mindate","maxdate","zet_coordonnees_bbox")
  metadataVariable <- c("definition","station_description","site_description","station_nom")
  metadataSensor <- c("fabricant","instrument","description_capteur","station_description")

  caracData <- unique(caracDataSensor[,c(variableCaracData,metadataVariable),with=FALSE])
  caracCarto <- unique(caracDataSensor[,c(variableCaracData,metadataSensor),with=FALSE])

  # lecture du fichier pour gérer les couleurs
  col_station <- read.csv("inst/app/www/csv/datatype_couleur.csv",sep=";",header=TRUE,stringsAsFactors=FALSE)
  # Sélection de la colonne en fonction de la langue
  col_station <- col_station[,c("datatype","couleur",type)]
  names(col_station) <- c(names(col_station[1:2]),"type")

  # Jointure
  outdbcarto <- merge(caracCarto,col_station, by.x = "datatype", by.y = "datatype", all.x = TRUE,all.y=FALSE)

  # Création des listes pour la sélection des sites
  sitesnot <- c("All peatlands",unique(outdbcarto$site_nom))

  output$UIoutputsiteInstru <- renderUI({
      pickerInput(ns("siteInstru"), translator$t("Tourbières du SNO-T"), sitesnot,selected="All peatlands",options = list(style = "btn-info"))
})

# Sélection des sites/stations
  checkinsiteInstru <- reactiveValues(checked = "All peatlands")

  observe({
    input$siteInstru
    isolate({
      if(!is.null(input$siteInstru)){
          checkinsiteInstru$checked <- input$siteInstru
      }else{checkinsiteInstru$checked <- "All peatlands"}
    })
  })

  facetWrapSelected <- reactive({
      input$facetWrapOption
  })

  stationInstru <- reactive({
      input$stationInstru
  })

outdbcartoDataStation <- reactiveValues()

  observe({
    stationInstru()
    isolate({
      if(checkinsiteInstru$checked=="All peatlands"){
        outdbcartoDataStation$mapinstru <- unique(outdbcarto[,list(code_site_station,zet_coordonnees_bbox,station_description,datatype,couleur,type,site_nom)])
        outdbcartoDataStation$optionMap <- TRUE

        outdbcartoDataStation$tableCaracData <- caracData[,list(site_nom,variable,definition,unite,mindate,maxdate)]
        names(outdbcartoDataStation$tableCaracData) <- c("Site","Variable","Description",translator$t("Unité"),translator$t("Début"),translator$t("Fin"))

        outdbcartoDataStation$tableinstru <- unique(outdbcarto[!is.na(instrument),list(code_site_station,instrument,description_capteur)])
        names(outdbcartoDataStation$tableinstru) <- c("Site/Station",translator$t("Capteur"),"Description")

        outdbcartoDataStation$figCaracData <- unique(caracData[,list(site_nom,theme,variable,definition,mindate,maxdate)])

      }else{
        outdbcartoDataStation$mapinstru <- unique(outdbcarto[code_site_station %in% stationInstru(),list(code_site_station,zet_coordonnees_bbox,station_description,datatype,couleur,type)])
        outdbcartoDataStation$optionMap <- FALSE
        
        outdbcartoDataStation$tableCaracData <- unique(caracData[code_site_station %in% stationInstru(),list(station_description,variable,definition,unite,mindate,maxdate)])
        names(outdbcartoDataStation$tableCaracData) <- c("Station","Variable","Description",translator$t("Unité"),translator$t("Début"),translator$t("Fin"))

        outdbcartoDataStation$figCaracData <- unique(caracData[code_site_station %in% stationInstru(),list(station_nom,theme,variable,definition,mindate,maxdate)])

        outdbcartoDataStation$tableinstru <- unique(outdbcarto[code_site_station %in% stationInstru() & !is.na(instrument),list(code_site_station,station_description,instrument,description_capteur,zet_coordonnees_bbox)])
        names(outdbcartoDataStation$tableinstru) <- c("Site/Station","Station",translator$t("Capteur"),"Description","Coord")
      }
    })
  })

  output$listesite <- renderUI({
    Liststation <- unique(outdbcarto[site_nom %in% checkinsiteInstru$checked,list(station_description,code_site_station)])
    checkboxGroupInput(ns('stationInstru'), translator$t('Stations'), choiceNames=Liststation$station_description,
      selected=Liststation$code_site_station,choiceValues=Liststation$code_site_station)
})

  output$snot_map <- renderLeaflet({
    print("snot_map")
    # Création de la map selon les conditions
    sensorSelectedMap(outdbcartoDataStation$mapinstru,outdbcartoDataStation$optionMap,translator)  
})

  output$tableInstu <- DT::renderDataTable({
    print("tableInstu")
    retInstru <- DT::datatable(outdbcartoDataStation$tableinstru, rownames= FALSE,filter = 'top',extensions = 'Buttons',
      options = list(dom = "Blfrtip", buttons = list("copy",list(extend = "collection", buttons = c("csv", "excel", "pdf"),text = translator$t("Télécharger la table"))),
                    pageLength = 5, autoWidth = TRUE))
    return(retInstru)
  })# Fin de la table

  output$tableDataDispo <- DT::renderDataTable({
    print("tableDataDispo")
    # Condition pour afficher tous les sites
    retDataDispo <- DT::datatable(outdbcartoDataStation$tableCaracData, rownames= FALSE,filter = 'top',extensions = 'Buttons',
      options = list(dom = "Blfrtip", buttons = list("copy",list(extend = "collection", buttons = c("csv", "excel", "pdf"),text = translator$t("Télécharger la table"))),
                    pageLength = 5, autoWidth = TRUE))
    return(retDataDispo)
  })# Fin de la table


  output$figureDataDispo <- renderPlotly({
      print("figureDataDispo")
    
      figcarac <- outdbcartoDataStation$figCaracData[,variable:=ifelse(grepl("SWC|TS|G",variable)==TRUE,variableToVariableSimple(variable),variable)]
      figcarac <- unique(figcarac)[order(theme)]
      levels(figcarac$variable) <- unique(rev(figcarac$variable))
      figcarac$variable <- with(figcarac,factor(variable,levels=levels(variable)))
     
      timelineDataAvailable(figcarac,outdbcartoDataStation$optionMap,facetWrapSelected(),translator)

  })# Fin de figure
}#Fin du module
