#' mod_accessdataUI
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
#' @importFrom plotly plotlyOutput
#' @importFrom DT dataTableOutput datatable renderDataTable
#' @importFrom leaflet leafletOutput renderLeaflet
#' @importFrom shinyWidgets awesomeRadio dropdownButton pickerInput actionBttn radioGroupButtons show_alert
#' @importFrom shinyalert useShinyalert
#' @importFrom rintrojs introjsUI introBox hintjs introjs
#' @importFrom data.table setkeyv setDT
#' @import esquisse
#' @importFrom shinyjs alert toggleState show
#' @importFrom readr write_csv
#' @import shinydashboard
#' @import toolboxApps
#' 
mod_accessdataUI <- function(id,translationVariable){
  ns <- NS(id)
  language <- get_golem_options("language")
  pool <- get_golem_options("pool")

  translator <- shiny.i18n::Translator$new(translation_csvs_path = "inst/app/www/translation")
  translator$set_translation_language(language)

  # Checkbocks on multi-column
  tweaks <- list(tags$head(tags$style(HTML(tweaks2()))))
  
  # Load data
  caracData <- caracdata(pool,language)[order(variable),]

  # List link to language
  choicesDayNight <- c("day/night","day","night")
  names(choicesDayNight) <- c(translator$t("Jour & Nuit"),translator$t("Jour"),translator$t("Nuit"))

  choicesFrequence <- c("30 min", "hour","day","week","month","year")
  names(choicesFrequence) <- c(translator$t("Infra-jour"),translator$t("Heure"),translator$t("Jour"),translator$t("Semaine"),translator$t("Mois"),translator$t("Année"))

# To create checkboxGroupInput 
dropdowSelectionStation <- function(caracDataStationSelection,nameNS){
  if(nrow(caracDataStationSelection)>0){
    lapply(1:length(unique(caracDataStationSelection$site_nom)),function(x){
    checkboxGroupInput(ns(paste(nameNS,x,sep="")),unique(caracDataStationSelection$site_nom)[x],selected = NULL,   
    choiceNames = caracDataStationSelection[site_nom %in% unique(caracDataStationSelection$site_nom)[x],3][[1]],
    choiceValues = caracDataStationSelection[site_nom %in% unique(caracDataStationSelection$site_nom)[x],]$code_site_station)
  })
  }else{}
}

tabPanel(translator$t("Accès aux données"),
         tags$head(tags$style(HTML("hr {border-top: 1px solid #ecf0f1;}"))),
         tags$head(tags$style(HTML('#run{background-color:orange}'))),
#         tags$head(HTML(googleAnalyticsParameter())),
         #tags$script(HTML("$(document).one('shiny:idle',function() {ga('set','userId', Shiny.user);});")),
  fluidPage(
    introjsUI(),
    fluidRow(
      column(2,
        introBox(
        dropdownButton(
          icon = icon("cloudversify"),width = "150%",circle = FALSE,status = "warning",label = translator$t("Stations"),
          dropdowSelectionStation(unique(caracData[grepl("pz|ch", caracData$code_site_station)==FALSE,list(code_site_station,site_nom,station_description)]),nameNS="siteSNOT_")
          ),
        br(),
        dropdownButton(
          icon = icon("tint"),width = "100%",circle = FALSE,status = "warning",label = translator$t("Piézomètres"),
          dropdowSelectionStation(unique(caracData[grepl("pz", caracData$code_site_station)==TRUE,list(code_site_station,site_nom,station_nom)]),"sitePiezo_")),
        br(),
        dropdownButton(
          icon = icon("bell"),width = "100%",circle = FALSE,status = "warning",label = translator$t("Chambres"),
          dropdowSelectionStation(unique(caracData[grepl("ch", caracData$code_site_station)==TRUE,list(code_site_station,site_nom,station_nom)]),"siteChambre_")),
        data.step = 1,
        data.intro = includeMarkdown(paste0("inst/app/www/md/localationSelected_",language,".md"))),
        br(),
        introBox(
        uiOutput(ns('minmaxdateSNOT')),
        data.step = 2,
        data.intro = includeMarkdown(paste0("inst/app/www/md/periodSelected_",language,".md"))
        ),
        introBox(
        awesomeRadio(
          inputId = ns("dayNight"),
          label = translator$t("Jour & Nuit"), 
          choices = choicesDayNight,
          selected = "day/night",
          inline = TRUE
        ),
        data.step = 3,
        data.intro = includeMarkdown(paste0("inst/app/www/md/dayNightSelected_",language,".md"))#translator$t("Sélectionnez le type de jour")
        ),
        introBox(
        fluidRow(
          column(7,
          pickerInput(ns("frequenceSNOT"), label = translator$t("Fréquence"),
            choices = choicesFrequence,
            selected = "day",options = list(style = "btn-info"))),     
          column(5,offset = 0,style='left:0px;top:3px;',conditionalPanel(condition=paste0("input['",ns("frequenceSNOT"),"']=='hour'"),uiOutput(ns('frequenceHoraire')))
            ),
          column(5,offset = 0,style='left:0px;top:3px;',conditionalPanel(condition=paste0("input['",ns("frequenceSNOT"),"']=='month'"),uiOutput(ns('frequenceMensuelle')))
            ),
          column(5,offset = 0,style='left:0px;top:3px;',conditionalPanel(condition=paste0("input['",ns("frequenceSNOT"),"']=='year'"),uiOutput(ns('frequenceAnnuelle')))
            )
          ),
          data.step = 4,
          data.intro = includeMarkdown(paste0("inst/app/www/md/frequencySelected_",language,".md"))
        ),
        introBox(
          dropdownButton(icon = icon("hand-pointer"),width = "650%",circle = FALSE, status = "warning",label = translator$t("Sélection des variables"),
            column(3,
              checkboxGroupInput(ns("variableEC"),label="NULL",selected = NULL,choiceNames=NULL,choiceValues=NULL),
              checkboxGroupInput(ns("variableChambres"),label="NULL",selected = NULL,choiceNames=NULL,choiceValues=NULL)),
            column(3,checkboxGroupInput(ns("variableBM"),label="NULL",selected = NULL,choiceNames=NULL,choiceValues=NULL)),
            column(4,
              fluidPage(tweaks,fluidRow(column(width = 8, list(NULL,tags$div(align = 'left',class = 'multicol',
                checkboxGroupInput(ns("variableTS"),label="NULL",selected = NULL,choiceNames=NULL,choiceValues=NULL)))))),
              fluidPage(tweaks,fluidRow(column(width = 8, list(NULL,tags$div(align = 'left',class = 'multicol',
                checkboxGroupInput(ns("variableSWC"),label="NULL",selected = NULL,choiceNames=NULL,choiceValues=NULL)))))),
              fluidPage(tweaks,fluidRow(column(width = 8, list(NULL,tags$div(align = 'left',class = 'multicol',
                checkboxGroupInput(ns("variableG"),label="NULL",selected = NULL,choiceNames=NULL,choiceValues=NULL))))))),
              column(2,checkboxGroupInput(ns("variableWTD"),label="NULL",selected = NULL,choiceNames=NULL,choiceValues=NULL)),
              column(2,checkboxGroupInput(ns("variableBiogeo"),label="NULL",selected = NULL,choiceNames=NULL,choiceValues=NULL))
          ),
          data.step = 5,
          data.intro = includeMarkdown(paste0("inst/app/www/md/variableSelected_",language,".md"))
          )
      ),
      column(10,
      fluidRow(
         column(3,
          introBox(
            actionBttn(ns("go0"),icon = icon("sync"),style = "fill", color = "danger",label = translator$t("Mise à jour de la sélection"),size="sm"),
            data.step = 6,
            data.intro = includeMarkdown(paste0("inst/app/www/md/updateSelection_",language,".md"))
            )
          ),
         column(4,offset = 0,style='margin: 0px 0px 0px -50px',
          introBox(
          actionBttn(ns("updatefrequence0"),icon = icon("sync"),style = "fill", color = "danger",label = translator$t("Mise à jour de la fréquence et du type de jour"),size="sm"),
            data.step = 7,
            data.intro = includeMarkdown(paste0("inst/app/www/md/updateFrequency_",language,".md"))
          )
          ),
        column(4,
                dropdownButton(
                            actionBttn(ns('starthelp'), icon = icon("question-circle"),style = "bordered", color = "primary", label=translator$t("Utilisation de l'application web étape par étape"),size="sm"),
                            br(),
                            actionBttn(ns('videoDataAccess'), icon = icon("video"),style = "bordered", color = "primary", label=translator$t("Comment utiliser Data Access ?"),size="sm"),
                            br(),
                            actionBttn(ns('videoChart'), icon = icon("video"),style = "bordered", color = "primary", label=translator$t("Comment construire un graphique ?"),size="sm"),

                            width = "70%",icon=icon("question-circle"),circle=FALSE,label=translator$t("Aide"),status = "danger"
                )
        )
        ),
        uiOutput(ns('captionRecap')),
      (
        tabsetPanel(
        # Data 
          tabPanel(p(icon("info"),translator$t("Description de la sélection")),
            fluidRow(
              box(width=5,
              title = p(icon("map"),translator$t("Carte des stations")),solidHeader = TRUE,collapsible = TRUE,collapsed=FALSE,status = "primary"
              ,leafletOutput(ns("sensorMap"))),
              box(title = p(icon("table"),translator$t("Description des variables")),width=7,collapsible = TRUE,collapsed=FALSE,solidHeader = TRUE,status = "primary"
              ,DT::dataTableOutput(ns("tableData")))
              ),
            fluidRow(
              box(title = p(icon("table"),translator$t("Instruments & Méthodes")),width=12,collapsible = TRUE,collapsed=TRUE,solidHeader = TRUE,status = "warning"
              ,DT::dataTableOutput(ns("tableSensor")))
              )
              ),
          tabPanel(p(icon("table"),translator$t("Données")),
            withSpinner(DT::dataTableOutput(ns("Data")),type=5)),                       
          tabPanel(p(icon("line-chart"),translator$t("Série temporelle")),
            withSpinner(uiOutput(ns("timetrendSNOT")),type=5)),
          tabPanel(p(icon("calculator"),translator$t("Statistiques")),
            checkboxGroupInput(ns('facetWrapOption1'),label=translator$t("Détail par"),inline=TRUE,choices = c("month","season","year","dayNight"),selected=NULL),
            withSpinner(DT::dataTableOutput(ns("SummaryData")),type=5)# end of "chart" tab panel
            ),
          tabPanel(p(icon("chart-bar"),translator$t("Construire un graphique")),
                    esquisserUI(id = ns("esquisse"),header = FALSE,choose_data = FALSE)
              ),
          tabPanel(p(icon("download"),translator$t("Télécharger les données")),
            br(),
            fluidRow(
              column(3,
                dropdownButton(
                  includeMarkdown(paste0("inst/app/www/md/licence_",language,".md")),width = "200%",icon=icon("creative-commons"),circle=FALSE,label=translator$t("Résumé de la licence"),status = "info"
                )
              ),
              column(3,
                dropdownButton(
                  includeMarkdown(paste0("inst/app/www/md/citation_",language,".md")),width = "200%",icon=icon("quote-right"),circle=FALSE,label=translator$t("Comment citer ?"),status = "info"
                )
              )
              ),
            br(),
            fluidRow(
              column(3,
                dropdownButton(
                  includeMarkdown(paste0("inst/app/www/md/formatTable_",language,".md")),width = "200%",icon=icon("table"),circle=FALSE,label=translator$t("Format de la table"),status = "info"
                )
              ),
              column(3,
                dropdownButton(
                  includeMarkdown(paste0("inst/app/www/md/metadata_",language,".md")),width = "200%",icon=icon("file-archive"),circle=FALSE,label=translator$t("Contenu du zip"),status = "info"
                )
              ),
              column(3,
                dropdownButton(
                  includeMarkdown(paste0("inst/app/www/md/howToRead_",language,".md")),width = "200%",icon=icon("readme"),circle=FALSE,label=translator$t("Comment lire les données ?"),status = "warning"
                )
              )
              ),
            fluidRow(
              br(),
              column(3,
                radioGroupButtons(inputId = ns("tableType"),label = translator$t("Format de la table"),choices = c(translator$t("Verticale"),translator$t("Horizontale")),individual = TRUE)),
              column(3,br(),h6(""),useShinyalert(),actionButton(ns('downloadDataConfirmation'),icon= icon("download"),status="warning",label=translator$t("Télécharger")))
              ),
            hr(),
            tabBox(width=12,
              tabPanel(title=translator$t("Sélection des données pour le téléchargement"),DT::dataTableOutput(ns("Recap"))),
              tabPanel(title="DOI & Citations",DT::dataTableOutput(ns("Citation")),DT::dataTableOutput(ns("underCitation")))
              )
            )#tabpanel
        )#tabset
      )  
      )
    )
)
)
}

    
#' mod_accessdata Server Function
#'
#' @noRd 
mod_accessdata <- function(input, output, session,translationVariable){
  my_wd <- getwd()
  ns <- session$ns

#Database connexion and language selection
  language <- get_golem_options("language")
  pool <- get_golem_options("pool")
  translator <- shiny.i18n::Translator$new(translation_csvs_path = "inst/app/www/translation")
  translator$set_translation_language(language)
  
# Load data
  loadData <- loadCaracDataAndCarto(pool,language)
  caracData <- loadData[[1]]
  caracCarto <- loadData[[3]]
  col_station <- loadData[[4]]

#Help
hintjs(session, options = list("hintButtonLabel"="Hope this hint was helpful"),
         events = list("onhintclose"=I('alert("Wasn\'t that hint helpful")')))


# Help button start introjs when button is pressed with custom options and events
observeEvent(input$starthelp,
               introjs(session, options = list("nextLabel"=translator$t("Suivant"),
                                               "prevLabel"=translator$t("Précédent"),
                                               "skipLabel"=translator$t("Annuler")))
                                #events = list("oncomplete"=I('alert("Glad that is over")')))
  )

observeEvent(input$videoChart, {
    show_alert(
      title = NULL,
      text = tags$span(
              img(src="https://raw.githubusercontent.com/Rosalien/doc_snot/master/Videos/Demo_esquisse.gif", align = "center",height='100%',width='100%')
      ),
      html = TRUE,
      btn_labels = NA,
      closeOnClickOutside = TRUE,
      showCloseButton = FALSE,
      width = "80%"
    )
  })

observeEvent(input$videoDataAccess, {
    show_alert(
      title = NULL,
      text = tags$span(
              img(src="https://raw.githubusercontent.com/Rosalien/doc_snot/master/Videos/Demo_DataAccess.gif", align = "center",height='100%',width='100%')
      ),
      html = TRUE,
      btn_labels = NA,
      closeOnClickOutside = TRUE,
      showCloseButton = FALSE,
      width = "80%"
    )
  })


################################################renderUI####################################

# renderUI for frequencies
output$frequenceHoraire <- renderUI({
  numericInput(ns("hour"),label=h5(tags$b("")),value=1,min = 1, max = 23,step=1)
})

output$frequenceMensuelle <- renderUI({
  numericInput(ns("month"),label=h5(tags$b("")),value=1,min = 1, max = 12,step=1)
})

output$frequenceAnnuelle <- renderUI({
  numericInput(ns("year"),label=h5(tags$b("")),value=1,min = 1, max = 50,step=1)
})
#

# renderUI to display min and max period on site selected
## Filter on caracData with reactive siteSelectedVariable()
output$minmaxdateSNOT <- renderUI({
  minmaxdate <- caracData[code_site_station %in% siteSelectedVariable(),c(min(mindate,na.rm=TRUE),max(maxdate,na.rm=TRUE))]
  dateRangeInput(ns("dateSNOT"),translator$t("Période"),min = minmaxdate[1],max = minmaxdate[2],start = as.Date(minmaxdate[1],"%Y-%m-%d"),
                 end = as.Date(minmaxdate[2],"%Y-%m-%d"),format = "dd-mm-yyyy",language=language,separator = translator$t("au"))
})

# renderUI recap table of parameters selected
  output$captionRecap <- renderUI({
    captionRecap()
  })

# renderUI for time series dy_graph
output$timetrendSNOT <- renderUI({
      withProgress(message = translator$t("Préparation des données ..."),{
        
        incProgress(0.5,translator$t("Requête de la base de données ..."))       
        
        # Query on data
        subsetoutbdSNOT <- sqlOutputAndAggregateMean()

        incProgress(0.3,translator$t("Préparation du graphique ..."))
        dy_graphSite <- dygraphSite(subsetoutbdSNOT[(grepl("SWC_|TS_|G_|ETR|FC|H|LE|FCH4", subsetoutbdSNOT$variable)==FALSE) & !(subsetoutbdSNOT$variable %in% c("WTD","TW","GPP","RE","NEE")),],frequenceSelected())
        dy_graphTypeVariable <- dygraphTypeVariable(subsetoutbdSNOT[grepl("SWC_|TS_|G_|ETR|FC|H|LE|FCH4", subsetoutbdSNOT$variable)==TRUE,],frequenceSelected())       
        dy_graphChambrePiezo <- dygraphPiezo(subsetoutbdSNOT[subsetoutbdSNOT$variable %in% c("GPP","RE","NEE","TW","WTD"),],frequenceSelected())
        incProgress(0.1,translator$t("Finalisation ..."))

        tagList(dy_graphSite,dy_graphTypeVariable,dy_graphChambrePiezo)
      })
})

# renderUI to display a map for stations selected
  output$sensorMap <- renderLeaflet({
    mapSensorSelected <- unique(caracCarto[code_site_station %in% siteSelectedVariable(),
                                list(code_site_station,zet_coordonnees_bbox,station_description,datatype)])
    # Add colors for stations types
    mapSensorSelected <- merge(mapSensorSelected, col_station, by.x = "datatype", by.y = "datatype", all.x = TRUE,all.y=FALSE)

    validate(
      need(nrow(mapSensorSelected)>0,translator$t("Sélectionner une station et/ou un piézomètre et/ou une chambre"))
      )

    # Map creation
    sensorSelectedMap(mapSensorSelected,FALSE,translator)
})

# renderUI for description table of variables selected
  output$tableData <- DT::renderDataTable({
    descriptionData <- unique(caracData[code_site_station %in% siteSelected() & variable %in% variableSelected(),list(code_site_station,station_description,variable,definition,unite)])
    names(descriptionData) <- c("Site/Station","Station","Variable","Description",translator$t("Unité"))
    
    retData <- DT::datatable(descriptionData,extensions = 'Buttons',rownames= FALSE,filter = 'top',
      options = list(dom = 'tip',pageLength = 10, autoWidth = TRUE))
    return(retData)
})#

# renderUI for sensors and variables
  output$tableSensor <- DT::renderDataTable({
    tableSensor <- recapTable()[,list(code_site_station,station_nom,variable,instrument,description_capteur,zet_coordonnees_bbox,description_methode)]
    names(tableSensor) <- c("Site/Station","Station","Variable",translator$t("Instrument"),"Description","Coord",translator$t("Méthode"))
    
    retData <- DT::datatable(tableSensor,extensions = 'Buttons',rownames= FALSE,filter = 'top',
      options = list(dom = 'tip',pageLength = 10, autoWidth = TRUE))
    return(retData)
})#

# renderUI to see data
  output$Data <- DT::renderDataTable({
    withProgress(message = translator$t("Préparation des données ..."),{
      incProgress(0.7,translator$t("Préparation de la table ..."))       
      tableData <- sqlOutputAndAggregateMean()[,list(code_site_station,Date,variable,definition,value)]
      retData <- DT::datatable(tableData,rownames= FALSE,filter = 'top')
      incProgress(0.3,translator$t("Fin de la préparation de la table ..."))       
    })
    return(retData)
})#

# renderUI to make a synthesis of queryr before download
  output$Recap <- DT::renderDataTable({
    recapTableDownload <- unique(recapTable()[,list(code_jeu,site_nom,station_nom,variable,definition,unite)])
    names(recapTableDownload) <- c(translator$t("Jeu de données"),"Site","Station","Variable","Description",translator$t("Unité"))
    retData <- DT::datatable(recapTableDownload,escape = FALSE,rownames= FALSE, 
                #caption = htmltools::tags$caption(
                  #style = 'caption-side: top; text-align: center;',
                  #htmltools::strong(translator$t("Sélection des données pour le téléchargement"))),
              options = list(dom = 'tip',pageLength = 10, autoWidth = TRUE))
    return(retData)   
})#

# renderUI for citations
  output$Citation <- DT::renderDataTable({
    CodeJeu <- unique(recapTable()[,code_jeu])
    Jeu <- tableJeu(pool)[code_jeu %in% CodeJeu,list(code_jeu,doi,citation)] 
    lapply(1:nrow(Jeu),function(x){
      if(!is.na(Jeu[x,doi])){
            Jeu[x,doi:=createLink(paste0("http://dx.doi.org/",doi))]
          }else{
            Jeu[x,doi:="No DOI"]
          }

      if(is.na(Jeu[x,citation])){
            Jeu[x,citation:="No citation, please add the sentence presented above in 'How to cite ?' box"]
          }
    })
    names(Jeu) <- c(translator$t("Jeu de données"),translator$t("DOI de toutes les versions"),"Citation")
    retData <- DT::datatable(Jeu,escape = FALSE,rownames= FALSE, 
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: center;',
                  htmltools::strong(translator$t("DOI et citations pour des jeux de données du SNO-Tourbières"))),
              options = list(dom = 'tip',pageLength = 10, autoWidth = TRUE))
    return(retData)
})#

# renderUI for citations for sub-dataset
output$underCitation <- DT::renderDataTable({
    CodeJeu <- unique(recapTable()[,code_jeu])
    SousJeu <- tableSousJeu(pool)[code_jeu %in% CodeJeu,list(code_jeu,doi,citation,date_debut,date_fin)] 
    names(SousJeu) <- c(translator$t("Fait partie de ce jeu de données"),"DOI","Citation",translator$t("Début"),translator$t("Fin"))
    SousJeu$DOI <- createLink(paste0("http://dx.doi.org/",SousJeu$DOI))
    retData <- DT::datatable(SousJeu,escape = FALSE,rownames= FALSE,
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: center;',
                  htmltools::strong(translator$t("DOI et citations pour les sous-jeux de données du SNO-Tourbières"))),
              options = list(dom = 'tip',pageLength = 10, autoWidth = TRUE))
    return(retData)
})#

# renderUI for esquisse module
 data_r <- reactiveValues(data=iris,name="dataSNOT")
  
  observeEvent(go(), {
        withProgress(message = translator$t("Préparation des données ..."),{
        incProgress(0.5,translator$t("Requête de la base de données ..."))    
        data_r$data <- sqlOutputAndAggregate()[,list(Date,code_site_station,value,variable,dayNight,month,season,year)]
      })
  })

 result <- callModule(module = esquisserServer,id = "esquisse",data = data_r)

# renderUI for data stats
  output$SummaryData <- DT::renderDataTable({

        if(!is.null(facetWrapSelectedTable())){
          #print("if#437")
          retData <- sqlOutputAndAggregate()[,list(Min=round(min(value,na.rm=TRUE),2),
                                 Q1=round(quantile(value, .25, na.rm=TRUE),2),
                                 Q2=round(median(value,na.rm=TRUE),2),
                                 M=round(mean(value,na.rm=TRUE),2),
                                 Q3=round(quantile(value, .75, na.rm=TRUE),2),
                                 Max=round(max(value, na.rm=TRUE),2),
                                 sd=round(sd(value, na.rm = TRUE),2)),
                                 by=c('variable','code_site_station',facetWrapSelectedTable())]
          names(retData) <- c("Variable","Site/Station",facetWrapSelectedTable(),"Min","Q1",translator$t("Médiane"),translator$t("Moyenne"),"Q3","Max",translator$t("Écart-type"))                      
        }else{
          #print("else#448")
          retData <- sqlOutputAndAggregate()[,list(Min=round(min(value,na.rm=TRUE),2),
                                 Q1=round(quantile(value, .25, na.rm=TRUE),2),
                                 Q2=round(median(value,na.rm=TRUE),2),
                                 M=round(mean(value,na.rm=TRUE),2),
                                 Q3=round(quantile(value, .75, na.rm=TRUE),2),
                                 Max=round(max(value, na.rm=TRUE),2),
                                 sd=round(sd(value, na.rm = TRUE),2)),
                                 by=c('variable','code_site_station')]
          names(retData) <- c("Variable","Site/Station","Min","Q1",translator$t("Médiane"),translator$t("Moyenne"),"Q3","Max",translator$t("Écart-type"))                      
        }
        retData <- DT::datatable(retData, rownames= FALSE,caption = htmltools::tags$caption(translator$t("Statistiques descriptives"),
          style='font-weight:bold; color:#333'), extensions = 'Buttons',filter = 'top',
          options = list(dom = "Blfrtip", buttons = list("copy",list(extend = "collection", buttons = c("csv", "excel", "pdf"),text = "Download")),
                    pageLength = 10, autoWidth = TRUE))
  return(retData)
})#

################################################End of renderUI####################################


################################################Reactive et Observe####################################
# eventReactive to select period (used for chart, table and extraction)
periodeSelected <- eventReactive(go(),{
  periode <- c(input$dateSNOT[1],input$dateSNOT[2])
  validate(need(!is.null(periode),translator$t("Veuillez sélectionner une période")))
  periode
})

# Reactive only used to update list of variable in widgets
siteSelectedVariable <- reactive({
  site <- c(input$siteSNOT_1,input$siteSNOT_2,input$siteSNOT_3,input$siteSNOT_4,
    input$sitePiezo_1,input$sitePiezo_2,input$sitePiezo_3,input$sitePiezo_4,
    input$siteChambre_1,input$siteChambre_2,input$siteChambre_3,input$siteChambre_4)
  site
})

# Reactive to update global selection
  go <- reactive({
    go <- input$go0
    go
  })

# Reactive utilisée pour les graphes, les tables et les extractions
siteSelected <- eventReactive(go(),{
  site <- c(input$siteSNOT_1,input$siteSNOT_2,input$siteSNOT_3,input$siteSNOT_4,
    input$sitePiezo_1,input$sitePiezo_2,input$sitePiezo_3,input$sitePiezo_4,
    input$siteChambre_1,input$siteChambre_2,input$siteChambre_3,input$siteChambre_4)
  validate(need(!is.null(site),translator$t("Veuillez sélectionner une station de mesure ou un piézomètre")))
  site
})

# eventReactive to update variable selected
variableSelected <- eventReactive(go(),{
    variables <- c(input$variableEC,input$variableChambres,input$variableSWC,input$variableG,input$variableTS,input$variableBM,input$variableWTD,input$variableBiogeo)
    validate(need(!is.null(variables),translator$t("Aucune variable sélectionnée. Veuillez sélectionner au moins une station et variable")))
    variables
  })

# reactiveValues for checked variables
variableChecked<- reactiveValues(checked=NULL)

# Reactive to update frequency
updateFrequence <- reactive({
    updatefrequence <- input$updatefrequence0
    updateFrequence
})  

# eventReactive for frequency selected
frequenceSelected <- eventReactive(updateFrequence(),{
  if(input$frequenceSNOT=="hour"){
    frequence <- paste(input$hour," hour",sep="")   
  }else if(input$frequenceSNOT=="month"){
    frequence <- paste(input$month," month",sep="")
    print(frequence)
  }else if(input$frequenceSNOT=="year"){
    frequence <- paste(input$year," year",sep="")
    print(frequence)
  }else{
    frequence <- input$frequenceSNOT    
  }
return(frequence)
})

# observeEvent to update checked variables
  observeEvent(go(),{
    variables <- c(input$variableEC,input$variableChambres,input$variableSWC,input$variableG,input$variableTS,input$variableBM,input$variableWTD,input$variableBiogeo)
    if(!is.null(variables)){
      variableChecked$checked <- variables
    }
})

# eventReactive link to update-button of frequency/dayNight
dayNightSelected <- eventReactive(updateFrequence(),{
  dayNightOption <- input$dayNight
  print(paste0("input$dayNight : ",input$dayNight))
  dayNightOption
})

# reactive to facetWrap option of stat table
facetWrapSelectedTable <- reactive({
    input$facetWrapOption1
})

# Deactivate download button when no variable selected
  observe({
    toggleState("downloadDataConfirmation", condition = !is.null(variableChecked$checked))
})

# Deactivate download button if confirm button is not selected
  observeEvent(input$readConfirmation,{
    toggleState("downloadData", condition = (input$readConfirmation==TRUE))
})

# Reactive for display a synthesis of the selection
  captionRecap <- reactive({
    caption <- paste0("Date : ",format(periodeSelected()[1],"%d-%m-%Y")," to ",format(periodeSelected()[2],"%d-%m-%Y"),
      " | Frequency : ",frequenceSelected()," | Type of day : ",dayNightSelected(), " | Sites/Stations : ",paste0(siteSelected(),collapse=", "), "| Variables : ",paste0(variableSelected(),collapse=", "))
    h5(style="color:#feab3a",HTML("<b>Summary of selection : </b>"),caption)
})

# Reactive to build a synthesis table before download
  recapTable <- reactive({
    recapTable <- unique(caracData[code_site_station %in% siteSelected() & variable %in% variableSelected(),])
    caracCarto[recapTable]
})

# eventReactive for run query + merge if clic on 'update selection'
  sqlOutputQuery <- eventReactive(go(),{
    print("Debut queryDataSNOT")
    # Run query + melt on data
    meltvalue <- queryDataSNOT(pool,variableSelected(),siteSelected(),periodeSelected())
    validate(need(nrow(meltvalue)>0,translator$t("Période d'analyse sans données")))
    print("Fin queryDataSNOT")
    print("Jointure avec caracData")
    # Merge with data.table (caracData[meltvalue])
    caracData[meltvalue]
})

# Reactive for sql query + aggregate
  sqlOutputAndAggregate <- reactive({
    print("---------Début sqlOutputAndAggregate ---------")
    print(paste0("dayNightSelected : ",dayNightSelected()))
    print(paste0("frequenceSelected : ",frequenceSelected()))
    print(paste0("siteSelected : ",siteSelected()))
    print(paste0("variableSelected : ",variableSelected()))
   
    # Data from query
    dataSNOT <- sqlOutputQuery()
    
    dbDayandNight <- dbDayNight(dataSNOT)
    print("dbselect de sqlOutputAndAggregate")
    dbDayandNightSelect <- dbselect(dbDayandNight,dayNightSelected(),frequenceSelected(),siteSelected(),variableSelected())           
    print("---------Fin sqlOutputAndAggregate ---------")
    dbDayandNightSelect
  })

# Reactive for sql query + aggregate + average day/night values (Used for extraction and dygrah)
  sqlOutputAndAggregateMean <- reactive({
    print("---------Lancement sqlOutputAndAggregateMean ---------")
    subsetoutbdAggregate <- sqlOutputAndAggregate()
    # Average calculation in case dayNightSelected()=="day/night"
        if(dayNightSelected()=="day/night"){
          subsetoutbdSNOT <- subsetoutbdAggregate[variable !="P_1_1_1",.(value = mean(value,na.rm=TRUE)), by = list(Date,variable,code_site_station,unite,definition,station_description,site_description,station_nom,site_nom)]
          subsetoutbdSNOT <- rbind(subsetoutbdSNOT,subsetoutbdAggregate[variable=="P_1_1_1" & complete.cases(value),.(value = sum(value,na.rm=TRUE)), by = list(Date,variable,code_site_station,unite,definition,station_description,site_description,station_nom,site_nom)])  
        }else{
          subsetoutbdSNOT <- subsetoutbdAggregate
        }
    print("---------Fin sqlOutputAndAggregateMean ---------")
    subsetoutbdSNOT
  })

dataModal <- function(failed = FALSE){
      modalDialog(
        title = translator$t("Licence & conditions d'utilisation des données du SNO-T"),
        span(paste0(translator$t('Sauf mentions contraires'),', ',translator$t('les données du SNO-T sont diffusées sous'))),
        tags$a(href=paste0("https://creativecommons.org/licenses/by-sa/4.0/deed.",language), "License: CC BY 4.0",target="_blank"),
        tags$br(),tags$br(),
        HTML('<center><a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/" target="_blank"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-sa/4.0/88x31.png" /></a></center>'),
        tags$hr(),
        tags$a(href="https://data-snot.cnrs.fr/snot/resources/manual/charte.pdf", translator$t("Conditions d'utilisation des données du SNO-T"),target="_blank"),
        checkboxInput(ns("readConfirmation"), translator$t("J'accepte les conditions d'utilisation"), FALSE),
        footer = tagList(
          modalButton(translator$t("Annuler")),
          downloadButton(ns("downloadData"), translator$t("Télécharger"))
        ),
        easyClose = TRUE
      )
}

# observeEvent to display condition when clic on download button
    observeEvent(input$downloadDataConfirmation, {
      showModal(dataModal())
    })
################################################End Reactive et Observe####################################


######################################Construction des checkin variables####################################

#########################################################################################
checkinEC <- reactiveValues(checked = NULL)
# Variables EC
  observe({
    input$variableEC
    isolate({
      if(!is.null(input$variableEC)){
          checkinEC$checked <- input$variableEC        
      }else{checkinEC$checked <- NULL}
    })
  })

  observe({ 
    listVariables <- unique(caracData[code_site_station %in% siteSelectedVariable() & grepl("ec1", caracData$code_site_station)==TRUE,list(variable,definition)])
    updateCheckboxGroupInput(session,
                         "variableEC", translator$t("Flux de GES par eddy-covariance"),
                         choiceValues = listVariables$variable,
                         choiceNames = listVariables$definition,
                         selected = checkinEC$checked)
 })
#########################################################################################
# List of chambers variables
checkinChambre <- reactiveValues(checked = NULL)
  observe({
    input$variableChambres
    isolate({
      if(!is.null(input$variableChambres)){
          checkinChambre$checked <- input$variableChambres
      }else{checkinChambre$checked <- NULL}
    })
  })

  observe({ 
    listVariables <- unique(caracData[code_site_station %in% siteSelectedVariable() & grepl("ch", caracData$code_site_station)==TRUE & grepl("ER|NEE|GPP", caracData$variable)==TRUE,list(variable,definition)])
      updateCheckboxGroupInput(session,
                           "variableChambres", translator$t("Flux de GES dans les chambres"),
                           choiceValues = listVariables$variable,
                           choiceNames = listVariables$definition,
                           selected = checkinChambre$checked)
 })

#########################################################################################
checkinBM <- reactiveValues(checked = NULL)
# Variables meteo
  observe({
    input$variableBM
    isolate({
      if(!is.null(input$variableBM)){
          checkinBM$checked <- input$variableBM
      }else{checkinBM$checked <- NULL}
    })
  })

  observe({ 
    listVariables <- unique(caracData[code_site_station %in% siteSelectedVariable() & grepl("bm", caracData$code_site_station)==TRUE & grepl("SWC|TS|G", caracData$variable)==FALSE,list(variable,definition)])
    updateCheckboxGroupInput(session,
                         "variableBM", translator$t("Météo"),
                         choiceValues = listVariables$variable,
                         choiceNames = listVariables$definition,
                         selected = checkinBM$checked)
 })

#########################################################################################
# List of WTD variables
checkinWTD <- reactiveValues(checked = NULL)
  observe({
    input$variableWTD
    isolate({
      if(!is.null(input$variableWTD)){
          checkinWTD$checked <- input$variableWTD
      }else{checkinWTD$checked <- NULL}
    })
  })

  observe({ 
    listVariables <- unique(caracData[code_site_station %in% siteSelectedVariable() & caracData$variable %in% c("WTD","TW"),list(variable,definition)])
      updateCheckboxGroupInput(session,
                           "variableWTD", translator$t("Hydrologie"),
                           choiceValues = listVariables$variable,
                           choiceNames = listVariables$definition,
                           selected = checkinWTD$checked)
 })

#########################################################################################
# List of Biogeo variables
checkinBiogeo <- reactiveValues(checked = NULL)
  observe({
    input$variableBiogeo 
    isolate({
      if(!is.null(input$variableBiogeo)){
          checkinBiogeo$checked <- input$variableBiogeo 
      }else{checkinBiogeo$checked <- NULL}
    })
  })

  observe({ 
    listVariables <- unique(caracData[code_site_station %in% siteSelectedVariable() & caracData$variable %in% c("FDOC","FPOC"),list(variable,definition)])
      updateCheckboxGroupInput(session,
                           "variableBiogeo", translator$t("Biogéochimie"),
                           choiceValues = listVariables$variable,
                           choiceNames = listVariables$definition,
                           selected = checkinBiogeo$checked)
 })

#########################################################################################
checkinSWC <- reactiveValues(checked = NULL)

# List of SWC variables
  observe({
    input$variableSWC
    isolate({
      if(!is.null(input$variableSWC)){
        checkinSWC$checked <- input$variableSWC
      }else{checkinSWC$checked <- NULL}
    })
  })

  observe({ 
    listVariables <- checkboxVariablePedo(dbCarac=caracData,codeVariable="SWC",siteBM=siteSelectedVariable())
    updateCheckboxGroupInput(session,
                         "variableSWC", translator$t("Teneur en eau du sol"),
                         choiceValues = listVariables$variable,
                         choiceNames = listVariables$definitionsimple,
                         selected = checkinSWC$checked)
 })
#########################################################################################
# List of TS variables
checkinTS <- reactiveValues(checked = NULL)#reactiveValues(EC = NULL,puts = NULL)

    observe({
    input$variableTS
    isolate({
      if(!is.null(input$variableTS)){
        checkinTS$checked <- input$variableTS
      }else{checkinTS$checked <- NULL}
    })
  })

  observe({ 
    listVariables <- checkboxVariablePedo(dbCarac=caracData,codeVariable="TS",siteBM=siteSelectedVariable())
    updateCheckboxGroupInput(session,
                         "variableTS", translator$t("Température du sol"),
                         choiceValues = listVariables$variable,
                         choiceNames = listVariables$definitionsimple,
                         selected = checkinTS$checked)
 })
#########################################################################################
# List of G variables
checkinG <- reactiveValues(checked = NULL)#reactiveValues(EC = NULL,puts = NULL)

observe({
    input$variableG
    isolate({
      if(!is.null(input$variableG)){
        checkinG$checked <- input$variableG
      }else{checkinG$checked <- NULL}
    })
  })

observe({ 
    listVariables <- checkboxVariablePedo(dbCarac=caracData,codeVariable="G_",siteBM=siteSelectedVariable())
    updateCheckboxGroupInput(session,
                         "variableG", translator$t("Flux de chaleur dans le sol"),
                         choiceValues = listVariables$variable,
                         choiceNames = listVariables$definitionsimple,
                         selected = checkinG$checked)
 })

######################################End of checkin variables construction####################################

##########################################Download button data###########################################

# Download button data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Data_SNOT_",format(Sys.time(), '%d_%m_%Y_%X'),".zip",sep="")
    },
    content = function(fname){

      withProgress(message = translator$t("Début de l'extraction ..."),{
      incProgress(0.1,translator$t("Requête de la base de données ..."))

      setwd(tempdir())
      csvDataFile <- paste0("Data_SNOTourbieres_",format(Sys.time(), '%d_%m_%Y_%X'),".csv")
      csvRecapFile <- paste0("Metadata_SNOTourbieres_",format(Sys.time(), '%d_%m_%Y_%X'),".csv")
      csvDOIFile <- paste0("DOIDataSet_SNOTourbieres_",format(Sys.time(), '%d_%m_%Y_%X'),".csv")

      extractionDataCpt <- sqlOutputAndAggregateMean()

      # Data extraction
      # Controler ici
      if(input$tableType==translator$t("Verticale")){
       # print("if#916")
        extractionDataCpt <- extractionDataCpt[,list(code_site_station,Date=as.character(Date),value,variable)]
        }else{
     #     print("else#916")
          extractionDataCpt <- extracData(extractionDataCpt,frequenceSelected())
      }

      incProgress(0.6,translator$t("Création de la métadonnée ..."))     
      # Recap table when download
      recapTableDownload <- recapTable()[,list(code_site_station,site_nom,station_nom,variable,definition,unite,fabricant,instrument,description_capteur,zet_coordonnees_bbox,description_methode)]
            
      # Recap DOI
      CodeJeu <- unique(recapTable()[,code_jeu])
      recapDOIJeu <- tableJeu(pool)[code_jeu %in% CodeJeu,list(code_jeu,doi,citation)] 
      names(recapDOIJeu) <- c(translator$t("Jeu de données"),translator$t("DOI de toutes les versions"),"Citation")

      incProgress(0.2,translator$t("Création du fichier ..."))
      write_csv(extractionDataCpt,csvDataFile)
      write_csv(recapDOIJeu,csvDOIFile)
      write_csv(recapTableDownload,csvRecapFile)
      incProgress(0.1,translator$t("Finalisation ..."))
      })#end of progress

      if (file.exists(paste0(fname, ".zip")))
        file.rename(paste0(fname, ".zip"), fname)  

    zip(zipfile=fname,files=c(csvDataFile,csvRecapFile,csvDOIFile))#unlist(metadata)
    
    setwd(my_wd)
    },
  contentType = "application/zip"
)


}#End module


  