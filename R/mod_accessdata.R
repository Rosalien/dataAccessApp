#translator <- shiny.i18n::Translator$new(translation_csvs_path = "translation")
#translator$set_translation_language("en")
#language <- "en"

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
#' @import shinyWidgets
#' @importFrom shinyalert useShinyalert
#' @importFrom rintrojs introjsUI introBox hintjs introjs
#' @import esquisse
#' @import shinyjs
#' @import shinydashboard
#' 
mod_accessdataUI <- function(id,translationVariable){
  ns <- NS(id)
  language <- get_golem_options("language")
  pool <- get_golem_options("pool")

  translator <- shiny.i18n::Translator$new(translation_csvs_path = "inst/app/www/translation")
  translator$set_translation_language(language)

  # Pour mettre des checkboxs sur différentes colonnes
  tweaks <- list(tags$head(tags$style(HTML(tweaks2()))))
  
  # Chargement des données 
  caracData <- caracdata(pool,language)[order(variable),]

  # Création de plusieurs listes pour l'internatinoalisation
  choicesDayNight <- c("day/night","day","night")
  names(choicesDayNight) <- c(translator$t("Jour & Nuit"),translator$t("Jour"),translator$t("Nuit"))

  choicesFrequence <- c("30 min", "hour","day","week","month","year")
  names(choicesFrequence) <- c(translator$t("Infra-jour"),translator$t("Heure"),translator$t("Jour"),translator$t("Semaine"),translator$t("Mois"),translator$t("Année"))

# Fonction pour générer des checkboxGroupInput à la volée 
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
         tags$head(HTML(googleAnalyticsParameter())),
         tags$script(HTML("$(document).one('shiny:idle',function() {ga('set','userId', Shiny.user);});")),
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
              #checkboxInput(ns("variableWindRose"),label=translator$t("Rose des vents"),value=NULL)),
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
              ,dataTableOutput(ns("tableData")))
              ),
            fluidRow(
              box(title = p(icon("table"),translator$t("Instruments & Méthodes")),width=12,collapsible = TRUE,collapsed=TRUE,solidHeader = TRUE,status = "warning"
              ,dataTableOutput(ns("tableSensor")))
              )
              ),
          tabPanel(p(icon("table"),translator$t("Données")),
            withSpinner(dataTableOutput(ns("Data")),type=5)),                       
          tabPanel(p(icon("line-chart"),translator$t("Série temporelle")),
            withSpinner(uiOutput(ns("timetrendSNOT")),type=5)),
            #withSpinner(plotOutput(ns("windRoseggplotSite")),type=5)),
          tabPanel(p(icon("calculator"),translator$t("Statistiques")),
            checkboxGroupInput(ns('facetWrapOption1'),label=translator$t("Détail par"),inline=TRUE,choices = c("month","season","year","dayNight"),selected=NULL),
            withSpinner(dataTableOutput(ns("SummaryData")),type=5)# end of "chart" tab panel
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
              tabPanel(title=translator$t("Sélection des données pour le téléchargement"),dataTableOutput(ns("Recap"))),
              tabPanel(title="DOI & Citations",dataTableOutput(ns("Citation")),dataTableOutput(ns("underCitation")))
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
  language <- get_golem_options("language")
  pool <- get_golem_options("pool")
  translator <- shiny.i18n::Translator$new(translation_csvs_path = "inst/app/www/translation")
  translator$set_translation_language(language)
  
##################################Chargement des données####################################
# Mettre une fonction
  col_station <- read.csv("inst/app/www/csv/datatype_couleur.csv",sep=";",header=TRUE,stringsAsFactors=FALSE)
  caracDataSensor <- caracdata(pool,language)[order(variable)]
  
  variableCaracData <- c("code_jeu","code_site","code_station","code_site_station","site_nom","theme","datatype","variable","unite","mindate","maxdate","zet_coordonnees_bbox")
  metadataVariable <- c("definition","station_description","site_description","station_nom")
  metadataSensor <- c("fabricant","instrument","description_capteur","station_description")
  metadataMethod <- c("description_methode")

  caracData <- unique(caracDataSensor[,c(variableCaracData,metadataVariable),with=FALSE])
  caracCarto <- unique(caracDataSensor[,c(variableCaracData,metadataSensor,metadataMethod),with=FALSE])

  ## Sélection de la colonne en fonction de la langue
  col_station <- col_station[,c("datatype","couleur",paste0("type_",language))]
  names(col_station) <- c(names(col_station[1:2]),"type")

############################################################################################

################################################Help####################################

hintjs(session, options = list("hintButtonLabel"="Hope this hint was helpful"),
         events = list("onhintclose"=I('alert("Wasn\'t that hint helpful")')))


# Help button start introjs when button is pressed with custom options and events
observeEvent(input$starthelp,
               introjs(session, options = list("nextLabel"=i18n()$t("Suivant"),
                                               "prevLabel"=i18n()$t("Précédent"),
                                               "skipLabel"=i18n()$t("Annuler")))
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

# renderUI pour les fréquences
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

# renderUI pour afficher la période min et max des sites sélectionnés.
# Filtre sur caracData avec la reactive siteSelectedVariable()
output$minmaxdateSNOT <- renderUI({
  minmaxdate <- caracData[code_site_station %in% siteSelectedVariable(),c(min(mindate,na.rm=TRUE),max(maxdate,na.rm=TRUE))]
  dateRangeInput(ns("dateSNOT"),i18n()$t("Période"),min = minmaxdate[1],max = minmaxdate[2],start = as.Date(minmaxdate[1],"%Y-%m-%d"),
                 end = as.Date(minmaxdate[2],"%Y-%m-%d"),format = "dd-mm-yyyy",language=language,separator = i18n()$t("au"))
})

# renderUI pour les tables récapitulatifs des paramètres sélectionnés
  output$captionRecap <- renderUI({
    captionRecap()
  })

# renderUI pour les dy_graph
# Construction décomposée pour l'affichage des messages
output$timetrendSNOT <- renderUI({
      withProgress(message = i18n()$t("Préparation des données ..."),{
        
        incProgress(0.5,i18n()$t("Requête de la base de données ..."))       
        
        # Requête sur les données   
        subsetoutbdSNOT <- sqlOutputAndAggregateMean()

        incProgress(0.3,i18n()$t("Préparation du graphique ..."))
        dy_graphSite <- dygraphSite(subsetoutbdSNOT[(grepl("SWC_|TS_|G_|ETR|FC|H|LE|FCH4", subsetoutbdSNOT$variable)==FALSE) & subsetoutbdSNOT$variable %!in% c("WTD","TW","GPP","RE","NEE"),],frequenceSelected())
        dy_graphTypeVariable <- dygraphTypeVariable(subsetoutbdSNOT[grepl("SWC_|TS_|G_|ETR|FC|H|LE|FCH4", subsetoutbdSNOT$variable)==TRUE,],frequenceSelected())       
        dy_graphChambrePiezo <- dygraphPiezo(subsetoutbdSNOT[subsetoutbdSNOT$variable %in% c("GPP","RE","NEE","TW","WTD"),],frequenceSelected())
        incProgress(0.1,i18n()$t("Finalisation ..."))

        tagList(dy_graphSite,dy_graphTypeVariable,dy_graphChambrePiezo)
      })
})

# renderUI pour la création de la carte des stations sélectionnées
  output$sensorMap <- renderLeaflet({
    mapSensorSelected <- unique(caracCarto[code_site_station %in% siteSelectedVariable(),
                                list(code_site_station,zet_coordonnees_bbox,station_description,datatype)])
    # Ajout des couleurs pour les types de stations
    mapSensorSelected <- merge(mapSensorSelected, col_station, by.x = "datatype", by.y = "datatype", all.x = TRUE,all.y=FALSE)

    validate(
      need(nrow(mapSensorSelected)>0,translator$t("Sélectionner une station et/ou un piézomètre et/ou une chambre"))
      )

    # Création de la map
    sensorSelectedMap(mapSensorSelected,FALSE,translator)
})

# renderUI pour la table de description des variables sélectionnées
  output$tableData <- DT::renderDataTable({
    descriptionData <- unique(caracData[code_site_station %in% siteSelected() & variable %in% variableSelected(),list(code_site_station,station_description,variable,definition,unite)])
    names(descriptionData) <- c("Site/Station","Station","Variable","Description",i18n()$t("Unité"))
    
    retData <- DT::datatable(descriptionData,extensions = 'Buttons',rownames= FALSE,filter = 'top',
      options = list(dom = 'tip',pageLength = 10, autoWidth = TRUE))
    return(retData)
})#

# renderUI pour les instruments et les variables
  output$tableSensor <- DT::renderDataTable({
    tableSensor <- recapTable()[,list(code_site_station,station_nom,variable,instrument,description_capteur,zet_coordonnees_bbox,description_methode)]
    names(tableSensor) <- c("Site/Station","Station","Variable",i18n()$t("Instrument"),"Description","Coord",i18n()$t("Méthode"))
    
    retData <- DT::datatable(tableSensor,extensions = 'Buttons',rownames= FALSE,filter = 'top',
      options = list(dom = 'tip',pageLength = 10, autoWidth = TRUE))
    return(retData)
})#

# renderUI pour voir les données
  output$Data <- DT::renderDataTable({
    withProgress(message = i18n()$t("Préparation des données ..."),{
      incProgress(0.7,i18n()$t("Préparation de la table ..."))       
      tableData <- sqlOutputAndAggregateMean()[,list(code_site_station,Date,variable,definition,value)]
      retData <- DT::datatable(tableData,rownames= FALSE,filter = 'top')
      incProgress(0.3,i18n()$t("Fin de la préparation de la table ..."))       
    })
    return(retData)
})#

# renderUI pour faire la synthèse de la requête avant le téléchargement
  output$Recap <- DT::renderDataTable({
    recapTableDownload <- unique(recapTable()[,list(code_jeu,site_nom,station_nom,variable,definition,unite)])
    names(recapTableDownload) <- c(i18n()$t("Jeu de données"),"Site","Station","Variable","Description",i18n()$t("Unité"))
    retData <- DT::datatable(recapTableDownload,escape = FALSE,rownames= FALSE, 
                #caption = htmltools::tags$caption(
                  #style = 'caption-side: top; text-align: center;',
                  #htmltools::strong(i18n()$t("Sélection des données pour le téléchargement"))),
              options = list(dom = 'tip',pageLength = 10, autoWidth = TRUE))
    return(retData)   
})#

# renderUI pour la synthèse des citations principales
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
    names(Jeu) <- c(i18n()$t("Jeu de données"),i18n()$t("DOI de toutes les versions"),"Citation")
    retData <- DT::datatable(Jeu,escape = FALSE,rownames= FALSE, 
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: center;',
                  htmltools::strong(i18n()$t("DOI et citations pour des jeux de données du SNO-Tourbières"))),
              options = list(dom = 'tip',pageLength = 10, autoWidth = TRUE))
    return(retData)
})#

# renderUI pour la synthèse des citations des sous-jeux de données
output$underCitation <- DT::renderDataTable({
    CodeJeu <- unique(recapTable()[,code_jeu])
    SousJeu <- tableSousJeu(pool)[code_jeu %in% CodeJeu,list(code_jeu,doi,citation,date_debut,date_fin)] 
    names(SousJeu) <- c(i18n()$t("Fait partie de ce jeu de données"),"DOI","Citation",i18n()$t("Début"),i18n()$t("Fin"))
    SousJeu$DOI <- createLink(paste0("http://dx.doi.org/",SousJeu$DOI))
    retData <- DT::datatable(SousJeu,escape = FALSE,rownames= FALSE,
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: center;',
                  htmltools::strong(i18n()$t("DOI et citations pour les sous-jeux de données du SNO-Tourbières"))),
              options = list(dom = 'tip',pageLength = 10, autoWidth = TRUE))
    return(retData)
})#

# Rose des vents
    #output$windRoseggplotSite <- renderPlot({
      #if(WindRose()==TRUE){
        #subsetoutbdSNOT <- sqlOutputSiteStationWind
        #windRose <- graphWindRose(sqlOutputSiteStationWind())
        #do.call(grid_arrange_shared_legend,c(windRose,list(position="right")))
      #}else{}
#})

# renderUI pour la construction des graphiques à façon
 data_r <- reactiveValues(data=iris,name="dataSNOT")
  
  observeEvent(go(), {
        withProgress(message = i18n()$t("Préparation des données ..."),{
        incProgress(0.5,i18n()$t("Requête de la base de données ..."))    
        data_r$data <- sqlOutputAndAggregate()[,list(Date,code_site_station,value,variable,dayNight,month,season,year)]
      })
  })

 result <- callModule(module = esquisserServer,id = "esquisse",data = data_r)

# renderUI pour les stats sur les données
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
          names(retData) <- c("Variable","Site/Station",facetWrapSelectedTable(),"Min","Q1",i18n()$t("Médiane"),i18n()$t("Moyenne"),"Q3","Max",i18n()$t("Écart-type"))                      
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
          names(retData) <- c("Variable","Site/Station","Min","Q1",i18n()$t("Médiane"),i18n()$t("Moyenne"),"Q3","Max",i18n()$t("Écart-type"))                      
        }
        retData <- DT::datatable(retData, rownames= FALSE,caption = htmltools::tags$caption(i18n()$t("Statistiques descriptives"),
          style='font-weight:bold; color:#333'), extensions = 'Buttons',filter = 'top',
          options = list(dom = "Blfrtip", buttons = list("copy",list(extend = "collection", buttons = c("csv", "excel", "pdf"),text = "Download")),
                    pageLength = 10, autoWidth = TRUE))
  return(retData)
})#

################################################Fin des renderUI####################################


################################################Reactive et Observe####################################

# Reactive pour le changement de langue, en test pour le moment
i18n <- reactive({
  selectedLanguage <- "en"#input$selected_language
  translator$set_translation_language(selectedLanguage)
  print(selectedLanguage)
  translator
})

# obserEvent sur le rafraichissement de la page
#observeEvent(input$refresh, {
      #shinyjs::reset("form")
#})

# eventReactive pour la sélection de la période de temps.
# Utilisée pour les graphes, les tables et les extractions
periodeSelected <- eventReactive(go(),{
  periode <- c(input$dateSNOT[1],input$dateSNOT[2])
  validate(need(!is.null(periode),i18n()$t("Veuillez sélectionner une période")))
  periode
})

# Reactive utilisée uniquement pour mettre à jour la liste des variables dans les widgets
siteSelectedVariable <- reactive({
  site <- c(input$siteSNOT_1,input$siteSNOT_2,input$siteSNOT_3,input$siteSNOT_4,
    input$sitePiezo_1,input$sitePiezo_2,input$sitePiezo_3,input$sitePiezo_4,
    input$siteChambre_1,input$siteChambre_2,input$siteChambre_3,input$siteChambre_4)
  site
})

# Reactive pour mettre à jour la sélection globale
  go <- reactive({
    go <- input$go0
    go
  })

# Reactive utilisée pour les graphes, les tables et les extractions
siteSelected <- eventReactive(go(),{
  site <- c(input$siteSNOT_1,input$siteSNOT_2,input$siteSNOT_3,input$siteSNOT_4,
    input$sitePiezo_1,input$sitePiezo_2,input$sitePiezo_3,input$sitePiezo_4,
    input$siteChambre_1,input$siteChambre_2,input$siteChambre_3,input$siteChambre_4)
  validate(need(!is.null(site),i18n()$t("Veuillez sélectionner une station de mesure ou un piézomètre")))
  site
})

# eventReactive pour mettre à jour les variables sélectionnées
variableSelected <- eventReactive(go(),{
    variables <- c(input$variableEC,input$variableChambres,input$variableSWC,input$variableG,input$variableTS,input$variableBM,input$variableWTD,input$variableBiogeo)
    validate(need(!is.null(variables),i18n()$t("Aucune variable sélectionnée. Veuillez sélectionner au moins une station et variable")))
    variables
  })

# reactiveValues pour les variables checkées
variableChecked<- reactiveValues(checked=NULL)

# Reactive pour mettre à jour la fréquence
updateFrequence <- reactive({
    updatefrequence <- input$updatefrequence0
    updateFrequence
})  

# eventReactive sur les fréquences sélectionnées
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

# observeEvent pour mettre à jour les variables checkées
  observeEvent(go(),{
    variables <- c(input$variableEC,input$variableChambres,input$variableSWC,input$variableG,input$variableTS,input$variableBM,input$variableWTD,input$variableBiogeo)
    if(!is.null(variables)){
      variableChecked$checked <- variables
    }
})

# eventReactive lié au bouton de mise à jour fréquence/dayNight
dayNightSelected <- eventReactive(updateFrequence(),{
  dayNightOption <- input$dayNight
  print(paste0("input$dayNight : ",input$dayNight))
  dayNightOption
})

# reactive pour le croisement des données de la table stats
facetWrapSelectedTable <- reactive({
    input$facetWrapOption1
})

# reactive pour le croisement des données des boxplots
#facetWrapSelectedChart <- reactive({
#    input$facetWrapOption2
#})

# Désactivation du bouton télécharger si aucune variable est sélectionnée
  observe({
    toggleState("downloadDataConfirmation", condition = !is.null(variableChecked$checked))
})

# Désactivation du téléchargement si le bouton confirmation n'est pas sélectionné
  observeEvent(input$readConfirmation,{
    toggleState("downloadData", condition = (input$readConfirmation==TRUE))
})

# Reactive pour récapituler la sélection
  captionRecap <- reactive({
    caption <- paste0("Date : ",format(periodeSelected()[1],"%d-%m-%Y")," to ",format(periodeSelected()[2],"%d-%m-%Y"),
      " | Frequency : ",frequenceSelected()," | Type of day : ",dayNightSelected(), " | Sites/Stations : ",paste0(siteSelected(),collapse=", "), "| Variables : ",paste0(variableSelected(),collapse=", "))
    h5(style="color:#feab3a",HTML("<b>Summary of selection : </b>"),caption)
})

# Reactive pour construire une table récapitulative avant téléchargement
  recapTable <- reactive({
    recapTable <- unique(caracData[code_site_station %in% siteSelected() & variable %in% variableSelected(),])
    caracCarto[recapTable]
})

# eventReactive pour lancer la requête + jointure uniquement si on clique sur 'update selection'
  sqlOutputQuery <- eventReactive(go(),{
    print("Debut queryDataSNOT")
    # Lancement de la requête + melt des données
    meltvalue <- queryDataSNOT(pool,variableSelected(),siteSelected(),periodeSelected())
    validate(need(nrow(meltvalue)>0,i18n()$t("Période d'analyse sans données")))
    print("Fin queryDataSNOT")
    print("Jointure avec caracData")
    # Jointure avec data.table (caracData[meltvalue])
    caracData[meltvalue]
})

# Reactive pour la requête sql + aggrégation
  sqlOutputAndAggregate <- reactive({
    print("---------Début sqlOutputAndAggregate ---------")
    print(paste0("dayNightSelected : ",dayNightSelected()))
    print(paste0("frequenceSelected : ",frequenceSelected()))
    print(paste0("siteSelected : ",siteSelected()))
    print(paste0("variableSelected : ",variableSelected()))
   
    # Données issues de la requête
    dataSNOT <- sqlOutputQuery()
    
    dbDayandNight <- dbDayNight(dataSNOT)
    print("dbselect de sqlOutputAndAggregate")
    dbDayandNightSelect <- dbselect(dbDayandNight,dayNightSelected(),frequenceSelected(),siteSelected(),variableSelected())           
    print("---------Fin sqlOutputAndAggregate ---------")
    dbDayandNightSelect
  })

# Reactive pour la requête sql + aggrégation + Moyenne des valeurs day et night
# (Utilisée pour l'extraction et les timeseries)
  # Controler cette reactive
  sqlOutputAndAggregateMean <- reactive({
    print("---------Lancement sqlOutputAndAggregateMean ---------")
    subsetoutbdAggregate <- sqlOutputAndAggregate()
    # Calcul de la moyenne des valeurs dans le cas dayNightSelected()=="day/night"
        if(dayNightSelected()=="day/night"){
          subsetoutbdSNOT <- subsetoutbdAggregate[variable !="P_1_1_1",.(value = mean(value,na.rm=TRUE)), by = list(Date,variable,code_site_station,unite,definition,station_description,site_description,station_nom,site_nom)]
          subsetoutbdSNOT <- rbind(subsetoutbdSNOT,subsetoutbdAggregate[variable=="P_1_1_1" & complete.cases(value),.(value = sum(value,na.rm=TRUE)), by = list(Date,variable,code_site_station,unite,definition,station_description,site_description,station_nom,site_nom)])  
        }else{
          subsetoutbdSNOT <- subsetoutbdAggregate
        }
    print("---------Fin sqlOutputAndAggregateMean ---------")
    subsetoutbdSNOT
  })

# Reactive pour sélectionner les variable de construction de la rose des vents
#  sqlOutputSiteStationWind <- reactive({
    #variableSelected <- c("WD_1_1_1","WS_1_1_1")

    # Lancement de la requête
    #meltvalue <- queryDataSNOT(pool,variableSelected,siteSelected(),periodeSelected())
    #validate(need(nrow(meltvalue)>0,"Période d'analyse sans données"))

    # Jointure avec data.table (caracData[meltvalue])
#    dataSelected <- caracData[meltvalue]

 #   valuecarac <- dbselect(dataSelected,dayNightSelected(),frequenceSelected(),siteSelected(),variableSelected)
  #  return(valuecarac)
#})#

dataModal <- function(failed = FALSE){
      modalDialog(
        title = i18n()$t("Licence & conditions d'utilisation des données du SNO-T"),
        span(paste0(i18n()$t('Sauf mentions contraires'),', ',i18n()$t('les données du SNO-T sont diffusées sous'))),
        tags$a(href=paste0("https://creativecommons.org/licenses/by-sa/4.0/deed.",language), "License: CC BY 4.0",target="_blank"),
        tags$br(),tags$br(),
        HTML('<center><a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/" target="_blank"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-sa/4.0/88x31.png" /></a></center>'),
        tags$hr(),
        tags$a(href="https://data-snot.cnrs.fr/snot/resources/manual/charte.pdf", i18n()$t("Conditions d'utilisation des données du SNO-T"),target="_blank"),
        checkboxInput(ns("readConfirmation"), i18n()$t("J'accepte les conditions d'utilisation"), FALSE),
        footer = tagList(
          modalButton(i18n()$t("Annuler")),
          downloadButton(ns("downloadData"), i18n()$t("Télécharger"))
        ),
        easyClose = TRUE
      )
}

# observeEvent pour viusalier les conditions de téléchargement lorsqu'on clique sur le bouton
    observeEvent(input$downloadDataConfirmation, {
      showModal(dataModal())
    })

# Reactive pour construire les métadonnées
  metadata_jeu <- reactive({
    # Construction du code Jeu avec caracData
    CodeJeu <- unique(caracData[code_site_station %in% siteSelected() & variable %in% variableSelected(),gsub("_[^_]+$","",code_jeu)])

    # Lancement de la requête curl pour chaque jdd
    lapply(CodeJeu,function(x){
      pivot_metadata <- robustCurl(httr::GET(paste("http://localhost:8081/rest/resources/pivot?codes_jeu=",x,sep=""),httr::timeout(60)))
      pivot_metadata <- ifelse((is.character(pivot_metadata))||(pivot_metadata$status_code==404),"Problème dans la génération des métadonnées",jsonlite::prettify(rawToChar(pivot_metadata$content)))
  })
})#
################################################Fin Reactive et Observe####################################


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
                         "variableEC", i18n()$t("Flux de GES par eddy-covariance"),
                         choiceValues = listVariables$variable,
                         choiceNames = listVariables$definition,
                         selected = checkinEC$checked)
 })
#########################################################################################
# Liste des variables des chambres
checkinChambre <- reactiveValues(checked = NULL)
# Variables piezo
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
                           "variableChambres", i18n()$t("Flux de GES dans les chambres"),
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
                         "variableBM", i18n()$t("Météo"),
                         choiceValues = listVariables$variable,
                         choiceNames = listVariables$definition,
                         selected = checkinBM$checked)
 })

#########################################################################################
# Liste des variables WTD
checkinWTD <- reactiveValues(checked = NULL)
# Variables piezo
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
                           "variableWTD", i18n()$t("Hydrologie"),
                           choiceValues = listVariables$variable,
                           choiceNames = listVariables$definition,
                           selected = checkinWTD$checked)
 })

#########################################################################################
# Liste des variables Biogeo
checkinBiogeo <- reactiveValues(checked = NULL)
# Variables Biogeo
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
                           "variableBiogeo", i18n()$t("Biogéochimie"),
                           choiceValues = listVariables$variable,
                           choiceNames = listVariables$definition,
                           selected = checkinBiogeo$checked)
 })

#########################################################################################
 #WindRose <- eventReactive(go(),{
  #  input$variableWindRose
  #})

  # reactiveValues
  #bm1Checked<- reactiveValues(checked=NULL)

  # observe event for updating the reactiveValues
  #observe({
   # siteBM <- match(siteSelectedVariable()[grepl("bm1",siteSelectedVariable())],siteSelectedVariable())
   # isolate({
#    if(length(siteBM)>0){
      #bm1Checked$checked <- siteBM
    #}else{bm1Checked$checked <- NULL}
  #})
  #})

#observe({
    #toggleState("variableWindRose", condition = !is.null(bm1Checked$checked))
  #})
#########################################################################################
checkinSWC <- reactiveValues(checked = NULL)#reactiveValues(EC = NULL,puts = NULL)

# List des variables SWC
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
                         "variableSWC", i18n()$t("Teneur en eau du sol"),
                         choiceValues = listVariables$variable,
                         choiceNames = listVariables$definitionsimple,
                         selected = checkinSWC$checked)
 })
#########################################################################################
# Liste des variables TS
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
                         "variableTS", i18n()$t("Température du sol"),
                         choiceValues = listVariables$variable,
                         choiceNames = listVariables$definitionsimple,
                         selected = checkinTS$checked)
 })
#########################################################################################
# Liste des variables G
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
                         "variableG", i18n()$t("Flux de chaleur dans le sol"),
                         choiceValues = listVariables$variable,
                         choiceNames = listVariables$definitionsimple,
                         selected = checkinG$checked)
 })

######################################Fin de la construction des checkin variables####################################

##########################################Bouton téléchargement des données###########################################

# Téléchargement des données
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Data_SNOT_",format(Sys.time(), '%d_%m_%Y_%X'),".zip",sep="")
    },
    content = function(fname){

      withProgress(message = i18n()$t("Début de l'extraction ..."),{
      incProgress(0.1,i18n()$t("Requête de la base de données ..."))

      setwd(tempdir())
      csvDataFile <- paste0("Data_SNOTourbieres_",format(Sys.time(), '%d_%m_%Y_%X'),".csv")
      csvRecapFile <- paste0("Metadata_SNOTourbieres_",format(Sys.time(), '%d_%m_%Y_%X'),".csv")
      csvDOIFile <- paste0("DOIDataSet_SNOTourbieres_",format(Sys.time(), '%d_%m_%Y_%X'),".csv")

      # Génération des métadonnées
      #CodeJeu <- unique(caracData[code_site_station %in% siteSelected() & variable %in% variableSelected(),code_jeu])

      extractionDataCpt <- sqlOutputAndAggregateMean()

      # Extraction des données
      # Controler ici
      if(input$tableType==i18n()$t("Verticale")){
       # print("if#916")
        extractionDataCpt <- extractionDataCpt[,list(code_site_station,Date=as.character(Date),value,variable)]
        }else{
     #     print("else#916")
          extractionDataCpt <- extracData(extractionDataCpt,frequenceSelected())
      }

      incProgress(0.6,i18n()$t("Création de la métadonnée ..."))     
      # Recap du téléchargement
      recapTableDownload <- recapTable()[,list(code_site_station,site_nom,station_nom,variable,definition,unite,fabricant,instrument,description_capteur,zet_coordonnees_bbox,description_methode)]
            
      # Recap des DOI
      CodeJeu <- unique(recapTable()[,code_jeu])
      recapDOIJeu <- tableJeu(pool)[code_jeu %in% CodeJeu,list(code_jeu,doi,citation)] 
      names(recapDOIJeu) <- c(i18n()$t("Jeu de données"),i18n()$t("DOI de toutes les versions"),"Citation")

      incProgress(0.2,i18n()$t("Création du fichier ..."))
      write.csv(extractionDataCpt,csvDataFile,quote=FALSE,row.names=FALSE)
      write.table(recapDOIJeu,csvDOIFile,quote=FALSE,row.names=FALSE,sep=";")
      write.table(recapTableDownload,csvRecapFile,quote=FALSE,row.names=FALSE,sep=";")
      incProgress(0.1,i18n()$t("Finalisation ..."))
      })#Fin de la progression

      if (file.exists(paste0(fname, ".zip")))
        file.rename(paste0(fname, ".zip"), fname)  

    zip(zipfile=fname,files=c(csvDataFile,csvRecapFile,csvDOIFile))#unlist(metadata)
    
    setwd(my_wd)
    },
  contentType = "application/zip"
)


}#Fin du module


  