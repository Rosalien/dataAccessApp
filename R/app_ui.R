#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    fluidPage(
      h1("dataAccessApp"),
      theme =shinytheme("flatly"),
      useShinyjs(),
      inlineCSS(appCSS()),
      # Loading message
      div(id = "loading-content",withSpinner(h4(translator$t("Chargement ...")),type=4)),
      hidden(
      div(
        id = "app-content",
        fluidPage(          
             titlePanel(
                    title="", windowTitle=translator$t("Visualisation & Extraction des donnée du SNO-T")
             )
          ),
        navbarPage(
          fluidRow(       
              div(img(src="https://raw.githubusercontent.com/Rosalien/doc_snot/master/Figures/logo_Tourbieres.jpg",height = "60px",style = "position: relative; top: -20px;left:0px;"), "")
          ),
        mod_welcomeUI("mod_welcomeUI_1"),
        mod_accessdataUI("mod_accessdataUI_1")
        )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'dataAccessApp'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

