#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinyjs hide show
#' @noRd
app_server <- function( input, output, session ) {
  	 Sys.sleep(5)  
     # Hide the loading message when the rest of the server function has executed
     shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")    
  	 shinyjs::show("app-content")
  	 callModule(mod_welcome,"mod_welcomeUI_1")
	 callModule(mod_accessdata,"mod_accessdataUI_1")
}

