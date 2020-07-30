#' extractiondataset UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_extractiondataset_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' extractiondataset Server Function
#'
#' @noRd 
mod_extractiondataset_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_extractiondataset_ui("extractiondataset_ui_1")
    
## To be copied in the server
# callModule(mod_extractiondataset_server, "extractiondataset_ui_1")
 
