#' about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tabPanel includeMarkdown
#' 
mod_aboutUI <- function(id){
	ns <- NS(id)
    tabPanel("About",
    	includeMarkdown("inst/app/www/md/about_en.md"))
}

#' about Server Function
#'
#' @noRd 
mod_about <- function(input, output, session){
  ns <- session$ns
}#Fin du module


