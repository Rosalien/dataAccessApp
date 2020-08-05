#' Run the Shiny Application
#'
#' @param language String of language for application ("en" or "fr")
#' @param pool String of path of yaml file database configuration (ex. "~/SI_SNOT/dbconfProd.yaml")
#' 
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' @importFrom toolboxApps confConnexion

run_app <- function(language,pool
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui, 
      server = app_server
    ), 
    golem_opts = list(
    	language = language,
    	pool = confConnexion(pool)
    	)
  )
}
