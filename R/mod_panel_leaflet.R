#' panel_leaflet UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_panel_leaflet_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' panel_leaflet Server Function
#'
#' @noRd 
mod_panel_leaflet_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_panel_leaflet_ui("panel_leaflet_ui_1")
    
## To be copied in the server
# callModule(mod_panel_leaflet_server, "panel_leaflet_ui_1")
 
