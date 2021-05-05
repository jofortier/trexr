#' panel_3d_big UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_panel_3d_big_ui <- function(id){
  ns <- NS(id)
  tagList(
    rglwidget::rglwidgetOutput(ns("PLOT3D_big"))
  )
}

#' panel_3d_big Server Function
#'
#' @noRd
mod_panel_3d_big_server <- function(input, output, session){
  ns <- session$ns

}

## To be copied in the UI
# mod_panel_3d_big_ui("panel_3d_big_ui_1")

## To be copied in the server
# callModule(mod_panel_3d_big_server, "panel_3d_big_ui_1")

