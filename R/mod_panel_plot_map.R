#' panel_plot_map UI Function
#'
#' @description A shiny Module defaults to a basic raster image if there is no projection on the fileInput raster.
#'
#' @param id Internal parameters for {shiny}.
#'
#'
#' @importFrom shiny NS tagList
mod_panel_plot_map_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns('plot_map'))
  )
}

#' panel_plot_map Server Function
#' @param input,output,session Internal parameters for {shiny}.
#' @param in_ras A reactiveValues that contains numerous rasters.
mod_panel_plot_map_server <- function(input, output, session, in_ras){
  ns <- session$ns
  output$plot_map <- renderPlot({
    validate(need(!is.null(in_ras$chmR), 'Waiting for Raster'))
    raster::plot(in_ras$chmR)

  })
}

## To be copied in the UI
# mod_panel_plot_map_ui("panel_plot_map_ui_1")

## To be copied in the server
# callModule(mod_panel_plot_map_server, "panel_plot_map_ui_1")

