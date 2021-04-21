#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  callModule(mod_panel_stats_server, "panel_stats_ui_1")
  callModule(mod_panel_stat_plot_server, "panel_stat_plot_ui_1")
}
