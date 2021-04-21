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
    shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(title = "Explore Tree Data", titleWidth = 300),

      shinydashboard::dashboardSidebar(width = 300,
                                       shinydashboard::sidebarMenu(id = 'menu1',
                                         shinydashboard::menuItem(
                                           "Welcome",
                                           tabName = "get_started",
                                           icon = icon("door-open")
                                         ),
                                         shinydashboard::menuItem(
                                           "Explore Tree Stuff",
                                           tabName = "chm_cp",
                                           icon = icon("door-open")
                                         ))
                                       ),
      shinydashboard::dashboardBody(tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "font.css"),tags$style('.nav-tabs-custom .nav-tabs li.active {
    border-top-color: #000000;}"'),
    tags$style(HTML('/* body */ .content-wrapper, .right-side {background-color: #FFFFFF;}'))),
    shinydashboard::tabItems(
      shinydashboard::tabItem(
        tabName = "get_started",
        tags$style(type = 'text/css', '#swe_maps {height: calc(100vh - 250px) !important;}')),

      shinydashboard::tabItem(
        tabName = "chm_cp",
        tags$style(type = 'text/css', '#swe_maps {height: calc(100vh - 250px) !important;}'),
        fluidRow(
                               tabPanel('Explore CHM',
                                        shinydashboard::box(mod_panel_stats_ui("panel_stats_ui_1"), width = 4),
                                        shinydashboard::box(mod_panel_stat_plot_ui("panel_stat_plot_ui_1"), width = 8),
                                        fileInput('chm', 'select a file', accept = c('.asc', '.tif', '.png')))

 ))
    )
  )
    )
) #end tagList
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
      app_title = 'trexr'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

