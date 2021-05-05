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
      shinydashboardPlus::dashboardHeader(title = 'trexr (Tree Exploration in R)',titleWidth = 300,
                                          shinydashboardPlus::userOutput('trexr')
                                      ),

      shinydashboard::dashboardSidebar(width = 300,
                                       shinydashboard::sidebarMenu(id = 'menu1',
                                         shinydashboard::menuItem(
                                           "Welcome",
                                           tabName = "get_started",
                                           icon = icon("door-open")
                                         ),
                                         shinydashboard::menuItem(
                                           "Explore Canopy Height Model",
                                           tabName = "chm_cp",
                                           icon = icon("door-open")
                                         ),
                                       conditionalPanel(condition = "input.menu1 === 'chm_cp'",
                                                        fileInput("chm", "Please Select File",
                                                                  accept = c('.tif', '.asc', '.img')),
                                                        numericInput('HTboxI', "Please Enter a Tree Height", value = 0,
                                                                     min = 0, max = 255),
                                                        checkboxInput("feet", "Do you want to change meters (Z) to feet?",
                                                                      value = FALSE),
                                                                             checkboxInput("smooth3d", "Do you want to smooth 3d-plot?",
                                                                                           value = FALSE),
                                                                         conditionalPanel(condition="input.smooth3d=='1'",

                                                                                              selectInput("sws", "Pick a window size.",
                                                                                                          choices = c("3x3","5x5","7x7","9x9"),selected="3x3")),
                                                        actionButton('clear', "Fresh Start"),
                                                        actionButton('change_ht', 'Run (change height)')
                                       ))),

      shinydashboard::dashboardBody(tags$head(
        tags$link(rel = "shortcut icon", href = "hex-trexr.png")),
    shinydashboard::tabItems(
      shinydashboard::tabItem(
        tabName = "get_started"),

      shinydashboard::tabItem(
        tabName = "chm_cp",
        fluidRow(
          shinydashboard::tabBox(width = 12, id = 'tabchart',
                               tabPanel(title = 'Explore CHM', style = "height:92vh;",
                                        shinydashboard::box( uiOutput('logic') %>% shinycssloaders::withSpinner(), width = 6, title = 'Mapping'),
                                        shinydashboard::box( mod_panel_3d_ui("panel_3d_ui_1"), width = 6, title = '3D Mapping'),
                                        shinydashboard::box(mod_panel_stat_plot_ui("panel_stat_plot_ui_1"), width = 6, title = "Plotting"),
                                        shinydashboard::box(mod_panel_stats_ui("panel_stats_ui_1"), width = 6, title = 'Summary Stats')
                                       ),
                               tabPanel(title = '3d-big',
                                        column(12, align='center',mod_panel_3d_ui("panel_3d_big_ui_1") %>% shinycssloaders::withSpinner()))

                               )))

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
    favicon('hex-trexr', ext = 'png'),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'trexr'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

