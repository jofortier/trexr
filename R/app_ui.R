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
      shinydashboardPlus::dashboardHeader(
                                          titleWidth = 300,
                                          shinydashboardPlus::userOutput('trexr'),
                                          title = dashboardthemes::shinyDashboardLogo(
                                            theme = "blue_gradient",
                                            boldText = "trexr",
                                            mainText = "(App)",
                                            badgeText = "v0.1"
                                          )
                                      ),

      shinydashboard::dashboardSidebar(width = 300,
                               dashboardthemes::shinyDashboardThemes(theme = "blue_gradient"),

                                       shinydashboard::sidebarMenu(id = 'menu1',

                                         shinydashboard::menuItem(
                                           "Explore Canopy Height Model",
                                           tabName = "chm_cp",
                                           icon = icon("door-open")
                                         ),
                                       conditionalPanel(condition = "input.menu1 === 'chm_cp'",
                                                        fileInput("chm", "Please Select File",
                                                                  accept = c('.tif', '.asc', '.img'), multiple = TRUE),

                                                        div(style = "padding: 14px 14px; margin-top:-2.5em",
                                                            fluidRow(sliderInput("HTboxI",
                                                                    label = "Filter Canopy Height Range:",
                                                                    min = 0, max = 300, value = c(0, 300), sep = ""))),
                                                        tags$head(
                                                          tags$style(HTML(
                                                            ".checkbox {margin: 0}
        .checkbox p {margin: 0;}
        .shiny-input-container {margin-bottom: 0;
                                margin-top: 0;}
       "
                                                          ))),
                                                                checkboxInput('switch_fil', label = 'Filter Outside Range',
                                                                     value = FALSE)),     tags$div(class= "checkbox-filter",checkboxInput("zvalues", "Define Z-values",
                                                                     value = FALSE),
                                                        conditionalPanel(condition = "input.zvalues=='1'",
                                                                         radioButtons('lab_sel', 'Label (graphs & stats)', choices = c('Z', 'Feet', 'Meters'), selected = 'Z',
                                                                                      inline = TRUE),
                                                                         selectInput('zsel', 'Z value', choices = c('feet', 'meters')),
                                                                         conditionalPanel(condition = "input.zsel==='feet'",
                                                                                          checkboxInput("feet", "Convert Z-values from Feet to Meters",
                                                                                                        value = FALSE)),
                                                                         conditionalPanel(condition = "input.zsel==='meters'",
                                                                                          checkboxInput("met", "Convert Z-values from Meters to Feet",
                                                                                                        value = FALSE))
                                                        ),
                                                        checkboxInput("smooth3d",label =  "Smooth 3d-plot (focal mean)",
                                                                      value = FALSE),

                                                                         conditionalPanel(condition="input.smooth3d=='1'",
                                                                                              selectInput("sws", "Focal Size",
                                                                                                          choices = c("3x3","5x5","7x7","9x9"),selected="3x3")),
                                                        div(style="display:inline-block;width:32%;text-align: center;",actionButton('clear', "Fresh Start")),
                                                            div(style="display:inline-block;width:32%;text-align: center;",
                                                                actionButton('change_ht', 'Run (change height)'))
                                                       ),
                                       shinydashboard::menuItem(
                                         "About",
                                         tabName = "get_started",
                                         icon = icon("door-open")
                                       )),br(),
                               tags$head(tags$style("
                                   .center-usda {
  display: block;
  margin-left: auto;
  margin-right: auto;
  width: 90%;
}")), div(img(src="www/usda_usfs2.png", class = 'center-usda', height="150%", width="150%", align='center'))),

      shinydashboard::dashboardBody(tags$head(
        tags$link(rel = "shortcut icon", href = "hex-trexr.png")),
    shinydashboard::tabItems(
      shinydashboard::tabItem(
        tabName = "get_started"),

      shinydashboard::tabItem(
        tags$head(tags$style("
                             .shiny-notification {
             margin-left: -10px !important;
             height: 40px !important;
             border-color: black;
             }
             ")),
        tags$head(tags$style(".modal-dialog{ width:350px}")),
        tags$head(tags$style(".modal-body{ min-height:25px}")),
        tabName = "chm_cp",
        fluidRow(
          shinydashboard::tabBox(width = 12, id = 'tabchart',
                               tabPanel(title = 'Explore CHM', style = "height:92vh;",
                                        shinydashboard::box( uiOutput('logic') %>% shinycssloaders::withSpinner(), width = 6, title = 'Mapping'),
                                        shinydashboard::box( mod_panel_3d_ui("panel_3d_ui_1"), width = 6, title = '3D Mapping'),
                                        shinydashboard::box(mod_panel_stat_plot_ui("panel_stat_plot_ui_1"), width = 6,height = '500px', title = "Plotting",
                                                            column(width = 7,radioButtons('plot_rad', label = '', choices = list('Boxplot' = 'bp', 'Density' = 'dens',
                                                                                                                'Histogram' = 'hist'), selected = 'hist',
                                                                         inline = TRUE)),
                                                            column(width = 5,downloadButton(outputId = "csv",
                                                                           label = "Download Plot Data"))),
                                        shinydashboard::box(mod_panel_stats_ui("panel_stats_ui_1"), width = 6, height = '500px', title = 'Summary Stats')
                                       )

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
    ),
    shinyalert::useShinyalert(),
    shinyjs::useShinyjs()
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )

}

