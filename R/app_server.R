#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  output$trexr <-  shinydashboardPlus::renderUser({
    shinydashboardPlus::dashboardUser(
      name = "trexr",
      image = "www/hex-trexr.png"
    )
  })
  in_ras <- reactiveValues()
  ctg_reac <- reactiveValues()
  input_box <- reactiveValues()
  #in_tree <- reactiveValues()

  callModule(mod_in_file_server, "in_file_ui_1",
             in_ras = in_ras,
             feet = reactive(input$feet),
             clear_map = reactive(input$clear),
             HTboxI = reactive(input$HTboxI),
             file_path = reactive(input$chm),
             change_ht = reactive(input$change_ht),
             input_box = input_box)

  observeEvent(input$clear,{

    x <- input_box$box_og

    updateNumericInput(session, 'HTboxI', value = x)

  })

  callModule(mod_panel_stats_server, "panel_stats_ui_1",in_ras = in_ras, clear_map = reactive(input$clear))

  callModule(mod_panel_stat_plot_server, "panel_stat_plot_ui_1",in_ras = in_ras, clear_map = reactive(input$clear))

  output$logic <- renderUI({

    validate(need(!is.null(in_ras$chmR), 'Waiting for Raster'))
    if(is.na(in_ras$projectCHM)){


      mod_panel_plot_map_ui("panel_plot_map_ui_1")

    } else {


      mod_panel_leaflet_ui("panel_leaflet_ui_1")

    }

  })
  callModule(mod_panel_leaflet_server, "panel_leaflet_ui_1", in_ras = in_ras, clear_map = reactive(input$clear))
  callModule(mod_panel_plot_map_server, "panel_plot_map_ui_1", in_ras = in_ras)
  callModule(mod_panel_3d_server, "panel_3d_ui_1", in_ras = in_ras, clear_map = reactive(input$clear),
             sws = reactive(input$sws),
             smooth3d = reactive(input$smooth3d))
#  callModule(mod_cross_section_server, "cross_section_ui_1", las = reactive(input$las))
#  callModule(mod_las_select_server, "las_select_ui_1", ctg_reac = ctg_reac)

  callModule(mod_panel_3d_server, "panel_3d_big_ui_1", in_ras = in_ras, clear_map = reactive(input$clear),
             sws = reactive(input$sws),
             smooth3d = reactive(input$smooth3d))

  # callModule(mod_panel_tree_detection_stats_plot_server, "panel_tree_detection_stats_plot_ui_1", in_ras,
  #            HTboxI = reactive(input$HTboxI), sws = reactive(input$sws), fws = reactive(input$fws),clear_map = reactive(input$clear),
  #            fil_ty = reactive(input$filtertype), sig = reactive(input$Sigma), in_tree, run_but = reactive(input$run_but))
  #
  # callModule(mod_panel_tree_detection_stats_server, "panel_tree_detection_stats_ui_1",
  #            in_tree, Ang = reactive(input$Ang), ht1 = reactive(input$ht1), ht2 = reactive(input$ht2),
  #            ht3 = reactive(input$ht3), frv = reactive(input$frv), radiustype = reactive(input$radiustype),
  #            equation = reactive(input$equation), maxcrown = reactive(input$maxcrown),
  #            exclusion = reactive(input$exclusion), run_but = reactive(input$run_but), in_ras)
  #
  # callModule(mod_panel_tree_detection_leaflet_server, "panel_tree_detection_leaflet_ui_1", in_tree)
}
