#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here

  options(rgl.useNULL = TRUE)
  output$trexr <-  shinydashboardPlus::renderUser({
    shinydashboardPlus::dashboardUser(
      name = "trexr",
      image = "www/hex-trexr.png"
    )
  })
  in_ras <- reactiveValues()
  ctg_reac <- reactiveValues()
  input_box <- reactiveValues()
  values <- shiny::reactiveValues()

  callModule(mod_in_file_server, "in_file_ui_1",
             in_ras = in_ras,
             feet = reactive(input$feet),
             switch_fil = reactive(input$switch_fil),
             clear_map = reactive(input$clear),
             HTboxI = reactive(input$HTboxI),
             file_path = reactive(input$chm),
             change_ht = reactive(input$change_ht),
             input_box = input_box,
             values = values)

  observeEvent(input$clear,{

    min_fil <- input_box$box_og1
    max_fil <- input_box$box_og2

    updateSliderInput(session, 'HTboxI', value = c(min_fil, max_fil))

  })

  observeEvent(input$feet,{


    min_fil <- input_box$box_og1
    max_fil <- input_box$box_og2

    updateSliderInput(session, 'HTboxI', value = c(min_fil, max_fil))

  })

  observeEvent(input$clear,{

    updateCheckboxInput(session, 'smooth3d', label = "Smooth 3d-plot (focal mean)", value = FALSE)
    updateCheckboxInput(session, 'switch_fil', label = 'Filter Outside Range', value = FALSE)
  })

  observeEvent(input$chm,{

    updateCheckboxInput(session, 'smooth3d', label = "Smooth 3d-plot (focal mean)", value = FALSE)
    updateCheckboxInput(session, 'switch_fil', label = 'Filter Outside Range', value = FALSE)
  })

  observeEvent(input$feet,{

    updateCheckboxInput(session, 'smooth3d', label = "Smooth 3d-plot (focal mean)", value = FALSE)
    updateCheckboxInput(session, 'switch_fil', label = 'Filter Outside Range', value = FALSE)
  })

  observeEvent(input$chm,{


    min_fil <- input_box$box_og1
    max_fil <- input_box$box_og2

    updateSliderInput(session, 'HTboxI', value = c(min_fil, max_fil))

  })

  callModule(mod_panel_stats_server, "panel_stats_ui_1",in_ras = in_ras, clear_map = reactive(input$clear))

  callModule(mod_panel_stat_plot_server, "panel_stat_plot_ui_1",in_ras = in_ras, clear_map = reactive(input$clear),
             plot_rad = reactive(input$plot_rad))

  output$logic <- renderUI({

    validate(need(!is.null(in_ras$chmR), 'Waiting for Raster'))
    if(is.na(in_ras$projectCHM)){


      mod_panel_plot_map_ui("panel_plot_map_ui_1")

    } else {


      mod_panel_leaflet_ui("panel_leaflet_ui_1")

    }

  })
  callModule(mod_panel_leaflet_server, "panel_leaflet_ui_1", in_ras = in_ras,
             clear_map = reactive(input$clear), values = values)
  callModule(mod_panel_plot_map_server, "panel_plot_map_ui_1", in_ras = in_ras)
  callModule(mod_panel_3d_server, "panel_3d_ui_1", in_ras = in_ras, clear_map = reactive(input$clear),
             sws = reactive(input$sws),
             smooth3d = reactive(input$smooth3d))


}
