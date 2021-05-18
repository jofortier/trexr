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
  values <- shiny::reactiveValues(data = NULL)
  points <- reactiveValues()



  callModule(mod_in_file_server, "in_file_ui_1",
             in_ras = in_ras,
             feet = reactive(input$feet),
             switch_fil = reactive(input$switch_fil),
             clear_map = reactive(input$clear),
             HTboxI = reactive(input$HTboxI),
             file_path = reactive(input$chm),
             change_ht = reactive(input$change_ht),
             input_box = input_box,
             values = values,
             zvalues = reactive(input$zvalues),
             zsel = reactive(input$zsel),
             met = reactive(input$met),
             lab_sel = reactive(input$lab_sel))

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
    updateCheckboxInput(session, "feet", "Convert Z-values from Feet to Meters",
                        value = FALSE)
    updateCheckboxInput(session, "met", "Convert Z-values from Meters to Feet",
                        value = FALSE)
    updateRadioButtons(session, 'lab_sel', 'Label (graphs & stats)', choices = c('Z', 'Feet', 'Meters'), selected = 'Z',
                       inline = TRUE)
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

  callModule(mod_panel_stats_server, "panel_stats_ui_1",in_ras = in_ras, clear_map = reactive(input$clear),
             values = values)

  callModule(mod_panel_stat_plot_server, "panel_stat_plot_ui_1",in_ras = in_ras, clear_map = reactive(input$clear),
             plot_rad = reactive(input$plot_rad), values = values)

  output$logic <- renderUI({

    req(in_ras$chmR_rec)

    if(is.na(in_ras$projectCHM)){


      mod_panel_plot_map_ui("panel_plot_map_ui_1")

    } else {


      mod_panel_leaflet_ui("panel_leaflet_ui_1")

    }

  })

  # points$pts <- reactive(input$selectedPoints)
  #
  #
  # observeEvent(input$selectedPoints,{
  #
  #
  #
  #   points$pts_new <- data.frame(lat = points$pts()[points$pts()>0], lng = points$pts()[points$pts()<0]) %>%
  #     sf::st_as_sf(coords = c('lng', 'lat'), crs = 4326)
  #
  #   output$downloadData <- downloadHandler(
  #     filename = function() {
  #       if(input$export_format == "shp"){
  #         paste0(input$export_filename, ".zip")
  #       }else if (input$export_format == "kml"){
  #         paste0(input$export_filename, ".kml")
  #       }
  #     },
  #     content = function(file) {
  #
  #       tmp.path <- dirname(file)
  #       name.base <- file.path(tmp.path, input$export_filename)
  #       #name.base <- file.path(tmp.path)
  #       name.glob <- paste0(name.base, ".*")
  #       name.shp  <- paste0(name.base, ".shp")
  #       name.zip  <- paste0(name.base, ".zip")
  #
  #       if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))
  #
  #       if(input$export_format == "shp"){
  #
  #         sf::st_write(points$pts_new, dsn = name.shp, layer = "shpExport",
  #                      driver = "ESRI Shapefile", quiet = TRUE)
  #
  #         zip::zipr(zipfile = name.zip, files = Sys.glob(name.glob))
  #         req(file.copy(name.zip, file))
  #
  #       } else if (input$export_format == "kml"){
  #         sf::st_write(points$pts_new, dsn = file, layer = "chm_kml", driver = "KML",
  #                      quiet = TRUE)
  #       }
  #     }
  #   )
  # })
  callModule(mod_panel_leaflet_server, "panel_leaflet_ui_1", in_ras = in_ras,
             clear_map = reactive(input$clear), values = values)
  callModule(mod_panel_plot_map_server, "panel_plot_map_ui_1", in_ras = in_ras)
  callModule(mod_panel_3d_server, "panel_3d_ui_1", in_ras = in_ras, clear_map = reactive(input$clear),
             sws = reactive(input$sws),
             smooth3d = reactive(input$smooth3d),
             values = values)

  session$onSessionEnded(function() {
    stopApp()
  })

}
