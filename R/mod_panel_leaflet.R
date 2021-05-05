#' panel_leaflet UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom magrittr '%>%'
mod_panel_leaflet_ui <- function(id){
  ns <- NS(id)
  tagList(

    leaflet::leafletOutput(ns('leaf_map'))

  )
}

#' panel_leaflet Server Function
#'
#' @noRd
mod_panel_leaflet_server <- function(input, output, session, in_ras, clear_map){
  ns <- session$ns

 #cb <- colorBin(palette = myPal, bins = at, domain = at)


  observeEvent(in_ras$chmR_rec, {
  output$leaf_map <- leaflet::renderLeaflet({
   validate(need(!is.null(in_ras$chmR), 'Waiting for Raster'))


 myPal <- myColorRamp(c("blue","green","yellow","red"),0:255)
 val <- as.numeric(seq(raster::cellStats(in_ras$chmR, min), raster::cellStats(in_ras$chmR, max), 1))
 pal = leaflet::colorNumeric(myPal, val,
                    na.color = "transparent")


 if(!is.null(in_ras$ras_crop)){

   base_map() %>% leaflet::addRasterImage(x = in_ras$chmR, group = 'Raster', colors = pal) %>%
     leaflet::addLegend(pal = pal, values = val, position = "bottomright") %>%
     leaflet::addLayersControl(overlayGroups = c('Raster', 'Hydrography'), baseGroups = c("Esri.WorldImagery",
                                                                                          "CartoDB.Positron",
                                                                                          "OpenStreetMap",
                                                                                          "CartoDB.DarkMatter",
                                                                                          "OpenTopoMap"))

 } else {

    base_map() %>% leaflet::addRasterImage(x = in_ras$chmR, group = 'Raster', colors = pal) %>%
      leaflet::addLegend(pal = pal, values = val, position = "bottomright") %>%
      leaflet.extras::addDrawToolbar(polylineOptions = F, circleOptions = F,circleMarkerOptions = F,
                                     rectangleOptions = leaflet.extras::drawRectangleOptions(repeatMode = TRUE),
                                     markerOptions = leaflet.extras::drawMarkerOptions(repeatMode = TRUE),
                                     polygonOptions = leaflet.extras::drawPolygonOptions(repeatMode = TRUE)) %>%
      leaflet::addLayersControl(overlayGroups = c('Raster', 'Hydrography'), baseGroups = c("Esri.WorldImagery",
                                                                                                                 "CartoDB.Positron",
                                                                                                                 "OpenStreetMap",
                                                                                                                 "CartoDB.DarkMatter",
                                                                                                                 "OpenTopoMap"))
}
})
})
  shiny::observeEvent(clear_map(), {



    values$sf <- NULL

map_update <- shiny::reactive({


  myPal <- myColorRamp(c("blue","green","yellow","red"),0:255)
  val <- as.numeric(seq(raster::cellStats(in_ras$chmR_og, min), raster::cellStats(in_ras$chmR_og, max), 1))
  pal = leaflet::colorNumeric(myPal, val,
                              na.color = "transparent")

  base_map() %>% leaflet::addRasterImage(x = in_ras$chmR_og, group = 'Raster', colors = myPal)%>%
    leaflet::addLegend(pal = pal, values = val, position = "bottomright") %>%
    leaflet.extras::addDrawToolbar(polylineOptions = F, circleOptions = F,circleMarkerOptions = F,
                                   rectangleOptions = leaflet.extras::drawRectangleOptions(repeatMode = TRUE),
                                   markerOptions = leaflet.extras::drawMarkerOptions(repeatMode = TRUE),
                                   polygonOptions = leaflet.extras::drawPolygonOptions(repeatMode = TRUE)) %>%
    leaflet::addLayersControl(overlayGroups = c('Raster', 'Hydrography'), baseGroups = c("Esri.WorldImagery",
                                                                                         "CartoDB.Positron",
                                                                                         "OpenStreetMap",
                                                                                         "CartoDB.DarkMatter",
                                                                                         "OpenTopoMap"))
})

    output$leaf_map <- leaflet::renderLeaflet({

                        map_update()


    })

  })


  #store the sf in a reactiveValues
  values <- shiny::reactiveValues()
  values$sf <- sf::st_sf(sf::st_sfc(crs = 4326))

  #update map with user input
  shiny::observeEvent(input$leaf_map_draw_new_feature, {

    in_ras$rec_feat <- reactive(input$leaf_map_draw_new_feature)
    feat <- input$leaf_map_draw_new_feature
    coords <- unlist(feat$geometry$coordinates)
    coords <- matrix(coords, ncol = 2, byrow = T)


      new_sf <- isolate(sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(coords))), crs = sf::st_crs(4326)) %>% sf::st_as_sf())
      shiny::isolate(values$sf <- rbind(values$sf, new_sf))

      bbox <- shiny::reactive(values$sf %>% sf::st_transform(crs = raster::crs(in_ras$chmR)))

      ras_crop <- raster::mask(in_ras$chmR, bbox())

      in_ras$ras_crop <- ras_crop


      output$leaf_map <- leaflet::renderLeaflet({


        myPal <- myColorRamp(c("blue","green","yellow","red"),0:255)
        val <- as.numeric(seq(raster::cellStats(in_ras$ras_crop, min), raster::cellStats(in_ras$ras_crop, max), 1))
        pal = leaflet::colorNumeric(myPal, val,
                                    na.color = "transparent")

        base_map() %>% leaflet::addRasterImage(x = in_ras$ras_crop, group = 'Raster', colors = myPal) %>%
          leaflet::addLegend(pal = pal, values = val, position = "bottomright") %>%
          leaflet::addLayersControl(overlayGroups = c('Raster', 'Hydrography'), baseGroups = c("Esri.WorldImagery",
                                                                                               "CartoDB.Positron",
                                                                                               "OpenStreetMap",
                                                                                               "CartoDB.DarkMatter",
                                                                                               "OpenTopoMap"))



      })

    })

observeEvent(in_ras$ras_crop_rec,{
  output$leaf_map <- leaflet::renderLeaflet({

      myPal <- myColorRamp(c("blue","green","yellow","red"),0:255)
      val <- as.numeric(seq(raster::cellStats(in_ras$chmR, min), raster::cellStats(in_ras$chmR, max), 1))
      pal = leaflet::colorNumeric(myPal, val,
                                  na.color = "transparent")

      base_map() %>% leaflet::addRasterImage(x = in_ras$chmR, group = 'Raster', colors = myPal) %>%
        leaflet::addLegend(pal = pal, values = val, position = "bottomright") %>%
        leaflet::addLayersControl(overlayGroups = c('Raster', 'Hydrography'), baseGroups = c("Esri.WorldImagery",
                                                                                             "CartoDB.Positron",
                                                                                             "OpenStreetMap",
                                                                                             "CartoDB.DarkMatter",
                                                                                             "OpenTopoMap"))

})

    })

}


## To be copied in the UI
# mod_panel_leaflet_ui("panel_leaflet_ui_1")

## To be copied in the server
# callModule(mod_panel_leaflet_server, "panel_leaflet_ui_1")

