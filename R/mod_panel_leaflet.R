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
mod_panel_leaflet_server <- function(input, output, session, in_ras, clear_map, values){
  ns <- session$ns

  observeEvent(in_ras$chmR_rec, {
  output$leaf_map <- leaflet::renderLeaflet({
   validate(need(!is.null(in_ras$chmR), 'Waiting for Raster'))


 if(!is.null(in_ras$ras_crop)){
   myPal <- myColorRamp(c("blue","green","yellow","red"),0:255)
   val <- as.numeric(seq(raster::cellStats(in_ras$ras_crop, min), raster::cellStats(in_ras$ras_crop, max), 1))

   pal = leaflet::colorNumeric(myPal, val,
                               na.color = "transparent")

   base_map() %>% leaflet::addRasterImage(x = in_ras$ras_crop, group = 'Raster', colors = pal) %>%
     leaflet::addLegend(pal = pal, values = val, position = "bottomright") %>%
     leaflet::addLayersControl(overlayGroups = c('Raster', 'Hydrography'), baseGroups = c("Esri.WorldImagery",
                                                                                          "CartoDB.Positron",
                                                                                          "OpenStreetMap",
                                                                                          "CartoDB.DarkMatter",
                                                                                          "OpenTopoMap"))

 } else {
   myPal <- myColorRamp(c("blue","green","yellow","red"),0:255)
   val <- as.numeric(seq(raster::cellStats(in_ras$chmR, min), raster::cellStats(in_ras$chmR, max), 1))
   pal = leaflet::colorNumeric(myPal, val,
                               na.color = "transparent")
   base_map() %>% leaflet::addRasterImage(x = in_ras$chmR, group = 'Raster', colors = pal) %>%
      leaflet::addLegend(pal = pal, values = val, position = "bottomright") %>%
      leaflet.extras::addDrawToolbar(polylineOptions = F, circleOptions = F,circleMarkerOptions = F,
                                     rectangleOptions = leaflet.extras::drawRectangleOptions(repeatMode = TRUE),
                                     markerOptions = F,
                                     polygonOptions = leaflet.extras::drawPolygonOptions(repeatMode = TRUE)) %>%
      leaflet::addLayersControl(overlayGroups = c('Raster', 'Hydrography'), baseGroups = c("Esri.WorldImagery",
                                                                                                                 "CartoDB.Positron",
                                                                                                                 "OpenStreetMap",
                                                                                                                 "CartoDB.DarkMatter","OpenTopoMap"))



}
})
})


  #update map with user input
  shiny::observeEvent(input$leaf_map_draw_new_feature, {


values$sf <- NULL

  values$sf <- sf::st_sf(sf::st_sfc(crs = 4326))

    feat <- input$leaf_map_draw_new_feature
    coords <- unlist(feat$geometry$coordinates)
    coords <- matrix(coords, ncol = 2, byrow = T)


      new_sf <- isolate(sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(coords))), crs = sf::st_crs(4326)) %>% sf::st_as_sf())
      shiny::isolate(values$sf <- rbind(values$sf, new_sf))

      bbox <- shiny::reactive(values$sf %>% sf::st_transform(crs = raster::crs(in_ras$chmR)))

      sf_pt <- reactive(sf::st_centroid(values$sf))

      ras_crop <- raster::mask(in_ras$chmR, bbox())

intersection <- reactive(raster::intersect(raster::extent(in_ras$chmR), raster::extent(bbox())))

      if(is.null(intersection())) {

        output$leaf_map <- leaflet::renderLeaflet({



          myPal <- myColorRamp(c("blue","green","yellow","red"),0:255)
          val <- as.numeric(seq(raster::cellStats(in_ras$chmR, min), raster::cellStats(in_ras$chmR, max), 1))
          pal = leaflet::colorNumeric(myPal, val,
                                      na.color = "transparent")


          base_map() %>%
            leaflet::addRasterImage(x = in_ras$chmR, group = 'Raster', colors = myPal) %>%
            leaflet::addPopups(sf_pt()[[1]][[1]][[1]], sf_pt()[[1]][[1]][[2]], 'Please select within the raster area!',
                      options = leaflet::popupOptions(closeButton = TRUE)
            ) %>%
            leaflet.extras::addDrawToolbar(polylineOptions = F, circleOptions = F,circleMarkerOptions = F,
                                           rectangleOptions = leaflet.extras::drawRectangleOptions(repeatMode = TRUE),
                                           markerOptions = F,
                                           polygonOptions = leaflet.extras::drawPolygonOptions(repeatMode = TRUE)) %>%
            leaflet::addLayersControl(overlayGroups = c('Raster', 'Hydrography'), baseGroups = c("Esri.WorldImagery",
                                                                                                 "CartoDB.Positron",
                                                                                                 "OpenStreetMap",
                                                                                                 "CartoDB.DarkMatter",
                                                                                                 "OpenTopoMap"))

        })


      } else {

      in_ras$ras_crop <- ras_crop


      output$leaf_map <- leaflet::renderLeaflet({


        myPal <- myColorRamp(c("blue","green","yellow","red"),0:255)
        val <- as.numeric(seq(raster::cellStats(in_ras$ras_crop, min), raster::cellStats(in_ras$ras_crop, max), 1))
        pal = leaflet::colorNumeric(myPal, val,
                                    na.color = "transparent")
        chmR <- in_ras$ras_crop
        reschmR<-raster::res(chmR)[1]
        newst<-raster::extent(chmR)

        r1NaM <- is.na(raster::as.matrix(chmR))
        colNotNA <- which(colSums(r1NaM) != nrow(chmR))
        rowNotNA <- which(rowSums(r1NaM) != ncol(chmR))

        exst <- raster::extent(chmR, rowNotNA[1], rowNotNA[length(rowNotNA)],
                               colNotNA[1], colNotNA[length(colNotNA)])
        chmR <- raster::crop(chmR,exst)

        base_map() %>% leaflet::addRasterImage(x = chmR, group = 'Raster', colors = myPal) %>%
          leaflet::addLegend(pal = pal, values = val, position = "bottomright") %>%
          leaflet::addLayersControl(overlayGroups = c('Raster', 'Hydrography'), baseGroups = c("Esri.WorldImagery",
                                                                                               "CartoDB.Positron",
                                                                                               "OpenStreetMap",
                                                                                               "CartoDB.DarkMatter",
                                                                                               "OpenTopoMap"))



      })

      in_ras$rec_feat <- reactive(input$leaf_map_draw_new_feature)
}
    })


}


## To be copied in the UI
# mod_panel_leaflet_ui("panel_leaflet_ui_1")

## To be copied in the server
# callModule(mod_panel_leaflet_server, "panel_leaflet_ui_1")

