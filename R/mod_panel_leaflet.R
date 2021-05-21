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
mod_panel_leaflet_server <- function(input, output, session, in_ras, clear_map, values, shape, clip){

   ns <- session$ns

   observeEvent(in_ras$chmR_rec, {

   req(in_ras$chmR)

   output$leaf_map <- leaflet::renderLeaflet({



    showNotification(ui = "loading map...")

isolate({
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
                                                                                          "OpenTopoMap"))%>%
     leaflet::addMeasure(
       position = "topright",
       primaryLengthUnit = "feet",
       primaryAreaUnit = "acres",
       activeColor = "#000000",
       completedColor = "#FF0000"
     )


   } else {
print('im here')
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
                                                                                                                 "CartoDB.DarkMatter","OpenTopoMap"))%>%
     leaflet::addMeasure(
       position = "topright",
       primaryLengthUnit = "feet",
       primaryAreaUnit = "acres",
       activeColor = "#000000",
       completedColor = "#FF0000") %>%
      leaflet::addControl(html = actionButton('crop', 'crop')) %>%
      leaflet::addControl(html = actionButton('shape', 'add shapefile'))
     # ) %>% leaflet::addEasyButton(
     #   leaflet::easyButton(id = 'edit-btn',
     #              states = list(
     #                leaflet::easyButtonState(
     #                  stateName = 'add-toolbar',
     #                  icon = icon('toggle-off'),
     #                  title = 'Edit',
     #                  onClick = htmlwidgets::JS("
     #      function(btn, map) {
     #        Shiny.onInputChange('edit_btn', 'TRUE');
     #        btn.state('remove-toolbar');
     #      }"
     #                  )
     #                ),
     #      leaflet::easyButtonState(
     #        stateName = 'remove-toolbar',
     #        icon = icon('toggle-on'),
     #        title = 'Editing',
     #        onClick = htmlwidgets::JS("
     #      function(btn, map) {
     #        Shiny.onInputChange('edit_btn', 'FALSE');
     #        btn.state('add-toolbar');
     #      }"
     #        )
     #      ))
     #   )
     # )



   }

 })

})




 })


   observe({
leaf_prox <- leaflet::leafletProxy('leaf_map')
# req(!is.null(shape$dat))

if(class(shape$dat)[[1]] %in% "SpatialPolygonsDataFrame") {
  print('hello')
     leaf_prox %>% leaflet::addPolygons(data = shape$dat, group = 'user_shape') %>%
    leaflet::addLayersControl(overlayGroups = c('Raster', 'Hydrography','user_shape'), baseGroups = c("Esri.WorldImagery",
                                                                                         "CartoDB.Positron",
                                                                                         "OpenStreetMap",
                                                                                         "CartoDB.DarkMatter",
                                                                                         "OpenTopoMap"))

} else if (class(shape$dat)[[1]] %in% 'SpatialPointsDataFrame'){

  leaf_prox %>% leaflet::addMarkers(data = shape$dat, group = 'user_shape') %>%
    leaflet::addLayersControl(overlayGroups = c('Raster', 'Hydrography','user_shape'), baseGroups = c("Esri.WorldImagery",
                                                                                                      "CartoDB.Positron",
                                                                                                      "OpenStreetMap",
                                                                                                      "CartoDB.DarkMatter",
                                                                                                      "OpenTopoMap"))

} else if (class(shape$dat)[[1]] %in% 'SpatialLinesDataFrame'){

  leaf_prox %>% leaflet::addPolylines(data = shape$dat, group = 'user_shape') %>%
    leaflet::addLayersControl(overlayGroups = c('Raster', 'Hydrography','user_shape'), baseGroups = c("Esri.WorldImagery",
                                                                                                      "CartoDB.Positron",
                                                                                                      "OpenStreetMap",
                                                                                                      "CartoDB.DarkMatter",
                                                                                                      "OpenTopoMap"))

    }
})



  #update map with user input
  shiny::observeEvent(clip(), {

    req(!is.null(input$leaf_map_draw_all_features))

  values$sf <- NULL

  values$sf <- sf::st_sf(sf::st_sfc(crs = 4326))

    feat <- input$leaf_map_draw_all_features

    if(feat$type %in% 'FeatureCollection'){

      sf_list <- list()
      for(i in 1:length(feat$features)){
        coords <- unlist(feat$features[[i]]$geometry$coordinates)
      coords <- matrix(coords, ncol = 2, byrow = T)

      new_sf <- isolate(sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(coords))), crs = sf::st_crs(4326)) %>% sf::st_as_sf())

      sf_list <- append(sf_list, new_sf)

      }

      new_sf <- do.call(rbind, sf_list)
      rownames(new_sf) <- NULL
      new_sf <- new_sf %>% data.frame() %>% sf::st_as_sf() %>% sf::st_set_crs(4326) %>%  sf::st_transform(4326)
      shiny::isolate(values$sf <- rbind(values$sf, new_sf))

    } else {

        coords <- unlist(feat$geometry$coordinates)
    coords <- matrix(coords, ncol = 2, byrow = T)


      new_sf <- isolate(sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(coords))), crs = sf::st_crs(4326)) %>% sf::st_as_sf())
      shiny::isolate(values$sf <- rbind(values$sf, new_sf))

    }



      bbox <- values$sf %>% sf::st_transform(crs = raster::crs(in_ras$chmR))

      sf_pt <- sf::st_centroid(values$sf)

      ras_crop <- raster::mask(in_ras$chmR, bbox)

      in_ras$ras_crop_og <- raster::mask(in_ras$chmR_og, bbox)

  intersection <- raster::intersect(raster::extent(in_ras$chmR), raster::extent(bbox))

      if(is.null(intersection)) {

        output$leaf_map <- leaflet::renderLeaflet({



          myPal <- myColorRamp(c("blue","green","yellow","red"),0:255)
          val <- as.numeric(seq(raster::cellStats(in_ras$chmR, min), raster::cellStats(in_ras$chmR, max), 1))
          pal = leaflet::colorNumeric(myPal, val,
                                      na.color = "transparent")


          base_map() %>%
            leaflet::addRasterImage(x = in_ras$chmR, group = 'Raster', colors = myPal) %>%
            leaflet::addPopups(sf_pt[[1]][[1]][[1]], sf_pt[[1]][[1]][[2]], 'Please select within the raster area!',
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
                                                                                                 "OpenTopoMap")) %>%
            leaflet::addMeasure(
              position = "topright",
              primaryLengthUnit = "feet",
              primaryAreaUnit = "acres",
              activeColor = "#000000",
              completedColor = "#FF0000"
            )

        })


      } else {
        isolate({
      output$leaf_map <- leaflet::renderLeaflet({


        myPal <- myColorRamp(c("blue","green","yellow","red"),0:255)
        val <- as.numeric(seq(raster::cellStats(ras_crop, min), raster::cellStats(ras_crop, max), 1))
        pal = leaflet::colorNumeric(myPal, val,
                                    na.color = "transparent")

        reschmR<-raster::res(ras_crop)[1]
        newst<-raster::extent(ras_crop)

        r1NaM <- is.na(raster::as.matrix(ras_crop))
        colNotNA <- which(colSums(r1NaM) != nrow(ras_crop))
        rowNotNA <- which(rowSums(r1NaM) != ncol(ras_crop))

        exst <- raster::extent(ras_crop, rowNotNA[1], rowNotNA[length(rowNotNA)],
                               colNotNA[1], colNotNA[length(colNotNA)])
        ras_crop <- raster::crop(ras_crop,exst)
        in_ras$ras_crop <- ras_crop
        base_map() %>% leaflet::addRasterImage(x = ras_crop, group = 'Raster', colors = myPal) %>%
          leaflet::addLegend(pal = pal, values = val, position = "bottomright") %>%
          leaflet::addLayersControl(overlayGroups = c('Raster', 'Hydrography'), baseGroups = c("Esri.WorldImagery",
                                                                                               "CartoDB.Positron",
                                                                                               "OpenStreetMap",
                                                                                               "CartoDB.DarkMatter",
                                                                                               "OpenTopoMap"))%>%
          leaflet::addMeasure(
            position = "topright",
            primaryLengthUnit = "feet",
            primaryAreaUnit = "acres",
            activeColor = "#000000",
            completedColor = "#FF0000"
          )



      })

      in_ras$rec_feat <- reactive(input$leaf_map_all_features)
})
      }

 })

}


## To be copied in the UI
# mod_panel_leaflet_ui("panel_leaflet_ui_1")

## To be copied in the server
# callModule(mod_panel_leaflet_server, "panel_leaflet_ui_1")

