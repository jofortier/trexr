#' panel_stats UI Function
#'
#' @description A shiny Module that controls the stats section of the panels. This let's the user switch metric labels (feet/meters) but is also reactive
#' to the cropping feature in leaflet.
#'
#' @param id Internal parameters for {shiny}.
#'
#'
#' @importFrom shiny NS tagList
mod_panel_stats_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::tableOutput(ns('summary'))

  )
}

#' panel_stats Server Function
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @param in_ras A reactiveValues that contains numerous rasters.
#' @param clear_map This is a reactive input that will clear everything back to the beginning. Some checkboxs will not
#' be set back to original (feet/meters) but height and aoi will.
#' @param values A reactiveValues that stores the sf information when cropping leaflet map. This makes it possible to crop but also let's
#' the app know what metric to use (feet/meters).
#'
mod_panel_stats_server <- function(input, output, session, in_ras, clear_map, values){
  ns <- session$ns


  observeEvent(in_ras$chmR_rec, {

    if(is.null(in_ras$ras_crop)){

  in_ras$area_ha <- (raster::ncell(in_ras$chmR)*raster::res(in_ras$chmR)[1]^2)/10000
  chm_hts<-in_ras$chmR[!is.na(in_ras$chmR)]

  output$summary <- shiny::renderTable({

    NameExp<-c("Area (acres)","Hmax","Hmean","Hmin","Hmedian","Hvar","Hsd","Hcv","Hkurtosis","Hskewness")
    MetricsExp<-c(round(in_ras$area_ha*2.47105,digits=2),
                  paste0(round(max(chm_hts),digits=2), ' ', values$data_stat),
                  paste0(round(mean(chm_hts),digits=2), ' ', values$data_stat),
                  paste0(round(min(chm_hts),digits=2), ' ', values$data_stat),
                  paste0(round(median(chm_hts),digits=2), ' ', values$data_stat),
                  paste0(round(var(chm_hts),digits=2), ' ', values$data_stat),
                  paste0(round(sd(chm_hts),digits=2), ' ',values$data_stat),
                  paste0(round(cv(chm_hts),digits=2), ' ', values$data_stats),
                  paste0(round(kurtosis(chm_hts),digits=2), ' ', values$data_stat),
                  paste0(round(skewness(chm_hts),digits=2),' ', values$data_stat))

    LiDARsummary0<-data.frame(cbind(NameExp,MetricsExp))
    colnames(LiDARsummary0)<-c("Parameters", "Value")

    LiDARsummary0
})

    } else {

      in_ras$area_ha <- (raster::ncell(in_ras$ras_crop)*raster::res(in_ras$ras_crop)[1]^2)/10000
      chm_hts<-in_ras$ras_crop[!is.na(in_ras$ras_crop)]

      output$summary <- shiny::renderTable({


        NameExp<-c("Area (acres)","Hmax","Hmean","Hmin","Hmedian","Hvar","Hsd","Hcv","Hkurtosis","Hskewness")
        MetricsExp<-c(round(in_ras$area_ha*2.47105,digits=2),
                      paste0(round(max(chm_hts),digits=2), ' ', values$data_stat),
                      paste0(round(mean(chm_hts),digits=2), ' ', values$data_stat),
                      paste0(round(min(chm_hts),digits=2), ' ', values$data_stat),
                      paste0(round(median(chm_hts),digits=2), ' ', values$data_stat),
                      paste0(round(var(chm_hts),digits=2), ' ', values$data_stat),
                      paste0(round(sd(chm_hts),digits=2), ' ',values$data_stat),
                      paste0(round(cv(chm_hts),digits=2), ' ', values$data_stats),
                      paste0(round(kurtosis(chm_hts),digits=2), ' ', values$data_stat),
                      paste0(round(skewness(chm_hts),digits=2),' ', values$data_stat))

        LiDARsummary0<-data.frame(cbind(NameExp,MetricsExp))
        colnames(LiDARsummary0)<-c("Parameters", "Value")

        LiDARsummary0
      })


    }
})

  observeEvent(in_ras$rec_feat, {

    in_ras$area_ha <- (raster::ncell(in_ras$ras_crop)*raster::res(in_ras$ras_crop)[1]^2)/10000
    chm_hts<-in_ras$ras_crop[!is.na(in_ras$ras_crop)]

    output$summary <- shiny::renderTable({


      NameExp<-c("Area (acres)","Hmax","Hmean","Hmin","Hmedian","Hvar","Hsd","Hcv","Hkurtosis","Hskewness")
      MetricsExp<-c(round(in_ras$area_ha*2.47105,digits=2),
                    paste0(round(max(chm_hts),digits=2), ' ', values$data_stat),
                    paste0(round(mean(chm_hts),digits=2), ' ', values$data_stat),
                    paste0(round(min(chm_hts),digits=2), ' ', values$data_stat),
                    paste0(round(median(chm_hts),digits=2), ' ', values$data_stat),
                    paste0(round(var(chm_hts),digits=2), ' ', values$data_stat),
                    paste0(round(sd(chm_hts),digits=2), ' ',values$data_stat),
                    paste0(round(cv(chm_hts),digits=2), ' ', values$data_stats),
                    paste0(round(kurtosis(chm_hts),digits=2), ' ', values$data_stat),
                    paste0(round(skewness(chm_hts),digits=2),' ', values$data_stat))

      LiDARsummary0<-data.frame(cbind(NameExp,MetricsExp))
      colnames(LiDARsummary0)<-c("Parameters", "Value")

      LiDARsummary0
    })


  })

}

## To be copied in the UI
# mod_panel_stats_ui("panel_stats_ui_1")

## To be copied in the server
# callModule(mod_panel_stats_server, "panel_stats_ui_1")

