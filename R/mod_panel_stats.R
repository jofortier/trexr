#' panel_stats UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
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
#' @noRd
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

