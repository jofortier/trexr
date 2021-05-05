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
mod_panel_stats_server <- function(input, output, session, in_ras, clear_map){
  ns <- session$ns


  observeEvent(in_ras$chmR_rec, {

  in_ras$area_ha <- (raster::ncell(in_ras$chmR)*raster::res(in_ras$chmR)[1]^2)/10000
  chm_hts<-in_ras$chmR[!is.na(in_ras$chmR)]




  output$summary <- shiny::renderTable({
    validate(need(!is.null(in_ras$chmR), 'Waiting for Raster'))



    NameExp<-c("Area (ha)","Hmax","Hmean","Hmin","Hmedian","Hvar","Hsd","Hcv","Hkurtosis","Hskewness")
    MetricsExp<-c(round(in_ras$area_ha,digits=2),#length(chm_hts),
                  round(max(chm_hts),digits=2),
                  round(mean(chm_hts),digits=2),
                  round(min(chm_hts),digits=2),
                  round(median(chm_hts),digits=2),
                  round(var(chm_hts),digits=2),
                  round(sd(chm_hts),digits=2),
                  round(cv(chm_hts),digits=2),
                  round(kurtosis(chm_hts),digits=2),
                  round(skewness(chm_hts),digits=2))

    LiDARsummary0<-data.frame(cbind(NameExp,MetricsExp))
    colnames(LiDARsummary0)<-c("Parameters", "Value")
    #browser()
    #Sys.sleep(1.5)
    if(!exists("LiDARsummary")){
      LiDARsummary0
    }
})

})
  observeEvent(in_ras$rec_feat, {
    in_ras$area_ha <- (raster::ncell(in_ras$ras_crop)*raster::res(in_ras$ras_crop)[1]^2)/10000
    chm_hts<-in_ras$ras_crop[!is.na(in_ras$ras_crop)]

    output$summary <- shiny::renderTable({



      NameExp<-c("Area (ha)","Hmax","Hmean","Hmin","Hmedian","Hvar","Hsd","Hcv","Hkurtosis","Hskewness")
      MetricsExp<-c(round(in_ras$area_ha,digits=2),#length(chm_hts),
                    round(max(chm_hts),digits=2),
                    round(mean(chm_hts),digits=2),
                    round(min(chm_hts),digits=2),
                    round(median(chm_hts),digits=2),
                    round(var(chm_hts),digits=2),
                    round(sd(chm_hts),digits=2),
                    round(cv(chm_hts),digits=2),
                    round(kurtosis(chm_hts),digits=2),
                    round(skewness(chm_hts),digits=2))

      LiDARsummary0<-data.frame(cbind(NameExp,MetricsExp))
      colnames(LiDARsummary0)<-c("Parameters", "Value")
      #browser()
      #Sys.sleep(1.5)
      if(!exists("LiDARsummary")){
        LiDARsummary0
      }

})
  })
  observeEvent(clear_map(), {


    in_ras$area_ha <- (raster::ncell(in_ras$chmR_og)*raster::res(in_ras$chmR_og)[1]^2)/10000
    chm_hts<-in_ras$chmR_og[!is.na(in_ras$chmR_og)]


    output$summary <- shiny::renderTable({



      NameExp<-c("Area (ha)","Hmax","Hmean","Hmin","Hmedian","Hvar","Hsd","Hcv","Hkurtosis","Hskewness")
      MetricsExp<-c(round(in_ras$area_ha,digits=2),#length(chm_hts),
                    round(max(chm_hts),digits=2),
                    round(mean(chm_hts),digits=2),
                    round(min(chm_hts),digits=2),
                    round(median(chm_hts),digits=2),
                    round(var(chm_hts),digits=2),
                    round(sd(chm_hts),digits=2),
                    round(cv(chm_hts),digits=2),
                    round(kurtosis(chm_hts),digits=2),
                    round(skewness(chm_hts),digits=2))

      LiDARsummary0<-data.frame(cbind(NameExp,MetricsExp))
      colnames(LiDARsummary0)<-c("Parameters", "Value")
      #browser()
      #Sys.sleep(1.5)
      if(!exists("LiDARsummary")){
        LiDARsummary0
      }


  })

})
}

## To be copied in the UI
# mod_panel_stats_ui("panel_stats_ui_1")

## To be copied in the server
# callModule(mod_panel_stats_server, "panel_stats_ui_1")

