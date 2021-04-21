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
mod_panel_stats_server <- function(input, output, session){
  ns <- session$ns



    output$summary <- shiny::renderTable({data.frame(x = seq(1,10,1))
      # NameExp<-c("Area (ha)","Hmax","Hmean","Hmin","Hmedian","Hvar","Hsd","Hcv","Hkurtosis","Hskewness")
      # MetricsExp<-c(round(area_ha,digits=2),#length(chm_hts),
      #               round(max(chm_hts),digits=2),
      #               round(mean(chm_hts),digits=2),
      #               round(min(chm_hts),digits=2),
      #               round(median(chm_hts),digits=2),
      #               round(var(chm_hts),digits=2),
      #               round(sd(chm_hts),digits=2),
      #               round(cv(chm_hts),digits=2),
      #               round(kurtosis(chm_hts),digits=2),
      #               round(skewness(chm_hts),digits=2))
      #
      # LiDARsummary0<-data.frame(cbind(NameExp,MetricsExp))
      # colnames(LiDARsummary0)<-c("Parameters", "Value")
      # #browser()
      # #Sys.sleep(1.5)
      # if(!exists("LiDARsummary")){
      #   LiDARsummary0
      # }

  })
}

## To be copied in the UI
# mod_panel_stats_ui("panel_stats_ui_1")

## To be copied in the server
# callModule(mod_panel_stats_server, "panel_stats_ui_1")

