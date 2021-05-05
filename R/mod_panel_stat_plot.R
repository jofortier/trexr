#' panel_stat_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom ggplot2 ggplot aes geom_col labs theme_bw coord_flip scale_fill_gradientn geom_point
mod_panel_stat_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
 shiny::plotOutput(ns('hist'))
  )
}

#' panel_stat_plot Server Function
#'
#' @noRd
mod_panel_stat_plot_server <- function(input, output, session,in_ras, clear_map){
  ns <- session$ns


  myPal <- myColorRamp(c("blue","green","yellow","red"),0:255)

  observeEvent(in_ras$chmR_rec, {

     f_all <- raster::hist(in_ras$chmR, breaks = 30, plot = F)
     dat <- data.frame(counts= f_all$counts,breaks = f_all$mids)


     output$hist <- shiny::renderPlot({
       ggplot(dat, aes(x = .data[['breaks']], y = .data[['counts']])) +
         geom_col(aes(fill = .data[['breaks']]), col = 'black') +
         theme_bw() + scale_fill_gradientn(colors = myPal)+
         coord_flip()
     })
})

  shiny::observeEvent(clear_map(), {


    f_all <- raster::hist(in_ras$chmR_og, breaks = 30, plot = F)
    dat <- data.frame(counts= f_all$counts,breaks = f_all$mids)

  output$hist <- shiny::renderPlot({
    ggplot(dat, aes(x = .data[['breaks']], y = .data[['counts']])) +
      geom_col(aes(fill = .data[['breaks']]), col = 'black') +
      theme_bw() + scale_fill_gradientn(colors = myPal)+
      coord_flip()
  })

  })


observeEvent(in_ras$rec_feat, {

  chmR <- in_ras$ras_crop

  f_all <- raster::hist(chmR, breaks = 30, plot = F)
  dat <- data.frame(counts= f_all$counts,breaks = f_all$mids)


  output$hist <- shiny::renderPlot({
    ggplot(dat, aes(x = .data[['breaks']], y = .data[['counts']])) +
      geom_col(aes(fill = .data[['breaks']]), col = 'black') +
      theme_bw() + scale_fill_gradientn(colors = myPal)+
      coord_flip()
  })

})






}

## To be copied in the UI
# mod_panel_stat_plot_ui("panel_stat_plot_ui_1")

## To be copied in the server
# callModule(mod_panel_stat_plot_server, "panel_stat_plot_ui_1")

