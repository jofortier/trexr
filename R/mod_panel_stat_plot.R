#' panel_stat_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom ggplot2 ggplot aes geom_point
mod_panel_stat_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
 shiny::plotOutput(ns('hist'))
  )
}

#' panel_stat_plot Server Function
#'
#' @noRd
mod_panel_stat_plot_server <- function(input, output, session){
  ns <- session$ns



  output$hist <- shiny::renderPlot({

    ggplot2::ggplot(mtcars, aes(wt, cyl)) + geom_point()
    # oldpar = par(mfrow=c(1,2), mar=c(5,5,2,1))
    # dens<-density(chm_hts,adjust = 1.3, kernel = "gaussian")
    # #par(mfrow=c(1,3), mar=c(5,5,2,2))
    # plot(dens$y,dens$x, cex.lab=2,col="transparent",xlab="Density",ylab="Height (m)",ylim=c(0,max(chm_hts*1.3)))
    # lines(dens$y,dens$x,col="black",lwd=1)
    # polygon(dens$y,dens$x, col=input$profColor, border="black")
    # legend("topright","CHM height distribution", bty="n", text.font=2, cex=1.5)
    # #if (!is.null(input$HTsliderI)) {abline(v=input$HTsliderI, lwd=2, col="red")}
    # #if (!is.null(input$HTboxI)) {abline(v=input$HTboxI, lwd=2, col="red")}
    # boxplot(chm_hts, cex.lab=2, ylim=c(0,max(chm_hts)*1.3),horizontal=F, col=input$profColor,ylab="Height (m)")
    #
    # par(oldpar)
  })

}

## To be copied in the UI
# mod_panel_stat_plot_ui("panel_stat_plot_ui_1")

## To be copied in the server
# callModule(mod_panel_stat_plot_server, "panel_stat_plot_ui_1")

