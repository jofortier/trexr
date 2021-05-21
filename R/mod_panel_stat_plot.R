#' panel_stat_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom ggplot2 ggplot aes geom_col labs theme_bw coord_flip scale_fill_gradientn geom_point geom_line geom_boxplot
#' @importFrom rlang .data
mod_panel_stat_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
 shiny::plotOutput(ns('hist'))
  )
}

#' panel_stat_plot Server Function
#'
#' @noRd
mod_panel_stat_plot_server <- function(input, output, session,in_ras, clear_map, plot_rad, values){
  ns <- session$ns


  myPal <- myColorRamp(c("blue","green","yellow","red"),0:255)

  observeEvent(in_ras$chmR_rec, {
    showNotification(ui = "rendering graph")

isolate({

observe({
  if(is.null(in_ras$ras_crop)){

if(plot_rad() == 'hist'){
     f_all <- raster::hist(in_ras$chmR, breaks = 30, plot = F)
     values$dat_plot <- data.frame(counts= f_all$counts,breaks = f_all$mids)

     output$hist <- shiny::renderPlot({
       ggplot(values$dat_plot, aes(x = .data[['breaks']], y = .data[['counts']])) +
         geom_col(aes(fill = .data[['breaks']]), col = 'black') +
         theme_bw() + scale_fill_gradientn(colors = myPal)+
         coord_flip()+
         labs(x = paste0(values$data))
     })

}

    if (plot_rad() == 'dens'){

  f_all <- raster::density(in_ras$chmR, plot = F)
  values$dat_plot <- data.frame(x= f_all$x, y = f_all$y)
  output$hist <- shiny::renderPlot({
    ggplot(values$dat_plot, aes(x = .data[['x']], y = .data[['y']])) +
      geom_line() +
      theme_bw() +
      coord_flip() +
      labs(x = paste0(values$data))
  })
}

    if (plot_rad() == 'bp') {

  f_all <- raster::boxplot(in_ras$chmR,plot = F)
  values$dat_plot <- data.frame(stats = f_all$stats, name = 'Raster')

  output$hist <- shiny::renderPlot({
    ggplot(data = values$dat_plot, aes(.data[['stats']], .data[['name']])) +
      geom_boxplot() + coord_flip() + theme_bw() +
      labs(y = '', x = paste0(values$data))

  })
    }

    } else {

      if(plot_rad() == 'hist'){
              f_all <- raster::hist(in_ras$ras_crop, breaks = 30, plot = F)
              values$dat_plot <- data.frame(counts= f_all$counts,breaks = f_all$mids)
              output$hist <- shiny::renderPlot({
                ggplot(values$dat_plot, aes(x = .data[['breaks']], y = .data[['counts']])) +
                  geom_col(aes(fill = .data[['breaks']]), col = 'black') +
                  theme_bw() + scale_fill_gradientn(colors = myPal)+
                  coord_flip()+
                  labs(x = paste0(values$data))
              })


            }

          if (plot_rad() == 'dens'){

              f_all <- raster::density(in_ras$ras_crop, plot = F)
              values$dat_plot <- data.frame(x= f_all$x, y = f_all$y)
              output$hist <- shiny::renderPlot({
                ggplot(values$dat_plot, aes(x = .data[['x']], y = .data[['y']])) +
                  geom_line() +
                  theme_bw() +
                  coord_flip() +
                  labs(x = paste0(values$data))
              })
            }

          if (plot_rad() == 'bp') {

              f_all <- raster::boxplot(in_ras$ras_crop,plot = F)
              values$dat_plot <- data.frame(stats = f_all$stats, name = 'Raster')

              output$hist <- shiny::renderPlot({
                ggplot(data = values$dat_plot, aes(.data[['stats']], .data[['name']])) +
                  geom_boxplot() + coord_flip() + theme_bw() +
                  labs(y = '', x = paste0(values$data))

              })

            }
    }


  })

  })
})



  observeEvent(in_ras$rec_feat,{

    showNotification(ui = "rendering graph")

    isolate({

      if(plot_rad() == 'hist'){
        f_all <- raster::hist(in_ras$ras_crop, breaks = 30, plot = F)
        values$dat_plot <- data.frame(counts= f_all$counts,breaks = f_all$mids)
        output$hist <- shiny::renderPlot({
          ggplot(values$dat_plot, aes(x = .data[['breaks']], y = .data[['counts']])) +
            geom_col(aes(fill = .data[['breaks']]), col = 'black') +
            theme_bw() + scale_fill_gradientn(colors = myPal)+
            coord_flip()+
            labs(x = paste0(values$data))
        })

      }

      if (plot_rad() == 'dens'){

        f_all <- raster::density(in_ras$ras_crop, plot = F)
        values$dat_plot <- data.frame(x= f_all$x, y = f_all$y)
        output$hist <- shiny::renderPlot({
          ggplot(values$dat_plot, aes(x = .data[['x']], y = .data[['y']])) +
            geom_line() +
            theme_bw() +
            coord_flip() +
            labs(x = paste0(values$data))
        })
      }

      if (plot_rad() == 'bp') {

        f_all <- raster::boxplot(in_ras$ras_crop,plot = F)
        values$dat_plot <- data.frame(stats = f_all$stats, name = 'Raster')

        output$hist <- shiny::renderPlot({
          ggplot(data = values$dat_plot, aes(.data[['stats']], .data[['name']])) +
            geom_boxplot() + coord_flip() + theme_bw() +
            labs(y = '', x = paste0(values$data))

        })

      }

    })

  })


}

## To be copied in the UI
# mod_panel_stat_plot_ui("panel_stat_plot_ui_1")

## To be copied in the server
# callModule(mod_panel_stat_plot_server, "panel_stat_plot_ui_1")

