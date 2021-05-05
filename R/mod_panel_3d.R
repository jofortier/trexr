#' panel_3d UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_panel_3d_ui <- function(id){
  ns <- NS(id)
  tagList(
    rglwidget::rglwidgetOutput(ns("PLOT3D"))
  )
}

#' panel_3d Server Function
#'
#' @noRd
mod_panel_3d_server <- function(input, output, session, in_ras, clear_map, sws, smooth3d){
  ns <- session$ns

  observeEvent(in_ras$chmR_rec, {
    output$PLOT3D <- rgl::renderRglwidget({
      validate(need(!is.null(in_ras$chmR), 'Waiting for Raster'))

      if (smooth3d() == 1){
        chmR0 <- in_ras$chmR

        if ( sws()=="3x3"){
          sws=3 }
        if ( sws()=="5x5"){
          sws=5 }
        if ( sws()=="7x7"){
          sws=7 }
        if ( sws()=="9x9"){
          sws=9 }

        wf<-matrix(c(rep(1,sws*sws)),nrow=sws,ncol=sws)
        chmR0 <- raster::focal(chmR0, w=wf, fun=mean)

      } else {

        chmR0 <- in_ras$chmR
      }

      while (rgl::rgl.cur() > 0) { try(rgl::rgl.close())}

     myPal <- myColorRamp(c("blue","green","yellow","red"),0:255)


      while (rgl::rgl.cur() > 0) { try(rgl::rgl.close()) }

      rasterVis::plot3D(chmR0,col=myPal,xlab="",ylab="",zlab="Height (m)")

      rgl::axes3d(c("x-", "y-"), col="black")
      rgl::title3d(xlab = "UTM Easting", ylab = "UTM Northing")#, col="green")

      rgl::rglwidget()

    })
  })

  observeEvent(in_ras$rec_feat, {

    if (smooth3d() == 1){

      chmR <- in_ras$ras_crop

      if ( sws()=="3x3"){
        sws=3 }
      if ( sws()=="5x5"){
        sws=5 }
      if ( sws()=="7x7"){
        sws=7 }
      if ( sws()=="9x9"){
        sws=9 }

      wf<-matrix(c(rep(1,sws*sws)),nrow=sws,ncol=sws)
      chmR <- raster::focal(chmR, w=wf, fun=mean)

    } else {

      chmR <- in_ras$ras_crop
    }


    chmR[chmR[]<0]<-0
    r1NaM <- is.na(raster::as.matrix(chmR))
    colNotNA <- which(colSums(r1NaM) != nrow(chmR))
    rowNotNA <- which(rowSums(r1NaM) != ncol(chmR))

    exst <- raster::extent(chmR, rowNotNA[1], rowNotNA[length(rowNotNA)],
                           colNotNA[1], colNotNA[length(colNotNA)])
    chmR <- raster::crop(chmR,exst)

    output$PLOT3D <- rgl::renderRglwidget({
      validate(need(!is.null(chmR), 'Waiting for Raster'))
      while (rgl::rgl.cur() > 0) { try(rgl::rgl.close())}


      myPal <- myColorRamp(c("blue","green","yellow","red"),0:255)


      while (rgl::rgl.cur() > 0) { try(rgl::rgl.close()) }

      rasterVis::plot3D(chmR,col=myPal,xlab="",ylab="",zlab="Height (m)")

      rgl::axes3d(c("x-", "y-"), col="black")
      rgl::title3d(xlab = "UTM Easting", ylab = "UTM Northing")#, col="green")

      rgl::rglwidget()

    })
  })

  observeEvent(clear_map(), {
    output$PLOT3D <- rgl::renderRglwidget({

      if (smooth3d() == 1){

        chmR0 <- in_ras$chmR_og

        if ( sws()=="3x3"){
          sws=3 }
        if ( sws()=="5x5"){
          sws=5 }
        if ( sws()=="7x7"){
          sws=7 }
        if ( sws()=="9x9"){
          sws=9 }



        wf<-matrix(c(rep(1,sws*sws)),nrow=sws,ncol=sws)
        chmR0 <- raster::focal(chmR0, w=wf, fun=mean)

      } else {

        chmR0 <- in_ras$chmR_og
      }
      while (rgl::rgl.cur() > 0) { try(rgl::rgl.close())}


     myPal <- myColorRamp(c("blue","green","yellow","red"),0:255)

      while (rgl::rgl.cur() > 0) { try(rgl::rgl.close()) }

      rasterVis::plot3D(chmR0,col=myPal,xlab="",ylab="",zlab="Height (m)")

      rgl::axes3d(c("x-", "y-"), col="black")
      rgl::title3d(xlab = "UTM Easting", ylab = "UTM Northing")#, col="green")

      rgl::rglwidget()

    })
  })
}

## To be copied in the UI
# mod_panel_3d_ui("panel_3d_ui_1")

## To be copied in the server
# callModule(mod_panel_3d_server, "panel_3d_ui_1")

