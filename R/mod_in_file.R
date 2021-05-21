#' in_file UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom stats median sd var
#' @importFrom methods slot
#' @importFrom grDevices colorRamp rgb
#' @importFrom utils head
mod_in_file_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' in_file Server Function
#'
#' @noRd
mod_in_file_server <- function(input, output, session, in_ras,HTboxI, file_path, clear_map, change_ht,
                               input_box, feet, values, switch_fil, zvalues, zsel, met, lab_sel){
  ns <- session$ns


  observeEvent(file_path(), {

  inFile<- file_path()
  if (is.null(inFile)){
    return(NULL)}

  if(length(inFile$datapath)>1){

chmR <- mosaicList(inFile$datapath)

  } else {

    chmR<-raster::raster(inFile$datapath)
  }

  chmR[chmR[]<0]<-0
  chmR[chmR[]<0] <- NA

  input_box$box_og1 <- 0
  input_box$box_og2 <- 500


  projecCHM <- raster::projection(chmR)
  in_ras$projectCHM <- projecCHM

  r1NaM <- is.na(raster::as.matrix(chmR))
  colNotNA <- which(colSums(r1NaM) != nrow(chmR))
  rowNotNA <- which(rowSums(r1NaM) != ncol(chmR))

  exst <- raster::extent(chmR, rowNotNA[1], rowNotNA[length(rowNotNA)],
                         colNotNA[1], colNotNA[length(colNotNA)])
  chmR <- raster::crop(chmR,exst)

  showNotification(ui = "uploaded raster")

  req(chmR)

  in_ras$chmR <- chmR

  in_ras$chmR_og <- chmR

  in_ras$chmR_og_og <- chmR

  in_ras$chmR_rec <- reactive(in_ras$chmR)

  in_ras$ras_crop <- NULL

  values$sf <- NULL
  observe({
  if(lab_sel() == 'Z'){

    values$data <- 'Z'
    values$data_stat <- ''
  } else if (lab_sel() == 'Feet') {

    values$data <- 'Feet'
    values$data_stat <- 'ft'

  } else if (lab_sel() == 'Meters'){

    values$data <- 'Meters'
    values$data_stat <- 'm'
  }
})
  })
observeEvent(feet(), {

  req(in_ras$chmR_og)

isolate({
if(feet()== 1){
req(in_ras$chmR_og)

  chmR <- in_ras$chmR_og_og

  showNotification(ui = "converting Z values to meters")

  chmR <- chmR/3.28084

  in_ras$chmR <- chmR

  in_ras$chmR_og <- chmR

  in_ras$chmR_rec <- reactive(in_ras$chmR)

  in_ras$ras_crop <- NULL

  values$sf <- NULL

} else if (feet()==0){

  showNotification(ui = "converting Z values back to feet")

  chmR <- in_ras$chmR_og_og

  req(chmR)

  in_ras$chmR <- chmR

  in_ras$chmR_og <- chmR

  in_ras$chmR_rec <- reactive(in_ras$chmR)

  in_ras$ras_crop <- NULL

  values$sf <- NULL
}
})
})

observeEvent(met(), {
  req(in_ras$chmR_og)
  isolate({
  if(met()== 1){

  chmR <- in_ras$chmR_og_og

    req(chmR)

    showNotification(ui = "converting Z values to feet")

    chmR <- chmR*3.28084

    in_ras$chmR <- chmR

    in_ras$chmR_og <- chmR

    in_ras$chmR_rec <- reactive(in_ras$chmR)

    in_ras$ras_crop <- NULL

    values$sf <- NULL

  } else if (met() == 0){

    showNotification(ui = "converting Z values back to meters")

    chmR <- in_ras$chmR_og_og
    req(chmR)

    in_ras$chmR <- chmR

    in_ras$chmR_og <- chmR

    in_ras$chmR_rec <- reactive(in_ras$chmR)

    in_ras$ras_crop <- NULL

    values$sf <- NULL
  }

    })
  })


  observeEvent(change_ht(),{

    req(in_ras$chmR_og)

    if(is.null(in_ras$ras_crop)){

      chmR <- in_ras$chmR_og

    } else {

      chmR <- in_ras$ras_crop_og
    }

    isolate({


      Htreshoud<-HTboxI()

    })

    isolate({
    if(switch_fil() == 1){

      showNotification(ui = "changing height 'outside'")

      chmR[chmR[]>Htreshoud[[1]] & chmR[]<Htreshoud[[2]]] <- NA

    } else {

      showNotification(ui = "changing height")

       chmR[chmR[]<Htreshoud[[1]]] <- NA
       chmR[chmR[]>Htreshoud[[2]]] <- NA
    }
})
if(raster::cellStats(chmR, mean) == 'NaN'){shinyalert::shinyalert(
  title = "Filtering Error",
  text = "No canopy values found within the defined height range.",
  size = "s",
  closeOnEsc = TRUE,
  closeOnClickOutside = TRUE,
  html = FALSE,
  type = "error",
  showConfirmButton = TRUE,
  showCancelButton = FALSE,
  confirmButtonText = "OK",
  confirmButtonCol = "#AEDEF4",
  timer = 0,
  imageUrl = "",
  animation = TRUE)

  } else {
#
#     projecCHM <- raster::projection(chmR)
#     in_ras$projectCHM <- projecCHM
    area_ha<-0
    reschmR<-raster::res(chmR)[1]
    newst<-raster::extent(chmR)

    r1NaM <- is.na(raster::as.matrix(chmR))
    colNotNA <- which(colSums(r1NaM) != nrow(chmR))
    rowNotNA <- which(rowSums(r1NaM) != ncol(chmR))

    exst <- raster::extent(chmR, rowNotNA[1], rowNotNA[length(rowNotNA)],
                           colNotNA[1], colNotNA[length(colNotNA)])
    chmR <- raster::crop(chmR,exst)


if(is.null(in_ras$ras_crop)){

  in_ras$chmR <- chmR
  in_ras$chmR_rec <- reactive(in_ras$chmR)

} else {

  in_ras$ras_crop <- chmR
  in_ras$chmR_rec <- reactive(in_ras$ras_crop)

}
}
  })




  observeEvent(clear_map(),{

    req(in_ras$chmR_og)
    showNotification(ui = "refreshing back to original")

    in_ras$ras_crop <- NULL

    chmR <- in_ras$chmR_og

    isolate({


      Htreshoud<-c(input_box$box_og1, input_box$box_og2)

    })

    chmR[chmR[]<Htreshoud[[1]]] <- NA
    chmR[chmR[]>Htreshoud[[2]]] <- NA

    # projecCHM <- raster::projection(chmR)
    # in_ras$projectCHM <- projecCHM
    area_ha<-0
    reschmR<-raster::res(chmR)[1]
    newst<-raster::extent(chmR)

    r1NaM <- is.na(raster::as.matrix(chmR))
    colNotNA <- which(colSums(r1NaM) != nrow(chmR))
    rowNotNA <- which(rowSums(r1NaM) != ncol(chmR))

    exst <- raster::extent(chmR, rowNotNA[1], rowNotNA[length(rowNotNA)],
                           colNotNA[1], colNotNA[length(colNotNA)])
    chmR <- raster::crop(chmR,exst)

    in_ras$chmR <- chmR

    in_ras$chmR_rec <- reactive(in_ras$chmR)


  })

}



## To be copied in the UI
# mod_in_file_ui("in_file_ui_1")

## To be copied in the server
# callModule(mod_in_file_server, "in_file_ui_1")

