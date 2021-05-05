

# leaflet basemap function ------------------------------------------------



base_map <- function() {


  grp <- c( "Esri.WorldImagery","CartoDB.Positron", "OpenStreetMap",
            "CartoDB.DarkMatter",
            "OpenTopoMap", "Hydrography")

  att <- paste0("<a href='https://www.usgs.gov/'>",
                "U.S. Geological Survey</a> | ",
                "<a href='https://www.usgs.gov/laws/policies_notices.html'>",
                "Policies</a>")

  GetURL <- function(service, host = "basemap.nationalmap.gov") {
    sprintf("https://%s/arcgis/services/%s/MapServer/WmsServer", host, service)
  }

  map <- leaflet::leaflet()

  map <- leaflet::addProviderTiles(map = map, provider = grp[[1]],
                                   group = grp[[1]])
  map <- leaflet::addProviderTiles(map = map, provider = grp[[2]],
                                   group = grp[[2]])
  map <- leaflet::addProviderTiles(map = map, provider = grp[[3]],
                                   group = grp[[3]])
  map <- leaflet::addProviderTiles(map = map, provider = grp[[4]],
                                   group = grp[[4]])
  map <- leaflet::addProviderTiles(map = map, provider = grp[[5]],
                                   group = grp[[5]])

  opt <- leaflet::WMSTileOptions(format = "image/png", transparent = TRUE)
  map <- leaflet::addWMSTiles(map, GetURL("USGSHydroCached"),
                              group = grp[6], options = opt, layers = "0",
                              attribution = att)

  map <- leaflet::hideGroup(map, grp[6])

  opt <- leaflet::layersControlOptions(collapsed = TRUE)

  map <- leaflet::addLayersControl(map, baseGroups = grp[1:5],
                                   overlayGroups = grp[6], options = opt)
  map %>%
    leafem::addMouseCoordinates(epsg = "EPSG:4326", proj4string = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

}



# treetop functions --------------------------------------------------


interpol<- function(input,col) {
  surf.3d <- t(geometry::convhulln(input,options = "QJ"))
  rgl::rgl.triangles(input[surf.3d,1],input[surf.3d,2],input[surf.3d,3],col=col,alpha = 1,
                     lit = TRUE,ambient = "black",specular = "white",emission = "black",shininess = 50.0,
                     smooth = TRUE, texture = NULL,front = "fill",back ="fill", box=F,axes = FALSE)}



# kurtosis and skewness from moments package
kurtosis<-function (x, na.rm = FALSE)
{
  if (is.matrix(x))
    apply(x, 2, kurtosis, na.rm = na.rm)
  else if (is.vector(x)) {
    if (na.rm)
      x <- x[!is.na(x)]
    n <- length(x)
    n * sum((x - mean(x))^4)/(sum((x - mean(x))^2)^2)
  }
  else if (is.data.frame(x))
    sapply(x, kurtosis, na.rm = na.rm)
  else kurtosis(as.vector(x), na.rm = na.rm)
}


cv<-function(x){
  sd(x, na.rm=T)/mean(x,na.rm=T)*100
}

skewness<-function (x, na.rm = FALSE)
{
  if (is.matrix(x))
    apply(x, 2, skewness, na.rm = na.rm)
  else if (is.vector(x)) {
    if (na.rm)
      x <- x[!is.na(x)]
    n <- length(x)
    (sum((x - mean(x))^3)/n)/(sum((x - mean(x))^2)/n)^(3/2)
  }
  else if (is.data.frame(x))
    sapply(x, skewness, na.rm = na.rm)
  else skewness(as.vector(x), na.rm = na.rm)
}
TreesModel<- function(crownshape=c("cone","ellipsoid","halfellipsoid","paraboloid","cylinder"),
                      nz=15, nalpha=15, CL=5, CR=5, HCB=10, x0=0, y0=0, z0=0, dbh = 0.3, crowncolor = "forestgreen",
                      stemcolor = "chocolate4", shape=1
){

  crownshape <- match.arg(crownshape)

  Ht<-HCB+CL

  if (shape==3) {

    if (Ht<=5){
      z <- rep(seq(0,1,length=10),each=5)
      angs <- rep(seq(0,2*pi, length=10),5)} else {

        nz=10; nalpha=10
        z <- rep(seq(0,1,length=nz),each=nalpha)
        angs <- rep(seq(0,2*pi, length=nalpha),nz)

      }

    z<-jitter(z,2)
    angs<-jitter(angs,2)

  } else {
    z <- rep(seq(0,1,length=nz),each=nalpha)
    angs <- rep(seq(0,2*pi, length=nalpha),nz)
  }

  if (min(z)<0){ z<-z+sqrt(min(z)^2)}
  z<-z/max(z)

  if(crownshape == "cone")distfun <- (1-z)
  if(crownshape == "ellipsoid")distfun <- sqrt(1 - ((z-1/2)^2)/((1/2)^2))
  if(crownshape == "halfellipsoid")distfun <- sqrt(1 - z**2)
  if(crownshape == "paraboloid")distfun <- sqrt(1-z)
  if(crownshape == "cylinder")distfun <- 1
  H <- HCB + CL
  r <- CR
  x <- x0 + r*distfun*cos(angs)
  y <- y0 + r*distfun*sin(angs)
  z <- z0 + HCB + z*CL

  keep <- !duplicated(cbind(x,y,z))
  x <- x[keep]
  y <- y[keep]
  z <- z[keep]
  klj=matrix(cbind(x,y,z),ncol=3)

  if (shape==1){

    mMatrix<-matrix(NA,ncol=3)[-1,]

    for ( i in 1:nrow(klj)){
      ln=i+nz
      if ( ln >= nrow(klj)) { ln2=nrow(klj) } else { ln2= ln}
      mMatrix<-rbind(mMatrix,rbind(klj[i,],klj[ln2,])) }
    kljzbase=subset(klj,klj[,3]==z[2])
    kljzbaseNew<-matrix(NA,ncol=3)[-1,]
    for ( i in 1:nrow(kljzbase)){
      kljzbaseNew<-rbind(kljzbaseNew,rbind(kljzbase[i,],c(x0,y0,HCB)))

    }
    newList<-rbind(kljzbaseNew,mMatrix,klj)
    rgl::lines3d(newList, col=crowncolor, add=T,box=F)
  }

  if (shape==2){interpol(klj,col=crowncolor)}

  if (shape==3){

    NewList<-matrix(ncol=3)[-1,]
    for ( k in 1:nrow(klj)){
      sk<-sample(c(0.25,-0.25,0.5,-0.5), 1)
      NewList<-rbind(NewList,rbind(klj[k,],c(x0,y0,klj[k,3]+sk)))
    }
    NewList<-jitter(NewList,15)
    head(NewList)
    if (Ht<=2){col=rep("green",nrow(NewList))}
    if (Ht> 2 & Ht<=5){col=sample(c("green3","green3","green"),nrow(NewList), TRUE)}
    if (Ht>5){col=c("darkgreen")}

    rgl::lines3d(NewList, col=col, add=T)
  }
}




GiniCoeff <- function (x, finite.sample = TRUE, na.rm = TRUE){
  if (!na.rm && any(is.na(x)))
    return(NA_real_)
  x <- as.numeric(stats::na.omit(x))
  n <- length(x)
  x <- sort(x)
  G <- 2 * sum(x * 1L:n)/sum(x) - (n + 1L)
  if (finite.sample)
    GC <- G/(n - 1L)
  else GC <- G/n
  return(GC)
}

getExtension <- function(file){
  ex <- strsplit(basename(file), split="\\.")[[1]]
  return(ex[length(ex)])
}



# random ------------------------------------------------------------------


myColorRamp <- function(colors, values) {
  v <- (values - min(values))/diff(range(values))
  x <- grDevices::colorRamp(colors)(v)
  grDevices::rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
}


#add in renderRGL
# renderRglwidget_hack <- function(expr, env = parent.frame(), quoted = FALSE,
#                             outputArgs = list()) {
#   if (!quoted) { expr <- substitute(expr) } # force quoted
#   markRenderFunction(rglwidgetOutput,
#                      shinyRenderWidget(expr, rglwidgetOutput, env, quoted = TRUE),
#                      outputArgs = outputArgs)
# }
# old UI treeCrown --------------------------------------------------------



# shinydashboard::menuItem(
#   "Explore Tree Detection",
#   tabName = "chm_tree",
#   icon = icon("door-open")
# ),
# conditionalPanel(condition = "input.menu1 === 'chm_tree'",
#                  selectInput("fws",label =HTML("Local Maximum Filter <a href = 'https://jean-romain.github.io/lidRbook/itd-its.html#itd'>(help)</a>"),
#
#                              choices = c("3x3","5x5","7x7","9x9","11x11","13x13","15x15","17x17"),
#                              selected="5x5")
#
# ,
#
# div(style ="margin-left: 2px;margin-top:-10px;color:white",
#     radioButtons("radiustype", "Tree Crowns Delineation",
#                  list("CrownSeg" = "FR","CrownAllometry" = "VR"),inline = TRUE)),
#
# conditionalPanel(condition="input.radiustype=='VR'",
#                  div(style ="margin-left: 0px;color:white",
#                      radioButtons("equation", " Equation: TCW = f(ht); ht= Height (m)",
#                                   list("Deciduous" = "Deciduous","Pines" = "Pines","Combined"="Combined",
#                                        "Use custom polynomial"="Custom"),inline = TRUE)),
#                  conditionalPanel(condition="input.equation=='YR'",
#                                   div(style = "margin-left: 2px;margin-top: -10px;color:white",
#                                       HTML("|---Inter-----------ht------------ht^2--------ht^3-|")),
#                                   div(class="row-fluid",
#                                       div(class="span6",style = "margin-left: 0px;color:white",
#                                           numericInput("Ang", "", "")),
#                                       div(class="span6",style = "margin-left: 71px;margin-top: -68px;color:white",
#                                           numericInput("ht1", "", "")),
#                                       div(class="span6",style = "margin-left: 141px;margin-top: -68px;color:white",
#                                           numericInput("ht2", "", "")),
#                                       div(class="span6",style = "margin-left: 211px;margin-top: -68.2px;color:white",
#                                           numericInput("ht3", "", ""))))),
# conditionalPanel(condition="input.radiustype=='FR'",
#                  div(style="margin-left: 2px;color:white;margin-top: -10px;",
#                      sliderInput("maxcrown", "maxcrown",0,1,0.6,step=0.01,sep="#.##")),
#                  div(style="margin-left: 2px;color:white;margin-top: -15px;",
#                      sliderInput("exclusion","exclusion",0,1,0.3,step=0.01,sep="#.##"))),
#
#
#                  div(style = "margin-left:0px; width:100px;margin-top:-35px;color:white",
#                      radioButtons("filtertype", "",
#                                   list("Mean" = "Mean","Gaussian" = "Gaussian"))),
#                  conditionalPanel(condition="input.filtertype=='Mean'",
#                                    div(style = "margin-left: 125px;text-align:center; width:130px;margin-top:-105px;color:white",
#                                       selectInput("sws", "Filter Windows Size",
#                                                   choices = c("3x3","5x5","7x7","9x9","11x11","13x13","15x15","17x17"),selected="3x3"))),
#
#                  conditionalPanel(condition="input.filtertype=='Gaussian'",
#                                   div(style = "color:white;margin-top:-15px;margin-left: 2px;",
#                                       sliderInput("Sigma","Gaussian Sigma ",0.1,3,1.5,step=0.01,format="#.##"))),
#
# div(style = "color:white;margin-top:-20px",
#     radioButtons("plotCHM2d", "", list("CHM 2d" = "plotchm2d",
#                                        "Lorenz curve" = "plotlorenzcurve"),inline = TRUE),
#     div(style = "color:white;margin-top:-30px",
#         radioButtons("plotProfile", "", list("CHM profile" = "plotCHMProfile",
#                                              "Ripley's K and L" = "plotRipley"),inline = TRUE)),
#
#     div(style = "color:white;margin-top:-30px",
#         radioButtons("plot3Dradio", "", list("CHM 3d" = "plotCHM3D",
#                                              "Trees 3d" = "plot3Dtrees"),inline = TRUE)),
#
#     conditionalPanel(condition="input.plot3Dradio=='plot3Dtrees'",
#                      div(style = "color:white;margin-top:-10px;width:130px",
#                          selectInput("plotShape", "Shape",
#                                      choices = c("cone","ellipsoid","halfellipsoid","paraboloid","cylinder"),selected="halfellipsoid"),
#                          div(style = "color:white;margin-top:-78px;width:130px;margin-left: 145px",
#                              selectInput("plotSurface", "Surface",
#                                          choices = c("solid","mesh","lines"),selected="lines"))
#                      )
#                      ),div(style="margin-left: 2px;margin-top:-15px;",
#     actionButton('run_but', label = "Run Selections"))


#      shinydashboard::tabItem(
#        tabName = "chm_tree",
#        fluidRow(
#                 tabPanel(title = 'Explore Tree Detection', style = 'height:92vh;',
#                          shinydashboard::box(mod_panel_tree_detection_stats_plot_ui("panel_tree_detection_stats_plot_ui_1"), width = 6, title = 'Plotting'),
#                          shinydashboard::box(mod_panel_tree_detection_stats_ui("panel_tree_detection_stats_ui_1")%>% shinycssloaders::withSpinner(), width = 6, title = 'Graphing'),
#                          shinydashboard::box(mod_panel_tree_detection_leaflet_ui("panel_tree_detection_leaflet_ui_1") %>% shinycssloaders::withSpinner(), width = 6, title = 'Mapping')))
#
# )
