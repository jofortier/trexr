

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
