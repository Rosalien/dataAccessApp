#' @title dygraphTypeVariable
#' @description Time trend with dygraphs library for different type of variable
#' @param subsetoutbd data.table from mod_accessdata
#' @param frequence : String of frequency (ex. "30 min", "hour", "day", "month")
#' @return dygraph of timetrend
#' @importFrom data.table setDT
#' @importFrom shiny tagList
#' @importFrom xts xts
#' @import dygraphs
#' @export

dygraphTypeVariable <- function(subsetoutbd,frequence){
  if(nrow(subsetoutbd)==0){
    res <- NULL
  }else{
  dateWindow <- c(min(subsetoutbd$Date),max(subsetoutbd$Date))
  fullSequence <- seq(min(subsetoutbd$Date),max(subsetoutbd$Date),by=frequence)
  variables <- unique(subsetoutbd$variable)

  keyWord <- c("Soil temperature","Soil water content","Soil heat flux","Température du sol","Teneur en eau du sol","Flux de chaleur dans le sol","Actual evapotranspiration by eddy-covariance","CO2 flux measurements by eddy-covariance","CH4 flux measurements by eddy-covariance","Sensitive heat flow by eddy-covariance","Latent heat flow by eddy-covariance")
  keyWord <- paste(keyWord,collapse = "|")
  subsetoutbd[,typeVariable:=ifelse(grepl(keyWord, definition)==TRUE,gsub(paste0("(",keyWord,").*"),"\\1",definition),definition)]

  # Génération d'une time series par type de variable et par site  
  typeVariables <- unique(subsetoutbd$typeVariable)

  res <- lapply(1:length(unique(subsetoutbd$typeVariable)),function(z){#Boucle pour chaque type de variable
    # Sélection du type de variable
    outp <- subsetoutbd[subsetoutbd$typeVariable %in% typeVariables[z],]    
    outp <- outp[order(outp$variable),]

    lapply(unique(outp$code_site_station),function(y){#Pour chaque site, construction d'un graph
            dataxts <- do.call("cbind", lapply(unique(outp$variable),function(x){#Pour chaque variable, construction d'une dbtimes-series
              if(nrow(outp[code_site_station %in% y & variable %in% x,])==0){#revoir ce test
                NULL
              }else{  
                tmp <-  outp[code_site_station %in% y & variable %in% x,list(Date,value)]
                tmp2 <- setDT(data.frame(Date=fullSequence,with(tmp,tmp[match(fullSequence,tmp$Date),])))
                db <- xts(tmp2[,value],order.by=tmp2[,Date])
                colnames(db) <- x
                db
              }
            }))
                if(grepl("year",frequence)){
                  axisYears<-"function(d){ return d.getFullYear() }"
                  tickerYears <- "function(a, b, pixels, opts, dygraph, vals) { 
                                  return Dygraph.getDateAxis(a, b, Dygraph.ANNUAL, opts, dygraph)}"
                  DataTimezone <- FALSE
                }else{
                axisYears<-NULL
                tickerYears <- NULL
                DataTimezone <- TRUE
                }

                dygraph(dataxts,group="groupe",main = paste(unique(outp[,"typeVariable"])," (",y,")",sep=""),ylab = unique(outp[,"unite"])[[1]],height = 250,width="100%")%>%
                dyAxis("x", axisLabelFormatter=axisYears,
                ticker= tickerYears) %>%
                dyOptions(stackedGraph = FALSE) %>%
                dyRangeSelector(height = 20,strokeColor = "") %>%
                dyLegend(show = "onmouseover") %>% 
                dyOptions(colors = wes_palette("Zissou1", length(unique(outp$variable)),type = "continuous"),retainDateWindow=TRUE,useDataTimezone=TRUE)%>%
                dyRangeSelector(dateWindow = dateWindow,retainDateWindow=TRUE)%>%
                dyCSScool() %>%
                dyHighlight(highlightSeriesBackgroundAlpha = 0.8,highlightSeriesOpts = list(strokeWidth = 3))
            })            
        })#fin de res
  }
    dy_graph <- tagList(res)
    return(dy_graph)
}

#' @title dygraphSite
#' @description Time trend with dygraphs library for different sites
#' @param subsetoutbd data.table from mod_accessdata
#' @param frequence : String of frequency (ex. "30 min", "hour", "day", "month")
#' @return dygraph of timetrend
#' @importFrom data.table setDT
#' @importFrom shiny tagList
#' @importFrom xts xts
#' @import dygraphs
#' @export

dygraphSite <- function(subsetoutbd,frequence){
  if(nrow(subsetoutbd)==0){
    res <- NULL
  }else{
    dateWindow <- c(min(subsetoutbd$Date),max(subsetoutbd$Date))
    fullSequence <- seq(min(subsetoutbd$Date),max(subsetoutbd$Date),by=frequence)
    variables <- unique(subsetoutbd$variable)

    res <- lapply(1:length(variables),function(z){
      outp <- subsetoutbd[subsetoutbd$variable == variables[z],]   
      # Génération de la time serie pour tous les sites
      dataxts <- do.call("cbind", lapply(unique(outp$code_site_station),function(x){
        if(nrow(outp[code_site_station %in% x,])==0){
        NULL
        }else{
        #Construction d'une time-series complète
          tmp <-  outp[code_site_station %in% x,list(Date,value)]
          tmp2 <- setDT(data.frame(Date=fullSequence,with(tmp,tmp[match(fullSequence,tmp$Date),])))
          db <- xts(tmp2[,value],order.by=tmp2[,Date])
        colnames(db) <- x
        db
        }
        }))

      if(grepl("year",frequence)){
        axisYears<-"function(d){ return d.getFullYear() }"
        tickerYears <- "function(a, b, pixels, opts, dygraph, vals) { 
                          return Dygraph.getDateAxis(a, b, Dygraph.ANNUAL, opts, dygraph)}"
        DataTimezone <- FALSE
      }else{
        axisYears<-NULL
        tickerYears <- NULL
        DataTimezone <- TRUE
      }
      # https://stackoverflow.com/questions/33885817/r-dygraphs-x-axis-granularity-from-monthly-to-yearly
      graph <- dygraph(dataxts,group="groupe",main = unique(outp[,definition]),ylab = unique(outp[,unite]),height = 250,width="100%") %>%
               dyAxis("x", axisLabelFormatter=axisYears,ticker= tickerYears) %>%
               dyOptions(stackedGraph = FALSE) %>%
               dyRangeSelector(height = 20,strokeColor = "") %>%  
               dyLegend(show = "onmouseover") %>% 
               dyRangeSelector(dateWindow = dateWindow,retainDateWindow=TRUE)%>%
               dyCSScool() %>%
               dyHighlight(highlightSeriesBackgroundAlpha = 0.8,highlightSeriesOpts = list(strokeWidth = 3))    %>%   
               dyOptions(colors = wes_palette("Zissou1", length(unique(outp$code_site_station)),type = "continuous"),retainDateWindow=TRUE,useDataTimezone=DataTimezone)

    # Condition pour générer un barplot pour les précipitations
    if(unique(outp$variable)=="P_1_1_1" & length(unique(outp$code_site_station)) >=1){graph<-graph %>% dyBarChart()}else{graph}
    if(unique(outp$variable)=="P_1_1_1" & length(unique(outp$code_site_station)) >1){graph<- graph %>% dyMultiColumn()}else{graph}
    graph
    })
  }

    dy_graph <- tagList(res)
    return(dy_graph)
}

#' @title dygraphPiezo
#' @description Time trend with dygraphs library for different piezo for the same site
#' @param subsetoutbd data.table from mod_accessdata
#' @param frequence : String of frequency (ex. "30 min", "hour", "day", "month")
#' @return dygraph of timetrend
#' @importFrom data.table setDT
#' @importFrom shiny tagList
#' @importFrom xts xts
#' @import dygraphs
#' @export
dygraphPiezo <- function(subsetoutbd,frequence){
if(nrow(subsetoutbd)==0){
    res <- NULL
  }else{
  dateWindow <- c(min(subsetoutbd$Date),max(subsetoutbd$Date))
  fullSequence <- seq(min(subsetoutbd$Date),max(subsetoutbd$Date),by=frequence)

  sitePiezo <- unique(subsetoutbd$site_description)
  
  res <- lapply(1:length(sitePiezo),function(z){#Boucle pour chaque site

    # Sélection du type de piezo
    outp <- subsetoutbd[subsetoutbd$site_description %in% sitePiezo[z],]    
    outp <- outp[order(outp$variable),]

    lapply(unique(outp$variable),function(x){#Pour chaque site, construction d'un graph pour les variables
            dataxts <- do.call("cbind", lapply(unique(outp$station_nom),function(y){#Pour chaque piezo, construction d'une dbtimes-series
              if(nrow(outp[variable %in% x,])==0){
                NULL
              }else{  
                #Construction d'une time-series complète
                tmp <-  outp[station_nom %in% y & variable %in% x,list(Date,value)]
                tmp2 <- setDT(data.frame(Date=fullSequence,with(tmp,tmp[match(fullSequence,tmp$Date),])))
                db <- xts(tmp2[,value],order.by=tmp2[,Date])
                colnames(db) <- y
                db
              }
            }))
            if(grepl("year",frequence)){
              axisYears<-"function(d){ return d.getFullYear() }"
              tickerYears <- "function(a, b, pixels, opts, dygraph, vals) { 
                              return Dygraph.getDateAxis(a, b, Dygraph.ANNUAL, opts, dygraph)}"
              DataTimezone <- FALSE
                }else{
                  axisYears<-NULL
                  tickerYears <- NULL
                  DataTimezone <- TRUE
                }

                dygraph(dataxts,group=z,main = paste(unique(outp[variable %in% x,"definition"])," (",sitePiezo[z],")",sep=""),ylab = unique(outp[variable %in% x,"unite"])[[1]],height = 250,width="100%")%>%
                dyAxis("x", axisLabelFormatter=axisYears,
                ticker= tickerYears) %>%
                dyOptions(stackedGraph = FALSE) %>%
                dyRangeSelector(height = 20,strokeColor = "") %>%
                dyLegend(show = "onmouseover") %>% 
                dyOptions(sigFigs=3,colors = wes_palette("Zissou1", length(unique(outp$station_nom)),type = "continuous"),retainDateWindow=TRUE,useDataTimezone=TRUE)%>%
                dyRangeSelector(dateWindow = dateWindow,retainDateWindow=TRUE)%>%
                dyCSScool() %>%
                dyHighlight(highlightSeriesBackgroundAlpha = 0.8,highlightSeriesOpts = list(strokeWidth = 3))
            })            
        })#fin de res
}
    dy_graph <- tagList(res)
    return(dy_graph)
}

#' @title timelineDataAvailable
#' @description Chart of data disponibility (used in mod_welcome)
#' @param tableCarac data.table from mod_welcome
#' @param allSite Logical operator to select allSite or not (TRUE, FALSE)
#' @param facetWrapOption Logical operator to use facetWrap on site or site/station (TRUE, FALSE)
#' @param translator translator object for translation
#' @return dygraph of timetrend
#' @importFrom data.table setDT
#' @importFrom graphics layout
#' @importFrom wesanderson wes_palette
#' @importFrom plotly ggplotly layout
#' @import ggplot2
#' @export
timelineDataAvailable <- function(tableCarac,allSite,facetWrapOption,translator){
  keyWord <- c("Soil temperature","Soil water content","Soil heat flux","Température du sol","Teneur en eau du sol","Flux de chaleur dans le sol")
  keyWord <- paste(keyWord,collapse = "|")
  tableCarac[,definition_simple:=ifelse(grepl(keyWord, definition)==TRUE,gsub(paste0("(",keyWord,").*"),"\\1",definition),definition)]

# Construction du graph
gg <- ggplot(unique(tableCarac),aes(colour=theme,text = 
                                                paste('<br>Begin:', mindate,
                                                      '<br>End: ', maxdate,
                                                      '<br>Variable: ', definition_simple)))+
      geom_segment(aes(x=mindate, xend=maxdate, y=variable, yend=variable),size=1)+
      labs(fill="",x="",y = "")+
      scale_colour_manual(values=wes_palette("Darjeeling1",length(unique(tableCarac$theme)),type="discrete"),name=translator$t("Thème")) +
      scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
      theme_chart()
      
      {if(allSite==TRUE & facetWrapOption==TRUE) gg <- gg+facet_wrap(~site_nom) else gg}
      {if(allSite==FALSE & facetWrapOption==TRUE) gg <- gg+facet_wrap(~station_nom) else gg}
    
return(ggplotly(gg,tooltip = c("text"))%>%layout(legend = list(orientation = "h", x = 0.4, y = -0.05))
)

}

#' @title sensorSelectedMap
#' @description Chart of disponibility data (welcome module)
#' @param tableCarac data.table from mod_welcome
#' @param allSite Logical operator to select allSite or not (TRUE, FALSE)
#' @param facetWrapOption Logical operator to use facetWrap on site or site/station (TRUE, FALSE)
#' @param translator translator object for translation
#' @return dygraph of timetrend
#' @importFrom stringr str_extract_all
#' @import leaflet
#' @import data.table
#' @export
sensorSelectedMap <- function(mapSensorSelected,allSite=FALSE,translator){

    # Extraction des coordonnées des stations 
    mapSensorSelected$lng <- matrix(as.numeric(unlist(
      str_extract_all(mapSensorSelected$zet_coordonnees_bbox, pattern = "-?\\d+\\.?\\d*")
    )), ncol = 2, byrow = T)[,1]
    
    mapSensorSelected$lat <- matrix(as.numeric(unlist(
      str_extract_all(mapSensorSelected$zet_coordonnees_bbox, pattern = "-?\\d+\\.?\\d*")
    )), ncol = 2, byrow = T)[,2]

    description_station <- paste0("<b>Site/Station : </b>",mapSensorSelected$code_site_station,"<br/>",
                                 "<b>Station : </b>",mapSensorSelected$station_description,"<br/>")
    
#    code_station <- paste(sep = "",
 #                                "<b>Code Site/Station : </b>",mapSensorSelected$code_site_station,"<br/>")
    # Condition pour afficher tous les sites
    if(allSite==TRUE){
      lng <- 2.067
      lat <- 46
      zoom <- 5 

    mapSiteSelected <- mapSensorSelected[,.(lngSite = mean(lng,na.rm=TRUE),
                     latSite = mean(lat,na.rm=TRUE)),by=list(site_nom)]
    description_site <- paste0("<b>Site SNO-Tourbières : </b>",mapSiteSelected$site_nom,"<br/>")
    }else{
      # lng et lat sur la dernière ligne
      lng <- mapSensorSelected[,.SD[.N]]$lng
      lat <- mapSensorSelected[,.SD[.N]]$lat
      zoom <- 16
    }

map <- leaflet(mapSensorSelected)%>%
      setView(lng = lng, lat = lat,zoom=zoom)%>%
      
      # Ajout des tuiles (pour en rajouter voir http://leaflet-extras.github.io/leaflet-providers/preview/)
      addProviderTiles("OpenStreetMap.DE",group="Open Street Map")%>%
      addProviderTiles("Esri.WorldImagery",group = "Image satellite") %>%
      
      # Ajout des stations 
      addCircleMarkers(lng = mapSensorSelected$lng, lat = mapSensorSelected$lat, 
                       radius=6,
                       fillColor = mapSensorSelected$couleur,color="#000000",weight = 1.7,opacity = 0.8,
                       stroke = TRUE, fillOpacity = 0.8, popup = description_station)%>%

      # Ajout des bouton pour revenir à la carte de france
      addEasyButton(easyButton(
        icon="fa-globe", title="France",
        onClick=JS("function(btn, map){map.setZoom(6); }")))  %>%

      # Pour contrôler les couches
      addLayersControl(
        baseGroups = c("Open Street Map", "Image satellite"),
        options = layersControlOptions(collapsed = FALSE),position = "topleft"
      )%>%

      leaflet::addLegend(position = 'topright',
                          colors = unique(mapSensorSelected$couleur), 
                          labels = unique(mapSensorSelected$type),title=translator$t("Type de station"))
      
      # Ajout des sites
      {if(allSite==TRUE) map <- addAwesomeMarkers(map,lng = mapSiteSelected$lngSite,
       lat = mapSiteSelected$latSite,popup = description_site) else map}

      return(map)
}

#' @title graphWindRose
#' @description Wind rose chart based on plot.windrose
#' @param subsetoutbd data.table from mod_accessdata
#' @return chart of Wind rose
#' @importFrom data.table dcast
#' @export
graphWindRose <- function(subsetoutbd){
if(nrow(subsetoutbd)==0){
    res <- NULL
  }else{

  Bdwind <- dcast(subsetoutbd[variable %in% c("WS_1_1_1","WD_1_1_1"),c("Date","code_site_station","variable","value")],formula=Date+code_site_station~variable)
  Bdwind <- Bdwind[complete.cases(Bdwind[,c("WS_1_1_1","WD_1_1_1")]),]
 
  res <- lapply(unique(Bdwind$code_site_station),function(z){
    plot.windrose(data=Bdwind[code_site_station %in% z,],
        spd="WS_1_1_1",
        dir="WD_1_1_1",
          spdres=0.7,
#         spdseq = c(0,3,6,12,20)
          spdmin=round(min(Bdwind$WS_1_1_1),2),spdmax=round(max(Bdwind$WS_1_1_1),2),
          titreLegend="Vitesse du vent (m/s)",
          titre=z)
  })
}
  return(res)
}

#' @title plot.windrose
#' @description ggplot2 chart of wind rose
#' @param data data.frame
#' @param spd String of wind speed column
#' @param dir String of wind direction column
#' @param spdres Speed resolution
#' @param dirres Direction resolution
#' @param spdmin Speed scale min
#' @param spdmax Speed scale max
#' @param spdseq Speed scale sequence
#' @param palette RColorBrewer code palette (ex. "YlGnBu")
#' @param countmax NA by default
#' @param debug debug mode with 1, not with 0 (default)
#' @param titreLegend Title of legend
#' @param titre Title of chart
#' @return chart of Wind rose
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRampPalette
#' @import ggplot2
#' @source https://stackoverflow.com/questions/17266780/wind-rose-with-ggplot-r
#' @export

plot.windrose <- function(data,
                      spd,
                      dir,
                      spdres = 2,
                      dirres = 30,
                      spdmin = 2,
                      spdmax = 20,
                      spdseq = NULL,
                      palette = "YlGnBu",
                      countmax = NA,
                      debug = 0,
                      titreLegend = "",
                      titre=""){


# Look to see what data was passed in to the function
  if (is.numeric(spd) & is.numeric(dir)){
    # assume that we've been given vectors of the speed and direction vectors
    data <- data.frame(spd = spd,
                       dir = dir)
    spd = "spd"
    dir = "dir"
  } else if (exists("data")){
    # Assume that we've been given a data frame, and the name of the speed 
    # and direction columns. This is the format we want for later use.    
  }  

  # Tidy up input data ----
  n.in <- NROW(data)
  dnu <- (is.na(data[[spd]]) | is.na(data[[dir]]))
  data[[spd]][dnu] <- NA
  data[[dir]][dnu] <- NA

  # figure out the wind speed bins ----
  if (missing(spdseq)){
    spdseq <- seq(spdmin,spdmax,spdres)
  } else {
    if (debug >0){
      cat("Using custom speed bins \n")
    }
  }
  # get some information about the number of bins, etc.
  n.spd.seq <- length(spdseq)
  n.colors.in.range <- n.spd.seq - 1

  # create the color map
  spd.colors <- colorRampPalette(brewer.pal(min(max(3,
                                                    n.colors.in.range),
                                                min(9,
                                                    n.colors.in.range)),                                               
                                            palette))(n.colors.in.range)

  if (max(data[[spd]],na.rm = TRUE) > spdmax){    
    spd.breaks <- c(spdseq,
                    max(data[[spd]],na.rm = TRUE))
    spd.labels <- c(paste(c(spdseq[1:n.spd.seq-1]),
                          '-',
                          c(spdseq[2:n.spd.seq])),
                    paste(spdmax,
                          "-",
                          round(max(data[[spd]],na.rm = TRUE),2)))
    spd.colors <- c(spd.colors, "grey50")
  } else{
    spd.breaks <- spdseq
    spd.labels <- paste(c(spdseq[1:n.spd.seq-1]),
                        '-',
                        c(spdseq[2:n.spd.seq]))    
  }
  data$spd.binned <- cut(x = data[[spd]],
                         breaks = spd.breaks,
                         labels = spd.labels,
                         ordered_result = TRUE)
  # clean up the data
  data. <- na.omit(data)

  # figure out the wind direction bins
  dir.breaks <- c(-dirres/2,
                  seq(dirres/2, 360-dirres/2, by = dirres),
                  360+dirres/2)  
  dir.labels <- c(paste(360-dirres/2,"-",dirres/2),
                  paste(seq(dirres/2, 360-3*dirres/2, by = dirres),
                        "-",
                        seq(3*dirres/2, 360-dirres/2, by = dirres)),
                  paste(360-dirres/2,"-",dirres/2))
  # assign each wind direction to a bin
  dir.binned <- cut(data[[dir]],
                    breaks = dir.breaks,
                    ordered_result = TRUE)
  levels(dir.binned) <- dir.labels
  data$dir.binned <- dir.binned

  # Run debug if required ----
  if (debug>0){    
    cat(dir.breaks,"\n")
    cat(dir.labels,"\n")
    cat(levels(dir.binned),"\n")       
  }  

  # deal with change in ordering introduced somewhere around version 2.2
  if(packageVersion("ggplot2") > "2.2"){    
    cat("Hadley broke my code\n")
    data$spd.binned = with(data, factor(spd.binned, levels = rev(levels(spd.binned))))
    spd.colors = rev(spd.colors)
  }

  # create the plot ----
  p.windrose <- ggplot(data = data,
                       aes(x = dir.binned,
                           fill = spd.binned)) +
    geom_bar() + 
    scale_x_discrete(drop = FALSE,
                     labels = waiver()) +
    coord_polar(start = -((dirres/2)/360) * 2*pi) +
    scale_fill_manual(name = titreLegend, 
                      values = spd.colors,
                      drop = FALSE) +
    theme(axis.title.x = element_blank()) +
    labs(title=titre)

  # adjust axes if required
  if (!is.na(countmax)){
    p.windrose <- p.windrose +
      ylim(c(0,countmax))
  }

  # print the plot
  print(p.windrose)  

  # return the handle to the wind rose
  return(p.windrose)
}