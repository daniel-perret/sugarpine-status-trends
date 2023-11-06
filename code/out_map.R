library(leafem)
library(htmlwidgets)
opacity <- 1
zoneColor <- "blue"
leafProj <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs +type=crs"

pal <- colorNumeric(
  palette = colorRampPalette(c('white', 'green'))(length(pila.plts$live_PILA)), 
  domain = pila.plts$live_PILA)

icons <- awesomeIcons(icon = "egg",
                      iconColor = pal(pila.plts$live_PILA),
                      library = "ion",
                      markerColor = "black",
                      squareMarker = F)

rtp.leaf <- sf::st_transform(rtp,CRS(old.proj))
mtbs.leaf <- mtbs %>% 
  sf::st_as_sf() %>% 
  sf::st_transform(.,CRS(old.proj))

recent.mtbs.leaf <- recent.mtbs %>% 
  sf::st_as_sf() %>% 
  sf::st_transform(.,CRS(old.proj))

gnn.leaf <- gnn.sca
fast.leaf <- fast.sca.post2010poly; names(fast.leaf) <- "layer"

gnn.pal <- colorBin(c("white","green4"), domain = values(gnn.leaf), bins = 6, na.color = "transparent")
fast.pal <- colorBin("YlOrRd", domain = 2010:2021, bins = 2010:2021)

leaflet() %>% 
  # basemap groups
  addTiles(group = "OSM (default)") %>% 
  addProviderTiles(providers$Stamen.TonerLite, group = "Light") %>% 
  addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>% 
  addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>% 
  
# polygon groups -----

addPolygons(data = rtp.leaf %>% 
              filter(climZone%in%c("A2")),
            fillColor = zoneColor,
            fillOpacity = opacity,
            stroke = T, col = "black", weight = 2.5,
            popup = ~climZone,
            group = "A2") %>% 
  addPolygons(data = rtp.leaf %>% 
                filter(climZone%in%c("A3")),
              fillColor = zoneColor,
              fillOpacity = opacity,
              stroke = T, col = "black", weight = 2.5,
              popup = ~climZone,
              group = "A3") %>% 
  addPolygons(data = rtp.leaf %>% 
                filter(climZone%in%c("A4")),
              fillColor = zoneColor,
              fillOpacity = opacity,
              stroke = T, col = "black", weight = 2.5,
              popup = ~climZone,
              group = "A4") %>% 
  addPolygons(data = rtp.leaf %>% 
                filter(climZone%in%c("A5")),
              fillColor = zoneColor,
              fillOpacity = opacity,
              stroke = T, col = "black", weight = 2.5,
              popup = ~climZone,
              group = "A5") %>% 
  
  addPolygons(data = rtp.leaf %>% 
                filter(climZone%in%c("B1")),
              fillColor = zoneColor,
              fillOpacity = opacity,
              stroke = T, col = "black", weight = 2.5,
              popup = ~climZone,
              group = "B1") %>% 
  addPolygons(data = rtp.leaf %>% 
                filter(climZone%in%c("B2")),
              fillColor = zoneColor,
              fillOpacity = opacity,
              stroke = T, col = "black", weight = 2.5,
              popup = ~climZone,
              group = "B2") %>% 
  addPolygons(data = rtp.leaf %>% 
                filter(climZone%in%c("B3")),
              fillColor = zoneColor,
              fillOpacity = opacity,
              stroke = T, col = "black", weight = 2.5,
              popup = ~climZone,
              group = "B3") %>% 
  addPolygons(data = rtp.leaf %>% 
                filter(climZone%in%c("B4")),
              fillColor = zoneColor,
              fillOpacity = opacity,
              stroke = T, col = "black", weight = 2.5,
              popup = ~climZone,
              group = "B4") %>% 
  addPolygons(data = rtp.leaf %>% 
                filter(climZone%in%c("B5")),
              fillColor = zoneColor,
              fillOpacity = opacity,
              stroke = T, col = "black", weight = 2.5,
              popup = ~climZone,
              group = "B5") %>% 
  
  addPolygons(data = rtp.leaf %>% 
                filter(climZone%in%c("C1")),
              fillColor = zoneColor,
              fillOpacity = opacity,
              stroke = T, col = "black", weight = 2.5,
              popup = ~climZone,
              group = "C1") %>% 
  addPolygons(data = rtp.leaf %>% 
                filter(climZone%in%c("C2")),
              fillColor = zoneColor,
              fillOpacity = opacity,
              stroke = T, col = "black", weight = 2.5,
              popup = ~climZone,
              group = "C2") %>% 
  addPolygons(data = rtp.leaf %>% 
                filter(climZone%in%c("C3")),
              fillColor = zoneColor,
              fillOpacity = opacity,
              stroke = T, col = "black", weight = 2.5,
              popup = ~climZone,
              group = "C3") %>% 
  addPolygons(data = rtp.leaf %>% 
                filter(climZone%in%c("C4")),
              fillColor = zoneColor,
              fillOpacity = opacity,
              stroke = T, col = "black", weight = 2.5,
              popup = ~climZone,
              group = "C4") %>% 
  # addPolygons(data = rtp.leaf %>% 
  #               sf::st_transform(CRS(old.proj)) %>% 
  #               filter(climZone%in%c("C5")),
  #             fillColor = zoneColor,
  #             fillOpacity = opacity,
  #             stroke = T, col = "black", weight = 2.5,
  #             group = "C5") %>% 
  
  addPolygons(data = rtp.leaf %>% 
                filter(climZone%in%c("D1")),
              fillColor = zoneColor,
              fillOpacity = opacity,
              stroke = T, col = "black", weight = 2.5,
              popup = ~climZone,
              group = "D1") %>% 
  addPolygons(data = rtp.leaf %>% 
                filter(climZone%in%c("D2")),
              fillColor = zoneColor,
              fillOpacity = opacity,
              stroke = T, col = "black", weight = 2.5,
              popup = ~climZone,
              group = "D2") %>% 
  addPolygons(data = rtp.leaf %>% 
                filter(climZone%in%c("D3")),
              fillColor = zoneColor,
              fillOpacity = opacity,
              stroke = T, col = "black", weight = 2.5,
              popup = ~climZone,
              group = "D3") %>% 
  addPolygons(data = rtp.leaf %>% 
                filter(climZone%in%c("D4")),
              fillColor = zoneColor,
              fillOpacity = opacity,
              stroke = T, col = "black", weight = 2.5,
              popup = ~climZone,
              group = "D4") %>% 
  addPolygons(data = rtp.leaf %>% 
                filter(climZone%in%c("D5")),
              fillColor = zoneColor,
              fillOpacity = opacity,
              stroke = T, col = "black", weight = 2.5,
              popup = ~climZone,
              
              group = "D5") %>% 
  
  addPolygons(data = rtp.leaf %>% 
                filter(climZone%in%c("E1")),
              fillColor = zoneColor,
              fillOpacity = opacity,
              stroke = T, col = "black", weight = 2.5,
              group = "E1") %>% 
  addPolygons(data = rtp.leaf %>% 
                filter(climZone%in%c("E2")),
              fillColor = zoneColor,
              fillOpacity = opacity,
              stroke = T, col = "black", weight = 2.5,
              popup = ~climZone,
              group = "E2") %>% 
  # addPolygons(data = rtp %>% 
  #               sf::st_transform(CRS(old.proj)) %>% 
  #               filter(climZone%in%c("E3")),
  #             fillColor = zoneColor,
  #             fillOpacity = opacity,
  #             stroke = T, col = "black", weight = 2.5,
  #             group = "E3") %>% 
  # addPolygons(data = rtp %>% 
  #               sf::st_transform(CRS(old.proj)) %>% 
  #               filter(climZone%in%c("E4")),
  #             fillColor = zoneColor,
#             fillOpacity = opacity,
#             stroke = T, col = "black", weight = 2.5,
#             group = "E4") %>% 
# addPolygons(data = rtp %>% 
#               sf::st_transform(CRS(old.proj)) %>% 
#               filter(climZone%in%c("E5")),
#             fillColor = zoneColor,
#             fillOpacity = opacity,
#             stroke = T, col = "black", weight = 2.5,
#             group = "E5") %>% '

# niche study design ----

# leafem::addLogo(img = "nichedesign.png",
#         src = "local",
#         position = "bottomright",
#         width = 450,
#         height = 400,
#         offset.x = 0,
#         offset.y = -50) %>% 

# USFS boundaries ----

addPolygons(data = usfs,
            #fill = F,
            fillColor = "forestgreen",
            fillOpacity = 0.3,
            stroke = TRUE,
            color = "forestgreen",
            weight = 3,
            popup = ~ADMIN_UNIT,
            group = "USFS boundaries") %>% 
  
  #FIA plots ----
addAwesomeMarkers(data = pila.plts,
                  #opacity = 1,
                  lng = ~LON_EXACT,
                  lat=~LAT_EXACT,
                  group = "FIA plots",
                  popup= ~paste("Plot CN: ", PLT_CN,
                                "<br>Year: ", MEASYEAR,
                                "<br>Live PILA: ", live_PILA,
                                "<br>Dead PILA: ", dead_PILA,
                                "<br>Mean DIA: ", mean_PILA_size,
                                "<br>Lat: ", LAT,
                                "<br>Long: ", LON,
                                "<br>Elev: ", ELEV),
                  icon = icons) %>%
  
  # Collaborator sampling locations ----
addCircleMarkers(data = collab.plts,
                 lng = ~LON,
                 lat=~LAT,
                 fillColor = "dodgerblue2",
                 group="Collaborator samples",
                 popup = ~paste("PI: ", PI,
                                "<br>Location: ", Location,
                                "<br>Comments: ", Comments)) %>% 
  
  # Preliminary sampling locations ----
addCircleMarkers(data = site.plts,
                 lng = ~LON,
                 lat = ~LAT,
                 fillColor = "red",
                 color = "red",
                 group = "Preliminary sites",
                 popup = ~paste("Site: ", SITEID,
                                "<br>Zone: ", ZONE,
                                "<br>Forest: ", FOREST,
                                "<br>Description: ", DESCRIPTION,
                                "<br>GNN: ", GNN,
                                "<br>Comment: ", COMMENT)) %>% 
  
  # gnn map ----

addRasterImage(x = gnn.leaf,
               opacity=0.9,
               project=FALSE,
               colors = gnn.pal,
               group="2017 PILA BA (GNN)",
               #popup = ~paste("PILA BA: ", value),
               maxBytes = Inf) %>% 
  addLegend(pal = gnn.pal, values = values(gnn.leaf), 
            title = "2017 PILA BA", position = "topright") %>% 
  
  # fast change detection ----

# addRasterImage(x = fast.change.or.post2010,
#                opacity = 0.6,
#                project=FALSE,
#                group = "Fast disturbance 2011-2021",
#                colors = "red",
#                #popup = ~paste("Disturbance year: ", value),
#                maxBytes = Inf) %>%

addPolygons(data = fast.leaf,
            fillColor = ~fast.pal(layer),
            stroke = FALSE,
            opacity=1,
            popup = ~paste("Disturbance year: ", layer),
            group = "Fast disturbance 2011-2021")  %>%
  addLegend(pal = fast.pal, values = 2010:2021, title = "Disturbance year",
            position = "topright") %>% 
  
  # MTBS perimeters ----

addPolygons(data = mtbs.leaf,
            fillColor = "black",
            fillOpacity = 0.6,
            stroke = F,
            popup = ~paste("Fire: ", Incid_Name,
                           "<br>Date: ", Ig_Date),
            group = "MTBS perimeters") %>%
  
  
  addPolygons(data = recent.mtbs.leaf,
              fillColor = "black",
              fillOpacity = 0.6,
              stroke = F,
              popup = ~paste("Fire: ", poly_Incid,
                             "<br>Date: ", poly_Creat),
              group = "Recent MTBS perimeters") %>%
  
  # layers control ----
addMouseCoordinates() %>% 
  addLayersControl(
    baseGroups = c("OSM (default)", "Light", "Dark", "Topo", "Imagery"),
    overlayGroups = c("FIA plots",
                      "Preliminary sites",
                      "Collaborator samples",
                      "USFS boundaries",
                      "2017 PILA BA (GNN)",
                      "Fast disturbance 2011-2021",
                      "MTBS perimeters",
                      "Recent MTBS perimeters",
                      #"A1","A2","A3","A4","A5",
                      "A2","A3","A4","A5",
                      "B1","B2","B3","B4","B5",
                      #"C1","C2","C3","C4","C5",
                      "C1","C2","C3","C4",
                      #"D1","D2","D3","D4","D5",
                      "D1","D2","D3","D4",
                      #"E1","E2","E3","E4","E5"),
                      "E1","E2"),
    options = layersControlOptions(collapsed = FALSE,
                                   position = "topleft")
  ) %>% 
  hideGroup(c("FIA plots",
              "Preliminary sites",
              "Collaborator samples",
              "USFS boundaries",
              "2017 PILA BA (GNN)",
              "Fast disturbance 2011-2021",
              "MTBS perimeters",
              "Recent MTBS perimeters",
              "A1","A2","A3","A4","A5",
              "B1","B2","B3","B4","B5",
              "C1","C2","C3","C4","C5",
              "D1","D2","D3","D4","D5",
              "E1","E2","E3","E4","E5")) %>% 
  
  #OPACITY SLIDER
  addControl(html = "<input id=\"OpacitySlide\" type=\"range\" min=\"0\" max=\"1\" step=\"0.1\" value=\"0.5\">", position="bottomleft") %>%   
  # Add Slider
  htmlwidgets::onRender(
    "function(el,x,data){
                     var map = this;
                     var evthandler = function(e){
                        var layers = map.layerManager.getVisibleGroups();
                        console.log('VisibleGroups: ', layers); 
                        console.log('Target value: ', +e.target.value);
                        layers.forEach(function(group) {
                          var layer = map.layerManager._byGroup[group];
                          console.log('currently processing: ', group);
                          Object.keys(layer).forEach(function(el){
                            if(layer[el] instanceof L.Polygon){;
                            console.log('Change opacity of: ', group, el);
                             layer[el].setStyle({fillOpacity:+e.target.value});
                            }
                          });
                          
                        })
                     };
              $('#OpacitySlide').mousedown(function () { map.dragging.disable(); });
              $('#OpacitySlide').mouseup(function () { map.dragging.enable(); });
              $('#OpacitySlide').on('input', evthandler)}
          ") -> leafmap


# export----
saveWidget(widget = leafmap,
           file = "sCA_samplingmapV3_incFIA.html",
           selfcontained = TRUE)

