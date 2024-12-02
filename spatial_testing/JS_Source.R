bgc_tileserver <- "https://tileserver.thebeczone.ca/data/WNA_MAP/{z}/{x}/{y}.pbf"
bgc_tilelayer <- "WNA_MAP"

plugins <- {list(vgplugin = 
                   htmltools::htmlDependency(
                     name = "leaflet.vectorgrid",
                     version = "1.3.0",
                     src = "htmlwidgets",
                     script = c("lfx-vgrid-prod.js","leaflet-tilelayer-colorpicker.js")
                   )
)
}
registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

addPlugin <- function(map) {
  map <- registerPlugin(map, plugins$vgplugin)
  map
}


addBGCTiles <- function(map) {
  map <- htmlwidgets::onRender(map, paste0('
    function(el, x, data) {
      ', paste0("var subzoneColors = {", paste0("'", subzones_colours_ref$classification, "':'", 
      subzones_colours_ref$colour,"'", collapse = ","), "}"), '
      
      L.bec_layer_opacity = 0.5
      
      var vectorTileOptions=function(layerName, layerId, activ,
                             lfPane, colorMap, prop, id) {
        return {
          vectorTileLayerName: layerName,
          interactive: activ, // makes it able to trigger js events like click
          vectorTileLayerStyles: {
            [layerId]: function(properties, zoom) {
              return {
                weight: 0,
                fillColor: colorMap[properties[prop]],
                fill: true,
                fillOpacity: L.bec_layer_opacity
              }
            }
          },
          pane : lfPane,
          getFeatureId: function(f) {
              return f.properties[id];
          }
        }
        
      };
      
      var subzLayer = L.vectorGrid.protobuf(
        "', bgc_tileserver, '",
        vectorTileOptions("bec_map", "', bgc_tilelayer, '", true,
                          "tilePane", subzoneColors, "MAP_LABEL", "MAP_LABEL")
      )
      this.layerManager.addLayer(subzLayer, "tile", "bec_map", "BGCs");
     subzLayer.on("click", function(e){
      Shiny.setInputValue("bgc_click",e.layer.properties.MAP_LABEL);
     });
     
     var selectHighlight = "SBSdk";
      subzLayer.on("click", function(e){
        console.log(e.layer);
        subzLayer.resetFeatureStyle(selectHighlight);
        Shiny.setInputValue("bgc_click",e.layer.properties.MAP_LABEL);
        var properties = e.layer.properties
  			  highlight = properties.MAP_LABEL
  			  var style = {
            weight: 1,
            color: "#fc036f",
            fillColor: subzoneColors[properties.MAP_LABEL],
            fillOpacity: 1,
            fill: true
          }
          subzLayer.setFeatureStyle(properties.MAP_LABEL, style);
      });
      
      
      var highlight
		  var clearHighlight = function() {
		  	if (highlight) {
		  		subzLayer.resetFeatureStyle(highlight);
		  	}
		  	highlight = null;
		  }
		  
      subzLayer.on("mouseout", function(e) {
        clearHighlight();
      })
     
    }'
  ))
  map
}

addRasterTiles <- function(map) {
  map <- htmlwidgets::onRender(map, paste0('
    function(el, x, data) {
      ', paste0("var subzoneColors = {", paste0("'", subzones_colours_ref$colour, "':'", 
      subzones_colours_ref$classification,"'", collapse = ","), "}"), '
      
    var baseCols = Object.keys(subzoneColors);
    console.log("working");
    map = this;
    
    function hexToRgb(hex) {
      hex = hex.replace(\'#\', \'\');
      const r = parseInt(hex.substring(0, 2), 16);
      const g = parseInt(hex.substring(2, 4), 16);
      const b = parseInt(hex.substring(4, 6), 16);
      return { r, g, b };
    }
    
    function prepRgb(rgb) {
      const r = rgb[0];
      const g = rgb[1];
      const b = rgb[2];
      return { r, g, b };
    }
    
    
    function findNearestColor(inputRgb, colorList) {
      let nearestColor = null;
      let smallestDistance = Infinity;
      
      for (const color of colorList) {
        const colorRgb = hexToRgb(color);
        
        // Calculate Euclidean distance
        const distance = Math.sqrt(
          Math.pow(colorRgb.r - inputRgb.r, 2) +
          Math.pow(colorRgb.g - inputRgb.g, 2) +
          Math.pow(colorRgb.b - inputRgb.b, 2)
        );
        
        if (distance < smallestDistance) {
          smallestDistance = distance;
          nearestColor = color;
        }
      }
      
      return nearestColor;
    }
    
    Shiny.addCustomMessageHandler("update_tiles", function(tile_url){
      t2 = tile_url + "?nocache";
      console.log(t2);
      //map.removeLayer(colorpicker);
      colorpicker = L.tileLayer.colorPicker(t2, {
        attribution: "CCISS"
      }).addTo(map);
    });
    
    
    //var colorpicker = L.tileLayer.colorPicker("https://tileserver.thebeczone.ca/data/bgc_EC-Earth3_2041_2060/{z}/{x}/{y}.png?nocache", {
     //attribution: "CCISS"
    //}).addTo(this);

    this.on("mousemove", function(event) {
      var a = colorpicker.getColor(event.latlng);
      if (a !== null) {
        var bgcCol = findNearestColor(prepRgb(a), baseCols)
        var bgc = subzoneColors[bgcCol]
        
        L.popup()
        .setLatLng(event.latlng)
        .setContent(bgc)
        .openOn(this);
      }
    });
    }
      '
  ))
  map
}

