library(shiny)
library(dplyr)
library(lubridate)
library(leaflet)
library(RColorBrewer)
library(KernSmooth)

accidents <- read.csv("./data/accidents.csv", header=TRUE, na.strings = c("","NA","-1"))

# Create Presets
severity_enum <- c("Fatal", "Serious", "Slight")
light_cond_enum <- c("Daylight","","","Darkness - Lights Lit", "Darkness - Lights Unlit", "Darkness - No Lighting",
                     "Darkness - Lighting Unknown")
weather_enum <- c("Fine no High Winds", "Raining no High Winds", "Snowing no High Winds", "Fine + High Winds",
                  "Raining + High Winds", "Snowing + High Winds", "Fog or Mist", "Other", "Unknown")
road_surface_enum <- c("Dry", "Wet or Damp", "Snow", "Frost or Ice", "Flood over 3cm", "Oil or Diesel", "Mud")


# Convert the date
accidents$Date <- dmy_hm(accidents$Date)

# Add Day Name to Dataset
accidents$Day <- factor(wday(accidents$Day_of_Week, label = TRUE, abbr = FALSE))

# Create Severity Column
accidents$Severity <- severity_enum[accidents$Accident_Severity]


shinyServer(
  function(input, output) {

    pal <- colorFactor(c("red","blue","green4"), domain=c("1","2","3"))

    accident_popup <- paste0("<h4>Accident Details</h4><strong>Date: </strong>", accidents$Date,
                             "<br><strong>Severity: </strong>", severity_enum[accidents$Accident_Severity],
                            "<br><strong>Casualties: </strong>", accidents$Number_of_Casualties,
                             "<br><strong>Vehicles: </strong>", accidents$Number_of_Vehicles,
                            "<br><strong>Light Conditions: </strong>", light_cond_enum[accidents$Light_Conditions],
                            "<br><strong>Weather Conditions: </strong>", weather_enum[accidents$Weather_Conditions],
                            "<br><strong>Road Surface: </strong>", road_surface_enum[accidents$Road_Surface_Conditions])

    road_type <- reactive({
      temp <- accidents[accidents$Date >= as.character(input$sel_date_range[1]) &
                accidents$Date <= as.character(input$sel_date_range[2]), ]

      if (input$sel_road_type > 0) {
        temp <- temp[temp$Road_Type == input$sel_road_type,]
      }

      if(input$sel_severity > 0) {
        temp <- temp[temp$Accident_Severity == input$sel_severity,]
      }
      road_type <- temp
    })

    output$map <- renderLeaflet({
      leaflet(accidents) %>%
        addTiles() %>%
        fitBounds(~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude))
    })

    observe({
      X <- cbind(road_type()$Longitude, road_type()$Latitude)

      if (input$sel_road_type == 2) {
        kde2d <- bkde2D(X, bandwidth=c(0.00225,0.00225))
      }
      else {
        kde2d <- bkde2D(X, bandwidth=c(0.00500,0.00500))
      }
      x=kde2d$x1
      y=kde2d$x2
      z=kde2d$fhat

      CL <- contourLines(x, y, z)

      leafletProxy("map", data=road_type()) %>%
        clearShapes() %>%
        addCircles(popup=accident_popup,
                   radius=~((4 - Accident_Severity) * 30),
                   stroke = FALSE, fillColor = ~pal(Accident_Severity),
                   fillOpacity = 0.7)

      if (input$sel_heat_map == 1) {
        leafletProxy("map", data=road_type()) %>%
          addTiles() %>%
          addPolygons(CL[[1]]$x, CL[[1]]$y,fillColor = "red", stroke = FALSE) %>%
          addPolygons(CL[[2]]$x, CL[[2]]$y,fillColor = "red", stroke = FALSE) %>%
          addPolygons(CL[[3]]$x, CL[[3]]$y,fillColor = "red", stroke = FALSE) %>%
          addPolygons(CL[[4]]$x, CL[[4]]$y,fillColor = "red", stroke = FALSE) %>%
          addPolygons(CL[[5]]$x, CL[[5]]$y,fillColor = "red", stroke = FALSE) %>%
          addPolygons(CL[[6]]$x, CL[[6]]$y,fillColor = "red", stroke = FALSE) %>%
          addPolygons(CL[[7]]$x, CL[[7]]$y,fillColor = "red", stroke = FALSE) %>%
          addPolygons(CL[[8]]$x, CL[[8]]$y,fillColor = "red", stroke = FALSE) %>%
          addPolygons(CL[[9]]$x, CL[[9]]$y,fillColor = "red", stroke = FALSE) %>%
          addPolygons(CL[[10]]$x, CL[[10]]$y,fillColor = "red", stroke = FALSE)
      }
    })

    observe({
      proxy <- leafletProxy("map", data = accidents)

            proxy %>% clearControls()

      val <- proxy %>%
        addLegend(position = "bottomleft",
                  title = "Accident Severity",
                  pal = pal,
                  values = ~factor(accidents$Accident_Severity, labels=c("Fatal","Serious","Slight"))
      )

      output$results <- renderTable({
        accident_count <- road_type() %>%
          group_by(Severity) %>%
          summarise(Total = length(Accident_Severity))

        accident_count
      })
    })

    output$plot_hist  <- renderPlot({
      map <- get_map(location = "UK, York",
                     zoom = 14,
                     maptype = "roadmap")

      p <- ggmap(map)
      overlay <- stat_density2d(data=road_type(),
                                aes(x = Longitude, y = Latitude, fill= ..level.., alpha=..level..),
                                size=5, bins=8, geom="polygon")
      p <- p + overlay
      print(p)
    })



  }
)
