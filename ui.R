library(shiny)
library(leaflet)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(
    top = 10,
    right = 20,
    width = 400,
    draggable = TRUE,
    wellPanel(
      h3("Road Traffic Accidents in York, UK between 2005 and 2014"),
      tabsetPanel(type = "tabs",
        tabPanel("Map Filtering",
          br(),
          p("Please filter your results by using the drop down lists and text boxes below:"),
          selectInput("sel_severity",
                      label = "Choose Accident Severity",
                      choices = c("All"=0, "Fatal"=1, "Serious"=2, "Slight"=3),
                      selected = "0"),

          selectInput("sel_road_type",
                      label = "Choose a Road Type",
                      choices = c("All"=0, "Roundabout"=1, "One Way Street"=2, "Dual Carriageway"=3,
                                  "Single Carriageway"=6, "Slip Road"=7),
                      selected = "0"),

          dateRangeInput('sel_date_range',
                         label = 'Date range input: yyyy-mm-dd',
                         start = "2005-01-01", end = "2014-12-31"
          ),

          selectInput("sel_heat_map",
                      label = "Show Density Map",
                      choices = c("No"=0, "Yes"=1),
                      selected = "0"),
          p("Please Note: Turn off the density map to view accident details.")
        ),
        tabPanel("About", includeMarkdown("about.md"))
      )
    ),
    style="opacity: 0.9"
  )
)
