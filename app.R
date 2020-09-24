library(leaflet)
library(htmltools)
library(htmlwidgets)
library(sf)
library(tidyverse)
library(rgdal)
library(shiny)
library(shinythemes)
library(ggmap)
library(censusr)
library(ggthemes)
library(rJava)
library(mailR)
library(shinyjs)
library(shinyWidgets)

source("googleKey.R") # Import google API key used for address lookup
source("password.R") # Import password for the email address used to email the feedback form submissions to my email

## MAP ---------------------------------------------------

projection <- CRS("+proj=longlat +datum=WGS84")

# Read shapefiles ----------------------------------------

# Extreme precipitation
scenario2 <- vector("list", 5)

for (i in 0:4) {
  layer_name <- str_c("scenario2_", as.character(i))
  scenario2[[i+1]] <- st_read(dsn = "Shapefiles",
                              layer = layer_name) %>%
    #ms_simplify() %>%
    st_transform(projection)
}

# Sea level rise/storm surge
scenario3 <- vector("list", 3)

for (i in 0:2) {
  layer_name <- str_c("scenario3_", as.character(i))
  scenario3[[i+1]] <- st_read(dsn = "Shapefiles",
                              layer = layer_name) %>%
    #ms_simplify() %>%
    st_transform(projection)
}

# Adaptive capacity
totalAC <- st_read(dsn = "Shapefiles",
                   layer = "totalAC") %>%
  st_transform(projection)

# Community resources
muniList <- sort(unique(totalAC$Muni))
resourceList <- c("churches", "hospitals", "schools")
resources <- vector("list", 4)

for (i in 1:3) {
  resources[[i]] <- vector("list", 17)
  for (j in 1:17) {
    layer_name <- str_c(as.character(muniList[[j]]), as.character(resourceList[[i]]))
    resources[[i]][[j]] <- st_read(dsn = "Shapefiles",
                             layer = layer_name) %>%
      st_transform(projection)
  }
}

resources[[4]] <- st_read(dsn = "Shapefiles",
                          layer = "Municipalgovernments") %>%
  st_transform(projection)

# Create a sorted list of municipalities for later use
muniTracts <- vector("list", 17)

for (i in 1:17) {
  muniTracts[[i]] <- as.data.frame(totalAC) %>%
    filter(Muni == muniList[[i]]) %>%
    select(NAME10) %>%
    arrange(NAME10)
}

# Set palettes ----------------------------------------
pal2 <- "#69ade5"
pal3 <- "#c9f2ee"
palAC <- colorFactor(palette = "Reds",
                     domain = 1:5,
                     reverse = T)
palChurches <- "#FA6AA1"
palHospitals <- "#ACAA26"
palSchools <- "#379492"
palGovernments <- "#B87CFF"

# Set highlight and labels ----------------------------
highlightSet <- highlightOptions(
  weight = 3,
  color = "#666",
  fillOpacity = 0.8,
  bringToFront = F)

labelSet <- labelOptions(
  style = list("font-weight" = "normal", padding = "3px 8px"),
  textsize = "12px",
  direction = "auto")

# Base map and boundaries -----------------------------
lonBounds <- c(-71.7672773, -70.5073883)
latBounds <- c(42.7396090, 42.1671195)

watershed <- leaflet() %>%
  addProviderTiles(providers$Stamen.TonerLite,
                   options = providerTileOptions(minZoom = 11)) %>%
  setView(-71.134767, 42.456871, zoom = 11) %>%
  setMaxBounds(min(lonBounds), max(latBounds), max(lonBounds), min(latBounds)) %>%
  addLegend(pal = palAC,
            values = 1:5,
            opacity = 0.7,
            title = "Adaptive Capacity",
            position = "bottomright")

## SHINY ----------------------------------------------------------

# UI -------------------------------------------------------------
ui <- fluidPage(
  useShinyjs(),
  
  # define <head>
  tags$head(
    
    # load js
    tags$script(type="text/javascript", src = "js/index.js"),
    
    # styling
    tags$style(
      HTML(".shiny-notification {
             position:fixed;
             top: calc(45%);
             left: calc(40%);
             width: calc(20%);
             }
             "
      ),
      type = "text/css",
      "a{cursor:pointer;}"
    )
  ),
  
  theme = shinytheme("cosmo"),
  
  # Tabs ---------------------------------------------------------
  navbarPage("Flood Risks in the Upper Mystic River Watershed",
             collapsible = TRUE,
             
             # Main tab: Map -------------------------------------
             tabPanel("Map",
                      value = "map",
                      div(class = "outer",
                          tags$head(
                            includeCSS("styles.css"))),
                      leafletOutput("map",
                                    width = "100%",
                                    height = 1000), 
                      
                      # Left side panel -------------------------
                      absolutePanel(id = "controls",
                                    class = "panel panel-default",
                                    top = 180,
                                    left = 40,
                                    width = 320,
                                    height = "auto",
                                    style = "padding: 10px",
                                    draggable = T,
                                    
                                    # Intro text ----------------
                                    h4("How will flooding affect you?"),
                                    p("Climate change has exacerbated flooding risks in the upper Mystic River watershed in Massachusetts.
                                      This page provides a simple tool for residents around the watershed to understand their relationship to future floods based on flood models and learn about resources to adapt and prepare for flood events."),
                                    br(),
                                    
                                    # Address look up -----------
                                    textInput(inputId = "address",
                                              label = tags$b("Is your home located in a flood zone?"),
                                              placeholder = "Enter address"),
                                    actionButton(inputId = "search",
                                                 label = "Search",
                                                 class = "btn-primary"),
                                    actionButton(inputId = "clearPin",
                                                 label = "Clear Pin"),
                                    p("-or-"),
                                    
                                    # Census tract look up ------
                                    fluidRow(column(width = 6,
                                                    selectInput(inputId = "muni",
                                                         label = "",
                                                         choices = c("Select municipality" = "",
                                                                     "Arlington" = "1",
                                                                     "Belmont" = "2",
                                                                     "Burlington" = "3",
                                                                     "Cambridge" = "4",
                                                                     "Everett" = "5",
                                                                     "Lexington" = "6",
                                                                     "Malden" = "7",
                                                                     "Medford" = "8",
                                                                     "Melrose" = "9",
                                                                     "Reading" = "10",
                                                                     "Somerville" = "11",
                                                                     "Stoneham" = "12",
                                                                     "Wakefield" = "13",
                                                                     "Watertown" = "14",
                                                                     "Wilmington" = "15",
                                                                     "Winchester" = "16",
                                                                     "Woburn" = "17"))),
                                             column(width = 6,
                                                    selectInput(inputId = "tract",
                                                                label = "",
                                                                choices = c("Select census tract" = "")))),
                                    hr(),
                                    
                                    # Census tract plot ------
                                    textOutput(outputId = "errorMsg"),
                                    textOutput(outputId = "tractName"),
                                    plotOutput(outputId = "tractPlot")
                                    ),
                      
                      # Right side panel ---------------------
                      absolutePanel(id = "controls",
                                    class = "panel panel-default",
                                    top = 100,
                                    right = 40,
                                    width = 270,
                                    height = "auto",
                                    style = "padding: 10px",
                                    draggable = T,
                                    
                                    # Toggle flood layers -----
                                    tags$b("Future flood scenarios"),
                                    tags$a("What is this?", onclick = "customHref('data'); customHref('dataFlood')"),
                                    checkboxGroupInput(inputId = "flood",
                                                       label = NULL,
                                                       choices = c("Extreme precipitation" = "EP",
                                                                   "Sea level rise/storm surge" = "SLRSS"),
                                                       selected = c("EP", "SLRSS")),
                                    hr(),
                                    
                                    # Toggle AC layers --------
                                    tags$b("Adaptive capacity (AC)"),
                                    tags$a("What is this?", onclick = "customHref('data'); customHref('dataAC')"),
                                    radioButtons(inputId = "ACLayers",
                                                 label = "",
                                                 choices = c("None" = "0",
                                                             "% without Bachelor's degree" = "2",
                                                             "% with disabilities" = "1",
                                                             "% elderly living alone" = "3",
                                                             "Median household income" = "4",
                                                             "% limited English households" = "5",
                                                             "% without cars" = "6",
                                                             "% renters" = "8",
                                                             "% rent burdened" = "13",
                                                             "% single parents" = "9",
                                                             "% without transit access" = "10",
                                                             "% unemployed" = "11",
                                                             "% non-Hispanic white" = "7",
                                                             "Total" = "12"),
                                                 selected = "12"
                                                 ),
                                    hr(),
                                    
                                    # Toggle community resources layers -------------
                                    tags$b("Community resources"),
                                    tags$a("What is this?", onclick = "customHref('data'); customHref('dataResources')"),
                                    checkboxGroupInput(inputId = "resources",
                                                       label = NULL,
                                                       c("Places of worship" = "churches",
                                                         "Hospitals" = "hospitals",
                                                         "Public schools" = "schools",
                                                         "Municipal governments" = "governments"))
                      )
                      ),
             
             # Data tab: methodology details ------------------
             tabPanel("Data",
                      value = "data",
                      
                      h3("Where did our data come from and what did we do with them?"),
                      
                      tabsetPanel(
                        tabPanel("Future flood scenarios",
                                 value = "dataFlood",
                                 p('The flood scenarios were put together by Juan Reynoso and Safeer Shersad, as part of their studio course "The Dam(n) Studio: Climate Change along the Mystic" in spring 2020 at the Harvard Graduate School of Design.'),
                                 p('Two scenarios were used for this map: extreme precipitation and sea level rise/storm surge.
                                 The extreme precipitation scenario was a combination of 3 flood models: FEMA 100-year flood zones, 2070 10-year storm modeled by Kleinfelder, Inc., and 2070 100-year storm modeled by Stantec Inc. ',
                                 strong('It is important to note that a "100-year storm" does not refer to a storm that happens every 100 years.
                                 Instead, it means a storm that has a 1% chance of happening in any given year, so it is possible for more than one 100-year storms to happen within 100 years.'),
                                 'Similarly, a 10-year storm is one that has a 10% chance of happening every year.'),
                                 p('The sea level rise/storm surge scenario combined the Boston Harbor Flood Risk Model (BH-FRM) for 2070 storms with a >1% probability, FEMA 100-year flood zones, and FEMA 500-year flood zones.
                                 The risk of sea level rise/storm surge in this watershed mainly came from the potential for stormwater to overtop or flank the Amelia Earhart Dam.
                                 Overall, it was assumed that sea level rise/storm surge would be less likely to occur than extreme precipitation.
                                   Extreme precipitation was also projected to result in a smaller depth of flooding, although the extent of flooding might be greater.'),
                                 p('Due to technical limits, some large flood models had been simplified.
                                   This meant that the boundaries of the flood zones might not be 100% aligned with those in the original models, though the difference would never exceed 10 meters (around 33 feet).'),
                                 p("While flood models represent climate scientists' best efforts to predict future flooding, no flood model is completely accurate.
                                   Therefore, it is important not to view flood zones literally and to ",
                                   strong("focus instead on the larger picture, such as the scope of flooding on the neighborhoods and regional level"), ".")),
                        
                        tabPanel("Adaptive capacity (AC)",
                                 value = "dataAC",
                                 p("Adaptive capacity measured the social vulnerability of people in a census tract during flood events.
                                 The assumption was that populations who were marginalized in the society would usually have less access to the resources (e.g. money, social connections, political power) to deal with disasters like floods.
                                 Adaptive capacity scores were based on demographic data from the 2010 Census and the 2014-18 American Community Survey (ACS) 5-Year Estimates, for demographic indicators not covered by the Census.
                                   The scoring was carried out by Juan Reynoso, Safeer Shersad, and Rui Su, as part of the same studio course mentioned above."),
                                 p('We selected the demographic indicators by reviewing existing social vulnerability studies and compiling the 11 most frequently used demographics.
                                 These were: educational attainment, disabilities, elderly living alone, income, limited English proficiency, car ownership, renter-occupied housing, single parent, proximity to public transit, unemployment, and race & ethnicity.
                                 For each demographic indicator, we obtained the data for all census tracts overlapping the upper Mystic River watershed and calculated the watershed average.
                                 We wanted to check if this watershed stood out in any of the demographics, so we compared the watershed average to the state average.
                                 All except "renter-occupied housing" fell within 1 standard deviation of the state average--relative to the state of Massachusetts as a whole, this watershed had a much higher percentage of renters.
                                   As a result, we added a final indicator: rent burden (defined by percent of households paying more than 30% of their income toward rent), to ensure that renters were sufficiently considered in our assessment.'),
                                 p('After obtaining census tract-level data for all 12 indicators, we divided the census tracts into 5 bins using the Jenks natural breaks method and assigned a score (1-5) to each bin.',
                                 strong('A lower score meant that the census tract had a higher percentage of people belonging to that demographic, and therefore the census tract was assumed to have a lower adaptive capacity for flooding.
                                   Conversely, a score of 4 or 5 meant that people in that census tract would probably be better able to cope with future floods.')),
                                 p('Lastly, we averaged the AC scores for the 12 indicators for each census tract, and again divided them into 5 bins.
                                   This final result represented the "Total" AC score.'),
                                 p("We understood that despite the efforts we put into the analysis, demographics could not tell the full story of people and neighborhoods. ",
                                 strong("Have suggestions or criticisms of this score? Let me know on the ", tags$a("Feedback", onclick = "customHref('feedback')"), "page."))),
                        
                        tabPanel("Community resources",
                                 value = "dataResources",
                                 p("In the face of climate disasters, a cohesive community is a strong community."),
                                 p("The mapping of community resources--neighborhood places that already provide critical services and could play an important role in preparing for floods--was carried out by Rui Su using Google Maps.
                                   One inspiration for this work was ",
                                   tags$a("Prof. Daniel P. Aldrich's research", href = "http://daldrich.weebly.com/disasters-and-resilience.html"),
                                   " on the positive relationship between social capital and disaster response.
                                   Therefore, I highlighted valuable community assets, including places of worship, hospitals, public schools, and municipal governments, to encourage creative thinking on strategies for community resilience.
                                   These places are significant not only because of their physical presence in our neighborhoods, but more so because they are an indispensable part of our social fabric.
                                   We may rely on schools for free lunches, or meet our best friends in the church down the street, not to mention the various city services we all depend on.
                                   These places can be fortified to stand strong during disasters and continue to serve the communities.
                                   And people have already proposed many cool ideas, like ",
                                   tags$a("resilience hubs", href = "https://www.usdn.org/resilience-hubs.html"),
                                   " and community-based organizing.
                                   But ultimately, it is up to community residents to decide how to best utilize their neighborhood assets to prepare for floods."),
                                 p("Of course, the places on the map are not the only ones holding communities together.",
                                   strong("If you think of other important resources in your community that should be on the map, let me know on the ", tags$a("Feedback", onclick = "customHref('feedback')"), "page.")))
                      )
                      ),
                      
                      
             # Tabs to be populated in the future ------------
             #tabPanel("The watershed"),
             
             #tabPanel("Climate change"),
             
             #tabPanel("Deal with floods"),
             
             # Feedback tab ----------------------------------
             tabPanel("Feedback",
                      value = "feedback",
                      
                      fluidRow(width = 8,
                               column(width = 8, offset = 2,
                                      p(strong("This map tool is a constant work in progress."), align = "center"),
                                      p(strong("Strategies to deal with flood risks would not be successful without everyone's involvement."), align = "center"),
                                      br(),
                                      p("Have an idea for a new map feature that would help residents learn about flood risks?", align = "center"),
                                      p("Spotted a mistake on this page?", align = "center"),
                                      p("Want to collaborate and cross-pollinate?", align = "center"),
                                      p("Anything else I have not thought of?", align = "center"),
                                      p("Feel free use the form below to send me your feedback, ideas, or thoughts.
                                        Look forward to your message!", align = "center"),
                                      br())),
                      
                      # Feedback form -----------------------
                      fluidRow(width = 8,
                               column(width = 8, align = "center", offset = 2,
                                      textInput(inputId = "name",
                                                label = "Your name"))),
                      fluidRow(width = 8,
                               column(width = 8, align = "center", offset = 2,
                                      textInput(inputId = "email",
                                                label = "Your email"))),
                      fluidRow(width = 8,
                               column(width = 8, align = "center", offset = 2,
                                      textAreaInput(inputId = "message",
                                                    label = "Your message",
                                                    width = "100%",
                                                    height = 250))),
                      fluidRow(width = 8,
                               column(width = 8, align = "center", offset = 2,
                                      actionButton(inputId = "submit", label = "Submit"))),
                      
                      # Message to notify user of form submission -----
                      fluidRow(width = 8,
                               column(width = 8, align = "center", offset = 2,
                                      textOutput(outputId = "submitted")))
                      )
             )
)

# Server ---------------------------------------------------
server <- function(input, output, session) {
  
  # Render base map ------------------------
  output$map <- renderLeaflet(watershed)
  
  showNotification('It may take a few moments for the map data to appear. Please be patient :)',
                   duration = 10,
                   type = "message")

  # Match text input to standard address ----
  latlon <- reactive({
    geocode(input$address,
            output = "latlona", # this returns a list of 3: latitude, longitude, and address
            source = "google")
  })
  
  # Map address ----------------------------
  observeEvent(input$search, {
    if (is.na(latlon()$lat) | is.na(latlon()$lon)) {
      leafletProxy("map", data = latlon()) %>%
        removeMarker(layerId = "pin")
      
      showNotification('This address is either outside the range of this map or not precise enough. Please make sure to use an address around the Upper Mystic River watershed or specify the city/town (e.g. "10 Concord Ave, Cambridge" instead of "10 Concord Ave").',
                       duration = 7,
                       type = "error")
      
      output$errorMsg <- renderText('This page does not have any info on the census tract where this address is located, either because this census tract does not overlap with the upper Mystic River watershed, or because the address entered is not precise enough (e.g. enter "10 common st, woburn" instead of "10 common st").')
      output$tractName <- renderText("")
      output$tractPlot <- renderPlot("")
      
    } else if (between(latlon()$lon, min(lonBounds), max(lonBounds)) & 
               between(latlon()$lat, min(latBounds), max(latBounds))) {
      leafletProxy("map", data = latlon()) %>%
        setView(lng = latlon()$lon,
                lat = latlon()$lat,
                zoom = 14) %>%
        addMarkers(icon = makeIcon(iconUrl = "pin.png",
                                   iconWidth = 40,
                                   iconHeight = 40,
                                   iconAnchorX = 20,
                                   iconAnchorY = 40),
                   layerId = "pin")
      
      # Match address to census tract ------
      adSplit <- strsplit(latlon()[[3]], ", ")[[1]]
      
      adTract <- tibble(
        street = adSplit[1],
        city = adSplit[2],
        state = str_sub(adSplit[3], 1, 2)) %>%
        append_geoid("tr")
      tract <- str_sub(adTract$geoid, -6, -1) # Extract last 6 digits of GEOID for a more readable census tract name
      
      if (tract %in% totalAC$TRACTCE10) {  # Check the corresponding census tract is within the watershed
        output$errorMsg <- renderText("")
        
        if (str_sub(tract, -2, -1) == "00") {
          tractCode <- str_sub(tract, 1, 4)
        } else {
          tractCode <- str_c(str_sub(tract, 1, 4), ".", str_sub(tract, 5, 6))
        }
        
        # Census tract plot -----------------
        output$tractName <- renderText(c("Your census tract (", tractCode, ") has these levels of adaptive capacity for the following demographic indicators:"))
        output$tractPlot <- renderPlot({
          tractAC <- totalAC %>%
            filter(TRACTCE10 == tract) %>%
            select(8:18, 20) %>%
            pivot_longer(cols = 1:12, names_to = "Tag", values_to = "Score")
          tractAC$Name <- c("% with disabilities",
                            "% without Bachelor's degree",
                            "% elderly living alone",
                            "Median household income",
                            "% limited English households",
                            "% without cars",
                            "% non-Hispanic white",
                            "% renters",
                            "% single parents",
                            "% without transit access",
                            "% unemployed",
                            "% rent burdened")
          
          ggplot(tractAC) +
            geom_col(mapping = aes(x= reorder(Name, -Score),
                                   y = Score,
                                   fill = as.factor(Score))) +
            coord_flip() +
            scale_fill_brewer(palette = "Reds",
                              limits = 5:1) +
            labs(x = NULL, y = "Adaptive capacity score") +
            ylim(0, 5) +
            theme_tufte(base_family = "sans") +
            theme(legend.position = "none")
        }
        )
      } else {  # Return error message the corresponding census tract is not in the watershed
        output$errorMsg <- renderText('This page does not have any info on the census tract where this address is located, either because this census tract does not overlap with the upper Mystic River watershed, or because the address entered is not precise enough (e.g. enter "10 common st, woburn" instead of "10 common st").')
        output$tractName <- renderText("")
        output$tractPlot <- renderPlot("")
      }
    } else {  # Show error notification if the address entered is outside of map bounds
      leafletProxy("map", data = latlon()) %>%
        removeMarker(layerId = "pin")
      
      showNotification('This address is either outside the range of this map or not precise enough. Please make sure to use an address around the Upper Mystic River watershed or specify the city/town (e.g. "10 Concord Ave, Cambridge" instead of "10 Concord Ave").',
                       duration = 7,
                       type = "error")
      
      output$errorMsg <- renderText('This page does not have any info on the census tract where this address is located, either because this census tract does not overlap with the upper Mystic River watershed, or because the address entered is not precise enough (e.g. enter "10 common st, woburn" instead of "10 common st").')
      output$tractName <- renderText("")
      output$tractPlot <- renderPlot("")
    }
  })
  
  observeEvent(input$clearPin, {
    leafletProxy("map", data = input$clearPin) %>%
      removeMarker(layerId = "pin")
  })
  
  # Look up census tract directly ----------------
  observeEvent(input$muni, {  # update the list of census tracts to choose from based on municipality selection
    updateSelectInput(session, "tract",
                      choices = muniTracts[[as.numeric(input$muni)]])
  })
  
  observeEvent(input$tract, {
    if (input$tract != "") {
      output$errorMsg <- renderText("") # clear error message
      
      # Census tract plot ------------------------
      output$tractName <- renderText(c("Your census tract (", input$tract, ") has these levels of adaptive capacity for the following demographic indicators:"))
      output$tractPlot <- renderPlot({
        tractAC <- totalAC %>%
          filter(NAME10 == input$tract) %>%
          select(8:18, 20) %>%
          pivot_longer(cols = 1:12, names_to = "Tag", values_to = "Score")
        tractAC$Name <- c("% with disabilities",
                          "% without Bachelor's degree",
                          "% elderly living alone",
                          "Median household income",
                          "% limited English households",
                          "% without cars",
                          "% non-Hispanic white",
                          "% renters",
                          "% single parents",
                          "% without transit access",
                          "% unemployed",
                          "% rent burdened")
        
        ggplot(tractAC) +
          geom_col(mapping = aes(x= reorder(Name, -Score),
                                 y = Score,
                                 fill = as.factor(Score))) +
          coord_flip() +
          scale_fill_brewer(palette = "Reds",
                            limits = 5:1) +
          labs(x = NULL, y = "Adaptive capacity score") +
          ylim(0, 5) +
          theme_tufte(base_family = "sans") +
          theme(legend.position = "none")
      })
    }
  })
  
  # Toggle flood layers --------------------
  observe({
    if ("EP" %in% input$flood == F) {
      leafletProxy("map", data = input$flood) %>%
        clearGroup(group = "EP")
    } else {
      for (i in 1:5) {
        leafletProxy("map", data = scenario2[[i]]) %>%
          addPolygons(fillColor = pal2,
                      weight = 1,
                      color = NA,
                      fillOpacity = 0.6,
                      group = "EP")
      }
    }
  })
  
  observe({
    if ("SLRSS" %in% input$flood == F) {
      leafletProxy("map", data = input$flood) %>%
        clearGroup(group = "SLRSS")
    } else {
      for (i in 1:3) {
        leafletProxy("map", data = scenario3[[i]]) %>%
          addPolygons(fillColor = pal3,
                      weight = 1,
                      color = NA,
                      fillOpacity = 0.6,
                      group = "SLRSS")
      }
    }
  })
  
  # Toggle AC layers --------------------
  ACLayer <- reactive({ as.numeric(input$ACLayers) })
  
  observe({
    leafletProxy("map", data = totalAC) %>%
      clearGroup(group = "AC")
    if (ACLayer() != 0) {
      leafletProxy("map", data = totalAC) %>%
        addPolygons(data = totalAC,
                    fillColor = ~palAC(totalAC[[ACLayer()+7]]),
                    weight = 1,
                    color = "white",
                    fillOpacity = 0.8,
                    highlight = highlightSet,
                    label = sprintf(
                      "<b>%s</b><br/>%s<br/>Adaptive Capacity: <b>%i</b>",
                      totalAC$Muni, totalAC$NAMELSAD10, totalAC[[ACLayer()+7]]) %>%
                      lapply(HTML),
                    popup = sprintf(
                      "<b>%s</b><br/>%s<br/>Adaptive Capacity: <b>%i</b>",
                      totalAC$Muni, totalAC$NAMELSAD10, totalAC[[ACLayer()+7]]) %>%
                      lapply(HTML),
                    labelOptions = labelSet,
                    group = "AC")
    }
  })
  
  # Toggle community resources layers -------------------
  resourcesOpacity <- 0.8
  
  observe({
    if ("churches" %in% input$resources == F) {
      leafletProxy("map", data = input$resources) %>%
        clearGroup(group = "churches")
    } else {
      for (i in 1:17) {
        leafletProxy("map", data = resources[[1]][[i]]) %>%
          addAwesomeMarkers(icon = awesomeIcons(icon = 'ios-heart',
                                                iconColor = 'white',
                                                library = 'ion',
                                                markerColor = "darkred"),
                            options = markerOptions(opacity = resourcesOpacity),
                            label = resources[[1]][[i]]$Name,
                            group = "churches",
                            clusterOptions = markerClusterOptions(showCoverageOnHover = F))
      }
    }
  })
  
  observe({
    if ("hospitals" %in% input$resources == F) {
      leafletProxy("map", data = input$resources) %>%
        clearGroup(group = "hospitals")
    } else {
      for (i in 1:17) {
        leafletProxy("map", data = resources[[2]][[i]]) %>%
          addAwesomeMarkers(icon = awesomeIcons(icon = "ios-pulse",
                                         iconColor = 'white',
                                         library = 'ion',
                                         markerColor = "beige"),
                            options = markerOptions(opacity = resourcesOpacity),
                            label = resources[[2]][[i]]$Name,
                            group = "hospitals",
                            clusterOptions = markerClusterOptions(showCoverageOnHover = F))
      }
    }
  })
  
  observe({
    if ("schools" %in% input$resources == F) {
      leafletProxy("map", data = input$resources) %>%
        clearGroup(group = "schools")
    } else {
      for (i in 1:17) {
        leafletProxy("map", data = resources[[3]][[i]]) %>%
          addAwesomeMarkers(icon = awesomeIcons(icon = 'ios-book',
                                         iconColor = 'white',
                                         library = 'ion',
                                         markerColor = "darkgreen"),
                            options = markerOptions(opacity = resourcesOpacity),
                            label = resources[[3]][[i]]$Name,
                            group = "schools",
                            clusterOptions = markerClusterOptions(showCoverageOnHover = F))
      }
    }
  })
  
  observe({
    if ("governments" %in% input$resources == F) {
      leafletProxy("map", data = input$resources) %>%
        clearGroup(group = "governments")
    } else {
      leafletProxy("map", data = resources[[4]]) %>%
        addAwesomeMarkers(icon = awesomeIcons(icon = 'ios-people',
                                       iconColor = 'white',
                                       library = 'ion',
                                       markerColor = "lightgray"),
                          options = markerOptions(opacity = resourcesOpacity),
                          label = resources[[4]]$Name,
                          group = "governments",
                          clusterOptions = markerClusterOptions(showCoverageOnHover = F))
      }
  })
  
  # Email feedback -----------------------
  observe({
    toggleState(id = "submit",
                condition = input$message != "") # Only enable submit button if user enters a message
  })
  
  observeEvent(input$submit, {
    if (input$message != "") {
      send.mail(from = "mysticshiny@hotmail.com",
                to = "rosemary.g.s@hotmail.com",
                subject = paste("Shiny feedback from", input$name),
                body = paste(paste("Sender's email:", input$email), input$message, sep = "\n"),
                smtp = list(host.name = "smtp-mail.outlook.com",
                            port = 587,
                            user.name = "mysticshiny@hotmail.com",
                            passwd = password,
                            tls = T),
                authenticate = T,
                debug = T)
      
      # Clear form and display confirmation message once email is sent
      updateTextInput(session, "name",
                      value = "")
      updateTextInput(session, "email",
                      value = "")
      updateTextAreaInput(session, "message",
                          value = "")
      output$submitted <- renderText("Your feedback has been submitted. Thank you!")
    }
  })
}

# Run app ---------------------------------------
shinyApp(ui, server)
