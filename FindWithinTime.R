# ===============================================================
#  Shiny App: Within Time — Find Nearest Shelters within Specified Time
# ===============================================================


library(shiny)
library(leaflet)
library(mapboxapi)
library(dplyr)
library(sf)
library(readr)

# --- 1. Load Mapbox token in  environment -----------------------
# token <- Sys.getenv("MAPBOX_TOKEN")
# 
# if (token == "") {
#   stop("❌ Mapbox token not found. Please set MAPBOX_TOKEN in your .Renviron file.")
# } else {
#   message("✅ Mapbox token loaded from environment.")
# }
# 

token <- read_lines("token.txt")

if (token == "") {
  stop("Mapbox token not found. Please set MAPBOX_TOKEN in your .Renviron file.")
} else print("token is copacetic")

mapboxapi::mb_access_token(token, install = TRUE, overwrite = TRUE)

# Read in the shelter data
shelter <- readRDS("data/BDG_wide2024.rds") 
# must be in WGSS84
shelter <- st_transform(shelter,4326)

# Set up a sidebar panel with a text box for an input address, 
# and a placeholder to print out the driving instructions
ui <- fluidPage(
  sidebarPanel(
    textInput("address_text", label = "Address",
              placeholder = "Type an address or place name"),
    selectInput("transport", label = "On foot or bike?", choices = c("walking", "cycling")),
    selectInput("time", label = "Within 5 or 10 mins?", choices = c(5, 10)),
    actionButton("action", "Find the nearest shelters within 5-10 min distance"),
    p(),
    p("Display route by clicking on a shelter marker:"),
    em("Beware: due to urban development shelters may no longer exist or be barred"),
    p("Shelter proximity is indicated by color (green = close, red = distant). Travel times may exceed set limit. Check travel time and capacity by clicking on shelter pointers."),
    #textInput("instructions_text", label = "Instructions to the nearest shelter /n (beware:location error ~100m)"),
    htmlOutput("instructions"),
    width = 3
  ),
  mainPanel(
    leafletOutput(outputId = "map", width = "100%", height = 1000)
  )
)

server <- function(input, output, session) {
  library(purrr)
  library(stringr)
  
 
  # reactive storage
  input_sf  <- reactiveVal(NULL)
  routes_all <- reactiveVal(list())
  
  # --- Initial base map ------------------------------------------------------
  output$map <- renderLeaflet({
    leaflet() %>%
      addMapboxTiles(
        style_id = "light-v9",
        username = "mapbox",
        access_token = token
      ) %>%
      addCircleMarkers(data = shelter, opacity = 0.5, radius = 2)
  })
  
  
  # --- Find nearest shelters within time limit -------------------------------
  closest_locations <- eventReactive(input$action, {
    
    output$instructions <- renderUI({ HTML("") })  # clear previous text
    
    req(input$address_text)
    
    # geocode input
    input_sf_value <- mb_geocode(
      input$address_text,
      output = "sf",
      access_token = token
    )
    st_crs(input_sf_value) <- 4326
    input_sf(input_sf_value)
    
    # get isochrone
    within_time <- mb_isochrone(
      location = input_sf_value,
      profile  = input$transport,
      time     = input$time,
      access_token = token
    )
    
    # subset shelters inside isochrone polygon
    hit <- st_filter(shelter, within_time, .predicate = st_within)
    
    if (nrow(hit) == 0) 
      return(shelter[0, ]) 
    
    # compute travel times to each hit
    hit$time <- mb_matrix(
      origins = input_sf_value,
      destinations = hit,
      profile = input$transport,
      access_token = token
    ) %>% as.vector()
    
    hit <- hit %>% arrange(time)
    return(hit)
    # --- NEW: strict filter by time threshold
    # hit <- hit %>% 
    #   filter(time <= input$time) %>%
    #   arrange(time)
    # return(hit)
    
  })
  
 
  
  
  # --- No results are calculated -------------------------------------
  observeEvent(closest_locations(), {
    locs <- closest_locations()
    
    # case 1: no shelters found or invalid results -------------------------------------
    if (is.null(locs) || nrow(locs) == 0) {
      leafletProxy("map") %>% 
        clearShapes() %>% 
        clearMarkers() %>% 
        flyToBounds( lng1 = st_bbox(shelter)[["xmin"]], 
                     lat1 = st_bbox(shelter)[["ymin"]], 
                     lng2 = st_bbox(shelter)[["xmax"]], 
                     lat2 = st_bbox(shelter)[["ymax"]] ) 
      output$instructions <- renderUI({ HTML("<b style='color:red;'>No shelters available within your selected limit. Try again.</b>") }) 
      routes_all(list()) 
      return(NULL)
    }
    
    # case 2: Valid results -------------------------------------
    
    # clear map before drawing
    leafletProxy("map") %>%
      clearShapes() %>%
      clearMarkers() 
    
    # --- highlight origin -------------------------------------------------------
    origin_coords <- st_coordinates(input_sf())
    
    leafletProxy("map") %>%
      # addMarkers(
      #   lng = origin_coords[1],
      #   lat = origin_coords[2],
      #   icon = makeAwesomeIcon(
      #     icon = "home",
      #     library = "fa",
      #     markerColor = "lightblue"
      #   ),
      #   popup = "Your location"
      # )
    addRectangles(
      lng1 = origin_coords[1] - 0.0002,
      lat1 = origin_coords[2] - 0.0001,
      lng2 = origin_coords[1] + 0.0002,
      lat2 = origin_coords[2] + 0.0001,
      color = "yellow",
      fillColor = "yellow",
      fillOpacity = 0.8
    )
    
    # compute routes to all reachable shelters
    routes <- map(seq_len(nrow(locs)), function(i) {
      mb_directions(
        origin = st_coordinates(input_sf()),
        destination = st_coordinates(locs[i,]),
        profile = input$transport,
        output = "sf",
        steps = TRUE,
        access_token = token
      )
    })
    routes_all(routes)
    
    # draw all routes faintly
    for (i in seq_along(routes)) {
      leafletProxy("map") %>%
        addPolylines(
          data = routes[[i]],
          color = "gray",
          weight = 2,
          opacity = 0.4
        )
    }
    
    
    # --- color markers by travel time -----------------------------------------
    
    pal <- colorNumeric(
      palette = "RdYlGn",    # Red-Yellow-Green (we’ll reverse it below)
      domain  = locs$time,
      reverse = TRUE          # so green = low time, red = high time
    )
    
    # --- add markers with layerId for click detection
    
    leafletProxy("map") %>%
      addCircleMarkers(
        data = locs,
        layerId = paste0("shelter_", seq_len(nrow(locs))),
        radius = ~Final_Pub_Size/10,
        stroke = TRUE,
        color = "aquamarine",
        weight = 3,
        fillColor = pal(locs$time),
        fillOpacity = 0.9,
        popup = ~paste0(
          "<b>Shelter:</b> ", BDnr_1987, "<br>",
          "<b>Capacity:</b> ", Final_Pub_Size, "<br>",
          "Est. ", input$transport, " time: ", round(time, 1), " min<br>",
          "<i>Click marker for route</i>"
        )
      ) %>%
      clearControls() %>% 
      addLegend(
        position = "bottomright",
        pal = pal,
        values = locs$time,
        title = paste("Travel time (min)"),
        labFormat = labelFormat(suffix = " min"),
        opacity = 1
      )
    
    # fly to center of all hits
    center <- st_centroid(st_union(locs)) |> st_coordinates()
    leafletProxy("map") %>% flyTo(lng = center[1], lat = center[2], zoom = 13)
  })
  
  
  # --- When user clicks a shelter marker -------------------------------------
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    if (is.null(click$id)) return()
    
    idx <- as.numeric(str_extract(click$id, "\\d+$"))
    route <- routes_all()[[idx]]
    if (is.null(route)) return()
    
    # highlight this route
    leafletProxy("map") %>%
      clearGroup("highlight") %>%
      addPolylines(
        data = route,
        color = "red",
        weight = 4,
        opacity = 1,
        group = "highlight"
      ) %>%
      flyTo(lng = click$lng, lat = click$lat, zoom = 15)
    
    # step-by-step instructions
    steps <- route$instruction
    if (is.null(steps)) {
      output$instructions <- renderUI({
        HTML("<i>No turn-by-turn instructions available for this route.</i>")
      })
    } else {
      output$instructions <- renderUI({
        HTML(paste0(
          "<b>Route Instructions:</b><br>",
          paste0("• ", steps, collapse = "<br>")
        ))
      })
    }
  })
}

shinyApp(ui = ui, server = server)