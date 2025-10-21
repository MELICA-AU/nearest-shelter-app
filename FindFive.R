# ===============================================================
#  Shiny App: FiveShelters — Find Nearest Shelters with Mapbox
# ===============================================================

library(shiny)
library(leaflet)
library(mapboxapi)
library(sf)
library(tidyverse)
library(RColorBrewer)
library(stringr)

# --- 1. Load Mapbox token from environment -----------------------
token <- Sys.getenv("MAPBOX_TOKEN")

if (token == "") {
  stop("❌ Mapbox token not found. Please set MAPBOX_TOKEN in your .Renviron file.")
} else {
  message("✅ Mapbox token loaded from environment.")
}

# --- 2. Load shelter data ---------------------------------------
shelter <- readRDS("data/BDG_wide2024.rds")
shelter <- st_transform(shelter, 4326)

# --- 3. UI ------------------------------------------------------
ui <- fluidPage(
  sidebarPanel(
    textInput("address_text", label = "Address",
              placeholder = "Type an address or place name"),
    selectInput("transport", label = "On foot or bike?",
                choices = c("walking", "cycling")),
    selectInput("time", label = "Within 5 or 10 mins?",
                choices = c(5, 10)),
    actionButton("action", "Find the nearest shelters within 5–10 min distance"),
    p(),
    p("Instructions to the nearest shelter:"),
    em("Beware: due to urban development, shelters may no longer exist or be barred."),
    p("Routes to five nearest shelters are color-coded by distance (red = closest, green = farthest)."),
    htmlOutput("instructions"),
    width = 3
  ),
  mainPanel(
    leafletOutput(outputId = "map", width = "100%", height = 1000)
  )
)

# --- 4. Server --------------------------------------------------
server <- function(input, output, session) {
  
  input_sf   <- reactiveVal(NULL)
  routes_all <- reactiveVal(list())   # store all computed routes
  
  # --- Base map
  output$map <- renderLeaflet({
    leaflet() %>%
      addMapboxTiles(
        style_id = "satellite-streets-v11",
        username = "mapbox",
        access_token = token
      ) %>%
      addCircleMarkers(data = shelter, opacity = 0.4, radius = 2)
  })
  
  
  # --- 5. Find closest shelters ---------------------------------
  closest_locations <- eventReactive(input$action, {
    output$instructions <- renderUI({ HTML("") })  # clear old
    req(input$address_text)
    
    # geocode input
    input_sf_value <- mb_geocode(
      input$address_text,
      output = "sf",
      access_token = token
    )
    st_crs(input_sf_value) <- 4326
    input_sf(input_sf_value)
    
    # compute isochrone
    within_time <- mb_isochrone(
      location = input_sf_value,
      profile  = input$transport,
      time     = input$time,
      access_token = token
    )
    
    # select shelters inside polygon
    hit <- st_filter(shelter, within_time, .predicate = st_within)
    
    if (nrow(hit) == 0) return(NULL)
    
    # compute travel times
    hit$time <- mb_matrix(
      origins = input_sf_value,
      destinations = hit,
      profile = input$transport,
      access_token = token
    ) %>% as.vector()
    
    hit$index_order <- order(hit$time)
    hit <- hit %>% arrange(time) %>% slice(1:5)
    
    return(hit)
  })
  
  
  # --- 6. Draw results ------------------------------------------
  observeEvent(closest_locations(), {
    locs <- closest_locations()
    
    # case: no shelters found
    if (is.null(locs) || nrow(locs) == 0) {
      leafletProxy("map") %>%
        clearShapes() %>%
        clearMarkers() %>%
        flyToBounds(
          lng1 = st_bbox(shelter)[["xmin"]],
          lat1 = st_bbox(shelter)[["ymin"]],
          lng2 = st_bbox(shelter)[["xmax"]],
          lat2 = st_bbox(shelter)[["ymax"]]
        )
      output$instructions <- renderUI({
        HTML("<b style='color:red;'>No shelters available within your specified limits. Try again.</b>")
      })
      routes_all(list())
      return(NULL)
    }
    
    # clear map before drawing new results
    leafletProxy("map") %>%
      clearShapes() %>%
      clearMarkers() %>%
      clearControls()
    
    # compute routes
    routelist <- map(seq_len(nrow(locs)), function(i) {
      mb_directions(
        origin = st_coordinates(input_sf()),
        destination = st_coordinates(locs[i,]),
        profile = input$transport,
        output = "sf",
        steps = TRUE,
        access_token = token
      )
    })
    routes_all(routelist)  # store routes reactively
    
    # --- color setup --------------------------------------------
    num_routes <- length(routelist)
    color_palette <- brewer.pal(min(num_routes, 9), "RdYlGn")
    color_palette <- rev(color_palette) # red = closest
    pal <- colorNumeric(color_palette, domain = locs$time, reverse = FALSE)
    
    # --- origin marker ------------------------------------------
    origin_coords <- st_coordinates(input_sf())
    leafletProxy("map") %>%
      addRectangles(
        lng1 = origin_coords[1] - 0.0002,
        lat1 = origin_coords[2] - 0.0001,
        lng2 = origin_coords[1] + 0.0002,
        lat2 = origin_coords[2] + 0.0001,
        color = "cyan", fillColor = "cyan", fillOpacity = 0.8
      )
    
    # --- draw routes and markers --------------------------------
    for (i in seq_along(routelist)) {
      route <- routelist[[i]]
      leafletProxy("map") %>%
        addPolylines(
          data = route,
          color = color_palette[i],
          weight = 3,
          opacity = 0.8,
          group = "routes"
        )
    }
    
    leafletProxy("map") %>%
      addCircleMarkers(
        data = locs,
        layerId = paste0("shelter_", seq_len(nrow(locs))),
        radius = ~Final_Pub_Size / 10,
        stroke = TRUE,
        color = "black",
        weight = 2,
        fillColor = pal(locs$time),
        fillOpacity = 0.9,
        popup = ~paste0(
          "<b>Shelter:</b> ", BDnr_1987, "<br>",
          "<b>Capacity:</b> ", Final_Pub_Size, "<br>",
          "<b>Est. ", input$transport, " time:</b> ",
          round(time, 1), " min<br>",
          "<i>Click marker for route</i>"
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = locs$time,
        title = "Travel time (min)",
        labFormat = labelFormat(suffix = " min"),
        opacity = 1
      )
    
    # fly to center of results
    center <- st_centroid(st_union(locs)) |> st_coordinates()
    leafletProxy("map") %>% flyTo(lng = center[1], lat = center[2], zoom = 13)
  })
  
  
  # --- 7. On marker click: highlight route + show instructions ---
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    if (is.null(click$id)) return()
    
    idx <- as.numeric(gsub("shelter_", "", click$id))
    all_routes <- routes_all()
    if (is.na(idx) || idx > length(all_routes)) return()
    
    route <- all_routes[[idx]]
    if (is.null(route) || nrow(route) == 0) return()
    
    # highlight route
    leafletProxy("map") %>%
      clearGroup("highlight") %>%
      addPolylines(
        data = route,
        color = "deepskyblue",
        weight = 5,
        opacity = 1,
        group = "highlight"
      ) %>%
      flyTo(lng = click$lng, lat = click$lat, zoom = 15)
    
    # extract route instructions
    steps <- NULL
    if ("instruction" %in% names(route)) {
      steps <- route$instruction
    } else if ("steps" %in% names(route) && is.data.frame(route$steps[[1]])) {
      steps <- route$steps[[1]]$instruction
    }
    
    if (is.null(steps)) {
      output$instructions <- renderUI({
        HTML("<i>No turn-by-turn instructions available for this route.</i>")
      })
    } else {
      # make each instruction clickable in browser (Google Maps search link)
      clickable_steps <- paste0(
        "• <a href='https://www.google.com/maps/search/?api=1&query=",
        gsub(" ", "+", steps),
        "' target='_blank'>", steps, "</a>"
      )
      output$instructions <- renderUI({
        HTML(paste0(
          "<b>Route Instructions:</b><br>",
          paste(clickable_steps, collapse = "<br>")
        ))
      })
    }
  })
}

# --- 8. Run the app ---------------------------------------------
shinyApp(ui = ui, server = server)
