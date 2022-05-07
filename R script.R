library(tidyverse)
library(tmap)
library(tmaptools)
library(leaflet)
library(htmltools)
library(shiny)
library(shinyjs)
library(shinycssloaders)
library(highcharter)
library(scales)

emissions <- read.csv('emissions_final.csv') %>% 
  select(-total, -missing)

emissions$sqrt <- sqrt(emissions$ghg)

emissions <- emissions %>% 
  group_by(level) %>% 
  arrange(desc(ghg)) %>% 
  mutate(rank = order(as.numeric(ghg), decreasing = T),
         ghg_pc = as.numeric(ghgpc)) %>% 
  arrange(desc(ghg_pc)) %>% 
  mutate(rank_int = order(as.numeric(ghg_pc), decreasing = T)) 

varnames <- c('coal', 'oil', 'ng', 'otherfos', 'nuclear', 'hydro', 'geothermal', 'pv', 'wind', 'otherren', 'other')
varreplace <- c('Coal', 'Oil', 'Natural gas', 'Other fossil fuels',
                'Nuclear', 'Hydro', 'Geothermal', 'Solar PV', 'Wind',
                'Other renewables', 'Other')

# hc test 2 --------------------------------------------------------------------

selected <- subset(emissions, country == "Germany"|country == emissions$region[emissions$country == "Germany"]|country == 'World') %>% 
  filter(level != 'continent') %>% 
  mutate(pal = as.factor(case_when(
    level == 'country' ~ "#96e97c", 
    level == 'region' ~ "#074d65", 
    level == 'world' ~ "#6ad5eb")),
    arranges = case_when(
      level == 'country' ~ 1,
      level == 'region' ~ 2,
      level == 'world' ~ 3)) %>% 
  arrange(arranges)

selectedtwo <- subset(emissions, country == "United Kingdom"|country == emissions$region[emissions$country == "United Kingdom"]|country == 'World') %>% 
  filter(level != 'continent') %>% 
  mutate(pal = as.factor(case_when(
    level == 'country' ~ "#96e97c", 
    level == 'region' ~ "#074d65", 
    level == 'world' ~ "#6ad5eb")),
    arranges = case_when(
      level == 'country' ~ 1,
      level == 'region' ~ 2,
      level == 'world' ~ 3)) %>% 
  arrange(arranges)

selected <- rbind(selected, selectedtwo)

# Create map ----------------------------------------------------------------

map_country <- filter(emissions, level == 'country') 

# Create palettes ----------------------------------------------------------------

pal_full <- c("#96e97c", "#074d65", "#6ad5eb", "#528e8c", "#cddb9b", "#57449b",
              "#9fa8e1", "#760796", "#ee80fe", "#598322", "#e08761")

pal <- c("#96e97c", "#074d65", "#6ad5eb")

join <- data.frame(mix = varreplace, color = pal_full)

# Shiny code -------------------------------------------------------------------

## UI --------------------------------------------------------------------------

ui<-fluidPage(
  tags$head(HTML("<title>Emissions </title>")),
  useShinyjs(),
  br(),
  span(style = "font-weight: 600; font-size: 25px; width: 100%;
         color: #074d65;", "Global Greenhouse Gas Emissions"),
  br(),br(),
  fluidRow(
    column(8, h6('Click a country, region, or continent to see detailed GHG statistics', align = 'center'),
           leafletOutput("map", height = "400px") %>% withSpinner(color="#0dc5c1")),
    column(4,
           br(),
           br(),
           br(),
           htmlOutput("country") %>% withSpinner(color="#0dc5c1"),
           br(),
           htmlOutput("ghg") %>% withSpinner(color="#0dc5c1"),
           br(),
           htmlOutput("rank") %>% withSpinner(color="#0dc5c1"),
           br(),
           htmlOutput("ghg_pc") %>% withSpinner(color="#0dc5c1"),
           br(),
           htmlOutput("rank_int") %>% withSpinner(color="#0dc5c1"),
           br(),
           htmlOutput("year") %>% withSpinner(color="#0dc5c1")
           
           
    )
  ),
  fluidRow(
    column(6, h4('Electricity generation mix', align = 'left'),
           highchartOutput("electricity", width = "100%", height = "350px")%>% 
             withSpinner(color="#0dc5c1")),
    column(6, h4('Per capita intensity benchmarking', align = 'left'),
           highchartOutput("intensity", width = "100%", height = "350px")%>% 
             withSpinner(color="#0dc5c1"))
  ),
  fluidRow(column(12, align = 'center', uiOutput('notes')))
)

## Server ----------------------------------------------------------------------

server <- shinyServer(function(input, output) {
  data <- reactiveValues(clickedMarker=NULL)
  # produce the basic leaflet map with single marker
  
  output$map <- renderLeaflet({
    
    labels <- sprintf("<strong>%s</strong><br/> Total Emissions:<br/> %s kt CO<sub>2</sub>e",
                      map_country$country, scales::comma(map_country$ghg)) %>% 
      lapply(HTML)
    
    leaflet() %>% 
      addTiles(urlTemplate = "", attribution = '<a href="https://leafletjs.com/">Leaflet</a>') %>% 
      addProviderTiles("Esri.WorldImagery") %>%
      addCircles(data = map_country, lng = ~lon, lat = ~lat, layerId = ~country,
                 radius = ~ghg/6, #CHANGE TO SQRT VALUES
                 weight = 1,
                 #  fill = TRUE,
                 fillOpacity = 0.5,
                 color = "#96e97c",
                 opacity = 0.7,
                 label = labels,
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px"),
                   textsize = "15px",
                   direction = "auto",
                   opacity = 0.75)) %>% 
      setView(lat = 0, lng = 0, zoom = 2)
  })
  
  # observe the marker click info and print to console when it is changed.
  
  
  # text
  output$country <- renderText({
    p <- input$map_shape_click
    
    if (is.null(p)) {
      selected <- subset(emissions, country == 'World')
    } else {
      selected <- subset(emissions, country == p$id)
    }
    
    paste( 
      "<center> <font size = 6> <strong> <span style = \'font-weight: 500;\'> ", selected$country, "</span> </strong> </font>
          <br>"
    )
  })
  
  output$ghg <- renderText({
    p <- input$map_shape_click
    
    if (is.null(p)) {
      selected <- subset(emissions, country == 'World')
    } else {
      selected <- subset(emissions, country == p$id)
    }    
    
    paste("<center> <font size = 3>", "Total emissions: ", "</font>", 
          "<font size = 5> <font color = #074d65> <b>", comma(selected$ghg), "kT CO<sub>2</sub>e", "</b> </font>")
  })
  
  output$rank <- renderText({
    p <- input$map_shape_click
    
    if (is.null(p)) {
      output_text <- paste("<center> <font size = 3> Country intensity rank: </font>",
                           "<font size = 5> <font color = #808080> <b> NA </b> </font>")
    } else {
      selected <- subset(emissions, country == p$id)
      tot <- emissions %>% filter(level == selected$level[selected$country == p$id])
      output_text <- paste("<center> <font size = 3>", str_to_title(selected$level), " rank: ", "</font>", 
          "<font size = 5> <font color = #074d65> <b>", selected$rank, "</b> </font>", 
          "<font size = 3>", " of ", "</font>",
          "<font size = 5> <font color = #074d65> <b>", nrow(tot), "</b> </font>")
    }
    
    output_text
  })
  
  output$ghg_pc <- renderText({
    p <- input$map_shape_click
    if (is.null(p)) {
      selected <- subset(emissions, country == 'World')
    } else {
      selected <- subset(emissions, country == p$id)
    }    
    
    paste("<center> <font size = 3>", "Emissions per capita: ", "</font>",
          "<font size = 5> <font color = #074d65> <b>", selected$ghgpc, " kT CO<sub>2</sub>e", "</b> </font>")
  })
  
  output$rank_int <- renderText({
    p <- input$map_shape_click

    if (is.null(p)) {
      output_text <- paste("<center> <font size = 3> Country intensity rank: </font>",
                           "<font size = 5> <font color = #808080> <b> NA </b> </font>")
    } else {
      selected <- subset(emissions, country == p$id)
      tot <- emissions %>% filter(level == selected$level[selected$country == p$id])
      
      output_text <- paste("<center> <font size = 3>", str_to_title(selected$level), " intensity rank: ", "</font>", 
          "<font size = 5> <font color = #074d65> <b>", selected$rank_int, "</b> </font>", 
          "<font size = 3>", " of ", "</font>",
          "<font size = 5> <font color = #074d65> <b>", nrow(tot), "</b> </font>")
    }
    
    output_text
    
  })
  
  output$year <- renderText({
    p <- input$map_shape_click
    
    if (is.null(p)) {
      output_text <- paste("<center> <i> These are global totals. Click a country to see detailed statistics</i>")
    } else {
      selected <- subset(emissions, country == p$id)
      output_text <- paste("<center> <font size = 3>", "Emissions source year: ", "</font>",
            "<font size = 5> <font color = #074d65> <b>", selected$source_year, "</b> </font>")
    }
    
    output_text
    
  })
  
  # electricity
  output$electricity <- renderHighchart({
    p <- input$map_shape_click
    
    if (is.null(p)) {
      selected <- subset(emissions, country == 'World')
    } else {
      selected <- subset(emissions, country == p$id)
    }    
    
    selected %>%
      rename_at(vars(varnames), ~varreplace) %>% 
      pivot_longer(cols = varreplace, names_to = "mix") %>% 
      mutate(mix = case_when(
        value <= 0.01 | mix == 'Other' ~ 'Other',
        TRUE ~ mix)) %>% 
      group_by(mix) %>% summarise(value = sum(value)) %>% 
      left_join(join, by = 'mix') %>% 
      hchart('pie', hcaes(x = mix, y = value, color = color), name = "Grid mix share", showInLegend = F) %>% 
      hc_tooltip(pointFormat = "<b>Electricity grid share:</b> {point.percentage:,.0f}%")
  })
  
  # Intensity
  output$intensity <- renderHighchart({
    p <- input$map_shape_click

    if (is.null(p)) {
      selected <- subset(emissions, country == 'World') %>% 
        mutate(pal = "#074d65")
      
      highchart_output <- selected %>% 
        filter(level != 'continent') %>% 
        mutate(country = str_to_title(country)) %>%
        hchart(pointWidth = 50, 'column', hcaes(x = country, y = as.numeric(ghgpc), color = pal), name = 'GHG per capita') %>% 
        hc_xAxis(title = list(text = "")) %>% 
        hc_yAxis(title = list(text = "GHG per capita (tCO<sub>2</sub>e)"))
      
    } else {
      selected <- subset(emissions, country == p$id|country == emissions$region[emissions$country == p$id]|country == 'World') %>% 
        filter(level != 'continent') %>% 
        mutate(pal = as.factor(case_when(
                 level == 'country' ~ "#96e97c", 
                 level == 'region' ~ "#074d65", 
                 level == 'world' ~ "#6ad5eb")),
               arranges = case_when(
                 level == 'country' ~ 1,
                 level == 'region' ~ 2,
                 level == 'world' ~ 3)) %>% 
        arrange(arranges)
      
      highchart_output <- selected %>% 
        filter(level != 'continent') %>% 
        hchart('column', hcaes(x = country, y = as.numeric(ghgpc), color = pal), name = 'GHG per capita') %>% 
        hc_xAxis(
          title = list(text = ""),
          categories = list(
            str_to_title(selected$country[selected$country == p$id]),
            str_to_title(selected$region[selected$country == p$id]),
            "World")) %>% 
        hc_yAxis(title = list(text = "GHG per capita (tCO<sub>2</sub>e)"))
      
    }
    
    highchart_output

  })
  
  output$notes <- renderUI({
    HTML(str_glue('Created with love by Zachary Gaeddert with the goal of facilitating a better understanding of the climate crisis.<br>
                                         Sources: <a https://www.climatewatchdata.org/ghg-emissions?end_year=2018&start_year=1990>Climate Watch</a> and <a https://www.iea.org/reports/world-energy-balances-overview>The International Energy Agency</a> Code: <a href="https://github.com/charlie86/covid-testing-us">GitHub</a> <br>
                      All data are at the country level. Regional, continental, and global figures are manually aggregated by myself.'))
  })
})




# Final ------------------------------------------------------------------------

shinyApp(ui, server)
