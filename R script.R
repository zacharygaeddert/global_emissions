library(tidyverse)
library(leaflet)
library(htmltools)
library(shiny)
library(shinyjs)
library(shinycssloaders)
library(highcharter)
library(scales)

emissions <- read.csv('emissions_final_2.csv') # %>% 
 # select(-total, -missing)

emissions$sqrt <- emissions$ghg^(1/1.5)

emissions <- emissions %>% 
  group_by(level) %>% 
  arrange(desc(ghg)) %>% 
  mutate(rank = order(as.numeric(ghg), decreasing = T),
         ghg_pc = as.numeric(ghgpc)) %>% 
  arrange(desc(ghg_pc)) %>% 
  mutate(rank_int = order(as.numeric(ghg_pc), decreasing = T)) 

# Create maps ------------------------------------------------------------------

map_country <- filter(emissions, level == 'country') 

# Create palletes and variable name lists --------------------------------------

## Electricity mix 

pal_full <- c("#96e97c", "#074d65", "#6ad5eb", "#528e8c", "#cddb9b", "#57449b",
              "#9fa8e1", "#760796", "#ee80fe", "#598322", "#e08761")

pal <- c("#96e97c", "#074d65", "#6ad5eb")

varnames <- c('coal', 'oil', 'ng', 'otherfos', 'nuclear', 'hydro', 'geothermal', 'pv', 'wind', 'otherren', 'other')
varreplace <- c('Coal', 'Oil', 'Natural gas', 'Other fossil fuels',
                'Nuclear', 'Hydro', 'Geothermal', 'Solar PV', 'Wind',
                'Other renewables', 'Other')

join <- data.frame(mix = varreplace, color = pal_full)

# Sector share

sector_names <- c("Agriculture", "Land use change and forestry", "Waste", "Industry",
                  "Manufacturing and construction", "Transport", "Electricity and heat",
                  "Buildings", "Fugitive emissions", "Other fuel combustion", "Aviation and shipping")

sector_replace_2016 <- colnames(emissions[22:32])
sector_replace_2017 <- colnames(emissions[33:43])
sector_replace_2018 <- colnames(emissions[44:54])

join_sector <- data.frame(sector = sector_names, color = pal_full)

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
    column(7, h6('Zoom in and click a country to see detailed GHG statistics', align = 'center'),
           leafletOutput("map", height = "400px") %>% withSpinner(color="#0dc5c1")),
    column(5,
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
    column(12,div(style = "height:25px;background-color: white;"))),
  
  fluidRow(
    column(3, h4('Per capita intensity benchmarking', align = 'center'),
           highchartOutput("intensity", width = "100%", height = "400px")%>% 
             withSpinner(color="#0dc5c1")),
    column(6, h4('Electricity generation mix', align = 'center'),
           highchartOutput("electricity", width = "100%", height = "400px")%>% 
             withSpinner(color="#0dc5c1")),
    column(3, h4('Emissions by sector*', align = 'center'),
           highchartOutput("sector", width = "100%", height = "400px")%>% 
             withSpinner(color="#0dc5c1"),
          htmlOutput("note"))
  ),
  fluidRow(column(12, align = 'center', uiOutput('notes')))
)

## Server ----------------------------------------------------------------------

server <- shinyServer(function(input, output) {
  data <- reactiveValues(clickedMarker=NULL)
  # produce the basic leaflet map with single marker
  
  output$map <- renderLeaflet({
    
    labels <- sprintf("<strong>%s</strong><br/> Total Emissions:<br/> %s kt CO<sub>2</sub>e",
                      map_country$name, scales::comma(map_country$ghg)) %>% 
      lapply(HTML)
    
    leaflet() %>% 
      addTiles(urlTemplate = "", attribution = '<a href="https://leafletjs.com/">Leaflet</a>') %>% 
      addProviderTiles("Esri.WorldImagery") %>%
      addCircles(data = map_country, lng = ~lon, lat = ~lat, layerId = ~name,
                 radius = ~sqrt*24.5, #makes small countries visible without China being overwhelming
                 weight = 1,
                 #  fill = TRUE,
                 color = "black",
                 fillColor = "#ee80fe",
                 opacity = 1,
                 fillOpacity = 0.5,
                 stroke = TRUE,
                 highlightOptions = highlightOptions(color = '#ffffff',
                                                     weight = 2.5,
                                                     bringToFront = T),
                 label = labels,
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px"),
                   textsize = "15px",
                   direction = "auto",
                   opacity = 0.75)) %>% 
      setView(lat = 0, lng = 0, zoom = 2)
  })

# Change colour of marker when clicked and zoom
  
  observeEvent(input$map_shape_click, {
    p <- input$map_shape_click
    selected <- subset(emissions, name == p$id)
    
    print(p)
    
    proxy <- leafletProxy("map")
    
    if(p$id == "Selected"){
      proxy %>% removeShape(layerId = "Selected")
    } else {
      proxy %>%
        addCircles(data = selected, lng = ~lon, lat = ~lat, layerId = "Selected",
                   radius = ~sqrt*24.5, #makes small countries visible without China being overwhelming
                   weight = 1,
                   #  fill = TRUE,
                   color = "black",
                   fillColor = "#6ad5eb",
                   opacity = 1,
                   fillOpacity = 0.5,
                   stroke = TRUE,
                   highlightOptions = highlightOptions(color = '#ffffff',
                                                       weight = 2.5,
                                                       bringToFront = T)) %>% 
        setView(lat = p$lat, lng = p$lng, zoom = 4)
    }
  })
  
  
  # text
  output$country <- renderText({
    p <- input$map_shape_click
    
    if (is.null(p)) {
      selected <- subset(emissions, name == 'World')
    } else {
      selected <- subset(emissions, name == p$id)
    }
    
    paste( 
      "<center> <font size = 6> <strong> <span style = \'font-weight: 500;\'> ", selected$name, "</span> </strong> </font>
          <br>"
    )
  })
  
  output$ghg <- renderText({
    p <- input$map_shape_click
    
    if (is.null(p)) {
      selected <- subset(emissions, name == 'World')
    } else {
      selected <- subset(emissions, name == p$id)
    }    
    
    paste("<center> <font size = 3>", "Total emissions: ", "</font>", 
          "<font size = 5> <font color = #074d65> <b>", comma(selected$ghg), "kT CO<sub>2</sub>e", "</b> </font>")
  })
  
  output$rank <- renderText({
    p <- input$map_shape_click
    
    if (is.null(p)) {
      output_text <- paste("<center> <font size = 3> Country rank: </font>",
                           "<font size = 5> <font color = #808080> <b> NA </b> </font>")
    } else {
      selected <- subset(emissions, name == p$id)
      tot <- emissions %>% filter(level == selected$level[selected$name == p$id])
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
      selected <- subset(emissions, name == 'World')
    } else {
      selected <- subset(emissions, name == p$id)
    }    
    
    paste("<center> <font size = 3>", "Emissions per capita: ", "</font>",
          "<font size = 5> <font color = #074d65> <b>", selected$ghgpc, " tCO<sub>2</sub>e", "</b> </font>")
  })
  
  output$rank_int <- renderText({
    p <- input$map_shape_click

    if (is.null(p)) {
      output_text <- paste("<center> <font size = 3> Country intensity rank: </font>",
                           "<font size = 5> <font color = #808080> <b> NA </b> </font>")
    } else {
      selected <- subset(emissions, name == p$id)
      tot <- emissions %>% filter(level == selected$level[selected$name == p$id])
      
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
      selected <- subset(emissions, name == p$id)
      output_text <- paste("<center> <font size = 3>", "Emissions source year: ", "</font>",
            "<font size = 5> <font color = #074d65> <b>", selected$source_year, "</b> </font>")
    }
    
    output_text
    
  })
  
  output$note <- renderText({
    paste("<font size = 1> <i>", "* GHG totals may be slightly different due to different data sources", "</i> </font>")
  })
  
  # electricity
  output$electricity <- renderHighchart({
    p <- input$map_shape_click
    
    if (is.null(p)) {
      selected <- subset(emissions, name == 'World')
    } else {
      selected <- subset(emissions, name == p$id)
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
      selected <- subset(emissions, name == 'World') %>% 
        mutate(pal = "#074d65")
      
      highchart_output <- selected %>% 
        filter(level != 'continent') %>% 
        mutate(country = str_to_title(name)) %>%
        hchart(pointWidth = 75, 'column', hcaes(x = country, y = as.numeric(ghgpc), color = pal), name = 'GHG per capita') %>% 
        hc_xAxis(title = list(text = "")) %>% 
        hc_yAxis(title = list(text = "GHG per capita (tCO<sub>2</sub>e)"))
      
    } else {
      selected <- subset(emissions, name == p$id|name == emissions$region[emissions$name == p$id]|name == 'World') %>% 
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
        mutate(country = str_to_title(name)) %>% 
        hchart('column', hcaes(x = country, y = as.numeric(ghgpc), color = pal), name = 'GHG per capita') %>% 
        hc_xAxis(
          title = list(text = ""),
          categories = list(
            str_to_title(selected$name[selected$name == p$id]),
            str_to_title(selected$region[selected$name == p$id]),
            "World")) %>% 
        hc_yAxis(title = list(text = "GHG per capita (tCO<sub>2</sub>e)"))
      
    }
    
    highchart_output

  })
  
  # Sector
  
  output$sector <- renderHighchart({
    p <- input$map_shape_click
    
    if (is.null(p)) {
      selected <- subset(emissions, name == 'World') 
      
    } else {
      selected <- subset(emissions, name == p$id)
    }
  
  sector_2016 <- selected[,c(1, 22:32)]
  sector_2017 <- selected[,c(1, 33:43)]
  sector_2018 <- selected[,c(1, 44:54)]
  
  sector_2016 <- sector_2016 %>% 
    mutate(group = 2016) %>% 
    rename_at(vars(sector_replace_2016), ~sector_names) %>% 
    pivot_longer(cols = sector_names, names_to = "sector")
  
  sector_2017 <- sector_2017 %>% 
    mutate(group = 2017) %>% 
    rename_at(vars(sector_replace_2017), ~sector_names) %>% 
    pivot_longer(cols = sector_names, names_to = "sector")
  
  sector_2018 <- sector_2018 %>% 
    mutate(group = 2018) %>% 
    rename_at(vars(sector_replace_2018), ~sector_names) %>% 
    pivot_longer(cols = sector_names, names_to = "sector")
  
  sector <- rbind(sector_2016, sector_2017, sector_2018) %>% 
    left_join(join_sector, by = 'sector')%>% 
    mutate(value = value/1000)
  
  sector %>% 
    hchart('column', hcaes(x = group, y = value, group = sector, color = color)) %>% 
    hc_plotOptions(series = list(stacking = "normal")) %>% 
    hc_xAxis(title = list(text = "")) %>% 
    hc_yAxis(title = list(text = "Total sectoral GHG (kTCO<sub>2</sub>e)")) %>% 
    hc_legend(enabled = FALSE)
  
  })
  
  output$notes <- renderUI({
    HTML(str_glue('Created with love by Zachary Gaeddert with the goal of facilitating a better understanding of the climate crisis.<br>
                                         Sources: <a href="https://www.climatewatchdata.org/ghg-emissions?end_year=2018&start_year=1990">Climate Watch</a> (for total emissions),
                                         <a href="https://www.iea.org/reports/world-energy-balances-overview">The International Energy Agency</a> (for electricity grid mix),
                                         <a href="https://ourworldindata.org/emissions-by-sector#annual-greenhouse-gas-emissions-by-sector">CAIT Climate Data Explorer </a> (for sectoral emissions breakdown),
                                         <a href="https://data.worldbank.org/">World Bank Open Data </a> (for demographic data).<br>
                                         Code: <a href="https://github.com/zacharygaeddert/global_emissions">GitHub</a>. <br>
                  <i> Temporary user note: it is still unclear to me why Leaflet is not rendering circle sizes proportionally; e.g., India is smaller than Russia despite having a greater quantity of emissions.'))
  })
})




# Final ------------------------------------------------------------------------

shinyApp(ui, server)
