library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinyWidgets)
library(tidyverse)
library(leaflet)
library(scales)
library(lubridate)
library(plotly)

# Load data and add popup label for the map
df_crime <- read_csv("dentoncrimedata_geocode.csv") %>% 
    janitor::clean_names() %>% 
    mutate(date = as.Date(date_time)) %>% 
    mutate(crime_lumped = fct_lump(crime, 5),
           popup_label = paste(paste0("<b>Crime Type: ", crime, "</b>"),
                               paste0("Address: ", public_address),
                               paste0("Time: ", date_time),
                               sep = "<br/>"))

# Get distinct name for the dropdown menu of crime type
distinct_crime <- df_crime %>% 
    distinct(crime)

# Custom shinydashboard logo
customLogo <- shinyDashboardLogoDIY(
    boldText = "Denton"
    ,mainText = "Crime Watch"
    ,textSize = 19
    ,badgeText = "Dashboard"
    ,badgeTextColor = "white"
    ,badgeTextSize = 1.5
    ,badgeBackColor = "#40E0D0"
    ,badgeBorderRadius = 8
)

# Make user interface
ui <- dashboardPage(
    dashboardHeader(title = customLogo,
                    titleWidth = 300),
    # Make dashboard sidebar
    dashboardSidebar(
        width = 300,
        # Create search box for street name
        sidebarSearchForm(textId = "address",
                          buttonId = "submit",
                          label = "Street name (e.g., Fry St)"),
        sidebarMenu(
            # Create about tab
            menuItem(text = "About",
                     tabName = "about",
                     icon = icon("info-circle"),
                     badgeLabel = "Read",
                     badgeColor = "green"),
            # Create map tab
            menuItem(text = "Map",
                     tabName = "map",
                     icon = icon("map")),
            # Create dashboard tab
            menuItem(text = "Dashboard",
                     tabName = "dashboard",
                     icon = icon("dashboard")),
            # Create data table tab
            menuItem(text = "Data Table",
                     tabName = "datatable",
                     icon = icon("table")),
            # Create source code tab
            menuItem("Source Code",
                     icon = icon("file-code-o"), 
                     href = "https://github.com/tsquall121/Denton_Crime_Watch")),
        # Create date range boxes
        dateRangeInput(inputId = "date", label = "Date Range:",
                       start = "2019-11-06",
                       end = "2021-05-06",
                       min = "2019-11-06",
                       max = "2021-05-06",
                       format = "yyyy-mm-dd",
                       separator = "to"),
        # Create crime type dropdown menu
        pickerInput(inputId = "crimetype",
                    label = "Crime Type:",
                    choices = distinct_crime,
                    multiple = TRUE,
                    options = pickerOptions(
                        actionsBox = TRUE,
                        size = 10, # Only show 10 items in the dropdown menu
                        dropupAuto = FALSE # Drop down menu only pops downward
                    ))),
    # Make dashboard main page
    dashboardBody(
        shinyDashboardThemes(
            theme = "grey_dark"
        ),
        tabItems(
            tabItem(tabName = "about",
                    uiOutput("about")),
            tabItem(tabName = "map",
                    tags$style(type = "text/css", # Set map to fill up the tab
                               "#map {height: calc(100vh - 80px) !important;}"),
                    leafletOutput("map")),
            tabItem(tabName = "dashboard",
                    fluidRow(
                        box(title = strong("Count the most common crime(s)"),
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            plotlyOutput("bar"),
                            width = 12)
                    ),
                    fluidRow(
                        box(title = strong("Count the most common public
                                           address(es) for crime"),
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            plotlyOutput("lollipop"),
                            width = 12)
                    )),
            tabItem(tabName = "datatable",
                    dataTableOutput("datatable"))
            )
    ))

# Make server
server <- function(input, output) {
    output$about <- renderUI({
        tagList(
            h2("Denton Crime Watch Shiny Dashboard"),
            h3("by", a("Jie Tao", href = "https://www.linkedin.com/
                       in/jie-tao-5a7a06126/")),
            br(),
            p(strong("About the dashboard:")),
            p("This application uses Denton crime data available from
                           the City of Denton",
              a("open data portal.", href = "https://data.cityofdenton.com/dataset"),
              "It allows users to retrieve and visualize crime data based on their
              choice of the 1) public address, 2) date range, and 3) crime type.
              Users can easily 1) identify crimes on the map, 2) figure out the
              number of crimes by crime type and public address from the
              dashboard, and 3) look up details of each crime in the data table."),
            br(),
            p(strong("How to use:")),
            p("1. Select a date range between",
              span(em("2019-11-06"), style = "color:lightblue"),
              "and",
              span(em("2021-05-06"), style = "color:lightblue"), ";"),                                                       
            p("2. Select", span(em("at least one"), style = "color:lightblue"),
              "type of crime from the",
              span(em("Crime Type box"), style = "color:lightblue"), ";"),
             p("3. Type a street name (e.g., Fry St, Ave A, Oak St, etc...)",
              span(em("without the street number"), style = "color:lightblue"),
              "due to privacy concern from the original data or you can simply",
              span(em("leave it blank for crimes at all locations"),
                   style = "color:lightblue"), ";"),
            p("4. Click on", code(icon("search"))),
            p("5. Navigate to the",
              span(em("Map tab"), style = "color:lightblue"), "for crime locations
              and click on each point for details;"),
            p("6. Navigate to the", span(em("Dashboard tab"), style = "color:lightblue"),
              "for the number of crimes by crime type and public address;"),
            p("7. Navigate to the",
              span(em("Data Table tab"), style = "color:lightblue"),
              "to look up details of each crime by yourself."),
            br(),
            p(strong("About me:")),
            p("I am an assistant professor & data scientist who loves to help more people understand the
              government with data science tools. If you have any questions about
              the dashboard, please reach me at",
              span("jtao@tarleton.edu", style = "color:orange"),
              code(icon("envelope-square")))
        )
    })
    # Create a reactive function to hold the user selected data frame
    filtered_data <- eventReactive(input$submit, {
        df_crime %>% 
            filter(str_detect(public_address,
                              toupper(input$address))) %>%
            filter(date >= input$date[1] & date <= input$date[2]) %>% 
            filter(crime %in% input$crimetype)}
        )
    # Make a palette for the map
    pal <- colorFactor(
        palette = viridis_pal(begin = 0, end = 1, option = 'B')(50),
        domain = df_crime$crime
    )
    # Create the map
    output$map <- renderLeaflet({
        leaflet() %>%
            addProviderTiles("Stamen.TonerLite") %>% 
            addCircleMarkers(data = filtered_data(),
                             stroke = FALSE,
                             radius = 4,
                             fillColor = ~pal(crime),
                             fillOpacity = 1,
                             popup = ~popup_label) %>% 
            addLegend(data = filtered_data(),
                      pal = pal,
                      values = ~crime,
                      title = "Crime Type")
    }) 
    # Make a reactive function to hold a user selected data frame for bar chart
    count_data <- eventReactive(input$submit, {
        df_crime %>% 
            filter(str_detect(public_address,
                              toupper(input$address))) %>%
            filter(date >= input$date[1] & date <= input$date[2]) %>% 
            filter(crime %in% input$crimetype) %>% 
            count(crime, sort = TRUE) %>% 
            mutate(crime = fct_reorder(crime, n))}
    )
    # Create a bar chart
    output$bar <- renderPlotly({
        p <- ggplot(count_data() %>% slice(1:10),
                    aes(n, crime,
                        fill = crime,
                        color = crime)) +
                   geom_col() +
                   scale_x_continuous(labels = comma) +
                   labs(x = "# of crimes",
                        title = paste("Number of crimes:",
                                      as.character(input$address),
                                       "<br>",
                                       "<sup>",
                                       "From",
                                      as.character(input$date[1]),
                                      "to",
                                      as.character(input$date[2]),
                                      "</sup>",
                                      sep = " ")) +
                    ggthemes::theme_fivethirtyeight(base_family = "Arial") +
                    theme(legend.position = "none")
                
        plotly::ggplotly(p, tooltip = "x")
        })
    # Make a reactive function to hold a user selected data frame for lollipop chart
    count_streetcrime <- eventReactive(input$submit, {
        df_crime %>% 
            filter(str_detect(public_address,
                              toupper(input$address))) %>%
            filter(date >= input$date[1] & date <= input$date[2]) %>% 
            filter(crime %in% input$crimetype) %>% 
            count(public_address, sort = TRUE) %>% 
            mutate(public_address = fct_reorder(public_address, n))}
    )
    # Create a lollipop chart
    output$lollipop <- renderPlotly({
        p <- ggplot(count_streetcrime() %>% slice(1:10),
                    aes(n, public_address)) +
            geom_errorbarh(aes(xmin = 0, xmax = n), height = 0,
                           size = 1.5) +
            geom_point(aes(size = n, color = public_address)) +
            scale_x_continuous(labels = comma) +
            labs(x = "# of crimes",
                 title = paste("Number of crimes:",
                               as.character(input$address),
                               "<br>",
                               "<sup>",
                               "From",
                               as.character(input$date[1]),
                               "to",
                               as.character(input$date[2]),
                               "</sup>",
                               sep = " ")) +
            ggthemes::theme_fivethirtyeight(base_family = "Arial") +
            theme(legend.position = "none")
        
        plotly::ggplotly(p, tooltip = "x")
    })
    # Make a reactive function to hold a user selected data frame for data table
    filtered_data_selected <- eventReactive(input$submit, {
        df_crime %>% 
            filter(str_detect(public_address,
                              toupper(input$address))) %>%
            filter(date >= input$date[1] & date <= input$date[2]) %>% 
            filter(crime %in% input$crimetype) %>% 
            select(id, date_time, crime, public_address, location_name)}
    )
    # Create a data table
    output$datatable <- renderDataTable(filtered_data_selected(),
                                        options = list(pageLength = 10))
}

shinyApp(ui, server)
