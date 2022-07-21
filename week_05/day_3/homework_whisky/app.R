library(shiny)
library(tidyverse)
library(leaflet)
library(bslib)

options(scipen = 999)
whisky <- CodeClanData::whisky
distillery_select <- unique(whisky$Distillery)
owner_select <- unique(whisky$Owner)



# UI ----------------------------------------------------------------------


ui <- fluidPage(
  titlePanel(title = "Whisky Distillery"),
  
  theme = bs_theme(bootswatch = "superhero"),
  

# Distillery Tab ----------------------------------------------------------

  
  
  tabsetPanel(
    tabPanel(
      title = "Distillery",
      
      HTML("<br>"),
      fluidRow(
        column(
          width = 6,
          offset = 3,
          selectInput(
            label = "Select Distillery",
            inputId = "distillery",
            choices = distillery_select,
            selected = "Aberfeldy"
          )
        )
      ),
      HTML("<br>"),
      HTML("<br>"),
      
      fluidRow(
        column(
          width = 6,
          leafletOutput("map_plot")
        ),
        
        column(
          width = 6,
          plotOutput("flavour")
        )
      ),
      
      HTML("<br>"),
      HTML("<br>"),
      fluidRow(
        column(
          width = 6,
          tableOutput("distillery_table")
        )
      )
      
    

# Owner tab ---------------------------------------------------------------

      
    ),
    tabPanel(
      title = "Owner",
      HTML("<br>"),
      fluidRow(
        column(
          width = 6,
          offset = 3,
          selectInput(
            label = "Select Owner",
            inputId = "owner",
            choices = owner_select
          )
        )
      ),
      HTML("<br>"),
      HTML("<br>"),
      fluidRow(
        column(
          width = 6,
          leafletOutput("owner_plot")
        ),
        
        column(
          width = 6,
          plotOutput("capacity_plot")
        )
      ),
      
      HTML("<br>"),
      HTML("<br>"),
      fluidRow(
        column(
          width = 6,
          tableOutput("owner_table")
        )
      )
    )
  )
)


# Server ------------------------------------------------------------------


server <- function(input, output, session) {
  
  filtered_data <- reactive({
    whisky %>%
      filter(Distillery == input$distillery)
  })
  
  owner_data <- reactive({
    whisky %>%
      filter(Owner == input$owner)
  })
   # latitude <- reactive({
   #   whisky$Latitude[which(whisky$Distillery == input$distillery_select)]
   #   })
   # longitude <- reactive({
   #   whisky$Longitude[which(whisky$Distillery == input$distillery_select)]
   #   })
  
  # points <- eventReactive(input$distillery, {
  #   
  #   if (length(distillery_select) == 0){
  #     filter_by_this = "Aberfeldy"
  #   }
  #   
  #   
  #   latitude <- whisky %>% 
  #     filter(Distillery == input$distillery_select) %>% 
  #     select(Latitude) %>% 
  #     pull()
  #   
  #   longitude <-  whisky %>% 
  #     filter(Distillery == input$distillery_select) %>% 
  #     select(Longitude) %>% 
  #     pull()
  #   
  #   cbind(latitude, longitude)
  # }, ignoreNULL = FALSE)
  
  
  output$map_plot <- renderLeaflet({
    # latitude <- whisky %>% 
    #   filter(Distillery == input$distillery_select) %>% 
    #   select(Latitude) %>% 
    #   pull()
    # 
    # longitude <-  whisky %>% 
    #   filter(Distillery == input$distillery_select) %>% 
    #   select(Longitude) %>% 
    #   pull()
    
    
    leaflet() %>% 
      addTiles() %>% 
      setView(lat = 57, lng = -4,zoom = 6)
      # addMarkers(data = points())
      # addCircleMarkers(data = whisky, lng = ~Longitude, lat = ~Latitude,
      #                  radius = 10, color = "#03F", popup = NULL)
  })
  
  
  output$flavour <- renderPlot({
    filtered_data() %>% 
      pivot_longer(Body:Floral, names_to = "flavours", values_to = "amount") %>% 
      ggplot(aes(x = flavours,
                 y = amount,
                 fill = flavours)) +
      geom_col(show.legend = FALSE) +
      scale_fill_brewer(palette = "Dark2") +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(title = "Taste of Whisky",
           y = "Rating",
           x = "Flavour") +
      coord_flip()
  })
  
  observeEvent(input$distillery, {

    leafletProxy("map_plot") %>%
      clearMarkers() %>%
      setView(lat = 57, lng = -4,zoom = 6) %>%
      addCircleMarkers(data = filtered_data(), lng = ~ Longitude, lat = ~ Latitude,
                       radius = 10, color = "#03F", popup = NULL)
  })
  
  output$distillery_table <- renderTable({
    filtered_data() %>% 
      select(-(Body:Floral), -RowID)
  })
  
  output$capacity_plot <- renderPlot({
    owner_data() %>% 
      ggplot(aes(x = Distillery,
                 y = Capacity,
                 fill = Distillery)) +
      geom_col(show.legend = FALSE) +
      scale_fill_brewer(palette = "Dark2") +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(title = "Capacity of Owner",
           x = "Distillery",
           y = "Capacity") +
      coord_flip()
  })
  
  output$owner_plot<- renderLeaflet({
    
    leaflet() %>% 
      addTiles() %>% 
      setView(lat = 57, lng = -4,zoom = 6)
  })
  
  observeEvent(input$owner, {
    
    leafletProxy("owner_plot") %>%
      clearMarkers() %>%
      setView(lat = 57, lng = -4,zoom = 6) %>%
      addCircleMarkers(data = owner_data(), lng = ~ Longitude, lat = ~ Latitude,
                       radius = 10, color = "#03F", popup = NULL)
  })
  
  
  output$owner_table <- renderTable({
    owner_data() %>% 
      select(-(Body:Floral), -RowID)
  })
  
}

shinyApp(ui, server)