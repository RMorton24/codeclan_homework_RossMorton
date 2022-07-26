library(shiny)
library(tidyverse)
library(shinyWidgets)
library(bslib)
library(plotly)


#thematic::thematic_shiny(font = "auto")
game_sales <- CodeClanData::game_sales

ui <- fluidPage(
  #theme = bs_theme(bootswatch = "minty"),
  titlePanel("Game Information"),
  
  sidebarPanel(
    textOutput("temp"),
    
    pickerInput(
      inputId = "develop_publish",
      label = "Select Video Game",
      choices = unique(game_sales$developer),
      options = list(`actions-box` = TRUE),
      multiple = TRUE
      ),
    
    sliderInput(
      inputId = "year",
      label = "Select Year of Release",
      min = min(game_sales$year_of_release),
      max = max(game_sales$year_of_release),
      value = c(min(game_sales$year_of_release),
                max(game_sales$year_of_release)),
      sep = ""
      
    ),
    
  ),
  
  mainPanel(
    tabsetPanel(
      id = "panel",
      
      # Developer panel# -------------------------------------------------------
      tabPanel(
        "Developer",
        HTML("<br>"),
        plotlyOutput("game_developer"),
        HTML("<br>"),
        fluidRow(
          column(
            6,
            plotlyOutput("score_developer")
          ),
          column(
            6,
            plotlyOutput("sales_developer")
            #plotOutput("sales_developer")
          )
        )
      ),
      # Publisher panel --------------------------------------------------------
      tabPanel(
        "Publisher",
        HTML("<br>"),
        plotlyOutput("game_publisher"),
        HTML("<br>"),
        fluidRow(
          column(
            6,
            plotlyOutput("score_publisher")
          ),
          column(
            6,
            plotlyOutput("sales_publisher")
            #plotOutput("sales_publisher")
          )
        )
      ),
      
      
      # Platform panel ---------------------------------------------------------
      
      tabPanel(
        "Platform",
        HTML("<br>"),
        plotlyOutput("game_platform"),
        HTML("<br>"),
        fluidRow(
          column(
            6,
            plotlyOutput("score_platform")
          ),
          column(
            6,
            plotlyOutput("sales_platform")
            #plotOutput("sales_platform")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  
  game_filter <- reactive(
    game_sales %>% 
      filter(!!as.name(str_to_lower(input$panel)) %in% input$develop_publish,
             between(year_of_release, input$year[1], input$year[2]))
      
  )
  
  
  observeEvent(input$panel,{
    updatePickerInput(session, inputId = "develop_publish",
                      label = paste("Select", input$panel),
                      choices = unique(select(game_sales, str_to_lower(input$panel)))
                      )
  })
  
  
  game_number_plot <- renderPlotly(
    game_filter() %>% 
      group_by(!!as.name(str_to_lower(input$panel)), year_of_release) %>% 
      summarise(number_games = n()) %>% 
      ungroup() %>% 
      plot_ly(x = ~year_of_release,
              y= ~ number_games,
              type = "bar",
              color = as.formula(paste0("~", str_to_lower(input$panel))),
              colors = "Dark2") %>% 
      layout(title = "Number of Games Per Year",
             yaxis = list(title = "Number of Games"),
             xaxis = list(title = "Year of Release",
                          dtick = 1))
  )
  
  
  score_plot <- renderPlotly(
    game_filter() %>% 
    plot_ly(x = ~critic_score,
            y= ~ user_score,
            text = ~ name,
            type = "scatter",
            color = as.formula(paste0("~", str_to_lower(input$panel))),
            colors = "Dark2") %>% 
      layout(title = "Critic vs User Score",
             yaxis = list(title = "User Score"),
             xaxis = list(title = "Critic Score"))
  )
  

  
  #  sales_plot <- renderPlot({
  #    game_filter() %>%
  #      group_by(genre) %>%
  #      mutate(total_sales = sum(sales)) %>%
  #      ggplot(aes(x = reorder(genre, total_sales),
  #                 y = sales,
  #                 fill = rating)) +
  #      geom_col() +
  #      scale_fill_brewer(palette = "Purples") +
  #      coord_flip() +
  #      labs(title = paste0("Sales per Genre for selected ", input$panel, "(s)"),
  #           x = "Genre",
  #           y = "Sales") +
  #      theme_classic()
  # })
  
  sales_plotly <- renderPlotly(
    game_filter() %>% 
      group_by(genre) %>% 
      mutate(total_sales = sum(sales)) %>% 
      # arrange(desc(total_sales)) %>% 
      plot_ly(x = ~sales,
              y= ~ reorder(genre, total_sales),
              type = "bar",
              orientation = "h",
              color = ~rating,
              colors = "Set2") %>% 
      layout(title = paste0("Sales per Genre for selected ", input$panel, "(s)"),
             barmode = "stack",
             yaxis = list(title = "Game Genre"),
             xaxis = list(title = "Sales"))
  )
  
  
  output$game_developer <- reactive(game_number_plot())
  
  output$game_publisher <- reactive(game_number_plot())
  
  output$game_platform <- reactive(game_number_plot())
  
  output$score_developer <- reactive(score_plot())
  
  output$score_publisher <- reactive(score_plot())
  
  output$score_platform <- reactive(score_plot())
  
  output$sales_publisher <- reactive(sales_plotly())

  output$sales_platform <- reactive(sales_plotly())

  output$sales_developer <- reactive(sales_plotly())
  
  
  # Somehow doesn't work??
  # output$sales_publisher <- reactive(sales_plot())
  # 
  # output$sales_platform <- reactive(sales_plot())
  # 
  # output$sales_developer <- reactive(sales_plot())
  
  # output$sales_platform <-  renderPlot(
  #   game_filter() %>% 
  #     group_by(genre) %>%
  #     mutate(total_sales = sum(sales)) %>%
  #     ggplot(aes(x = reorder(genre, total_sales),
  #                y = sales,
  #                fill = rating)) +
  #     geom_col() +
  #     scale_fill_brewer(palette = "Set2") +
  #     coord_flip() +
  #     labs(title = paste0("Sales per Genre for selected ", input$panel, "(s)"),
  #          x = "Genre",
  #          y = "Sales") +
  #     theme_classic()
  # )
  # 
  # output$sales_publisher <-  renderPlot(
  #   game_filter() %>%
  #     group_by(genre) %>%
  #     mutate(total_sales = sum(sales)) %>%
  #     ggplot(aes(x = reorder(genre, total_sales),
  #                y = sales,
  #                fill = rating)) +
  #     geom_col() +
  #     scale_fill_brewer(palette = "set2") +
  #     coord_flip() +
  #     labs(title = paste0("Sales per Genre for selected ", input$panel, "(s)"),
  #          x = "Genre",
  #          y = "Sales") +
  #     theme_classic()
  # )
  # 
  # output$sales_developer <-  renderPlot(
  #   game_filter() %>% 
  #     group_by(genre) %>%
  #     mutate(total_sales = sum(sales)) %>%
  #     ggplot(aes(x = reorder(genre, total_sales),
  #                y = sales,
  #                fill = rating)) +
  #     geom_col() +
  #     scale_fill_brewer(palette = "Set2") +
  #     coord_flip() +
  #     labs(title = paste0("Sales per Genre for selected ", input$panel, "(s)"),
  #          x = "Genre",
  #          y = "Sales") +
  #     theme_classic()
  # )
  # 
  
}

shinyApp(ui, server)
