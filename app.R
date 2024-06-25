library(tidyverse)
library(shiny)

player_season_stats = read_csv("data/nba_season_stats.csv", show_col_types = FALSE)

ui <- navbarPage(
      title = "NBA",
      tabPanel(title = "Visualization",
               titlePanel(title = "NBA Player Stats By Season: 1999 - 2020"),
               sidebarLayout(
                 sidebarPanel(
                   selectInput(inputId = "team", 
                               label = "Team:", 
                               choices = sort(unique(player_season_stats$Team))),
                   selectInput(inputId = "player", 
                               label = "Player:", 
                               choices = sort(unique(player_season_stats$Player))),
                   selectInput(inputId = "stat", 
                               label = "Stat:", 
                               choices = sort(unique(player_season_stats$Stat))),
                   checkboxInput(inputId = "avg",
                                 label = "Show Player's Avg Stat",
                                 value = FALSE
                                 )
                 ),
                 mainPanel(plotOutput("plot"))
               )),
      tabPanel(title = "Table", dataTableOutput(("table"))),
      tabPanel(title = "About", includeMarkdown("about.Rmd"))
)

server <- function(input, output) {
  
    nba_team = reactive({
      player_season_stats %>% 
        filter(Team == input$team)
    })

    observeEvent(
      eventExpr = input$team,
      handlerExpr = {
        updateSelectInput(inputId = "player", choices = sort(unique(nba_team()$Player)))
      }
    )
    
    nba_player = reactive(
      nba_team() %>% 
        filter(Player == input$player)
    )
    
    nba_stat = reactive(
      nba_player() %>% 
        filter(Stat == input$stat)
    )
    
    output$plot <- renderPlot({
      player_season_stats %>% 
        filter(Team == input$team) %>% 
        filter(Player == input$player) %>%
        filter(Stat == input$stat) %>% 
        ggplot() + 
        aes(x = Season, y = Total, fill = Season) + 
        geom_bar(stat = "identity") + 
        geom_text(aes(label = Total), vjust = -0.5, size = 3) +
        geom_segment(aes(xend = Season, yend = Total), size = 1) +
        theme_bw() 
    })
    
    output$table = renderDataTable({
      tab = nba_player() %>% 
        avg_stat()
      
      if(input$avg == TRUE) {
        tab
      } else {
        nba_player() %>% 
          pivot_wider(names_from = Stat, values_from = Total)
      }
    })
}

shinyApp(ui = ui, server = server)
