library(dplyr)
avg_stat = function(data) {
  wide_data = data %>% 
    pivot_wider(names_from = Stat, values_from = Total) %>% 
    mutate(Avg_Points = round(PTS / GP, digits = 2),
           Avg_Rebounds = round(REB / GP, digits = 2),
           Avg_Assists = round(AST / GP, digits = 2),
           Avg_Steals= round(STL / GP, digits = 2),
           Avg_Blocks= round(BLK / GP, digits = 2),
           Avg_Minutes= round(MIN / GP, digits = 2),
           Avg_FGM = round(FGM / GP, digits = 2),
           Avg_3PM = round(`3PM` / GP, digits = 2),
           Avg_FTM = round(FTM / GP, digits = 2),
           Avg_Turnovers= round(TOV / GP, digits = 2)) %>% 
    select(Season,
           Player,
           Team,
           GP, 
           Avg_Minutes,
           Avg_Points,
           Avg_Assists,
           Avg_Rebounds,
           Avg_Steals,
           Avg_Blocks,
           Avg_Turnovers,
           Avg_FGM,
           Avg_FTM,
           Avg_3PM
           )
}
