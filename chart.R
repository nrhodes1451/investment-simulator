library(tidyverse)
library(jsonlite)
library(plotly)
library(TTR)
library(shiny)
library(shinydashboard)
library(shinyjs)

# df_gold <- read_csv("https://www.quandl.com/api/v3/datasets/WGC/GOLD_DAILY_USD.csv?api_key=UomxA9hhyrRZEzg8inv_")
# df_gold %>% saveRDS("data/gold_data.RDS")
df_gold <- readRDS("data/gold_data.RDS")
df_gold <- df_gold[order(df_gold$Date),]

plot_data <- function(df, periods=14, ub=70, lb=30){
  df <- df[order(df$Date),]
  df$RSI <- RSI(df$Value, periods)
  df <- df %>% filter(Date > '2018-01-01')
  
  df <- df %>% 
    mutate(low = ifelse(RSI>30, 0, 30-RSI)) %>% 
    mutate(fill_low = 30-low) %>% 
    mutate(high=ifelse(RSI>70, RSI-70,0)) %>% 
    mutate(fill_high=40)
  
  plot_ly(df, x=~Date, y=~Value, type="scatter", 
          mode="lines", name="Price",
          line=list(color="#111111")) %>%
    add_trace(y=~fill_low, name="RSI", xaxis="x2", yaxis="y2",
              stackgroup = 'one', opacity=0,
              line=list(color="#FFFFFF")) %>% 
    add_trace(y=~low, name="RSI", xaxis="x2", yaxis="y2",
              stackgroup = 'one', fillcolor = '#BB0000',
              line=list(color="#FFFFFF")) %>% 
    add_trace(y=~fill_high, name="RSI", xaxis="x2", yaxis="y2",
              stackgroup = 'one', opacity=0,
              line=list(color="#FFFFFF")) %>% 
    add_trace(y=~high, name="RSI", xaxis="x2", yaxis="y2",
              stackgroup = 'one', fillcolor = '#00BB00',
              line=list(color="#FFFFFF")) %>% 
    add_trace(y=~RSI, name="RSI", xaxis="x2", yaxis="y2",
              line=list(color="#111111")) %>% 
    layout(showlegend = FALSE,
           xaxis = list(showticklabels = FALSE, title="", showgrid=F,
                        linecolor="#111111"),
           yaxis = list(domain = c(0.2, 1), anchor='x', showgrid=F,
                        linecolor="#111111"),
           xaxis2 = list(domain = c(0, 1), anchor='y2', showgrid=F),
           yaxis2 = list(domain = c(0, 0.2), anchor='x2', showgrid=F,
                         linecolor="#111111"))
}