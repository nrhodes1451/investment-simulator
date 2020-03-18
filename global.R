library(tidyverse)
library(lubridate)
library(plotly)
library(zoo)
library(TTR)
library(shiny)
library(shinydashboard)
library(shinyjs)

global_options <- list(
  color = 'primary',
  charts = list(
    font = list(family = "sans-serif", size = 12),
    colors = list(
      purple='rgb(111,87,152)',
      blue='rgb(47,195,199)',
      grey="rgb(100,100,100")
  )
)

global <- list(
  data = list(
    rsi = readRDS("data/precious-metals.RDS") %>% 
      tail(-1) %>% 
      mutate(gold = na.approx(gold),
             silver = na.approx(silver),
             ratio = na.approx(ratio))
  ),
  start_date = '1995-01-01',
  end_date = '2019-12-31'
)