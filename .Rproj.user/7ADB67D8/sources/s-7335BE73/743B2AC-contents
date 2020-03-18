# Header
header <- dashboardHeader(title = "Investment Simulator")

# Sidebar ----

sidebar <- dashboardSidebar(
  hr(),
  sidebarMenu(id="tabs",
    menuItem("RSI", tabName = "rsi", icon = icon("coins"))
  ),
  collapsed = TRUE
)

# Body ----

body <- dashboardBody(
  useShinyjs(),
  # HTML Tags ----
  tags$head(
    tags$script(src = "scripts.js"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(rel="shortcut icon",
    href="favicon.ico")
  ),

  # Loading bars ----
  conditionalPanel(condition="check_shiny_busy()", id="loading",
    tags$div(class="plotlybars-wrapper",
      tags$div(class="plotlybars",
        tags$div(class="plotlybars-bar b1"),
        tags$div(class="plotlybars-bar b2"),
        tags$div(class="plotlybars-bar b3"),
        tags$div(class="plotlybars-bar b4"),
        tags$div(class="plotlybars-bar b5"),
        tags$div(class="plotlybars-bar b6"),
        tags$div(class="plotlybars-bar b7")
      ),
    tags$div(class="plotlybars-text")
    )
  ),

  tabItems(
    # Error message
    htmlOutput("error_message"),
    # RSI tab ----
    tabItem(tabName = "rsi",
      fluidRow(id="rsi_row1",
        column(width=3,
          box(
            width = NULL,
            height=590,
            status = global_options$color,
            solidHeader = TRUE,
            title=paste("Parameters"),
            dateRangeInput("inp_rsi_dat", "Date range",
                           start="2015-01-01", end="2019-12-31"),
            selectInput("inp_rsi_src", "Data Series:", c("gold", "silver")),
            numericInput("inp_rsi_lbw", "Lookback window:", 14),
            numericInput("inp_rsi_ub", "Upper Threshold:", 70),
            numericInput("inp_rsi_lb", "Lower Threshold:", 30),
            numericInput("inp_rsi_com", "% Commission:", 1),
            numericInput("inp_rsi_sell", "Sell %:", 50),
            numericInput("inp_rsi_inv", "Investment:", 100),
            actionButton('btn_rsi_calc', 'Calculate')
          )
        ),
        box(
          width = 9,
          height=590,
          status = global_options$color,
          solidHeader = TRUE,
          title="Price History vs RSI",
          plotlyOutput("render_rsi", height="535px")
        )),
      fluidRow(id="rsi_row2",
       box(
         width = 12,
         height=400,
         status = global_options$color,
         solidHeader = TRUE,
         title="Time Series",
         plotlyOutput("render_rsi_invest", height="345px"))
      )
    )
  )
)
# Run dashboard ----
dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body,
  title = paste("Investment Simulator")
)
