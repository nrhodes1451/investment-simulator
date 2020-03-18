shinyServer(function(input, output, clientData, session) {
  options(warn =-1)

  observe({
    # Error message ----
    display_error <- function(message){
      renderUI({
        HTML(paste0(
          "<div class=\"error-notification-overlay\"></div>
        <div class=\"shiny-notification shiny-notification-error\">
          <div class=\"shiny-notification-close\"
            onclick=\"$('.error-notification-overlay').hide();
                    $('.shiny-notification-error').hide();\">
            <i class=\"fa fa-times-circle\"></i>
          </div>
          <div class=\"shiny-notification-content\">
            <div class=\"shiny-notification-content-text\">",
          message,
          "</div>
            <div class=\"shiny-notification-content-action\"></div>
          </div>
        </div>"
        ))
      })
    }

    # RSI tab ----
    print(input$inp_rsi_src)

    # RSI Plot
    output$render_rsi <- renderPlotly({
      if(input$inp_rsi_src == ''){
        plotly_empty()
      }
      else{
        df <- global$data$rsi[c("date", input$inp_rsi_src)] %>% 
          filter(date >= input$inp_rsi_dat[1] &
                 date <= input$inp_rsi_dat[2])
        df$RSI <- RSI(df[[2]], input$inp_rsi_lbw)
        df <- df %>% 
          mutate(low = ifelse(RSI>input$inp_rsi_lb, 0, input$inp_rsi_lb-RSI)) %>% 
          mutate(fill_low = input$inp_rsi_lb-low) %>% 
          mutate(high=ifelse(RSI>input$inp_rsi_ub, RSI-input$inp_rsi_ub,0)) %>% 
          mutate(fill_high=input$inp_rsi_ub-input$inp_rsi_lb)
        
        plot_ly(df, x=~date, y=df[[2]], type="scatter", 
                mode="lines", name="Price",
                line=list(color="#333333", width=0.85)) %>%
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
                    line=list(width = 0.75, color="#888888")) %>% 
          layout(showlegend = FALSE,
                 shapes=list(
                   list(type='line',
                     x0=input$inp_rsi_dat[1],
                     x1=input$inp_rsi_dat[2],
                     y0=input$inp_rsi_lb,
                     y1=input$inp_rsi_lb, 
                     yref="y2",
                     line=list(color="#AAAAAA", width=0.5)),
                   list(type='line',
                     x0=input$inp_rsi_dat[1],
                     x1=input$inp_rsi_dat[2],
                     y0=input$inp_rsi_ub,
                     y1=input$inp_rsi_ub, 
                     yref="y2",
                     line=list(color="#AAAAAA", width=0.5))),
                 xaxis = list(showticklabels = FALSE, title="", showgrid=F,
                              linecolor="#111111"),
                 yaxis = list(domain = c(0.2, 1), title="Price", anchor='x', showgrid=F,
                              linecolor="#111111"),
                 xaxis2 = list(domain = c(0, 1), anchor='y2', showgrid=F),
                 yaxis2 = list(domain = c(0, 0.2), title="RSI", anchor='x2', showgrid=F,
                               linecolor="#111111"))
      }
    })
    
    # Investment plot ----
    observeEvent(input$btn_rsi_calc, {
      df <- global$data$rsi[c("date", input$inp_rsi_src)] %>% 
        filter(date >= input$inp_rsi_dat[1] &
                 date <= input$inp_rsi_dat[2])
      df$RSI <- RSI(df[[input$inp_rsi_src]], input$inp_rsi_lbw)
      initial_investment <- input$inp_rsi_inv
      cash <- initial_investment
      value_history <- cash
      total_spend <- cash
      spend_history <- cash
      commission <- input$inp_rsi_com / 100
      units <- (1-commission) * cash / df[[2]][1]
      commission_history = commission * cash / df[[2]][1]
      total_commission = commission_history
      units_history <- units
      cash <- 0
      cash_history <- cash
      sell_perc = input$inp_rsi_sell / 100
      
      i <- 1
      rsi <- df$RSI[1]
      case <- "none"
      
      while(i < nrow(df)){
        i <- i+1
        price = df[[2]][i]
        
        prev_rsi <- df$RSI[i-1]
        new_rsi <- df$RSI[i]
        
        if(is.na(prev_rsi)) prev_rsi <- 50
        if(is.na(new_rsi)) new_rsi <- 50
        if(new_rsi < 50 & prev_rsi > 50) case="none"
        if(new_rsi > 50 & prev_rsi < 50) case="none"
        # Sell
        if(case!="sell" & new_rsi < 70 & prev_rsi>70){
          case="sell"
          sale_value <- units * sell_perc * price * (1-commission)
          total_commission <- total_commission + units * sell_perc * price * commission
          cash <- cash+sale_value
          units <- units * (1-sell_perc)
        }
        # Buy
        else if(case!="buy" & new_rsi>30 & prev_rsi<30){
          case="buy"
          buy_volume <- (1-commission) * initial_investment / price
          total_commission <- total_commission + commission * initial_investment
          units <- units + buy_volume
          cash <- cash
          total_spend <- total_spend + initial_investment
        }
        
        value_history <- c(value_history, cash+units*price)
        cash_history <- c(cash_history, cash)
        units_history <- c(units_history, units)
        spend_history <- c(spend_history, total_spend)
        commission_history <- c(commission_history, total_commission)
      }
      df$investment <- spend_history
      df$assets <- value_history
      df$commission <- commission_history
      
      growth <-  tail(df$assets, 1)/tail(df$investment, 1)
      n_years <- interval(min(df$date), max(df$date)) / years(1)
      ann_growth = round(100*(growth ^ (1/n_years) - 1), 2)
    
      # Render investment plot ----
      output$render_rsi_invest <- renderPlotly({
        if(input$inp_rsi_src == ''){
          plotly_empty()
        }
        else if(length(df)==3){
          plot_ly(df, x=~date, y=df[[2]], type="scatter", 
                  mode="lines", name="Price",
                  line=list(color="#333333", width=0.85))
        }
        else{
          plot_ly(x=df$date,
                  y=df$investment,
                  type="scatter", mode="lines",
                  name="Cash comparison (assuming no investment)",
                  line=list(color="#111111")) %>%
            add_trace(x=df$date, y=df$assets,
                      name="Simulated Assets",
                      line=list(color="#11AA11")) %>% 
            add_trace(x=df$date, y=df$commission,
                      name="Total Commission",
                      line=list(color="#880000")) %>% 
            layout(
              title=paste0("Annual return: ", ann_growth, "%"),
              legend = list(orientation = 'h')
            )
        }
      })
    })
  })
})
