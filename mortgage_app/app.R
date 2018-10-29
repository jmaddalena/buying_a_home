library(shiny)
library(tidyverse)
source("source.R")

ui <- fluidPage(
  
  titlePanel("Mortgage Considerations"),
  br(),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Mortgage Specification"),
      sliderInput("purchase_price", label = h5("Purchase Price"), min = 100000, max = 1000000,
                  value = 400000, step = 10000),
      sliderInput("perc_down", label = h5("Percent Down Payment"), min = 0, max = 100,
                  value = 3, step = .1),
      radioButtons("years", label = h5("Loan Term"), selected = 30,
                   choices = list("15 year" = 15, "20 year" = 20, "30 year" = 30)),
      sliderInput("int", label = h5("Interest Rate"), min = 0, max = 10,
                   value = 5.375, step = .005),
      
      br(),
      h4("Closing Costs"),
      sliderInput("settlement_cost", label = h5("Settlement Costs"), 
                  min = 0, max = 25000, value = 12356, step = 1),
      sliderInput("ins_prem", label = h5("Insurance Premium"),
                 min = 0, max = 5000, value = 1000, step = 100),
      
      br(),
      h4("Additional Monthly Expenses"),
      sliderInput("tax_perc", label = h5("Property Tax Percentage"),
                 value = 0.35, min = 0, max = 2, step = .05),
      sliderInput("pmi_perc", label = h5("PMI Percentage"),
                 value = 0.5, min = 0, step = .1, max = 5),
      sliderInput("ins_perc", label = h5("Homeowners Insurance Percentage"),
                 value = 0.25, min = 0, max = 1, step = .05),
      
      br(),
      h4("Assumptions"),
      sliderInput("est_growth", label = h5("Annual Appreciation Percentage"), 
                  value = 5.7, min = 0, max = 20, step = .1)
  
    ),
    
    mainPanel(
      plotOutput(outputId = "pay_plot")  
    )
  )
)

server <- function(input, output){

  data_list <- reactive({
    
    years <- as.numeric(input$years)
    
    principle <- input$purchase_price - (input$perc_down/100)*input$purchase_price
    
    month_pay <- ((input$int/1200)*principle*(1+(input$int/1200))^(years*12))/((1+(input$int/1200))^(years*12) - 1)
    
    months <- 12*years
    
    # updatable variables
    curr_balance <- principle
    equity <- (input$perc_down/100)*input$purchase_price
    home_value <- input$purchase_price
    
    all_expenses <- NULL
    equity_sunk <- NULL
    
    for(month in 1:months){
      
      # Settlement, down
      if(month == 1){
        month1 <- data_frame(`Down Payment`= (input$perc_down/100)*input$purchase_price,
                             `Settlement Costs` = input$settlement_cost,
                             `Insurance Premium` = input$ins_prem) %>%
          gather(expense, cost)
      } else {
        month1 <- NULL
      }
      
      # Must pay every month
      
      home_value <- home_value + input$purchase_price*(input$est_growth/100)/12
      
      if(month == 1){
        print(curr_balance) ; print(input$int/100); print(month_pay)
      }
      
      mo_pay_breakdown <- calc_mo_pay(curr_balance, int = input$int/100, payment = month_pay)
      
      curr_balance <- mo_pay_breakdown$new_balance
      
      principle_pay <- mo_pay_breakdown$old_balance - mo_pay_breakdown$new_balance
      
      must_pays <- data_frame(`Interest` = mo_pay_breakdown$int_pay,
                              `Principle` = principle_pay,
                              `Taxes` = (input$tax_perc/100)*home_value/12,
                              `Insurance` = (input$ins_perc/100)*home_value/12) %>%
        gather(expense, cost)
      
      equity <- equity + principle_pay + input$purchase_price*(input$est_growth/100)/12
      
      # PMI
      
      if(equity < .2*input$purchase_price){
        pmi <- data_frame(`PMI` = (input$pmi_perc/100)*input$purchase_price/12) %>%
          gather(expense, cost)
      } else {
        pmi <- NULL
      }
      
      month_expenses <- bind_rows(month1, must_pays, pmi) %>%
        mutate(month = month)
      
      all_expenses <- all_expenses %>%
        bind_rows(month_expenses)
      
      equity_sunk_mo <- data_frame(Equity = equity, 
                                   Costs = sum(month_expenses$cost)) %>%
        mutate(Month = month)
      
      equity_sunk <- equity_sunk %>%
        bind_rows(equity_sunk_mo)
      
    }
    
    list(equity_sunk = equity_sunk, all_expenses = all_expenses)
  })
  
  output$pay_plot <- renderPlot({
    
    print(head(data_list()$all_expenses))
    
    ggplot(data_list()$all_expenses %>% filter(month > 1), aes(x = month, y = cost, fill = expense)) + 
      geom_bar(stat = "identity", width = 1) +
      scale_x_continuous(breaks = seq(0, 12*as.numeric(input$years), by = 12)) 
  })
  
  output$info_table <- renderTable({
    
    
  })
  
  
}

shinyApp(
  ui = ui,
  server = server
)
