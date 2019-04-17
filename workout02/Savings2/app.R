#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(h4("Annual Savings"), 
                fluidRow(
                  column(4, 
                         sliderInput(inputId = "amount",
                                     label = "Initial Amount",
                                     value = 1000, min = 0, max = 100000,
                                     step = 500)
                  ),
                  column(4,
                         sliderInput(inputId = "rate",
                                     label = "Return Rate (in%)",
                                     value = 5, min = 0, max = 20,
                                     step = 0.1)
                  ),
                  column(4,
                         sliderInput(inputId = "years",
                                     label = "Years",
                                     value = 10, min = 0, max = 50,
                                     step = 1)
                  ),
                  column(4, 
                         sliderInput(inputId = "contrib",
                                     label = "Annual Contribution",
                                     value = 2000, min = 0, max = 50000,
                                     step = 500)
                  ),
                  column(4,
                         sliderInput(inputId = "growth",
                                     label = "Growth Rate (in%)",
                                     value = 2, min = 0, max = 20,
                                     step = 0.1)
                  ),
                  column(4,
                         selectInput(inputId = "face",
                                     label = "Facet?",
                                     c("no" = "no", "yes" = "yes"))
                         
                  )),
                h4("Timelines"),
                plotOutput("timeline"),
                h4("Balances"),
                tableOutput("Balances")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  library(ggplot2)
  library(tidyr)
  library(reshape2)
   
  future_value <- function(amount, rate, years){
    #rate = rate/100
    FV = amount*(1 + rate)^years
  }
  
  ## Savings with Annuity
  
  
  annuity <- function(contrib, rate, years){
    #rate = rate/100
    FV <- contrib*((1+rate)^years - 1)/rate
    
  }
  
  ## 3) Future Value of Growing Annuity
  
  growing_annuity <- function(contrib, rate, growth, years){
    # rate = rate/100
    #growth = growth/100
    FVGA = contrib*((1+rate)^years - (1+growth)^years)/(rate - growth)
    
  }
  
  ### 4) Data into Data Frame
  
  
  Balances <- function(amount, contrib, rate, growth, years) {
    
    modalities <- as.data.frame(matrix(c(0:years), nrow = years + 1, ncol = 4))
    
    z <- c('years', 'no_contrib', 'fixed_contrib', 'growing_contrib')
    colnames(modalities) = z
    
    for (i in 1:10){
      modalities$no_contrib[1] <- future_value(amount, rate, 0)
      modalities$no_contrib[i+1] <- future_value(amount, rate, i)
      
      modalities$fixed_contrib[1] <- modalities$no_contrib[1] 
      modalities$fixed_contrib[i+1] <- annuity(contrib, rate, i) + modalities$no_contrib[i+1] 
      
      modalities$growing_contrib[1] <- modalities$no_contrib[1]
      modalities$growing_contrib[i+1] <- modalities$no_contrib[i+1] + growing_annuity(contrib, rate, growth, i)
    }
    
    years <- modalities$years
    no_contrib <- modalities$no_contrib
    fixed_contrib <- modalities$fixed_contrib
    growing_contrib <- modalities$growing_contrib
    
    return(modalities)
  }
  
  Balances <- reactive({ 
    
    
    ##################################
    ## Shiny App Command 
    
    Balances <- savings_df(input$amount, input$contrib, input$rate/100,
                           input$growth/100, input$years)
    
    
    
    
    return(Balances[c(1,2,3,5)])
    #return(Balances[c(1,2,3,5)])
    
  })
  
  Balances2 <- reactive({
    Balances2 <- melt(Balances(),id.vars = "years",
                      measure.vals = c("no_contrib", "fixed_contrib",
                                       "growing_contrib"))
    return(Balances2)
  })
  output$timeline <- renderPlot({
    
    if (input$face == "yes"){
      
      ggplot(Balances2(), aes(x = 'years', y = "value", xlab = "year")) +
        geom_line(aes(Balances2()$years, Balances2()$value, col = variable)) +
        geom_point(aes(Balances2()$years, Balances2()$value, col = variable)) +
        geom_area(aes(Balances2()$years, Balances2()$value, fill = variable)) +
        facet_grid(~variable) +
        ggtitle("Three Modes of Investing")
      
    }
    else {
      
      ggplot(Balances(), aes(x = 'year', y = "balance", xlab = "year")) +
        geom_line(aes(Balances()$years, Balances()$no_contrib, col = "no_contrib")) +
        geom_point(aes(Balances()$years, Balances()$no_contrib, col = "no_contrib")) +
        geom_point(aes(Balances()$years, Balances()$fixed_contrib, col = "fixed_contrib")) +
        geom_line(aes(Balances()$years, Balances()$fixed_contrib, col = "fixed_contrib")) +
        geom_point(aes(Balances()$years, Balances()$growing_contrib, col = "growing_contrib")) +
        geom_line(aes(Balances()$years, Balances()$growing_contrib, col = "growing_contrib")) +
        ggtitle("Three Modes of Investing")
      
    }
    
  })
  
  output$Balances <- renderTable({
    
    head(Balances())
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

