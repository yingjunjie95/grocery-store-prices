#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Data Resource: https://www.bls.gov/regions/mid-atlantic/data/averageretailfoodandenergyprices_usandwest_table.htm

library(shiny)
library(tidyverse)
library(spData) # For getting spatial data
library(sf) # For preserving spatial data
library(leaflet) # For making maps
library(DT) # For making fancy tables
library(readr)
average_prices <- read_csv("C:/Users/junji/OneDrive/Desktop/Marquette Subjects/COSC 5500/average_prices.csv")
View(average_prices)

#average_prices$item_name <- recode(average_prices$item_name,)

# Define UI for application that draws a histogram

ui <- navbarPage("Grocery Store Average Prices",
  tabPanel(
    "Introduction",
    titlePanel("Grocery Store Price Data"),
    img(src = "https://images.unsplash.com/photo-1604719312566-8912e9227c6a?ixlib=rb-4.0.3&ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&auto=format&fit=crop&w=1548&q=80", height = 750, width = 750),
    br(),br(),
    p("This is avearge price data for each types of food.")
  ),

  tabPanel("Plots",
    sidebar_content <- sidebarPanel(
      "Select conditions:",
      selectizeInput("itemInput", "Select Item:",
                     choices = unique(average_prices$item_name),
                     selected = "Flour, white, all purpose, per lb. (453.6 gm)", multiple = FALSE),
      selectizeInput("areaInput", "Select Area:",
                     choices = unique(average_prices$area_name),
                     selected = "U.S. city average", multiple = FALSE),
      selectizeInput("yearInput", "Select Year:",
                     choices = unique(average_prices$year),
                     selected = 1995, multiple = FALSE),
    ),
    
    mainPanel(
      plotOutput("averagepricebarplot"),
      p("The following is stastistic summary:"),
      verbatimTextOutput("stats"),
      p("The following is mean of values:"),
      verbatimTextOutput("mean"),
      p("The following is standard deviation value:"),
      verbatimTextOutput("sd"),
      p("The following is showing z-score values:"),
      verbatimTextOutput("zscore"),
      p("The following is showing z-score graphs:"),
      plotOutput("zscoreoutput"),
    )
  ),
  
  tabPanel("Average Price Tables",
      p("The following is average price table, you can
        put words in the searchbar", align = "center"),
      mainPanel(
        dataTableOutput("averagePriceTable")
      )
  ),
  
  tabPanel("Z-score image",
    img(src = "https://media.geeksforgeeks.org/wp-content/uploads/20210525140711/empiricalrule-660x388.png"),
    p("This is the example of z-score, you can check the Z-score table to check the probabilities
      corresponding to Z-score.")
  ),  
  
  tabPanel("Conclusions",
           img(src = "https://cdn.winsightmedia.com/platform/files/public/cspdn/800x420/kroger-gas-station-885.jpg"),
           p("The label of price shows average price for each month,
             from the label, you can see average and standard 
             deviation of annual price and compare different types
             of price per year."),
           p("Every time when people are seeing the trend of prices,
             they are going to pay attention to it and check whether
             they have membership or not."),
           ),
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  p <- reactive({
    average_prices %>%
      filter(item_name == input$itemInput,
             area_name == input$areaInput,
             year == input$yearInput)
  })
  
  output$averagepricebarplot <- renderPlot({
    
    validate (
      need(nrow(p()) > 0, "No price values found. Please make another selection.")
    )
    
    ggplot(p(), aes(x = period, y = value)) +
      geom_bar(stat='identity')+
      geom_text(aes(label=value),vjust=-0.3, size=3.5)+
      theme_bw()+
      xlab("Period")+
      ylab("Value")+
      ggtitle("Average prices for each month period")+
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$stats <- renderPrint({
    summary(p()[c('value')])
  })
  
  output$mean <- renderPrint({
    value_vector <- as.numeric(unlist(p()[c('value')]))
    mean(value_vector)
  })
  
  output$sd <- renderPrint({
    value_vector <- as.numeric(unlist(p()[c('value')]))
    sd(value_vector)
  })
  
  output$zscore <- renderPrint({
    value_vector <- as.numeric(unlist(p()[c('value')]))
    z_score <- (value_vector-mean(value_vector))/sd(value_vector)
    show(z_score)
  })
  
  output$zscoreoutput <- renderPlot({
    validate (
      need(nrow(p()) > 0, "No Z-scores values found. Please make another selection.")
    )
    
    value_vector <- as.numeric(unlist(p()[c('value')]))
    z_score <- (value_vector-mean(value_vector))/sd(value_vector)
    plot(z_score, type="o", col="blue", xlab = "Month", ylab = "Z-score")
  })
  
  output$averagePriceTable <- DT::renderDataTable({
    average_prices %>%
      select(year, period, value, item_name, area_name) %>%
      DT::datatable(p(), options = list(pageLength=12))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


