library(shiny)
library(ggplot2)
library(GGally)

ui <- navbarPage("Facebook Data Explorer",
                 tabPanel("Scatterplot Matrix",
                          fluidPage(
                            titlePanel('Scatterplot Matrix by Hour and Day of Week'),
                            mainPanel(
                              plotOutput("plot1"),
                              width = 12
                            ),
                            
                            fluidRow(
                              column(3,
                                     selectInput('colorby', 'Color by:',
                                                 c('Type' = 'Type', 'Category' = 'Category', 'Paid' = 'Paid'),
                                                 selected = 'Type')
                              ),
                              column(3, offset=1,
                                     selectInput('xaxis', 'X:',
                                                 c('Likes' = 'like', 'Comments' = 'comment',  
                                                   'Shares' = 'share', 'Interactions' = 'Total.Interactions'),
                                                 selected = 'Likes')
                              ),
                              column(3, offset=1,
                                     selectInput('yaxis', 'Y:',
                                                 c('Comments' = 'comment', 'Likes' = 'like', 
                                                   'Shares' = 'share', 'Interactions' = 'Total.Interactions'),
                                                 selected = 'Comments')
                              )
                            )
                          )
                        ),
                 tabPanel("Heatmap",
                          fluidPage(
                            titlePanel('Heatmap by Month and Day of Week'),
                            mainPanel(
                              plotOutput('plot2'),
                              width = 12
                            ),
                            fluidRow(
                              column(5,
                                     selectInput('colorby2', 'Color by:',
                                                 c('Engaged User Rate' = 'EngagedUserRate', 
                                                   'Conversion Rate' = 'ConversionRate',
                                                   'Consumption Rate' = 'ConsumptionRate',
                                                   'Number of People Reached' = 'Reach',
                                                   'Total Impressions' = 'Impressions',
                                                   'Number of Engaged Users' = 'EngagedUser',
                                                   'Number of Consumers' = 'Consumers',
                                                   'Number of Consumptions' = 'Consumptions'),
                                                 selected = 'Type')
                              )
                            )
                          )),
                 tabPanel("Parallel Coordinates",
                          fluidPage(
                            titlePanel('Parallel Coordinates Plot'),
                            mainPanel(
                              plotOutput('plot3'),
                              width = 12
                            ),
                            fluidRow(
                              column(4,
                                     selectInput('colorby3', 'Color by:',
                                                 c('Type' = 'Type', 
                                                   'Category' = 'Category',
                                                   'Month' = 'Month',
                                                   'Day of Week' = 'Weekday',
                                                   'Hour of Day' = 'Hour'),
                                                 selected = 'Type')
                              ),
                              column(4,
                                     radioButtons('scale', 'Y-Axis Scale:', c('Log', 'Linear'), 'Log')
                              )
                            )
                          ))
)

server <- function(input, output) {
  ## Import & process data for 1st tab
  data <- read.csv("data_clean.csv")
  data$Hour <- factor(data$Hour, ordered=TRUE, levels=c('0-5', '6-11', '12-17', '18-24'))
  data$Paid <- as.factor(data$Paid)
  data$Category <- as.factor(data$Category)
  
  ## Import & process data for 2nd tab
  data2 <- read.csv("data_agg.csv")
  data2$Month <- as.factor(data2$Month)
  data2$Weekday <- as.factor(data2$Weekday)
  
  ## Import & process data for 3rd tab
  data3 <- read.csv("data3.csv")
  
  ## Output of 1st tab
  output$plot1 <- renderPlot({
    ggplot(data, aes_string(x=input$xaxis, y=input$yaxis)) + geom_point(aes_string(color=input$colorby)) + 
      facet_grid(Weekday ~ Hour, labeller = label_both)
  })
  
  ## Output of 2nd tab
  output$plot2 <- renderPlot({
    ggplot(data2, aes(Month, Weekday)) + 
      geom_tile(aes_string(fill=input$colorby2)) + 
      scale_fill_gradient(low = "white", high = "steelblue")
  })
  
  ## Output of 3rd tab
  output$plot3 <- renderPlot({
    data3_2 <- data3
    if (input$scale == 'Log') {
      data3_2[, 6:10] <- log(data3[, 6:10])
    }
    ggparcoord(data3_2, columns = 6:10, groupColumn = input$colorby3, scale = 'uniminmax')
  })
}

shinyApp(ui = ui, server = server)