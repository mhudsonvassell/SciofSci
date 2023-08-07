#todo
# Load the plotly package
library(plotly)
library(ggplot2)
library(tidyverse)
library(readxl)

setwd("C:/Users/musik/Downloads/Berry Postdoc/SciofSci")
prop1_age <- read_excel("prop1_age.xlsx")


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("age", "Author age range",
                  choices = levels(prop1_age$age),
                  selected = "20-25"),
      selectInput("race", "Author race",
                  choices = levels(prop1_age$race),
                  selected = "Black African"),
      selectInput("gender", "Author gender",
                  choices = levels(prop1_age$sex),
                  selected = "Female"),
      selectInput("subject", "Subject",
                  choices = levels(prop1_age$subject_lvl1),
                  selected = "{Natural sciences}"),
      sliderInput("years", "For which years?",
                  min(prop1_age$year), max(prop1_age$year),
                  value = c(2004, 2020),round=TRUE,sep="")
    ),
    mainPanel(
      # Output: Tabset w/ plots by outcomes ----
      tabsetPanel(type = "tabs",
                  tabPanel("Group Counts", plotlyOutput("plot1")),
                  tabPanel("Percentages", plotlyOutput("plot2")),
                  tabPanel("Over/under-representation", plotlyOutput("plot3")),
                  tabPanel("Subject Counts", plotlyOutput("plot4"))
      )
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Replace the `renderPlot()` with the plotly version
  output$plot1 <- renderPlotly({
    # Convert the existing ggplot2 to a plotly plot
    ggplotly({
      data <- subset(prop1_age,
                     race %in% input$race &
                       sex %in% input$gender &
                       age %in% input$age &
                       year >= input$years[1] & year <= input$years[2])
      
      ggplot(data, aes(year, totcount)) +
        geom_point(position="jitter", show.legend = FALSE, alpha=(1/3)) +
          theme(legend.position='none') +
        ggtitle(paste0("# of Authors by Year ","(Group: ",input$race," ",input$gender," ",input$age,")"))
    })
  })
  # Replace the `renderPlot()` with the plotly version
  output$plot2 <- renderPlotly({
    # Convert the existing ggplot2 to a plotly plot
    ggplotly({
      data <- subset(prop1_age,
                     race %in% input$race &
                       sex %in% input$gender &
                       age %in% input$age &
                       year >= input$years[1] & year <= input$years[2])
      
      ggplot(data, aes(year, pct, 
                       text = paste0("subject count: ", totcount))) +
        geom_point(position="jitter", show.legend = FALSE, alpha=(1/3)) +
        theme(legend.position='none') +
        ggtitle(paste0("% of Authors by Year ","(Group: ",input$race," ",input$gender," ",input$age,")"))
    })
  })
  # Replace the `renderPlot()` with the plotly version
  output$plot3 <- renderPlotly({
    # Convert the existing ggplot2 to a plotly plot
    ggplotly({
      data <- subset(prop1_age,
                     race %in% input$race &
                       sex %in% input$gender &
                       age %in% input$age &
                       year >= input$years[1] & year <= input$years[2])
      
      ggplot(data, aes(year, repdiff, 
                       text = paste0("subject count: ", repdiff))) +
        geom_point(position="jitter", show.legend = FALSE, alpha=(1/3)) +
        theme(legend.position='none') +
        ggtitle(paste0("Over/under-representation by Year (Group: ",input$race," ",input$gender," ",input$age,")"))
    })
  })
  # Replace the `renderPlot()` with the plotly version
  output$plot4 <- renderPlotly({
    # Convert the existing ggplot2 to a plotly plot
    ggplotly({
      data <- subset(prop1_age,
                     subject_lvl1 %in% input$subject & 
                       year >= input$years[1] & year <= input$years[2])
      
      ggplot(data, aes(x = year, y=lvl1count, color=race,
                       text = paste0("subgroup:", race_gender_age))) +
        geom_point(position="jitter", show.legend = FALSE, alpha=(1/3)) +
        theme(legend.position='none') +
        ggtitle(paste0("# of Authors by Subject and Year (Subject: ",input$subject,")"))
    })
  })
}

shinyApp(ui = ui, server = server)
