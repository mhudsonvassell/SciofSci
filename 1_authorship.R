#todo
# Load the plotly package
library(plotly)
library(ggplot2)
library(tidyverse)
library(openxlsx)

prop1_age <- read.xlsx("https://raw.github.com/mhudsonvassell/SciofSci/main/prop1_age.xlsx")
prop1_age <- prop1_age %>%
  mutate_at(c("sex","age","race","subject_lvl1"),as.factor) %>%
  mutate(across(c("pct","repdiff"),round,2))

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("age", "Author age range",
                  choices = c("",levels(prop1_age$age)),
                  selected = NULL),
      selectInput("race", "Author race",
                  choices = c("",levels(prop1_age$race)),
                  selected = NULL),
      selectInput("gender", "Author gender",
                  choices = c("",levels(prop1_age$sex)),
                  selected = NULL),
      selectInput("subject", "Subject",
                  choices = c("",levels(prop1_age$subject_lvl1)),
                  selected = NULL),
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
      if(input$subject!=""){data <- data %>% filter(subject_lvl1 %in% input$subject)}
      ggplot(data, aes(year, totcount, color = subject_lvl1)) +
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
      if(input$subject!=""){data <- data %>% filter(subject_lvl1 %in% input$subject)}
      ggplot(data, aes(year, pct, color = subject_lvl1,
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
            if(input$subject!=""){data <- data %>% filter(subject_lvl1 %in% input$subject)}
      
      ggplot(data, aes(year, repdiff, color = subject_lvl1,
                       text = paste0("subject count: ", totcount))) +
        geom_point(position="jitter", show.legend = FALSE, alpha=(1/3)) +
        theme(legend.position='none') +
        ggtitle(paste0("Over/under-representation by Year (Group: ",input$race," ",input$gender," ",input$age,")"))
    })
  })
  # Replace the `renderPlot()` with the plotly version
  output$plot4 <- renderPlotly({
    # Convert the existing ggplot2 to a plotly plot
    ggplotly({
      data <- subset(prop1_age, subject_lvl1 %in% input$subject & 
                       year >= input$years[1] & year <= input$years[2])
      if(input$race!=""){data <- data %>% filter(race %in% input$race)}
      if(input$gender!=""){data <- data %>% filter(sex %in% input$gender)}
      if(input$age!=""){data <- data %>% filter(age %in% input$age)}
      ggplot(data, aes(x = year, y=lvl1count, color=race,
                       text = paste0("subgroup: ", race_gender_age))) +
        geom_point(position="jitter", show.legend = FALSE, alpha=(1/3)) +
        theme(legend.position='none') +
        ggtitle(paste0("# of Authors by Subject and Year (Subject: ",input$subject,")"))
    })
  })
}

shinyApp(ui = ui, server = server)
