library(shiny)
library(shinydashboard)
library(tidyverse)

my_apps <- read_csv('apps_final_2.csv')

quality <- read_csv("quality_csv.csv")

df <- quality %>%
  pivot_longer(-X1, names_to = "intervention", values_to = "scores")

df %>% pivot_wider(names_from = X1, values_from = scores) %>% 
  filter(`Q14: Attempt to blind participants?` == 1 & `Q15: Attempt to blind assessors?` == 1) %>% select(intervention)


df %>% pivot_wider(names_from = X1, values_from = scores) %>% 
  filter(`Q27: Power calculation?` == 1) %>% select(intervention)

inner_join(df, my_apps, by = "intervention") %>% group_by(intervention) %>% summarise(scores = sum(scores)) 

blinded <- df %>% group_by(intervention) %>% filter(X1 == "Q15: Attempt to blind assessors?" & scores == "1") %>% select(intervention)

ui <- dashboardPage(
  
  dashboardHeader(title = "User-Led Psychosis App Repository", titleWidth = 450),
  dashboardSidebar(
    conditionalPanel(
      'input.dataset === "my_apps"',
      checkboxGroupInput("show_vars", "Columns in Sys Review data to show:",
                         names(my_apps), selected = names(my_apps), ),
      downloadButton("downloadData", "Download"),
      helpText("Click what columns you're interested in, if you like you can download your choices as a .csv")
    ),
    #downloadButton("downloadData", "Download"),
    conditionalPanel(
      'input.dataset === "quality"',
      helpText("Quality of Theory Reporting")
    ),
    conditionalPanel(
      'input.dataset === "blinded"',
      checkboxGroupInput("show_vars", "Columns in Sys Review data to show:",
                         names(blinded), selected = names(blinded)),
      helpText("Display 5 records by default.")
    )
  ),
  dashboardBody(
    tabsetPanel(
      id = 'dataset',
      tabPanel("my_apps", div(style = 'overflow-x: scroll', DT::dataTableOutput("mytable1"))),             
      tabPanel("blinded assessors",  DT::dataTableOutput("mytable2"))
    )
  )
)

server <- function(input, output) {
  
  # choose columns to display
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(my_apps[, input$show_vars, drop = FALSE], list(pageLength = 33, info = FALSE, lengthMenu = list(c(32, -1), c("32", "All"))))
    #DT::datatable(my_apps, options = list(lengthMenu = c(5,30,50), pageLength = 35))
    
  })
  
  # sorted columns are colored now because CSS are attached to them
  output$mytable2 <- DT::renderDataTable({
    #        DT::datatable(blinded, options = list(orderClasses = TRUE))
    DT::datatable(blinded, options = list(lengthMenu = c(5, 30, 50), pageLength = 35))
  })
  
  # customize the length drop-down menu; display 5 rows per page by default
  output$mytable3 <- DT::renderDataTable({
    DT::datatable(iris, options = list(lengthMenu = c(5, 30, 50), pageLength = 35))
    
  })
  
  datasetInput <- reactive({
    my_apps[, input$show_vars]
  })
#  datasetInput <- reactive({
#    blinded[, input$show_vars]
#  })
#  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("your_choices", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  

}

shinyApp(ui, server)