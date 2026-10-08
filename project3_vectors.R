# Project 3: Vectors - Weekly Step Tracker
# Setup once: install.packages("shiny")   |   Run: click "Run App" in RStudio
library(shiny)

ui <- fluidPage(
  titlePanel("Weekly Step Tracker (Vectors)"),
  sidebarLayout(
    sidebarPanel(
      textInput("steps", "7 daily steps (Mon-Sun, comma separated)",
                "4200,8500,12000,6700,9800,15000,3000"),
      numericInput("goal", "Daily goal", 8000)
    ),
    mainPanel(plotOutput("splot"), verbatimTextOutput("stext"))
  )
)

server <- function(input, output, session) {
  steps <- reactive({
    v <- suppressWarnings(as.numeric(strsplit(input$steps, ",")[[1]]))
    validate(need(length(v) == 7 && !anyNA(v), "Enter exactly 7 numbers"))
    names(v) <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    v
  })

  output$splot <- renderPlot({
    v <- steps()
    barplot(v, col = ifelse(v >= input$goal, "seagreen", "tomato"),
            main = "Steps per day (green = goal met)")
    abline(h = input$goal, lty = 2)
  })

  output$stext <- renderText({
    v <- steps()
    paste0("Total steps: ", sum(v),
           "\nAverage per day: ", round(mean(v)),
           "\nBest day: ", names(v)[which.max(v)],
           "\nLaziest day: ", names(v)[which.min(v)],
           "\nGoal days: ", paste(names(v)[v >= input$goal], collapse = ", "),
           "\nRunning total: ", paste(cumsum(v), collapse = ", "),
           "\nSorted (high to low): ", paste(sort(v, decreasing = TRUE), collapse = ", "))
  })
}

shinyApp(ui, server)
