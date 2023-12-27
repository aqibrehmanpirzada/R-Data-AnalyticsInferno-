library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(readxl)


# Assuming you have loaded your datasets
superstore_data <- read_excel("SuperStore_SalesData_8Aug2020.xlsx")
sales_sample_data <- read.csv("Sales_Sample.csv")
#Sample data for illustration purposes
superstore_data <- data.frame(
  Region = sales_sample_data$Region,
  Sales = sales_sample_data$Sales
)

sales_sample_data <- data.frame(
  SalesRep = superstore_data$Region,
  QTR = sales_sample_data$QTR,
  Sales = sales_sample_data$Sales,
  Units_Sold = sales_sample_data$Units_Sold
)

#
# # Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Sales Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview"),
      menuItem("Plots", tabName = "plots")
    )
  ),
  dashboardBody(
    tabItems(
      # Overview Tab
      tabItem(
        tabName = "overview",
        fluidRow(
          box(
            title = "Sales Summary",
            status = "info",
            solidHeader = TRUE,
            plotOutput("salesSummaryPlot")
          )
        ),
        fluidRow(
          box(
            title = "Select Region",
            status = "primary",
            solidHeader = TRUE,
            selectInput("selectedRegion", "Region:", choices = unique(superstore_data$Region))
          )
        ),
        fluidRow(
          box(
            title = "Selected Region Summary",
            status = "success",
            solidHeader = TRUE,
            textOutput("selectedRegionSummary")
          )
        )
      ),
      # Plots Tab
      tabItem(
        tabName = "plots",
        fluidRow(
          box(
            title = "Bar Plot",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("barPlot")
          ),
          box(
            title = "Heatmap",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("heatmapPlot")
          )
        ),
        fluidRow(
          box(
            title = "Line Plot",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("linePlot")
          ),
          box(
            title = "Pie Chart",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("pieChart")
          )
        ),
        fluidRow(
          box(
            title = "Dynamic Dropdown",
            status = "primary",
            solidHeader = TRUE,
            selectInput("dynamicDropdown", "Sales Rep:", choices = unique(sales_sample_data$SalesRep))
          )
        ),
        fluidRow(
          box(
            title = "Slider Example",
            status = "primary",
            solidHeader = TRUE,
            sliderInput("sliderExample", "Select a value:", min = 1, max = 100, value = 50)
          )
        )
      )
    )
  )
)

# Define server
server <- function(input, output) {
  # Sales Summary Plot
  output$salesSummaryPlot <- renderPlot({
    ggplot(superstore_data, aes(x = Region, y = Sales)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(title = "Sales Summary", x = "Region", y = "Sales")
  })

  # Selected Region Summary Text
  output$selectedRegionSummary <- renderText({
    selected_region <- input$selectedRegion
    selected_data <- superstore_data[superstore_data$Region == selected_region, ]
    paste("Total Sales in", selected_region, ":", sum(selected_data$Sales))
  })

  # Bar Plot
  output$barPlot <- renderPlot({
    ggplot(superstore_data, aes(x = Region, y = Sales)) +
      geom_bar(stat = "identity", fill = "coral") +
      labs(title = "Bar Plot", x = "Region", y = "Sales")
  })

  # Heatmap Plot
  output$heatmapPlot <- renderPlot({
    ggplot(superstore_data, aes(x = Region, y = "Sales", fill = Sales)) +
      geom_tile() +
      labs(title = "Heatmap", x = "", y = "") +
      scale_fill_gradient(low = "lightblue", high = "darkblue")
  })

  # Line Plot
  output$linePlot <- renderPlot({
    ggplot(superstore_data, aes(x = Region, y = Sales, group = 1)) +
      geom_line(color = "orange") +
      labs(title = "Line Plot", x = "Region", y = "Sales")
  })

  # Pie Chart
  output$pieChart <- renderPlot({
    ggplot(superstore_data, aes(x = "", y = Sales, fill = Region)) +
      geom_bar(stat = "identity", width = 1) +
      labs(title = "Pie Chart", x = "", y = "") +
      theme_void() +
      scale_fill_brewer(palette = "Set3") +
      coord_polar(theta = "y")
  })
  
}

# Run the application
shinyApp(ui, server)
