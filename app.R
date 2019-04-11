### Libraries ###
library(shiny)
library(ggplot2)
library(gridExtra)
library(stringr)

source("PrepData.R")

ui <- fluidPage(
  titlePanel("US Food and Alcohol Purchases"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("Food.Alc",
                   label = "Food/Alcohol",
                   choices = list("Food", "Alcohol"),
                   selected = "Food"),
      radioButtons("Home.Away",
                   label = "Consumed At Home/Away from Home",
                   choices = list("Home", "Away from Home", "Both"),
                   selected = "Home"),
      selectInput("Locations",
                  label = "Data from what locations?",
                  choices = foodAtHomeChoices,
                  multiple = TRUE,
                  selected = NULL),
      radioButtons("ShowAs",
                   label = "Show as",
                   choices = list("Sales", "Percentage of Total", "Per Capita"),
                   selected = "Sales"),
      checkboxInput("Tax.Tip",
                   label = "Include taxes and tips?"),
      checkboxInput("Tax.Tip.Separate",
                    label = "Show taxes and tips on another graph?")
    ),
    
    mainPanel(plotOutput("plot", height = "500"))
))
  
server <- function(input, output, session) {
  
  active <- reactive({
    if (input$Food.Alc == "Food") {
      type <- "food"
    } else if (input$Food.Alc == "Alcohol") {
      type <- "alcohol"
    }
    if (input$Home.Away == "Home") {
      home <- "home"
    } else if (input$Home.Away == "Away from Home") {
      home <- "away"
    } else if (input$Home.Away == "Both") {
      home <- "both"
    }
    
    if (type == "food" & home == "home") {
      updateSelectInput(session, "Locations", choices = foodAtHomeChoices)
      active <- foodAtHome
    } else if (type == "food" & home == "away") {
      updateSelectInput(session, "Locations", choices = foodAwayFromHomeChoices)
      active <- foodAwayFromHome
    } else if (type == "food" & home == "both") {
      updateSelectInput(session, "Locations", choices = bothFoodChoices)
      active <- bothFood
    } else if (type == "alcohol" & home == "home") {
      updateSelectInput(session, "Locations", choices = alcoholAtHomeChoices)
      active <- alcoholAtHome
    } else if (type == "alcohol" & home == "away") {
      updateSelectInput(session, "Locations", choices = alcoholAwayFromHomeChoices)
      active <- alcoholAwayFromHome
    } else if (type == "alcohol" & home == "both") {
      updateSelectInput(session, "Locations", choices = bothAlcoholChoices)
      active <- bothAlcohol
    }
    return(active)
  })
  
  output$plot <- renderPlot({
    active <- active()
    totals <- active %>% filter(Location == "Total")
    if (input$Tax.Tip & input$ShowAs == "Sales") {
      yvar = "SalesWithTaxTip"
      figTitle = "Sales in Millions of 2017 Dollars"
      yTitle = "Sales (Millions USD)"
      ttTitle = "Tips and Taxes in Millions of 2017 Dollars"
      tty = "Tips.Taxes"
    } else if(input$Tax.Tip & input$ShowAs == "Percentage of Total") {
      yvar = "PercentTotal"
      figTitle = "Percentage of Total Food and Alcohol Expenditures"
      yTitle = "Percentage of Expenditures"
      ttTitle = "Percentage of Total Food/Alcohol Tips and Taxes"
      tty = "PercentTipsTaxes"
    } else if(input$Tax.Tip & input$ShowAs == "Per Capita") {
      yvar = "PerCapitaTotal"
      figTitle = "Per Capita Sales"
      yTitle = "Per Capita Sales (USD)"
      ttTitle = "Per Capita Food/Alcohol Tips and Taxes (USD)"
      tty = "PerCapitaTipsTaxes"
    } else if(input$ShowAs == "Percentage of Total") {
      yvar = "PercentSales"
      figTitle = "Percentage of Total Food and Alcohol Expenditures"
      yTitle = "Percentage of Expenditures"
      ttTitle = "Percentage of Total Food/Alcohol Tips and Taxes"
      tty = "PercentTipsTaxes"
    } else if(input$ShowAs == "Per Capita") {
      yvar = "PerCapitaSales"
      figTitle = "Per Capita Sales"
      yTitle = "Per Capita Sales (USD)"
      ttTitle = "Per Capita Food/Alcohol Tips and Taxes (USD)"
      tty = "PerCapitaTipsTaxes"
    } else {
      yvar = "Sales"
      figTitle = "Sales in Millions of 2017 Dollars"
      yTitle = "Sales (Millions USD)"
      ttTitle = "Tips and Taxes in Millions of 2017 Dollars"
      tty = "Tips.Taxes"
    }
    
    totalsPlot <- ggplot(totals, aes(x = Year, y = get(yvar))) +
      geom_point(size = 5, shape = "square") +
      labs(title = figTitle,
           x = "Year",
           y = yTitle)
    
    if (!is.null(input$Locations)) {
      active <- active %>% filter(Location %in% input$Locations)
      specificPlot <- ggplot(active, aes(x = Year, y = get(yvar), color = Location)) +
        geom_point(size = 5, shape = "square") + 
        labs(title = figTitle,
             x = "Year",
             y = yTitle)
    } else { specificPlot <- NULL }
    
    if (input$Tax.Tip.Separate & !is.null(specificPlot)) {
      taxTipPlot <- ggplot(active, aes(x = Year, y = get(tty), color = Location)) +
        geom_point(size = 5, shape = "square") +
        labs(title = ttTitle,
             x = "Year",
             y = yTitle)
    } else if (input$Tax.Tip.Separate) {
      taxTipPlot <- ggplot(totals, aes(x = Year, y = get(tty))) +
        geom_point(size = 5, shape = "square") +
        labs(title = ttTitle,
             x = "Year",
             y = yTitle)
    } else { taxTipPlot <- NULL }
    
    if (!is.null(specificPlot) & ! is.null(taxTipPlot)) {
      grid.arrange(specificPlot, taxTipPlot)
    } else if (!is.null(specificPlot)) {
      grid.arrange(specificPlot)
    } else if (!is.null(taxTipPlot)) {
      grid.arrange(totalsPlot, taxTipPlot)
    } else {
      grid.arrange(totalsPlot)
    }
  })
}

shinyApp(ui, server)
