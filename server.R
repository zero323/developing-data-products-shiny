library(shiny)

# Plotting 
library(ggplot2)
library(rCharts)
library(ggvis)

# Data processing libraries
library(data.table)
library(reshape2)
library(dplyr)

# Required by includeMarkdown
library(markdown)

# It has to loaded to plot ggplot maps on shinyapps.io
library(mapproj)
library(maps)

# Load helper functions
source("helpers.R", local = TRUE)


# Load data
states_map <- map_data("state")
dt <- fread('data/events.agg.csv') %>% mutate(EVTYPE = tolower(EVTYPE))
evtypes <- sort(unique(dt$EVTYPE))


# Shiny server 
shinyServer(function(input, output, session) {
    
    # Define and initialize reactive values
    values <- reactiveValues()
    values$evtypes <- evtypes
    
    # Create event type checkbox
    output$evtypeControls <- renderUI({
        checkboxGroupInput('evtypes', 'Event types', evtypes, selected=values$evtypes)
    })
    
    # Add observers on clear and select all buttons
    observe({
        if(input$clear_all == 0) return()
        values$evtypes <- c()
    })
    
    observe({
        if(input$select_all == 0) return()
        values$evtypes <- evtypes
    })

    # Preapre datasets
    
    # Prepare dataset for maps
    dt.agg <- reactive({
        aggregate_by_state(dt, input$range[1], input$range[2], input$evtypes)
    })
    
    # Prepare dataset for time series
    dt.agg.year <- reactive({
        aggregate_by_year(dt, input$range[1], input$range[2], input$evtypes)
    })
    
    # Prepare dataset for downloads
    dataTable <- reactive({
        prepare_downolads(dt.agg())
    })
    
    # Render Plots
    
    # Population impact by state
    output$populationImpactByState <- renderPlot({
        print(plot_impact_by_state (
            dt = compute_affected(dt.agg(), input$populationCategory),
            states_map = states_map, 
            year_min = input$range[1],
            year_max = input$range[2],
            title = "Population impact %d - %d (number of affected)",
            fill = "Affected"
        ))
    })
    
    # Economic impact by state
    output$economicImpactByState <- renderPlot({
        print(plot_impact_by_state(
            dt = compute_damages(dt.agg(), input$economicCategory),
            states_map = states_map, 
            year_min = input$range[1],
            year_max = input$range[2],
            title = "Economic impact %d - %d (Million USD)",
            fill = "Damages"
        ))
    })
    
    # Events by year
    output$eventsByYear <- renderChart({
       plot_events_by_year(dt.agg.year())
    })
    
    # Population impact by year
    output$populationImpact <- renderChart({
        plot_impact_by_year(
            dt = dt.agg.year() %>% select(Year, Injuries, Fatalities),
            dom = "populationImpact",
            yAxisLabel = "Affected",
            desc = TRUE
        )
    })
    
    # Economic impact by state
    output$economicImpact <- renderChart({
        plot_impact_by_year(
            dt = dt.agg.year() %>% select(Year, Crops, Property),
            dom = "economicImpact",
            yAxisLabel = "Total damage (Million USD)"
        )
    })
    
    # Render data table and create download handler
    output$table <- renderDataTable(
        {dataTable()}, options = list(bFilter = FALSE, iDisplayLength = 50))
    
    output$downloadData <- downloadHandler(
        filename = 'data.csv',
        content = function(file) {
            write.csv(dataTable(), file, row.names=FALSE)
        }
    )
})


