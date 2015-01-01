library(shiny)
library(ggplot2)
library(data.table)
library(maps)
library(rCharts)
library(reshape2)
library(markdown)
library(mapproj)
library(ggvis)
library(dplyr)

#' Aggregate dataset by state
#' 
#' @param dt data.table
#' @param year_min integer
#' @param year_max integer
#' @param evtypes character vector
#' @return data.table
#'
aggregate_by_state <- function(dt, year_min, year_max, evtypes) {
    replace_na <- function(x) ifelse(is.na(x), 0, x)
    round_2 <- function(x) round(x, 2)
    
    states <- data.table(STATE=sort(unique(dt$STATE)))
    
    aggregated <- dt %>% filter(YEAR >= year_min, YEAR <= year_max, EVTYPE %in% evtypes) %>%
            group_by(STATE) %>%
            summarise_each(funs(sum), COUNT:CROPDMG)

    # We want all states to be present even if nothing happened
    left_join(states,  aggregated, by = "STATE") %>%
        mutate_each(funs(replace_na), FATALITIES:CROPDMG) %>%
        mutate_each(funs(round_2), PROPDMG, CROPDMG)    
}

#' Aggregate dataset by year
#' 
#' @param dt data.table
#' @param year_min integer
#' @param year_max integer
#' @param evtypes character vector
#' @return data.table
#'
aggregate_by_year <- function(dt, year_min, year_max, evtypes) {
    round_2 <- function(x) round(x, 2)
    
    # Filter
    dt %>% filter(YEAR >= year_min, YEAR <= year_max, EVTYPE %in% evtypes) %>%
    # Group and aggregate
    group_by(YEAR) %>% summarise_each(funs(sum), COUNT:CROPDMG) %>%
    # Round
    mutate_each(funs(round_2), PROPDMG, CROPDMG)    
}

#' Add Affected column based on category
#'
#' @param dt data.table
#' @param category character
#' @return data.table
#'
compute_affected <- function(dt, category) {
    dt %>% mutate(Affected = {
        if(category == 'both') {
            INJURIES + FATALITIES
        } else if(category == 'fatalities') {
            FATALITIES
        } else {
            INJURIES
        }
    })
}

#' Add Damages column based on category
#' 
#' @param dt data.table
#' @param category character
#' @return data.table
#'
compute_damages <- function(dt, category) {
    dt %>% mutate(Damages = {
        if(category == 'both') {
            PROPDMG + CROPDMG
        } else if(category == 'crops') {
            CROPDMG
        } else {
            PROPDMG
        }
    })
}

#' Prepare map of population impact
#' 
#' @param dt data.table
#' @param states_map data.frame returned from map_data("state")
#' @param year_min integer
#' @param year_max integer
#' @return ggplot
#' 
plot_population_impact_by_state <- function (dt, states_map, year_min, year_max) {
    title <- paste("Population impact", year_min, "-", year_max, "(number of affected)")
    p <- ggplot(dt, aes(map_id = STATE))
    p <- p + geom_map(aes(fill = Affected), map = states_map, colour='black') + expand_limits(x = states_map$long, y = states_map$lat)
    p <- p + coord_map() + theme_bw()
    p <- p + labs(x = "Long", y = "Lat", title = title)
    p
}

#' Prepare map of economic impact
#' 
#' @param dt data.table
#' @param states_map data.frame returned from map_data("state")
#' @param year_min integer
#' @param year_max integer
#' @return ggplot
#' 
plot_economic_impact_by_state <- function (dt, states_map, year_min, year_max) {
    title <- paste("Economic impact", year_min, "-", year_max, "(Million USD)")
    p <- ggplot(dt, aes(map_id = STATE))
    p <- p + geom_map(aes(fill = Damages), map = states_map, colour='black') + expand_limits(x = states_map$long, y = states_map$lat)
    p <- p + coord_map() + theme_bw()
    p <- p + labs(x = "Long", y = "Lat", title = title)
    p
}

# Load data
states_map <- map_data("state")
dt <- fread('data/events.agg.csv') %>% mutate(EVTYPE = tolower(EVTYPE))
evtypes <<- sort(unique(dt$EVTYPE))


# Shiny server 
shinyServer(function(input, output, session) {
    values <- reactiveValues()
    values$evtypes <- evtypes

    
    # Prepare dataset for maps
    dt.agg <- reactive({
        aggregate_by_state(dt, input$range[1], input$range[2], input$evtypes)
    })
    
    # Prepare dataset for time series
    dt.agg.year <- reactive({
        aggregate_by_year(dt, input$range[1], input$range[2], input$evtypes)
    })
        
   
    output$populationImpactByState <- renderPlot({
        print(plot_population_impact_by_state (
            dt = compute_affected(dt.agg(), input$populationCategory),
            states_map = states_map, 
            year_min = input$range[1],
            year_max = input$range[2]
        ))
    })
    
    output$economicImpactByState <- renderPlot({
        print(plot_economic_impact_by_state(
            dt = compute_damages(dt.agg(), input$economicCategory),
            states_map = states_map, 
            year_min = input$range[1],
            year_max = input$range[2]
        ))
    })
    
    observe({
        if(input$clear_all == 0) return()
        values$evtypes <- c()
    })
    
    observe({
        if(input$select_all == 0) return()
        values$evtypes <- evtypes
    })
    
    output$evtypeControls <- renderUI({
        if(1) {
            checkboxGroupInput('evtypes', 'Event types', evtypes, selected=values$evtypes)
        }
    })
    
    dataTable <- reactive({
        dt.agg()[, list(
            State=state.abb[match(STATE, tolower(state.name))],
            Count=COUNT,
            Injuries=INJURIES,
            Fatalities=FATALITIES,
            Property.damage=PROPDMG,
            Crops.damage=CROPDMG)
        ]   
    })
    
    output$table <- renderDataTable(
        {dataTable()}, options = list(bFilter = FALSE, iDisplayLength = 50))
    
    output$eventsByYear <- renderChart({
        data <- dt.agg.year()[, list(COUNT=sum(COUNT)), by=list(YEAR)]
        setnames(data, c('YEAR', 'COUNT'), c("Year", "Count"))
 
        eventsByYear <- nPlot(
            Count ~ Year,
            data = data[order(data$Year)],
            type = "lineChart", dom = 'eventsByYear', width = 650
        )
        
        eventsByYear$chart(margin = list(left = 100))
        eventsByYear$yAxis( axisLabel = "Count", width = 80)
        eventsByYear$xAxis( axisLabel = "Year", width = 70)
        return(eventsByYear)
    })
    
    output$populationImpact <- renderChart({
        data <- melt(
            dt.agg.year()[, list(Year=YEAR, Injuries=INJURIES, Fatalities=FATALITIES)],
            id='Year'
        )
        populationImpact <- nPlot(
            value ~ Year, group = 'variable', data = data[order(-Year, variable, decreasing = T)],
            type = 'stackedAreaChart', dom = 'populationImpact', width = 650
        )
        
        populationImpact$chart(margin = list(left = 100))
        populationImpact$yAxis( axisLabel = "Affected", width = 80)
        populationImpact$xAxis( axisLabel = "Year", width = 70)
        
        return(populationImpact)
    })
    
    output$economicImpact <- renderChart({
        data <- melt(
            dt.agg.year()[, list(Year=YEAR, Propety=PROPDMG, Crops=CROPDMG)],
            id='Year'
        )
        economicImpact <- nPlot(
            value ~ Year, group = 'variable', data = data[order(-Year, variable, decreasing = T)],
            type = 'stackedAreaChart', dom = 'economicImpact', width = 650
        )
        economicImpact$chart(margin = list(left = 100))
        economicImpact$yAxis( axisLabel = "Total damage (Million USD)", width = 80)
        economicImpact$xAxis( axisLabel = "Year", width = 70)
        
        return(economicImpact)
    })
  
    output$downloadData <- downloadHandler(
        filename = 'data.csv',
        content = function(file) {
            write.csv(dataTable(), file, row.names=FALSE)
        }
    )
})


