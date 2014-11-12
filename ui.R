
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(rCharts)

shinyUI(
    navbarPage("Storm Database Explorer",
        tabPanel("Plot",
                sidebarPanel(
                    sliderInput("range", 
                        "Range:", 
                        min = 1950, 
                        max = 2011, 
                        value = c(1993, 2011),
                        format="####"),
                    uiOutput("evtypeControls")
                ),
  
                mainPanel(
                    tabsetPanel(
                        
                        # Data by state
                        tabPanel('By state',
                            column(3,
                                wellPanel(
                                    radioButtons(
                                        "populationCategory",
                                        "Population impact category:",
                                        c("Both" = "both", "Injuries" = "injuries", "Fatalities" = "fatalities"))
                                )
                            ),
                            column(3,
                                wellPanel(
                                    radioButtons(
                                        "economicCategory",
                                        "Economic impact category:",
                                        c("Both" = "both", "Property damage" = "property", "Crops damage" = "crops"))
                                )
                            ),
                            column(7,
                                plotOutput("populationImpactByState"),
                                plotOutput("economicImpactByState")
                            )

                        ),
                        
                        # Time series data
                        tabPanel('By year',
                                 h4('Number of events by year', align = "center"),
                                 showOutput("eventsByYear", "nvd3"),
                                 h4('Population impact by year', align = "center"),
                                 showOutput("populationImpact", "nvd3"),
                                 h4('Economic impact by year', align = "center"),
                                 showOutput("economicImpact", "nvd3")
                        ),
                        

                        
                        # Data 
                        tabPanel('Data',
                            dataTableOutput(outputId="table"),
                            downloadButton('downloadData', 'Download')
                        )
                    )
                )
            
        ),
        
        tabPanel("About",
            mainPanel(
                includeMarkdown("include.md")
            )
        )
    )
)
