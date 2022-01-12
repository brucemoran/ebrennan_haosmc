#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Gene Abundance by Group and Treatment"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "group",
                        label = "Group",
                        choices = c("Scramble", "Let7d"),
                        selected = "Scramble"),
            selectInput(inputId = "drug",
                        label = "Drug",
                        choices = c("None", "TNFa", "Ator", "Lova"),
                        selected = "None"),
            shinyWidgets::searchInput(inputId = "gene",
                      label = "Gene") ##input$<INPUTID>_search
        ),
        
        mainPanel(
        tabsetPanel(
            tabPanel("Plot", plotOutput("plotOut")
                     )
            ),
            tabPanel("Table", DT::dataTableOutput("genetable"))
        ),
    )
))
