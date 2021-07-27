#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/ 
#
library(tidyverse)
library(DT)
library(shiny)
source("utils.R")
setup_workspace()

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("ICMPC16/ESCOM11 Progamme"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "filter_name", 
                        label = "Author",
                        choices = c("All", sort(unique(master$full_name))), selected = "All",
                        multiple = F, selectize = T),
            selectInput(inputId = "first_author", 
                        label = "First Author",
                        choices = c("All", sort(unique(master$first_author))), selected = "All",
                        multiple = F, selectize = T),
            selectInput(inputId = "theme_cleaned", 
                        label = "Theme",
                        choices = c("All", sort(unique(master$theme_cleaned))), selected = "All",
                        multiple = F, selectize = T),
            selectInput(inputId = "slot_type", 
                        label = "Slot Type",
                        choices = c("All", sort(unique(master$slot_type_long))), selected = "All",
                        multiple = F, selectize = T),
            selectInput(inputId = "day", 
                        label = "Day",
                        choices = c("All", unique(master$real_day)), selected = "All",
                        multiple = F, selectize = T),
            selectInput(inputId = "hub", 
                        label = "Time Hub",
                        choices = c("All", unique(master$hub)), selected = "All",
                        multiple = F, selectize = T),
            selectInput(inputId = "time_zone", 
                        label = "Time ",
                        choices = tz, selected = "time_utc",
                        multiple = F, selectize = T),
            p(
                "ICMPC/ESCOM 2021 programme browser v0.2", 
                shiny::tags$br(), 
                shiny::tags$br(), 
                "Author: Klaus Frieler, Max Planck Institute for Empirical Aesthetics, Frankfurt/M, Germany",
                shiny::tags$br(), 
                shiny::tags$br(),
                "Data provided by Michael Weiss", 
                shiny::tags$br(),
                shiny::tags$br(), 
                "Powered by the Deutsche Gesellschaft für Musikspsychologie",
                shiny::tags$br(),
                shiny::tags$br(), 
                "Hint: All drop boxes have incremental search functions. Use also the table search.",
                shiny::tags$br(),
                shiny::tags$br(), 
                "Stay tuned for further updates. Have fun!",
                style = "font-size: 10pt; display: block"
            ),
            width = 2
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            DT::DTOutput("programme")
        )
    )
)
setup_workspace()
# Define server logic required to draw a histogram
server <- function(input, output) {

    output$programme <- renderDataTable({
        # generate bins based on input$bins from ui.R
        #browser()
        data <- master
        if(input$day != "All"){
            data <- data %>% 
                filter(real_day %in% input$day) 
        }
        if(input$filter_name != "All"){
            data <- data %>% 
                filter(full_name %in% input$filter_name) 
        }
        if(input$first_author != "All"){
            data <- data %>% 
                filter(first_author %in% input$first_author) 
        }
        if(input$hub != "All"){
            data <- data %>% 
                filter(hub %in% input$hub) 
        }
        if(input$theme_cleaned != "All"){
            data <- data %>% 
                filter(theme_cleaned %in% input$theme_cleaned) 
        }
        if(input$slot_type != "All"){
            data <- data %>% filter(slot_type_long %in% input$slot_type) 
            
        }
        data %>% 
            select(input$time_zone, !starts_with("time")) %>% 
            distinct(authors, title, .keep_all = T) %>% 
            select(-day, -theme) %>% 
            mutate(strand = as.integer(strand)) %>% 
            select(day = real_day, starts_with("time"), type = slot_type_long, 
                   theme = theme_cleaned, authors, title, strand, room, order, abstract_id)
            #select(-day, day = real_day, -full_name, -first_author, -last_author, -hub, -theme, theme = theme_cleaned, -slot_type, type = slot_type_long, -last_name, -poster_slot, -country)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
