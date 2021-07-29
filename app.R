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

time_strings <- union(master$time_utc, 
                      union(master$time_cest, union(master$time_bst, 
                                                    union(master$time_aest, master$time_cdt)))) %>% 
    sort()

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
                        choices = c("All", unique(days)), selected = "All",
                        multiple = F, selectize = T),
            selectInput(inputId = "time", 
                        label = "Time",
                        choices = c("All", time_strings[c(9:24, 1:8)]), selected = "All",
                        multiple = F, selectize = T),
            # selectInput(inputId = "hub", 
            #             label = "Time Hub",
            #             choices = c("All", unique(master$hub)), selected = "All",
            #             multiple = F, selectize = T),
            selectInput(inputId = "time_zone", 
                        label = "Time Zone",
                        choices = tz, selected = "time_utc",
                        multiple = F, selectize = T),
            selectInput(inputId = "abstract_id", 
                        label = "Abstract ID",
                        choices = c("All", sort(na.omit(unique(master$abstract_id)))), selected = "All",
                        multiple = F, selectize = T),
            p(
                "ICMPC16/ESCOM11 Navigator v0.5", 
                shiny::tags$br(), 
                shiny::tags$br(), 
                "Author: Klaus Frieler", 
                shiny::tags$br(), 
                shiny::a(href = "https://www.aesthetics.mpg.de/en.html", 
                         "Max Planck Institute for Empirical Aesthetics, Frankfurt/M, Germany", 
                         target = "_blank"),
                shiny::tags$br(), 
                shiny::tags$br(),
                "Data provided by", 
                shiny::a(href = "https://m-w-w.github.io/", "Michael Weiss", target = "_blank"), 
                shiny::tags$br(),
                shiny::tags$br(), 
                "Powered by",
                shiny::tags$br(),
                shiny::a(href = "http://www.music-psychology.de/",
                "Deutsche Gesellschaft für Musikspsychologie", target = "_blank"),
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
        day_zone <- str_replace(input$time_zone, "time_", "day_")
        
        if(input$day != "All"){
            data <- data %>% 
                filter(!!sym(day_zone) %in% input$day) 
        }
        if(input$time != "All"){
            data <- data %>% 
                mutate(hours = substr(!!sym(input$time_zone), 1, 2)) %>% 
                filter(hours %in% substr(input$time, 1, 2)) 
        }
        if(input$filter_name != "All"){
            data <- data %>% 
                filter(full_name %in% input$filter_name) 
        }
        if(input$first_author != "All"){
            data <- data %>% 
                filter(first_author %in% input$first_author) 
        }
        # if(input$hub != "All"){
        #     data <- data %>% 
        #         filter(hub %in% input$hub) 
        # }
        if(input$theme_cleaned != "All"){
            data <- data %>% 
                filter(theme_cleaned %in% input$theme_cleaned) 
        }
        if(input$slot_type != "All"){
            data <- data %>% filter(slot_type_long %in% input$slot_type) 
            
        }
        if(input$abstract_id != "All"){
            browser()
            data <- data %>% filter(abstract_id %in% input$abstract_id) 
            
        }
        data %>% 
            select(input$time_zone, !starts_with("time")) %>% 
            select(day_zone, !starts_with("day")) %>% 
            distinct(authors, title, .keep_all = T) %>% 
            select(-theme) %>% 
            mutate(strand = as.integer(strand)) %>% 
            select(day_zone, input$time_zone, type = slot_type_long, 
                   theme = theme_cleaned, authors, title, strand, room, order, abstract_id)
            #select(-day, day = real_day, -full_name, -first_author, -last_author, -hub, -theme, theme = theme_cleaned, -slot_type, type = slot_type_long, -last_name, -poster_slot, -country)
    }, options = list(lengthMenu = list(c(25, 50,  -1), c("25", "50",  "All"))))
}

# Run the application 
shinyApp(ui = ui, server = server)
