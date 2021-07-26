#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(shiny)
messagef <- function(...) messagef(sprintf(...))

slot_codes <- c("P" = "Poster", 
                "S" = "Symposium", 
                "W" =  "Workshop", 
                "K" = "Keynote",
                "C" = "Concert", 
                "1" = "Talk", 
                "2" = "Talk", 
                "3" = "Talk", 
                "4" = "Talk")
setup_workspace <- function(fname = "ICMPC-ESCOM-2021-Programme.csv"){
    
    master <- readr::read_csv(fname)
    names(master) <- c("day", "time_utc", "strand", "room", "order", "theme", "authors", "title", "abstract_id", 
                       "country")
    master <- master %>% 
        mutate(authors = str_replace(authors, "\\([0-9,]+\\)", "")) %>% 
#        mutate(first_authors = str_split(authors, ";") %>% map_chr(., ~{.x[[1]][1]})) %>% 
        mutate(full_name = str_split(authors, ";")) %>%
        unnest(full_name) %>% 
        mutate(last_name = str_split(full_name, ",") %>% map_chr(., ~{.x[[1]][1]})) %>% 
        mutate(full_name = trimws(full_name)) %>% 
        mutate(last_name = trimws(last_name)) %>% 
        mutate(time_utc =  str_replace_all(time_utc, "\\([a-zA-Z]+\\)", "") %>% trimws() %>% as.numeric()) %>% 
        mutate(time_cest = time_utc  + 2, 
               time_gmt = time_utc + 1, 
               time_ist = time_utc  + 5.5, 
               time_cdt = time_utc - 5,
               time_aest = time_utc + 10) %>% 
        mutate(slot_type = substr(room, 1, 1), poster_slot = substr(room, 2, 2)) %>% 
        mutate(slot_type_long = slot_codes[slot_type])

    assign("master", master, globalenv())
}
setup_workspace()
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("ICMPC11/ESCOM16 Progamme"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "filter_name", 
                        label = "Last Name",
                        choices = unique(master$full_name), selected = "Alluri, Vinoo",
                        multiple = T, selectize = T),
            selectInput(inputId = "slot_type", 
                        label = "Slot Type",
                        choices = c("All", unique(master$slot_type_long)), selected = "All",
                        multiple = F, selectize = T),
            selectInput(inputId = "time_zone", 
                        label = "Time ",
                        choices = c(names(master %>% select(starts_with("time")))), selected = "time_utc",
                        multiple = F, selectize = T),
            width = 2
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tableOutput("programme")
        )
    )
)
setup_workspace()
# Define server logic required to draw a histogram
server <- function(input, output) {

    output$programme <- renderTable({
        # generate bins based on input$bins from ui.R
        #browser()
        data <- master %>% filter(full_name %in% input$filter_name) 
        if(input$slot_type != "All"){
            data <- data %>% filter(slot_type_long %in% input$slot_type_long) 
            
        }
        data %>% select(input$time_zone, !starts_with("time"), -slot_type, -last_name, -full_name, -country, )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
