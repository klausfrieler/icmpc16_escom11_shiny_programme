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
library(r2d3)
library(shiny)
library(shinythemes)
source("utils.R")
source("stats.R")
source("networks.R")
setup_workspace()

impressum <- function(){
    p(
        "ICMPC16/ESCOM11 Stats v0.2", 
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
        style = "font-size: 10pt; display: block"
    )
    
}
ui <- fluidPage(
    
    # App title ----
    titlePanel(
        h1("ICMPC16/ESCOM11 Stats"), 
        windowTitle = "ICMPC/ESCOM"
    ),
    # Sidebar layout with flowLayout input and output definitions ----
    sidebarLayout(
        sidebarPanel(
            # Input: Select information ----
            selectInput(inputId = "stats_type", 
                        label = "Statistic",
                        choices = c("Basic", "Author", "Theme (Original)", "Theme (Categorized)"), selected = "Basic",
                        multiple = F, selectize = F),
            selectInput(inputId = "highlight", 
                        label = "Highlight Author",
                        choices = c("", sort(unique(master$full_name))), selected = "",
                        multiple = F, selectize = T),
            impressum(),
            width = 2
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            tabsetPanel(type = "tabs",
                        #tabPanel("Introduction", htmlOutput("introduction")),
                        tabPanel("Stats", 
                                 DT::DTOutput("stats")
                                 
                                 ),
                        tabPanel("Network", 
                                 forceNetworkOutput("collab_network", height = "1000px"))
                        )

            )
        )
        
    )

ui_alt <-   
    shiny::shinyUI(
    navbarPage(
        title = "ICMPC16/ESCOM11 Stats", 
        theme = shinytheme("spacelab"),
        id = "tabs",
        tabPanel(
            "Stats",
            sidebarLayout(
                sidebarPanel(
                    # Input: Select information ----
                    selectInput(inputId = "stats_type", 
                                label = "Statistic",
                                choices = c("Basic", "Author", "Theme (Original)", "Theme (Categorized)"), selected = "Basic",
                                multiple = F, selectize = F),
                    impressum(),
                    width = 2
                ),
                
                # Main panel for displaying outputs ----
                mainPanel(
                    DT::DTOutput("stats")
                    )
                    
                )
            ),
        tabPanel(
            "Network",
            sidebarLayout(
                sidebarPanel(
                    # Input: Select information ----
                    selectInput(inputId = "subset", 
                                label = "Subnetwork",
                                choices = c("All", "Core", "Rim"), selected = "All",
                                multiple = F, selectize = T),
                    selectInput(inputId = "highlight", 
                                label = "Highlight Author",
                                choices = c("", sort(unique(master$full_name))), selected = "",
                                multiple = F, selectize = T),
                    selectInput(inputId = "charge", 
                                label = "Node Charge",
                                choices = seq(1, 5)*(-60), selected = "-120",
                                multiple = F, selectize = F),
                    selectInput(inputId = "link_distance", 
                                label = "Link Distance",
                                choices = seq(1, 5)*10, selected = "20",
                                multiple = F, selectize = F),
                    selectInput(inputId = "font_size", 
                                label = "Font Size",
                                choices = seq(1, 10)*2 + 12, selected = "24",
                                multiple = F, selectize = F),
                    selectInput(inputId = "opacity", 
                                label = "Opacity",
                                choices = seq(0, 1, .1), selected = "0.1",
                                multiple = F, selectize = F),
                    impressum(),
                    width = 2
                ),
                
                # Main panel for displaying outputs ----
                mainPanel(forceNetworkOutput("collab_network", height = "1000px"))
            )
        )))
            
        

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    output$stats <- renderDataTable({
        # generate bins based on input$bins from ui.R
        #browser()
        data <- NULL
        if(input$stats_type == "Basic"){
            data <- get_basic_stats(master)
        }
        if(input$stats_type == "Author"){
            data <- get_author_stats(master) %>% 
            arrange(desc(n_paper)) %>% 
                set_names("Name", 
                          "Papers", 
                          "First Authored", 
                          "Last Authored", 
                          "Middle Authored",
                          "Themes (Original)",
                          "Themes (Categorized)",
                          "Thematic Diversity (Original)",
                          "Thematic Diversity (Categorized)",
                          "Co-Workers",
                          "Mean Co-Workers/Paper",
                ) %>% select(-`Themes (Original)`, -`Thematic Diversity (Original)` )
        }
        if(input$stats_type == "Theme (Categorized)"){
            data <- get_theme_stats(master) %>% 
                arrange(desc(n_papers)) %>% 
                set_names("Theme (Categorized)", "Papers", "Authors", "Mean Author/Paper")
        }
        if(input$stats_type == "Theme (Original)"){
            data <- get_theme_stats(master, "theme_cleaned") %>% 
                arrange(desc(n_papers)) %>% 
                set_names("Theme (Original)", "Papers", "Authors", "Mean Author/Paper")
        }
        data %>% mutate_if(is.numeric, round, 2)
    }, options = list(lengthMenu = list(c(25, 50,  -1), c("25", "50",  "All"))))
    output$collab_network <- renderForceNetwork({
        d3n <- get_network(master, 
                           author  = input$highlight, 
                           set_globals = F, 
                           format = "d3", 
                           subset = tolower(input$subset)) 
        plot_D3_network(d3n, 
                        charge = as.numeric(input$charge),
                        linkDistance = as.numeric(input$link_distance),
                        fontSize = as.numeric(input$font_size),
                        opacityNoHover = as.numeric(input$opacity),
                        file = NULL)
    })
}

# Run the application 
shinyApp(ui = ui_alt, server = server)
