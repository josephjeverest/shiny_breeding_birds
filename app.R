# Targets ALL Species: Shiny App
# Joseph Everest
# September 2026


# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/


# LIBRARIES, DIRECTORY & THEMES ----

# Set working directory
# setwd("C:/Users/josepheverest/OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS/_RSPB_CDMU/Shiny_Apps/targets_by_reserve_shiny_app/")

# Load packages
library(tidyverse)
library(ggthemes)
library(gridExtra)
library(shiny)
library(sf)
library(rsconnect) # For publishing apps online
library(bslib)
library(DT)
library(sjmisc)
library(shinyWidgets)

# Generate manifest.json file
# rsconnect::writeManifest()

# Load in ggplot themes
theme_1 <- function(){
  theme_bw() +
    theme(text = element_text(family = "Segoe UI Light"),
          axis.text = element_text(size = 16), 
          axis.title = element_text(size = 18),
          axis.line.x = element_line(color="black"), 
          axis.line.y = element_line(color="black"),
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),                                          
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(1, 1, 1, 1), units = , "cm"),
          plot.title = element_text(size = 14, vjust = 1, hjust = 0, face = "bold"),
          plot.subtitle = element_text(size = 12, hjust = 0, face = "plain"),
          plot.caption = element_text(size = 12, hjust = 1, face = "plain"),
          legend.text = element_text(size = 12),          
          legend.title = element_text(size = 10, face = "bold", hjust = 0.5),
          legend.position = c(0.9, 0.9), 
          legend.key = element_blank(),
          legend.background = element_rect(color = "black", 
                                           fill = "transparent", 
                                           size = 2, linetype = "blank"))
  }


# LOAD IN DATA ---

# Load in the site-level plots
single.site.plots <- get(load("Data/modelled_plots_single.site.RData"))

# Load in the aggregated estate-level plots
estate.plots <- get(load("Data/modelled_plots_aggregated_sites.RData"))

# Extract species list
species.list <- unique(sub("\\|.*", "", names(single.site.plots)))


# USER INTERFACE (ui.R) ----

# Define the user interface (ui.R)
ui <- fluidPage(
  
  # Add logo to Google tab icon
  list(tags$head(HTML('<link rel="icon", href="RSPB_logo_2.png", 
                                   type="image/png" />'))),
  # Add title to Google tab
  div(style = "padding: 1px 0px; width: '100%'", titlePanel(title = "", windowTitle = "Breeding Birds")),
  
  setBackgroundColor("#008B8B"),
  
  # Change styling of app
  tagList(
    tags$style(HTML('.navbar-default .navbar-brand {color: #FFFFFF; }',
                    # '.navbar-default .navbar-brand {color: #FFFFFF; font-weight: bold}',
                    '.navbar-default { background-color: #0A5354 !important; }')),
    tags$style(HTML(' .navbar { height: 60px;  min-height:60px !important; }
                      .navbar-nav > li > a, .navbar-brand { padding-top:15px !important; 
                      padding-bottom:1px !important; height: 60px; }')),
    # tags$style('#national_species {color: #000000; background-color: #ff770050; border-color: #000000;}'),
    tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #0A5354}")),
    tags$style(HTML(".tabbable > .nav > li > a {background-color: #E3E3E3;  color:black}")),
    tags$head(tags$style(".butt1{background:#E3E3E3;} .butt1{color: black;} ")), 
    tags$head(tags$style(".butt2{background:#E3E3E3;} .butt2{color: black;} ")), 
    tags$head(tags$style(".butt3{background:#E3E3E3;} .butt3{color: black;} ")), 
    tags$head(tags$style(".butt4{background:#E3E3E3;} .butt4{color: black;} ")), 
    tags$head(tags$style(".butt5{background:#E3E3E3;} .butt5{color: black;} ")), 
    tags$head(tags$style(".butt6{background:#E3E3E3;} .butt6{color: black;} ")),
    tags$head(tags$style(".butt7{background:#E3E3E3;} .butt7{color: black;} ")),
    tags$head(tags$style(".dattab{background:#FFFFFF;} .dattab{color: black;} "))
  ),
  
  # Create navigation page
  navbarPage(
    
    # Title bar with icon
    title = div(img(src = "RSPB_logo_2.png", height = "6%", width = "6%"), "Breeding Birds on Reserves", style = "font-size:32px"),
    
    # Add footer
    footer = "Made by CDMU",
    
    sidebarLayout(
      sidebarPanel(selectInput(inputId = "species",  # Give the input a name "genotype"
                               label = "Select Species",  # Give the input a label to be displayed in the app
                               choices = species.list, selected = "Arctic Tern"),
                   radioButtons(),
                   selectInput(),
                   width = 2.5,
                   style = "background-color: #E3E3E3;"),

      # Panel(s) of plots
      tabsetPanel(

        tabPanel("Estate",
                 card("`\n", plotOutput("estate_level"),
                      downloadButton(outputId = "download_estate", label = "Download Plot (.png)", class = "butt1"),
                      class = "dattab")),

        tabPanel("Site",
                 card("`\n", plotOutput("site_level"),
                      downloadButton(outputId = "download_site", label = "Download Data (.csv)", class = "butt2"),
                      class = "dattab"))

      ) # End of tabsetPanel

    ) # End of sidebarLayout

  ) # End of navbarPage
  
) # End of fluid page


# SERVER (server.R) ----

# Set the server.R
server <- function(input, output) {
  
  # Filter down to just the correct species
  
  # Use that to refine the selection criteria for both time frame and site selection
  
  # ...
  
}


# RUN APP ----

# Run the shiny app
shinyApp(ui = ui, server = server)
