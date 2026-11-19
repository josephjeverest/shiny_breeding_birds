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

# Load packages
library(tidyverse)
library(ggthemes)
library(gridExtra)
library(lme4)
library(mgcv)
library(ggeffects)
library(shiny)
library(rsconnect) # For publishing apps online
library(bslib)
library(bsicons)
library(DT)
library(sjmisc)
library(shinyWidgets)
library(shinydashboard)

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

# # Load in the site-level plots
# single.site.plots <- get(load("Data/single_site_list.RData"))

# Load in the standardised data
standardised.values <- read.csv("Data/single_site_standardised_values.csv")

# Load in the predicted data
predicted.values <- (read.csv("Data/single_site_predicted_values.csv"))

# Extract species list
species.list <- unique(standardised.values$Species)


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
                   # radioButtons(),
                   selectInput(inputId = "site",
                               label = "2) Select Site of Interest",
                               choices = unique(sort(standardised.values$Site)),
                               selected = NULL,
                               multiple = FALSE),
                   sliderInput(inputId = "year",
                               label = "3) Include Records since which Year?",
                               min = 1976, max = 2025, value = 2000, sep = ""),
                   width = 3,
                   style = "background-color: #E3E3E3;"),

      # Panel(s) of plots
      tabsetPanel(

        tabPanel("Estate",
                 card("`\n",
                      layout_column_wrap(value_box(title = "Number of Sites", 
                                                   showcase = bs_icon("geo-alt-fill", size = 30), 
                                                   value = uiOutput("numsites"),
                                                   theme = "bg-success"),
                                         value_box(title = "First Year Monitored", 
                                                   showcase = bs_icon("calendar-fill", size = 30), 
                                                   value = uiOutput("firstyear"),  
                                                   theme = "bg-info"),
                                         # value_box(title = "Current Selected Period", 
                                         #           showcase = bs_icon("hourglass", size = 30), 
                                         #           value = uiOutput("selectedperiod"),  
                                         #           theme = "bg-gradient-blue-purple"),
                                         value_box(title = "Trend over Selected Period (sig.)",
                                                   showcase = bs_icon("graph-down", size = 30), 
                                                   value = uiOutput("trend"),  
                                                   theme = "bg-success")),
                      plotOutput("estate"),
                      downloadButton(outputId = "download_estate", label = "Download Plot (.png)", class = "butt1"),
                      class = "dattab")),

        tabPanel("Site",
                 card("`\n",
                      plotOutput("single_site"),
                      downloadButton(outputId = "download_site", label = "Download Data (.csv)", class = "butt2"),
                      class = "dattab")),
        
        tabPanel("Sites (All)",
                 card("`\n",
                      plotOutput("all_sites", height = "900px"),
                      downloadButton(outputId = "download_all_sites", label = "Download Data (.csv)", class = "butt2"),
                      class = "dattab"))

      ) # End of tabsetPanel

    ) # End of sidebarLayout

  ) # End of navbarPage
  
) # End of fluid page


# SERVER (server.R) ----

# Set the server.R
server <- function(input, output, session) {
  
  # Make drop-down choice of site dependent upon user input of species
  observeEvent(input$species, {
    updateSelectInput(session, "site", choices = filter(standardised.values, Species == input$species)$Site,
                      selected = (filter(standardised.values, Species == input$species) %>% 
                                    group_by(Site) %>% 
                                    summarise(Total_Site_Count = sum(Count, na.rm = TRUE)) %>% 
                                    ungroup() %>% 
                                    arrange(desc(Total_Site_Count)) %>%
                                    head(1) %>% 
                                    arrange(Site))$Site)
  })
  
  # Make drop-down choice of site dependent upon user input of species
  observeEvent(input$species, {
    updateSliderInput(session, "year", value = min(filter(predicted.values, Species == input$species)$Year),
                      min = min(filter(predicted.values, Species == input$species)$Year),
                      max = max(filter(predicted.values, Species == input$species)$Year))
  })
  
  # Generate single site plot of change
  single_site_plot <- reactive({
    
    # Filter down data to just correct species, site and year
    standardised.values.single <- filter(standardised.values, Species == input$species, Site == input$site, Year >= input$year)
    predicted.values.single <- filter(predicted.values, Species == input$species, Site == input$site, Year >= input$year)
    
    # Determine where to put label on hline
    left.side.diff <- abs(filter(standardised.values.single, Year == min(standardised.values.single$Year))$Count - 100)
    right.side.diff <- abs(filter(standardised.values.single, Year == max(standardised.values.single$Year))$Count - 100)
    up.side.diff <- abs(max(standardised.values.single$CountSt) - 100)
    bottom.side.diff <- abs(100 - min(standardised.values.single$CountSt))
    
    # Plot the outputs
    ggplot() +
      geom_hline(yintercept = 100, linetype = "dashed", size = 1) +
      # geom_text(aes(ifelse(left.side.diff > right.side.diff, min(predicted.values.single$Year) + 2, max(predicted.values.single$Year) - 2),
      #               ifelse(up.side.diff > bottom.side.diff, 100, 88),
      #               label = "Relative Abundance (100) during\nfirst year of species presence", vjust = -0.5)) +
      geom_point(data = standardised.values.single, aes(x = Year, y = CountSt), colour = "#000000", fill = "#000000", shape = 21, size = 3, alpha = 1) +
      geom_ribbon(data = predicted.values.single, aes(x = Year, ymin = conf.low, ymax = conf.high),
                  fill = "#1E90FF", alpha = 0.2, colour = "#1E90FF", linewidth = 0.05) + # Adds c.intervals for predictions as ribbon
      geom_line(data = predicted.values.single, aes(x = Year, y = Count), colour = "#000000", linewidth = 1.1) +
      scale_x_continuous(breaks = seq(min(predicted.values.single$Year), max(predicted.values.single$Year), by = 3)) +
      expand_limits(y = c(0, NA)) +
      labs(y = "Relative Abundance\n",
           x = "\nYear",
           title = paste0(unique(standardised.values.single$Species), " | ", unique(standardised.values.single$Site)),
           caption = "\n- - - - - -   Relative abundance (100) during first year of species presence at site") +
      theme_1()
    
  })
  
  # Make the reactive into a plot
  output$single_site <- renderPlot({ single_site_plot() })
  
  # Download the data
  output$download_site <- downloadHandler(
    
    # The placeholder name for the file will be called mtcars_plot.png
    filename = function() {paste0(input$species, "_", input$site, "_since", input$year, ".png")},
    
    # The content of the file will be the contents of the mtcars_plot() reactive expression
    content = function(file){
      ggsave(filename = file, plot = single_site_plot(), width = 14, height = 7)
    })
  
  # Generate all sites plots of change
  all_sites_plot <- reactive({
    
    # Filter down data to just correct species, site and year
    standardised.values.all <- filter(standardised.values, Species == input$species, Year >= input$year)
    predicted.values.all <- filter(predicted.values, Species == input$species, Year >= input$year)
    
    # Plot the outputs
    ggplot() +
      geom_hline(yintercept = 100, linetype = "dashed", size = 1) +
      # geom_text(aes(ifelse(left.side.diff > right.side.diff, min(predicted.values.single$Year) + 2, max(predicted.values.single$Year) - 2),
      #               ifelse(up.side.diff > bottom.side.diff, 100, 88),
      #               label = "Relative Abundance (100) during\nfirst year of species presence", vjust = -0.5)) +
      geom_point(data = standardised.values.all, aes(x = Year, y = CountSt), colour = "#000000", fill = "#000000", shape = 21, size = 3, alpha = 1) +
      geom_ribbon(data = predicted.values.all, aes(x = Year, ymin = conf.low, ymax = conf.high),
                  fill = "#1E90FF", alpha = 0.2, colour = "#1E90FF", linewidth = 0.05) + # Adds c.intervals for predictions as ribbon
      geom_line(data = predicted.values.all, aes(x = Year, y = Count), colour = "#000000", linewidth = 1.1) +
      # scale_x_continuous(breaks = seq(min(predicted.values.all$Year), max(predicted.values.all$Year), by = 4)) +
      facet_wrap(vars(Site), scales = "free", ncol = 3) +
      expand_limits(y = c(0, NA)) +
      labs(y = "Relative Abundance\n",
           x = "\nYear",
           title = paste0(unique(standardised.values.all$Species)),
           caption = "\n- - - - - -   Relative abundance (100) during first year of species presence at site"
      ) +
      theme_1()
    
  })
  
  # Make the reactive into a plot
  output$all_sites <- renderPlot({ all_sites_plot() })
  
  # Download the data
  output$download_all_sites <- downloadHandler(
    
    # The placeholder name for the file will be called mtcars_plot.png
    filename = function() {paste0(input$species, "_ALLsites_since", input$year, ".png")},
    
    # The content of the file will be the contents of the mtcars_plot() reactive expression
    content = function(file){
      ggsave(filename = file, plot = all_sites_plot(), width = 20, height = 30)
    })
  
  # Generate an overall plot of change for species at estate level
  estate_plot <- reactive({
    
    # Filter down data to just correct species, site and year
    standardised.values.estate <- filter(standardised.values, Species == input$species, Year >= input$year)
    predicted.values.estate <- filter(predicted.values, Species == input$species, Year >= input$year) %>% 
      mutate(Year_st = Year -  (min(.$Year)-1))
    
    # predicted.values.estate <- filter(predicted.values, Species == "Arctic Tern", Year >= 1994) %>% 
    #   mutate(Year_st = Year -  (min(.$Year)-1))
    
    # Model all the values across time as GLM
    saveRDS(glm(Count ~ Year_st, data = predicted.values.estate, weights = Starting_Count, family = poisson(link = "log")), "Data/glm_temp.RDS")
    aggregated.estate.glm <- readRDS("Data/glm_temp.RDS")
    file.remove("Data/glm_temp.RDS")
    
    # Range to predict variable
    range.to.predict.glm <- paste0("Year_st [", min(predicted.values.estate$Year_st), ":", max(predicted.values.estate$Year_st), "]")
    
    # Determine minimum year minus one
    one.below.min <- min(predicted.values.estate$Year) - 1
    
    # Extract predictions
    aggregated.predictions.glm <- data.frame(ggpredict(aggregated.estate.glm, terms = range.to.predict.glm)) %>%
      mutate(Species = unique(predicted.values.estate$Species),
             predicted = round(predicted, digits = 3),
             Year = x  + one.below.min) %>%
      rename(Count = predicted) %>%
      relocate(Species, Year, .before = ) %>%
      dplyr::select(-group, -x)
    
    # Plot the outputs
    ggplot() +
      geom_hline(yintercept = 100, linetype = "dashed", size = 1) +
      # geom_point(data = filter(predicted.values.estate, Count < 2000), aes(x = Year, y = Count)) +
      # geom_text(aes(ifelse(left.side.diff > right.side.diff, min(predicted.values.single$Year) + 2, max(predicted.values.single$Year) - 2),
      #               ifelse(up.side.diff > bottom.side.diff, 100, 88),
      #               label = "Relative Abundance (100) during\nfirst year of species presence", vjust = -0.5)) +
      geom_ribbon(data = aggregated.predictions.glm, aes(x = Year, ymin = conf.low, ymax = conf.high),
                  fill = "#1E90FF", alpha = 0.2, colour = "#1E90FF", linewidth = 0.05) + # Adds c.intervals for predictions as ribbon
      geom_line(data = aggregated.predictions.glm, aes(x = Year, y = Count), colour = "#000000", linewidth = 1.1) +
      scale_x_continuous(breaks = seq(min(predicted.values.estate$Year), max(predicted.values.estate$Year), by = 4)) +
      expand_limits(y = c(ifelse(min(aggregated.predictions.glm$Count) <= 50, 0, 50),
                          ifelse(max(aggregated.predictions.glm$Count) >= 150, max(aggregated.predictions.glm$Count) + 10, 150))) +
      labs(y = "Relative Abundance\n",
           x = "\nYear",
           # title = paste0(unique(predicted.values.estate$Species)),
           caption = "\n- - - - - -   Relative abundance (100) during first year of species presence at site"
      ) +
      theme_1()
    
  })
  
  # Generate a second version of the plot
  estate_plot_2 <- reactive({
    
    # Filter down data to just correct species, site and year
    standardised.values.estate <- filter(standardised.values, Species == input$species, Year >= input$year)
    predicted.values.estate <- filter(predicted.values, Species == input$species, Year >= input$year)
    
    # predicted.values.estate <- filter(predicted.values, Species == "Arctic Tern", Year >= 1994)

    # Sum and weight the predicted index values
    weighted.values.estate <- predicted.values.estate %>% 
      mutate(weighted_count = Count*Starting_Count) %>% 
      group_by(Year) %>% 
      summarise(total_weighted_count = sum(weighted_count)) %>% 
      ungroup()
    
    # Run GAMs on this data
    weighted.gam <- mgcv::gam(total_weighted_count ~ s(Year, k = 10), data = weighted.values.estate, family = poisson(link = "log"))
    
    # Extract slope angles at various points
    # slope_data <- gratia::derivatives(model, select = "s(x)")
    
    # Range to predict variable
    range.to.predict.gam <- paste0("Year [", min(predicted.values.estate$Year), ":", max(predicted.values.estate$Year), "]")
    
    # Extract predictions
    aggregated.predictions.gam <- data.frame(ggpredict(weighted.gam, terms = range.to.predict.gam)) %>%
      mutate(Species = unique(predicted.values.estate$Species),
             predicted = round(predicted, digits = 3)) %>%
      rename(Year = x,
             Count = predicted) %>%
      relocate(Species, .before = Year) %>%
      dplyr::select(-group)
    
    # Plot the outputs
    ggplot() +
      # geom_hline(yintercept = 100, linetype = "dashed", size = 1) +
      geom_point(data = weighted.values.estate, aes(x = Year, y = total_weighted_count), size = 0.5) +
      # geom_text(aes(ifelse(left.side.diff > right.side.diff, min(predicted.values.single$Year) + 2, max(predicted.values.single$Year) - 2),
      #               ifelse(up.side.diff > bottom.side.diff, 100, 88),
      #               label = "Relative Abundance (100) during\nfirst year of species presence", vjust = -0.5)) +
      geom_ribbon(data = aggregated.predictions.gam, aes(x = Year, ymin = conf.low, ymax = conf.high),
                  fill = "#1E90FF", alpha = 0.2, colour = "#1E90FF", linewidth = 0.05) + # Adds c.intervals for predictions as ribbon
      geom_line(data = aggregated.predictions.gam, aes(x = Year, y = Count), colour = "#000000", linewidth = 1.1) +
      scale_x_continuous(breaks = seq(min(predicted.values.estate$Year), max(predicted.values.estate$Year), by = 4)) +
      labs(y = "Summed Weighted\nRelative Abundance\n",
           x = "\nYear",
           # title = paste0(unique(predicted.values.estate$Species)),
           caption = "\n- - - - - -   Relative abundance (100) during first year of species presence at site"
      ) +
      theme_1()
    
  })
  
  # Make the reactive into a plot
  output$estate <- renderPlot({ estate_plot() })
  
  # Determine number of sites for that species in estate level trend
  output$numsites <- renderText({ paste0(length(unique(filter(predicted.values, Species == input$species, Year == input$year)$Site))) })
  
  # Determine first year values monitored on for that species
  output$firstyear <- renderText({ paste0(min(filter(predicted.values, Species == input$species)$Year)) })
  
  # Determine the current selected period
  output$selectedperiod <- renderText({ paste0(input$year, " - 2025") })
  
  # Make reactive object for generating plot outputs
  estate_stats <- reactive({
    
    # Generate predicted values for that species and year
    predicted.values.estate <- filter(predicted.values, Species == input$species, Year >= input$year)

    # Determine minimum year minus one
    one.below.min <- min(predicted.values.estate$Year) - 1

    # Model all the values across time
    saveRDS(glm(Count ~ I(Year - one.below.min), data = predicted.values.estate, weights = Starting_Count, family = poisson(link = "log")), "Data/glm_temp.RDS")
    aggregated.estate.model <- readRDS("Data/glm_temp.RDS")
    file.remove("Data/glm_temp.RDS")

    # Generate the slope values
    slope <- round(exp(coef(aggregated.estate.model)["I(Year - one.below.min)"]), digits = 2)

    # Generate the sign
    sign <- ifelse(as.list(summary(aggregated.estate.model))$coefficients[2,1] < 0, "-", "+")

    # Determine the significance
    signif <- case_when(as.list(summary(aggregated.estate.model))$coefficients[2,4] >= 0.05 ~ " ",
                        as.list(summary(aggregated.estate.model))$coefficients[2,4] < 0.05 &
                          as.list(summary(aggregated.estate.model))$coefficients[2,4] >= 0.01 ~ "*",
                        as.list(summary(aggregated.estate.model))$coefficients[2,4] < 0.01 &
                          as.list(summary(aggregated.estate.model))$coefficients[2,4] >= 0.001 ~ "**",
                        as.list(summary(aggregated.estate.model))$coefficients[2,4] < 0.001 ~ "***")

    # Set slope as output with significance
    paste0(sign, "", slope, " (", signif, ")")
    
    # paste0("NA")
    
  })
  
  # Determine trend of the aggregated estate model
  output$trend <- renderText({ estate_stats() })
  
  # Download the data
  output$download_estate <- downloadHandler(
    
    # The placeholder name for the file will be called mtcars_plot.png
    filename = function() {paste0(input$species, "_FULLestate_since", input$year, ".png")},
    
    # The content of the file will be the contents of the mtcars_plot() reactive expression
    content = function(file){
      ggsave(filename = file, plot = estate_plot(), width = 14, height = 8)
    })
  
  
}


# RUN APP ----

# Run the shiny app
shinyApp(ui = ui, server = server)


# SPARE CODE ----

# Boxes
# fluidRow(box(title = "hi", width = 4, height = 300, background = "black",
#              "A box with a solid black background"),
#          box(title = "Title 5", width = 4, height = 300, background = "blue",
#              "A box with a solid light-blue background"),
#          box(title = "Title 6",width = 4, height = 300, background = "maroon",
#              "A box with a solid maroon background"))