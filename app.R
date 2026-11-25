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
library(ggtext)
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
source("code/00_ggplot_themes.R")


# LOAD & PREPARE DATA ---

# Load in the full modelled data
estate <- read.csv("Data/full_aggregated_output.csv") %>% 
  mutate(BetterTrend = ifelse(str_detect(BetterTrend, pattern = "RSPB"), "RSPB", "National"))

# Load in the single site level data
single.site <- read.csv("Data/site_by_site_output.csv")

# Load in the combined species group data
aggregated.group <- read.csv("Data/group_model_outputs.csv")

# Load in the WeBS data
webs <- read.csv("Data/webs_interannual_output.csv")

# Determine selection choices
species.list <- unique(estate$Species)
time.periods <- unique(estate$TimePeriod)

# Function to determine the number of rows in facet_plot()
gg_facet_nrow <- function(p){
  num_panels <- length(unique(ggplot_build(p)$data[[1]]$PANEL)) # get number of panels
  num_cols <- ggplot_build(p)$layout$facet$params$ncol # get number of columns set by user
  num_rows <- wrap_dims(num_panels, ncol=num_cols)[1] # determine number of rows
}


# USER INTERFACE (ui.R) ----

# Define the user interface (ui.R)
ui <- fluidPage(
  
  # Add logo to Google tab icon
  list(tags$head(HTML('<link rel="icon", href="RSPB_logo_2.png", 
                                   type="image/png" />'))),
  # Add title to Google tab
  div(style = "padding: 1px 0px; width: '100%'", titlePanel(title = "", windowTitle = "Trends on Reserves")),
  
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
    title = div(img(src = "RSPB_logo_2.png", height = "4%", width = "4%"), "Trends on Reserves (TEST - not for use)", style = "font-size:28px"),
    
    # Add footer
    footer = "Made by CDMU",
    
    sidebarLayout(
      sidebarPanel(selectInput(inputId = "species",
                               label = "1) Select Species",
                               choices = species.list, selected = "Arctic Tern"),
                   
                   radioButtons(inputId = "period",
                                label = "2) Select Time Period",
                                choices = time.periods,
                                selected = time.periods[1],
                                inline = FALSE),
                   
                   selectInput(inputId = "site",
                               label = "3) Select Individual Site",
                               choices = unique(sort(single.site$Site)),
                               selected = NULL,
                               multiple = FALSE),
                   
                   radioButtons(inputId = "imputed",
                                label = "- Show Imputed Model Values?",
                                choices = c("Yes", "No"),
                                selected = "Yes",
                                inline = TRUE),
                   
                   radioButtons(inputId = "equalaxes",
                                label = "- Show all sites with equal axes?",
                                choices = c("Yes", "No"),
                                selected = "Yes",
                                inline = TRUE),
                   
                   selectInput(inputId = "group",
                               label = "4) Select Group for Aggregated Trend",
                               choices = unique(sort(aggregated.group$Group)),
                               selected = NULL,
                               multiple = FALSE),
                   
                   width = 3,
                   style = "background-color: #E3E3E3;"),

      # Panel(s) of plots
      tabsetPanel(

        tabPanel("Estate",
                 card("`\n",
                      layout_column_wrap(value_box(title = "Trend on RSPB Reserves", 
                                                   showcase = bs_icon("geo", size = 30), 
                                                   value = uiOutput("rspbtrend"),
                                                   theme = "bg-success"),
                                         
                                         value_box(title = "National Trend", 
                                                   showcase = bs_icon("map", size = 30), 
                                                   value = uiOutput("nationaltrend"),  
                                                   theme = "bg-info"),
                                         
                                         value_box(title = "Which performed better?",
                                                   showcase = bs_icon("graph-up", size = 30), 
                                                   value = uiOutput("better"),  
                                                   theme = "bg-success")),
                      plotOutput("estate", height = "600px"), 
                      downloadButton(outputId = "download_estate", label = "Download Plot (.png)", class = "butt1"),
                      class = "dattab")),

        tabPanel("Site (Single)",
                 card("`\n",
                      plotOutput("single_site", height = "600px"),
                      downloadButton(outputId = "download_single_site", label = "Download Data (.csv)", class = "butt2"),
                      class = "dattab")),
        
        tabPanel("Sites (All)",
                 card(downloadButton(outputId = "download_all_sites", label = "Download Data (.csv)", class = "butt2"),
                      plotOutput("all_sites", height = "2000px"),
                      class = "dattab")),
        
        tabPanel("Species Group",
                 card("`\n",
                      layout_column_wrap(value_box(title = "", 
                                                   value = "",
                                                   theme = "bg-info"),
                                         value_box(title = "Aggregated Trend on RSPB Reserves", 
                                                   showcase = bs_icon("geo", size = 30), 
                                                   value = uiOutput("grouptrend"),
                                                   theme = "bg-success"),
                                         value_box(title = "", 
                                                   value = "",
                                                   theme = "bg-info")),
                      plotOutput("aggregated_group", height = "600px"),
                      downloadButton(outputId = "download_group", label = "Download Data (.csv)", class = "butt2"),
                      class = "dattab")),
        
        tabPanel("Wintering (WeBS)",
                 card("`\n",
                      layout_column_wrap(value_box(title = "WeBS Trend on RSPB Reserves",
                                                   showcase = bs_icon("geo", size = 30),
                                                   value = uiOutput("rspbwebs"),
                                                   theme = "bg-success"),
                                         value_box(title = "National WeBS Trend",
                                                   showcase = bs_icon("map", size = 30),
                                                   value = uiOutput("nationalwebs"),
                                                   theme = "bg-info"),
                                         value_box(title = "Which performed better?",
                                                   showcase = bs_icon("graph-up", size = 30),
                                                   value = uiOutput("betterwebs"),
                                                   theme = "bg-success")),
                      plotOutput("wintering", height = "600px"),
                      downloadButton(outputId = "download_wintering", label = "Download Data (.csv)", class = "butt2"),
                      class = "dattab"))

      ) # End of tabsetPanel

    ) # End of sidebarLayout

  ) # End of navbarPage
  
) # End of fluid page


# SERVER (server.R) ----

# Set the server.R
server <- function(input, output, session) {
  
  # SITE-DEPENDENT CHOICE ----
  
  # Make drop-down choice of site dependent upon user input of species
  observeEvent(input$species, {
    updateSelectInput(session, "site", choices = filter(single.site, Species == input$species)$Site,
                      # selected = (filter(single.site, Species == input$species) %>%
                      #               group_by(Site) %>%
                      #               summarise(Total_Site_Count = sum(Count, na.rm = TRUE)) %>%
                      #               ungroup() %>%
                      #               arrange(desc(Total_Site_Count)) %>%
                      #               head(1) %>%
                      #               arrange(Site))$Site
                      selected = filter(single.site, Species == input$species)$Site[1]
                      )
  })
  
  
  # GROUP-DEPENDENT CHOICE ----
  
  # Make drop-down choice of site dependent upon user input of species
  observeEvent(input$species, {
    updateSelectInput(session, "group", choices = filter(aggregated.group, str_detect(SpeciesInGroup, pattern = input$species))$Group)
  })
  
  
  # FULL ESTATE PLOT ----
  
  # Generate an overall plot of change for species at estate level
  estate_plot <- reactive({
    
    # Filter down data to just correct species, site and year
    estate.species <- filter(estate, Species == input$species, TimePeriod == input$period)
    
    # Determine width of error bar tops
    e.b.width <- case_when(input$period == "30 years (1995-2025)" ~ 0.3,
                           input$period == "15 years (2010-2025)" ~ 0.15,
                           input$period == "4 years (2021-2025)" ~ 0.03)
    
    # Generate a plot for that species and time period
    (estate_plot <- ggplot() +
        
        geom_hline(yintercept = 100, linetype = "dashed", size = 0.5) +
        
        geom_errorbar(data = filter(estate.species, DataUse == "Points"),
                      aes(x = Year, ymin = lo, ymax = hi), width = 0, colour = "#66CD00") +
        
        geom_point(data = filter(estate.species, DataUse == "Points"),
                   aes(x = Year, y = value, fill = Source, shape = Source, colour = Source), size = 3) +
        
        geom_ribbon(data = filter(estate.species, DataUse == "Trendline"),
                    aes(x = Year, ymin = lo, ymax = hi, fill = Source), alpha = 0.3) +
        
        geom_line(data = filter(estate.species, DataUse == "Trendline"),
                  aes(x = Year, y = value, colour = Source), linewidth = 1) +
        
        scale_shape_manual(values = c(24, 21)) +
        
        scale_fill_manual(values = c("#4876FF", "#66CD00")) +
        
        scale_colour_manual(values = c("#4876FF", "#66CD00")) +
        
        scale_y_continuous(limits = c(0, NA)) + 
        
        labs(x = "\nYear\n.",
             y = "Population Index\n",
             title = input$species,
             subtitle = input$period,
             caption = paste0("**Model Parameters:** trend is **",
                              ifelse(str_detect(unique(estate.species$RSPB_ModelSlope), pattern = "increase"),
                                     "<span style = 'color: #228B22;'>",
                                     "<span style = 'color: #CD0000;'>"),
                              str_to_lower(unique(estate.species$RSPB_ModelSlope)),
                              "**; chi-squared p-value = **",
                              ifelse(unique(estate.species$RSPB_ModelChiSq) < 0.05,
                                     "<span style = 'color: #228B22;'>",
                                     "<span style = 'color: #CD0000;'>"),
                              unique(estate.species$RSPB_ModelChiSq),
                              "</span>",
                              "** with **",
                              ifelse(unique(estate.species$RSPB_ModelFit) == "Good",
                                     "<span style = 'color: #228B22;'>",
                                     "<span style = 'color: #CD0000;'>"),
                              str_to_lower(unique(estate.species$RSPB_ModelFit)),
                              "</span>",
                              "** model fit; bars represent SE")) +
        
        theme_1() +
        theme(legend.position = "right",
              plot.caption = element_markdown()))
    
  })
  
  # Make the reactive into a plot
  output$estate <- renderPlot({ estate_plot() })
  
  # Generate a download button for the plot
  output$download_estate <- downloadHandler(
    
    # The placeholder name for the file will be called mtcars_plot.png
    filename = function() {paste0(input$species, "_", input$period, ".png")},
    
    # The content of the file will be the contents of the mtcars_plot() reactive expression
    content = function(file){ ggsave(filename = file, plot = estate_plot(), width = 15, height = 8) }
    
    )
  
  
  # SINGLE SITE PLOT ----
  
  # Generate an single site plot of change for species at a site-by-site level
  single_site_plot <- reactive({
    
    # Filter down the data to the site level
    individual <- filter(single.site, Species == input$species, Site == input$site)

    # Reorder factor level of ValueSource
    individual$ValueSource <- factor(individual$ValueSource,
                                     levels = c("Surveyed", "GAM"))
    
    # Generate single site plot with flexibe y-axes
    (single_site_plot <- ggplot() +
        
        geom_vline(xintercept = 1995, colour = "gray", alpha = 0.4, size = 0.3) +
        geom_vline(xintercept = 2000, colour = "gray", alpha = 0.4, size = 0.3) +
        geom_vline(xintercept = 2005, colour = "gray", alpha = 0.4, size = 0.3) +
        geom_vline(xintercept = 2010, colour = "gray", alpha = 0.4, size = 0.3) +
        geom_vline(xintercept = 2015, colour = "gray", alpha = 0.4, size = 0.3) +
        geom_vline(xintercept = 2020, colour = "gray", alpha = 0.4, size = 0.3) +
        geom_vline(xintercept = 2025, colour = "gray", alpha = 0.4, size = 0.3) +
        
        geom_vline(data = individual, aes(xintercept = AcquisitionYear),
                   linetype = "dashed", size = 0.75, colour = "#00008B") +
        
        geom_line(data = filter(individual, ValueType == "Imputed"),
                  aes(x = Year, y = Count), colour = "#000000", linewidth = 1,
                  alpha = ifelse(input$imputed == "Yes", 0.2, 0)) +
        
        geom_line(data = filter(individual, ValueType == "Imputed", Year >= FirstYear, Year <= FinalYear),
                  aes(x = Year, y = Count), colour = "#000000", linewidth = 1,
                  alpha = ifelse(input$imputed == "Yes", 1, 0)) +
        
        geom_point(data = filter(individual, ValueType == "Fitted"), aes(x = Year, y = Count),
                   shape = 22, size = 3, colour = "#000000", fill = "#EE2C2C",
                   alpha = ifelse(input$imputed == "Yes", 0.3, 0)) +
        
        geom_point(data = filter(individual, ValueType == "Fitted", Year >= FirstYear, Year <= FinalYear),
                   aes(x = Year, y = Count), shape = 22, size = 3, colour = "#000000", fill = "#EE2C2C",
                   alpha = ifelse(input$imputed == "Yes", 1, 0)) +
        
        geom_point(data = filter(individual, ValueType == "Observed"), aes(x = Year, y = Count, fill = ValueSource),
                   shape = 21, size = 3, colour = "#000000") +
        
        scale_fill_manual(values = c("#FFD700", "#00EE00")) +
        
        scale_y_continuous(limits = c(0, NA)) +
        
        scale_x_continuous(breaks = c(1995, 2005, 2015, 2025)) +
        
        labs(title = input$site,
             subtitle = input$species,
             caption = paste0("**KEY:  ",
                              "<span style = 'color: #FFD700;'>",
                              "Yellow**",
                              "</span>",
                              " = OBSERVED; ",
                              "<span style = 'color: #00EE00;'>",
                              " **Green**",
                              "</span>",
                              " = GAM (Additional Imputation); ",
                              "<span style = 'color: #EE2C2C;'>",
                              "**Red**",
                              "</span>",
                              " = FITTED; ",
                              "**Black line**",
                              " = IMPUTED; ",
                              "<span style = 'color: #00008B;'>",
                              "**Blue line**",
                              "</span>",
                              " = Acquisition year"),
             x = "\nYear\n.",
             y = "Count\n") +
        
        theme_1() +
        theme(legend.position = "none",
              plot.caption = element_markdown(),
              panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)))
      
  })
  
  # Make the reactive into a plot
  output$single_site <- renderPlot({ single_site_plot() })
  
  # Generate a download button for the plot
  output$download_single_site <- downloadHandler(
    
    # The placeholder name for the file will be called mtcars_plot.png
    filename = function() {paste0(input$species, "_", input$site, "_", input$period, ".png")},
    
    # The content of the file will be the contents of the mtcars_plot() reactive expression
    content = function(file){ ggsave(filename = file, plot = single_site_plot(), width = 15, height = 8) }
    
  )
  
  
  # ALL SITES PLOT ----

  # Generate an overall plot of change for all sites for species at a site-by-site level
 all_sites_plot <- reactive({
   
   # Generate dataframe for all sites for the selected species
   all.sites <- filter(single.site, Species == input$species)
    
   # Reorder factor level of ValueSource
   all.sites$ValueSource <- factor(all.sites$ValueSource,
                                    levels = c("Surveyed", "GAM"))
   
   # Generate single site plot with flexibe y-axes
   (all_sites_plot <- ggplot() +
       
       geom_vline(xintercept = 1995, colour = "gray", alpha = 0.4, size = 0.3) +
       geom_vline(xintercept = 2000, colour = "gray", alpha = 0.4, size = 0.3) +
       geom_vline(xintercept = 2005, colour = "gray", alpha = 0.4, size = 0.3) +
       geom_vline(xintercept = 2010, colour = "gray", alpha = 0.4, size = 0.3) +
       geom_vline(xintercept = 2015, colour = "gray", alpha = 0.4, size = 0.3) +
       geom_vline(xintercept = 2020, colour = "gray", alpha = 0.4, size = 0.3) +
       geom_vline(xintercept = 2025, colour = "gray", alpha = 0.4, size = 0.3) +
       
       geom_vline(data = all.sites, aes(xintercept = AcquisitionYear),
                  linetype = "dashed", size = 0.75, colour = "#00008B") +
       
       geom_line(data = filter(all.sites, ValueType == "Imputed"),
                 aes(x = Year, y = Count), colour = "#000000", linewidth = 1,
                 alpha = ifelse(input$imputed == "Yes", 0.2, 0)) +
       
       geom_line(data = filter(all.sites, ValueType == "Imputed", Year >= FirstYear, Year <= FinalYear),
                 aes(x = Year, y = Count), colour = "#000000", linewidth = 1,
                 alpha = ifelse(input$imputed == "Yes", 1, 0)) +
       
       geom_point(data = filter(all.sites, ValueType == "Fitted"), aes(x = Year, y = Count),
                  shape = 22, size = 3, colour = "#000000", fill = "#EE2C2C",
                  alpha = ifelse(input$imputed == "Yes", 0.3, 0)) +
       
       geom_point(data = filter(all.sites, ValueType == "Fitted", Year >= FirstYear, Year <= FinalYear),
                  aes(x = Year, y = Count), shape = 22, size = 3, colour = "#000000", fill = "#EE2C2C",
                  alpha = ifelse(input$imputed == "Yes", 1, 0)) +
       
       geom_point(data = filter(all.sites, ValueType == "Observed"), aes(x = Year, y = Count, fill = ValueSource),
                  shape = 21, size = 3, colour = "#000000") +
       
       scale_fill_manual(values = c("#FFD700", "#00EE00")) +
       
       # scale_y_continuous(ifelse(input$equalaxes == "Yes", limits = c(0, max(all.sites$Count, na.rm = TRUE)), limits = c(0,NA))) +
       scale_y_continuous(limits = c(0,NA)) +
       
       scale_x_continuous(breaks = c(1995, 2005, 2015, 2025)) +
       
       facet_wrap(vars(Site), ncol = 3, scales = ifelse(input$equalaxes == "Yes", "free_x", "free")) +
       
       labs(title = input$species,
            subtitle = "All monitoring sites since 1995",
            caption = paste0("**KEY:  ",
                             "<span style = 'color: #FFD700;'>",
                             "Yellow**",
                             "</span>",
                             " = OBSERVED; ",
                             "<span style = 'color: #00EE00;'>",
                             " **Green**",
                             "</span>",
                             " = GAM (Additional Imputation); ",
                             "<span style = 'color: #EE2C2C;'>",
                             "**Red**",
                             "</span>",
                             " = FITTED; ",
                             "**Black line**",
                             " = IMPUTED; ",
                             "<span style = 'color: #00008B;'>",
                             "**Blue line**",
                             "</span>",
                             " = Acquisition year"),
            x = "\nYear\n.",
            y = "Count\n") +
       
       theme_1() +
       theme(legend.position = "none",
             plot.caption = element_markdown(),
             panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)))
    
  })
  
  # Determine window height for facet_plot()
  window_height <- reactive(gg_facet_nrow(all_sites_plot()))
 
  # Make the reactive into a plot
  output$all_sites <- renderPlot( { all_sites_plot() }, height = function(){ window_height()*250 })
  
  
  # Generate a download button for the plot
  output$download_all_sites <- downloadHandler(
    
    # The placeholder name for the file will be called mtcars_plot.png
    filename = function() {paste0(input$species, "_ALL_SITES_", input$period, ".png")},
    
    # The content of the file will be the contents of the mtcars_plot() reactive expression
    content = function(file){ ggsave(filename = file, plot = all_sites_plot(), width = 15, height = 8) }
    
  )

  
  # AGGREGATED GROUP PLOT ----
  
  # Generate an overall plot of change for species group
  group_plot <- reactive({
    
    # Filter down data to just correct species, site and year
    single.group <- filter(aggregated.group, Group == input$group, TimePeriod == input$period)
    
    # Generate a plot for that species and time period
    (group_plot <- ggplot() +
        
        geom_hline(yintercept = 100, linetype = "dashed", size = 0.5) +
        
        geom_point(data = filter(single.group, DataUse == "Points"),
                   aes(x = Year, y = value), size = 3, colour = "#66CD00", fill = "#66CD00", shape = 21) +
        
        geom_ribbon(data = filter(single.group, DataUse == "Trendline"),
                    aes(x = Year, ymin = lo, ymax = hi), fill = "#66CD00", alpha = 0.3) +
        
        geom_line(data = filter(single.group, DataUse == "Trendline"),
                  aes(x = Year, y = value), colour = "#66CD00", linewidth = 1) +
        
        labs(x = "\nYear\n.",
             y = "Combined Population Index\n",
             title = input$group,
             subtitle = input$period,
             caption = paste0("**Model Parameters:** trend is **",
                              ifelse(str_detect(unique(single.group$RSPB_ModelSlope), pattern = "increase"),
                                     "<span style = 'color: #228B22;'>",
                                     "<span style = 'color: #CD0000;'>"),
                              str_to_lower(unique(single.group$RSPB_ModelSlope)),
                              "**; chi-squared p-value = **",
                              ifelse(unique(single.group$RSPB_ModelChiSq) < 0.05,
                                     "<span style = 'color: #228B22;'>",
                                     "<span style = 'color: #CD0000;'>"),
                              unique(single.group$RSPB_ModelChiSq),
                              "</span>",
                              "** with **",
                              ifelse(unique(single.group$RSPB_ModelFit) == "Good",
                                     "<span style = 'color: #228B22;'>",
                                     "<span style = 'color: #CD0000;'>"),
                              str_to_lower(unique(single.group$RSPB_ModelFit)),
                              "</span>",
                              "** model fit")) +
        
        theme_1() +
        theme(legend.position = "right",
              plot.caption = element_markdown()))
    
  })
  
  # Make the reactive into a plot
  output$aggregated_group <- renderPlot({ group_plot() })
  
  # Generate a download button for the plot
  output$download_group <- downloadHandler(
    
    # The placeholder name for the file will be called mtcars_plot.png
    filename = function() {paste0(input$species, "_grouped_by_", input$group, "_", input$period, ".png")},
    
    # The content of the file will be the contents of the mtcars_plot() reactive expression
    content = function(file){ ggsave(filename = file, plot = group_plot(), width = 15, height = 8) }
    
  )
  
  
  # WINTERING ----
  
  # Generate an overall plot of change for species at estate level
  wintering_plot <- reactive({

    # Filter down WeBS data to just correct species
    webs.species <- filter(webs, Species == input$species, Period == input$period)

    # Generate a plot for that species and time period
    (wintering_plot <- ggplot() +
        
        geom_hline(yintercept = 100, linetype = "dashed", size = 0.5) +
        
        geom_point(data = filter(webs.species, DataUse == "Points"),
                   aes(x = StartYear, y = value, fill = Source, shape = Source, colour = Source), size = 3) +
        
        geom_ribbon(data = filter(webs.species, DataUse == "Trendline"),
                    aes(x = StartYear, ymin = lo, ymax = hi, fill = Source), alpha = 0.3) +
        
        geom_line(data = filter(webs.species, DataUse == "Trendline"),
                  aes(x = StartYear, y = value, colour = Source), linewidth = 1) +
        
        scale_shape_manual(values = c(24, 21)) +
        
        scale_fill_manual(values = c("#4876FF", "#66CD00")) +
        
        scale_colour_manual(values = c("#4876FF", "#66CD00")) +
        
        scale_y_continuous(limits = c(0, NA)) +
        
        labs(title = unique(webs.species$Period),
             subtitle = paste0("**Wintering trends:**",
                               "<br>",
                               "     - RSPB reserves: **",
                               ifelse(unique(filter(webs.species, Source == "RSPB Estate")$PercentChange) >= 0,
                                      "<span style = 'color: #228B22;'>",
                                      "<span style = 'color: #CD0000;'>"),
                               unique(filter(webs.species, Source == "RSPB Estate")$PercentChange),
                               "</span>",
                               "** %",
                               "<br>",
                               "     - Nationally (UK): **",
                               ifelse(unique(filter(webs.species, Source == "National Trend")$PercentChange) >= 0,
                                      "<span style = 'color: #228B22;'>",
                                      "<span style = 'color: #CD0000;'>"),
                               unique(filter(webs.species, Source == "National Trend")$PercentChange),
                               "</span>",
                               "** %"
             ),
             x = "\nYear",
             y = "Index of Estimated Maximum Monthly Count*\n",
             caption = paste0("RSPB trend is **",
                              ifelse(unique(filter(webs.species, Source == "RSPB Estate")$pvalue) < 0.05,
                                     "<span style = 'color: #228B22;'>significant",
                                     "<span style = 'color: #CD0000;'>not significant"),
                              "</span>",
                              "**",
                              " as p = ",
                              unique(filter(webs.species, Source == "RSPB Estate")$pvalue),
                              "<br>",
                              "National trend is **",
                              ifelse(unique(filter(webs.species, Source == "National Trend")$pvalue) < 0.05,
                                     "<span style = 'color: #228B22;'>significant",
                                     "<span style = 'color: #CD0000;'>not significant"),
                              "</span>",
                              "**",
                              " as p = ",
                              unique(filter(webs.species, Source == "National Trend")$pvalue),
                              "<br>",
                              "<br>",
                              "* index is set to zero in final year, not first year, due to BTO processes"
             )) +
        theme_1_tiny() +
        theme(legend.position = "bottom",
              plot.subtitle = element_markdown(),
              plot.caption = element_markdown()))

  })

  # Make the reactive into a plot
  output$wintering <- renderPlot({ wintering_plot() })

  # Generate a download button for the plot
  output$download_wintering <- downloadHandler(

    # The placeholder name for the file will be called mtcars_plot.png
    filename = function() {paste0("wintering_", input$species, "_", input$period, ".png")},

    # The content of the file will be the contents of the mtcars_plot() reactive expression
    content = function(file){ ggsave(filename = file, plot = wintering_plot(), width = 15, height = 8) }

  )
  
 
 # STATS ----
  
 # Determine RSPB level trend
 output$rspbtrend <- renderText({
   
   paste0(ifelse(unique(filter(estate, Species == input$species, TimePeriod == input$period)$RSPB_PercentChange) > 0, "+", ""),
          unique(filter(estate, Species == input$species, TimePeriod == input$period)$RSPB_PercentChange),
          "%") 
 })
  
 # Determine national level trend
 output$nationaltrend <- renderText({
   
   paste0(ifelse(unique(filter(estate, Species == input$species, TimePeriod == input$period)$National_PercentChange) > 0, "+", ""),
          unique(filter(estate, Species == input$species, TimePeriod == input$period)$National_PercentChange),
          "%") 
 })
 
 # Determine the better of the two trends
 output$better <- renderText({
   
   paste0(unique(filter(estate, Species == input$species, TimePeriod == input$period)$BetterTrend)) 
   
 })
 
 # Determine the rate of change by group
 output$grouptrend <- renderText({
   
   paste0(ifelse(unique(filter(aggregated.group, Group == input$group, TimePeriod == input$period)$RSPB_PercentChange) > 0, "+", ""),
          unique(filter(aggregated.group, Group == input$group, TimePeriod == input$period)$RSPB_PercentChange),
          "%") 
 })
 
 # Determine the RSPB WeBS change
 output$rspbwebs <- renderText({
   
   paste0(ifelse(unique(filter(webs, Species == input$species, Period == input$period, Source == "RSPB Estate")$PercentChange) > 0, "+", ""),
          unique(filter(webs, Species == input$species, Period == input$period, Source == "RSPB Estate")$PercentChange),
          "%")
 })
 
 # Determine the national WeBS change
 output$nationalwebs <- renderText({
   
   paste0(ifelse(unique(filter(webs, Species == input$species, Period == input$period, Source == "National Trend")$PercentChange) > 0, "+", ""),
          unique(filter(webs, Species == input$species, Period == input$period, Source == "National Trend")$PercentChange),
          "%")
 })
 
 # Determine which is better
 output$betterwebs <- renderText({
   
   paste0(case_when(unique(filter(webs, Species == input$species, Period == input$period, Source == "RSPB Estate")$PercentChange) > 
                      unique(filter(webs, Species == input$species, Period == input$period, Source == "National Trend")$PercentChange) ~ "RSPB",
                    unique(filter(webs, Species == input$species, Period == input$period, Source == "RSPB Estate")$PercentChange) < 
                      unique(filter(webs, Species == input$species, Period == input$period, Source == "National Trend")$PercentChange) ~ "National",
                    unique(filter(webs, Species == input$species, Period == input$period, Source == "RSPB Estate")$PercentChange) == 
                      unique(filter(webs, Species == input$species, Period == input$period, Source == "National Trend")$PercentChange) ~ "Equal"))
 })
  
}


# RUN APP ----

# Run the shiny app
shinyApp(ui = ui, server = server)
