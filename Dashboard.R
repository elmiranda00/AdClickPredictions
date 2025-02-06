if (!require(shiny)) install.packages("shiny")
if (!require(shinydashboard)) install.packages("shinydashboard")
if (!require(DT)) install.packages("DT")
if (!require(dashboardthemes)) install.packages("dashboardthemes")
if (!require(shinyWidgets)) install.packages("shinyWidgets")
if (!require(readxl)) install.packages("readxl")
if (!require(dplyr)) install.packages("dplyr")
if (!require(tidyr)) install.packages("tidyr")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(plotly)) install.packages("plotly")
if (!require(randomForest)) install.packages("randomForest")
if (!require(sf)) install.packages("sf")
if (!require(leaflet)) install.packages("leaflet")
if (!require(shinycssloaders)) install.packages("shinycssloaders")
if (!require(shinyalert)) install.packages("shinyalert")

# Load Libraries
library(shiny)
library(shinydashboard)
library(DT)
library(dashboardthemes)
library(shinyWidgets)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(randomForest)
library(sf)
library(leaflet)
library(shinycssloaders)
library(shinyalert)


load("DeliveryAdClick.RData")


######## Helper functions & variables
{
  #### 1. Prediction model helper functions
  
  # Modelling Helper Functions
  {
    # 1. Min-Max Scaling
    min_max_scale <- function(x) {
      return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
    }
    
    # 2. Z Score Scaling
    z_score_scale <- function(x) {
      return((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
    }
    
    # 3. Function to standardize data
    standardize_data <- function(data, feature_list) {
      
      for (var in feature_list) {
        if (var %in% colnames(data)) {
          if (var == "Number_of_Previous_Orders") {
            data[[var]] <- z_score_scale(data[[var]])
          } else {
            data[[var]] <- min_max_scale(data[[var]])
          }}}
      
      return(data)
    }
    
    # 4. Function for Data Cleaning
    modify_missing_values <- function(data, column_name, replacement_string){
      
      data[[column_name]][is.na(data[[column_name]])] <- replacement_string
      return(data)
    }
    
    # 5. Function for One Hot Encoding
    one_hot_encode <- function(data, vars) {
      encoded_data <- data
      for (var in vars) {
        encoded <- model.matrix(~ . - 1, data.frame(data[[var]]))
        colnames(encoded) <- paste0(var, "_", colnames(encoded))
        encoded_data <- cbind(encoded_data, encoded)
        encoded_data[[var]] <- NULL
      }
      return(encoded_data)
    }
    
    # 6. Function for feature engineering
    feature_engineering <- function(data){
      
      engineered_data <- data
      
      # Polynomial Features (Non-linear Relationships)
      engineered_data$Daytime_Squared <- data$Daytime^2
      engineered_data$Time_On_Previous_Website_Squared <- data$Time_On_Previous_Website^2
      
      # Interaction Features (Cross-Products)
      engineered_data$Daytime_x_Orders <- data$Daytime * data$Number_of_Previous_Orders
      engineered_data$SocialNetwork_x_Daytime <- data$Daytime * data$Social_Network_data..var..Facebook
      
      # Recency Transformation (Inverse Time)
      engineered_data$Recency <- 1 / (1 + data$Time_On_Previous_Website)
      
      # Weekend Indicator (Binary)
      engineered_data$Is_Weekend <- ifelse(
        data$Weekday_data..var..Saturday == 1 |
          data$Weekday_data..var..Sunday == 1, 1, 0)
      
      # Ratio Features (Relative Importance)
      engineered_data$Orders_Per_Daytime <- data$Number_of_Previous_Orders / (data$Daytime + 1)
      
      return(engineered_data)
    }
  }
  
  # Model

  # Real Prediction Function
  
  prediction_model <- function(data) {
    
    # Feature Lists
    categorical_vars <- c("Region", "Carrier", "Weekday", "Social_Network", "Restaurant_Type")
    continuous_vars <- c("Time_On_Previous_Website", "Number_of_Previous_Orders",
                         "Time_On_Previous_Website_Squared",
                         "Daytime_x_Orders", "SocialNetwork_x_Daytime",
                         "Recency", "Orders_Per_Daytime")
    
    # Features (original & engineered) selected based on correlation analysis
    selected_features <- c("Daytime", "Is_Weekend", "Carrier_data..var..SFR",
                           "Carrier_data..var..Orange", "SocialNetwork_x_Daytime",
                           "Time_On_Previous_Website", "Daytime_Squared")
    # MODELLING WORKFLOW 
    # 1. Handling Missing Values in Restaurant Type
    cleaned_data <- modify_missing_values(data, "Restaurant_Type", "Unknown") 
    
    # 2. One Hot Encoding for categorical variables
    encoded_data <- one_hot_encode(cleaned_data, categorical_vars)
    
    # 3. Feature Engineering
    engineered_data <- feature_engineering(encoded_data)
    
    # 4. Standardize/Normalize Data
    final_data <- standardize_data(engineered_data, continuous_vars)
    
    # 5. Generate predictions based on trained Random Forest model
    load("rf_model.Rdata")
    rf_pred <- predict(rf_model, final_data)
    
    # Add predicted class columns to the output dataset
    rf_pred <- data.frame(Predicted_Class = rf_pred)
    data$Clicks_Conversion_Prediction <- rf_pred$Predicted_Class
    
    return(data)
  }
  
  
  #### 2. Dashboard helper functions & variables
  
  # Function to add "All" to the select options
  add_all_option <- function(vector) {
    c("All", unique(vector))
  }
  
  # Weekday labels
  weekday_labels <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  
  custom_colors <- c("#17a2b8", "#007bff", "#00c5ff", "#0056b3", "#6c757d")
  
  # Interactive map
  FRmap <- read_sf("MapData/regions-20180101.shp")
  
  plot_click_map <- function(data, mapShapeObject, click_column = "Clicks_Conversion") {
    
    # Check if the provided column exists in the data
    if (!click_column %in% colnames(data)) {
      stop(paste("Column", click_column, "not found in the dataset"))
    }
    
    # Ensure the click column is numeric
    if (is.factor(data[[click_column]])) {
      data[[click_column]] <- as.numeric(as.character(data[[click_column]]))
    }
    
    # Group data by Region aggregated by clicks
    clicks_df <- data %>%
      group_by(Region) %>% 
      summarize(Click = sum(.data[[click_column]], na.rm = TRUE)) %>% # Use the dynamic column name
      arrange(desc(Click))
    
    FRmap <- mapShapeObject
    
    # Group regions of France based on regions in the data
    FRmap <- FRmap %>%
      mutate(
        RegionGroup = case_when(
          nom %in% c("Normandie", "Hauts-de-France") ~ "North France",
          nom %in% c("Provence-Alpes-Côte d'Azur", "Occitanie", "Corse") ~ "South France",
          nom %in% c("Grand Est", "Bourgogne-Franche-Comté") ~ "Alsace and East France",
          nom %in% c("Bretagne", "Pays de la Loire") ~ "West France",
          nom == "Île-de-France" ~ "Paris",
          TRUE ~ "Rest of France"
        )
      )
    
    # Create new geometries based on the custom grouping
    FRmap <- FRmap %>%
      group_by(RegionGroup) %>% 
      summarize(geometry = st_union(geometry))
    
    # Join the clicks in each region with the map, create a data label column
    FRmap <- FRmap %>%
      left_join(clicks_df, by = c("RegionGroup" = "Region"))
    FRmap$LabelText <- paste(FRmap$RegionGroup, "(", FRmap$Click, " clicks)", sep = "")
    
    # Colour Palette based on Click
    pal <- colorNumeric(
      palette = c("#ADD8E6", "#4682B4", "#00008B"),  # Set colours here
      domain = FRmap$Click,  # Column containing aggregated counts
      na.color = "gray" 
    )
    
    # Create the interactive map
    m <- leaflet(FRmap, options = leafletOptions(scrollWheelZoom = FALSE, zoomControl = FALSE)) %>%  
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lat = 46.603354, lng = 1.888334, zoom = 5) %>%  # Set view to focus on France
      addPolygons(
        fillColor = ~pal(Click),  # Color based on Clicks
        weight = 1,  
        color = "white",  
        fillOpacity = 0.7, 
        smoothFactor = 0.5,  
        label = ~paste(RegionGroup, ": ", Click, " clicks"),  # Add data labels 
        highlight = highlightOptions(
          weight = 3,  #
          color = "black",  
          bringToFront = TRUE  
        )
      ) %>%
      addLegend(
        "bottomright", pal = pal, values = ~Click, title = "Click Count", opacity = 0.7
      )
    
    # Display the map
    m
  }
  
}


# 3. Pre-processing
ClickTraining <- modify_missing_values(ClickTraining, "Restaurant_Type", "Unknown")



######## DASHBOARD FRAMEWORK

# UI 

header <- dashboardHeader(title = tags$div(style = "white-space: nowrap;", "Online Ad Click Predictor"))

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Customer Insights", tabName = "insights", icon = icon("chart-bar")),
    menuItem("Clicks vs Non-Clicks", tabName = "insights2", icon = icon("mouse-pointer")),
    menuItem("Ad Click Predictions", tabName = "predictions", icon = icon("table"))
  )
)

body <- dashboardBody(
  
  # Dahsboard presentation
  useShinyalert(),
  
  # Define dashboard theme and customize appearance
  shinyDashboardThemes(theme = "poor_mans_flatly"),
  tags$head(tags$style(HTML("
  .box-title {color: black !important;}
  .small-box .inner h3 {white-space: normal !important; 
                        word-wrap: break-word !important; 
                        overflow-wrap: break-word !important;}
  "))),
  
  # Define tab content
  tabItems(
    
    # Tab 1: Customer Insights
    tabItem(tabName = "insights",
            h2("Customer Insights"),
            
            # Filter options and Features exploration
            fluidRow(
              
              # Filter options
              box(
                title = "Customer Profile", width = 3, status = "primary",
                selectInput("insights_region", "Region", choices = add_all_option(ClickTraining$Region)),
                selectInput("insights_carrier", "Carrier", choices = add_all_option(ClickTraining$Carrier)),
                selectInput("insights_social", "Social Network", choices = add_all_option(ClickTraining$Social_Network)),
                sliderInput("insights_daytime", "Daytime Range", min = 0, max = 24, value = c(0, 24), step = 1, ticks = FALSE, post = "h"),
                checkboxGroupInput(inputId = "insights_weekday", label = "Select Weekdays:", choices = weekday_labels, selected = weekday_labels, inline = TRUE),
                sliderInput("insights_time_prev", "Time on Previous Website (Minutes)", min = 0, max = 2000, value = c(0, 2000)),
                sliderInput("insights_num_orders", "Number of Previous Orders", min = 0, max = 15, value = c(0, 15), step = 1, ticks = TRUE),
                selectInput("insights_restaurant", "Restaurant Type", choices = add_all_option(ClickTraining$Restaurant_Type), multiple = TRUE, selected = "All"),
                selectInput("insights_click_filter", "Clicks:", choices = c("All", "Clicks (1)", "Non-Clicks (0)"), selected = "All"),
                # Filter action (apply or reset)
                fluidRow(
                  actionButton("apply_filters", "Apply Filters", class = "btn-primary"),
                  actionButton("reset_filters", "Reset Filters", class = "btn-secondary")
                )
              ),
              
              # Features exploration
              box(
                title = "Feature Analysis", width = 9, status = "primary",
                tabBox(
                  width = 12,
                  tabPanel("Interactive Table", DTOutput("filtered_table", height = "400px")),
                  tabPanel("Carrier", plotlyOutput("click_percentage_carrier")),
                  tabPanel("Social Network", plotlyOutput("click_percentage_social")),
                  tabPanel("Time Spent on Previous Website", plotlyOutput("time_on_website_analysis")),
                  tabPanel("Order History", plotlyOutput("order_behavior_analysis")),
                  tabPanel("Time of Day", plotlyOutput("daytime_conversion_trends")),
                  tabPanel("Weekday", plotlyOutput("weekday_analysis")),
                  tabPanel("Restaurant Type", plotlyOutput("restaurant_analysis")),
                  tabPanel("Interactive Map", withSpinner(leafletOutput("click_map", height = "500px")))
                )
              )
              
            )
    ),
    
    # Tab 2: Clicks vs Non-Clicks 
    tabItem(tabName = "insights2",
            h2("Clicks vs Non-Clicks"),
            
            # Row 1: Click rate and Clickers' profile (based on filters)
            fluidRow(
              valueBoxOutput("avg_click_rate_2", width = 3),
              box(title = "Who Clicks?", status = "primary", width = 12, collapsible = TRUE,
                  fluidRow(
                    column(width = 3, valueBoxOutput("avg_time_on_prev_2", width = 12)),
                    column(width = 3, valueBoxOutput("most_common_region_2", width = 12)),
                    column(width = 3, valueBoxOutput("most_used_social_2", width = 12)),
                    column(width = 3, valueBoxOutput("most_common_carrier_2", width = 12))
                  ),
                  fluidRow(
                    column(width = 3, valueBoxOutput("most_active_weekday_2", width = 12)),
                    column(width = 3, valueBoxOutput("most_common_time_2", width = 12)),
                    column(width = 3, valueBoxOutput("top_restaurant_2", width = 12)),
                    column(width = 3, valueBoxOutput("avg_orders_clickers_2", width = 12))
                  )
              )
            ),
            
            # Row 2: Filters and analysis
            fluidRow(
              # Filter options
              box(
                title = "Customer Profile", width = 3, status = "primary",
                selectInput("insights_region_2", "Region", choices = add_all_option(ClickTraining$Region)),
                selectInput("insights_carrier_2", "Carrier", choices = add_all_option(ClickTraining$Carrier)),
                selectInput("insights_social_2", "Social Network", choices = add_all_option(ClickTraining$Social_Network)),
                sliderInput("insights_daytime_2", "Daytime Range", min = 0, max = 24, value = c(0, 24), step = 1, ticks = FALSE, post = "h"),
                checkboxGroupInput(inputId = "insights_weekday_2", label = "Select Weekdays:", choices = weekday_labels, selected = weekday_labels, inline = TRUE),
                sliderInput("insights_time_prev_2", "Time on Previous Website (Minutes)", min = 0, max = 2000, value = c(0, 2000)),
                sliderInput("insights_num_orders_2", "Number of Previous Orders", min = 0, max = 15, value = c(0, 15), step = 1, ticks = TRUE),
                selectInput("insights_restaurant_2", "Restaurant Type", choices = add_all_option(ClickTraining$Restaurant_Type), multiple = TRUE, selected = "All"),
                # Filter action (apply or reset)
                fluidRow(
                  actionButton("apply_filters_2", "Apply Filters", class = "btn-primary"),
                  actionButton("reset_filters_2", "Reset Filters", class = "btn-secondary")
                )
              ),
              # Features exploration
              box(
                title = "Feature Analysis", width = 9, status = "primary",
                tabBox(
                  width = 12,
                  tabPanel("Carrier", plotlyOutput("click_percentage_carrier_2")),
                  tabPanel("Social Network", plotlyOutput("click_percentage_social_2")),
                  tabPanel("Time Spent on Previous Website", plotlyOutput("time_on_website_analysis_2")),
                  tabPanel("Order History", plotlyOutput("order_behavior_analysis_2")),
                  tabPanel("Time of Day", plotlyOutput("daytime_conversion_trends_2")),
                  tabPanel("Weekday", plotlyOutput("weekday_analysis_2")),
                  tabPanel("Restaurant Type", plotlyOutput("restaurant_analysis_2")),
                  tabPanel("Interactive Map", withSpinner(leafletOutput("click_map_2", height = "500px")))
                )
              )
            )
    ),
    
    # Tab 3: Predictions
    tabItem(tabName = "predictions",
            h2("Ad Click Predictions"),
            
            # Row 1: Upload customer data
            fluidRow(
              column(width = 12,
                     fileInput("upload_file", "Upload Customer Data (in .csv or .xlsx format)", accept = c(".csv", ".xlsx"), 
                               buttonLabel = "Select file"), 
                     actionButton("predict_button", "Generate Predictions", class = "btn-primary"),
                     div(style = "margin-bottom: 20px;")
              )
            ),
            
            # Row 2: Display clicks summary 
            fluidRow(
              
              # Average Click Rate
              valueBoxOutput("avg_click_rate_pred", width = 3),
              
              # Clickers' Profile Summary
              box(title = "Target Profiles", status = "primary", width = 12, collapsible = TRUE,
                  fluidRow(
                    column(width = 3, valueBoxOutput("avg_time_on_prev_pred", width = 12)),
                    column(width = 3, valueBoxOutput("most_common_region_pred", width = 12)),
                    column(width = 3, valueBoxOutput("most_used_social_pred", width = 12)),
                    column(width = 3, valueBoxOutput("most_common_carrier_pred", width = 12))
                  ),
                  fluidRow(
                    column(width = 3, valueBoxOutput("most_active_weekday_pred", width = 12)),
                    column(width = 3, valueBoxOutput("most_common_time_pred", width = 12)),
                    column(width = 3, valueBoxOutput("top_restaurant_pred", width = 12)),
                    column(width = 3, valueBoxOutput("avg_orders_clickers_pred", width = 12))
                  )
              )
              
            ),
            
            # Row 3: Filter options and Features exploration
            fluidRow(
              
              # Filters options
              box(
                title = "Customer Profile", width = 3, status = "primary",
                selectInput("pred_region", "Region", choices = add_all_option(ClickTraining$Region)),
                selectInput("pred_carrier", "Carrier", choices = add_all_option(ClickTraining$Carrier)),
                selectInput("pred_social", "Social Network", choices = add_all_option(ClickTraining$Social_Network)),
                sliderInput("pred_daytime", "Daytime Range", min = 0, max = 24, value = c(0, 24), step = 1, ticks = FALSE, post = "h"),
                checkboxGroupInput(inputId = "pred_weekday", label = "Select Weekdays:", choices = weekday_labels, selected = weekday_labels, inline = TRUE),
                sliderInput("pred_time_prev", "Time on Previous Website (Minutes)", min = 0, max = 2000, value = c(0, 2000)),
                sliderInput("pred_num_orders", "Number of Previous Orders", min = 0, max = 15, value = c(0, 15), step = 1, ticks = TRUE),
                selectInput("pred_restaurant", "Restaurant Type", choices = add_all_option(ClickTraining$Restaurant_Type), multiple = TRUE, selected = "All"),
                fluidRow(
                  actionButton("apply_filters_pred", "Apply Filters", class = "btn-primary"),
                  actionButton("reset_filters_pred", "Reset Filters", class = "btn-secondary")
                )
              ),
              
              # Features exploration
              box(
                title = "Feature Analysis", width = 9, status = "primary",
                tabBox(
                  width = 12,
                  tabPanel("Interactive Table", 
                           div(downloadButton("download_predictions", "Download Predictions (in CSV format)"),
                               style = "margin-bottom: 15px;"),
                           DTOutput("predicted_table", height = "400px")),
                  tabPanel("Carrier", plotlyOutput("carrier_pred_plot")),
                  tabPanel("Social Network", plotlyOutput("social_pred_pred_plot")),
                  tabPanel("Time Spent on Previous Website", plotlyOutput("time_on_website_analysis_pred_plot")),
                  tabPanel("Order History", plotlyOutput("order_behavior_analysis_pred_plot")),
                  tabPanel("Time of Day", plotlyOutput("daytime_conversion_trends_pred_plot")),
                  tabPanel("Weekday", plotlyOutput("weekday_analysis_pred_plot")),
                  tabPanel("Restaurant Type", plotlyOutput("restaurant_analysis_pred_plot")),
                  tabPanel("Interactive Map", withSpinner(leafletOutput("click_map_pred", height = "500px")))
                )
              )
            )
    )
  )
)

ui <- dashboardPage(header, sidebar, body)


# SERVER

server <- function(input, output, session) {
  
  # Display the pop-up when the app loads
  shinyalert(
    title = "Welcome to the Online Ad Click Predictor!",
    text = "This dashboard helps you analyze customer behavior and predict ad clicks. Here’s what each tab offers:\n
    1. Customer Insights: Freely explore customer behavior by appling filters and visualizing trends with graphs and maps.
    2. Clicks vs Non-Clicks: Compare customers who clicked versus those who didn’t to uncover key patterns.
    3. Ad Click Predictions: Upload customer data to predict ad clicks, review results, and download insights.\n
    Use the sidebar to navigate between tabs. Enjoy exploring!",
    type = "info",
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "Got it!"
  )
  
  #### 1. Customer Insights
  
  # Reactive data filtering based on button click
  
  filtered_data <- reactiveVal(ClickTraining)
  observeEvent(input$apply_filters, {
    # Load training data
    data <- ClickTraining
    # Apply filters
    if (input$insights_region != "All") {data <- data[data$Region == input$insights_region, ]}
    if (input$insights_carrier != "All") {data <- data[data$Carrier == input$insights_carrier, ]}
    if (length(input$insights_restaurant) > 0 && !"All" %in% input$insights_restaurant) {data <- data[data$Restaurant_Type %in% input$insights_restaurant, ]}
    if (input$insights_social != "All") {data <- data[data$Social_Network == input$insights_social, ]}
    data <- data[data$Daytime >= (input$insights_daytime[1] / 24) & data$Daytime <= (input$insights_daytime[2] / 24), ]
    data <- data[data$Time_On_Previous_Website >= input$insights_time_prev[1] & data$Time_On_Previous_Website <= input$insights_time_prev[2], ]
    if (length(input$insights_weekday) > 0) {data <- data[data$Weekday %in% input$insights_weekday, ]}
    data <- data[data$Number_of_Previous_Orders >= input$insights_num_orders[1] & data$Number_of_Previous_Orders <= input$insights_num_orders[2], ]
    if (input$insights_click_filter == "Clicks (1)") {
      data <- data[data$Clicks_Conversion == 1, ]
    } else if (input$insights_click_filter == "Non-Clicks (0)") {
      data <- data[data$Clicks_Conversion == 0, ]
    }
    filtered_data(data)
  })
  
  # Reset filters button : reset all filters to their default values
  observeEvent(input$reset_filters, {
    # buttons reset
    updateSelectInput(session, "insights_region", selected = "All")
    updateSelectInput(session, "insights_carrier", selected = "All")
    updateSelectInput(session, "insights_social", selected = "All")
    updateSliderInput(session, "insights_daytime", value = c(0, 24))
    updateCheckboxGroupInput(session, "insights_weekday", selected = weekday_labels)
    updateSliderInput(session, "insights_time_prev", value = c(0, 2000))
    updateSliderInput(session, "insights_num_orders", value = c(0, 15))
    updateSelectInput(session, "insights_restaurant", selected = "All")
    # data reset
    filtered_data(ClickTraining)
  })
  
  ## Features Exploration Tabs
  
  # 1. interactive datatable
  
  output$filtered_table <- renderDT({
    # Modify the data
    column_mapping <- c(
      "Region" = "Region",
      "Daytime" = "Daytime",
      "Carrier" = "Carrier",
      "Time_On_Previous_Website" = "Time on Website",
      "Weekday" = "Weekday",
      "Social_Network" = "Social Network",
      "Number_of_Previous_Orders" = "Previous Orders",
      "Clicks_Conversion" = "Clicks Conversion",
      "Restaurant_Type" = "Restaurant Type"
    )
    rounded_data <- filtered_data() %>%
      # rename columns
      rename_with(~ column_mapping[.x], everything()) %>%
      # Round numeric columns to 2 decimals
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
      as.data.frame(row.names = NULL)
    # Render the DataTable with additional options
    datatable(rounded_data, options = list(scrollX = TRUE, scrollY = "400px", whiteSpace = "nowrap"), rownames=FALSE)
  })
  
  # 2. graphs from all features
  
  output$click_percentage_carrier <- renderPlotly({
    # Calculate carrier data
    carrier_data <- table(filtered_data()$Carrier)
    percentages <- (carrier_data / sum(carrier_data)) * 100
    # Create an interactive Plotly bar chart
    plot_ly(
      x = names(carrier_data),        # Carrier names as x-axis labels
      y = percentages,               # Percentages as y-axis values
      type = "bar",
      text = paste0(round(percentages, 2), "%"),  # Add percentages as hover text
      hoverinfo = "text",
      textposition="none", # Only show hover text
      marker = list(color = custom_colors)  # Apply custom colors
    ) %>%
      layout(
        title = "Distribution by Carrier",
        xaxis = list(title = "Carrier"),
        yaxis = list(title = "Percentage (%)", range = c(0, 30)),  # Ensure y-axis ends at 100%
        bargap = 0.1,  # Slight gap between bars
        showlegend = FALSE  # No legend needed for a bar chart
      )
  })
  
  output$click_percentage_social <- renderPlotly({
    # Calculate social network data
    social_data <- table(filtered_data()$Social_Network)
    percentages <- (social_data / sum(social_data)) * 100
    # Create an interactive Plotly bar chart
    plot_ly(
      x = names(social_data),        # Social network names as x-axis labels
      y = percentages,               # Percentages as y-axis values
      type = "bar",
      text = paste0(round(percentages, 2), "%"),  # Add percentages as hover text
      textposition="none",
      hoverinfo = "text",             # Only show hover text
      marker = list(color = custom_colors)  # Apply custom colors
    ) %>%
      layout(
        title = "Distribution by Social Network",
        xaxis = list(title = "Social Network"),
        yaxis = list(title = "Percentage (%)", range = c(0, 40)),  # Ensure y-axis ends at 100%
        bargap = 0.1,  # Slight gap between bars
        showlegend = FALSE  # No legend needed for a bar chart
      )
  })
  
  output$time_on_website_analysis <- renderPlotly({
    # Extract the data
    time_data <- filtered_data()$Time_On_Previous_Website
    
    # Define bins
    bin_breaks <- seq(0, max(time_data, na.rm = TRUE), by = 100)  # Create bins of size 100 up to 1900
    binned_data <- cut(time_data, breaks = bin_breaks, include.lowest = TRUE, right = FALSE)
    
    # Aggregate counts and percentages
    bin_counts <- as.data.frame(table(binned_data))
    colnames(bin_counts) <- c("Bin", "Count")
    total_users <- sum(bin_counts$Count)
    bin_counts$Percentage <- round((bin_counts$Count / total_users) * 100, 2)
    
    # Format x-axis labels
    bin_labels <- paste0(head(bin_breaks, -1), "-", tail(bin_breaks, -1))
    bin_counts$Bin <- factor(bin_counts$Bin, levels = unique(bin_counts$Bin), labels = bin_labels)
    
    # Create an interactive Plotly bar chart
    plot_ly(
      data = bin_counts,
      x = ~Bin,
      y = ~Count,
      type = "bar",
      text = ~paste0(Percentage, "%"),  # Add percentage inside the bars
      textposition = "inside",
      textfont = list(color = "white"),
      hovertext = ~paste("Count:", Count, "<br>Frequency:", Percentage, "%"),  # Add counts and percentages in hover text
      hoverinfo = "text",
      textangle=270,
      marker = list(color = "#17a2b8")  # Use the custom teal color
    ) %>%
      layout(
        title = "Distribution by Time Spent on Previous Website",
        xaxis = list(
          title = "Time on Previous Website (seconds)",
          tickangle = -45,  # Rotate labels for better visibility
          automargin = TRUE
        ),
        yaxis = list(title = "Number of Users"),
        bargap = 0.1  # Slight gap between bars
      )
  })
  
  
  output$order_behavior_analysis <- renderPlotly({
    # Calculate order data (counts)
    order_data <- table(filtered_data()$Number_of_Previous_Orders)
    
    # Convert x-axis to factors with a defined order
    order_levels <- as.numeric(names(order_data))  # Ensure numeric order
    order_factors <- factor(order_levels, levels = order_levels, ordered = TRUE)
    
    # Calculate relative frequencies
    total_users <- sum(order_data)
    relative_frequencies <- round((order_data / total_users) * 100, 2)
    
    # Create an interactive Plotly bar chart with absolute and relative frequencies
    plot_ly(
      x = order_factors,
      y = as.numeric(order_data),
      type = "bar",
      textfont = list(color = "white"),
      text = ~paste0(relative_frequencies, "%"),  
      textposition = "inside",
      hovertext = ~paste("Count:", order_data, "<br>Frequency:", relative_frequencies, "%"),
      hoverinfo = "text",
      marker = list(color = "#007bff")
    ) %>%
      layout(
        title = "Distribution by Number of Previous Orders",
        xaxis = list(title = "Number of Previous Orders"),
        yaxis = list(title = "Number of Users"),  
        bargap = 0.1, 
        showlegend = FALSE  
      )
  })
  
  
  output$daytime_conversion_trends <- renderPlotly({
    # Calculate daytime data (counts)
    daytime_data <- filtered_data()
    daytime_data$Hour <- floor(daytime_data$Daytime * 24)  # Convert Daytime to Hour
    hour_data <- table(daytime_data$Hour)
    
    # Convert x-axis to factors with a defined order
    hour_levels <- as.numeric(names(hour_data))  # Ensure numeric order
    hour_factors <- factor(hour_levels, levels = hour_levels, ordered = TRUE)
    
    # Calculate relative frequencies
    total_users <- sum(hour_data)
    relative_frequencies <- round((hour_data / total_users) * 100, 2)
    
    # Create an interactive Plotly bar chart with consistent color
    plot_ly(
      x = hour_factors,             # Hour of the day as x-axis labels
      y = as.numeric(hour_data),    # Counts as y-axis values
      type = "bar",
      textfont = list(color = "black"),
      text = ~paste0(relative_frequencies, "%"),  
      textposition = "inside",
      hovertext = ~paste("Count:", hour_data, "<br>Frequency:", relative_frequencies, "%"),
      hoverinfo = "text",
      textangle = 90,
      marker = list(color = "#00c5ff")  # Single consistent color for all bars
    ) %>%
      layout(
        title = "Distribution by Time of Day",
        xaxis = list(title = "Hour of Day"),
        yaxis = list(title = "Number of Users"),  # Y-axis represents counts
        bargap = 0.1,  # Slight gap between bars
        showlegend = FALSE  # No legend needed for a bar chart
      )
  })  
  output$weekday_analysis <- renderPlotly({
    # Calculate weekday data
    weekday_labels <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    weekday_data <- table(factor(filtered_data()$Weekday, levels = weekday_labels))
    percentages <- round((weekday_data / sum(weekday_data)) * 100,2)
    
    # Create the interactive Plotly bar chart
    plot_ly(
      x = names(weekday_data), 
      y = percentages, 
      type = "bar",
      textfont = list(color = "white"),
      text = ~paste0(percentages, "%"),  
      textposition = "inside",
      hovertext = ~paste(weekday_data,"users (" , percentages, "%)"),
      hoverinfo = "text",
      textangle=0
      
    ) %>%
      layout(
        title = "Distribution by Weekday",
        xaxis = list(title = "Weekday", categoryorder = "array", categoryarray = weekday_labels),
        yaxis = list(title = "Percentage (%)"),
        hovermode = "closest"
      )
  })
  
  output$restaurant_analysis <- renderPlotly({
    data <- table(filtered_data()$Restaurant_Type)
    percentages <- (data / sum(data)) * 100
    # Create an interactive Plotly bar chart
    plot_ly(
      x = names(data),      
      y = percentages,        
      type = "bar",
      text = paste0(round(percentages, 2), "%"),  # Add percentages as hover text
      textposition="none",
      hoverinfo = "text",             # Only show hover text
      marker = list(color = custom_colors)  # Apply custom colors
    ) %>%
      layout(
        title = "Distribution by Restaurant Type",
        xaxis = list(title = "Restaurant Type"),
        yaxis = list(title = "Percentage (%)", range = c(0, 40)),  # Ensure y-axis ends at 100%
        bargap = 0.1,  # Slight gap between bars
        showlegend = FALSE  # No legend needed for a bar chart
      )
  })  
  
  output$click_map <- renderLeaflet({
    # Load the map data
    FRmap <- read_sf("MapData/regions-20180101.shp")
    
    # Generate the map using the function provided
    plot_click_map(filtered_data(), FRmap)
  })
  
  
  
  #### 2. Click vs Non-Clicks
  
  # Reactive data filtering for duplicated tab
  filtered_data_2 <- reactiveVal(ClickTraining)
  observeEvent(input$apply_filters_2, {
    data <- ClickTraining
    # Apply filters for duplicated tab
    if (input$insights_region_2 != "All") {data <- data[data$Region == input$insights_region_2, ]}
    if (input$insights_carrier_2 != "All") {data <- data[data$Carrier == input$insights_carrier_2, ]}
    if (length(input$insights_restaurant_2) > 0 && !"All" %in% input$insights_restaurant_2) {
      data <- data[data$Restaurant_Type %in% input$insights_restaurant_2, ]
    }
    if (input$insights_social_2 != "All") {data <- data[data$Social_Network == input$insights_social_2, ]}
    data <- data[data$Daytime >= (input$insights_daytime_2[1] / 24) & data$Daytime <= (input$insights_daytime_2[2] / 24), ]
    data <- data[data$Time_On_Previous_Website >= input$insights_time_prev_2[1] &
                   data$Time_On_Previous_Website <= input$insights_time_prev_2[2], ]
    if (length(input$insights_weekday_2) > 0) {data <- data[data$Weekday %in% input$insights_weekday_2, ]}
    data <- data[data$Number_of_Previous_Orders >= input$insights_num_orders_2[1] &
                   data$Number_of_Previous_Orders <= input$insights_num_orders_2[2], ]
    filtered_data_2(data)
  })
  
  filtered_click_data_2 <- reactive({
    filtered_data_2()[filtered_data_2()$Clicks_Conversion == 1, ]
  })
  
  # Reset filters button 
  observeEvent(input$reset_filters_2, {
    updateSelectInput(session, "insights_region_2", selected = "All")
    updateSelectInput(session, "insights_carrier_2", selected = "All")
    updateSelectInput(session, "insights_social_2", selected = "All")
    updateSliderInput(session, "insights_daytime_2", value = c(0, 24))
    updateCheckboxGroupInput(session, "insights_weekday_2", selected = weekday_labels)
    updateSliderInput(session, "insights_time_prev_2", value = c(0, 2000))
    updateSliderInput(session, "insights_num_orders_2", value = c(0, 15))
    updateSelectInput(session, "insights_restaurant_2", selected = "All")
    filtered_data_2(ClickTraining)
  })
  
  
  # ValueBox Outputs 
  output$avg_click_rate_2 <- renderValueBox({
    avg_click_rate <- mean(filtered_data_2()$Clicks_Conversion, na.rm = TRUE)
    valueBox(
      format(round(avg_click_rate * 100, 2), nsmall = 2),
      "Click Rate (%)",
      icon = icon("mouse-pointer"),
      color = "green"
    )
  })
  
  output$avg_time_on_prev_2 <- renderValueBox({
    avg_time_prev <- mean(filtered_click_data_2()$Time_On_Previous_Website, na.rm = TRUE)
    valueBox(
      format(round(avg_time_prev, 2), nsmall = 2),
      "Avg. Time on Previous Website (seconds)",
      icon = icon("clock"),
      color = "teal"
    )
  })
  
  output$most_common_region_2 <- renderValueBox({
    most_common_region <- names(sort(table(filtered_click_data_2()$Region), decreasing = TRUE))[1]
    valueBox(
      most_common_region,
      "Most common Region",
      icon = icon("globe"),
      color = "blue"
    )
  })
  
  output$most_common_carrier_2 <- renderValueBox({
    most_common_carrier <- names(sort(table(filtered_click_data_2()$Carrier), decreasing = TRUE))[1]
    valueBox(
      most_common_carrier,
      "Most common Carrier",
      icon = icon("signal"),
      color = "light-blue"
    )
  })
  
  output$most_used_social_2 <- renderValueBox({
    most_used_social <- names(sort(table(filtered_click_data_2()$Social_Network), decreasing = TRUE))[1]
    valueBox(
      most_used_social,
      "Most used Social",
      icon = icon("share-alt"),
      color = "aqua"
    )
  })
  
  output$most_active_weekday_2 <- renderValueBox({
    most_active_weekday <- names(sort(table(filtered_click_data_2()$Weekday), decreasing = TRUE))[1]
    valueBox(
      most_active_weekday,
      "Most active Weekday",
      icon = icon("calendar"),
      color = "aqua"
    )
  })
  
  output$most_common_time_2 <- renderValueBox({
    daytime_data <- filtered_click_data_2()
    daytime_data$Hour <- floor(daytime_data$Daytime * 24)
    most_common_hour <- as.numeric(names(sort(table(daytime_data$Hour), decreasing = TRUE))[1])
    time_bin <- sprintf("%02dh-%02dh", most_common_hour, most_common_hour + 1)
    valueBox(
      time_bin,
      "Most active Time of Day",
      icon = icon("clock"),
      color = "teal"
    )
  })
  
  output$top_restaurant_2 <- renderValueBox({
    top_restaurant <- names(sort(table(filtered_click_data_2()$Restaurant_Type), decreasing = TRUE))[1]
    valueBox(
      top_restaurant,
      "Top Restaurant",
      icon = icon("utensils"),
      color = "blue"
    )
  })
  
  output$avg_orders_clickers_2 <- renderValueBox({
    avg_orders <- mean(filtered_click_data_2()$Number_of_Previous_Orders, na.rm = TRUE)
    valueBox(
      format(round(avg_orders, 2), nsmall = 2),
      "Avg. number of Previous Orders",
      icon = icon("list-ol"),
      color = "aqua"
    )
  })
  
  
  ## Features exploration tabs

  output$click_percentage_carrier_2 <- renderPlotly({
    data <- filtered_data_2()
    # Aggregate the data by Carrier and Clicks Conversion
    carrier_data <- as.data.frame(table(data$Carrier, data$Clicks_Conversion))
    colnames(carrier_data) <- c("Carrier", "Clicks_Conversion", "Count")
    
    # Calculate percentages for each group within Carrier
    carrier_totals <- aggregate(Count ~ Carrier, data = carrier_data, sum)
    carrier_data <- merge(carrier_data, carrier_totals, by = "Carrier", suffixes = c("", "_Total"))
    carrier_data$Percentage <- round((carrier_data$Count / carrier_data$Count_Total) * 100, 2)
    
    # Map Clicks Conversion values to meaningful labels
    carrier_data$Clicks_Conversion <- factor(
      carrier_data$Clicks_Conversion,
      levels = c(0, 1),
      labels = c("No Click", "Click")
    )
    
    # Create an interactive stacked bar chart
    plot_ly(
      data = carrier_data,
      x = ~Carrier,                     
      y = ~Count,                       
      color = ~Clicks_Conversion,       
      type = "bar",
      textfont = list(color = "black"),
      text = ~paste0(Percentage, "%"),  
      textposition = "inside",
      hovertext = ~paste("Count:", Count, "<br>Frequency:", Percentage, "%"),
      hoverinfo = "text",             
      colors = c("No Click" = "#00c5ff", "Click"="#007bff")            
    ) %>%
      layout(
        title = "Distribution by Carrier",
        xaxis = list(title = ""),         
        yaxis = list(title = "Count"),    
        barmode = "stack",               
        legend = list(title = list(text = "Click Conversion"))
      )
  })
  
  output$click_percentage_social_2 <- renderPlotly({
    data <- filtered_data_2()
    
    social_data <- as.data.frame(table(data$Social_Network, data$Clicks_Conversion))
    colnames(social_data) <- c("Social_Network", "Clicks_Conversion", "Count")
    
    social_totals <- aggregate(Count ~ Social_Network, data = social_data, sum)
    social_data <- merge(social_data, social_totals, by = "Social_Network", suffixes = c("", "_Total"))
    social_data$Percentage <- round((social_data$Count / social_data$Count_Total) * 100, 2)
    
    social_data$Clicks_Conversion <- factor(
      social_data$Clicks_Conversion,
      levels = c(0, 1),
      labels = c("No Click", "Click")
    )
    
    plot_ly(
      data = social_data,
      x = ~Social_Network,
      y = ~Count,
      color = ~Clicks_Conversion,
      type = "bar",
      textfont = list(color = "black"),
      text = ~paste0(Percentage, "%"),
      textposition = "inside",
      hovertext = ~paste("Count:", Count, "<br>Frequency:", Percentage, "%"),
      hoverinfo = "text",
      colors = c("No Click" = "#00c5ff", "Click" = "#007bff")
    ) %>%
      layout(
        title = "Distribution by Social Network",
        xaxis = list(title = ""),
        yaxis = list(title = "Count"),
        barmode = "stack",
        legend = list(title = list(text = "Click Conversion"))
      )
  })
  
  output$time_on_website_analysis_2 <- renderPlotly({
    data <- filtered_data_2()
    
    # Define custom bins (19 bins of size 100)
    bin_breaks <- seq(0, max(data$Time_On_Previous_Website, na.rm = TRUE), by = 100)
    
    # Aggregate data into these bins
    time_data <- as.data.frame(table(cut(data$Time_On_Previous_Website, breaks = bin_breaks, include.lowest = TRUE), 
                                     data$Clicks_Conversion))
    colnames(time_data) <- c("Time_Spent", "Clicks_Conversion", "Count")
    
    # Format x-axis labels
    bin_labels <- paste0(head(bin_breaks, -1), " - ", tail(bin_breaks, -1))
    time_data$Time_Spent <- factor(time_data$Time_Spent, levels = levels(time_data$Time_Spent), labels = bin_labels)
    
    # Aggregate data
    time_totals <- aggregate(Count ~ Time_Spent, data = time_data, sum)
    time_data <- merge(time_data, time_totals, by = "Time_Spent", suffixes = c("", "_Total"))
    time_data$Percentage <- round((time_data$Count / time_data$Count_Total) * 100, 2)
    
    time_data$Clicks_Conversion <- factor(
      time_data$Clicks_Conversion,
      levels = c(0, 1),
      labels = c("No Click", "Click")
    )
    
    custom_colors <- c("No Click" = "#00c5ff", "Click" = "#007bff")
    
    plot_ly(
      data = time_data,
      x = ~Time_Spent,
      y = ~Count,
      color = ~Clicks_Conversion,
      type = "bar",
      textfont = list(color = "black"),
      textangle = 0,
      text = ~paste0(Percentage, "%"),
      hovertext = ~paste("Count:", Count, "<br>Frequency:", Percentage, "%"),
      hoverinfo = "text",
      colors = custom_colors
    ) %>%
      layout(
        title = "Distribution by Time Spent on Previous Website",
        xaxis = list(
          title = "Time Spent (seconds)",
          tickangle = -45,
          automargin = TRUE
        ),
        yaxis = list(title = "Count"),
        barmode = "stack",
        legend = list(title = list(text = "Click Conversion"))
      )
  })
  
  output$order_behavior_analysis_2 <- renderPlotly({
    data <- filtered_data_2()
    
    order_data <- as.data.frame(table(data$Number_of_Previous_Orders, data$Clicks_Conversion))
    colnames(order_data) <- c("Number_of_Previous_Orders", "Clicks_Conversion", "Count")
    
    order_totals <- aggregate(Count ~ Number_of_Previous_Orders, data = order_data, sum)
    order_data <- merge(order_data, order_totals, by = "Number_of_Previous_Orders", suffixes = c("", "_Total"))
    order_data$Percentage <- round((order_data$Count / order_data$Count_Total) * 100, 2)
    
    order_data$Clicks_Conversion <- factor(
      order_data$Clicks_Conversion,
      levels = c(0, 1),
      labels = c("No Click", "Click")
    )
    
    plot_ly(
      data = order_data,
      x = ~Number_of_Previous_Orders,
      y = ~Count,
      color = ~Clicks_Conversion,
      type = "bar",
      textfont = list(color = "black"),
      text = ~paste0(Percentage, "%"),
      textangle=0,
      textposition = "inside",
      hovertext = ~paste("Count:", Count, "<br>Frequency:", Percentage, "%"),
      hoverinfo = "text",
      colors = c("No Click" = "#00c5ff", "Click" = "#007bff")
    ) %>%
      layout(
        title = "Distribution by Number of Previous Orders",
        xaxis = list(title = "Number of Previous Orders"),
        yaxis = list(title = "Count"),
        barmode = "stack",
        legend = list(title = list(text = "Click Conversion"))
      )
  })
  
  output$daytime_conversion_trends_2 <- renderPlotly({
    # Preprocess data and create the "Hour" column
    daytime_data <- filtered_data_2()
    daytime_data$Hour <- floor(daytime_data$Daytime * 24)
    
    # Aggregate the data by Hour and Clicks Conversion
    hour_data <- as.data.frame(table(daytime_data$Hour, daytime_data$Clicks_Conversion))
    colnames(hour_data) <- c("Hour", "Clicks_Conversion", "Count")
    
    # Calculate total counts and percentages for each hour
    hour_totals <- aggregate(Count ~ Hour, data = hour_data, sum)
    hour_data <- merge(hour_data, hour_totals, by = "Hour", suffixes = c("", "_Total"))
    hour_data$Percentage <- round((hour_data$Count / hour_data$Count_Total) * 100, 2)
    
    # Map Clicks Conversion values to meaningful labels
    hour_data$Clicks_Conversion <- factor(
      hour_data$Clicks_Conversion,
      levels = c(0, 1),
      labels = c("No Click", "Click")
    )
    
    # Plotly stacked bar chart
    plot_ly(
      data = hour_data,
      x = ~Hour,
      y = ~Count,
      color = ~Clicks_Conversion,
      type = "bar",
      textfont = list(color = "black"),
      text = ~paste0(Percentage, "%"),  # Add percentage inside the bars
      textposition = "inside",
      textangle=90,
      hovertext = ~paste("Count:", Count, "<br>Frequency:", Percentage, "%"),  # Add hover info
      hoverinfo = "text",
      colors = c("No Click" = "#00c5ff", "Click" = "#007bff")
    ) %>%
      layout(
        title = "Distribution by Time of Day (Hourly)",
        xaxis = list(
          title = "Time of Day (Hours)",
          dtick = 1  # Ensure x-axis ticks are integers (hourly)
        ),
        yaxis = list(title = "Count"),
        barmode = "stack",
        legend = list(title = list(text = "Click Conversion"))
      )
  })
  
  output$weekday_analysis_2 <- renderPlotly({
    # Ensure weekdays are in the correct order
    weekday_data <- filtered_data_2()
    weekday_data$Weekday <- factor(weekday_data$Weekday, levels = weekday_labels)
    
    # Aggregate the data by Weekday and Clicks Conversion
    weekday_counts <- as.data.frame(table(weekday_data$Weekday, weekday_data$Clicks_Conversion))
    colnames(weekday_counts) <- c("Weekday", "Clicks_Conversion", "Count")
    
    # Calculate total counts and percentages for each weekday
    weekday_totals <- aggregate(Count ~ Weekday, data = weekday_counts, sum)
    weekday_counts <- merge(weekday_counts, weekday_totals, by = "Weekday", suffixes = c("", "_Total"))
    weekday_counts$Percentage <- round((weekday_counts$Count / weekday_counts$Count_Total) * 100, 2)
    
    # Map Clicks Conversion values to meaningful labels
    weekday_counts$Clicks_Conversion <- factor(
      weekday_counts$Clicks_Conversion,
      levels = c(0, 1),
      labels = c("No Click", "Click")
    )
    
    # Plotly stacked bar chart
    plot_ly(
      data = weekday_counts,
      x = ~Weekday,
      y = ~Count,
      color = ~Clicks_Conversion,
      type = "bar",
      text = ~paste0(Percentage, "%"),  # Add percentage inside the bars
      textposition = "inside",
      textangle=0,
      textfont = list(color = "black"),
      hovertext = ~paste("Count:", Count, "<br>Percentage:", Percentage, "%"),  # Add hover info
      hoverinfo = "text",
      colors = c("No Click" = "#00c5ff", "Click" = "#007bff")
    ) %>%
      layout(
        title = "Distribution by Weekday",
        xaxis = list(
          title = "Weekday"
        ),
        yaxis = list(title = "Count"),
        barmode = "stack",
        legend = list(title = list(text = "Click Conversion"))
      )
  })
  
  output$restaurant_analysis_2 <- renderPlotly({
    # Filtered data
    restaurant_data <- filtered_data_2()
    
    # Aggregate the data by Restaurant Type and Clicks Conversion
    restaurant_counts <- as.data.frame(table(restaurant_data$Restaurant_Type, restaurant_data$Clicks_Conversion))
    colnames(restaurant_counts) <- c("Restaurant_Type", "Clicks_Conversion", "Count")
    
    # Calculate total counts and percentages for each restaurant type
    restaurant_totals <- aggregate(Count ~ Restaurant_Type, data = restaurant_counts, sum)
    restaurant_counts <- merge(restaurant_counts, restaurant_totals, by = "Restaurant_Type", suffixes = c("", "_Total"))
    restaurant_counts$Percentage <- round((restaurant_counts$Count / restaurant_counts$Count_Total) * 100, 2)
    
    # Map Clicks Conversion values to meaningful labels
    restaurant_counts$Clicks_Conversion <- factor(
      restaurant_counts$Clicks_Conversion,
      levels = c(0, 1),
      labels = c("No Click", "Click")
    )
    
    # Plotly stacked bar chart
    plot_ly(
      data = restaurant_counts,
      x = ~Restaurant_Type,
      y = ~Count,
      color = ~Clicks_Conversion,
      type = "bar",
      text = ~paste0(Percentage, "%"),  # Add percentage inside the bars
      textposition = "inside",
      textangle=0,
      textfont = list(color = "black"),
      hovertext = ~paste("Count:", Count, "<br>Percentage:", Percentage, "%"),  # Add hover info
      hoverinfo = "text",
      colors = c("No Click" = "#00c5ff", "Click" = "#007bff")
    ) %>%
      layout(
        title = "Distribution by Restaurant Type",
        xaxis = list(title = "Restaurant Type"),
        yaxis = list(title = "Count"),
        barmode = "stack",
        legend = list(title = list(text = "Click Conversion"))
      )
  })

  output$click_map_2 <- renderLeaflet({
    # Load the map data
    FRmap <- read_sf("MapData/regions-20180101.shp")
    
    # Generate the map using the function provided
    plot_click_map(filtered_data_2(), FRmap)
  })  
  
  #### 3. Predictions tab
  
  # Reactive values for uploaded data and predictions
  customer_data <- reactiveVal()
  predicted_data <- reactiveVal()
  filtered_data_pred <- reactiveVal()
  
  # Upload data box
  observeEvent(input$upload_file, {
    req(input$upload_file)
    ext <- tools::file_ext(input$upload_file$name)
    uploaded_data <- if (ext == "csv") {
      read.csv(input$upload_file$datapath)
    } else if (ext %in% c("xlsx", "xls")) {
      readxl::read_excel(input$upload_file$datapath)
    } else {
      showNotification("Invalid file type. Please upload a .csv or .xlsx file.", type = "error")
      return(NULL)
    }
    uploaded_data <- modify_missing_values(uploaded_data, "Restaurant_Type", "Unknown")
    customer_data(uploaded_data)
  })
  
  # Predict button
  observeEvent(input$predict_button, {
    req(customer_data())
    predicted_data(prediction_model(customer_data())) # Populate predicted_data
    filtered_data_pred(predicted_data())            # Set the initial filtered_data_pred
  })
  
  # Reactive data filtering based on button click
  observeEvent(input$apply_filters_pred, {
    data <- filtered_data_pred()
    # Filter by Region
    if (input$pred_region != "All") {data <- data[data$Region == input$pred_region, ]}
    # Filter by Carrier
    if (input$pred_carrier != "All") {data <- data[data$Carrier == input$pred_carrier, ]}
    # Filter by Restaurant Type
    if (length(input$pred_restaurant) > 0 && !"All" %in% input$pred_restaurant) {data <- data[data$Restaurant_Type %in% input$pred_restaurant, ]}
    # Filter by Social Network
    if (input$pred_social != "All") {data <- data[data$Social_Network == input$pred_social, ]}
    # Filter by Daytime Range
    data <- data[data$Daytime >= (input$pred_daytime[1] / 24) & data$Daytime <= (input$pred_daytime[2] / 24), ]
    # Filter by Time on Previous Website
    data <- data[data$Time_On_Previous_Website >= input$pred_time_prev[1] & data$Time_On_Previous_Website <= input$pred_time_prev[2], ]
    # Filter by Selected Weekdays
    if (length(input$pred_weekday) > 0) {data <- data[data$Weekday %in% input$pred_weekday, ]}
    # Filter by Number of Previous Orders
    data <- data[data$Number_of_Previous_Orders >= input$pred_num_orders[1] & data$Number_of_Previous_Orders <= input$pred_num_orders[2], ]
    filtered_data_pred(data)
  })
  
  filtered_click_data_pred <- reactive({
    data <- filtered_data_pred()  
    data <- data[data$Clicks_Conversion == 1, ]  
    return(data)
  })
  
  filtered_non_click_data_pred <- reactive({
    data <- filtered_data_pred()  
    data <- data[data$Clicks_Conversion == 1, ]  
    return(data)
  })
  
  # Reset all filters to their default values
  observeEvent(input$reset_filters_pred, {
    updateSelectInput(session, "pred_region", selected = "All")
    updateSelectInput(session, "pred_carrier", selected = "All")
    updateSelectInput(session, "pred_social", selected = "All")
    updateSliderInput(session, "pred_daytime", value = c(0, 24))
    updateCheckboxGroupInput(session, "pred_weekday", selected = weekday_labels)
    updateSliderInput(session, "pred_time_prev", value = c(0, 2000))
    updateSliderInput(session, "pred_num_orders", value = c(0, 15))
    updateSelectInput(session, "pred_restaurant", selected = "All")
    # Reset the filtered data to the full dataset
    filtered_data_pred(predicted_data())
  })
  
  
  # 1. Summary statistics outputs
  
  output$avg_click_rate_pred <- renderValueBox({
    req(filtered_data_pred())  # Ensure data exists before proceeding
    
    data <- filtered_data_pred()
    
    # Ensure Clicks_Conversion_Prediction exists and convert if necessary
    if ("Clicks_Conversion_Prediction" %in% colnames(data)) {
      # Force conversion to numeric, handling strings or factors
      data$Clicks_Conversion_Prediction <- as.numeric(as.character(data$Clicks_Conversion_Prediction))
      
      # Debug to confirm correct conversion
      print("Converted Clicks_Conversion_Prediction:")
      print(table(data$Clicks_Conversion_Prediction, useNA = "ifany"))
      
      # Calculate click rate
      clicks <- sum(data$Clicks_Conversion_Prediction == 1, na.rm = TRUE)
      total <- nrow(data)
      click_rate <- (clicks / total) * 100
    } else {
      click_rate <- 0
    }
    
    valueBox(
      paste0(format(round(click_rate, 2), nsmall = 2), "%"),
      "Click Rate (%)",
      icon = icon("mouse-pointer"),
      color = "green"
    )
  })
  
  
  output$avg_time_on_prev_pred <- renderValueBox({
    avg_time_on_prev_pred <- mean(filtered_click_data_pred()$Time_On_Previous_Website, na.rm = TRUE)
    valueBox(
      format(round(avg_time_on_prev_pred, 2), nsmall = 2), 
      "Avg. Time on Previous Website (seconds)", 
      icon = icon("clock"), 
      color = "teal"
    )
  })
  
  output$most_common_region_pred <- renderValueBox({
    most_common_region_pred <- names(sort(table(filtered_click_data_pred()$Region), decreasing = TRUE))[1]
    valueBox(
      most_common_region_pred, 
      "Most common Region", 
      icon = icon("globe"), 
      color = "blue"
    )
  })
  
  output$most_common_carrier_pred <- renderValueBox({
    most_common_carrier_pred <- names(sort(table(filtered_click_data_pred()$Carrier), decreasing = TRUE))[1]
    valueBox(
      most_common_carrier_pred, 
      "Most common Carrier", 
      icon = icon("signal"), 
      color = "light-blue"
    )
  })
  
  output$most_used_social_pred <- renderValueBox({
    most_used_social_pred <- names(sort(table(filtered_click_data_pred()$Social_Network), decreasing = TRUE))[1]
    valueBox(
      most_used_social_pred, 
      "Most used Social", 
      icon = icon("share-alt"), 
      color = "aqua"
    )
  })
  
  output$most_active_weekday_pred <- renderValueBox({
    most_active_weekday_pred <- names(sort(table(filtered_click_data_pred()$Weekday), decreasing = TRUE))[1]
    valueBox(
      most_active_weekday_pred, 
      "Most active Weekday", 
      icon = icon("calendar"), 
      color = "aqua"
    )
  })
  
  output$most_common_time_pred <- renderValueBox({
    # Calculate the hour with the highest number of clicks
    daytime_data <- filtered_click_data_pred()
    daytime_data$Hour <- floor(daytime_data$Daytime * 24) # Convert to hour bins
    most_common_hour <- as.numeric(names(sort(table(daytime_data$Hour), decreasing = TRUE))[1]) 
    time_bin <- sprintf("%02dh-%02dh", most_common_hour, most_common_hour + 1)     # Format time bin
    valueBox(
      time_bin, 
      "Most active Time of Day", 
      icon = icon("clock"), 
      color = "teal"
    )
  })
  
  output$top_restaurant_pred <- renderValueBox({
    top_restaurant <- names(sort(table(filtered_click_data_pred()$Restaurant_Type), decreasing = TRUE))[1]
    valueBox(
      top_restaurant, 
      "Top Restaurant", 
      icon = icon("utensils"), 
      color = "blue"
    )
  })
  
  output$avg_orders_clickers_pred <- renderValueBox({
    avg_orders <- mean(filtered_click_data_pred()$Number_of_Previous_Orders, na.rm = TRUE)
    valueBox(
      format(round(avg_orders, 2), nsmall = 2), 
      "Avg. number of Previous Orders", 
      icon = icon("list-ol"), 
      color = "aqua"
    )
  })
  
  # 2. Customer insights tab 
  
  # Interactive datatable
  output$predicted_table <- renderDT({
    
    
    # Modify the data
    column_mapping_pred <- c(
      "Region" = "Region",
      "Daytime" = "Daytime",
      "Carrier" = "Carrier",
      "Time_On_Previous_Website" = "Time on Website",
      "Weekday" = "Weekday",
      "Social_Network" = "Social Network",
      "Number_of_Previous_Orders" = "Previous Orders",
      "Clicks_Conversion_Prediction" = "Predicted Clicks Conversion",
      "Restaurant_Type" = "Restaurant Type"
    )
    rounded_data <- filtered_data_pred() %>%
      # rename columns
      rename_with(~ column_mapping_pred[.x], everything()) %>%
      # Round numeric columns to 2 decimals
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
      as.data.frame(row.names = NULL)
    # Render the DataTable with additional options
    datatable(rounded_data, options = list(scrollX = TRUE, scrollY = "400px", whiteSpace = "nowrap"), rownames=FALSE)
  })
  
  # dowload prediction file
  output$download_predictions <- downloadHandler(
    filename = function() {
      paste("Predicted_Data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(filtered_data_pred())
      write.csv(filtered_data_pred(), file, row.names = FALSE)
    }
  )

  
  # Feature analysis plots
  
  output$carrier_pred_plot <- renderPlotly({
    data <- filtered_data_pred()
    # Aggregate the data by Carrier and Clicks Conversion
    carrier_data <- as.data.frame(table(data$Carrier, data$Clicks_Conversion))
    colnames(carrier_data) <- c("Carrier", "Clicks_Conversion", "Count")
    
    # Calculate percentages for each group within Carrier
    carrier_totals <- aggregate(Count ~ Carrier, data = carrier_data, sum)
    carrier_data <- merge(carrier_data, carrier_totals, by = "Carrier", suffixes = c("", "_Total"))
    carrier_data$Percentage <- round((carrier_data$Count / carrier_data$Count_Total) * 100, 2)
    
    # Map Clicks Conversion values to meaningful labels
    carrier_data$Clicks_Conversion <- factor(
      carrier_data$Clicks_Conversion,
      levels = c(0, 1),
      labels = c("No Click", "Click")
    )
    
    # Create an interactive stacked bar chart
    plot_ly(
      data = carrier_data,
      x = ~Carrier,                     
      y = ~Count,                       
      color = ~Clicks_Conversion,       
      type = "bar",
      textfont = list(color = "black"),
      text = ~paste0(Percentage, "%"),  
      textposition = "inside",
      hovertext = ~paste("Count:", Count, "<br>Frequency:", Percentage, "%"),
      hoverinfo = "text",             
      colors = c("No Click" = "#00c5ff", "Click"="#007bff")            
    ) %>%
      layout(
        title = "Distribution by Carrier",
        xaxis = list(title = ""),         
        yaxis = list(title = "Count"),    
        barmode = "stack",               
        legend = list(title = list(text = "Click Conversion"))
      )
  })
  
  
  output$social_pred_pred_plot <- renderPlotly({
    data <- filtered_data_pred()
    
    social_data <- as.data.frame(table(data$Social_Network, data$Clicks_Conversion))
    colnames(social_data) <- c("Social_Network", "Clicks_Conversion", "Count")
    
    social_totals <- aggregate(Count ~ Social_Network, data = social_data, sum)
    social_data <- merge(social_data, social_totals, by = "Social_Network", suffixes = c("", "_Total"))
    social_data$Percentage <- round((social_data$Count / social_data$Count_Total) * 100, 2)
    
    social_data$Clicks_Conversion <- factor(
      social_data$Clicks_Conversion,
      levels = c(0, 1),
      labels = c("No Click", "Click")
    )
    
    plot_ly(
      data = social_data,
      x = ~Social_Network,
      y = ~Count,
      color = ~Clicks_Conversion,
      type = "bar",
      textfont = list(color = "black"),
      text = ~paste0(Percentage, "%"),
      textposition = "inside",
      hovertext = ~paste("Count:", Count, "<br>Frequency:", Percentage, "%"),
      hoverinfo = "text",
      colors = c("No Click" = "#00c5ff", "Click" = "#007bff")
    ) %>%
      layout(
        title = "Distribution by Social Network",
        xaxis = list(title = ""),
        yaxis = list(title = "Count"),
        barmode = "stack",
        legend = list(title = list(text = "Click Conversion"))
      )
  })
  
  
  output$time_on_website_analysis_pred_plot <- renderPlotly({
    data <- filtered_data_pred()
    
    # Define custom bins (19 bins of size 100)
    bin_breaks <- seq(0, max(data$Time_On_Previous_Website, na.rm = TRUE), by = 100)
    
    # Aggregate data into these bins
    time_data <- as.data.frame(table(cut(data$Time_On_Previous_Website, breaks = bin_breaks, include.lowest = TRUE), 
                                     data$Clicks_Conversion))
    colnames(time_data) <- c("Time_Spent", "Clicks_Conversion", "Count")
    
    # Format x-axis labels
    bin_labels <- paste0(head(bin_breaks, -1), " - ", tail(bin_breaks, -1))
    time_data$Time_Spent <- factor(time_data$Time_Spent, levels = levels(time_data$Time_Spent), labels = bin_labels)
    
    # Aggregate data
    time_totals <- aggregate(Count ~ Time_Spent, data = time_data, sum)
    time_data <- merge(time_data, time_totals, by = "Time_Spent", suffixes = c("", "_Total"))
    time_data$Percentage <- round((time_data$Count / time_data$Count_Total) * 100, 2)
    
    time_data$Clicks_Conversion <- factor(
      time_data$Clicks_Conversion,
      levels = c(0, 1),
      labels = c("No Click", "Click")
    )
    
    custom_colors <- c("No Click" = "#00c5ff", "Click" = "#007bff")
    
    plot_ly(
      data = time_data,
      x = ~Time_Spent,
      y = ~Count,
      color = ~Clicks_Conversion,
      type = "bar",
      textfont = list(color = "black"),
      textangle = 0,
      text = ~paste0(Percentage, "%"),
      hovertext = ~paste("Count:", Count, "<br>Frequency:", Percentage, "%"),
      hoverinfo = "text",
      colors = custom_colors
    ) %>%
      layout(
        title = "Distribution by Time Spent on Previous Website",
        xaxis = list(
          title = "Time Spent (seconds)",
          tickangle = -45,
          automargin = TRUE
        ),
        yaxis = list(title = "Count"),
        barmode = "stack",
        legend = list(title = list(text = "Click Conversion"))
      )
  })
  
  
  
  output$order_behavior_analysis_pred_plot <- renderPlotly({
    data <- filtered_data_pred()
    order_data <- as.data.frame(table(data$Number_of_Previous_Orders, data$Clicks_Conversion))
    colnames(order_data) <- c("Number_of_Previous_Orders", "Clicks_Conversion", "Count")
    
    order_totals <- aggregate(Count ~ Number_of_Previous_Orders, data = order_data, sum)
    order_data <- merge(order_data, order_totals, by = "Number_of_Previous_Orders", suffixes = c("", "_Total"))
    order_data$Percentage <- round((order_data$Count / order_data$Count_Total) * 100, 2)
    
    order_data$Clicks_Conversion <- factor(
      order_data$Clicks_Conversion,
      levels = c(0, 1),
      labels = c("No Click", "Click")
    )
    
    plot_ly(
      data = order_data,
      x = ~Number_of_Previous_Orders,
      y = ~Count,
      color = ~Clicks_Conversion,
      type = "bar",
      textfont = list(color = "black"),
      text = ~paste0(Percentage, "%"),
      textangle=0,
      textposition = "inside",
      hovertext = ~paste("Count:", Count, "<br>Frequency:", Percentage, "%"),
      hoverinfo = "text",
      colors = c("No Click" = "#00c5ff", "Click" = "#007bff")
    ) %>%
      layout(
        title = "Distribution by Number of Previous Orders",
        xaxis = list(title = "Number of Previous Orders"),
        yaxis = list(title = "Count"),
        barmode = "stack",
        legend = list(title = list(text = "Click Conversion"))
      )
  })
  
  
  output$daytime_conversion_trends_pred_plot <- renderPlotly({
    # Preprocess data and create the "Hour" column
    daytime_data <- filtered_data_pred()
    daytime_data$Hour <- floor(daytime_data$Daytime * 24)
    
    # Aggregate the data by Hour and Clicks Conversion
    hour_data <- as.data.frame(table(daytime_data$Hour, daytime_data$Clicks_Conversion))
    colnames(hour_data) <- c("Hour", "Clicks_Conversion", "Count")
    
    # Calculate total counts and percentages for each hour
    hour_totals <- aggregate(Count ~ Hour, data = hour_data, sum)
    hour_data <- merge(hour_data, hour_totals, by = "Hour", suffixes = c("", "_Total"))
    hour_data$Percentage <- round((hour_data$Count / hour_data$Count_Total) * 100, 2)
    
    # Map Clicks Conversion values to meaningful labels
    hour_data$Clicks_Conversion <- factor(
      hour_data$Clicks_Conversion,
      levels = c(0, 1),
      labels = c("No Click", "Click")
    )
    
    # Plotly stacked bar chart
    plot_ly(
      data = hour_data,
      x = ~Hour,
      y = ~Count,
      color = ~Clicks_Conversion,
      type = "bar",
      textfont = list(color = "black"),
      text = ~paste0(Percentage, "%"),  # Add percentage inside the bars
      textposition = "inside",
      textangle=90,
      hovertext = ~paste("Count:", Count, "<br>Frequency:", Percentage, "%"),  # Add hover info
      hoverinfo = "text",
      colors = c("No Click" = "#00c5ff", "Click" = "#007bff")
    ) %>%
      layout(
        title = "Distribution by Time of Day (Hourly)",
        xaxis = list(
          title = "Time of Day (Hours)",
          dtick = 1  # Ensure x-axis ticks are integers (hourly)
        ),
        yaxis = list(title = "Count"),
        barmode = "stack",
        legend = list(title = list(text = "Click Conversion"))
      )
  })
  
  
  output$weekday_analysis_pred_plot <- renderPlotly({
    # Ensure weekdays are in the correct order
    weekday_data <- filtered_data_pred()
    weekday_data$Weekday <- factor(weekday_data$Weekday, levels = weekday_labels)
    
    # Aggregate the data by Weekday and Clicks Conversion
    weekday_counts <- as.data.frame(table(weekday_data$Weekday, weekday_data$Clicks_Conversion))
    colnames(weekday_counts) <- c("Weekday", "Clicks_Conversion", "Count")

    # Calculate total counts and percentages for each weekday
    weekday_totals <- aggregate(Count ~ Weekday, data = weekday_counts, sum)
    weekday_counts <- merge(weekday_counts, weekday_totals, by = "Weekday", suffixes = c("", "_Total"))
    weekday_counts$Percentage <- round((weekday_counts$Count / weekday_counts$Count_Total) * 100, 2)
    
    # Map Clicks Conversion values to meaningful labels
    weekday_counts$Clicks_Conversion <- factor(
      weekday_counts$Clicks_Conversion,
      levels = c(0, 1),
      labels = c("No Click", "Click")
    )
    
    # Plotly stacked bar chart
    plot_ly(
      data = weekday_counts,
      x = ~Weekday,
      y = ~Count,
      color = ~Clicks_Conversion,
      type = "bar",
      text = ~paste0(Percentage, "%"),  # Add percentage inside the bars
      textposition = "inside",
      textangle=0,
      textfont = list(color = "black"),
      hovertext = ~paste("Count:", Count, "<br>Percentage:", Percentage, "%"),  # Add hover info
      hoverinfo = "text",
      colors = c("No Click" = "#00c5ff", "Click" = "#007bff")
    ) %>%
      layout(
        title = "Distribution by Weekday",
        xaxis = list(
          title = "Weekday"
        ),
        yaxis = list(title = "Count"),
        barmode = "stack",
        legend = list(title = list(text = "Click Conversion"))
      )
  })
  
  
  output$restaurant_analysis_pred_plot <- renderPlotly({
    # Filtered data
    restaurant_data <- filtered_data_pred()
    
    # Aggregate the data by Restaurant Type and Clicks Conversion
    restaurant_counts <- as.data.frame(table(restaurant_data$Restaurant_Type, restaurant_data$Clicks_Conversion))
    colnames(restaurant_counts) <- c("Restaurant_Type", "Clicks_Conversion", "Count")
    
    # Calculate total counts and percentages for each restaurant type
    restaurant_totals <- aggregate(Count ~ Restaurant_Type, data = restaurant_counts, sum)
    restaurant_counts <- merge(restaurant_counts, restaurant_totals, by = "Restaurant_Type", suffixes = c("", "_Total"))
    restaurant_counts$Percentage <- round((restaurant_counts$Count / restaurant_counts$Count_Total) * 100, 2)
    
    # Map Clicks Conversion values to meaningful labels
    restaurant_counts$Clicks_Conversion <- factor(
      restaurant_counts$Clicks_Conversion,
      levels = c(0, 1),
      labels = c("No Click", "Click")
    )
    
    # Plotly stacked bar chart
    plot_ly(
      data = restaurant_counts,
      x = ~Restaurant_Type,
      y = ~Count,
      color = ~Clicks_Conversion,
      type = "bar",
      text = ~paste0(Percentage, "%"),  # Add percentage inside the bars
      textposition = "inside",
      textangle=0,
      textfont = list(color = "black"),
      hovertext = ~paste("Count:", Count, "<br>Percentage:", Percentage, "%"),  # Add hover info
      hoverinfo = "text",
      colors = c("No Click" = "#00c5ff", "Click" = "#007bff")
    ) %>%
      layout(
        title = "Distribution by Restaurant Type",
        xaxis = list(title = "Restaurant Type"),
        yaxis = list(title = "Count"),
        barmode = "stack",
        legend = list(title = list(text = "Click Conversion"))
      )
  })
  
  output$click_map_pred <- renderLeaflet({
    # Load the map data
    FRmap <- read_sf("MapData/regions-20180101.shp")
    
    # Generate the map using the function provided
    plot_click_map(filtered_data_pred(), FRmap, click_column = "Clicks_Conversion_Prediction")
  })

  
  observe({
    if (!is.null(filtered_data_pred())) {
      cat("Column names in filtered_data_pred():\n")
      print(colnames(filtered_data_pred()))
    } else {
      cat("filtered_data_pred() is NULL.\n")
    }
  })
  
}

# Run App
shinyApp(ui, server)
