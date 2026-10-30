
# NISR 2026 Big Data Hackathon
# Agricultural Extension and Crop Productivity in Rwanda
install.packages(c("shiny", "dplyr", "ggplot2", "DT"))
library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

# Load the CSV from the same folder as app.R
dat <- read.csv(
  "seasonA_province.csv",
  stringsAsFactors = FALSE
)

# Check the columns required by this dashboard
required <- c(
  "hhid",
  "crop_name",
  "extension_access",
  "household_median_kg_ha",
  "province"
)

missing_cols <- setdiff(required, names(dat))

if (length(missing_cols) > 0) {
  stop(
    paste(
      "Missing columns:",
      paste(missing_cols, collapse = ", ")
    )
  )
}

# Prepare data
dat <- dat %>%
  mutate(
    productivity = as.numeric(household_median_kg_ha),
    extension_label = case_when(
      tolower(trimws(as.character(extension_access))) %in%
        c("1", "yes", "true") ~ "Access",
      tolower(trimws(as.character(extension_access))) %in%
        c("0", "no", "false") ~ "No access",
      TRUE ~ as.character(extension_access)
    )
  )

# User interface
ui <- fluidPage(
  titlePanel(
    "Agricultural Extension and Crop Productivity in Rwanda"
  ),
  
  p("NISR 2026 Big Data Hackathon | AHS 2024, Season A"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "crop",
        "Select crop:",
        choices = c("All crops", sort(unique(dat$crop_name)))
      ),
      
      selectInput(
        "extension",
        "Extension service:",
        choices = c(
          "All groups",
          sort(unique(na.omit(dat$extension_label)))
        )
      ),
      
      selectInput(
        "province",
        "Select province:",
        choices = c(
          "All provinces",
          sort(unique(na.omit(dat$province)))
        )
      )
    ),
    
    mainPanel(
      h3("Summary indicators"),
      
      fluidRow(
        column(
          4,
          wellPanel(
            h4("Observations"),
            textOutput("n_obs")
          )
        ),
        column(
          4,
          wellPanel(
            h4("Households"),
            textOutput("n_households")
          )
        ),
        column(
          4,
          wellPanel(
            h4("Median productivity"),
            textOutput("median_yield")
          )
        )
      ),
      
      h3("Productivity by extension access"),
      plotOutput("productivity_plot", height = "400px"),
      
      h3("Median productivity by province"),
      plotOutput("province_plot", height = "350px"),
      
      h3("Filtered data"),
      DTOutput("data_table")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    x <- dat
    
    if (input$crop != "All crops") {
      x <- x %>% filter(crop_name == input$crop)
    }
    
    if (input$extension != "All groups") {
      x <- x %>% filter(extension_label == input$extension)
    }
    
    if (input$province != "All provinces") {
      x <- x %>% filter(province == input$province)
    }
    
    x
  })
  
  output$n_obs <- renderText({
    format(nrow(filtered_data()), big.mark = ",")
  })
  
  output$n_households <- renderText({
    format(
      n_distinct(filtered_data()$hhid, na.rm = TRUE),
      big.mark = ","
    )
  })
  
  output$median_yield <- renderText({
    y <- filtered_data()$productivity
    y <- y[is.finite(y) & y >= 0]
    
    if (length(y) == 0) {
      return("Not available")
    }
    
    paste(
      format(round(median(y), 2), big.mark = ","),
      "kg/ha"
    )
  })
  
  output$productivity_plot <- renderPlot({
    x <- filtered_data() %>%
      filter(
        !is.na(extension_label),
        is.finite(productivity),
        productivity >= 0
      )
    
    validate(
      need(nrow(x) > 0, "No data available for these filters.")
    )
    
    ggplot(
      x,
      aes(x = extension_label, y = productivity)
    ) +
      geom_boxplot(na.rm = TRUE) +
      labs(
        title = "Crop productivity by extension access",
        x = "Extension-service access",
        y = "Productivity (kg/ha)"
      ) +
      theme_minimal()
  })
  
  output$province_plot <- renderPlot({
    x <- filtered_data() %>%
      filter(
        !is.na(province),
        is.finite(productivity),
        productivity >= 0
      ) %>%
      group_by(province) %>%
      summarise(
        median_productivity = median(productivity),
        .groups = "drop"
      )
    
    validate(
      need(nrow(x) > 0, "No data available for these filters.")
    )
    
    ggplot(
      x,
      aes(
        x = reorder(province, median_productivity),
        y = median_productivity
      )
    ) +
      geom_col() +
      coord_flip() +
      labs(
        x = "Province",
        y = "Median productivity (kg/ha)"
      ) +
      theme_minimal()
  })
  
  output$data_table <- renderDT({
    datatable(
      filtered_data(),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
}

# Launch the dashboard
shinyApp(ui = ui, server = server)
setwd("C:/Users/user/Desktop/AHS_Dashboard")
shiny::runApp()
