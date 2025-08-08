library(shiny)
library(bslib)


ui <- fluidPage(
  titlePanel("Incidence of Conditions in Walla Walla County"),
  sidebarLayout(
    sidebarPanel(
      h4("About This App"),
      p("Select a condition from the list below to view annual incidence rates."),
      p("Gray shading represents suppressed data, where case counts were too low to
        report an incidence rate. Unshaded, missing sections of the plot represent
        years for which no data was available."),
      br(),
      selectInput("condition", "Choose a condition:",
                  choices = sort(unique(ww_inc$condition)))
    ),
    mainPanel(
      plotOutput("incidencePlot", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  
  output$incidencePlot <- renderPlot({
    
    # Add indicator for whether an NA is due to missingness or suppression
    toplot <- ww_inc |>
      mutate(suppressed = is_tagged_na(inc))
    
    toplot$year <- as.numeric(toplot$year)
    
    # Filter to condition of user selection
    toplot <- toplot |>
      filter(condition == input$condition)
    
    # Suppressed data rectangle construction
    rects <- toplot |>
      filter(suppressed) |>
      mutate(xmin = year,
             xmax = year + 1,
             ymin = -Inf,
             ymax = Inf)
    
    # Slope of regression line
    slope <- slope_table |>
      filter(condition == input$condition)
    
    # Create a sequence of years for x-axis of regression line. UPDATE FOR FUTURE ANALYSES
    regression_line <- data.frame(
      year = seq(2010, 2023, length.out = 100)
    )
    
    # Compute predicted y values using the slope and intercept
    regression_line$case_rate <- slope$slope[1] * regression_line$year + 
      slope$slope_intercept[1]
    
    # Ensures that graph goes to 100 per 100k by default and expands as needed otherwise
    max_y <- max(toplot$inc, na.rm = TRUE)
    upper_limit <- ifelse(max_y < 100, 100, max_y * 1.1) 
    
    # Final plot
    toplot |> 
      ggplot(aes(x = year, y = inc)) +
      geom_rect(data = rects,
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                fill = "seashell4",
                linewidth = 0.5,
                alpha = 0.3,
                inherit.aes = FALSE) +
      geom_step(color = "darkgoldenrod3", linewidth = 1) +
      geom_point(size = 0.7, color = "black") +
      geom_text(aes(label = round(inc, 2)), 
                vjust = 0.2, hjust = -0.4,
                size = 3, angle = 45) +
      geom_line(data = regression_line,
                aes(x = year, y = case_rate),
                color = "black", linewidth = 0.3,
                linetype = "dotted") +
      labs(
        x = "Year",
        y = "Incidence per 100,000",
        title = paste("Incidence of", input$condition, "in Walla Walla County")
      ) +
      scale_x_continuous(
        breaks = 2010:2024,
        expand = expansion(mult = c(0, 0.02))
      ) +
      coord_cartesian(xlim = c(2010, 2024),
                      ylim = c(0, upper_limit)) +
      scale_y_continuous(
        expand = expansion(add = c(2, 0))
      ) +
      theme_classic()
  })
}

shinyApp(ui, server)
