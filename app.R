if(!require(pacman)){install.packages("pacman");
  library(pacman)}
p_load(shiny, ggplot2, gapminder, leaflet, sf, tidycensus, bslib, tidyverse, thematic,viridis,ggthemes)

##################### data
df <- st_read("t6ej_final.geojson")
mdf <- read.csv("merged_df.csv")

mdf_grp <- mdf %>%
  group_by(Peak, Year) %>%
  summarize(
    av_tti = mean(TTI),
    av_pti = mean(PTI),
    av_bti = mean(BTI),
    av_sev = mean(Severity),
    av_var = mean(Variability)) %>%
  mutate(Year = as.integer(Year))

# Define UI for application
ui <- fluidPage(theme = bslib::bs_theme(
  bg = "#002b36", fg = "#EEE8D5",
  "progress-bar-bg" = "orange"
),
titlePanel("Crater PDC Socio-Economic Dashboard"),
tabsetPanel(
  tabPanel("Congestion PMs",
           sidebarLayout(
             sidebarPanel(
               selectInput("variable", "Select Variable:",
                           choices = c("TTI" = "av_tti",
                                       "PTI" = "av_pti",
                                       "BTI" = "av_bti"))
             ),
             mainPanel(
               plotOutput("linePlot")
             )
           )),
  tabPanel("Reliability",
           sidebarLayout(
             sidebarPanel(
               selectInput("vars", "Select Variable:",
                           choices = c("Severity" = "Severity",
                                       "Variability" = "Variability"))
             ),
             mainPanel(
               plotOutput("barPlot")
             )
           )),
  tabPanel("Congestion & Reliability",
           sidebarLayout(
             sidebarPanel(
               selectInput("Impact", "Select Impact:",
                           choices = c("Severity", "Variability")),
               selectInput("Congestion", "Select Congestion:",
                           choices = c("TTI", "PTI", "BTI"))
             ),
             mainPanel(
               plotOutput("scatterPlot")
             )
           )),
  tabPanel("Title VI & EJ",
           sidebarLayout(
             sidebarPanel(
               selectInput("year", "Select Year:", unique(df$year)),
               selectInput("attribute", "Select Attribute:", c("Poverty","Disability","Carless","Elderly","Young","LEP"))
             ),
             mainPanel(
               leafletOutput("map")
             )
           ))
)
)

# Define server logic
server <- function(input, output) {  
  output$linePlot <- renderPlot({
    ggplot(mdf_grp, aes(x = Year, y = get(input$variable), color = Peak)) +
      geom_line() +  scale_color_manual(values=c("#999999", "#E69F00")) +
      labs(x = "Year", y = input$variable) +
      scale_x_continuous(breaks = 2018:2020) + theme_stata()
  })
  
  output$barPlot <- renderPlot({
    mdf_grp_2 <- mdf %>%
      group_by(Peak, Year) %>%
      summarize(value = mean(get(input$vars)))
    
    ggplot(mdf_grp_2, aes(x = Year, y = value, fill = Peak)) +
      geom_bar(stat = "identity", position = "dodge") + scale_fill_manual(values=c("#999999", "#E69F00")) +
      labs(x = "Year", y = input$vars) + theme_stata()
  })
  
  output$scatterPlot <- renderPlot({
    ggplot(mdf, aes_string(x = input$Impact, y = input$Congestion, color="Peak")) +
      geom_point() +   scale_color_manual(values=c('#999999','#E69F00'))+
      labs(title = "Scatter Plot",
           x = input$Impact,
           y = input$Congestion) + theme_stata()
  })
  
  output$map <- renderLeaflet({
    filtered_data <- df %>% filter(year == input$year)
    pal <- colorFactor(palette = viridis::plasma(length(unique(filtered_data[[input$attribute]]))), 
                       domain = filtered_data[[input$attribute]])
   
    leaflet(filtered_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addPolygons(fillColor = ~pal(filtered_data[[input$attribute]]),
                  fillOpacity = 0.6, 
                  color = "", 
                  weight = 1) %>%
      addLegend("bottomright", pal = pal, values = ~filtered_data[[input$attribute]],
                title = input$attribute,
                opacity = 1)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
