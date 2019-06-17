#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# DPLRY

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)

setwd("D:/Documents/Rstudio_CPU_GPU-master")
all_gpu <- read.csv2("ALL_GPUs.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
all_cpu <- read.csv2("Intel_CPUs.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

gpu <- all_gpu %>%
   select(Architecture, Boost_Clock, Core_Speed, L2_Cache, Manufacturer, Max_Power,
          Memory, Memory_Bandwidth, Memory_Bus, Memory_Speed, Memory_Type, Name,
          PSU, Pixel_Rate, Process, ROPs, Release_Date, Resolution_WxH, Texture_Rate, TMUs)

# withGPU <- subset(gpu, (grepl("Yes", gpu$Notebook_GPU)))
# withGPU <- subset(gpu$Name, (grepl("Yes", gpu$Notebook_GPU)))
# colnames(gpu)

# gpu[c("Dedicated", "Direct_X", "DisplayPort_Connection", "HDMI_Connection",
#       "Integrated", "Open_GL", "Power_Connector", "ROPs", "VGA_Connection",
#       "Shader", "Release_Price", "DVI_Connection")] <- NULL

gpu$Core_Speed <- gsub("[ MHz]", "", gpu$Core_Speed)
gpu$Memory_Speed <- gsub("[ MHz]", "", gpu$Memory_Speed)
gpu$Memory <- gsub("[ MB]", "", gpu$Memory)
gpu$Memory_Bandwidth <- gsub("[GB/sec]", "", gpu$Memory_Bandwidth)
gpu$Memory_Bus <- gsub("[ Bit]", "", gpu$Memory_Bus)
gpu$Max_Power <- gsub("[ Watts]", "", gpu$Max_Power)
gpu$Pixel_Rate <- gsub("[ GPixel/s]", "", gpu$Pixel_Rate)
gpu$Process <- gsub("[nm]", "", gpu$Process)
gpu$L2_Cache <- gsub("[KB]", "", gpu$L2_Cache)
gpu$Texture_Rate <- gsub("[ GTexel/s]", "", gpu$Texture_Rate)

parcial <- gpu %>%
   select(Name, Memory_Bus, Memory_Speed) %>%
   filter(gpu$Architecture == "Sandy Bridge ")

parcial$Memory_Bus[parcial$Memory_Bus == ""] <- 0
parcial$Memory_Speed[parcial$Memory_Speed == ""] <- 0
# min(parcial$Memory_Bus)
# min(parcial$Memory_Speed)
# parcial <- arrange(parcial, Memory_Bus)

grafico = plot_ly(parcial, x = parcial$Name, y = parcial$Memory_Bus, name = "Memory Bus (Bit)", type = 'bar', orientation = 'v') %>%
   add_trace(y = parcial$Memory_Speed, name = "Memory Speed (MHz)") %>%
   layout(title = "Sandy Bridge Architecture")

grafico
# gpu <- mutate(gpu, PSU_Watt = (gsub("")))
variaveis <- c(colnames(gpu))

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         
         selectInput("arch1", "List of arch1:",
                     choices = c(unique(gpu$Architecture)),
                     selected = gpu$Architecture[1]),
         
         selectInput("arch2", "List of arch2:",
                     choices = c(unique(gpu$Architecture)),
                     selected = gpu$Architecture[2]),

         selectInput("parameter1", "Parameter 1:",
                     choices = variaveis[-1],
                     selected = variaveis[1])

      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("Comparing Plot", plotlyOutput("compare"))
        )
      )
   )
)

# Animals <- c("giraffes", "orangutans", "monkeys")
# SF_Zoo <- c(20, 14, 23)
# LA_Zoo <- c(12, 18, 29)
# data <- data.frame(Animals, SF_Zoo, LA_Zoo)

# p <- plot_ly(gpu, x = ~Architecture, y = ~Max_Power, type = 'bar', name = 'Max Power') %>%
  # add_trace(y = ~Texture_Rate, name = 'Txt Rate') %>%
  # layout(yaxis = list(title = 'Count'), barmode = 'group')

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$compare <- renderPlotly({
     x1 <- subset(gpu, (grepl(input$arch1, gpu$Architecture)))

     plot_ly(gpu) %>%
       add_trace(x = ~x1, y = ~input$parameter1, type = 'bar', name = input$arch1) %>%
       # add_trace(x = ~input$arch2, y = ~input$parameter1, type = 'bar', name = input$arch2) %>%
       layout(title = "Comparision",
              # barmode = 'group',
              xaxis = list(title = ""),
              yaxis = list(title = ""))
   })

}

# Run the application
shinyApp(ui = ui, server = server)

