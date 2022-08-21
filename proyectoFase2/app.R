library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(shinythemes)

setwd('/cloud/project')

md <- read.csv("match.data.csv")

ui <- fluidPage(
  
  dashboardPage(
    
    dashboardHeader(title = "Proyecto final equipo 4"),
    
    dashboardSidebar(
      
      sidebarMenu(
        menuItem("Gráficos de barras", tabName = "graficosBarras", icon = icon("chart-simple")),
        menuItem("Imágenes Histogramas", tabName = "imagenesHistogramas", icon = icon("images")),
        menuItem("Data Table", tabName = "dataTable", icon = icon("table")),
        menuItem("Imágenes Momios", tabName = "imagenesMomios", icon = icon("image"))
      )
      
    ),
    
    dashboardBody(
      
      #Para generar pestañas en el dashboard
      tabItems(
        
        #Pestaña 1
        #Una pestaña con gráficas de barras, donde en el eje de las X se muestren 
        #los goles de local y visitante, con un menu de selección 
        #(en choices deben aparecer éstas variables), utiliza la geometría de tipo 
        #barras, además de hacer un facet_wrap con la variable de el equipo visitante
        tabItem(tabName = "graficosBarras",
                fluidRow(
                   titlePanel(h3(textOutput("output_text"))), 
                   selectInput("x", "Selecciona el eje de las X",
                               choices = c('Local', 'Visitante')),
                   plotOutput("output_plot", height = "700px") 
                )),
        tabItem(tabName = "imagenesHistogramas",
                fluidRow(
                  #Pestaña 2
                  #Realiza una pestaña donde agregues las imágenes de las gráficas del postwork 3
                  titlePanel(h3("Imágenes de histogramas y heatmap de probabilidades marginales y conjuntas de los goles")),
                  img(src ="barras_casa.png", width = 650, height = 650),
                  img(src ="barras_visita.png", width = 650, height = 650),
                  img(src ="heatmap_conjuntas.png", width = 650, height = 650)
                )),
        tabItem(tabName = "dataTable",
                fluidRow(
                  #Pestaña 3
                  #En otra pestaña coloca el data table del fichero match.data.csv
                  titlePanel(h3("Data Table de todos mis datos")),
                  dataTableOutput("dataTable"),   # salida del data table
                )),
        tabItem(tabName = "imagenesMomios",
                fluidRow(
                  #Pestaña 4
                  #Por último en otra pestaña agrega las imágenes de las gráficas de los factores 
                  #de ganancia promedio y máximo
                  titlePanel(h3("Imágenes de factores de ganancia promedio y máximo")),
                  img(src ="MomiosPromedio.png", width = 650, height = 650),
                  img(src ="MomiosMaximos.png", width = 650, height = 650)
                ))
      )
    )
  )
)

server <- function(input, output) {
  
  #Pestaña 1
  #Texto inteligente para saber qué se está graficando
  output$output_text <- renderText({
    paste("Gráficos de barras de cantidad de goles del equipo ", tolower(input$x))
  }) 
  
  #Pestaña 2
  #Obtener tablas de frecuencias absolutas/equipos/FTR (H = casa ganó, A =
  #visita ganó, D = empate)
  home.score <- table(md$home.team, md$home.score, md$FTR)
  away.score <- table(md$away.team, md$away.score, md$FTR)
  
  #Convertir a data frame para poder graficar
  home.score <- as.data.frame(home.score)
  away.score <- as.data.frame(away.score)
  
  selector <- reactive({input$x})
  
  data <- reactive(if(selector() == 'Local') home.score else away.score)
  
  #Graficar según input
  output$output_plot <- renderPlot({
    ggplot(data()) +
    aes(x = Var2, y = Freq, fill = Var3) + 
    geom_bar(stat="identity") + 
    facet_wrap(data()$Var1) +
    ggtitle(paste("Goles del equipo", tolower(selector()))) +
    labs(x = paste('Clasificación de goles anotados por partido del equipo', tolower(selector())), 
         y = 'Frecuencia de partidos dónde se anotaron x goles', fill = 'FTR')
  })
  
  #Pestaña 3
  output$dataTable <- renderDataTable({md},       #Data table
                                     options = list(aLengthMenu = c(10,20,50), 
                                                    iDisplayLength = 10))
}

# Run the application 
shinyApp(ui = ui, server = server)
