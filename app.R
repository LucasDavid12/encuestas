#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(magick)
library(ggtextures)
library(ggtext)
library(bslib)
library(shinydashboardPlus)
library(tidyverse)
library(shinydashboard)
library(stringr)
library(htmltools)
library(shinyWidgets)
library(shinycustomloader)
library(fresh)
library(forcats)
library(wordcloud2)
library(wordcloud)
library(RColorBrewer)


encuestas <- readRDS("encuestas.RDS")
imagen <- readRDS("imagen.RDS")


# Define UI for application that draws a histogram
ui <- dashboardPage(
                    dashboardHeader(title = "Encuestas", titleWidth = 300), 
                    dashboardSidebar(width = 300,
                                     sidebarMenu(                                  
                                       id = "sidebar", 
                                       menuItem(text = h5(strong("Promedio de encuestas")), tabName = "rank"),
                                       conditionalPanel("input.sidebar == 'rank' && input.t1 == 'filter_ran'", pickerInput(inputId = "sele_mes", strong("Filtro por mes"), choices = unique(encuestas$mes), selected = unique(encuestas$mes))),
                                       conditionalPanel("input.sidebar == 'rank' && input.t1 == 'filter_ran'", pickerInput(inputId = "sele_partido", strong("Filtro por partido"), choices = unique(encuestas$partido), selected = unique(encuestas$partido), multiple = T)),
                                       conditionalPanel("input.sidebar == 'rank' && input.t1 == 'filter_ran'", pickerInput(inputId = "sele_poli", strong("Filtro por politico"), choices = unique(encuestas$politico), selected = unique(encuestas$politico), multiple = T)),
                                       conditionalPanel("input.sidebar == 'rank' && input.t1 == 'filter_ran'", pickerInput(inputId = "sele_encu", strong("Filtro por consultora"), choices = unique(encuestas$Encuestadora), selected = unique(encuestas$Encuestadora), multiple = T)),
                                       menuItem(text = h5(strong("Imagen")), tabName = "imagen"), 
                                       conditionalPanel("input.sidebar == 'imagen' && input.t2 == 'filter_img'", pickerInput(inputId = "sele_mes_emo", strong("Filtro por mes"), choices = unique(imagen$mes), selected = unique(imagen$mes))),
                                       conditionalPanel("input.sidebar == 'imagen' && input.t2 == 'filter_img'", pickerInput(inputId = "sele_partido_emo", strong("Filtro por partido"), choices = unique(imagen$partido), selected = unique(imagen$partido), multiple = T)),
                                       conditionalPanel("input.sidebar == 'imagen' && input.t2 == 'filter_img'", pickerInput(inputId = "sele_poli_emo", strong("Filtro por politico"), choices = unique(imagen$politico), selected = unique(imagen$politico), multiple = T)),
                                       conditionalPanel("input.sidebar == 'imagen' && input.t2 == 'filter_img'", pickerInput(inputId = "sele_encu_emo", strong("Filtro por consultora"), choices = unique(imagen$Encuestadora), selected = unique(imagen$Encuestadora), multiple = T))
                                     )),                      
                    dashboardBody(
                                  tabItems(
                                    
                                    tabItem(
                                      
                                      #pestaña de ranking
                                      
                                      tabName = "rank", 
                                      tabBox(id = "t1", width = 15,
                                             tabPanel(title = strong("Julio"), value = 'filter_ran', withLoader(plotOutput("plot_rank", height ="800"), type="html", loader="dnaspin")))),
                                    tabItem(
                                      
                                      #pestaña de ranking
                                      
                                      tabName = "imagen", 
                                      tabBox(id = "t2", width = 15,
                                             tabPanel(title = strong("Julio"), value = 'filter_img', withLoader(plotOutput("plot_emo", height ="800"), type="html", loader="dnaspin"))))
                                   
                                    
                                    
                                    
                                  )
                    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ### ranking de menciones
  
  fecha_plot_ran <- reactive({
    lv <- encuestas %>% filter(partido %in% input$sele_partido) %>% 
      filter(Encuestadora %in% input$sele_encu) %>%
      filter(politico %in% input$sele_poli) %>% 
      filter(mes %in% input$sele_mes) %>%
      group_by(politico) %>% filter(!is.na(intencion)) %>% mutate(promedio = mean(intencion)) %>% distinct(mes, politico, partido, promedio, imagen) %>%
      ungroup() %>%
      mutate_if(is.numeric, round, digits = 2)
  
    return(lv)})
  
  output$plot_rank <- renderPlot({
    
    
    ggplot(data = fecha_plot_ran(), mapping = aes(x = reorder(politico, promedio), 
                                                  y = promedio, fill = partido, image = imagen)) +
      geom_bar(stat = "identity", colour = "black") + coord_flip() + theme_classic() + 
      xlab(NULL) + ylab("promedio") + 
      theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5), 
            axis.text.y = element_text(size = 15, face = "bold", hjust = 0.5)) + 
      scale_fill_manual("Partido Político", values = c("Juntos" = "#ffff33", "Frente de Todos" = "#a6cee3", "Avanza Libertad" = "#AE17EB", 
                                                       "FIT" = "#FF1616")) +
      geom_isotype_col(
        img_height = grid::unit(0.7, "null"), img_width = NULL,
        ncol = 1, nrow = 1, hjust = 1, vjust = 0.5
      ) + 
      geom_label(
        aes(label = paste0(promedio, "%")),
        size = 6, fontface = "bold", family = "Fira Sans",
        fill = "white", label.size = 0, position = position_stack(vjust=0.5)
      )
    
    
  })
  
  
  ### ranking de emocion
  
  fecha_plot_emo <- reactive({
    lv <- imagen %>%
      filter(Encuestadora %in% input$sele_encu_emo) %>%
      filter(partido %in% input$sele_partido_emo) %>%
      filter(politico %in% input$sele_poli_emo) %>%
      filter(mes %in% input$sele_mes_emo) %>%
      mutate(tipo = factor(tipo, levels = c("No lo conoce", "Negativa", "Regular", "Positiva"))) %>%
      group_by(politico, tipo) %>% filter(!is.na(tipo_n)) %>% mutate(tipo_n = mean(tipo_n)) %>% distinct(politico, partido, tipo_n, tipo) %>% 
      ungroup() %>%
      mutate_if(is.numeric, round, digits = 2)
    
    return(lv)})
  
  output$plot_emo <- renderPlot({
    
    ggplot(data = fecha_plot_emo(), mapping = aes(x = reorder(politico, tipo_n), 
                                                  y = tipo_n, fill = tipo)) +
      geom_bar(stat = "identity", colour = "black") + coord_flip() + theme_classic() + 
      xlab(NULL) + ylab("Imagen") + 
      theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5), 
            axis.text.y = element_text(size = 15, face = "bold", hjust = 0.5)) +
      scale_fill_manual("imagen", values = c("Negativa" = "#FF1616", "Regular" = "#fed976", "Positiva" = "#238b45", 
                                                                   "No lo conoce" = "#bdbdbd")) + 
      geom_text(aes(x = politico, label = paste0(tipo_n,'%')),
                colour = 'black', fontface = "bold", position=position_stack(vjust=0.5)
      )
    
    
  })
 
  
}

# Run the application 
shinyApp(ui = ui, server = server)
