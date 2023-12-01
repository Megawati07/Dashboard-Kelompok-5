library(shiny)
library(ggplot2)
library(sjPlot)
library(plotly)
library(tidyverse)
# Data 1 : Sudarmaji (2004)
a=26;b=35;c=19;d=35;
Kelompok<-factor(c(rep(c("Kontrol"),a),rep(c("Terpapar"),b),
                   rep(c("Kontrol"),c),rep(c("Terpapar"),d)),
                 levels=c("Kontrol","Terpapar"))
Keluhan<-factor(rep(c("Tidak","Ada"),c(a+b,c+d)),levels=c("Tidak","Ada"))
mercury_fish<-data.frame(Keluhan,Kelompok)

# Data 2 : Iswari et al (2021)
a=18;b=49;c=32;d=135;
Perkembangan<-factor(c(rep(c("Tidak Sesuai"),a),rep(c("Sesuai"),c),
                       rep(c("Tidak Sesuai"),b),rep(c("Sesuai"),d)),
                     levels=c("Tidak Sesuai","Sesuai"))
Status_Gizi<-factor(rep(c("Tidak Normal","Normal"),c(a+b,c+d)),
                    levels=c("Tidak Normal","Normal"))
Gizi<-data.frame(Status_Gizi,Perkembangan)

ui <- fluidPage(
  navbarPage(title = "Asosiasi Dua Peubah Kategorik",
             # Panel 1 : Khi Kuadrat           
             tabPanel("Khi Kuadrat",           
                      sidebarLayout(
                        sidebarPanel(
                          # Input: Selector for choosing dataset ----
                          selectInput(inputId = "dataset",
                                      label = "Pilih Data : ",
                                      choices = c("Konsumsi Ikan (Sudarmadji, 2004)",
                                                  "Perkembangan Anak (Iswari et al, 2021)"))
                        ),
                        mainPanel("Deskriptif Data",
                                  fluidRow(
                                    column(4,htmlOutput("html")),  
                                    column(8,plotlyOutput("plotbar"))
                                  )
                        )
                      )
             )
  )
)

server <- function(input, output) 
{
  # Return the requested dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "Konsumsi Ikan (Sudarmadji, 2004)" = mercury_fish,
           "Perkembangan Anak (Iswari et al, 2021)" = Gizi)
  })
  output$html <- renderUI({
    dataset <- datasetInput()
    label <- colnames(dataset)
    HTML(tab_xtab(var.row = dataset[,1], var.col = dataset[,2],
                  var.labels = c(label[1], label[2]),
                  title = "Ringkasan dan Hasil", 
                  show.col.prc = TRUE,show.exp = TRUE)$knitr)
  })
  output$plotbar = renderPlotly({
    dataset <- datasetInput()
    label <- colnames(dataset)
    dataset2 <- dataset %>% group_by(assign(label[1],dataset[,1]),assign(label[2],dataset[,2])) %>% 
      summarise(Frekuensi=n()) %>% as.data.frame()
    dataset2 <- data.frame(Kelompok=factor(unique(dataset[,2]),levels=levels(dataset[,2])),
                           Ada=dataset2[dataset2$assign(label[1], dataset[, 1])=="Ada","Frekuensi"],
                           Tidak=dataset2[dataset2$assign(label[1], dataset[, 1])=="Tidak","Frekuensi"])
    label2 <- colnames(dataset2)
    fig <- plot_ly(x = dataset2[,label2[1]], y = dataset2[,label2[2]], type = 'bar', name = label2[2])
    fig <- fig %>% add_trace(y = dataset2[label2[3]], name = label2[3])
    fig <- fig %>% layout(yaxis = list(title = 'Frekuensi'), barmode = 'group')
  })
}


shinyApp(ui, server)