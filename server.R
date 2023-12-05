#----------------------------------server.R------------------------------------#
server <- function(input, output) 
{
  # Return the requested dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "Konsumsi Ikan" = mercury_fish,
           "Perkembangan Anak" = Gizi,
           "Inner Child"=InnerChild,
           "Teknologi Petani"=Tani,
           "Frekuensi Makan"=Makan,
           "Lemak"=Kesehatan)
  })
  output$contingency_table <- rhandsontable::renderRHandsontable({
    dataset <- datasetInput()
    dataset<-as.data.frame(table(dataset))
    dataset <- dataset %>% 
      pivot_wider(names_from = colnames(dataset)[2], values_from = colnames(dataset)[3]) %>%
      as.data.frame()
    rhandsontable(
      dataset,height =  100,width = 300,minRows=2,minCols=2,
      readOnly = F,stretchH = "all")
  })
  output$html <- renderUI({
    dataset <- datasetInput()
    label <- colnames(dataset)
    HTML(tab_xtab(var.row = dataset[,1], var.col = dataset[,2],tdcol.n = "red",
                  var.labels = c(label[1], label[2]),
                  title = "Tabel Kontingensi", show.summary=F,
                  show.col.prc = TRUE,show.exp = TRUE)$knitr)
  })
  output$plotbar = renderPlotly({
    dataset <- datasetInput()
    label <- colnames(dataset)
    dataset2 <- dataset %>% group_by(assign(label[1],dataset[,1]),assign(label[2],dataset[,2])) %>% 
      summarise(Frekuensi=n()) %>% as.data.frame()
    kat<-levels(dataset2[,2])
    dataset2 <- data.frame(V1=factor(unique(dataset[,2]),levels=levels(dataset[,2])),
                           V2=dataset2[dataset2$`assign(label[2], dataset[, 2])`==kat[1],"Frekuensi"],
                           V3=dataset2[dataset2$`assign(label[2], dataset[, 2])`==kat[2],"Frekuensi"])
    label1 <- levels(dataset[,1])
    colnames(dataset2)<-c(label[2],label1[1],label1[2])
    label2 <- colnames(dataset2)
    fig <- plot_ly(x = dataset2[,label2[1]], y = dataset2[,label2[2]], type = 'bar', name = label2[2])
    fig <- fig %>% add_trace(y = dataset2[,label2[3]], name = label2[3])
    fig <- fig %>% layout(yaxis=list(title=list(text=paste0('<b> ','Frekuensi',' </b>'))),
                          xaxis=list(title=list(text=paste0('<b> ',label[2],' </b>'))),
                          legend=list(title=list(text=paste0('<b> ',label[1],' </b>'))), barmode = 'group')
  })
  output$chisq = rhandsontable::renderRHandsontable({
    dataset <- datasetInput()
    hasil_chisq<-chisq.test(dataset[,2], dataset[,1], correct=FALSE)
    data_hasil_chisq<-data.frame(Ukuran=c("Khi Kuadrat Hitung","Derajat Bebas","p-value"),
                                 Hasil=c(round(hasil_chisq$statistic,3),
                                         round(hasil_chisq$parameter,3),
                                         round(hasil_chisq$p.value,3)))
    rownames(data_hasil_chisq)<-NULL
    rhandsontable(
      data_hasil_chisq,height =  100,width = 300,
      readOnly = T,stretchH = "all") %>% hot_col("Hasil", format = "0.000")
  })
  
  output$risk = rhandsontable::renderRHandsontable({
    dataset <- datasetInput()
    hasil_riskr<-riskratio.wald(dataset[,2],dataset[,1])
    data_hasil_riskr<-data.frame(Ukuran=c("Rasio Kejadian","Batas Bawah (SK)","Batas Atas (SK)"),
                                 Hasil=c(round(hasil_riskr$measure[2,1],3),
                                         round(hasil_riskr$measure[2,2],3),
                                         round(hasil_riskr$measure[2,3],3)))
    rownames(data_hasil_riskr)<-NULL
    rhandsontable(
      data_hasil_riskr,height =  100,width = 300,
      readOnly = T,stretchH = "all") %>% hot_col("Hasil", format = "0.000")
  })
  
  output$odd = rhandsontable::renderRHandsontable({
    dataset <- datasetInput()
    hasil_odd<-oddsratio.wald(dataset[,2],dataset[,1])
    data_hasil_odd<-data.frame(Ukuran=c("Rasio Odd","Batas Bawah (SK)","Batas Atas (SK)"),
                               Hasil=c(round(hasil_odd$measure[2,1],3),
                                       round(hasil_odd$measure[2,2],3),
                                       round(hasil_odd$measure[2,3],3)))
    rownames(data_hasil_odd)<-NULL
    rhandsontable(
      data_hasil_odd,height =  100,width = 300,
      readOnly = T,stretchH = "all") %>% hot_col("Hasil", format = "0.000")
  })
}


shinyApp(ui, server)
