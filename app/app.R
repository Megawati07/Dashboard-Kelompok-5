library(shiny)
library(sjPlot)
library(plotly)
library(rhandsontable) 
library(tidyverse)
library(epitools)
library(markdown)
library(colourpicker)
library(knitr)

a=26;b=35;c=19;d=35;
Kelompok<-factor(c(rep(c("Kontrol"),a),rep(c("Terpapar"),b),
                   rep(c("Kontrol"),c),rep(c("Terpapar"),d)),
                 levels=c("Kontrol","Terpapar"))
Keluhan<-factor(rep(c("Tidak","Ada"),c(a+b,c+d)),levels=c("Tidak","Ada"))
mercury_fish<-data.frame(Keluhan,Kelompok)

# Data 2 : Iswari et al (2021)
a=18;b=49;c=32;d=135;
Perkembangan<-factor(c(rep(c("Tidak Sesuai"),a),rep(c("Sesuai"),b),
                       rep(c("Tidak Sesuai"),c),rep(c("Sesuai"),d)),
                     levels=c("Tidak Sesuai","Sesuai"))
Status_Gizi<-factor(rep(c("Tidak Normal","Normal"),c(a+b,c+d)),
                    levels=c("Tidak Normal","Normal"))
Gizi<-data.frame(Status_Gizi,Perkembangan)
colnames(Gizi)<-c("Status Gizi","Perkembangan")

# Data 3 : Permatasari et al (2021)
a=20;b=12;c=14;d=52;
Inner_Child<-factor(c(rep(c("Terluka"),a),rep(c("Tidak Terluka"),b),
                      rep(c("Terluka"),c),rep(c("Tidak Terluka"),d)),
                    levels=c("Terluka","Tidak Terluka"))
Keadaan_Keluarga<-factor(rep(c("Tidak Harmonis","Harmonis"),c(a+b,c+d)),
                         levels=c("Tidak Harmonis","Harmonis"))
InnerChild<-data.frame(Inner_Child,Keadaan_Keluarga)
colnames(InnerChild)<-c("Inner Child","Keadaan Keluarga")

# Data 4 : Saranani et al (2022)
a=6;b=31;c=13;d=4;
Dinamika_Kelompok<-factor(c(rep(c("Sedang"),a),rep(c("Tinggi"),b),
                            rep(c("Sedang"),c),rep(c("Tinggi"),d)),
                          levels=c("Sedang","Tinggi"))
Penerapan_Teknologi<-factor(rep(c("Sedang","Tinggi"),c(a+b,c+d)),
                            levels=c("Sedang","Tinggi"))
Tani<-data.frame(Dinamika_Kelompok,Penerapan_Teknologi)
colnames(Tani)<-c("Dinamika Kelompok","Penerapan Teknologi")

# Data 5 : Sawarko, S (2023)
a=54;b=16;c=14;d=35;
Kejadian<-factor(c(rep(c("Gastritis"),a),rep(c("Tidak Gastritis"),b),
                   rep(c("Gastritis"),c),rep(c("Tidak Gastritis"),d)),
                 levels=c("Tidak Gastritis","Gastritis"))
Frekuensi_Makan<-factor(rep(c("Tidak Baik","Baik"),c(a+b,c+d)),
                        levels=c("Tidak Baik","Baik"))
Makan<-data.frame(Frekuensi_Makan,Kejadian)
colnames(Makan)<-c("Frekuensi Makan","Kejadian")

# Data 6 : Hidayati, et  al (2017)
a=10;b=2;c=16;d=7;
Klasifikasi_Trigliserida<-factor(c(rep(c("Normal"),a),rep(c("Tinggi"),b),
                                   rep(c("Normal"),c),rep(c("Tinggi"),d)),
                                 levels=c("Normal","Tinggi"))
Asupan_Lemak<-factor(rep(c("Cukup","Lebih"),c(a+b,c+d)),
                     levels=c("Cukup","Lebih"))
Kesehatan<-data.frame(Asupan_Lemak,Klasifikasi_Trigliserida)
colnames(Kesehatan)<-c("Asupan Lemak","Klasifikasi Trigliserida")

# Data Isi Sendiri
a=0;b=0;c=0;d=0;
Peubah_1<-factor(c(rep(c("Kat 1"),a),rep(c("Kat 2"),b),
                   rep(c("Kat 1"),c),rep(c("Kat 2"),d)),
                 levels=c("Kat 1","Kat 2"))
Peubah_2<-factor(rep(c("Kat 1","Kat 2"),c(a+b,c+d)),
                 levels=c("Kat 1","Kat 2"))
manual<-data.frame(Peubah_1,Peubah_2)
colnames(manual)<-c("Peubah Baris","Peubah Kolom")

# User Interface Dashboard Asosiasi Dua Peubah Kategorik
ui <- fluidPage(
  # Menghilangkan Pesan Error pada Proses Memuat Dashboard
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: visible; content: 'Sedang Memuat Konten ...'; color:black}"
  ),
  # Judul Dashboard
  tags$style(HTML(".navbar-header { width:100% }
                   .navbar-brand { width: 100%; text-align: center }")
  ),
  navbarPage(h1(strong("Asosiasi Dua Peubah Kategorik")),
             tabsetPanel(
               # Tab Informasi berisi Penjelasan tentang Asosiasi Dua Peubah Kategorik
               tabPanel("Informasi",
                        withMathJax(includeMarkdown("https://raw.githubusercontent.com/Megawati07/Dashboard-Kelompok-5/main/markdown/information.Rmd"))),
               # Tab Asosiasi Dua Peubah Kategorik dengan menggunakan metode Chi Square 
               tabPanel("Khi Kuadrat",           
                        sidebarLayout(
                          sidebarPanel(
                            # Memilih Data
                            fluidRow(
                              column(12,selectInput(inputId = "dataset",
                                                    label = "Pilihan Data : ",
                                                    choices = c("Konsumsi Ikan",
                                                                "Perkembangan Anak",
                                                                "Inner Child",
                                                                "Teknologi Petani",
                                                                "Frekuensi Makan",
                                                                "Lemak",
                                                                "Isi Data Sendiri")))),
                            # Memberikan nama peubah baris dan kolom
                            fluidRow(
                              column(6,textInput("var_row", "Nama Peubah Baris :", "Contoh : Usia")),
                              column(6,textInput("var_col", "Nama Peubah Kolom :", "Contoh : Pendapatan"))),
                            # Memberikan nama kategori pada peubah baris dan kolom
                            fluidRow(
                              column(6,textInput("col_name", "Kolom :", "Kat 1,Kat 2")),
                              column(6,textInput("row_name", "Baris :", "Kat 1,Kat 2"))),
                            # Memberikan warna pada diagram batang
                            # Menampilkan Input Tabel Kontingensi dari Data yang dipilih
                            h5(strong("Data")),
                            fluidRow(column(12,rHandsontableOutput("contingency_table"))),
                            h5(strong("Pilih Warna yang ditampilkan pada Diagram Batang")),
                            fluidRow(
                              column(width = 6, colourInput("col_1", "Kategori 1", "blue", showColour = "background")),
                              column(width = 6, colourInput("col_2", "Kategori 2", "orange", showColour = "background")),align = 'center'),
                            # Memilih ukuran - ukuran yang akan ditampilkan pada Tabel Kontingensi
                            h5(strong("Pilih Nilai yang Ditampilkan pada Tabel Kontingensi :")),
                            h5(""),
                            checkboxInput(inputId = "obs", value = TRUE,label = HTML('Tampilkan Nilai <span style="color: #4A588A; font-weight: bold;">Observasi</span>')),
                            checkboxInput(inputId = "exp", label = HTML('Tampilkan Nilai <span style="color: #A80000; font-weight: bold;">Ekspektasi</span>')),
                            checkboxInput(inputId = "row_per", label = HTML('Tampilkan Nilai <span style="color: #536F18; font-weight: bold;">Persentase Baris</span>')),
                            checkboxInput(inputId = "col_per", label = HTML('Tampilkan Nilai <span style="color: #CC9900; font-weight: bold;">Persentase Kolom</span>')),
                            br(),
                            # Memilih hasil asosiasi yang ingin ditampilkan
                            fluidRow(column(12,selectInput(inputId = "output_test",label = "Pilih Hasil Asosiasi yang Ingin Ditampilkan : ",
                                                           choices = c("Khi Kuadrat","Rasio Odd","Resiko Relatif")))),
                            h5(strong(textOutput("currentTime")))
                          ),
                          # Panel Utama
                          # Panel Deskriptif Data (Tabel Kontongensi dan Diagram Batang)
                          mainPanel(h2("Deskriptif Data", align = "center"),
                                    fluidRow(
                                      column(5,htmlOutput("html")),
                                      column(7,plotlyOutput("plotbar"))
                                    ),
                                    # Panel Memilih Hasil Analisis (Khi Kuadrat, Resiko Relatif dan Rasio Odd)
                                    fluidRow(h2("Hasil Analisis", align = "center"),
                                             conditionalPanel(
                                               condition = "input.output_test == 'Khi Kuadrat'", 
                                               column(12,offset = 3,rHandsontableOutput("chisq"))),
                                             conditionalPanel(
                                               condition = "input.output_test == 'Resiko Relatif'", 
                                               column(12,offset = 3,rHandsontableOutput("riskr"))),
                                             conditionalPanel(
                                               condition = "input.output_test == 'Rasio Odd'", 
                                               column(12,offset = 3,rHandsontableOutput("odd")))
                                             
          )
        )
       )
      )
    )
  )
)

server <- function(input, output,session) 
{
  output$currentTime <- renderText({
    invalidateLater(1000, session)
    paste("Tanggal dan Waktu Sekarang : ", Sys.time())
  })
  
  # Input Data
  datasetInput <- reactive({
    switch(input$dataset,
           "Konsumsi Ikan" = mercury_fish,
           "Perkembangan Anak" = Gizi,
           "Inner Child"=InnerChild,
           "Teknologi Petani"=Tani,
           "Frekuensi Makan"=Makan,
           "Lemak"=Kesehatan,
           "Isi Data Sendiri"=manual)
  })
  
  # Memperbarui text box nama kolom dan baris beserta kategori
  observe({
    nrowm<-colnames(datasetInput())[1]
    ncolm<-colnames(datasetInput())[2]  
    rown<-levels(datasetInput()[,1])
    coln<-levels(datasetInput()[,2]) 
    updateTextInput(session, "row_name", value = paste0(rown[1],", ",rown[2]))
    updateTextInput(session, "col_name", value = paste0(coln[1],", ",coln[2]))
    updateTextInput(session, "var_row", value = nrowm)
    updateTextInput(session, "var_col", value = ncolm)
    updateColourInput(session, "col_1", label = rown[1])
    updateColourInput(session, "col_2", label = rown[2])
  })
  
  
  # Warna Ukuran Tabel Kontingensi
  col_n<-"#4A588A" #Biru
  col_exp<-"#A80000" #Merah
  col_row<-"#536F18" #Hijau
  col_col<-"#CC9900" #Kuning
  
  # Membuat Tabel Kontingensi
  output$contingency_table <- rhandsontable::renderRHandsontable({
    dataset <- datasetInput()
    dataset<-as.data.frame(table(dataset))
    dataset <- dataset %>% 
      pivot_wider(names_from = colnames(dataset)[2], values_from = colnames(dataset)[3]) %>%
      as.data.frame()
    rownames(dataset)<-dataset[,1]
    rownames(dataset)<-unlist(strsplit(req(input$row_name), ","))
    dataset<-dataset[,-1]
    colnames(dataset)<-unlist(strsplit(req(input$col_name), ","))
    rhandsontable(
      dataset,height =  100,width = 370,minRows=2,minCols=2,rowHeaderWidth = 100,
      readOnly = F,stretchH = "all")
  })
  
  # Menampilkan Tabel Kontingensi Beserta Ukuran-Ukuran Lainnya
  output$html <- renderUI({
    data <- hot_to_r(input$contingency_table)
    a=data[1,1];b=data[1,2];c=data[2,1];d=data[2,2];
    label_col<-unlist(strsplit(req(input$col_name), ","))
    label_row<-unlist(strsplit(req(input$row_name), ","))
    Y<-factor(c(rep(label_col[1],a),rep(label_col[2],b),rep(label_col[1],c),rep(label_col[2],d)),
              levels=label_col)
    X<-factor(rep(c(label_row[1],label_row[2]),c(a+b,c+d)),levels=label_row)
    dataset<-data.frame(X,Y)
    colnames(dataset)<-c(req(input$var_row),req(input$var_col))
    label <- colnames(dataset)
    coba_dulu<-function(o,p,q,r){
      HTML(tab_xtab(var.row = dataset[,1], var.col = dataset[,2],
                  var.labels = c(label[1], label[2]),
                  title = "Tabel Kontingensi", show.summary=F,show.obs = o,
                  show.row.prc = p,show.exp = q,show.col.prc = r,
                  tdcol.n = col_n,
                  tdcol.expected = col_exp,
                  tdcol.row = col_row,
                  tdcol.col = col_col,
    )$knitr)
    }
    coba_dulu(o=input$obs,p=input$row_per,q=input$exp,r=input$col_per)
  })
  
  # Membuat Diagram Batang Dari Data yang dipilih
  output$plotbar = renderPlotly({
    data <- hot_to_r(input$contingency_table)
    a=data[1,1];b=data[1,2];c=data[2,1];d=data[2,2];
    label_col<-unlist(strsplit(req(input$col_name), ","))
    label_row<-unlist(strsplit(req(input$row_name), ","))
    Y<-factor(c(rep(label_col[1],a),rep(label_col[2],b),rep(label_col[1],c),rep(label_col[2],d)),
              levels=c(label_col[1],label_col[2]))
    X<-factor(rep(c(label_row[1],label_row[2]),c(a+b,c+d)),levels=c(label_row[1],label_row[2]))
    dataset<-data.frame(X,Y)
    colnames(dataset)<-c(req(input$var_row),req(input$var_col))
    label <- colnames(dataset)
    dataset2 <- dataset %>% group_by(assign(label[1],dataset[,1]),assign(label[2],dataset[,2])) %>% 
      summarise(Frekuensi=n()) %>% as.data.frame()
    kat<-levels(dataset2[,1])
    dataset2 <- data.frame(V1=factor(unique(dataset[,2]),levels=levels(dataset[,2])),
                           V2=dataset2[dataset2$`assign(label[1], dataset[, 1])`==kat[1],"Frekuensi"],
                           V3=dataset2[dataset2$`assign(label[1], dataset[, 1])`==kat[2],"Frekuensi"])
    label1 <- levels(dataset[,1])
    colnames(dataset2)<-c(label[2],label1[1],label1[2])
    label2 <- colnames(dataset2)
    fig <- plot_ly(x = dataset2[,label2[1]], y = dataset2[,label2[2]], type = 'bar', name = label2[2],
                   marker = list(color = input$col_1,line = list(color = 'black', width = 1.5)))
    fig <- fig %>% add_trace(y = dataset2[,label2[3]], name = label2[3],
                             marker = list(color = input$col_2,line = list(color = 'black', width = 1.5)))
    fig <- fig %>% layout(yaxis=list(title=list(text=paste0('<b> ','Frekuensi',' </b>'))),
                          xaxis=list(title=list(text=paste0('<b> ',label[2],' </b>'))),
                          legend=list(title=list(text=paste0('<b> ',label[1],' </b>'))), barmode = 'group')
  })
  
  # Membuat hasil analisis chi - square
  # Chi Square Test
  output$chisq = rhandsontable::renderRHandsontable({
    data <- hot_to_r(input$contingency_table)
    a=data[1,1];b=data[1,2];c=data[2,1];d=data[2,2];
    label_col<-colnames(data)
    label_row<-rownames(data)
    Y<-factor(c(rep(label_col[1],a),rep(label_col[2],b),rep(label_col[1],c),rep(label_col[2],d)),
              levels=c(label_col[1],label_col[2]))
    X<-factor(rep(c(label_row[1],label_row[2]),c(a+b,c+d)),levels=c(label_row[1],label_row[2]))
    dataset<-data.frame(X,Y)
    colnames(dataset)<-c(colnames(datasetInput())[1],colnames(datasetInput())[2])
    hasil_chisq<-chisq.test(dataset[,2], dataset[,1], correct=FALSE)
    data_hasil_chisq<-data.frame(Ukuran=c("Khi Kuadrat Hitung","Khi Kuadrat Tabel","Derajat Bebas","p-value"),
                                 Hasil=c(round(hasil_chisq$statistic,3),
                                         round(qchisq(0.95,df=hasil_chisq$parameter),3),
                                         round(hasil_chisq$parameter,3),
                                         round(hasil_chisq$p.value,3)))
    rownames(data_hasil_chisq)<-NULL
    rhandsontable(
      data_hasil_chisq,height =  200,width = 400,
      readOnly = T,stretchH = "all") %>% hot_col("Hasil", format = "0.000")
  })
  
  # Risk Ratio
  output$riskr = rhandsontable::renderRHandsontable({
    data <- hot_to_r(input$contingency_table)
    a=data[1,1];b=data[1,2];c=data[2,1];d=data[2,2];
    label_col<-colnames(data)
    label_row<-rownames(data)
    Y<-factor(c(rep(label_col[1],a),rep(label_col[2],b),rep(label_col[1],c),rep(label_col[2],d)),
              levels=c(label_col[1],label_col[2]))
    X<-factor(rep(c(label_row[1],label_row[2]),c(a+b,c+d)),levels=c(label_row[1],label_row[2]))
    dataset<-data.frame(X,Y)
    colnames(dataset)<-c(colnames(datasetInput())[1],colnames(datasetInput())[2])
    hasil_riskr<-riskratio.wald(dataset[,2],dataset[,1])
    data_hasil_riskr<-data.frame(Ukuran=c("Resiko Relatif","Batas Bawah (SK)","Batas Atas (SK)"),
                                 Hasil=c(round(hasil_riskr$measure[2,1],3),
                                         round(hasil_riskr$measure[2,2],3),
                                         round(hasil_riskr$measure[2,3],3)))
    rownames(data_hasil_riskr)<-NULL
    rhandsontable(
      data_hasil_riskr,height =  100,width = 400,
      readOnly = T,stretchH = "all") %>% hot_col("Hasil", format = "0.000")
  })
  
  # Odd Rasio
  output$odd = rhandsontable::renderRHandsontable({
    data <- hot_to_r(input$contingency_table)
    a=data[1,1];b=data[1,2];c=data[2,1];d=data[2,2];
    label_col<-colnames(data)
    label_row<-rownames(data)
    Y<-factor(c(rep(label_col[1],a),rep(label_col[2],b),rep(label_col[1],c),rep(label_col[2],d)),
              levels=c(label_col[1],label_col[2]))
    X<-factor(rep(c(label_row[1],label_row[2]),c(a+b,c+d)),levels=c(label_row[1],label_row[2]))
    dataset<-data.frame(X,Y)
    colnames(dataset)<-c(colnames(datasetInput())[1],colnames(datasetInput())[2])
    hasil_odd<-oddsratio.wald(dataset[,2],dataset[,1])
    data_hasil_odd<-data.frame(Ukuran=c("Rasio Odd","Batas Bawah (SK)","Batas Atas (SK)"),
                               Hasil=c(round(hasil_odd$measure[2,1],3),
                                       round(hasil_odd$measure[2,2],3),
                                       round(hasil_odd$measure[2,3],3)))
    rownames(data_hasil_odd)<-NULL
    rhandsontable(
      data_hasil_odd,height =  100,width = 400,
      readOnly = T,stretchH = "all") %>% hot_col("Hasil", format = "0.000")
  })
}

shinyApp(ui, server)
