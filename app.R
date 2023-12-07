#---------------------------------library--------------------------------------#
library(shiny)
library(sjPlot)
library(plotly)
library(rhandsontable)
library(tidyverse)
library(epitools)
#------------------------------------data--------------------------------------#
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
colnames(Gizi)<-c("Status Gizi","Perkembangan")

# Data 3 : Permatasari et al (2021)
a=20;b=14;c=12;d=52;
Inner_Child<-factor(c(rep(c("Terluka"),a),rep(c("Tidak Terluka"),c),
                      rep(c("Terluka"),b),rep(c("Tidak Terluka"),d)),
                    levels=c("Terluka","Tidak Terluka"))
Keadaan_Keluarga<-factor(rep(c("Tidak Harmonis","Harmonis"),c(a+c,b+d)),
                         levels=c("Tidak Harmonis","Harmonis"))
InnerChild<-data.frame(Inner_Child,Keadaan_Keluarga)
colnames(InnerChild)<-c("Inner Child","Keadaan Keluarga")

# Data 4 : Saranani et al (2022)
a=31;b=4;c=6;d=13;
Dinamika_Kelompok<-factor(c(rep(c("T"),a),rep(c("S"),c),
                            rep(c("T"),b),rep(c("S"),d)),
                          levels=c("T","S"))
Penerapan_Teknologi<-factor(rep(c("T","S"),c(a+c,b+d)),
                            levels=c("T","S"))
Tani<-data.frame(Dinamika_Kelompok,Penerapan_Teknologi)
colnames(Tani)<-c("Dinamika Kelompok","Penerapan Teknologi")

# Data 5 : Sawarko, S (2023)
a=54;b=16;c=14;d=35;
Frekuensi_Makan<-factor(c(rep(c("Gastritis"),a),rep(c("Tidak Gastritis"),c),
                          rep(c("Gastritis"),b),rep(c("Tidak Gastritis"),d)),
                        levels=c("Gastritis","Tidak Gastritis"))
Kelompok<-factor(rep(c("Tidak Baik","Baik"),c(a+b,c+d)),
                 levels=c("Tidak Baik","Baik"))
Makan<-data.frame(Frekuensi_Makan,Kelompok)
colnames(Makan)<-c("Frekuensi Makan","Kelompok")

# Data 6 : Hidayati, et  al (2017)
a=10;b=2;c=16;d=7;
Klasifikasi_Trigliserida<-factor(c(rep(c("Normal"),a),rep(c("Tinggi"),c),
                                   rep(c("Normal"),b),rep(c("Tinggi"),d)),
                                 levels=c("Normal","Tinggi"))
Asupan_Lemak<-factor(rep(c("Cukup","Lebih"),c(a+b,c+d)),
                     levels=c("Cukup","Lebih"))
Kesehatan<-data.frame(Klasifikasi_Trigliserida,Asupan_Lemak)
colnames(Kesehatan)<-c("Klasifikasi Trigliserida","Asupan Lemak")


#------------------------------------ui.R--------------------------------------#
ui <- fluidPage(
  navbarPage(title = "Asosiasi Dua Peubah Kategorik",
             # Panel 1 : Khi Kuadrat           
             tabPanel("Khi Kuadrat",           
                      sidebarLayout(
                        sidebarPanel(
                          fluidRow(
                            column(6,selectInput(inputId = "data_input",
                                                 label = "Pilih Data : ",
                                                 choices = c("Isi Data",
                                                             "Data Contoh"))),
                            column(6,selectInput(inputId = "dataset",
                                                 label = "Pilih Data : ",
                                                 choices = c("Konsumsi Ikan",
                                                             "Perkembangan Anak",
                                                             "Inner Child",
                                                             "Teknologi Petani",
                                                             "Frekuensi Makan",
                                                             "Lemak")))),
                          fluidRow(
                            column(6,textInput("var_row", "Nama Peubah Baris :", "Contoh : Usia")),
                            column(6,textInput("var_col", "Nama Peubah Kolom :", "Contoh : Pendapatan"))),
                          fluidRow(
                            column(6,textInput("col_name", "Kolom :", "Kat 1,Kat 2")),
                            column(6,textInput("row_name", "Baris :", "Kat 1,Kat 2"))),
                          br(),
                          fluidRow(column(6,rHandsontableOutput("contingency_table"))),
                          br(),
                          fluidRow(column(12,selectInput(inputId = "output_test",label = "Pilih Hasil yang ingin ditampilkan : ",
                                                         choices = c("Khi Kuadrat","Rasio Odd","Resiko Relatif")))),
                          h5(""),
                          checkboxInput(inputId = "obs", label = HTML('Tampilkan Nilai <span style="color: #5703ff; font-weight: bold;">Observasi</span>')),
                          checkboxInput(inputId = "exp", label = "Tampilkan Nilai Harapan"),
                          checkboxInput(inputId = "row_per", label = "Tampilkan Persentase Baris"),
                          checkboxInput(inputId = "col_per", label = "Tampilkan Persentase Kolom")
                        ),
                        mainPanel("Deskriptif Data",
                                  fluidRow(
                                    column(5,htmlOutput("html")),  
                                    column(7,plotlyOutput("plotbar"))
                                  ),
                                  fluidRow(
                                    column(7,rHandsontableOutput("chisq"))
                                  ),br(),br()
                                  
                        )
                      )
             )
  )
)

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
  
  # Color for contingecy table
  col_n<-"#5703ff" #Biru
  col_exp<-"#ff3200" #Merah
  col_row<-"#96ceb4" #Hijau
  col_col<-"#ffcc5c" #Kuning
  
  output$contingency_table <- rhandsontable::renderRHandsontable({
    dataset <- datasetInput()
    dataset<-as.data.frame(table(dataset))
    dataset <- dataset %>% 
      pivot_wider(names_from = colnames(dataset)[2], values_from = colnames(dataset)[3]) %>%
      as.data.frame()
    rownames(dataset)<-dataset[,1]
    dataset<-dataset[,-1]
    rhandsontable(
      dataset,height =  100,width = 370,minRows=2,minCols=2,
      readOnly = F,stretchH = "all")
  })
  
  output$html <- renderUI({
    dataset <- datasetInput()
    label <- colnames(dataset)
    HTML(tab_xtab(var.row = dataset[,1], var.col = dataset[,2],
                  var.labels = c(label[1], label[2]),
                  title = "Tabel Kontingensi", show.summary=F,
                  show.row.prc = TRUE,show.exp = TRUE,show.col.prc = TRUE,
                  tdcol.n = col_n,
                  tdcol.expected = col_exp,
                  tdcol.row = col_row,
                  tdcol.col = col_col,
    )$knitr)
  })
  
  output$plotbar = renderPlotly({
    dataset <- datasetInput()
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
    fig <- plot_ly(x = dataset2[,label2[1]], y = dataset2[,label2[2]], type = 'bar', name = label2[2])
    fig <- fig %>% add_trace(y = dataset2[,label2[3]], name = label2[3])
    fig <- fig %>% layout(yaxis=list(title=list(text=paste0('<b> ','Frekuensi',' </b>'))),
                          xaxis=list(title=list(text=paste0('<b> ',label[2],' </b>'))),
                          legend=list(title=list(text=paste0('<b> ',label[1],' </b>'))), barmode = 'group')
  })
  
  output$chisq = rhandsontable::renderRHandsontable({
    dataset <- datasetInput()
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
  
  risk = rhandsontable::renderRHandsontable({
    dataset <- datasetInput()
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
  
  odd = rhandsontable::renderRHandsontable({
    dataset <- datasetInput()
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
