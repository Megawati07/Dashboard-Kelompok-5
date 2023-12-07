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
