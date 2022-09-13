library(shiny)
library(shinydashboard)
library(DT)
library(readxl)
library(ggplot2)
library(plotly)
library(gganimate)
library(quantmod)
library(leaflet)
library(leafpop)
# esquisse::esquisser()

setwd("C:/Users/Tiaranisa/Documents/aaaaa mata kuliah/6 Sistem Informasi Manajemen/pasca ETS/Project")

pelanggan<-read_excel("databasepln.xlsx", sheet="JPel")
pendapatan<-read_excel("databasepln.xlsx", sheet="PPen")
energi<-read_excel("databasepln.xlsx", sheet="ETer")
daya<-read_excel("databasepln.xlsx", sheet="DTer")
pelanggansum<-read_excel("databasepln.xlsx", sheet="JPelSum")
pendapatansum<-read_excel("databasepln.xlsx", sheet="PPenSum")
energisum<-read_excel("databasepln.xlsx", sheet="ETerSum")
dayasum<-read_excel("databasepln.xlsx", sheet="DTerSum")

databasepln <- read_excel("databasepln.xlsx", 
                          sheet = "fix", col_types = c("text", 
                                                        "text", "text", "text", "numeric"))

ui<-
  dashboardPage(title="KELISTRIKAN BANDAR LAMPUNG",skin=c("yellow"),
                dashboardHeader(title="DASHBOARD"),
                dashboardSidebar(
                  sidebarMenu(
                    menuItem("HOME",tabName = "HOME",icon=icon('home')),
                    menuItem("DEFINISI",tabName="DEFINISI",icon=icon('book'),
                             menuSubItem("PROFIL ULP", tabName = "ULP"),
                             menuSubItem("VARIABEL", tabName = "VARIABEL")),
                    menuItem("DATABASE",tabName="DATABASE",icon=icon('hands-helping')),
                    menuItem("VISUALISASI",tabName="VISUALISASI",icon=icon('chart-bar'))
                  )
                ),
                dashboardBody(
                  tabItems(
                    tabItem(tabName="HOME",
                            h2(strong("DATA KELISTRIKAN"),align="center"),
                            h3(("PLN UIW LAMPUNG"),align="center"),
                            basicPage(
                              fluidRow(
                                box(strong("DESKRIPSI"),
                                    solidHeader = T,
                                    width=12,
                                    "",br(),
                                    "Tema   :",br(),
                                    "KELISTRIKAN KOTA BANDAR LAMPUNG",br(),
                                    "",br(),
                                    "Tujuan :",br(),
                                    "1. Mengetahui variabel-variabel terkait kelistrikan.",br(),
                                    "2. Menyusun database kelistrikan.",br(),
                                    "3. Mengetahui statistika deskriptif data kelistrikan.",br(),
                                    "4. Menyusun visualisasi data kelistrikan.",br(),
                                    "5. Melakukan pemetaan terhadap data kelistrikan.",br()
                                )
                              ),
#                              fluidRow(
 #                                     box(
  #                                     title = "Anggota 1", status = "warning", solidHeader = TRUE, collapsible = FALSE, collapsed = FALSE,
   #                                   tags$img(src="coba.png", width="150px", height="150px"), width = 6, align="center",
    #                                  h4("ICHWAN"),
     #                                h4("062118400000XX")),
      #                            box(
       #                            title = "Anggota 2", status = "warning", solidHeader = TRUE, collapsible = FALSE, collapsed = FALSE,
        #                          tags$img(src="coba.png", width="150px", height="150px"), width = 6, align="center",
         #                        h4("TIARA"),
          #                      h4("062118400000XX"))
           #                     )
                              fluidRow(
                                box(
                                  width=6,
                                  strong("Anggota 1:"),br(),
                                  strong("Ichwanul Kahfi Prasetya"),br(),
                                  strong("06211840000014"),br(),
                                  tags$img(src="ichwan.jpeg", width="200px", height="200px"),align="center"
                                ),
                                box(
                                  width=6,
                                  strong("Anggota 2:"),br(),
                                  strong("Tiaranisa'i Fadhilla"),br(),
                                  strong("06211840000032"),br(),
                                  tags$img(src="tiara.jpeg", width="200px", height="200px"),align="center"
                                )
                              )
                            )
                            
                    ),
                    tabItem(tabName="ULP",
                            box(width=12,
                                h2(strong("PROFIL ULP"),align="center"),
                                h3("Peta",align="center"),
                                leafletOutput("m"),
                                box(width=4,
                                    strong("ULP 1 :"),br(),
                                    "PLN ULP KARANG",br(),
                                    "Jl. Diponegoro No. 14, Gotong Royong",br(),
                                    "Kec. Tj. Karang Pusat, Kota Bandar Lampung",br(),
                                    "Lampung 35213",br(),
                                    "(0721) 267065",
                                    align="justify"),
                                box(width=4,
                                    strong("ULP 2 :"),br(),
                                    "PLN ULP WAY HALIM",br(),
                                    "Jl. Raden Gunawan, Rajabasa",br(),
                                    "Kec. Rajabasa, Kota Bandar Lampung",br(),
                                    "Lampung 35132",br(),
                                    "082371767808",
                                    align="justify"),
                                box(width=4,
                                    strong("ULP 3 :"),br(),
                                    "PLN ULP TELUK BETUNG",br(),
                                    "Jl. Basuki Rahmat No.02, Gedong Pakuon",br(),
                                    "Kec. Telukbetung Selatan, Kota Bandar Lampung",br(),
                                    "Lampung 35211",br(),
                                    "(0721) 474802",
                                    align="justify"
                                ))
                            
                    ),
                    tabItem(tabName="VARIABEL",
                            box(width=12,
                                h2(strong("VARIABEL"),align="center"),
                                box(width=6,
                                    strong("JUMLAH PELANGGAN"),br(),
                                    "Jumlah pelanggan berdasarkan Statistik PLN 2018 diberlakukan segmentasi menurut golongan tarif yaitu Pelayanan Sosial (S), Rumah Tangga (R), Bisnis (B), Industri (I), dan Kantor Pemerintahan (P).",
                                    align="justify"
                                ),
                                box(width=6,
                                    strong("DAYA TERSAMBUNG"),br(),
                                    "Berdasarkan definisi istilah kelistrikan PLN, pengertian daya tersambung adalah besarnya daya yang disepakati oleh PLN dan pelanggan dalam Perjanjian Jual Beli Tenaga Listrik yang menjadi dasar perhitungan biaya beban.",
                                    align="justify"
                                ),
                                box(width=6,
                                    strong("ENERGI TERJUAL"),br(),
                                    "Energi Terjual kepala pelanggan adalah energi (kWh) yang terjual kepada pelanggan TT (Tegangan Tinggi), TM (Tegangan Menengah), dan TR (Tegangan Rendah) sesuai dengan jumlah kWh yang dibuat rekening (TUL III-09).",
                                    align="justify"
                                ),
                                box(width=6,
                                    strong("PENDAPATAN PENJUALAN"),br(),
                                    "Pendapatan penjualan merupakan laporan penjualan berdasarkan penggunaan listrik pelanggan.",
                                    align="justify"
                                )
                            )
                    ),
                    tabItem(tabName="DATABASE",
                            tabsetPanel(
                              tabPanel("Jumlah Pelanggan",
                                       DT::dataTableOutput("JPel"),
                                       verbatimTextOutput("summaryJPel")),
                              tabPanel("Pendapatan Penjualan",
                                       DT::dataTableOutput("PPen"),
                                       verbatimTextOutput("summaryPPen")),
                              tabPanel("Energi Terjual",
                                       DT::dataTableOutput("ETer"),
                                       verbatimTextOutput("summaryETer")),
                              tabPanel("Daya Tersambung",
                                       DT::dataTableOutput("DTer"),
                                       verbatimTextOutput("summaryDTer"))
                            )
                    ),
                    tabItem(tabName="VISUALISASI",
                            h2(strong("VISUALIASI"),align="center"),
                            h2(),
                            tabsetPanel(
                              tabPanel("Jumlah Pelanggan",icon=icon('chart-line'),
                                       fluidPage(
                                         plotlyOutput("viz1")
                                       )
                              ),
                              tabPanel("Daya Tersambung",icon=icon('chart-line'),
                                       fluidPage(
                                         plotlyOutput("viz2")
                                       )
                              ),
                              tabPanel("Energi Terjual",icon=icon('chart-line'),
                                       fluidPage(
                                         plotlyOutput("viz3")
                                       )
                              ),
                              tabPanel("Pendapatan Penjualan",icon=icon('chart-line'),
                                       fluidPage(
                                         plotlyOutput("viz4")
                                       )
                              )
                            )
                          )
                    
                  )))

server <- function(input, output) {
  output$JPel = DT::renderDataTable({
    pelanggan
  })
  output$PPen = DT::renderDataTable({
    pendapatan
  })
  output$ETer = DT::renderDataTable({
    energi
  })
  output$DTer = DT::renderDataTable({
    daya
  })
  output$summaryJPel=renderPrint({
    descr(pelanggansum)
  })
  output$summaryPPen=renderPrint({
    descr(pendapatansum)
  })
  output$summaryETer=renderPrint({
    descr(energisum)
  })
  output$summaryDTer=renderPrint({
    descr(dayasum)
  })
  output$m = renderLeaflet({
    df<-structure(list(Points = c("ULP Karang", "ULP Way Halim", "ULP Teluk Betung"), latitudes = c(-5.423884, -5.365598, -5.224912), 
                       longitudes = c(105.258040, 105.219391, 105.254705)), class="data.frame",row.names = c(NA, -3L))
    m=leaflet(data=df)%>%
      addTiles() %>%
      addMarkers(lng=~longitudes, lat=~latitudes, popup=points)
  })
  output$viz1=renderPlotly({
    databasepln %>%
      filter(Variabel %in% "Jumlah Pelanggan") %>%
      ggplot() +
      aes(x = Tahun, fill = `Golongan Tarif`, weight = Nilai) +
      geom_bar() +
      scale_fill_brewer(palette = "Set3", direction = 1) +
      theme_minimal() +
      facet_wrap(vars(ULP))
  })
  output$viz2=renderPlotly({
    databasepln %>%
      filter(Variabel %in% "Daya Tersambung") %>%
      ggplot() +
      aes(x = Tahun, fill = `Golongan Tarif`, weight = Nilai) +
      geom_bar() +
      scale_fill_brewer(palette = "Set3", direction = 1) +
      theme_minimal() +
      facet_wrap(vars(ULP))
  })
  output$viz3=renderPlotly({
    databasepln %>%
      filter(Variabel %in% "Energi Terjual") %>%
      ggplot() +
      aes(x = Tahun, fill = `Golongan Tarif`, weight = Nilai) +
      geom_bar() +
      scale_fill_brewer(palette = "Set3", direction = 1) +
      theme_minimal() +
      facet_wrap(vars(ULP))
  })
  output$viz4=renderPlotly({
    databasepln %>%
      filter(Variabel %in% "Pendapatan Penjualan") %>%
      ggplot() +
      aes(x = Tahun, fill = `Golongan Tarif`, weight = Nilai) +
      geom_bar() +
      scale_fill_brewer(palette = "Set3", direction = 1) +
      theme_minimal() +
      facet_wrap(vars(ULP))
  })
}

shinyApp (ui, server)

