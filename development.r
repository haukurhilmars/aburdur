library(tidyverse)
library(shiny)
library(shinythemes)
library(readxl)
library(plotly)
library(shinyWidgets)

tegundir <-  read_xlsx("heimildir.xlsx", sheet = 1)
dreifing <-  read_xlsx("heimildir.xlsx", sheet = 2)
losun <- read_xlsx("heimildir.xlsx", sheet = 3)

ui <- fluidPage(
  theme = shinytheme("cosmo"),
  
  setSliderColor(rep('#42464b',4), 1:4),
  chooseSliderSkin("Flat"),
  
  titlePanel( div(h3("Hagræn- og loftslagsleg áhrif af notkun lífræns áburðar")),
              windowTitle="Áburður-reiknivél"
  ),
  
  column(
    width=4,
    selectInput("aburdur", "Lífrænn áburður",
                c("Bokashi",
                  "Fiskeldisúrgangur",
                  "Gor",
                  "Hrossatað",
                  "Hænsnaskítur",
                  "Kjúklingaskítur",
                  "Kjötmjöl",
                  "Kúamykja",
                  "Molta",
                  "Sauðatað",
                  "Seyra",
                  "Svartvatn",
                  "Svínaskítur"
                )),
    br(),
    
    sliderInput("hekt","Stærð Uppgræðslusvæðis", min=10, max=200, value=100, step=10),
    
    br(),
    
    sliderInput("nitur", "Viðmið-Köfnunarefni\n(kg N/ha.)", min=10, max=300, value=200, step=10),
    
    br(),
    
    sliderInput("km_lifraenn", "Lífrænn áburður flutningur að uppgræðslusvæði (km)", min=10, max=300, value=100, step=10),
    
    br(),
    
    sliderInput("km_tilbuinn",label="Tilbúinn áburður flutningur að uppgræðslusvæði (km)", min=10, max=300, value=100, step=10),
    
    br(),
    tags$img(src = "land.jpg",width=80, height=80),
    tags$img(src = "efla_hreint.jpg",width=80, height=25)
    
  ),
  
  column(
    width=8,
    fluidRow(
      tableOutput("tafla1"),
      tableOutput("tafla2")
    ),
    fluidRow(
      plotOutput("mynd1"),
      plotOutput("mynd2")
    ),
    fluidRow(
      plotOutput("mynd3")
    )
    
  )
  
)



server <- function(input, output, session){
  

}


shinyApp(ui, server)