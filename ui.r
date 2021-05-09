library(tidyverse)
library(shiny)
library(shinythemes)
library(readxl)
library(plotly)
library(shinyWidgets)
library(DT)

forsendur <-  read_xlsx("heimildir.xlsx", sheet = 1)

ui <- fluidPage(
  theme = shinytheme("cosmo"),
  
  setSliderColor(rep('#42464b',4), 1:4),
  chooseSliderSkin("Flat"),
  
  titlePanel( div(h2("Hagræn- og loftslagsleg áhrif af notkun lífræns áburðar")),
              windowTitle="Áburður-reiknivél"
  ),
  
  column(
    width=4,
    selectInput("aburdur", "Lífrænn áburður",
                c("Bokashi",
                  "Fiskeldisúrgangur",
                  "Fiskislóg",
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
    tags$img(src = "land.png",width=80, height=80),
    tags$img(src = "EFLA_hreint.png",width=80*1.25, height=25*1.25)
    
  ),
  
  column(
    width=8,
    
    tabsetPanel(type = "tabs",
                tabPanel(uiOutput("title_panel1"),
                         fluidRow(
                           plotlyOutput("kostn_mynd"),
                           plotlyOutput("losun_mynd") 
                         )
                         
                         
                ),
                tabPanel(uiOutput("title_panel2"),
                         fluidRow(
                           dataTableOutput("kostn_tafla"),
                           br(),
                           br(),
                           dataTableOutput("losun_tafla")
                         )
                         
                ),
                tabPanel("Samanburður við aðrar áburðartegundir",
                         fluidRow(
                           plotlyOutput("allir_mynd")
                         )
                )
                
                
    )
    
  )
  
)
