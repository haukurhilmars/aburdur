library(tidyverse)
library(shiny)
library(shinythemes)
library(readxl)
library(plotly)
library(shinyWidgets)
library(DT)

forsendur <-  read_xlsx("heimildir.xlsx", sheet = 1)

server <- function(input, output, session){
  
  output$title_panel1 = renderText({
    paste0(input$aburdur, " - Tilbúinn áburður, myndir") 
  })
  
  output$title_panel2 = renderText({
    paste0(input$aburdur, " - Tilbúinn áburður, tafla") 
  })
  
  data_manip <- reactive({
    df <- forsendur%>%
      mutate(
        #Magn
        kg_hekt = input$nitur/(N/100), #Magn á hektara reiknað út frá niturviðmiði og niturmagn áburðar
        magn_kg = input$hekt * kg_hekt, #Magn á hektara margfaldað með fjölda hektara
        
        #Innkaup
        innkaupkostn = magn_kg/1000*innkaupsverd, #Heildarmagn(kg)/1000 * verð á tonn
        co2_framl = magn_kg * los_framl, #Heildarmagn(kg)*losun co2 per kg framleitt
        
        #Flutningur
        fj_ferda_flutningur = ceiling(magn_kg/1000/magn_per_flutn), #Heildarmagn(kg)/1000 / tonn sem kemst í hverja flutningsferd (byggt á teg flutnings)
        fj_km = ifelse(aburdur == "Tilbúinn áburður", input$km_tilbuinn, input$km_lifraenn)*fj_ferda_flutningur*2, #Fjarlægð*fjöldi ferða*2(fram og til baka)
        
        flutningskostn = fj_km * flutningsverd, #Fjöldi km*kostn per km
        co2_flutn = los_flutn*fj_km, #Losun per km (byggt á tegund flutnings)* fj km
        
        #Dreifing
        N_per_ferd = ifelse(N_per_ferd_proxy == 200, input$nitur, N_per_ferd_proxy), #Skilgreinum þannig að f. tilbúinn og kjötmjöl þarf stundum fleiri en 1 dreifingarumferd
        fj_ferda_dreifing = ceiling(input$nitur/N_per_ferd), #Fjöldi ferða í dreifingu, byggt á niturviðmiði og nitri sem hægt er að láta á í hverri ferð
        kg_dreift_ferd = magn_kg/fj_ferda_dreifing, #Heildarmagn(kg)/fjöldi dreifingarferða
        fj_ferda_i_hekt = 100/dreifibreidd, #miðum við 100m*100m hektara, 100/dreifibreidd
        sek_per_ferd = 100/(medalhradi_dreif*5/18), #sekundur að keyra 100 m, breytum km/klst í m/s
        sek_hekt = fj_ferda_i_hekt*sek_per_ferd, #Tími að covera hektara, sek per ferd*fjöldi ferða
        min_hekt = sek_hekt/60, #breytum í min að covera hekt
        hekt_per_klst = 60/min_hekt, #Fjöldi hektara sem er hægt að covera á klst
        
        klst_akstur = input$hekt/hekt_per_klst*fj_ferda_dreifing, #hektarar/hekt per klst * fjöldi dreifinga
        
        fj_afyllinga = kg_dreift_ferd /1000/ magn_per_dreif, #fj afyllingar = magn í dreifingu(kg)/1000/hversu mikið kemst á tæki (tonn)
        klst_afyllingar = fj_afyllinga*afylling_min/60*fj_ferda_dreifing, # fjöldi afyllinga*min per afylling /60 (minutur breytt i klst)*fj dreifinga
        
        heildartimi = klst_akstur+klst_afyllingar, #Heildartími dreifing
        
        kostn_dreifing = heildartimi*(laun_starfsm+leiga_drattarvel+leiga_dreiftaeki), #Fj klst*laun+leigukostnaður per klst
        
        kostn_amokstur = amokstur * kg_dreift_ferd/1000*fj_ferda_dreifing, #kostnaður við ámokstur (kr/tonn)* magn(kg)/1000*fj_dreifingarferda
        
        dreifingarkostn = kostn_dreifing+kostn_amokstur,#Kostnaður við dreifinguna + amokstur
        co2_dreif = heildartimi*los_dreif #heildartimi*los per klst akstur
      )
  })
  
  # Myndir um samanburð á tilbúnum og lífrænum áburði  
  tilbuinn_kost <- reactive({
    flutn_kost <- data_manip()%>%
      filter(aburdur=="Tilbúinn áburður")%>%
      select(flutningskostn)%>%
      pull()
    
    
    dreif_kost <-  data_manip()%>%
      filter(aburdur=="Tilbúinn áburður")%>%
      select(dreifingarkostn)%>%
      pull()
    
    innkaup_kost <-  data_manip()%>%
      filter(aburdur=="Tilbúinn áburður")%>%
      select(innkaupkostn)%>%
      pull()
    
    c(flutn_kost, dreif_kost, innkaup_kost)
    
    
  })
  
  
  lifraenn_kost <- reactive({
    flutn_kost <- data_manip()%>%
      filter(aburdur==input$aburdur)%>%
      select(flutningskostn)%>%
      pull()
    
    dreif_kost <-  data_manip()%>%
      filter(aburdur==input$aburdur)%>%
      select(dreifingarkostn)%>%
      pull()
    
    innkaup_kost <-  data_manip()%>%
      filter(aburdur==input$aburdur)%>%
      select(innkaupkostn)%>%
      pull()
    
    c(flutn_kost, dreif_kost, innkaup_kost)
  })
  
  
  tegundir <- reactive({c("Tilbúinn áburður", input$aburdur)})
  flutn_kost <- reactive({c(tilbuinn_kost()[1], lifraenn_kost()[1])})
  dreif_kost <- reactive({c(tilbuinn_kost()[2], lifraenn_kost()[2])})
  innk_kost <- reactive({c(tilbuinn_kost()[3], lifraenn_kost()[3])})
  
  plot_df_kost <- reactive({
    data.frame(tegundir =tegundir(), flutn_kost = flutn_kost(), dreif_kost = dreif_kost(),innk_kost =  innk_kost())
  })
  
  
  tilbuinn_los <- reactive({
    flutn_los <- data_manip()%>%
      filter(aburdur=="Tilbúinn áburður")%>%
      select(co2_flutn)%>%
      pull()
    
    
    dreif_los <-  data_manip()%>%
      filter(aburdur=="Tilbúinn áburður")%>%
      select(co2_dreif)%>%
      pull()
    
    innkaup_los <-  data_manip()%>%
      filter(aburdur=="Tilbúinn áburður")%>%
      select(co2_framl)%>%
      pull()
    
    c(flutn_los, dreif_los, innkaup_los)
    
    
  }) 
  
  
  
  lifraenn_los <- reactive({
    flutn_los <- data_manip()%>%
      filter(aburdur==input$aburdur)%>%
      select(co2_flutn)%>%
      pull()
    
    dreif_los <-  data_manip()%>%
      filter(aburdur==input$aburdur)%>%
      select(co2_dreif)%>%
      pull()
    
    innkaup_los <-  data_manip()%>%
      filter(aburdur==input$aburdur)%>%
      select(co2_framl)%>%
      pull()
    
    c(flutn_los, dreif_los, innkaup_los)
  })
  
  flutn_los <- reactive({c(tilbuinn_los()[[1]], lifraenn_los()[[1]])})
  dreif_los <- reactive({c(tilbuinn_los()[[2]], lifraenn_los()[[2]])})
  innk_los <- reactive({c(tilbuinn_los()[[3]], lifraenn_los()[[3]])})
  
  
  plot_df_los <- reactive({
    data.frame(tegundir =tegundir(), flutn_los = flutn_los(), dreif_los = dreif_los(),innk_los =  innk_los())
  })
  
  
  output$kostn_mynd <-  renderPlotly({
    
    plot_ly(plot_df_kost(), x=~tegundir, y=~flutn_kost, type='bar', name='Flutningskostnaður',hoverinfo = 'text',
            text="Kostnaður við flutning",
            marker = list(color='rgb(152,158,163)',line=list(color = 'rgb(0,0,0)', width=1.25)))%>%
      add_trace(y = ~dreif_kost, name="Dreifingarkostnaður",hoverinfo = 'text',
                text="Kostnaður við dreifingu",
                marker=list(color='rgb(66,70,75)',
                            line=list(color = 'rgb(0,0,0)', width=1.25)))%>%
      add_trace(y = ~innk_kost, name = "Innkaupakostnaður",hoverinfo = 'text',
                text="Kostnaður við innkaup", 
                marker=list(color='rgb(238,53,36)',
                            line=list(color = 'rgb(0,0,0)', width=1.25)))%>%
      layout(yaxis =list(title="Kostnaður"), 
             xaxis = list(title = "Tegund áburðar"),
             barmode='stack')
    
    
  })
  
  
  
  output$losun_mynd <-  renderPlotly({
    
    plot_ly(plot_df_los(), x=~tegundir, y=~flutn_los, type='bar', name='Losun vegna flutnings',hoverinfo = 'text',
            text="Losun vegna flutnings",
            marker = list(color='rgb(152,158,163)',line=list(color = 'rgb(0,0,0)', width=1.25)))%>%
      add_trace(y = ~dreif_los, name="Losun vegna dreifingar",hoverinfo = 'text',
                text="Losun vegna dreifingar",
                marker=list(color='rgb(66,70,75)',
                            line=list(color = 'rgb(0,0,0)', width=1.25)))%>%
      add_trace(y = ~innk_los, name = "Losun vegna framleiðslu",hoverinfo = 'text',
                text="Losun vegna framleiðslu",
                marker=list(color='rgb(238,53,36)',
                            line=list(color = 'rgb(0,0,0)', width=1.25)))%>%
      layout(yaxis =list(title= "Kg CO2 ígildi"),
             xaxis = list(title = "Tegund áburðar"),
             barmode='stack')
    
    
  })
  
  
  #Stór mynd með öllum tegundum
  big_plot_df <- reactive({
    data_manip()%>%
      mutate(heildarkostn = innkaupkostn+dreifingarkostn+flutningskostn,
             heildarlosun = co2_framl+co2_flutn+co2_dreif)%>%
      select(aburdur, heildarkostn,heildarlosun)
  })
  
  output$allir_mynd <- renderPlotly({
    
    plot_ly(big_plot_df(), x=~heildarlosun, y=~heildarkostn, type="scatter",
            text = ~paste(aburdur), hoverinfo = "text",
            marker = list(size = 10,
                          color = 'rgb(238,53,36)',
                          line = list(color = 'rgba(0, 0, 0, 1)',
                                      width = 2)))%>%
      layout(
        yaxis = list(zeroline = FALSE, title="Krónur"),
        xaxis = list(zeroline = FALSE, title="Kg CO2 ígildi"))
    
  })
  
  
  #Töflur
  
  temp1 <- reactive({
    plot_df_kost()%>%
      pivot_longer(-tegundir)
  })
  
  
  kostntafla <- reactive({
    
    jaja <-  tibble("Kostnaður" = c("Kostnaður við innkaup", "Kostnaður við dreifingu", "Kostnaður við flutning"),
                    "Lifrænn" = c(temp1()%>%filter(tegundir == input$aburdur & name=="innk_kost")%>%pull()%>%round(),
                                  temp1()%>%filter(tegundir == input$aburdur & name=="dreif_kost")%>%pull()%>%round(),
                                  temp1()%>%filter(tegundir == input$aburdur & name=="flutn_kost")%>%pull()%>%round()
                    ),
                    "Tilbúinn áburður" = c(temp1()%>%filter(tegundir == "Tilbúinn áburður" & name=="innk_kost")%>%pull()%>%round(),
                                           temp1()%>%filter(tegundir == "Tilbúinn áburður" & name=="dreif_kost")%>%pull()%>%round(),
                                           temp1()%>%filter(tegundir == "Tilbúinn áburður" & name=="flutn_kost")%>%pull()%>%round()
                    )
    )
    
    colnames(jaja)[which(colnames(jaja)=="Lifrænn")]=input$aburdur
    
    jaja
  })
  
  
  temp2 <- reactive({
    plot_df_los()%>%
      pivot_longer(-tegundir)
  })
  
  
  lostafla <- reactive({
    
    jaja <-  tibble("Kostnaður" = c("Losun vegna framleiðslu", "Losun vegna dreifingar", "Losun vegna flutnings"),
                    "Lifrænn" = c(temp2()%>%filter(tegundir == input$aburdur & name=="innk_los")%>%pull()%>%round(),
                                  temp2()%>%filter(tegundir == input$aburdur & name=="dreif_los")%>%pull()%>%round(),
                                  temp2()%>%filter(tegundir == input$aburdur & name=="flutn_los")%>%pull()%>%round()
                    ),
                    "Tilbúinn áburður" = c(temp2()%>%filter(tegundir == "Tilbúinn áburður" & name=="innk_los")%>%pull()%>%round(),
                                           temp2()%>%filter(tegundir == "Tilbúinn áburður" & name=="dreif_los")%>%pull()%>%round(),
                                           temp2()%>%filter(tegundir == "Tilbúinn áburður" & name=="flutn_los")%>%pull()%>%round()
                    )
    )
    
    colnames(jaja)[which(colnames(jaja)=="Lifrænn")]=input$aburdur
    
    jaja
  })
  
  output$kostn_tafla <- renderDataTable(datatable(kostntafla(), class='hover', rownames = FALSE,
                                                  options = list(dom='t'))%>%
                                          formatCurrency(2:3,currency="", mark = ".", digits=0)
  )
  output$losun_tafla <- renderDataTable(datatable(lostafla(), class='hover', rownames = FALSE,
                                                  options=list(dom='t'))%>%
                                          formatCurrency(2:3,currency="", mark = ".", digits=0)
  )
  
}