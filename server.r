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
        co2_dreif = heildartimi*los_dreif, #heildartimi*los per klst akstur
        
        co2_e_dreif = los_n*magn_kg #Losun vegna niturs, eftir dreifingu
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
    
    e_dreif_los <- data_manip()%>%
      filter(aburdur=="Tilbúinn áburður")%>%
      select(co2_e_dreif)%>%
      pull()
    
    c(flutn_los, dreif_los, innkaup_los,  e_dreif_los)
    
    
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
    
    e_dreif_los <-  data_manip()%>%
      filter(aburdur==input$aburdur)%>%
      select(co2_e_dreif)%>%
      pull()
    
    c(flutn_los, dreif_los, innkaup_los, e_dreif_los)
  })
  
  flutn_los <- reactive({c(tilbuinn_los()[[1]], lifraenn_los()[[1]])})
  dreif_los <- reactive({c(tilbuinn_los()[[2]], lifraenn_los()[[2]])})
  innk_los <- reactive({c(tilbuinn_los()[[3]], lifraenn_los()[[3]])})
  e_dreif_los <- reactive({c(tilbuinn_los()[[4]], lifraenn_los()[[4]])})
  
  
  plot_df_los <- reactive({
    data.frame(tegundir =tegundir(), flutn_los = flutn_los(), dreif_los = dreif_los(),innk_los =  innk_los(),
               e_dreif_los = e_dreif_los())
  })
  
  tot_magn_lifr <- reactive({
    data_manip()%>%
      filter(aburdur == input$aburdur)%>%
      select(magn_kg)%>%
      pull()
  })
  
  magn_hekt_lifr <- reactive({
    data_manip()%>%
      filter(aburdur == input$aburdur)%>%
      select(kg_hekt)%>%
      pull()
  })
  
  tot_magn_tilb <- reactive({
    data_manip()%>%
      filter(aburdur == "Tilbúinn áburður")%>%
      select(magn_kg)%>%
      pull()
  })
  
  magn_hekt_tilb <- reactive({
    data_manip()%>%
      filter(aburdur == "Tilbúinn áburður")%>%
      select(kg_hekt)%>%
      pull()
  })
  
  kost_y_lifr <- reactive({
    sum(lifraenn_kost())
  })
  
  kost_y_tilb <- reactive({
    sum(tilbuinn_kost())
  })
  
  los_y_lifr <- reactive({
    sum(lifraenn_los())
  })
  
  los_y_tilb <- reactive({
    sum(tilbuinn_los())
  })
  
  output$kostn_mynd <-  renderPlotly({
    
    plot_ly(plot_df_kost(), x=~tegundir, y=~flutn_kost, type='bar', name='Flutningskostnaður',hoverinfo = 'text',
            text="Kostnaður við flutning",
            marker = list(color='rgb(0,0,0)',line=list(color = 'rgb(0,0,0)', width=0)))%>%
      add_trace(y = ~dreif_kost, name="Dreifingarkostnaður",hoverinfo = 'text',
                text="Kostnaður við dreifingu",
                marker=list(color='rgb(247,245,173)',
                            line=list(color = 'rgb(247,245,173)', width=0)))%>%
      add_trace(y = ~innk_kost, name = "Innkaupakostnaður",hoverinfo = 'text',
                text="Kostnaður við innkaup", 
                marker=list(color='rgb(199,207,216)',
                            line=list(color = 'rgb(199,207,216)', width=0)))%>%
      layout(yaxis =list(title="Kostnaður"), 
             xaxis = list(title = ""),
             barmode='stack',
             hovermode = 'compare',showlegend = FALSE)%>%
      add_annotations(x = 0.1,
                      y=kost_y_lifr()*1.2,
                      text=paste0("Kg/ha: ",round(magn_hekt_lifr()),"<br>Heildarmagn: ", round(tot_magn_lifr()/1000), " tonn"),
                      xref="paper", yref="y",showarrow=FALSE, align='left')%>%
      add_annotations(x = 0.9,
                      y=kost_y_tilb()+(.2*kost_y_lifr()),
                      text=paste0("Kg/ha: ",round(magn_hekt_tilb()),"<br>Heildarmagn: ", round(tot_magn_tilb()/1000), " tonn"),
                      xref="paper", yref="y",showarrow=FALSE, align='right')
  })
  
  
  
  output$losun_mynd <-  renderPlotly({
    
    plot_ly(plot_df_los(), x=~tegundir, y=~flutn_los, type='bar', name='Losun vegna flutnings',hoverinfo = 'text',
            text="Losun vegna flutnings",
            marker = list(color='rgb(0,0,0)',line=list(color = 'rgb(0,0,0)', width=0)))%>%
      add_trace(y = ~dreif_los, name="Losun vegna dreifingar",hoverinfo = 'text',
                text="Losun vegna dreifingar",
                marker=list(color='rgb(247,245,173)',
                            line=list(color = 'rgb(247,245,173)', width=0)))%>%
      add_trace(y = ~innk_los, name = "Losun vegna framleiðslu",hoverinfo = 'text',
                text="Losun vegna framleiðslu",
                marker=list(color='rgb(199,207,216)',
                            line=list(color = 'rgb(199,207,216)', width=0)))%>%
      add_trace(y = ~e_dreif_los, name = "Losun eftir dreifingu",hoverinfo = 'text',
                text="Losun eftir dreifingu",
                marker=list(color='rgb(5,166,107)',
                            line=list(color = 'rgb(5,166,107)', width=0)))%>%
      layout(yaxis =list(title= "Kg CO2 ígildi"),
             xaxis = list(title = ""),
             barmode='stack',
             hovermode = 'compare',showlegend = FALSE)
    # %>%
    #   add_annotations(x = 0.1,
    #                   y=los_y_lifr()+(los_y_tilb()*0.2),
    #                   text=paste0("Kg/ha: ",round(magn_hekt_lifr()),"<br>Heildarmagn: ", round(tot_magn_lifr()/1000), " tonn"),
    #                   xref="paper", yref="y",showarrow=FALSE, align='left')%>%
    #   add_annotations(x = 0.9,
    #                   y=los_y_tilb()*1.2,
    #                   text=paste0("Kg/ha: ",round(magn_hekt_tilb()),"<br>Heildarmagn: ", round(tot_magn_tilb()/1000), " tonn"),
    #                   xref="paper", yref="y",showarrow=FALSE, align='right')
    
    
  })
  
  
  #Stór mynd með öllum tegundum
  big_plot_df <- reactive({
    data_manip()%>%
      mutate(heildarkostn = innkaupkostn+dreifingarkostn+flutningskostn,
             heildarlosun = co2_framl+co2_flutn+co2_dreif+co2_e_dreif)%>%
      select(aburdur, heildarkostn,heildarlosun)%>%
      mutate(valinn = ifelse(aburdur == input$aburdur,1,0))
  })
  
  
  output$allir_mynd <- renderPlotly({
    
    plot_ly(filter(big_plot_df(),valinn==0), x=~heildarlosun, y=~heildarkostn, type="scatter",
            text = ~paste(aburdur), hoverinfo = "text",
            marker = list(size = 12,
                          color = 'rgb(247,245,173)',
                          line = list(color = 'rgba(0, 0, 0, 1)',
                                      width = 2)))%>%
      add_trace(data=filter(big_plot_df(),valinn==1), x=~heildarlosun, y=~heildarkostn, type="scatter",
                text = ~paste(aburdur), hoverinfo = "text",
                marker = list(size = 12,
                              color = 'rgb(5,166,107)',
                              line = list(color = 'rgba(0, 0, 0, 1)',
                                          width = 2)))%>%
      add_annotations(
        x=filter(big_plot_df(),valinn==1)$heildarlosun, y=filter(big_plot_df(),valinn==1)$heildarkostn,text = filter(big_plot_df(),valinn==1)$aburdur,
        xref = "x",
        yref = "y",
        showarrow = TRUE,
        arrowhead = 4,
        arrowsize = .5,
        ax = 0,
        ay = -40
      )%>%
      layout(
        yaxis = list(zeroline = FALSE, title="Krónur"),
        xaxis = list(zeroline = FALSE, title="Kg CO2 ígildi"),
        showlegend = FALSE)
    
  })
  
  # output$allir_mynd <- renderPlot({
  #   ggplot(big_plot_df(), aes(x=heildarlosun, y=heildarkostn, color=as.factor(valinn), label=aburdur))+
  #     geom_point(size=3)+
  #     geom_label_repel(color="black", nudge_y = max(big_plot_df()$heildarkostn)*0.2, nudge_x=max(big_plot_df()$heildarlosun)*0.05)+
  #     scale_y_continuous(label = scales::comma_format(big.mark = ".",decimal.mark=","),
  #                        limits = c(0, max(big_plot_df()$heildarkostn)*1.1))+
  #     scale_x_continuous(label = scales::comma_format(big.mark = ".",decimal.mark=","),
  #                        limits = c(0, max(big_plot_df()$heildarlosun)*1.1))+
  #     scale_color_manual(values=c("#f7f5ad","#05a66b"))+
  #     labs(y="Krónur",
  #          x="Kg CO2 ígildi")+
  #     theme_minimal()+
  #     theme(legend.position = "none",
  #           panel.grid.major.x= element_blank(),
  #           axis.text= element_text(size=14, colour = "black"),
  #           axis.title=element_text(size=18, colour = "black"))
  # 
  # })
  
  
  #Töflur
  
  temp1 <- reactive({
    plot_df_kost()%>%
      pivot_longer(-tegundir)
  })
  
  
  kostntafla <- reactive({
    
    kostn_tafla <-  tibble("Kostnaður á hektara (Kr)" = c("Kostnaður við innkaup", "Kostnaður við dreifingu", "Kostnaður við flutning", "Heildarkostnaður á hektara"),
                           "Lifrænn" = c(temp1()%>%filter(tegundir == input$aburdur & name=="innk_kost")%>%pull()%>%round(),
                                         temp1()%>%filter(tegundir == input$aburdur & name=="dreif_kost")%>%pull()%>%round(),
                                         temp1()%>%filter(tegundir == input$aburdur & name=="flutn_kost")%>%pull()%>%round(),
                                         sum(
                                           temp1()%>%filter(tegundir == input$aburdur & name=="innk_kost")%>%pull()%>%round(),
                                           temp1()%>%filter(tegundir == input$aburdur & name=="dreif_kost")%>%pull()%>%round(),
                                           temp1()%>%filter(tegundir == input$aburdur & name=="flutn_kost")%>%pull()%>%round()
                                         )
                           ),
                           "Tilbúinn áburður" = c(temp1()%>%filter(tegundir == "Tilbúinn áburður" & name=="innk_kost")%>%pull()%>%round(),
                                                  temp1()%>%filter(tegundir == "Tilbúinn áburður" & name=="dreif_kost")%>%pull()%>%round(),
                                                  temp1()%>%filter(tegundir == "Tilbúinn áburður" & name=="flutn_kost")%>%pull()%>%round(),
                                                  sum(
                                                    temp1()%>%filter(tegundir == "Tilbúinn áburður" & name=="innk_kost")%>%pull()%>%round(),
                                                    temp1()%>%filter(tegundir == "Tilbúinn áburður" & name=="dreif_kost")%>%pull()%>%round(),
                                                    temp1()%>%filter(tegundir == "Tilbúinn áburður" & name=="flutn_kost")%>%pull()%>%round()
                                                  )
                           )
    )%>%
      mutate(Lifrænn = Lifrænn/input$hekt,
             `Tilbúinn áburður` =`Tilbúinn áburður`/input$hekt)
    
    colnames(kostn_tafla)[which(colnames(kostn_tafla)=="Lifrænn")]=input$aburdur
    
    kostn_tafla
  })
  
  kostntafla2 <- reactive({
    
    kostn_tafla <-  tibble("Heildarkostnaður (Kr)" = c("Kostnaður við innkaup", "Kostnaður við dreifingu", "Kostnaður við flutning", "Heildarkostnaður"),
                           "Lifrænn" = c(temp1()%>%filter(tegundir == input$aburdur & name=="innk_kost")%>%pull()%>%round(),
                                         temp1()%>%filter(tegundir == input$aburdur & name=="dreif_kost")%>%pull()%>%round(),
                                         temp1()%>%filter(tegundir == input$aburdur & name=="flutn_kost")%>%pull()%>%round(),
                                         sum(
                                           temp1()%>%filter(tegundir == input$aburdur & name=="innk_kost")%>%pull()%>%round(),
                                           temp1()%>%filter(tegundir == input$aburdur & name=="dreif_kost")%>%pull()%>%round(),
                                           temp1()%>%filter(tegundir == input$aburdur & name=="flutn_kost")%>%pull()%>%round()
                                         )
                           ),
                           "Tilbúinn áburður" = c(temp1()%>%filter(tegundir == "Tilbúinn áburður" & name=="innk_kost")%>%pull()%>%round(),
                                                  temp1()%>%filter(tegundir == "Tilbúinn áburður" & name=="dreif_kost")%>%pull()%>%round(),
                                                  temp1()%>%filter(tegundir == "Tilbúinn áburður" & name=="flutn_kost")%>%pull()%>%round(),
                                                  sum(
                                                    temp1()%>%filter(tegundir == "Tilbúinn áburður" & name=="innk_kost")%>%pull()%>%round(),
                                                    temp1()%>%filter(tegundir == "Tilbúinn áburður" & name=="dreif_kost")%>%pull()%>%round(),
                                                    temp1()%>%filter(tegundir == "Tilbúinn áburður" & name=="flutn_kost")%>%pull()%>%round()
                                                  )
                           )
    )
    
    colnames(kostn_tafla)[which(colnames(kostn_tafla)=="Lifrænn")]=input$aburdur
    
    kostn_tafla
  })
  
  
  temp2 <- reactive({
    plot_df_los()%>%
      pivot_longer(-tegundir)
  })
  
  
  lostafla <- reactive({
    
    los_tafla <-  tibble("Losun á hektara (Kg CO2 ígildi)" = c("Losun vegna framleiðslu", "Losun vegna dreifingar", "Losun vegna flutnings", "Losun eftir dreifingu", "Heildarlosun á hektara"),
                         "Lifrænn" = c(temp2()%>%filter(tegundir == input$aburdur & name=="innk_los")%>%pull()%>%round(),
                                       temp2()%>%filter(tegundir == input$aburdur & name=="dreif_los")%>%pull()%>%round(),
                                       temp2()%>%filter(tegundir == input$aburdur & name=="flutn_los")%>%pull()%>%round(),
                                       temp2()%>%filter(tegundir == input$aburdur & name=="e_dreif_los")%>%pull()%>%round(),
                                       sum(
                                         temp2()%>%filter(tegundir == input$aburdur & name=="innk_los")%>%pull()%>%round(),
                                         temp2()%>%filter(tegundir == input$aburdur & name=="dreif_los")%>%pull()%>%round(),
                                         temp2()%>%filter(tegundir == input$aburdur & name=="flutn_los")%>%pull()%>%round(),
                                         temp2()%>%filter(tegundir == input$aburdur & name=="e_dreif_los")%>%pull()%>%round()
                                       )
                                       
                         ),
                         "Tilbúinn áburður" = c(temp2()%>%filter(tegundir == "Tilbúinn áburður" & name=="innk_los")%>%pull()%>%round(),
                                                temp2()%>%filter(tegundir == "Tilbúinn áburður" & name=="dreif_los")%>%pull()%>%round(),
                                                temp2()%>%filter(tegundir == "Tilbúinn áburður" & name=="flutn_los")%>%pull()%>%round(),
                                                temp2()%>%filter(tegundir == "Tilbúinn áburður" & name=="e_dreif_los")%>%pull()%>%round(),
                                                sum(
                                                  temp2()%>%filter(tegundir == "Tilbúinn áburður" & name=="innk_los")%>%pull()%>%round(),
                                                  temp2()%>%filter(tegundir == "Tilbúinn áburður" & name=="dreif_los")%>%pull()%>%round(),
                                                  temp2()%>%filter(tegundir == "Tilbúinn áburður" & name=="flutn_los")%>%pull()%>%round(),
                                                  temp2()%>%filter(tegundir == "Tilbúinn áburður" & name=="e_dreif_los")%>%pull()%>%round()
                                                )
                                                
                         )
    )%>%
      mutate(Lifrænn = Lifrænn/input$hekt,
             `Tilbúinn áburður` =`Tilbúinn áburður`/input$hekt)
    
    colnames(los_tafla)[which(colnames(los_tafla)=="Lifrænn")]=input$aburdur
    
    los_tafla
  })
  
  lostafla2 <- reactive({
    
    los_tafla <-  tibble("Heildarlosun (Kg CO2 ígildi)" = c("Losun vegna framleiðslu", "Losun vegna dreifingar", "Losun vegna flutnings", "Losun eftir dreifingu", "Heildarlosun"),
                         "Lifrænn" = c(temp2()%>%filter(tegundir == input$aburdur & name=="innk_los")%>%pull()%>%round(),
                                       temp2()%>%filter(tegundir == input$aburdur & name=="dreif_los")%>%pull()%>%round(),
                                       temp2()%>%filter(tegundir == input$aburdur & name=="flutn_los")%>%pull()%>%round(),
                                       temp2()%>%filter(tegundir == input$aburdur & name=="e_dreif_los")%>%pull()%>%round(),
                                       sum(
                                         temp2()%>%filter(tegundir == input$aburdur & name=="innk_los")%>%pull()%>%round(),
                                         temp2()%>%filter(tegundir == input$aburdur & name=="dreif_los")%>%pull()%>%round(),
                                         temp2()%>%filter(tegundir == input$aburdur & name=="flutn_los")%>%pull()%>%round(),
                                         temp2()%>%filter(tegundir == input$aburdur & name=="e_dreif_los")%>%pull()%>%round()
                                       )
                         ),
                         "Tilbúinn áburður" = c(temp2()%>%filter(tegundir == "Tilbúinn áburður" & name=="innk_los")%>%pull()%>%round(),
                                                temp2()%>%filter(tegundir == "Tilbúinn áburður" & name=="dreif_los")%>%pull()%>%round(),
                                                temp2()%>%filter(tegundir == "Tilbúinn áburður" & name=="flutn_los")%>%pull()%>%round(),
                                                temp2()%>%filter(tegundir == "Tilbúinn áburður" & name=="e_dreif_los")%>%pull()%>%round(),
                                                sum(
                                                  temp2()%>%filter(tegundir == "Tilbúinn áburður" & name=="innk_los")%>%pull()%>%round(),
                                                  temp2()%>%filter(tegundir == "Tilbúinn áburður" & name=="dreif_los")%>%pull()%>%round(),
                                                  temp2()%>%filter(tegundir == "Tilbúinn áburður" & name=="flutn_los")%>%pull()%>%round(),
                                                  temp2()%>%filter(tegundir == "Tilbúinn áburður" & name=="e_dreif_los")%>%pull()%>%round()
                                                )
                         )
    )
    
    colnames(los_tafla)[which(colnames(los_tafla)=="Lifrænn")]=input$aburdur
    
    los_tafla
  })
  

  
  output$kostn_tafla_hekt <- renderDataTable(datatable(kostntafla(), class='hover', rownames = FALSE,
                                                       options = list(dom='t'))%>%
                                               formatCurrency(2:3,currency="", mark = ".", digits=0)
  )
  output$losun_tafla_hekt <- renderDataTable(datatable(lostafla(), class='hover', rownames = FALSE,
                                                       options=list(dom='t'))%>%
                                               formatCurrency(2:3,currency="", mark = ".", digits=0)
  )
  
  output$kostn_tafla_heild <- renderDataTable(datatable(kostntafla2(), class='hover', rownames = FALSE,
                                                        options = list(dom='t'))%>%
                                                formatCurrency(2:3,currency="", mark = ".", digits=0)
  )
  output$losun_tafla_heild <- renderDataTable(datatable(lostafla2(), class='hover', rownames = FALSE,
                                                        options=list(dom='t'))%>%
                                                formatCurrency(2:3,currency="", mark = ".", digits=0)
  )
  
  #Ítarleg tafla
  forsendur_grunnur <- reactive({
    temp <- tibble(
      "Forsendur og grunnupplýsingar" = c("Stærð landsvæðis (Hektarar)", "Niturviðmið (kg N/hektara)", "Niturprósenta áburðar (%)",
                                          "Tonn/hektara til þess að uppfylla niturviðmið", "Heildarmagn (tonn)", "Fjöldi dreifinga",
                                          "Magn áburðar í hverri drefingu (tonn)"),
      Lifrænn = c(input$hekt, input$nitur, data_manip()%>%filter(aburdur == input$aburdur)%>%select(N)%>%pull(),
                  data_manip()%>%filter(aburdur == input$aburdur)%>%select(kg_hekt)%>%pull()/1000,data_manip()%>%filter(aburdur == input$aburdur)%>%select(magn_kg)%>%pull()/1000, data_manip()%>%filter(aburdur == input$aburdur)%>%select(fj_ferda_dreifing)%>%pull(),
                  data_manip()%>%filter(aburdur == input$aburdur)%>%select(kg_dreift_ferd)%>%pull()/1000
      ),
      "Tilbúinn Áburður" = c(input$hekt, input$nitur, data_manip()%>%filter(aburdur == "Tilbúinn áburður")%>%select(N)%>%pull(),
                             data_manip()%>%filter(aburdur == "Tilbúinn áburður")%>%select(kg_hekt)/1000,data_manip()%>%filter(aburdur == "Tilbúinn áburður")%>%select(magn_kg)%>%pull()/1000, data_manip()%>%filter(aburdur =="Tilbúinn áburður")%>%select(fj_ferda_dreifing)%>%pull(),
                             data_manip()%>%filter(aburdur == "Tilbúinn áburður")%>%select(kg_dreift_ferd)%>%pull()/1000
      )
    )
    colnames(temp)[which(colnames(temp)=="Lifrænn")]=input$aburdur
    temp
  })
  
  flutningur <- reactive({
    temp <- tibble(
      "Flutningur" = c("Magn í hverri ferð með ökutæki (tonn)", "Fjöldi ferða", "Fjöldi ekinna km"),
      Lifrænn = c(
        data_manip()%>%filter(aburdur == input$aburdur)%>%select(magn_per_flutn)%>%pull(),
        data_manip()%>%filter(aburdur == input$aburdur)%>%select(fj_ferda_flutningur)%>%pull(),
        data_manip()%>%filter(aburdur == input$aburdur)%>%select(fj_km)%>%pull()),
      "Tilbúinn áburður" = c(
        data_manip()%>%filter(aburdur == "Tilbúinn áburður")%>%select(magn_per_flutn)%>%pull(),
        data_manip()%>%filter(aburdur == "Tilbúinn áburður")%>%select(fj_ferda_flutningur)%>%pull(),
        data_manip()%>%filter(aburdur == "Tilbúinn áburður")%>%select(fj_km)%>%pull())
    )
    colnames(temp)[which(colnames(temp)=="Lifrænn")]=input$aburdur
    
    temp
  })
  
  dreifing <- reactive({
    temp <- tibble(
      "Dreifing" = c( "Fjöldi dreifinga", "Magn í dreifingu (tonn)", "Dreifibreidd", "Meðalhraði", "Hektarar á klukkustund", "Magn á tæki (tonn)",
                      "Fjöldi áfyllinga", "Heildartími (klst)", "Kostnaður við ámokstur"),
      Lifrænn = c(
        data_manip()%>%filter(aburdur == input$aburdur)%>%select(fj_ferda_dreifing)%>%pull(),
        data_manip()%>%filter(aburdur == input$aburdur)%>%select(kg_dreift_ferd)%>%pull()/1000,
        data_manip()%>%filter(aburdur == input$aburdur)%>%select(dreifibreidd)%>%pull(),
        data_manip()%>%filter(aburdur == input$aburdur)%>%select(medalhradi_dreif)%>%pull(),
        data_manip()%>%filter(aburdur == input$aburdur)%>%select(hekt_per_klst)%>%pull(),
        data_manip()%>%filter(aburdur == input$aburdur)%>%select(magn_per_dreif)%>%pull(),
        data_manip()%>%filter(aburdur == input$aburdur)%>%select(fj_afyllinga)%>%pull()%>%round(),
        data_manip()%>%filter(aburdur == input$aburdur)%>%select(heildartimi)%>%pull()%>%round(),
        data_manip()%>%filter(aburdur == input$aburdur)%>%select(kostn_amokstur)%>%pull()%>%round()),
      "Tilbúinn áburður" = c(
        data_manip()%>%filter(aburdur == "Tilbúinn áburður")%>%select(fj_ferda_dreifing)%>%pull(),
        data_manip()%>%filter(aburdur == "Tilbúinn áburður")%>%select(kg_dreift_ferd)%>%pull()/1000,
        data_manip()%>%filter(aburdur == "Tilbúinn áburður")%>%select(dreifibreidd)%>%pull(),
        data_manip()%>%filter(aburdur == "Tilbúinn áburður")%>%select(medalhradi_dreif)%>%pull(),
        data_manip()%>%filter(aburdur == "Tilbúinn áburður")%>%select(hekt_per_klst)%>%pull(),
        data_manip()%>%filter(aburdur == "Tilbúinn áburður")%>%select(magn_per_dreif)%>%pull(),
        data_manip()%>%filter(aburdur == "Tilbúinn áburður")%>%select(fj_afyllinga)%>%pull()%>%round(),
        data_manip()%>%filter(aburdur == "Tilbúinn áburður")%>%select(heildartimi)%>%pull()%>%round(),
        data_manip()%>%filter(aburdur == "Tilbúinn áburður")%>%select(kostn_amokstur)%>%pull()%>%round())
    )
    colnames(temp)[which(colnames(temp)=="Lifrænn")]=input$aburdur
    
    temp
  })
  
  output$fors_grunn <- renderDataTable(datatable(forsendur_grunnur(), class='hover', rownames = FALSE,
                                                 options=list(dom='t'))%>%
                                         formatCurrency(2:3,currency="", mark = ".", dec.mark = ",", digits=1)
  )
  
  output$flutn <- renderDataTable(datatable(flutningur(), class='hover', rownames = FALSE,
                                            options=list(dom='t'))%>%
                                    formatCurrency(2:3,currency="", mark = ".", dec.mark = ",", digits = 0)
  )
  
  output$dreif <- renderDataTable(datatable(dreifing(), class='hover', rownames = FALSE,
                                            options=list(dom='t'))%>%
                                    formatCurrency(2:3,currency="", mark = ".", dec.mark = ",", digits=1)
  )
}
