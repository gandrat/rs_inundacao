
## Beach Monitoring interactive mapping tool
# load required packages
library('ggplot2')
library('dplyr')
library('RColorBrewer')
library('leaflet')
library('plotly')
library('maps')
library('shiny')
library('shinyWidgets')
library('shinydashboard')
library('shinythemes')
library('leaflet.extras')
library('sf')

rm(list=ls()) #removing previous objects
options(scipen=999)

#Modifying plots layout
theme_set(theme_bw())

#Loading data-----------

load('output_data/input_data.Rda')   #produced after 0_prepare_data.R script
max(enc$data)
max(i10saida$data)
#Creating lists for inputs
splist<-sort(unique(as.character(enc$especie)))
anolist <- as.character(unique(enc$ano))
turtlelist<-c('Caretta caretta','Chelonia mydas','Dermochelys coriacea','Eretmochelys imbricata','Lepidochelys olivacea')

# Map colors and pallettes-----------

cls = rep(c(brewer.pal(12, "Set3"), brewer.pal(8,"Dark2"), brewer.pal(10, "Paired"),  brewer.pal(8,"Set2"), brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2")))

pal_sp<-colorFactor(cls,
                    domain = splist)
global_colors <- setNames(pal_sp(splist), splist) # this maps brand to color

#basemap ----
basemap = leaflet(option=leafletOptions(zoomControl=T)) %>% 
  addTiles() %>% 
  addLayersControl(
    position = "bottomright",
    overlayGroups = c("Encalhes",
                      "Mapa de Densidade",
                      "Área de Estudo (Total)",
                      "Área de Estudo (Porto)"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(c("Mapa de Densidade","Área de Estudo (Porto)" ))%>%
  # addProviderTiles(providers$Esri.OceanBasemap) %>%
  setView(-52, -32.5, 8)
# basemap

#Plot functions-----------
n_ano_plot = function(i10saida,saidasano) {
  g1 = ggplot(i10saida, aes(x = ano, y=n_enc, fill=especie))+geom_bar(stat='sum')+
    ylab("Nº de Encalhes")+xlab('Anos')+
    scale_fill_manual(values=global_colors) +
    theme(legend.title = element_blank(), legend.position = "none", plot.title = element_text(size=10))+
    scale_x_continuous(labels=as.character(saidas$ano),breaks=saidas$ano)
  
  g2 = ggplot(saidasano, aes(x = ano, y=km_total))+geom_col(fill='#006666')+
    ylab("Esforço (km)")+xlab('Anos')+
    theme(legend.title = element_blank(), legend.position = "none", plot.title = element_text(size=10))+
    scale_x_continuous(labels=as.character(saidas$ano),breaks=saidas$ano)
  subplot(g1, g2, titleY=T,shareX=T, titleX=T, nrows=2, margin = .03, which_layout=1)
}

n_mes_plot = function(i10saida,saidasmes) {
  g1 = ggplot(i10saida, aes(x = mes, y=n_enc, fill=especie))+geom_bar(stat='sum')+
    ylab("Nº de Encalhes")+xlab('Mês')+
    scale_fill_manual(values=global_colors) +
    theme(legend.title = element_blank(), legend.position = "none", plot.title = element_text(size=10))+
    scale_x_continuous(labels=as.character(i10saida$mes),breaks=i10saida$mes)
  
  g2 = ggplot(saidasmes, aes(x = mes, y=km_total))+geom_col(fill='#006666')+
    ylab("Esforço (km)")+xlab('Mês')+
    theme(legend.title = element_blank(), legend.position = "none", plot.title = element_text(size=10))+
    scale_x_continuous(labels=as.character(saidasmes$mes),breaks=saidasmes$mes)
  subplot(g1, g2, titleY=T,shareX=T, titleX=T, nrows=2, margin = .03, which_layout=1)
}

pesq_plot = function(pesq) {
  g1 = ggplot(pesq,aes(x=km,y=nome,fill=ano))+geom_bar(stat='identity')+
    scale_fill_distiller(palette='Greens')+
    ylab(NULL)+xlab('Esforço (km)')+
    theme(legend.title = element_blank(), legend.position = "none", plot.title = element_text(size=10))
  g1
}

n_sp_plot = function(i10saida) {
  g1 = ggplot(i10saida,aes(y=especie, x=n_enc, fill=especie))+geom_bar(stat='sum')+
    ylab(NULL)+xlab('Nº de Encalhes')+
    scale_fill_manual(values=global_colors)+
    theme(legend.title = element_blank(), legend.position = "none", plot.title = element_text(size=10))
  g1
}

i10ano_plot = function(i10saida) {
  g1 = ggplot(i10saida, aes(x =as.factor(ano), y=i10, fill=especie))+
    geom_bar(stat='sum')+
    ylab("Encalhes/10 km (i10)")+xlab("Anos")+
    scale_fill_manual(values=global_colors) +
    theme(legend.title = element_blank(), legend.position = "none")
}

i10mes_plot = function(i10saida) {
  g1 = ggplot(i10saida, aes(x =as.factor(mes), y=i10, fill=especie))+
    geom_bar(stat='sum')+
    ylab("Encalhes/10 km (i10)")+xlab("Mês")+
    scale_fill_manual(values=global_colors) +
    theme(legend.title = element_blank(), legend.position = "none")
}
  
  #UI-----
  ui <- 
    navbarPage(theme = shinytheme("flatly"), collapsible = TRUE, id="nav",
               title=div(img(src= 'logo_ufrgs.png', height="34px", width='60px'),"Geoportal da Inundação no RS"), 
               
               ##Tab Maps-----------
               tabPanel("Mapas",
                        sidebarLayout(
                          sidebarPanel(id = "mapcontrols",width = 2,
                                       pickerInput("sp_select", "Municípios",
                                                   choices = splist,
                                                   selected = splist,
                                                   options = list(`actions-box` = TRUE),
                                                   multiple = TRUE),
                                       dateInput("dataini_select1",
                                                 "Data Inicial:",
                                                 min = min(enc$data),
                                                 max = max(enc$data),
                                                 value=min(enc$data)),
                                       dateInput("datafim_select1",
                                                 "Data Final:",
                                                 min = min(enc$data),
                                                 max = max(enc$data),
                                                 value=max(enc$data)),
                                       actionButton("update_view", "Gerar Mapa!")),
                          mainPanel(width=10,
                                    leafletOutput("mymap",height = 800))
                          
                        )
               ),
               
               ##Tab Plots----------------
               
               tabPanel("Gráficos",
                        # Cria a barra lateral (filtros)          
                        sidebarLayout(
                          sidebarPanel(width=2,
                                       pickerInput("grupo_select", "Grupos:",
                                                   choices = unique(enc$grupo),
                                                   selected = unique(enc$grupo),
                                                   options = list(`actions-box` = TRUE),
                                                   multiple = TRUE),
                                       pickerInput("sp_select2", "Espécies:",
                                                   choices = splist,
                                                   selected = splist,
                                                   options = list(`actions-box` = TRUE),
                                                   multiple = TRUE),
                                       pickerInput("direcao_select", "Direção da Saída",
                                                   choices = c('N','S'),
                                                   selected = c('N','S'),
                                                   options = list(`actions-box` = TRUE),
                                                   multiple = TRUE),
                                       pickerInput("estacao_select", "Estacao do Ano",
                                                   choices = unique(as.character(enc$estacao)),
                                                   selected = unique(as.character(enc$estacao)),
                                                   options = list(`actions-box` = TRUE),
                                                   multiple = TRUE),
                                       dateInput("dataini_select",
                                                 "Data Inicial:",
                                                 min = min(enc$data),
                                                 max = max(enc$data),
                                                 value=min(enc$data)),
                                       dateInput("datafim_select",
                                                 "Data Final:",
                                                 min = min(enc$data),
                                                 max = max(enc$data),
                                                 value=max(enc$data))
                                       
                          ),
                          #Cria o painel geral e sub-abas
                          mainPanel(width=10,
                                    tabsetPanel(
                                      tabPanel("Encalhes por ano", plotlyOutput("n_ano_plot",height="600px")),
                                      tabPanel("Encalhes por mês", plotlyOutput("n_mes_plot",height="600px")),
                                      tabPanel("Espécies", plotlyOutput("n_sp_plot",height="600px")),
                                      tabPanel("Índice de Encalhes por Esforço Anual (I10)", plotlyOutput("i10ano_plot",height="600px")),
                                      tabPanel("Índice de Encalhes por Esforço Mensal (I10)", plotlyOutput("i10mes_plot",height="600px")),
                                      tabPanel("Pesquisadores", plotlyOutput("pesq_plot",height="600px")),
                                      tabPanel("Downloads", numericInput("maxrows", "Rows to show", 10),verbatimTextOutput("rawtable"),
                                               downloadButton("downloadCsv", "Download as CSV"))
                                    )
                          )
                        )
                        
               ),
               
               
               
               
               ##Tab About----------
               tabPanel("Sobre o site",
                        tags$div(
                          tags$h1(("PIE-NEMA"), tags$image(img(src= 'NEMA_Logo.png', height="159px", width='318px', align = "right"))),
                          tags$br(),
                          tags$br(),
                          tags$br(),
                          tags$br(),
                          tags$h3("Detalhes"),
                          tags$body("A Plataforma de Informações Espaciais do Núcleo de Educação e Monitoramento Ambiental (PIE-NEMA), é o resultado de um projeto de conclusão do curso técnico em geoprocessamento, no Instituto Federal do Rio Grande do Sul - IFRS campus Rio Grande. Em parceria com o NEMA, seu desenvolvimento tem como principal objetivo auxiliar a consulta dos dados coletados em monitoramentos de praia. A partir de uma interface didática e funcional, a PIE-NEMA disponibiliza dados armazenados no Banco de Dados do NEMA (BD-NEMA) através de gráficos e mapas. É possível também, realizar o download de tabelas .CSV, ou seja, com valores separados por vírgula e com as informações escolhidas a partir da seleção dos filtros desejados.", tags$br(),
                          "O Núcleo de Educação e Monitoramento Ambiental atua no litoral médio e sul do Rio Grande do Sul, com o objetivo de acompanhar e caracterizar os registros da megafauna marinha. A equipe do NEMA possui uma metodologia de coleta de dados que envolve as atividades de monitoramento de praia, realizadas mensalmente na região. Essa atividade engloba duas direções: a direção Norte se inicia no Molhe Leste, no município de São José do Norte, e se finaliza na Barra da Lagoa do Peixe, no município de Tavares, percorrendo 135 km; a direção Sul se inicia no Molhe Oeste da Barra do Rio Grande, no município de Rio Grande, e se finaliza na Barra do Chuí, no município de Santa Vitória do Palmar, percorrendo 225 km. Os dados disponibilizados nesta plataforma estão totalmente atribuídos à coleta dos monitoramentos.", tags$br(),
                          "É de extrema relevância ressaltar que todas as informações disponíveis na plataforma são de total domínio do NEMA. Os dados utilizados para construção da PIE-NEMA são provenientes de, aproximadamente, 27 anos de coleta e análise sobre a megafauna marinha da região. Portanto, a plataforma é um produto restrito à equipe do NEMA e a segurança dos dados é garantida a partir desta limitação de acesso. "),
                          tags$h3("Autores"),
                          "Catarina De Zotti Pinho",tags$br(),
                          "Técnica em Geoprocessamento", tags$br(),
                          "Núcleo de Educação e Monitoramento Ambiental (NEMA)", tags$br(),
                          "<catarinadezotti03@gmail.com>", tags$br(),
                          tags$br(),
                          "Dr. Tiago Gandra", tags$br(),
                          "Professor do Instituto Federal de Educação, Ciência e Tecnologia do Rio Grande do Sul", tags$br(),
                          "Laboratório de Geotecnologias e Meio Ambiente (GEOMA)", tags$br(),
                          "<tiago.gandra@riogrande.ifrs.edu.br>", tags$br(),
                          tags$h3("Colaboradores"),
                          tags$body("Equipe do NEMA"),
                          tags$h3("Contatos NEMA"),
                          "Endereço: Rua Maria Araújo, 450 | Rio Grande - RS / Brasil",tags$br(),
                          "Telefone: (53)3236-2420", tags$br(),
                          "E-mail: nema@nema-rs.org.br", tags$br(),
                          "Site: https://www.nema-rs.org.br/", tags$br(),
                          tags$br(),
                          tags$br(),
                          tags$h5("Última Atualização:"),
                          h6(format(file.info('app.R')$atime, format= '%Y-%m-%d'))
                        )
               )
    )
  
  
  #Server-----
  server = function(input, output) {
    
    # Reactive DBs----------
    reactive_db_enc = reactive({
      enc %>% filter (especie %in% input$sp_select & 
                        data >= input$dataini_select1 & 
                        data <= input$datafim_select1 &
                        coord_correta==T)
    })
    
    reactive_db_enc2 = reactive({
      enc %>% filter (grupo %in% input$grupo_select &
                        especie %in% input$sp_select2 &
                        data >= input$dataini_select & 
                        data <= input$datafim_select &
                        direcao %in% input$direcao_select &
                        estacao %in% input$estacao_select &
                        coord_correta==T)
    })
    
    reactive_db_enc_ano = reactive({
      i10saida %>% filter (grupo %in% input$grupo_select &
                             especie %in% input$sp_select2 &
                             data >= input$dataini_select & 
                             data <= input$datafim_select &
                             direcao %in% input$direcao_select &
                             estacao %in% input$estacao_select )%>%
        group_by(especie, ano, direcao)%>%
        summarise(n_enc=sum(n_enc, na.rm=T))
    })
    
    reactive_db_saidasano = reactive({
      saidas %>% filter (data >= input$dataini_select & 
                           data <= input$datafim_select &
                           direcao %in% input$direcao_select &
                           estacao %in% input$estacao_select) %>%
        group_by(ano)%>%summarise(n=n(),km_total=sum(km_total,na.rm=T))
    })
    reactive_db_enc_mes = reactive({
      i10saida %>% filter (grupo %in% input$grupo_select &
                             especie %in% input$sp_select2 &
                             data >= input$dataini_select & 
                             data <= input$datafim_select &
                             direcao %in% input$direcao_select &
                             estacao %in% input$estacao_select)%>%
        group_by(especie, mes, direcao)%>%
        summarise(n_enc=sum(n_enc, na.rm=T))
    })
    reactive_db_saidasmes = reactive({
      saidas %>% filter (data >= input$dataini_select & 
                           data <= input$datafim_select &
                           direcao %in% input$direcao_select &
                           estacao %in% input$estacao_select) %>%
        group_by(mes)%>%summarise(n=n(),km_total=sum(km_total,na.rm=T))
    })
    
    reactive_db_i10ano = reactive({
      i10saida %>% filter (grupo %in% input$grupo_select &
                           especie %in% input$sp_select2 & 
                             data >= input$dataini_select & 
                             data <= input$datafim_select &
                             direcao %in% input$direcao_select &
                             estacao %in% input$estacao_select) %>%
        group_by(ano, especie)%>%
        summarise(i10=sum(n_enc,na.rm=T)*10/sum(km_total,na.rm=T),
                  n_enc=sum(n_enc,na.rm=T))
    })
    
    reactive_db_i10mes = reactive({
      i10saida %>% filter (grupo %in% input$grupo_select &
                             especie %in% input$sp_select2 & 
                             data >= input$dataini_select & 
                             data <= input$datafim_select &
                             direcao %in% input$direcao_select &
                             estacao %in% input$estacao_select) %>%
        group_by(mes, especie)%>%
        summarise(i10=sum(n_enc,na.rm=T)*10/sum(km_total,na.rm=T),
                  n_enc=sum(n_enc,na.rm=T))
    })
    
    reactive_db_sp = reactive({
      i10saida %>% filter (grupo %in% input$grupo_select &
                             especie %in% input$sp_select2 & 
                             data >= input$dataini_select & 
                             data <= input$datafim_select &
                             direcao %in% input$direcao_select &
                             # sexo %in% input$sexo_select &
                             estacao %in% input$estacao_select) %>%
        group_by(especie)%>%
        summarise(n_enc=sum(n_enc,na.rm=T))
    })
    #Outputs------------
    output$mymap <- renderLeaflet({ 
      basemap
    })
    
    output$n_ano_plot <- renderPlotly({
      n_ano_plot(reactive_db_enc_ano(),reactive_db_saidasano())
    })  
    
    output$n_mes_plot <- renderPlotly({
      n_mes_plot(reactive_db_enc_mes(),reactive_db_saidasmes())
    })  
    
    output$n_sp_plot <- renderPlotly({
      n_sp_plot(reactive_db_sp())
    })  
    
    output$i10ano_plot <- renderPlotly({
      i10ano_plot(reactive_db_i10ano())
    })  
    
    output$i10mes_plot <- renderPlotly({
      i10mes_plot(reactive_db_i10mes())
    })  
    
    output$pesq_plot <- renderPlotly({
      pesq_plot(pesq)
    })  
    
    output$rawtable <- renderPrint({
      orig <- options(width = 1000)
      print(head(reactive_db_enc2(), input$maxrows), row.names = FALSE)
      options(orig)
    })
    
    # output to download data------
    output$downloadCsv <- downloadHandler(
      filename = function() {
        paste("encalhes_", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(reactive_db_enc2(), file)
      }
    )
    
    #Map-------------
    observeEvent(input$update_view,{
      leafletProxy("mymap") %>% 
        # clearMarkers() %>%
        clearGroup("Mapa de Densidade")%>%
        
        addPolygons(data = area_estudo%>%filter(id %in% c(1,4)), 
                    color=c("#A50026"),
                    group = 'Área de Estudo (Total)')%>%
        addPolygons(data = area_estudo%>%filter((id %in% c(2,3))), 
                    color=c("#A50026"),
                    group = 'Área de Estudo (Porto)')%>%
        
        addCircleMarkers(data = reactive_db_enc(), ~lon, ~lat, radius=4,
                         fillOpacity = 0.3, group = "Encalhes",clusterOptions = markerClusterOptions(),
                         label = sprintf("<strong>%s </strong><br/>Date: %s<br",
                                         reactive_db_enc()$especie, format(reactive_db_enc()$data, "%Y-%m-%d")) %>% 
                           lapply(htmltools::HTML),
                         labelOptions = labelOptions(
                           style = list("font-weight" = "normal", padding = "3px 8px"),
                           textsize = "15px", direction = "auto"))%>%  
        
        addHeatmap(data = reactive_db_enc(), ~lon, ~lat,
                   radius=10, layerId = 1,
                   group = "Mapa de Densidade")
      
      
    })
  }
  
  #Run---------
  shinyApp(ui, server)
  