
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
library('raster')

rm(list=ls()) #removing previous objects
options(scipen=999)

#Modifying plots layout
theme_set(theme_bw())

#Loading data-----------

load('output_data/data.Rda')   #produced after 0_prepare_data.R script



#Creating lists for inputs
munlist<-sort(unique(as.character(mun$NM_MUN)))

set$dens=format(round(set$dens, 2), nsmall = 2)


set<-set%>%mutate(`Cenário 1`=c17p,
                  `Cenário 2`=c25p)

cenariolist<-set%>%dplyr::select(`Cenário 1`,`Cenário 2`)
cenariolist$geom<-NULL
# pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(dem2),
#                     na.color = "transparent")




# Map colors and pallettes-----------

# cls = rep(c(brewer.pal(12, "Set3"), brewer.pal(8,"Dark2"), brewer.pal(10, "Paired"),  brewer.pal(8,"Set2"), brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2")))


#basemap ----
basemap = leaflet(option=leafletOptions(zoomControl=T)) %>% 
  addTiles() %>% 
  addLayersControl(
    position = "bottomright",
    overlayGroups = c("Municípios",
                      "Setores Censitários Afetados",
                      "Cenário 1 (1,7 m)",
                      'Cenário 2 (2,5 m)'),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(c("Cenário 2 (2,5 m)"))%>%
  setView(-52, -32, 10)
basemap


#UI-----
ui <- 
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE, id="nav",
             title=div(img(src= 'logo_ufrgs.png', height="40px", width='71px'),"Geoportal da Inundação no RS",img(src= 'ifrs_logo.png', height="40px", width='97px')), 
             
             ##Tab Maps-----------
             tabPanel("Mapas",
                      sidebarLayout(
                        sidebarPanel(id = "mapcontrols",width = 2,
                                     pickerInput("mun_select", "Municípios",
                                                 choices = munlist,
                                                 selected = 'Rio Grande',
                                                 options = list(`actions-box` = TRUE),
                                                 multiple = FALSE),
                                     
                                     
                                     
                                     
                                     # radioButtons("cenario_select",
                                     #              "Selecione um Cenário",
                                     #              choices = list("Cenário 1 (1,7 m)" = 'c17p', "Cenário 2 (2,5 m)" = 'c25p'),
                                     #              selected = "Cenário 1 (1,7 m)"),
                                     
                                     varSelectInput("cenario_select", "Cenário:", cenariolist),
                                     
                                     sliderInput("area_select","Percentual de inundação no setor",
                                                 min=0,
                                                 max=100,
                                                 step=5,
                                                 value=30),
                                     
                                     actionButton("update_view", "Gerar Mapa!"),
                                     tags$br(),tags$br(),
                                     h4('População Afetada:'),
                                     h5(textOutput("reactive_pop"), align = "left"),
                                     tags$br(),tags$br(),
                                     h4('Número de Domicílios:'),
                                     h5(textOutput("reactive_dom"), align = "left"),
                                     
                        ),
                        

                        mainPanel(width=10,
                                  leafletOutput("mymap",height = 800)),
                        

                        
                        
                        
                      )
             ),
             
             ##Tab Plots----------------
             
             # tabPanel("Gráficos",
             #          # Cria a barra lateral (filtros)          
             #          sidebarLayout(
             #            sidebarPanel(width=2,
             #                        
             #                         
             #            ),
             #            #Cria o painel geral e sub-abas
             #            mainPanel(width=10,
             #                      tabsetPanel(
             #                        tabPanel("Encalhes por ano", plotlyOutput("n_ano_plot",height="600px")),
             #                        tabPanel("Encalhes por mês", plotlyOutput("n_mes_plot",height="600px")),
             #                        tabPanel("Espécies", plotlyOutput("n_sp_plot",height="600px")),
             #                        tabPanel("Índice de Encalhes por Esforço Anual (I10)", plotlyOutput("i10ano_plot",height="600px")),
             #                        tabPanel("Índice de Encalhes por Esforço Mensal (I10)", plotlyOutput("i10mes_plot",height="600px")),
             #                        tabPanel("Pesquisadores", plotlyOutput("pesq_plot",height="600px")),
             #                        tabPanel("Downloads", numericInput("maxrows", "Rows to show", 10),verbatimTextOutput("rawtable"),
             #                                 downloadButton("downloadCsv", "Download as CSV"))
             #                      )
             #            )
             #          )
             #          
             # ),
             # 
             
             
             
             ##Tab About----------
             tabPanel("Sobre o site",
                      tags$div(
                        tags$h1(("GeoPortal da Inundação"), tags$image(img(src= 'logo_ufrgs.png', height="159px", width='318px', align = "right"))),
                        tags$br(),
                        tags$br(),
                        tags$br(),
                        tags$br(),
                        tags$h3("Detalhes"),
                        tags$body("O GeoPortal para inundação... "),
                        tags$h3("Autores"),
                        
                        "Tiago Gandra", tags$br(),
                        "Instituto Federal de Educação, Ciência e Tecnologia do Rio Grande do Sul (IFRS)", tags$br(),
                        "<tiago.gandra@riogrande.ifrs.edu.br>", tags$br(),tags$br(),tags$br(),
                        "Tatiana Silva da Silva",tags$br(),
                        "Universidade Federal do Rio Grande do Sul (UFRGS)", tags$br(),
                        "<tatiana.silva@ufrgs@gmail.com>", tags$br(),
                        tags$br(),
                        tags$h3("Colaboradores"),
                        "José", tags$br(),
                        "Natália", tags$br(),
                        "Júlia", tags$br(),
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
  reactive_db_mun = reactive({
    mun %>% filter (NM_MUN %in% input$mun_select)
  })
  
  reactive_db_set = reactive({
    set %>% filter (NM_MUN %in% input$mun_select&
                      !!input$cenario_select>=input$area_select)
  })
  output$reactive_pop <- renderText({
    paste0(format(as.integer(sum(reactive_db_set()$pop*reactive_db_set()$c17p/100,na.rm=T)), big.mark=".", decimal.mark=",",1), " pessoas")
  })
  
  output$reactive_dom <- renderText({
    paste0(format(as.integer((sum(reactive_db_set()$dom*reactive_db_set()$c17p/100,na.rm=T))), big.mark=".", decimal.mark=",",1), " domicílios")
  })
  
  #Outputs------------
  output$mymap <- renderLeaflet({ 
    basemap
  })
  

  #Map-------------
  observeEvent(input$update_view,{
    leafletProxy("mymap") %>% 
      clearMarkers() %>%
      clearGroup(c("Municípios",'Setores Censitários Afetados','Cenário 2 (2,5 m)','Cenário 1 (1,7 m)'))%>%
      
      addPolygons(data = reactive_db_mun(),
                  color=c("darkgray"),
                  fillOpacity=0,
                  label=reactive_db_mun()$NM_MUN,
                  group = 'Municípios')%>%
      addPolygons(data = c17_pol,
                  color=c("darkblue"),
                  fillOpacity=0.4,
                  stroke= F,
                  group = 'Cenário 1 (1,7 m)')%>%
      addPolygons(data = c25_pol,
                  color=c("darkblue"),
                  fillOpacity=0.4,
                  stroke= F,
                  group = 'Cenário 2 (2,5 m)')%>%
      addPolygons(data = reactive_db_set(),
                  color=~colorNumeric("Reds", domain=reactive_db_set()$pop)(reactive_db_set()$pop),
                  fillOpacity = 0.8,
                  stroke=F,
                  label = sprintf("Município: %s <br/>População: %s<br>Nº de domicílios:%s <br>Densidade Demográfica:%s",
                                  reactive_db_set()$NM_MUN, reactive_db_set()$pop,reactive_db_set()$dom,format(reactive_db_set()$dens,nsmall = 2)) %>%
                    lapply(htmltools::HTML),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px", direction = "auto"),
                  group = 'Setores Censitários Afetados')
    
    
    
    
  })
}

#Run---------
shinyApp(ui, server)
