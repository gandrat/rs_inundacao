
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

# load('output_data/nivel.Rda')

#Creating lists for inputs
munlist<-sort(unique(as.character(mun$NM_MUN)))

set$dens=format(round(set$dens, 2), nsmall = 2)


set<-set%>%mutate(`Cenário 1`=c17p,
                  `Cenário 2`=c25p,
                  `Cenário 3`=c50p)

setdf<-set
setdf$geom<-NULL

cenariolist<-set%>%dplyr::select(`Cenário 1`,`Cenário 2`,`Cenário 3`)
cenariolist$geom<-NULL


# Palettes---------
palette_rev <- rev(brewer.pal(5, "Spectral"))

cls = rep(c(brewer.pal(12, "Set3"), brewer.pal(8,"Dark2"), brewer.pal(10, "Paired"),  brewer.pal(8,"Set2"), brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2")))

pal_sp<-colorFactor(cls,
                    domain = munlist)
global_colors <- setNames(pal_sp(munlist), munlist) 
#basemap ----
basemap = leaflet(option=leafletOptions(zoomControl=T)) %>% 
  addTiles() %>% 
  addLayersControl(
    position = "bottomright",
    overlayGroups = c("Municípios",
                      "Setores Afetados (% de área)",
                      "População",
                      # "Densidade Demográfica",
                      "Número de Domicílios",
                      "Cenário 1 (1,7 m)",
                      'Cenário 2 (2,5 m)',
                      'Cenário 3 (5,0 m)'),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(c("População",
              "Densidade Demográfica",
              "Número de Domicílios",
              "Cenário 1 (1,7 m)",
              "Cenário 2 (2,5 m)",
              'Cenário 3 (5,0 m)'))%>%
  setView(-52, -31.5, 7)
# basemap


nivel_plot = function(nivel) {
  g1 = ggplot(nivel, aes(x = time, y = nivel)) +
    geom_path(alpha=.5,fill='darkblue')+
    ylab("Nível (m)")+ theme_bw() +
    xlab(NULL)
  ggplotly(g1, tooltip = c("text"))
}


pop_plot = function(setdf) {
  g1 = ggplot(setdf, aes(x = NM_MUN, y = pop,fill=NM_MUN)) +
    geom_bar(position="stack", stat="identity") +
    scale_fill_manual(values=global_colors) +
    ylab("População Afetada")+xlab(NULL) + theme_bw() +
    coord_flip()+
    
    theme(legend.title = element_blank(), legend.position = "none", plot.title = element_text(size=12,hjust=0.5))
  ggplotly(g1, tooltip = c("text"))
}
#UI-----
ui <- 
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE, id="nav",
             title=div(img(src= 'logo_ufrgs.png', height="40px"),"       Geoportal da Inundação no RS       ",img(src= 'ifrs_logo.png', height="40px")), 
             
             ##Tab Maps-----------
             tabPanel("Mapas",
                      sidebarLayout(
                        sidebarPanel(id = "mapcontrols",width = 3,
                                     pickerInput("mun_select", "Municípios",
                                                 choices = munlist,
                                                 selected = 'Rio Grande',
                                                 options = list(`actions-box` = TRUE),
                                                 multiple = FALSE),
                              
                                     
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
                        

                        mainPanel(width=9,
                                  leafletOutput("mymap",height = 800))
                      )
             ),
             
             ##Tab Pop Plots-----------
             tabPanel("População por Município",
                      sidebarLayout(
                        sidebarPanel(id = "mapcontrols",width = 3,
                                     pickerInput("mun_select2", "Municípios",
                                                 choices = munlist,
                                                 selected = munlist,
                                                 options = list(`actions-box` = TRUE),
                                                 multiple = TRUE),
                                     
                                     
                                     varSelectInput("cenario_select2", "Cenário:", cenariolist),
                                     
                                     sliderInput("area_select2","Percentual de inundação no setor",
                                                 min=0,
                                                 max=100,
                                                 step=5,
                                                 value=30),
                                     
                                     
                                     tags$br(),tags$br(),
                                     h4('População Afetada:'),
                                     h5(textOutput("reactive_pop2"), align = "left"),
                                     tags$br(),tags$br(),
                                     h4('Número de Domicílios:'),
                                     h5(textOutput("reactive_dom2"), align = "left"),
                                     
                        ),
                        
                        
                        mainPanel(width=9,
                                  plotlyOutput("pop_ploto",height="600px"))
                      )
             ),
             
             ##Tab Plots----------------
             
             # tabPanel("Nível (TideSat)",
             #          # Cria a barra lateral (filtros)
             #          sidebarLayout(
             #            sidebarPanel(width=2,
             # 
             # 
             #            ),
             #            #Cria o painel geral e sub-abas
             #            mainPanel(width=10,
             #                      tabsetPanel(
             #                        tabPanel("Rio Grande (RIG)", plotlyOutput("nivel_plot_rg",height="600px"))
             #                        
             #                      )
             #            )
             #          )
             # 
             # ),

             
             
             
             ##Tab About----------
             tabPanel("Sobre o site",
                      tags$div(
                        tags$h1(("GeoPortal da Inundação")), 
                        tags$br(),
                        tags$image(img(src= 'logo_ufrgs.png',  height='200px', align = "center")),
                        tags$image(img(src= 'ifrs_logo.png',  height='200px',  align = "center")),
                        tags$br(),
                        tags$br(),
                        tags$br(),
                        tags$br(),
                        tags$br(),
                        tags$br(),
                        tags$h3("Detalhes"),
                        tags$body("O GeoPortal para inundação no Rio Grande do Sul usa um modelo de elevação e dados do Censo Demográfico (IBGE) para estimar o número de pessoas e domicílios afetados pelas inundações no estado."),
                        tags$br(),
                        tags$body("Foram gerados 3 cenários de elevação da Lagoa dos Patos: (1) 1,7 m, (2) 2,5 m e (3) 5,0 m."),
                        tags$br(),
                        tags$body("As estimativas de população e domicílios depende da escolha do cenário e de um limiar do percentual de área de inundação do setor censitário. Quanto maior este limiar de percentual, menor será o número de setores censitários computados."),
                        
                        tags$br(),
                        tags$h3("Fontes de Dados:"),
                        tags$ol(
                          tags$li(tags$a(href="https://www.ibge.gov.br/estatisticas/sociais/populacao/22827-censo-demografico-2022.html?edicao=39499&t=resultados", "IBGE - Censo de 2022: Agregados por setores censitários preliminares")), 
                          tags$li(tags$a(href="https://spacedata.copernicus.eu/collections/copernicus-digital-elevation-model", "COPERNICUS - Modelo de Elevação Global DEM30")), 
                        ),
                        tags$br(),
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
  
  reactive_db_set2 = reactive({
    setdf %>% filter (NM_MUN %in% input$mun_select2&
                      !!input$cenario_select2>=input$area_select2)%>%
      group_by(NM_MUN)%>%
      summarise(pop=sum(pop,na.rm=T),
                dom=sum(dom,na.rm=T))
  })
  
  reactive_db_set = reactive({
    set %>% filter (NM_MUN %in% input$mun_select&
                      !!input$cenario_select>=input$area_select)
  })
  
  # reactive_db_setall = reactive({
  #   set %>% filter (NM_MUN %in% input$mun_select)
  # })
  output$reactive_pop <- renderText({
    paste0(format(as.integer(sum(reactive_db_set()$pop,na.rm=T)), big.mark=".", decimal.mark=",",1), " pessoas")
  })
  
  output$reactive_dom <- renderText({
    paste0(format(as.integer((sum(reactive_db_set()$dom,na.rm=T))), big.mark=".", decimal.mark=",",1), " domicílios")
  })
  
  output$reactive_pop2 <- renderText({
    paste0(format(as.integer(sum(reactive_db_set2()$pop,na.rm=T)), big.mark=".", decimal.mark=",",1), " pessoas")
  })
  
  output$reactive_dom2 <- renderText({
    paste0(format(as.integer((sum(reactive_db_set2()$dom,na.rm=T))), big.mark=".", decimal.mark=",",1), " domicílios")
  })
  
  #Outputs------------
  output$mymap <- renderLeaflet({ 
    basemap
  })
  
  output$nivel_plot_rg <- renderPlotly({
    nivel_plot(nivel)
  })
  
  output$pop_ploto <- renderPlotly({
    pop_plot(reactive_db_set2())
  })
  

  #Map-------------
  observeEvent(input$update_view,{
    leafletProxy("mymap") %>% 
      clearMarkers() %>%
      clearGroup(c("Municípios",
                   "Setores Afetados (% de área)",
                   "População",
                   "Densidade Demográfica",
                   "Número de Domicílios",
                   "Cenário 1 (1,7 m)",
                   'Cenário 2 (2,5 m)'))%>%
      
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
      addPolygons(data = c50_pol,
                  color=c("darkblue"),
                  fillOpacity=0.4,
                  stroke= F,
                  group = 'Cenário 3 (5,0 m)')%>%
      addPolygons(data = reactive_db_set(),
                  color=~colorNumeric(palette_rev, domain=reactive_db_set()$c17p)(reactive_db_set()$c17p),
                  fillOpacity = 0.8,
                  stroke=F,
                  label = sprintf("Município: %s <br/>População: %s<br>Nº de domicílios: %s <br>Densidade Demográfica: %s<br> Percentual de Área Afetada: %s",
                                  reactive_db_set()$NM_MUN,
                                  reactive_db_set()$pop,
                                  reactive_db_set()$dom,
                                  format(reactive_db_set()$dens,nsmall = 2),
                                  as.integer(reactive_db_set()$c17p)) %>%
                    lapply(htmltools::HTML),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px", direction = "auto"),
                  group = "Setores Afetados (% de área)")%>%
      addPolygons(data = reactive_db_set(),
                  color=~colorNumeric("Oranges", domain=reactive_db_set()$dom)(reactive_db_set()$dom),
                  fillOpacity = 0.5,
                  stroke=F,
                  label = sprintf("Município: %s <br/>População: %s<br>Nº de domicílios: %s <br>Densidade Demográfica: %s<br> Percentual de Área Afetada: %s",
                                  reactive_db_set()$NM_MUN,
                                  reactive_db_set()$pop,
                                  reactive_db_set()$dom,
                                  format(reactive_db_set()$dens,nsmall = 2),
                                  as.integer(reactive_db_set()$c17p)) %>%
                    lapply(htmltools::HTML),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px", direction = "auto"),
                  group = "Número de Domicílios")%>%
      addPolygons(data = reactive_db_set(),
                  color=~colorNumeric("Oranges", domain=reactive_db_set()$pop)(reactive_db_set()$pop),
                  fillOpacity = 0.5,
                  stroke=F,
                  label = sprintf("Município: %s <br/>População: %s<br>Nº de domicílios: %s <br>Densidade Demográfica: %s<br> Percentual de Área Afetada: %s",
                                  reactive_db_set()$NM_MUN,
                                  reactive_db_set()$pop,
                                  reactive_db_set()$dom,
                                  format(reactive_db_set()$dens,nsmall = 2),
                                  as.integer(reactive_db_set()$c17p)) %>%
                    lapply(htmltools::HTML),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px", direction = "auto"),
                  group = "População")
    
    
    
    
  })
}

#Run---------
shinyApp(ui, server)
