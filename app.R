library(shinydashboard)
library(plotly)
library(dplyr)
library(DT)
library(data.table)
library(htmltools)
library(shiny)

ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
                          tags$head(tags$link(href="https://fonts.googleapis.com/css?family=Source Code Pro", rel="stylesheet"))
  ),
  h1(div(span(strong("Detalles que salvan",style="text-align: center; color: #003A5B; font-size:20pt")))),
  mainPanel(width=12,
            tabsetPanel(type="tabs",
                        tabPanel("Movilidad",
                                 selectInput("localidad",h3("Localidad"),choices=as.character(localidad),selected=localidad[1]),
                                 column(6,plotlyOutput("Salirayer"),
                                        selectInput("motivosInd",h3("Motivos para salir ayer"),choices=as.character(motivos),selected=motivos[1]),
                                        plotlyOutput("motivosalirayer")),
                                 column(6,
                                        selectInput("motivo15dias",h3("¿usted o alguno de los miembros de su familia acudió a cualquiera de los siguientes lugares...?"),
                                                    choices=c("Supermercado","Restaurante","Ventas informales en calle","Plazas","Transmilenio","SITP","Iglesia y/o lugares de culto",
                                                              "Centros comerciales","Colegios y/o jardines",	"Centros médicos","Gimnasio",	"Oficinas","Ciclovía","Parque","Terminales aéreas y/o terrestres"),
                                                    selected="Supermercado"),
                                        plotlyOutput("motivosalir15dias")
                                 )
                        ),
                        tabPanel("Mental",
                                 fluidRow(column(6,
                                                 h3("¿Qué tan probable cree usted que se contagie de Coronavirus? "),
                                                 plotlyOutput("propCovidContagio")),
                                          column(6,
                                                 h3("¿Usted cree que podría estar contagiado de coronavirus y no tener síntomas? "),
                                                 plotlyOutput("CreeCovidProp"))
                                 ),
                                 fluidRow(column(6,DT::dataTableOutput("ConfianEnLaGente")),
                                          column(6,plotlyOutput("SaleConCOVID"))
                                 )
                        )
            )
  )
  )
)


server <- function(input, output){
  
  #load("~/Pruebas Dashboard/data/DATOS.RDATA")
  
  output$Salirayer<-renderPlotly({
    aux1<-input$localidad
    fig<-salioayer[LOCALIDAD==aux1,] %>% 
      plot_ly(labels=~P3,values=~Salio/Universo) %>% 
      add_pie(hole=0.6) %>% 
      layout(title = "Salió ayer",  showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    fig
  })
  
  output$motivosalirayer<-renderPlotly({
    aux1<-input$localidad
    aux2<-input$motivosInd
    graf<-data.table(expand.grid(aux1[1],estrato))
    names(graf)<-c("LOCALIDAD","ESTRATO")
    graf<-merge(graf,aux[LOCALIDAD==aux1 & P4==aux2,],by=c("LOCALIDAD","ESTRATO"),all.x = TRUE)
    graf$Cant[is.na(graf$Cant)]=0
    graf %>% 
      plot_ly(x=~ESTRATO,y=~Cant,type = "bar")
  })
  
  output$motivosalir15dias<-renderPlotly({
    aux1<-input$localidad
    # aux1<-localidad[1]
    aux2<-input$motivo15dias
    # aux2<-"Colegios y/o jardines"
    aux3<-names(datos4)[names(datos4)%like%aux2]
    graf<-data.frame(t(datos4[LOCALIDAD==aux1,..aux3]))
    graf$Base<-c("Salen","Ninguno","No sabe/No responde")
    names(graf)[1]<-"Y"
    graf %>% 
      plot_ly(x=~Base,y=~Y,type="bar") %>% 
      layout(title=aux2)
  })
  output$propCovidContagio<-renderPlotly({
    covidProp %>% 
      plot_ly(labels=~P11,values=~V1) %>% 
      add_pie(hole=0.6) %>% 
      layout(showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  output$CreeCovidProp<-renderPlotly({
    
    CreeCovid %>% 
      plot_ly(y=~Base,x=~Sí,type="bar",orientation="h",name="Sí") %>% 
      add_trace(x=~No,name="No") %>% 
      add_trace(x=~NSNR,name="No sabe no responde") %>% 
      layout(showlegend = T, barmode="stack",
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE,title=""),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE,title="Cree tener COVID19"))
  })
  
  output$SaleConCOVID<-renderPlotly({
    SaleyCovid %>% 
      plot_ly(labels=~P3,values=~Cant) %>% 
      add_pie() %>% 
      layout(showlegend = T, title="Sale creyendo tener COVID",
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$ConfianEnLaGente<-DT::renderDataTable({
    DT::datatable(ConfiarGente,
                  rownames = F,filter = "none", options = list(pageLength = 5, dom = 'tip'))
  })
}

shinyApp(ui,server)
