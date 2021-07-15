#####################################################################################
#  Proyecto D365 FO                                                                 #
#  Extracción de datos Stock Insuficiente                                           #
#  Autor: Junior T. Ortiz Mejia                                                     #
#  Fecha: 08/03/2021                                                                #                                                                              
#####################################################################################

options(encoding = "utf-8")
options(shiny.maxRequestSize = 30*1024^2)
options(warn=-1)

ent <- 1
if(ent == 1){
	pathglo <- "D:/github/appi011/appR"
}else {
	pathglo <- "/srv/shiny-server/appi011/appR"
}


library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(shinyjs)

library(httr)    
library(rjson)  
library(jsonlite)
library(dplyr)
library(openxlsx)
library(data.table)
library(bit64)
library(stringr)


shinyApp(
	ui = fluidPage( theme = shinytheme("lumen"), useShinyjs(),useShinydashboard(),
			list(
				tags$head(HTML('<link rel="icon", href="http://181.65.149.162:4001/app014/img/cropped-fvtrujll02-32x32.png", type="image/png">')),
				tags$style(HTML("
						.navbar {left: -20px; }
						.navbar-default .navbar-brand { color: #FFF;
														front-size: 16px;
														background-color: #E1120B ;}
					"))
			),

			shinythemes::themeSelector(), #seleccionar themas libreria shinythemes
			navbarPage( "Módulo Actualización de Precios - Trujillo Investment 2021",
				tabPanel("Ingresos",
					sidebarPanel(style='margin-left:-40',width = 2,

						actionButton("idBtn1","Calculate", class = "btn-danger"),
						downloadButton('idBtn2','Download', class = "btn-success"),
						textOutput("selected_var")

						),
			
							fluidRow(
								column(7,
									tabPanel("Tabla de Productos",DT::dataTableOutput('table0.output'),style = 'font-size:85%')
								)
					)
				),
				tabPanel("Diferencias",
						# sidebarPanel(

						# 	),
							fluidRow( column(2),
								column(7,
									tabPanel("Tabla de Productos",DT::dataTableOutput('table1.output'),style = 'font-size:85%')
								)
							)

					)
			)
		),	
	server <- function(input, output,session ){
		
	 	shinyjs::hide("idBtn2")
	# Carga del algoritmo del ultimo comprobante registrado
		source(paste(pathglo,"/functions/ultima_compra.r",sep=""))
		data1 <- ultimascompra()

	# Algoritmo com caracteristicas reactivas
		frame1 <- reactive({data1[[1]]})
		frame2 <- reactive({data1[[2]]})

	#Fecha de consulta del reporte
		fechaid <- paste("Reporte para indetificar variaciones de precios | Actualizado al ",Sys.Date(),sep="")
		
	# Observe event del Btn 01 - Calcular mostras la facturas
		observeEvent(input$idBtn1,{

			#Tabla 01
			output$table0.output <- DT::renderDataTable({

				DT::datatable(frame1(),selection = "single", 
					options = list(pageLength = 22,
									autoWidth = TRUE,
									filter = "top",
									regex = TRUE,searchHighlight = TRUE)) 
				}) 

			#Tabla 02
			output$table1.output <- DT::renderDataTable({

				DT::datatable(frame2(),selection = "single", 
					options = list(pageLength = 22,
									autoWidth = TRUE,
									filter = "top",
									regex = TRUE,searchHighlight = TRUE)) %>% 
									formatStyle('Diff', color = styleInterval(c(0), c('red', 'blue')))

				})


	# Visualizar la fecha de consulta
			output$selected_var <- renderText({fechaid})


	# Muestra el segundo btn de Download
			shinyjs::show("idBtn2")
		
		# Funacion para descargar el archivo data 
			output$idBtn2 <- downloadHandler(

				filename = function(){
					paste("Factura de productos",Sys.Date(),".xlsx",sep="")
				},
				content = function(file) {
					write.xlsx(data1,file,row.names=TRUE)
				})




	    })

})
