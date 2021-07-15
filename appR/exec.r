ent <- 1
if(ent == 1){
	pathglo <- "D:/github/d365fo/appR"
}else {
	pathglo <- "/srv/shiny-server/d365fo/appR"
}

setwd(pathglo)

library(shiny)
library(shinydashboard)

runApp("app", host="0.0.0.0", port=8400)