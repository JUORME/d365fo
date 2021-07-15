#####################################################################################
#  Proyecto D365 FO                                                                 #
#  Extraccion de precios de coste de ultima compra                                  #
#  Autor: Junior T. Ortiz Mejia                                                     #
#  Fecha: 22/03/2021                                                                #                                                                              
#####################################################################################

ultimascompra <- function (){

#Fijar el la ruta de trabajo

ent <- 1
if(ent == 1){
	pathglo <- "D:/github/appi011/appR/functions"
}else {
	pathglo <- "/srv/shiny-server/appi011/appR/functions"
}


setwd(pathglo)


#Blibliotecas requeridas
	library(httr)    
	library(rjson)  
	library(jsonlite)
	library(dplyr)
	library(openxlsx)
	library(data.table)
	library(bit64)
	library(stringr)

#Conexion a D365FO TRUJILLO INVESTMENT
	body <- list(grant_type = "client_credentials", client_id = "7cb678f1-2bc4-4456-a590-f7216b23dd88",
	client_secret = "~9X1PR2etZ1sHw1tsv-15Y.pD3F_RaTCxG", resource = "https://mistr.operations.dynamics.com")

	response <-POST("https://login.microsoftonline.com/ceb88b8e-4e6a-4561-a112-5cf771712517/oauth2/token",add_headers("Cookie: x-ms-gateway-slice=prod; stsservicecookie=ests; fpc=AqQZzzXZjstDgAtb0IfeeFZVotOLAQAAANAmrtYOAAAA"), body=body,encode = "form")

	datatoken <-fromJSON(content(response,type="text")) %>% as.data.frame
	tok_type<-as.character(datatoken[1,1])
	tok<-as.character(datatoken[1,7])
	token <- paste(tok_type," ",tok,"",sep="")


# Uso de la funcion para extraer datos con el Entity proporcionado	
	fi <- "2021-04-30"
	source("algoritmo_count.r")
	# url1 <- paste("https://mistr.operations.dynamics.com/data/MSAVendInvoicesV2Entities/$count?$filter=InventQty%20ge%200%20and%20CreatedDateTimeInvoice%20le%20",fi,"",sep="") #URl para data 01
	url1 <- paste("https://mistr.operations.dynamics.com/data/MSAVendInvoicesV2Entities/$count?$filter=InventQty%20ge%200%20and%20CreatedDateTimeInvoice%20ge%20",fi,"",sep="")
	count<-get_count_url(url1,token)

	lote<-10000
	source("genera_url.r")
	# urldata1<-paste("https://mistr.operations.dynamics.com/data/MSAVendInvoicesV2Entities?$skip=rvar_s&$top=rvar_t&$filter=InventQty%20ge%200%20and%20CreatedDateTimeInvoice%20le%20",fi,"%20&$select=InventLocationId,PurchId,InvoiceId,ItemId,PurchUnit,LineAmount,TaxAmount,InventQty,InvoiceDocumentDate,CreatedDateTimeInvoice,CurrencyCode,OrderAccount,Name",sep="")
	urldata1<-paste("https://mistr.operations.dynamics.com/data/MSAVendInvoicesV2Entities?$skip=rvar_s&$top=rvar_t&$filter=InventQty%20ge%200%20and%20CreatedDateTimeInvoice%20ge%20",fi,"%20&$select=InventLocationId,PurchId,InvoiceId,ItemId,PurchUnit,LineAmount,TaxAmount,InventQty,InvoiceDocumentDate,CreatedDateTimeInvoice,CurrencyCode,OrderAccount,Name",sep="")
	vec1<-genera_url(count,lote,urldata1)

	source("function_get_collect.r")
	prodprice <- get_records_url(vec1,token)
	#head(prodprice)


# Guardar data1
	# fwrite(prodprice,"../upload/data1.csv", sep=",")

	data2 <- as.data.frame(prodprice)

	data1 <- read.csv("../upload/data1.csv", sep=",")

	alldata0 <- rbind(data1,data2)

	alldata0$InvoiceDocumentDate <- as.character(as.POSIXct(alldata0$InvoiceDocumentDate, format="%Y-%m-%d",tz="UTC"))
	#prodprice$CreatedDateTimeInvoice <- as.character(as.POSIXct(prodprice$CreatedDateTimeInvoice, format="%Y-%m-%d %H:%M:%S",tz="UTC"))


	fwrite(alldata0,"../upload/alldata0.csv", sep= ",")	



# Algoritmo Ingresos

	source("conver_unid.r")
	p2 <- alldata0 %>%
			group_by(ItemId) %>% 
			filter(LineAmount != 0.0000) %>%
			arrange(desc(InvoiceDocumentDate)) %>%
			subset(InvoiceDocumentDate > seq(as.Date(max(InvoiceDocumentDate)), length=2, by="-3 months")[2] & InvoiceDocumentDate <= max(InvoiceDocumentDate)) %>%
			slice_head(n=10) %>%
			mutate(ConvU = conver_unidades(PurchUnit)) %>%
			mutate(P_igv = LineAmount + TaxAmount,P_uni = round( P_igv/ InventQty,3))


	# Seleccionar el valor maximo del precio unitario
	p3 <- p2 %>% group_by(ItemId) %>%
				 slice(which.max(P_uni)) %>%
				 mutate(Cnt = round(InventQty/as.numeric(ConvU),3))



# Algoritmo de diferencias

	source("conver_unid.r")
	f2 <- alldata0 %>%
			group_by(ItemId,InventLocationId) %>% 
			filter(LineAmount != 0.0000) %>%
			arrange(InventLocationId,desc(InvoiceDocumentDate)) %>%
			slice_head(n=2) %>%
			mutate(ConvU = conver_unidades(PurchUnit))%>%
			mutate(P_igv = LineAmount + TaxAmount,P_uni = round( P_igv/ InventQty,3))



	f3 <- f2 %>%
			group_by(ItemId, InventLocationId) %>%
			filter(P_uni == max(P_uni),InvoiceDocumentDate == max(InvoiceDocumentDate))
			

	f4 <- f2 %>%
			group_by(ItemId, InventLocationId) %>%
			filter(P_uni == min(P_uni),InvoiceDocumentDate == min(InvoiceDocumentDate))


	f5 <- merge(f3, f4 ,by = c('InventLocationId','ItemId'))

	f6 <- f5 %>% select(OrderAccount.x,Name.x,InventLocationId,ItemId,InvoiceId.x,CurrencyCode.x,P_uni.x,P_uni.y) %>%
					mutate(diff = round(P_uni.x - P_uni.y,3))

	names(f6) <- c("Cuenta Proveedor","Nombre","Almacén","Cod","Factura","Divisa","P_Ult","P_Ant","Diff")

	f7 <- as.data.frame(f6)
			

	# p3 <- p2 %>% group_by(ItemId) %>%
	# 			 slice(which.max(P_uni)) %>%
	# 			 mutate(Cnt = round(InventQty/as.numeric(ConvU),3))


	p4 <- as.data.frame(p3)



# Extracción de nombre de los productos
	# Descarga de datos 
	source("function_get_collect.r")
	nameprod <- get_records_url("https://mistr.operations.dynamics.com/data/AllProducts?$select=ProductNumber,ProductName",token)
	# head(nameprod)

	prod <- nameprod
	# fwrite(prod,"../upload/nameprod.csv", sep = ",")

#carga de datos 
	# nameprod <- read.csv("../upload/nameprod.csv",sep = ",")



#Unir los dos data frames
	u1 <- merge(p4, nameprod , by.x = 'ItemId', by.y="ProductNumber", all.x = TRUE)

	u2 <- u1 %>% 
			select(ItemId,ProductName,InventLocationId,InvoiceDocumentDate,InvoiceId,Cnt,PurchUnit,P_igv,P_uni,CurrencyCode)


	names(u2) <- c("Cod","Descripción","Almacén","FechaE","Factura","Cnt","CUni","CPrec","PUni","Divisa")	

	q1 <- list(u2,f7)	

	return(q1)

} 





