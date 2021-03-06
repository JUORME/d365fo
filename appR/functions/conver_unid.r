conver_unidades <- function(x){
	x <- gsub("Emp.10$","10", x,ignore.case = FALSE)
	x <- gsub("Emp.100$","100", x,ignore.case = FALSE)
	x <- gsub("Emp.1000$","100", x,ignore.case = FALSE)
	x <- gsub("Emp.102$","102", x,ignore.case = FALSE)
	x <- gsub("Emp.104$","104", x,ignore.case = FALSE)
	x <- gsub("Emp.108$","108", x,ignore.case = FALSE)
	x <- gsub("Emp.104$","104", x,ignore.case = FALSE)
	x <- gsub("Emp.11$","11", x,ignore.case = FALSE)
	x <- gsub("Emp.112$","112", x,ignore.case = FALSE)
	x <- gsub("Emp.12$","12", x,ignore.case = FALSE)
	x <- gsub("Emp.120$","120", x,ignore.case = FALSE)
	x <- gsub("Emp.128$","128", x,ignore.case = FALSE)
	x <- gsub("Emp.13$","13", x,ignore.case = FALSE)
	x <- gsub("Emp.135$","135", x,ignore.case = FALSE)
	x <- gsub("Emp.14$","14", x,ignore.case = FALSE)
	x <- gsub("Emp.140$","140", x,ignore.case = FALSE)
	x <- gsub("Emp.144$","144", x,ignore.case = FALSE)
	x <- gsub("Emp.146$","146", x,ignore.case = FALSE)
	x <- gsub("Emp.15$","15", x,ignore.case = FALSE)
	x <- gsub("Emp.150$","150", x,ignore.case = FALSE)
	x <- gsub("Emp.16$","16", x,ignore.case = FALSE)
	x <- gsub("Emp.160$","160", x,ignore.case = FALSE)
	x <- gsub("Emp.162$","162", x,ignore.case = FALSE)
	x <- gsub("Emp.168$","168", x,ignore.case = FALSE)
	x <- gsub("Emp.18$","18", x,ignore.case = FALSE)
	x <- gsub("Emp.180$","180", x,ignore.case = FALSE)
	x <- gsub("Emp.182$","182", x,ignore.case = FALSE)
	x <- gsub("Emp.192$","192", x,ignore.case = FALSE)
	x <- gsub("Emp.2$","2", x,ignore.case = FALSE)
	x <- gsub("Emp.20$","20", x,ignore.case = FALSE)
	x <- gsub("Emp.200$","200", x,ignore.case = FALSE)
	x <- gsub("Emp.21$","21", x,ignore.case = FALSE)
	x <- gsub("Emp.216$","216", x,ignore.case = FALSE)
	x <- gsub("Emp.22$","22", x,ignore.case = FALSE)
	x <- gsub("Emp.24$","24", x,ignore.case = FALSE)
	x <- gsub("Emp.240$","240", x,ignore.case = FALSE)
	x <- gsub("Emp.25$","25", x,ignore.case = FALSE)
	x <- gsub("Emp.250$","250", x,ignore.case = FALSE)
	x <- gsub("Emp.252$","252", x,ignore.case = FALSE)
	x <- gsub("Emp.256$","256", x,ignore.case = FALSE)
	x <- gsub("Emp.26$","26", x,ignore.case = FALSE)
	x <- gsub("Emp.27$","27", x,ignore.case = FALSE)
	x <- gsub("Emp.270$","270", x,ignore.case = FALSE)
	x <- gsub("Emp.28$","28", x,ignore.case = FALSE)
	x <- gsub("Emp.288$","288", x,ignore.case = FALSE)
	x <- gsub("Emp.3$","3", x,ignore.case = FALSE)
	x <- gsub("Emp.30$","30", x,ignore.case = FALSE)
	x <- gsub("Emp.300$","300", x,ignore.case = FALSE)
	x <- gsub("Emp.312$","312", x,ignore.case = FALSE)
	x <- gsub("Emp.32$","21", x,ignore.case = FALSE)
	x <- gsub("Emp.320$","320", x,ignore.case = FALSE)
	x <- gsub("Emp.336$","336", x,ignore.case = FALSE)
	x <- gsub("Emp.34$","34", x,ignore.case = FALSE)
	x <- gsub("Emp.342$","342", x,ignore.case = FALSE)
	x <- gsub("Emp.35$","35", x,ignore.case = FALSE)
	x <- gsub("Emp.36$","36", x,ignore.case = FALSE)
	x <- gsub("Emp.360$","360", x,ignore.case = FALSE)
	x <- gsub("Emp.37$","37", x,ignore.case = FALSE)
	x <- gsub("Emp.38$","38", x,ignore.case = FALSE)
	x <- gsub("Emp.4$","4", x,ignore.case = FALSE)
	x <- gsub("Emp.40$","40", x,ignore.case = FALSE)
	x <- gsub("Emp.400$","400", x,ignore.case = FALSE)
	x <- gsub("Emp.42$","42", x,ignore.case = FALSE)
	x <- gsub("Emp.432$","432", x,ignore.case = FALSE)
	x <- gsub("Emp.45$","45", x,ignore.case = FALSE)
	x <- gsub("Emp.48$","48", x,ignore.case = FALSE)
	x <- gsub("Emp.480$","480", x,ignore.case = FALSE)
	x <- gsub("Emp.5$","5", x,ignore.case = FALSE)
	x <- gsub("Emp.50$","50", x,ignore.case = FALSE)
	x <- gsub("Emp.500$","500", x,ignore.case = FALSE)
	x <- gsub("Emp.504$","504", x,ignore.case = FALSE)
	x <- gsub("Emp.528$","528", x,ignore.case = FALSE)
	x <- gsub("Emp.54$","54", x,ignore.case = FALSE)
	x <- gsub("Emp.56$","56", x,ignore.case = FALSE)
	x <- gsub("Emp.560$","560", x,ignore.case = FALSE)
	x <- gsub("Emp.576$","576", x,ignore.case = FALSE)
	x <- gsub("Emp.6$","6", x,ignore.case = FALSE)
	x <- gsub("Emp.60$","60", x,ignore.case = FALSE)
	x <- gsub("Emp.600$","600", x,ignore.case = FALSE)
	x <- gsub("Emp.624$","624", x,ignore.case = FALSE)
	x <- gsub("Emp.63$","63", x,ignore.case = FALSE)
	x <- gsub("Emp.64$","64", x,ignore.case = FALSE)
	x <- gsub("Emp.648$","648", x,ignore.case = FALSE)
	x <- gsub("Emp.7$","7", x,ignore.case = FALSE)
	x <- gsub("Emp.70$","70", x,ignore.case = FALSE)
	x <- gsub("Emp.72$","72", x,ignore.case = FALSE)
	x <- gsub("Emp.78$","78", x,ignore.case = FALSE)
	x <- gsub("Emp.8$","8", x,ignore.case = FALSE)
	x <- gsub("Emp.80$","80", x,ignore.case = FALSE)
	x <- gsub("Emp.800$","800", x,ignore.case = FALSE)
	x <- gsub("Emp.81$","81", x,ignore.case = FALSE)
	x <- gsub("Emp.84$","84", x,ignore.case = FALSE)
	x <- gsub("Emp.840$","840", x,ignore.case = FALSE)
	x <- gsub("Emp.86$","86", x,ignore.case = FALSE)
	x <- gsub("Emp.9$","9", x,ignore.case = FALSE)
	x <- gsub("Emp.90$","90", x,ignore.case = FALSE)
	x <- gsub("Emp.92$","92", x,ignore.case = FALSE)
	x <- gsub("Emp.96$","96", x,ignore.case = FALSE)
	x <- gsub("Emp.98$","98", x,ignore.case = FALSE)
	x <- gsub("KGM$","1", x,ignore.case = FALSE)
	x <- gsub("LTR$","1", x,ignore.case = FALSE)
	x <- gsub("SA$","1", x,ignore.case = FALSE)
	x <- gsub("SRV$","1", x,ignore.case = FALSE)
	x <- gsub("U$","1", x,ignore.case = FALSE)
	x <- gsub("U.$","1", x,ignore.case = FALSE)
	x <- gsub("UND$","1", x,ignore.case = FALSE)
}