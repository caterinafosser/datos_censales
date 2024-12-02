###MUNICIPIO DE LOMAS DE ZAMORA, PCIA. DE BUENOS AIRES
#CODIGO PARA LA ADAPTACION DE LOS DATOS DE RADIO CENSALES A FORMATO COMPATIBLE CON QGIS
#diciembre 2024

#PREPARACION
setwd("") #setear directorio de trabajo
library(readxl) #importar librerias
library(openxlsx)

#IMPORTACION DE LOS DATOS EN FORMATO TABLA
#establecer la direccion del archivo. basta con reeemplazar el nombre del archivo .xlsx (con esta terminacion) en nombre_del_archivo
direccion<-paste(getwd(),"/",nombre_del_archivo,sep="") 
#leer la tabla con readxl. saltea las primeras 13 filas y determina nombres para las columnas
tabla <- read_excel(direccion,skip=13,col_names = c("Area","Variable","Casos","Porcentaje","acumulado"))
#recortar las primeras 4 columnas. si se quiere el %acumulado, puede omitirse.
tabla<-tabla[,1:4]


#RECORRIDO Y AJUSTE DE LOS DATOS
#recorrer la tabla por fila
for (i in 1:nrow(tabla)){
  #si la 2da columna contiene el area, extrae el numero del area y pasa a la siguiente fila
  if (grepl("AREA", tabla[[i,2]])==TRUE){
    nro_area<-substr(tabla[[i,2]],8,16)
    next
  }
  #si la 3ra columna son los casos (=si toda la fila es el encabezado para ese radio), guarda la 2da columna como titulo para exportar la tabla final y pasa a la siguiente fila
  if (grepl("Casos",tabla[[i,3]])==TRUE){
    titulo<-tabla[[i,2]]
    next
  }
  #si la 2da columna esta vacia o si dice Total, pasa a la siguiente fila
  if (is.na(tabla[[i,2]])==TRUE | (tabla[[i,2]]=="Total")==TRUE){
    next
  }
  #si la 2da columna dice Resumen, no considera estos datos ni los del final del documento. corta el ciclo
  if (grepl("RESUMEN", tabla[[i,2]])==TRUE){
    break
  }
  #en todas las filas no consideradas antes, el valor guardado de numero de area se asigna a la primera columna.
  tabla[[i,1]]<-nro_area
}

#crear una nueva tabla con las filas que sirven, es decir, aquellas cuya primera columna es distinta de NA
nueva_tabla <- tabla[!is.na(tabla[, 1]), ]
colnames(nueva_tabla)<-c("CLAVERA",toupper(titulo),"CASOS","PORCENTAJE")
nueva_tabla$CASOS<-as.numeric(nueva_tabla$CASOS)
nueva_tabla$PORCENTAJE<-as.numeric(nueva_tabla$PORCENTAJE)

#EXPORTAR LOS DATOS COMO TABLA CORREGIDA
#el nombre de archivo termina en 2022 por el anio del censo (puede modificarse)
#finalizacion con .csv, y row.names=F para que no se exporte con los nombres de las filas
write.csv(nueva_tabla,paste(getwd(),"/",titulo," 2022.csv",sep=""),row.names = F)
