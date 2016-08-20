## Directorio de trabajo, cargar datos y librería ##
Sys.setenv(LANG = "es")
setwd("MEGAsync/Pediatria/Proyecto Investigación/Hanta/Bases de Datos/Final")
options(OutDec= ",", digits=3)
datos <-read.csv("R.csv", na.strings=c(""," ","NA", "IG"))
xcentro <-read.csv("../muertes.por.centro.csv", na.strings=c(""," ","NA"))
datos.old <- read.csv("../bs2.csv", na.strings=c(""," ","NA"))

## Bilbiotecas
library(lattice)
library(RColorBrewer)
library(ggplot2)
library(plyr)
library(reshape2)
library(lsr)
library(lubridate)
library(xtable)
library(gmodels)
library(scales)
library(qdapTools)
library(R.utils)
library(odfWeave)

## Conversión datos ##
# Transofrmar factores
datos$SEXO[datos$SEXO == 2] <- "Mujer"
datos$SEXO[datos$SEXO == 1] <- "Hombre"
datos$SEXO <- as.factor(datos$SEXO)
datos$Año <- as.factor(datos$Año)
datos$ESTADODELP <- as.factor(datos$ESTADODELP)
datos$LETALIDAD <- as.factor(datos$LETALIDAD)
datos$EDAD <- as.numeric(datos$EDAD)

# Fechas
datos$FPRIMSI <- as.Date(datos$FPRIMSI, "%d-%m-%Y")
datos$FCONSUL <- as.Date(datos$FCONSUL, "%d-%m-%Y")
datos$F <- as.Date(datos$F, "%d-%m-%Y")
datos$FFALLEC <- as.Date(datos$FFALLEC, "%d-%m-%Y")
datos$MESPRIMSI <- as.factor(format(datos$FPRIMSI, "%b"))
datos$MESPRIMSI <- factor(datos$MESPRIMSI, levels = c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic"))
datos$TRIMPRIMSI <- as.factor(quarter(datos$FPRIMSI))
# Teimpo entre síntomas y consulta
datos$TCONSULTA <- as.numeric(datos$FCONSUL - datos$FPRIMSI)
datos$TFALLEC <- as.numeric(datos$FFALLEC - datos$FPRIMSI)
#daF == Fecha Dg????
#datos$TDG <- datos$F - datos$FPRIMSI

## Comunas, remplazando según los códigos DEIS 2010 y luego 2000
deis <- read.csv("../Minsal/deis.csv", colClasses = "character")
deis2 <- deis[,c("cod2010", "nombrecom")]
deis1 <- deis[,c("cod2000", "nombrecom")]
deis3  <- deis[,c("nombrecom", "cod_reg")]
deis3$nombrecom <- capitalize(tolower(deis3$nombrecom))
datos$COMUNA <- as.character(datos$COMUNA)
datos$COMUNA <- lookup(datos$COMUNA, deis2, missing = NULL)
datos$COMUNA <- lookup(datos$COMUNA, deis1, missing = NULL)
datos$COMUNA <- as.factor(capitalize(tolower(datos$COMUNA)))
datos$COMUNA <- revalue(datos$COMUNA, c("Aisén"="Aysén","Cisne"="Cisnes","Pto cisnes"="Cisnes","Coihaique"="Coyhaique",
                        "Curacautin"="Curacautín","La·pintana"="La pintana","Valparaiso"="Valparaíso",
                        "Los angeles"="Los ángeles","Ñiquen"="Ñiquén","Peñalolen"="Peñalolén",
                        "Pitrufquen"="Pitrufquén","Tolten"="Toltén","Alhue"="Alhué","Machali"="Machalí",
                        "San fabian"="San fabián","Mafil" ="Máfil", "Licanten"="Licantén", "Hualpen"="Hualpén",
                        "Trehuaco"="Treguaco","Coleemu"="Coelemu","Traiguen"="Traiguén","San ramon"="San ramón",
                        "San pablo "="San pablo","Entre lagos"="Puyehue","O higgins"="O'higgins",
                        "Niebla"="Valdivia", "S. vicente tagua tagua"="San vicente", "San josé de la mariquina"="Mariquina"))
datos$REGION <- as.character(datos$COMUNA)
datos$REGION <- lookup(datos$REGION, deis3, missing = NULL)
datos$REGION <- revalue(datos$REGION, c("Aysén"="11","Coyhaique"="11"))
datos$REGION <- as.factor(datos$REGION)
# Reagrupar
# Grupos Etarios
datos$GRUPOEDAD <- factor(findInterval(datos$EDAD, c(-Inf, 5, 10, 15, 18, 35, 60, Inf)), 
                          labels = c("0-5", "5-10", "10-15", "15-18", "18-35", "35-60", "60->"))

edades <- summary(datos$GRUPOEDAD)
edades <- as.data.frame(prop.table(edades))
colnames(edades) <- c("Freq")
# edades$Edad <- as.factor(edades$Edad)
# edades$Freq <- as.numeric(edades$Freq)


# Tiempo entre síntomas ## No vale la pena
  # datos$G_TCONSULTA <- factor(findInterval(datos$TCONSULTA, c(-Inf, 0, 2, Inf)),  
    #                     labels = c("-0NULL", "0-48h", ">48h"))

## Función portapapeles
clipboard <- function(x, sep="\t", row.names=TRUE, col.names=TRUE){
  con <- pipe("xclip -selection clipboard -i", open="w")
  write.table(x, con, sep=sep, row.names=row.names, col.names=col.names)
  close(con)
}
