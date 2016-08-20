## Estadística

### Descriptivos ####

tblFun <- function(x){
  tbl <- table(x)
  res <- cbind(tbl,round(prop.table(tbl)*100,2))
  colnames(res) <- c('N','%')
  print(res)
  write.csv(res, file = paste(deparse(substitute(x)), "csv", sep ='.'))
}

tblFun(datos$SEXO)
tblFun(datos$GRUPOEDAD)
tblFun(datos$ESTADODELP)
tblFun(datos$LETALIDAD)
tblFun(datos$COMUNA)

## función comparación cruzada de proporciones

prop.letalidad <- function(x, y){
  tbl <- table(x,y)
  coltable <- rbind(colSums(tbl), tbl)
  rownames(coltable)[1] <- "TOTAL"
  res.1 <- cbind(rowSums(coltable), coltable, (prop.table(coltable, 1) * 100))
  res.1 <- res.1[,c(1,2,4)]
  colnames(res.1) <- c("N", "Fallecidos", "Letalidad (%)")
  res.2 <- pairwise.prop.test(coltable, p.adjust.method = "none")
  cat("Disctribución de variables\n")
  print(res.1)
  clipboard(res.1)
  cat("\nComparación pareada de proporciones\n")
  print(res.2)
  clipboard(res.2$p.value)
}
prop.cx <- function(x, y){
  tbl <- table(x,y)
  coltable <- rbind(colSums(tbl), tbl)
  rownames(coltable)[1] <- "TOTAL"
  res.1 <- cbind(rowSums(coltable), coltable, (prop.table(coltable, 1) * 100))
  res.1 <- res.1[,c(1,2,4)]
  colnames(res.1) <- c("N", "Positivos", "Proporción (%)")
  res.2 <- pairwise.prop.test(coltable, p.adjust.method = "none")
  cat("Disctribución de variables\n")
  print(res.1)
  cat("\nComparación pareada de proporciones\n")
  print(res.2)
  clipboard(as.data.frame(res.2$p.value))
}

prop.letalidad(datos$SEXO, datos$LETALIDAD)
prop.letalidad(datos$Obs_Hanta, datos$LETALIDAD)
prop.cx(datos$GRUPOEDAD, datos$Obs_Hanta)


### Letalidad / Gravedad / Tiempo de consulta ####

table(datos$ESTADODELP)
table(datos$LETALIDAD)
table(datos$GRAVEDAD)

#Sacando SD de letalidad por año
CrossTable(datos$Año, datos$SEXO, expected = TRUE)

# Letalidad por edad
# table(datos$GRUPOEDAD, datos$LETALIDAD)
# prop.table(table(datos$GRUPOEDAD, datos$SEXO), 1)*100
# prop.table(table(datos$GRUPOEDAD, datos$LETALIDAD), 1)*100
# clipboard(prop.table(table(datos$GRUPOEDAD, datos$LETALIDAD), 1)*100)
prop.letalidad(datos$GRUPOEDAD, datos$LETALIDAD)
prop.letalidad(datos$Año, datos$LETALIDAD)

# Letalidad por sexo
table(datos$SEXO, datos$LETALIDAD)
prop.table(table(datos$SEXO, datos$LETALIDAD), 1)*100
prop.test(table(datos$SEXO, datos$LETALIDAD))

# letalidad por comuna
prop.table(table(datos$COMUNA, datos$LETALIDAD), 1)*100
# Comunas con más de 10 casos
comunas10 <- datos[ datos$COMUNA %in%  names(table(datos$COMUNA))[table(datos$COMUNA) >15] , ]
comunas10$COMUNA <- factor(comunas10$COMUNA)
CrossTable(comunas10$COMUNA, comunas10$LETALIDAD)

prop.letalidad(comunas10$COMUNA, comunas10$LETALIDAD)

## Letalidad por Región
pprop.letalidad(datos$REGION, datos$LETALIDAD)

## Descripción de TCONSULTA~ESTADODELP

cbind(mean(na.omit(datos$TCONSULTA)), ciMean(na.omit(datos$TCONSULTA)))

cbind(aggregate(TCONSULTA~ESTADODELP, datos, mean), aggregate(TCONSULTA~ESTADODELP, datos, ciMean))
cbind(aggregate(TCONSULTA~GRUPOEDAD, datos, mean), aggregate(TCONSULTA~GRUPOEDAD, datos, ciMean))

aggregate(TCONSULTA~ESTADODELP, datos, mean)
aggregate(TCONSULTA~LETALIDAD, datos, mean)
aggregate(TCONSULTA~ESTADODELP, datos, ciMean)
aggregate(TCONSULTA~GRAVEDAD, datos, mean)
aggregate(TCONSULTA~GRAVEDAD, datos, ciMean)
# T Test Días entre sintomas y consultas por Resultado
t.test(datos$TCONSULTA[datos$ESTADODELP == "FALLECIDO"], datos$TCONSULTA[datos$ESTADODELP == "GRAVE"])
t.test(datos$TCONSULTA[datos$ESTADODELP == "FALLECIDO"], datos$TCONSULTA[datos$ESTADODELP == "NO_GRAVE"])
t.test(datos$TCONSULTA[datos$ESTADODELP == "GRAVE"], datos$TCONSULTA[datos$ESTADODELP == "NO_GRAVE"])
# No grave vs todo lo demás
t.test(TCONSULTA~LETALIDAD, data=datos)
t.test(TCONSULTA~GRAVEDAD, data=datos)
t.test(TCONSULTA~SEXO, data=subset(datos, !is.na(datos$SEXO) & TCONSULTA >= 0 & TCONSULTA < 11))


## Descripción de RECBCOS~ESTADODELP
aggregate(RECBCOS~ESTADODELP, datos, mean)
aggregate(RECBCOS~ESTADODELP, datos, ciMean)
t.test(datos$RECBCOS[datos$ESTADODELP == "FALLECIDO"], datos$RECBCOS[datos$ESTADODELP == "GRAVE"])
t.test(datos$RECBCOS[datos$ESTADODELP == "FALLECIDO"], datos$RECBCOS[datos$ESTADODELP == "NO_GRAVE"])
t.test(RECBCOS~GRAVEDAD, data=datos)

## Descripción de TRIMESTRE~ESTADODELP
table(datos$TRIMPRIMSI, datos$LETALIDAD)
prop.table(table(datos$TRIMPRIMSI, datos$LETALIDAD), 1) * 100
# cbind(table(datos$TRIMPRIMSI, datos$ESTADODELP), prop.table(table(datos$TRIMPRIMSI, datos$ESTADODELP), 1), deparse.level = 2)
t.test(datos$TRIMPRIMSI[datos$ESTADODELP == "FALLECIDO"], datos$TRIMPRIMSI[datos$ESTADODELP == "GRAVE"])
prop.test(table(datos$TRIMPRIMSI, datos$LETALIDAD), 1)
# subset(datos, TRIMPRIMSI == c("3","4"))

## Muertes por Mes
prop.table(table(datos$MESPRIMSI, datos$ESTADODELP), 1) * 100
prop.table(table(datos$MESPRIMSI, datos$LETALIDAD), 1) * 100
pairwise.prop.test(rbind(table(datos$MESPRIMSI, datos$LETALIDAD), colSums(table(datos$MESPRIMSI, datos$LETALIDAD))), 
                   p.adjust.method = "none")
prop.cx(datos$MESPRIMSI, datos$LETALIDAD)


rbind(table(datos$MESPRIMSI, datos$LETALIDAD), 
      colSums(table(datos$MESPRIMSI, datos$LETALIDAD)))
## Muertes por Centro:
CrossTable(datos$ESTABLECIM, datos$LETALIDAD, 
           prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
table(datos$ESTABLECIM, datos$LETALIDAD)
prop.table(table(datos$ESTABLECIM, datos$LETALIDAD), 1)

# Tiempo entre primeros sítnomas y muerte.
fallecidos <- subset(datos, !is.na(FFALLEC) & TFALLEC >= 0 & TFALLEC < 100)
t.test(fallecidos$TFALLEC)

# Residencia Rrual
rural <- subset(datos, RERURAL == "SI" | RERURAL == "NO")
rural$RERURAL <- droplevels(rural$RERURAL)
summary(rural$RERURAL)
prop.table(summary(rural$RERURAL))
table(rural$RERURAL, rural$LETALIDAD)
prop.table(table(rural$RERURAL, rural$LETALIDAD),1 )
prop.test(table(rural$RERURAL, rural$LETALIDAD))

# Agricola
agricola <- subset(datos, TRABAG =="SI" | TRABAG =="NO")
agricola$TRABAG <- droplevels(agricola$TRABAG)
summary(agricola$TRABAG)
prop.table(summary(agricola$TRABAG))
prop.table(table(agricola$TRABAG, agricola$LETALIDAD),1 )
prop.test(table(agricola$TRABAG, agricola$LETALIDAD))

# Forestal
forestal <- subset(datos, TRABFOR == "SI" | TRABFOR == "NO")
forestal$TRABFOR <- droplevels(forestal$TRABFOR)
summary(forestal$TRABFOR)
prop.table(summary(forestal$TRABFOR))
prop.table(table(forestal$TRABFOR, forestal$LETALIDAD),1)
prop.test(table(forestal$TRABFOR, forestal$LETALIDAD))

# Excursión
excur <- subset(datos, EXCURSIONI == "SI" | EXCURSIONI == "NO")
excur$EXCURSIONI <- droplevels(excur$EXCURSIONI)
summary(excur$EXCURSIONI)
prop.table(summary(excur$EXCURSIONI))
prop.table(table(excur$EXCURSIONI, excur$LETALIDAD),1)
prop.test(table(excur$EXCURSIONI, excur$LETALIDAD))

# Contacto Urbano
urb <- subset(datos, TIPOLOC=="RURAL" | TIPOLOC=="URBANO")
urb$TIPOLOC <- droplevels(urb$TIPOLOC)
summary(urb$TIPOLOC)
prop.table(summary(urb$TIPOLOC))
prop.table(table(urb$TIPOLOC, urb$LETALIDAD),1)
prop.test(table(urb$TIPOLOC, urb$LETALIDAD))

# ¿Qué estaban haciendo los no rurales no agricolas?
norul <- subset(datos, TRABFOR == "NO" & TRABAG =="NO")
summary(norul$EXCURSIONI)
prop.table(summary((norul$EXCURSIONI)))*100
summary(norul$RERURAL)
prop.table(summary((norul$RERURAL)))*100
summary(na.omit(norul$TIPOLOC))
prop.table(summary(na.omit(norul$TIPOLOC)))
length(subset(datos, TRABFOR == "SI" | TRABFOR == "NO" | TRABAG =="SI" | TRABAG =="NO")$TRABAG)

# Síntomas:

tblFun1 <- function(y){
  tbl <- table(y)
  res <- cbind(tbl,round(prop.table(tbl)*100,2))
  colnames(res) <- c('N','Porentaje')
  res
}
tblFun1(datos$GASTRO)
tblFun1(datos$FIEBRE)
tblFun1(datos$CEFALEA)
tblFun1(datos$CRESPIRA)
tblFun1(datos$MIALGIAS)
## Mayores de 15
tblFun1(datos$GASTRO[datos$EDAD < 10])
tblFun1(datos$FIEBRE[datos$EDAD < 10])
tblFun1(datos$CEFALEA[datos$EDAD < 10])
tblFun1(datos$CRESPIRA[datos$EDAD < 10])
tblFun1(datos$MIALGIAS[datos$EDAD < 10])

# Regresión lineal muertes por centro:
xcentro10 <- subset(xcentro, Freq > 10)
summary(lm(FALLECIDO.1 ~ Freq, data=xcentro10))
xyplot(FALLECIDO.1 ~ Freq, data=xcentro10)

# Comparación cruzada entre centros con más de 10 casos
pairwise.prop.test(xcentro10$FALLECIDO, xcentro10$Freq, p.adjust.method = "none")