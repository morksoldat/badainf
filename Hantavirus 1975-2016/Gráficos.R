##### Gráficos #####

#### Descriprivos ####

# Edades #
edades <- summary(datos$GRUPOEDAD)
edades <- prop.table(edades)
edades <- data.frame(Edad=names(edades), Freq=edades)
edades$Edad <- as.factor(edades$Edad)
edades$Freq <- as.numeric(edades$Freq)
br <- cumsum(edades$Freq) - edades$Freq/2

ggplot(edades, aes(x="", y=Freq, fill=Edad))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0)+
  scale_y_continuous(
    breaks=br,   # where to place the labels
    labels= paste(round(edades$Freq*100,2), "%") # the labels 
    ) 
# Sacando NA de letalidad y gravedad.
letalidad <- subset(datos, !is.na(LETALIDAD))
gravedad <- subset(datos, !is.na(ESTADODELP))

# Muertes por Año
letalidad.año <- subset(letalidad, !Año=="1995" & !Año=="1994" & !Año=="1993" & !Año=="1975")
letalidad.año$Año <- droplevels(letalidad.año$Año)
ggplot(letalidad.año, aes(x=letalidad.año$Año)) +
  geom_bar(aes(fill=letalidad.año$LETALIDAD), position = "fill") +
  ggtitle("") + labs(x="Año", y="Letalidad", fill="aa") +
  scale_y_continuous(labels=percent)+
  scale_fill_manual(values = rev(brewer.pal(3,"RdBu")))+theme_bw()+
  # scale_fill_grey(start = 0, end = .9)+ #blanco y negro
  theme(legend.position="none") +
  # theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) +
  theme(text = element_text(size=15))

# Grupos por Sexo

gravedad.sexo <- subset(gravedad, !is.na(SEXO))
ggplot(gravedad.sexo, aes(x=gravedad.sexo$SEXO)) +
  geom_bar(aes(fill=gravedad.sexo$ESTADODELP), position = "fill",) +
  ggtitle("Gravedad por Sexo") + labs(x="Sexo") + scale_y_continuous("",labels  = percent) +
  theme(legend.position="bottom", legend.direction="horizontal", 
        legend.title = element_blank()) +
  theme(text = element_text(size=20))

ggplot(gravedad.sexo, aes(x=gravedad.sexo$GRUPOEDAD)) +
  geom_bar(aes(fill=gravedad.sexo$SEXO), position = "fill",)

# Grupos por Edad
gravedad.edad <- subset(gravedad, !is.na(GRUPOEDAD))
ggplot(gravedad.edad, aes(x=gravedad.edad$GRUPOEDAD)) +
  geom_bar(aes(fill=gravedad.edad$ESTADODELP), position = "fill",) +
  ggtitle("Gravedad por Edad") + labs(x="Edad") + scale_y_continuous("",labels  = percent) +
  theme(legend.position="bottom", legend.direction="horizontal", 
        legend.title = element_blank()) +
  theme(text = element_text(size=20))

# Letalidad por Edad

ggplot(gravedad.edad, aes(x=gravedad.edad$GRUPOEDAD)) +
  geom_bar(aes(fill=gravedad.edad$LETALIDAD), position = "fill",) +
  ggtitle("Gravedad por Edad") + labs(x="Edad") + scale_y_continuous("",labels  = percent) +
  theme(legend.position="bottom", legend.direction="horizontal", 
        legend.title = element_blank()) +
  theme(text = element_text(size=20))

# Letalidad por Sexo
letalidad.sexo <- subset(letalidad, !is.na(SEXO))
ggplot(letalidad.sexo, aes(x=letalidad.sexo$SEXO)) +
  geom_bar(aes(fill=letalidad.sexo$LETALIDAD), position = "fill",) +
  ggtitle("Letalidad por Sexo") + labs(x="Sexo", y="Proporción") +
  theme(legend.position="bottom", legend.direction="horizontal", 
        legend.title = element_blank())

# Letalidad por comunas
ggplot(comunas10, aes(x=comunas10$COMUNA)) +
  geom_bar(aes(fill=comunas10$LETALIDAD), position = "fill",) +
  ggtitle("") + labs(x="Comuna") + scale_y_continuous("Letalidad",labels  = percent) +
  scale_fill_manual(values = rev(brewer.pal(3,"RdBu")))+theme_bw()+
  # scale_fill_grey(start = 0, end = .9)+ #blanco y negro +
  theme(legend.position="none") +
  #theme(legend.position="right", legend.direction="vertical", 
   #     legend.title = element_blank(), text = element_text(size=20)) +
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5)) +
  coord_cartesian(ylim=c(0,0.60))

# Casos por Mes
qplot(datos$MESPRIMSI, data=datos) +
  labs(x="Mes del Año", y="Total de casos", title="Casos por mes") +
  theme(text = element_text(size=20))

# Muertes por Mes

letalidad.mes <- subset(letalidad, !is.na(MESPRIMSI))
ggplot(letalidad.mes, aes(x=letalidad.mes$MESPRIMSI)) +
  geom_bar(aes(fill=letalidad.mes$LETALIDAD), position = "fill",) +
  ggtitle("") + labs(x="", y="") + 
  scale_y_continuous("",labels  = percent) +
  scale_fill_manual(values = rev(brewer.pal(3,"RdBu")))+theme_bw()+
  theme(legend.position="none", legend.direction="vertical", 
        legend.title = element_blank(), text = element_text(size=20)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  theme(text = element_text(size=15))+
  coord_cartesian(ylim=c(0,0.50))

## Sexo según días entre Primeros síntomas y Consulta
qplot(SEXO, as.numeric(TCONSULTA), 
      data = subset(datos, !is.na(datos$SEXO) & TCONSULTA >= 0 & TCONSULTA < 11), 
      geom= "boxplot")+
  labs(x="Estado del Paciente", y="Días entre 1° Síntomas y 1° Consulta",
       title="Resultado según Demora en Consultar") +
  theme(text = element_text(size=20))


# Estado según días entre Primeros síntomas y Consulta
qplot(ESTADODELP, as.numeric(TCONSULTA), 
      data = subset(datos, !is.na(datos$ESTADODELP) & TCONSULTA >= 0 & TCONSULTA < 11), 
      geom= "boxplot")+
  labs(x="Estado del Paciente", y="Días entre 1° Síntomas y 1° Consulta",
       title="Resultado según Demora en Consultar") +
  theme(text = element_text(size=20))

qplot(GRUPOEDAD, as.numeric(TCONSULTA), 
      data = subset(datos, !is.na(datos$GRUPOEDAD) & TCONSULTA >= 0 & TCONSULTA < 11), 
      geom= "boxplot")

# Letalidad según días entre Primeros síntomas y Consulta
qplot(LETALIDAD, as.numeric(TCONSULTA), 
      data = subset(datos, !is.na(datos$ESTADODELP) & TCONSULTA >= 0 & TCONSULTA < 11), 
      geom= "boxplot")+
  labs(x="Estado del Paciente", y="Días entre 1° Síntomas y 1° Consulta",
       title="Resultado según Demora en Consultar") +
  theme(text = element_text(size=20))

## Muertes por centro
# 10 con más casos

ggplot(data=xcentro, aes(x=Freq, y=FALLECIDO.1)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x, size = 1)+
  scale_x_continuous(limits = c(10, 35))+
  scale_y_continuous(limits = c(0, 0.6))+
  theme_bw()+
  labs(x="Casos por Centro", y="Letalidad", title="Letalidad según número de casos")+
  theme(text = element_text(size=20))

#### no corregidos a ggplot!, qplot ya no satisface necesidades y cambió código.
# Grupos por Edad

# Letalidad por Edad
qplot(EDAD, data=datos, fill=LETALIDAD, position="fill")+
  labs(x="Sexo", y="Prorporción de Casos", title="Letalidad por Sexo",
       fill="Estado Del Paciente")


## con líneas de error
mt.new<-ddply(mtcars,.(cyl),summarise,
              prop=sum(am)/length(am),
              low=prop.test(sum(am),length(am))$conf.int[1],
              upper=prop.test(sum(am),length(am))$conf.int[2])

ggplot(mt.new,aes(as.factor(cyl),y=prop,ymin=low,ymax=upper))+
  geom_bar(stat="identity")+
  geom_errorbar()


# Muertes por Trimestre
qplot(datos$TRIMPRIMSI, data=datos, fill=ESTADODELP,
      position="fill") 

# Síntomas GI
qplot(GASTRO, data=datos, fill=LETALIDAD, position="fill")


# Estado según días entre Primeros síntomas y Consulta 2
qplot(ESTADODELP_2, as.numeric(TCONSULTA), 
      data = subset(datos, !is.na(datos$ESTADODELP) & TCONSULTA >= 0 & TCONSULTA < 11), 
      geom= "boxplot")+
  labs(x="Estado del Paciente", y="Días entre 1° Síntomas y 1° Consulta",
       title="Resultado según Demora en Consultar")
# Edad
qplot(EDAD, data=datos, geom="density", fill=SEXO, alpha=I(.5))+
  labs(y="Densidad", title="Descripción Demográfica")
qplot(TCONSULTA, data=subset(datos, TCONSULTA > 0 & TCONSULTA < 11), geom = "density")

# Recuento de Blancos
qplot(ESTADODELP, RECBCOS, data = subset(datos, !is.na(datos$ESTADODELP)), geom= "boxplot", outlier.shape = NA, ylim = c(0, 50000),
      main = "Recuendo de Blancos Según Outcome", ylab= "Recuento de Blancos", xlab= "Estado del Paciente")
qplot(ESTADODELP_2, RECBCOS, data = subset(datos, !is.na(datos$ESTADODELP_2)), geom= "boxplot", outlier.shape = NA, ylim = c(0, 50000),
      main = "Recuendo de Blancos Según Outcome", ylab= "Recuento de Blancos", xlab= "Estado del Paciente")

qplot(RECBCOS, data=subset(datos, !is.na(datos$ESTADODELP_2)), geom="density", fill=ESTADODELP_2, alpha=I(.5))




qplot(reorder(ESTABLECIM, -table(ESTABLECIM)[ESTABLECIM]), data=datos.old)
fill=ESTADODELP_FV, position="fill")
