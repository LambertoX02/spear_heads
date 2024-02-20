nuevo_dir <- "C:/Ejercicios/spear_heads"
setwd(nuevo_dir)

if (file.exists(nuevo_dir)) {
  cat("Directorio creado correctamente: ", nuevo_dir, "\n")
} else {
  cat("Fallo al crear directorio: ", nuevo_dir, "\n")
}

install.packages("readxl")
library(readxl)

#1
#Encontramos la ubicación del excel.
spear = read_excel("C:/Ejercicios/spear_heads/spearheads.xlsx")
View(spear)
str(spear)

#El comando -class- nos da información sobre el tipo.
class(spear)

#Convertimos en data frame la tabla excel.
spear <- as.data.frame(spear)

#2
names(spear)[names(spear) == "Mat"] <- "Materiales"
names(spear)[names(spear) == "Con"] <- "Contexto"
names(spear)[names(spear) == "Cond"] <- "Conservación"
names(spear)[names(spear) == "Loo"] <- "Loop"
names(spear)[names(spear) == "Peg"] <- "Remache"
names(spear)[names(spear) == "Date"] <- "Fecha"
names(spear)[names(spear) == "Maxle"] <- "Longitud_max"
names(spear)[names(spear) == "Socle"] <- "Longitud_encaje"
names(spear)[names(spear) == "Maxwi"] <- "Ancho_max"
names(spear)[names(spear) == "Upsoc"] <- "Ancho_encaje"
names(spear)[names(spear) == "Maxwit"] <- "Ancho_max_encaje"
names(spear)[names(spear) == "Weight"] <- "Peso"
View(spear)

#3
spear$Contexto=factor(spear$Contexto, levels=c('1','2','3'), labels = c('S/C', 'Habitacional', 'Funerario'))
spear$Conservación=factor(spear$Conservación, levels=c('1','2','3','4'), labels = c('Excelente', 'Bueno', 'Regular', 'Malo'))
spear$Remache=factor(spear$Remache, levels=c('1','2'), labels = c('Sí', 'No'))
spear$Materiales=factor(spear$Materiales, levels=c('1','2'), labels = c('Bronce', 'Hierro'))
View(spear)

#4
freq.mat = table(spear$Materiales)
freq.con = table(spear$Contexto)
freq.cond = table(spear$Conservación)

View(freq.mat)
View(freq.con)
View(freq.cond)

#5
cross.condcon = table(spear$Materiales,spear$Contexto)
cross.condmat = table(spear$Materiales,spear$Conservación)
View(cross.condcon)
View(cross.condmat)

#6
prop.mat = prop.table(freq.mat)*100
prop.con = prop.table(freq.con)*100
prop.cond = prop.table(freq.cond)*100

View(prop.mat)
View(prop.con)
View(prop.cond)

#7
prop.cross.propcond = round(prop.table(cross.condcon)*100)
prop.cross.propmat = round(prop.table(cross.condmat)*100)
View(prop.cross.propcond)
View(prop.cross.propmat)

#8
barras_ver_cond = barplot(freq.cond, 
                          main = "Frecuencia de Conservación",
                          xlab = "Grado de Conservación",
                          ylab = "Porcentaje",
                          col = "blue")

barras_ver_con = barplot(freq.con, 
                          main = "Frecuencia de Contexto",
                          xlab = "Grado de Contexto",
                          ylab = "Porcentaje",
                          col = "yellow")

#9
barras_hor_mat = barplot(freq.mat, horiz = TRUE,
                         main = "Frecuencia del Material",
                         xlab = "Porcentaje",
                         ylab = "Grado del Material",
                         col = "green")


freq.rem = table(spear$Remache)
barras_hor_rem = barplot(freq.rem, horiz = TRUE,
                         main = "Frecuencia del Remache",
                         xlab = "Porcentaje",
                         ylab = "Grado del Remache",
                         col = "red")

#10
barplot(cross.condmat,
        beside = TRUE,
        main = "Frecuencia de la conservación sobrem el material",
        xlab = "Grado de conservación",
        ylab = "Porcentaje",
        col = c("black","white"),
        legend = rownames(cross.condmat))

#11
pie(prop.cond,
    main = "Distribución sectorial de los grados de conservación",
    col = c("green","blue","yellow","red"),
    labels = paste(names(prop.cond),"(", (prop.cond),"%)"))

#12
variables_continuas = spear[sapply(spear, is.numeric)]
windows(width = 10, height = 10)

hist_prob = hist(unlist(variables_continuas), 
                 main = "Histograma de Probabilidad de Variables Continuas",
                 xlab = "Valores",
                 col = "pink",
                 prob = TRUE)