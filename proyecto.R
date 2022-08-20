# Importar librerías necesarias

#Para instalar fbRanks: 
# 1. Descárgalo del CRAN https://cran.r-project.org/src/contrib/Archive/fbRanks/fbRanks_2.0.tar.gz
# 2. Instala igraphs install.packages("igraph")
# 3. install.packages("path_absoluta_del_archivo", repos = NULL, type = "source")

install.packages("D:/ABEL/Documents/S9/fbRanks", repos = NULL, type = "source")
install.packages("plyr") #Para pasar los datos de una lista cuyos objetos tienen la misma longitud a un dataframe

library(fbRanks)
library(plyr) #Importante que se lea antes de dplyr
library(dplyr)
library(ggplot2)

# Colocar el directorio de trabajo según corresponda

setwd("D:/ABEL/Documents/S9/postwork")

# Descarga de archivos
# https://www.football-data.co.uk/spainm.php

u1011 <- "https://www.football-data.co.uk/mmz4281/1011/SP1.csv"
u1112 <- "https://www.football-data.co.uk/mmz4281/1112/SP1.csv"
u1213 <- "https://www.football-data.co.uk/mmz4281/1213/SP1.csv"
u1314 <- "https://www.football-data.co.uk/mmz4281/1314/SP1.csv"
u1415 <- "https://www.football-data.co.uk/mmz4281/1415/SP1.csv"
u1516 <- "https://www.football-data.co.uk/mmz4281/1516/SP1.csv"
u1617 <- "https://www.football-data.co.uk/mmz4281/1617/SP1.csv"
u1718 <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"
u1819 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
u1920 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"

rawData <- "D:/ABEL/Documents/S9/postwork" #Path donde se guardarán los archivos

download.file(url = u1011, destfile ="SP1-1011.csv", mode = "wb")
download.file(url = u1112, destfile ="SP1-1112.csv", mode = "wb")
download.file(url = u1213, destfile ="SP1-1213.csv", mode = "wb")
download.file(url = u1314, destfile ="SP1-1314.csv", mode = "wb")
download.file(url = u1415, destfile ="SP1-1415.csv", mode = "wb")
download.file(url = u1516, destfile ="SP1-1516.csv", mode = "wb")
download.file(url = u1617, destfile ="SP1-1617.csv", mode = "wb")
download.file(url = u1718, destfile ="SP1-1718.csv", mode = "wb")
download.file(url = u1819, destfile ="SP1-1819.csv", mode = "wb")
download.file(url = u1920, destfile ="SP1-1920.csv", mode = "wb")

# Lectura de datos

csv <- lapply(list.files(path = rawData, pattern = "*.csv"), read.csv)
class(csv)

# Renombrar columnas

names(csv) <- c("d1011", "d1112", "d1213", "d1314", "d1415", "d1516", "d1617", "d1718", "d1819", "d1920")

###### Procesamiento de datos ######
d1920 <- as.data.frame(csv[10]) #Obtener los datos de d1920 como dataframe
class(d1920); summary(d1920)

csv[10] <- NULL #Eliminar d1920 de la lista

# Seleccionar datos de interés en la lista
csv <- lapply(csv, select, Date:FTR, BbMx.2.5:BbAv.2.5.1)

# Seleccionar datos de interés en el dataframe
d1920S <- select(d1920, d1920.Date:d1920.FTR, d1920.Max.2.5:d1920.Avg.2.5.1)
d1920S <- select(d1920S, -d1920.Time)

# Renombrar columnas de d1920S
d1920S <- rename(d1920S, Date = d1920.Date, HomeTeam = d1920.HomeTeam, AwayTeam = d1920.AwayTeam,
                 FTHG = d1920.FTHG, FTAG = d1920.FTAG, FTR = d1920.FTR, Max.2.5.O = d1920.Max.2.5,
                 Max.2.5.U = d1920.Max.2.5.1, Avg.2.5.O = d1920.Avg.2.5, Avg.2.5.U = d1920.Avg.2.5.1)
# Terminamos de depurar los datos de d1920S

# Continuamos, ahora, depurando d1019S
# Mutar la lista a un dataframe con todos sus datos
d1019S <- ldply (csv, data.frame) #Se crea una columna .id #Función de la paquetería plyr
class(d1019S); summary (d1019S)

d1019S <- d1019S[,-1] #Eliminar la columna .id
summary (d1019S)
# Terminamos de depurar los datos de d1019S

# Pasamos las fechas de tipo character a tipo fecha
d1019S <- mutate(d1019S, Date = as.Date(Date, format = "%d/%m/%y"))
d1920S <- mutate(d1920S, Date = as.Date(Date, format = "%d/%m/%Y"))

summary(d1019S); summary(d1920S) #Última verificación de los datos

# Renombrar columnas de d1019S
d1019S <- dplyr::rename(d1019S,  Max.2.5.O = BbMx.2.5, 
                                  Avg.2.5.O = BbAv.2.5, 
                                  Max.2.5.U = BbMx.2.5.1,
                                  Avg.2.5.U = BbAv.2.5.1)

# Ordenamos las columnas

d1019S <- select(d1019S, colnames(d1920S))

# Unión de todos los datos

d1020S <- rbind(d1019S, d1920S)
head(d1020S); tail(d1020S) #Para verificar que abarca la información de 2010 a 2020

# Renombramos

d1020S <- rename(d1020S, date = Date, home.team = HomeTeam, home.score = FTHG, away.team = AwayTeam, away.score = FTAG)

# Ordenamos columnas

#Added FTR
data <- select(d1020S, date, home.team, home.score, away.team, away.score, FTR:Avg.2.5.U) # Este data frame contiene todos los datos necesarios

head(data, n = 2L); tail(data, n = 2L)

# Data frames de partidos y equipos

#Added FTR
md <- data %>% select(date:FTR)
write.csv(md, "match.data.csv", row.names = FALSE)
df <- create.fbRanks.dataframes(scores.file = "match.data.csv")
teams <- df$teams; scores <- df$scores

head(teams, n = 2L); dim(teams); head(scores, n = 2L); dim(scores)

# Conjuntos iniciales de entrenamiento y de prueba

f <- scores$date # Fechas de partidos
fu <- unique(f) # Fechas sin repetición
Ym <- format(fu, "%Y-%m") # Meses y años
Ym <- unique(Ym) # Meses y años sin repetir
places <- which(Ym[15]==format(scores$date, "%Y-%m")) # Consideramos partidos de 15 meses para comenzar a ajustar el modelo
ffe <- scores$date[max(places)] # Fecha final conjunto de entrenamiento

# Consideraremos partidos de 15 meses para comenzar a ajustar el modelo. Así, nuestro primer conjunto de entrenamiento consiste de datos de partidos hasta el `r ffe` 

train <- scores %>% filter(date <= ffe)
test <- scores %>% filter(date > ffe)

head(train, n = 1); tail(train, n = 1)
head(test, n = 1); tail(test, n = 1)

# Primer ajuste del modelo

traindate <- unique(train$date)
testdate <- unique(test$date)

ranks <- rank.teams(scores = scores, teams = teams, 
                    min.date = traindate[1], 
                    max.date = traindate[length(traindate)])

# Primera predicción

pred <- predict(ranks, date = testdate[1])

phs <- pred$scores$pred.home.score # predicted home score
pas <- pred$scores$pred.away.score # predicted away score
pht <- pred$scores$home.team # home team in predictions
pat <- pred$scores$away.team # away team in predictions

# Continuar ajustando y prediciendo

phs <- NULL; pas <- NULL; pht <- NULL; pat <- NULL
for(i in 1:(length(unique(scores$date))-170)){
  ranks <- rank.teams(scores = scores, teams = teams, 
                      min.date = unique(scores$date)[i], 
                      max.date = unique(scores$date)[i+170-1], 
                      silent = TRUE,
                      time.weight.eta = 0.0005)
  pred <- predict(ranks, date = unique(scores$date)[i+170],
                  silent = TRUE)
  
  phs <- c(phs, pred$scores$pred.home.score) # predicted home score
  pas <- c(pas, pred$scores$pred.away.score) # predicted away score
  pht <- c(pht, pred$scores$home.team) # home team in predictions
  pat <- c(pat, pred$scores$away.team) # away team in predictions
}

# Eliminamos NA's

buenos <- !(is.na(phs) | is.na(pas)) # 
phs <- phs[buenos] # predicted home score
pas <- pas[buenos] # predicted away score
pht <- pht[buenos] # home team in predictions
pat <- pat[buenos] # away team in predictions
momio <- data %>% filter(date >= unique(scores$date)[171]) # momios conjunto de prueba
momio <- momio[buenos,]
mean(pht == momio$home.team); mean(pat == momio$away.team)
mean(phs + pas > 2.5 & momio$home.score + momio$away.score > 2.5)
mean(phs + pas < 2.5 & momio$home.score + momio$away.score < 2.5)
hs <- momio$home.score
as <- momio$away.score

# Probabilidades condicionales

mean(phs + pas > 3) # proporción de partidos con más de tres goles según el modelo
mean(phs + pas > 3 & hs + as > 2.5)/mean(phs + pas > 3) 
# probabilidad condicional estimada de ganar en over 2.5
mean(phs + pas < 2.1) # proporción de partidos con menos de 2.1 goles según el modelo
mean(phs + pas < 2.1 & hs + as < 2.5)/mean(phs + pas < 2.1) 
# probabilidad condicional estimada de ganar en under 2.5

# Juegos con momios máximos

cap <- 50000; g <- NULL

for(j in 1:length(phs)){
  if(((phs[j] + pas[j]) > 3) & (0.64/(momio$Max.2.5.O[j]^-1) > 1)){
    if((hs[j] + as[j]) > 2.5) cap <- cap + 1000*(momio$Max.2.5.O[j]-1)
    else cap <- cap - 1000
    g <- c(g, cap)
  }
  
  if(((phs[j] + pas[j]) < 2.1) & (0.58/(momio$Max.2.5.U[j]^-1) > 1)){
    if((hs[j] + as[j]) < 2.5) cap <- cap + 1000*(momio$Max.2.5.U[j]-1)
    else cap <- cap - 1000
    g <- c(g, cap)
  }
}

# Escenario con momios máximos

g <- data.frame(Num_Ap = 1:length(g), Capital = g)
p <- ggplot(g, aes(x=Num_Ap, y=Capital)) + geom_line( color="purple") + geom_point() +
  labs(x = "Número de juego", 
       y = "Capital",
       title = "Realizando una secuencia de juegos") +
  theme(plot.title = element_text(size=12))  +
  theme(axis.text.x = element_text(face = "bold", color="blue" , size = 10, angle = 25, hjust = 1),
        axis.text.y = element_text(face = "bold", color="blue" , size = 10, angle = 25, hjust = 1))  # color, ángulo y estilo de las abcisas y ordenadas 
p

# Escenario con momios promedio

cap <- 50000; g <- NULL

for(j in 1:length(phs)){
  if(((phs[j] + pas[j]) > 3) & (0.64/(momio$Avg.2.5.O[j]^-1) > 1)){
    if((hs[j] + as[j]) > 2.5) cap <- cap + 1000*(momio$Avg.2.5.O[j]-1)
    else cap <- cap - 1000
    g <- c(g, cap)
  }
  
  if(((phs[j] + pas[j]) < 2.1) & (0.58/(momio$Avg.2.5.U[j]^-1) > 1)){
    if((hs[j] + as[j]) < 2.5) cap <- cap + 1000*(momio$Avg.2.5.U[j]-1)
    else cap <- cap - 1000
    g <- c(g, cap)
  }
}

g <- data.frame(Num_Ap = 1:length(g), Capital = g)
p <- ggplot(g, aes(x=Num_Ap, y=Capital)) + geom_line( color="purple") + geom_point() +
  labs(x = "Número de juego", 
       y = "Capital",
       title = "Realizando una secuencia de juegos") +
  theme(plot.title = element_text(size=12))  +
  theme(axis.text.x = element_text(face = "bold", color="blue" , size = 10, angle = 25, hjust = 1),
        axis.text.y = element_text(face = "bold", color="blue" , size = 10, angle = 25, hjust = 1))  # color, ángulo y estilo de las abcisas y ordenadas 
p
