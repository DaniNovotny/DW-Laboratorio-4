library(tidyverse)
# install.packages('highcharter')
library(highcharter)
library(dplyr)
library(readr)

tabla_completa <- read.csv("Lab4/tabla_completa.csv",encoding = 'latin1')

# subset(tabla_completa$CLIENTE[1], grepl("EL", tabla_completa$CLIENTE[1]))
# which(grepl("Faltante", tabla_completa$CLIENTE))
tabla_completa$FALTANTE <- 0
tabla_completa$FALTANTE[which(grepl("Faltante", tabla_completa$CLIENTE))] <- 1
tabla_completa$FALTANTE[which(grepl("FALTANTE", tabla_completa$CLIENTE))] <- 1
tabla_completa$DEVOLUCION <- 0
tabla_completa$DEVOLUCION[which(grepl("DEVOLUCION", tabla_completa$CLIENTE))] <- 1


# Nueva columna solo con los nombres de los clientes
tabla_completa$CLIENT[tabla_completa$CLIENTE == "EL PINCHE OBELISCO / Despacho a cliente"| tabla_completa$CLIENTE
                       == "EL PINCHE OBELISCO |||Faltante"] <- "EL PINCHE OBELISCO"
tabla_completa$CLIENT[tabla_completa$CLIENTE == "EL GALLO NEGRO |||DEVOLUCION"| tabla_completa$CLIENTE
                       == "EL GALLO NEGRO / Despacho a cliente"] <- "EL GALLO NEGRO"
tabla_completa$CLIENT[tabla_completa$CLIENTE == "POLLO PINULITO|||FALTANTE"| tabla_completa$CLIENTE
                       == "POLLO PINULITO/Despacho a cliente"] <- "POLLO PINULITO"
tabla_completa$CLIENT[tabla_completa$CLIENTE == "TAQUERIA EL CHINITO |||Faltante" | tabla_completa$CLIENTE == "TAQUERIA EL CHINITO"] <- "TAQUERIA EL CHINITO"
tabla_completa$CLIENT[tabla_completa$CLIENTE == "UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente"] <- "UNIVERSIDAD FRANCISCO MARROQUIN"
tabla_completa$CLIENT[tabla_completa$CLIENTE == "UBIQUO LABS |||FALTANTE" | tabla_completa$CLIENTE == "UBIQUO LABS"] <- "UBIQUO LABS"
tabla_completa$CLIENT[tabla_completa$CLIENTE == "TIENDA LA BENDICION / Despacho a cliente"] <- "TIENDA LA BENDICION"
tabla_completa$CLIENT[tabla_completa$CLIENTE == "SPORTA, S.A./Despacho a cliente |||Faltante"] <- "SPORTA, S.A."
tabla_completa$CLIENT[tabla_completa$CLIENTE == "ABARROTERIA EBENEZER/Despacho a cliente"] <- "ABARROTERIA EBENEZER"
tabla_completa$CLIENT[tabla_completa$CLIENTE == "CHICHARRONERIA EL RICO COLESTEROL |||Faltante"] <- "CHICHARRONERA EL RICO COLESTEROL"
tabla_completa$CLIENT[tabla_completa$CLIENTE == "HOSPITAL ROOSEVELT / Despacho a cliente"] <- "HOSPITAL ROOSEVELT"
tabla_completa$CLIENT[tabla_completa$CLIENTE == "BAR LA OFICINA"] <- "BAR LA OFICINA"
tabla_completa$CLIENT[tabla_completa$CLIENTE == "HOSPITAL LAS AMERICAS"] <- "HOSPITAL LAS AMERICAS"

tabla_completa$COMENTARIO <- "Despacho"
tabla_completa$COMENTARIO[tabla_completa$CLIENTE == "EL PINCHE OBELISCO |||Faltante"] <- "Faltante"
tabla_completa$COMENTARIO[tabla_completa$CLIENTE == "POLLO PINULITO|||FALTANTE"] <- "Faltante"
tabla_completa$COMENTARIO[tabla_completa$CLIENTE == "TAQUERIA EL CHINITO |||Faltante"] <- "Faltante"
tabla_completa$COMENTARIO[tabla_completa$CLIENTE == "TAQUERIA EL CHINITO |||Faltante"] <- "Faltante"
tabla_completa$COMENTARIO[tabla_completa$CLIENTE == "UBIQUO LABS |||FALTANTE"] <- "Faltante"
tabla_completa$COMENTARIO[tabla_completa$CLIENTE == "SPORTA, S.A./Despacho a cliente |||Faltante"] <- "Faltante"
tabla_completa$COMENTARIO[tabla_completa$CLIENTE == "CHICHARRONERIA EL RICO COLESTEROL |||Faltante"] <- "Faltante"
tabla_completa$COMENTARIO[tabla_completa$CLIENTE == "EL GALLO NEGRO |||DEVOLUCION"] <- "Devolucion"




# ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * ~ * 


# Comenzar a trabajar:

mean(tabla_completa$CREDITO) # muchos dias de credito
summary(tabla_completa$CREDITO)
summary(tabla_completa$CANTIDAD)
mean(tabla_completa$CREDITO)

# Clientes:

c <- tabla_completa %>% 
  select(CLIENT,CANTIDAD,Q,DEVOLUCION,FALTANTE,CREDITO) %>% 
  group_by(CLIENT) %>% 
  summarise(entrega = n(),
            cantidad = sum(CANTIDAD),
            pedido = mean(CANTIDAD),
            ventas = sum(Q),
            credito = mean(CREDITO),
            devoluciones = sum(DEVOLUCION),
            faltantes = sum(FALTANTE)) %>% 
  arrange(desc(ventas))

sum(tabla_completa$Q)
sum(tabla_completa$Q)*0.8
sum(c$ventas[1:4])

cl <- ggplot(c, aes(x = CLIENT,
                      y = entrega))
cl + geom_bar(stat = "identity",
             color = "black",
             width = 0.7) +
  theme(axis.text.x = element_text(angle = -45,hjust = 0)) +
  labs(title = "Ordenes por cliente",x = "CLIENTE")


# Pilotos:

pilotos <- tabla_completa %>% 
  group_by(PILOTO) %>%
  summarise(faltantes = sum(FALTANTE),
            viajes = n(), 
            porcentaje = faltantes/viajes) %>%
  arrange(desc(porcentaje))
  
p <- ggplot(pilotos, aes(x = reorder(PILOTO,-value), y = viajes))
p + geom_bar(stat = "identity",
             color = "black",
             width = 0.7) +
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) +
  labs(title = "Viajes por piloto",x = "Pilotos", y = "Cantidad de viajes") 

p2 <- ggplot(pilotos, aes(x = reorder(PILOTO,-faltantes), y = faltantes))
p2 + geom_bar(stat = "identity",
              color = "black",
              width = 0.7) +
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) +
  labs(title = "Faltantes por piloto",x = "Pilotos", y = "Cantidad de viajes con faltantes") 


p3 <- ggplot(pilotos, aes(x = reorder(PILOTO,-porcentaje), y = porcentaje))
p3 + geom_bar(stat = "identity",
              color = "black",
              width = 0.7) +
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) +
  labs(title = "Porcentaje de faltantes por piloto",x = "Pilotos", y = "Porcentaje de faltantes") 



# lugares que roban

robo <- tabla_completa %>% 
  select(CLIENT,CANTIDAD,Q,DEVOLUCION,FALTANTE,CREDITO) %>% 
  group_by(CLIENT) %>% 
  summarise(entrega = n(),
            faltantes = sum(FALTANTE)) 

q <- ggplot(robo, aes(CLIENT,faltantes))
q + geom_bar(stat = "identity",
             color = "black",
             width = 0.7) +
  theme(axis.text.x = element_text(angle = -45,hjust = 0)) +
  labs(title = "Clientes con los que hay faltantes",x = "CLIENTE")

# transporte

transporte <- tabla_completa %>% 
  group_by(UNIDAD) %>% 
  summarise(cantidad = n(), 
            transporte = mean(CANTIDAD)) %>% 
  arrange(desc(transporte))

t <- ggplot(transporte, aes(x = UNIDAD, y = transporte))
t + geom_bar(stat = "identity",
             color = "black",
             width = 0.7) +
  theme(axis.text.x = element_text(angle = 0,hjust = 0)) +
  labs(title = "Cantidad de transporte cada tipo de transporte",subtitle = "Capacidad en promedio",x = "Tipo de transporte", y = "Cantidad") 

tabla_completa %>% 
  group_by(UNIDAD) %>% 
  summarise(cantidad = n(), 
            transporte = mean(CANTIDAD),
            max = max(CANTIDAD),
            min = min(CANTIDAD)) %>% 
  arrange(desc(transporte))


# comprar unidades

info <- tabla_completa %>% 
  group_by(CLIENT,UNIDAD) %>% 
  summarise(VENTAS = sum(Q)) 

unidades <- pivot_wider(data = info,names_from = UNIDAD,values_from = VENTAS)

p <- ggplot(info, aes(x = CLIENT,
                      y = VENTAS,
                      fill = UNIDAD))
p + geom_bar(stat = "identity",
             color = "black",
             width = 0.7) +
  theme(axis.text.x = element_text(angle = -45,hjust = 0)) +
  labs(title = "Ordenes por cliente",x = "CLIENTE")


# por mes:

mes <- tabla_completa %>% 
  group_by(MES) %>% 
  summarise(pedidos = n(),
            cantidad = sum(CANTIDAD),
            ventas = sum(Q)) %>% 
  arrange(desc(MES))
m <- ggplot(mes, aes(x = MES, y = ventas))
m + geom_bar(stat = "identity",
             color = "black",
             width = 0.7) +
  theme(axis.text.x = element_text(angle = 0,hjust = 0)) +
  labs(title = "Ingresos por mes",x = "mes", y = "monto en Quetzales") +
  ylim(0,80000)

# Pareto Chart

df <- tabla_completa %>% 
  group_by(CLIENT) %>% 
  summarise(ventas = sum(Q)) %>% 
  arrange(desc(ventas)) %>% 
  mutate(cumsum = cumsum(ventas))

df <- cbind(df[,2],df[,1],df[,3])

# library(ggplot2)

# ggplot(df, aes(x=CLIENT)) +
#   geom_bar(aes(y=ventas), fill='blue', stat="identity") +
#   geom_point(aes(y=cumsum), color = rgb(0, 1, 0), pch=16,size=1) +
#   geom_path(aes(y=cumsum, group=1), colour="slateblue1", lty=3, size=0.9) +
#   theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
#   labs(title = "Pareto Plot",  x = 'Clientes', y = 'Ventas')

library(qcc)
tipo <- df$ventas
names(tipo) <- df$CLIENT
tipo
pareto.chart(tipo, ylab = "Ventas", ylab2 = "",
             ylim = c(0,max(tipo)),
             col = heat.colors(length(tabla)),
             main = "80/20 de Clientes")

# tabla <- table(tabla_completa$CLIENT)
# tabla
# pareto.chart(tabla, ylab = "Frecuencia", ylab2 = "",
#              ylim = NULL,
#              col = heat.colors(length(tabla)),
#              main = "80/20 de Clientes",
#              axis.text.x = element_text(angle=90, vjust=0.6))





