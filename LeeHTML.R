### borrado de variables rm(list = ls())  

##########################################################
######### Iniciando la extracción de información #########
##########################################################

# Usando la librería rvest
library('rvest')

# inicializando la variable archivo con el nombre de mi página
archivo <- 'index.html'

# Leyendo el HTML del archivo
webpage <- read_html(archivo)

##########################################################
############# Extracción del texto noticia ###############
##########################################################

# Extrayendo contenido en la clase justificado
contenidoWebNoticia <- html_nodes(webpage,'.justificado')

# Pasando la info a texto
textoNoticia <- html_text(contenidoWebNoticia)

# Viendo a priori la info en la variable textoNoticia
print(textoNoticia)

# Pregunta: ¿Qué representa el \n?

# Eliminando los \n,comillas("),puntos(.) y comas(,) del texto
textoNoticia <- gsub("\n","",textoNoticia)
textoNoticia <- gsub("\"","",textoNoticia)
textoNoticia <- gsub("[.]","",textoNoticia)
textoNoticia <- gsub(",","",textoNoticia)

# Viendo a priori la info en la variable textoNoticia
print(textoNoticia)

# Separando las palabras por espacio
splitEspacioNoticia <- strsplit(textoNoticia," ")[[1]]

# Pasando todas las palabras a minúsculas
splitEspacioNoticia <- tolower(splitEspacioNoticia)

# Contando palabras
unlistNoticias<-unlist(splitEspacioNoticia)
tablaPalabras<-table(unlistNoticias)

# Pasando la información a un data frame
dfPalabrasNoticia <- as.data.frame(tablaPalabras)

# Almacenando la información en CSV
write.csv(dfPalabrasNoticia, file="PalabrasNoticia.csv")

# o en un txt
write.table(dfPalabrasNoticia, file="PalabrasNoticia2.csv",sep = ";")

##########################################################
############ Extraccion información tabla ################
##########################################################

# Extrayendo los elementos que contienen las tablas
tablaProductos <- html_nodes(webpage, ".productos")

# Extraccio de el contenido de las tablas usando el tag table
contenedorDeTablas <- html_nodes(tablaProductos, "table")

# Extracción información tabla 1
tabla1 <- html_table(contenedorDeTablas[1][[1]])

# Viendo el contenido de la posición 1,2 de la tabla1
print(tabla1[1,2])

# Extracción información tabla 2
tabla2 <- html_table(contenedorDeTablas[2][[1]])

# Viendo el contenido de la posición 1,2 de la tabla2
print(tabla2[1,2])

write.table(tabla1, file="Tabla2.csv",sep = ";")

# Limpiando $ comas y cambios de puntos por coma
tabla1$Valor <- gsub("\\$","",tabla1$Valor)
tabla1$Valor <- gsub("[.]","",tabla1$Valor)
# as.numeric transforma la columna a elementos numéricos
tabla1$Valor <- as.numeric(tabla1$Valor)

# Limpiando $ comas y cambios de puntos por coma
tabla2$Valor <- gsub("\\$","",tabla2$Valor)
tabla2$Valor <- gsub("[.]","",tabla2$Valor)
# as.numeric transforma la columna a elementos numéricos
tabla2$Valor <- gsub(",",".",tabla2$Valor)
tabla2$Valor <- as.numeric(tabla2$Valor)

# Combinando los dos data frames y creando un tercer data frame
tablaMerge <- rbind(tabla1,tabla2)

# Realizando una busqueda en el dataframe
elementosEncontrados <- tablaMerge[which(tablaMerge$Supermercado == "Unimarc"), ]

# Creando una tercera columna "ProductoSupermercado" con la 
# intención de generando nombres únicos, esto es para
# graficar el valor de cada producto en cada supermercado
tablaMerge$ProductoSupermercado <- paste(tablaMerge$Producto,tablaMerge$Supermercado) 

################### Graficando los productos
library('ggplot2')

# Gráfico Barra por producto concatenado con supermercado,
# respecto al costo
tablaMerge %>%
  ggplot() +
  aes(x = ProductoSupermercado, y = Valor) +
  geom_bar(stat="identity")+
  coord_flip()

# Gráfico boxplot diferenciado por producto
tablaMerge %>%
  ggplot() +
  geom_boxplot(aes(x = Producto, y = Valor)) +
  theme_bw()

# Ejercicio guarde la tabla de productos en un CSV o txt
# cuidado con sobreescribir el archivo anterior

##########################################################
################## Usando otras fuentes ##################
##########################################################

#==================== usando xvideos ====================#

paginaXVideos <- 'https://www.xvideos.com/new/2'

webpageXVideos <- read_html(paginaXVideos)

# Extracción del texto contenido en la clase thumb-under
contenidoWebXVideos <- html_nodes(webpageXVideos,'.thumb-under > p > a')

# viendo el contenido de la posición 1
# de la variable contenidoWebXVideos
print(contenidoWebXVideos[1])

# Extrayendo el texto de contenidoWebXVideos
textoXVideos <- html_text(contenidoWebXVideos)

# Viendo que tiene la posición 1 la variable textoXVideos
print(textoXVideos[1])

# Unificando los títulos
todosLosTitulosXVideo <- ""
for(i in 1 : length(textoXVideos)){
  todosLosTitulosXVideo <- paste(todosLosTitulosXVideo," ",textoXVideos[[i]])
}

# Separando las palabras por espacio
todosLosTitulosXVideo <- strsplit(todosLosTitulosXVideo," ")[[1]]

# Pasando todas las palabras a minúsculas
todosLosTitulosXVideo <- tolower(todosLosTitulosXVideo)

# Contando palabras
unlistTitulosXVideos <- unlist(todosLosTitulosXVideo)
tablaXVideos <- table(unlistTitulosXVideos)

# Transformando a data framtabla
tituloXVideos <- as.data.frame(tablaXVideos)

#==================== usando booking.com ====================#

# 1.- Se realiza la búsqueda y se copia la URL generada
# 2.- Se asigna la url generada a la variable paginaBookingCom
paginaBookingCom <- 'https://www.booking.com/searchresults.es.html?label=gen173nr-1FCAEoggI46AdIM1gEaC-IAQGYAQq4ARnIAQ_YAQHoAQH4AQuIAgGoAgM&lang=es&sid=654096c55a6b4e739ddbb6a39eb44e43&sb=1&src=searchresults&src_elem=sb&error_url=https%3A%2F%2Fwww.booking.com%2Fsearchresults.es.html%3Flabel%3Dgen173nr-1FCAEoggI46AdIM1gEaC-IAQGYAQq4ARnIAQ_YAQHoAQH4AQuIAgGoAgM%3Bsid%3D654096c55a6b4e739ddbb6a39eb44e43%3Btmpl%3Dsearchresults%3Bclass_interval%3D1%3Bdest_id%3D6125%3Bdest_type%3Dregion%3Bdtdisc%3D0%3Bfrom_sf%3D1%3Bgroup_adults%3D2%3Bgroup_children%3D0%3Binac%3D0%3Bindex_postcard%3D0%3Blabel_click%3Dundef%3Bno_rooms%3D1%3Boffset%3D0%3Bpostcard%3D0%3Broom1%3DA%252CA%3Bsb_price_type%3Dtotal%3Bshw_aparth%3D1%3Bslp_r_match%3D0%3Bsrc%3Dindex%3Bsrc_elem%3Dsb%3Bss%3Dchiloe%3Bss_all%3D0%3Bssb%3Dempty%3Bsshis%3D0%26%3B&ss=Chilo%C3%A9&is_ski_area=0&ssne=Chilo%C3%A9&ssne_untouched=Chilo%C3%A9&dest_id=6125&dest_type=region&checkin_monthday=3&checkin_month=1&checkin_year=2019&checkout_monthday=4&checkout_month=1&checkout_year=2019&group_adults=2&group_children=0&no_rooms=1&from_sf=1'

webpage <- read_html(paginaBookingCom)

# Extracción del la info contenida en la clase sr-hotel__name
contenidoEnSrHotelName <- html_nodes(webpage,'.sr-hotel__name')

# Viendo el primer elemento de la variable contenidoEnSrHotelName
print(contenidoEnSrHotelName[1])

# Pasando la información contenida en contenidoEnSrHotelName
# a texto
textoBookingCom <- html_text(contenidoEnSrHotelName)

# viendo el elemento 1 de la variable textoBookingCom
print(textoBookingCom[1])

# Quitando los \n
textoBookingCom <- gsub("\n","",textoBookingCom)

# viendo el elemento 1 de la variable textoBookingCom
print(textoBookingCom[1])

# Extracción del la info contenida en la clase bui-review-score__badge
contenidoEnBuiReviewScoreBadge <- html_nodes(webpage,'.bui-review-score__badge')

# Viendo el primer elemento de la variable contenidoEnSrHotelName
print(contenidoEnBuiReviewScoreBadge[1])

# Pasando la información contenida en contenidoEnSrHotelName
# a texto
ratesBookingCom <- html_text(contenidoEnBuiReviewScoreBadge)

# Viendo el elemento 1 de la variable textoBookingCom
print(ratesBookingCom[1])

# Convirtiendo comas(,) en puntos(.), esto se debe a que los
# decimales son con puntos y no con comas
ratesBookingCom <- gsub(",",".",ratesBookingCom)

# Convirtiendo los rates a números
ratesBookingCom <- as.numeric(ratesBookingCom)

# Unificando la información
dfBookingCom <- data.frame(nombreHotel = textoBookingCom, rate = ratesBookingCom)

# Grafique la variable dfBookingCom
dfBookingCom %>%
  ggplot() +
  aes(x = nombreHotel, y = ratesBookingCom) +
  geom_bar(stat="identity")+
  coord_flip()

#==================== usando reclamos.cl/trasportes ====================#

for(i in 1:5){
  # Se asigna la segunda página de reclamos.cl/trasportes
  paginaReclamosCl<- paste('https://www.reclamos.cl/transportes?page=',i,sep = "")
  
  print(paginaReclamosCl)
  # Leyendo la página de transportes
  #readHtmlReclamosCl <- read_html(paginaReclamosCl)
}

# Extrayendo el contenido de la tabla que contiene todos los reclamos dispuestos en este sitio
contenidoReclamosClTable <- html_nodes(readHtmlReclamosCl,'table')

# Se extraen todos los elementos que contienen links dentro de la tabla
contenidoReclamosClA <- html_nodes(contenidoReclamosClTable,'a')

# Se extraen todos los links y se almacenan en una lista
linksReclamosCl <- html_attr(contenidoReclamosClA,"href")

# Descargue cada uno de los link y guarde su información

#####

pagina <- "https://www.tiendapet.cl/catalogo/producto/1285/orijen-orijen-six-fish-dog"

leePagina <- read_html(pagina)

marca <- html_nodes(leePagina,"[itemprop=brand]")



#==================== usando feria chilena del libro ====================#

paginaChilenaDelLibr <- "https://www.feriachilenadellibro.cl/"

paginaChilenaRead <- read_html(paginaChilenaDelLibr)

paginaChilenaNodesReferencias <- html_nodes(paginaChilenaRead,".product-item-photo")

referencias <- html_attr(paginaChilenaNodesReferencias,"href")


for(refe in referencias){
  print(refe)
  lecturaLibro <- read_html(refe)
  precio <- html_text(html_nodes(lecturaLibro,".price"))
  print(precio)
}



paste("https://www.antartica.cl","/antartica/servlet/LibroServlet?action=fichaLibro&id_libro=237004",sep = "")

