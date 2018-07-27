
## PAQUETERIAS USADAS ##
library(httr)         
library(XML)
library(rebus)
library(stringr)
library(tidyverse)
library(tesseract)
library(pdftools)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(png)

# Cambiamos el locale
Sys.setlocale("LC_ALL", 'en_US.UTF-8')

# Link de la informacion (Julio, 2018) 
link <- "http://irrigacion.chapingo.mx/posgrado-e-investigacion-tesis-licenciatura/"

# Patron para detectar los documentos `pdf` dentro del codigo html de la pagina 
patron <- ANY_CHAR %R% ".pdf" %R% ANY_CHAR

# Almacena el codigo de la pagina de internet `link` en forma de texto manipulable
cod_html = readLines(link)

# Detecta en que partes del codigo html se encuentra el patron definido 
xx <- cod_html[str_detect(cod_html, patron)]

# Generamos una charClass que detecte todos los tipos de guion para las tesis con 2 autores
guion <-char_class("-_")

# Generamos un patron de texto que capture del html todas las terminaciones de las paginas que 
# nos lleven al pdf del resumen de las tesis.

patron_autor <- "/pdf/" %R% capture(one_or_more(WRD)) %R% optional(guion) %R% optional(capture(one_or_more(WRD))) %R% ".pdf"
patron_autor_2 <- "/pdf/Tesis/2017/" %R% capture(one_or_more(WRD)) %R% optional(guion) %R% optional(capture(one_or_more(WRD))) %R% ".pdf"
patron_autor_3 <- "/pdf/Tesis/2017/" %R% capture(one_or_more(WRD)) %R% optional(guion) %R% optional(capture(one_or_more(WRD))) %R% ".pdf"

patron <- c(patron_autor, patron_autor_2, patron_autor_3)

# Convertimos a DataFrame los datos de texto donde se presente el patron previo y le ponemos un nombre sugerente :P 
xxx <- as.data.frame(xx)

# Extraemos los autores detectados por los patrones
a <- as.data.frame(as.vector(levels(as.data.frame(str_extract(as.character(xxx$xx), patron_autor)) %>% drop_na())))
b <- as.data.frame(as.vector(levels(as.data.frame(str_extract(as.character(xxx$xx), patron_autor_2)) %>% drop_na())))

# Renombramos las variables de dichos DataFrames
names(a) <- "autores"
names(b) <- "autores"

# Hacemos una base con los autores finales y desechamos las bases parciales
c <- as.data.frame(rbind(a,b))
rm(a,b)

##############
# Patrones
p_1 <- or1("palabras clave ", "palabra clave")
p_2 <- p_1 %R% capture(one_or_more(WRD %R% SPC))
p_3 <- "palabras claves " %R% capture(one_or_more(WRD %R% SPC)) 
p_4 <- "palabras clave" %R% capture(one_or_more(WRD %R% SPC)) 
pat_5 <- "palabras" %R% capture(one_or_more(WRD %R% SPC)) 
pat_6 <- "descriptores" %R% capture(one_or_more(WRD %R% SPC)) 
pattt <- "palabra" %R% capture(one_or_more(WRD %R% SPC)) 
comas <- char_class(",:.", "\n")

patron <- c(p_1, p_2, p_3, p_4, pat_5, pat_6)   # 6 patrones distintos
###############

##################################################
# Filtramos tesis que no presentan el error 404 #
# estas tesis se checaron de manera individual y efectivamente no tienen un archivo enlazado #
tesis_buenas <- c(1:nrow(c))
tesis_malas  <- c(54,63,90,93,108,111,123,165,171,190,230,255,268,270,306,318,334,338)
tesis_buenas[tesis_malas] <- NA
tesis_buenas <- na.omit(tesis_buenas)
#################################################

#########################################################################
# Obtenemos las palabras clave de las tesis normales #
todas_tesis <- c()
for(i in tesis_buenas) {
  tesis <- pdf_text(paste0("http://irrigacion.chapingo.mx/", c$autores[i]))
  tesis <- tesis %>% 
    str_remove_all(pattern = comas) %>%
    str_to_lower()
  
  todas_tesis[i] <- paste(tesis[1], tesis[2], tesis[3])
}
todas_tesis <- as.data.frame(todas_tesis)
#########################################################################

todas_tesis$patron <- str_detect(todas_tesis$todas_tesis, pattern = p_2)

tt <- todas_tesis

tt$patron <- str_detect(tt$todas_tesis, patron[2])
tt$palabras_clave <- str_extract(tt$todas_tesis, patron[2])
pal_clave <- tt$palabras_clave[tt$patron == T] 

###########
pal_clave <- as.data.frame(pal_clave) 
pal_clave <- na.omit(pal_clave)
pal_clave$spc <- str_remove(pal_clave$pal_clave, pattern = p_1)
##########

#############
# WordCloud #
#############

texto <- pal_clave$spc
texto <- sapply(texto, tolower)
text_1 <- texto
docs_1 <- Corpus(VectorSource(text_1))
#Replacing ???/???, ???@??? and ???|??? with space:
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs_1 <- tm_map(docs_1, toSpace, "/")
docs_1 <- tm_map(docs_1, toSpace, "@")
docs_1 <- tm_map(docs_1, toSpace, "\\|")

# Convert the text to lower case
docs_1 <- tm_map(docs_1, content_transformer(tolower))
# Remove numbers
docs_1 <- tm_map(docs_1, removeNumbers)
# Remove english common stopwords
# docs_1 <- tm_map(docs_1, removeWords, stopwords("spanish"))
# Remove your own stop word
# specify your stopwords as a character vector
docs_1 <- tm_map(docs_1, removeWords, c("que", "del", "los", "para", "por", "uno", "sus", "una", "las", "con", 
                                        "as??", "qu??", "fue", "ser", "fue", "han", "son", "sin", "estos", "sea", "les", "cual", "este", 
                                        "etc", "universidad", "departamento", "tipos", "tipo", "viii", "pozos")) 
# Remove punctuations
docs_1 <- tm_map(docs_1, removePunctuation)
# Eliminate extra white spaces
docs_1 <- tm_map(docs_1, stripWhitespace)
# Text stemming
# docs_1 <- tm_map(docs_1, stemDocument)

dtm <- TermDocumentMatrix(docs_1)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
#head(d, 10)

#png(file =   "/Users/admin/Desktop/Proyectos/Otros/Proyectos de Scrapping/tesises.png")
# Graficar la nube de palabras
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
#dev.off()


