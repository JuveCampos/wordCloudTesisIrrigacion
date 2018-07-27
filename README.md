# wordCloudTesisIrrigacion

El presente código de R muestra como resultado final las palabras mas usadas en una muestra de 109 resumenes de tesis disponibles para su consulta en la página del Departamento de Irrigación de la Universidad Autónoma Chapingo. El objetivo de este trabajo es el ver que clase de temas son los más trabajados por los alumnos de licenciatura de la carrera de ingeniería en irrigación.

Para aquellos que deseen correr el codigo en sus computadoras, necesitan instalar las siguientes librerias: 


httr,XML,                 * para el manejo de paginas web *
rebus,                    * establecer patrones de texto *
stringr,                  * manejo de strings *
tidyverse,                * limpieza de bases de datos *
tesseract,                * reconocimiento OCR (aunque no lo use en este ejemplo)*
pdftools,                 * manipular archivos pdf *
"tm",                     * mineria de textos *
"SnowballC", "wordcloud", * para hacer las nubes de palabras *
"RColorBrewer",           * paletas de colores predefinidas *
png,                      * importar a imagenes png *



El trabajo esta incompleto y pienso anadir el análisis de palabras incluyendo unos 50 resumenes mas en el futuro, sin embargo,
hay que mencionar que un buen numero de tesis quedaron fuera por no incluir sus respectivas palabras clave dentro del formato
de sus resumenes (lo cual deberia ser obligatorio). Pienso continuarlo en el futuro, cuando aprenda un poco mas de R y cuando tenga tiempo y ganas para añadir estos casos especiales.






