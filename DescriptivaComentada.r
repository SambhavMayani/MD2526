# ------------------------------------------------------------------
# PARTE 1: CONFIGURACIÓN Y CARGA DE DATOS 📂
# ------------------------------------------------------------------

# Estos comentarios iniciales son notas del programador original.
# READING CREDSCO.CSV. NOTE: Change the path of the file for the proper one in your computer
# Note: Take care to use "/" fo the directory file. "\" provides errors

# setwd: "Set Working Directory". Le dice a R en qué carpeta de tu ordenador debe buscar los archivos.
# Debes cambiar esta ruta a la ubicación donde guardaste tu archivo CSV.
setwd("C:/Users/sambh/Desktop/UNI/4-Q1/MD/MD")

# read.table: Lee los datos de un archivo de texto y los carga en R.
# "Myocardial_infarction_complications_Database.csv": Es el nombre del archivo.
# header=T: Significa que la primera fila del archivo contiene los nombres de las columnas (T = TRUE).
# sep=",": Indica que los valores en el archivo están separados por comas.
# dd <- ... : Guarda la tabla de datos leída en una variable llamada 'dd'.
dd <- read.table("Myocardial_infarction_complications_Database.csv", header=T, sep=",");

# ------------------------------------------------------------------
# PARTE 2: INSPECCIÓN INICIAL DE LOS DATOS 🧐
# ------------------------------------------------------------------

# class(dd): Pregunta a R "¿Qué tipo de objeto es 'dd'?". Debería responder 'data.frame', que es la tabla de datos de R.
class(dd)

# dim(dd): Muestra las dimensiones de la tabla: [número de filas, número de columnas].
dim(dd)

# n <- dim(dd)[1]: Guarda el número de filas (el primer elemento de 'dim') en una variable 'n'.
n <- dim(dd)[1]
# Muestra el valor de 'n' en la consola.
n

# K <- dim(dd)[2]: Guarda el número de columnas (el segundo elemento de 'dim') en una variable 'K'.
K <- dim(dd)[2]
# Muestra el valor de 'K' en la consola.
K

# names(dd): Muestra una lista con los nombres de todas las columnas de la tabla 'dd'.
names(dd)

# ------------------------------------------------------------------
# PARTE 3: ANÁLISIS EXPLORATORIO (EJEMPLO CON UNA VARIABLE) 📊
# ------------------------------------------------------------------

# hist(): Crea un histograma para ver la distribución de una variable numérica.
# dd[,14]: Selecciona la columna número 14.
hist(dd[,2])

# dd$Age: Es otra forma de seleccionar una columna, usando su nombre. Es más claro.
hist(dd$AGE)

# boxplot(): Crea un diagrama de caja, útil para ver la mediana, cuartiles y valores atípicos.
# main=...: Añade un título al gráfico.
boxplot(dd[,2], main=paste("Boxplot of", names(dd)[2]))

# horizontal=TRUE: Dibuja el diagrama de caja en horizontal en lugar de vertical.
boxplot(dd[,2], horizontal=TRUE, main=paste("Boxplot of", names(dd)[2]))

# summary(): Muestra un resumen estadístico de una variable (mínimo, 1er cuartil, mediana, media, 3er cuartil, máximo).
summary(dd[,2])

# sd(): Calcula la desviación estándar (mide la dispersión de los datos).
sd(dd$Age)

# cv: Calcula el coeficiente de variación (desviación estándar / media). Sirve para comparar la dispersión entre variables.
cv <- sd(dd$Age) / mean(dd$Age)
cv

# summary(dd): Aplica la función summary() a TODAS las columnas de la tabla 'dd'. Es una vista general muy útil.
summary(dd)

# Pasa tu histograma a escala de densidad (freq=FALSE)
hist(dd$AGE, freq=FALSE, main="Histograma y Curva Normal para Edad", xlab="Edad") 

# Añade una curva de densidad normal (linea azul)
curve(dnorm(x, mean=mean(dd$AGE), sd=sd(dd$AGE)), 
      add=TRUE, col="blue", lwd=2)

qqnorm(dd$AGE, main = "QQ Plot of the Age") # Crea el gráfico Q-Q
qqline(dd$AGE, col="red") # Añade la línea de referencia normal

shapiro.test(dd$AGE)


# attach(dd): Permite usar los nombres de las columnas como si fueran variables normales, sin tener que escribir "dd$" antes.
# Por ejemplo, ahora puedes escribir 'Age' en lugar de 'dd$Age'.
# ¡CUIDADO! Puede causar confusión si tienes variables con el mismo nombre. A muchos expertos no les gusta usarlo.
attach(dd)

# Ahora que 'dd' está "attached", estas líneas hacen lo mismo que las anteriores pero con menos código.
summary(AGE)
hist(AGE)
boxplot(AGE, horizontal=TRUE, main=paste("Boxplot of", names(dd)[14]))

# ------------------------------------------------------------------
# PARTE 4: CONVERTIR Y ETIQUETAR VARIABLES CATEGÓRICAS 🏷️
# ------------------------------------------------------------------

# sapply(dd, class): Revisa el tipo de dato ('class') de cada columna en 'dd'. Útil para ver qué necesita ser convertido.
sapply(dd, class)

# as.factor() / factor(): Convierte una columna a tipo "factor". Un factor es cómo R entiende las variables categóricas.
# Por ejemplo, una columna con números 1, 2, 3 que representan "Soltero", "Casado", "Divorciado".
# Dictamen <- as.factor(Dictamen)
SEX <- factor(SEX, 
              levels= c(0,1), 
              labels = c("Female", "Male"))
# Estado.civil <- factor(Estado.civil)
# Registros <- factor(Registros)
# Tipo.trabajo <- factor(Tipo.trabajo)

# class(Dictamen): Comprueba que la conversión ha funcionado y ahora la variable es de tipo 'factor'.
# class(Dictamen)

# levels(): Muestra las categorías (niveles) que contiene una variable factor.
# levels(Dictamen)
# levels(Vivienda)

# table(): Cuenta cuántas veces aparece cada categoría en la variable.
# barplot(): Crea un gráfico de barras con esas cuentas.
barplot(table(SEX))

# pie(): Crea un gráfico circular (de pastel) con esas cuentas.
pie(table(SEX))
# pie(table(Vivienda))

# Ahora, vamos a poner etiquetas descriptivas a las categorías (que ahora son solo números).
# levels(...) <- c(...): Asigna nuevos nombres a los niveles del factor. El orden es CRÍTICO.
# La primera etiqueta de la lista ("positiu") reemplazará al primer nivel existente, la segunda al segundo, etc.
# levels(Dictamen) <- c(NA, "positiu", "negatiu") # Asigna etiquetas a los niveles de 'Dictamen'.
# levels(Vivienda) <- c("VivUnkown", "lloguer", "escriptura", "contr_privat", "ignora_cont", "pares", "altres viv")
# levels(Estado.civil) <- c("ECUnknown", "solter", "casat", "vidu", "separat", "divorciat")
# levels(Registros) <- c("reg_no", "reg_si")

# Esta es una forma más controlada de etiquetar, especificando qué número corresponde a qué etiqueta.
# Tipo.trabajo <- factor(Tipo.trabajo, levels=c("1", "2", "3", "4", "0"), labels=c("fixe", "temporal", "autonom", "altres sit", "WorkingTypeUnknown"))

# A veces las categorías tienen un orden lógico (ej: "malo" < "regular" < "bueno").
# ordered=TRUE: Le dice a R que el orden de los niveles importa.
# levels=c(...): Especifica cuál es ese orden.
# Tipo.trabajo <- factor(Tipo.trabajo, ordered=TRUE, levels=c("WorkingTypeUnknown", "temporal", "fixe", "autonom", "altres sit"))

# frecs <- table(...): Guarda las frecuencias en una variable para reutilizarlas.
frecs <- table(SEX)
# barplot con opciones extra: las=3 (etiquetas del eje X en vertical), cex.names (tamaño de las etiquetas).
barplot(frecs, las=3, cex.names=0.7, main=paste("Barplot of", "SEX"))

# ------------------------------------------------------------------
# PARTE 5: ACTUALIZAR LA TABLA Y GUARDAR EL TRABAJO 💾
# ------------------------------------------------------------------

# Ahora que hemos limpiado y etiquetado las variables, las volvemos a meter en la tabla original 'dd'.
# Reemplazamos la columna 1 original por la nueva variable 'Dictamen' (que ahora es un factor con etiquetas).
# dd[,1] <- Dictamen
# dd[,3] <- Vivienda
# dd[,6] <- Estado.civil
# dd[,7] <- Registros
# dd[,8] <- Tipo.trabajo

# Comprobamos que el cambio se ha hecho correctamente en la tabla 'dd'.
# class(dd[,1])

# View(dd): Abre la tabla 'dd' completa en una nueva pestaña de RStudio, como si fuera una hoja de cálculo. Muy útil para inspeccionar.
# View(dd)

# Volvemos a mirar el resumen, pero ahora de la tabla 'dd' ya modificada. Las variables categóricas se mostrarán como conteos.
# summary(dd)

# write.table(): Guarda la tabla 'dd' modificada en un nuevo archivo CSV.
# file = "credscoCategoriques.csv": Nombre del archivo de salida.
# sep = ";": Usa punto y coma como separador.
# na = "NA": Cómo escribir los valores perdidos (missing values).
# row.names = FALSE: No guarda los números de fila.
# col.names = TRUE: Sí guarda la fila con los nombres de las columnas.
# write.table(dd, file = "credscoCategoriques.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)

# ------------------------------------------------------------------
# PARTE 6: TÉCNICAS ADICIONALES Y EXPORTACIÓN DE GRÁFICOS/RESULTADOS 🚀
# ------------------------------------------------------------------

# Esta es una forma más eficiente de convertir varias columnas a factor a la vez.
# dcat <- c(...): Crea un vector con los números de las columnas categóricas.

# dcat <- c(1, 2, 3, 6, 7, 8)

# lapply(...): Aplica la función 'factor' a cada una de las columnas seleccionadas en 'dcat'.

# dd[, dcat] <- lapply(dd[, dcat], factor)

# par(ask=TRUE): Pone R en un modo interactivo. Antes de mostrar el siguiente gráfico, te preguntará en la consola.

# par(ask=TRUE)

# Este bucle 'for' recorre una lista de variables numéricas ('varNum', que no está definida en este script) y crea un histograma para cada una.
# for(k in varNum){hist(dd[,k], main=paste("Histogram of", names(dd)[k]))}
# par(ask=FALSE): Desactiva el modo interactivo.

# par(ask=FALSE)

# Este bloque guarda los gráficos directamente en un archivo PDF en lugar de mostrarlos en RStudio.
# pdf("..."): Crea y abre un archivo PDF. Todos los gráficos a partir de ahora se guardarán ahí.
# pdf("outputs/informeDescripCredsCo.pdf")
# El mismo bucle 'for' de antes para generar los histogramas.
# for(k in varNum){hist(dd[,k], main=paste("Histogram of", names(dd)[k]))}
# dev.off(): Cierra el archivo PDF, guardando todo.
# dev.off()

# Este bloque guarda la salida de texto de la consola directamente en un archivo .doc.
# sink("..."): Redirige toda la salida de texto a un archivo.
# sink("outputs/informeDescripCredsCo.doc")
# El resultado de 'names(dd)' ahora se escribirá en el archivo, no en la consola.
# names(dd)
# sink(): Detiene la redirección y la salida vuelve a la consola.
# sink()

# El código que está fuera de los bloques 'pdf()' o 'sink()' se ejecuta normalmente.
# names(dd)

# ------------------------------------------------------------------
# CÓDIGO FINAL Y REPETIDO (limpieza del script original)
# ------------------------------------------------------------------
# Las siguientes líneas son en su mayoría repeticiones o comprobaciones que ya se hicieron antes.
# Probablemente son restos de cuando el programador original estaba explorando los datos.

# Guarda de nuevo la tabla modificada (esta línea es redundante si ya se ejecutó antes).

# write.table(dd, file = "credscoCategoriques.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
