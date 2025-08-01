#Librerías
library(rvest)
library(dplyr)
library(BatchGetSymbols)
library(ggplot2)
library(tidyr)
library(rlang)
library(quadprog)
library(readxl)
library(ggrepel)
library(knitr)
library(RColorBrewer)
library(ggthemes)
library(xtable)
library(lubridate)
library(tidyquant)
library(foreach)
library(PortfolioAnalytics)
library(quantmod)
library(PerformanceAnalytics)
library(timetk)
library(ggcorrplot)
library(ggrepel)
library(cluster)
library(factoextra)
library(caret)

#Descargamos la información de las acciones de interes

primera_fecha <- "2022-01-01"
ultima_fecha <- "2024-01-01"

frecuencia <- "daily"

tickers <- c("AC.MX", "ALFAA.MX", "ALSEA.MX", "AMXB.MX", "ASURB.MX", "BBAJIOO.MX",
             "BIMBOA.MX", "BOLSAA.MX","CEMEXCPO.MX", "CHDRAUIB.MX", "CUERVO.MX", 
             "FEMSAUBD.MX","GAPB.MX", "GCARSOA1.MX", "GCC.MX", "GENTERA.MX", 
             "GFINBURO.MX", "GFNORTEO.MX", "GMEXICOB.MX", "GRUMAB.MX", "KIMBERA.MX", 
             "LABB.MX","LIVEPOLC-1.MX", "MEGACPO.MX", "OMAB.MX", "ORBIA.MX", 
             "PINFRA.MX", "Q.MX", "RA.MX", "TLEVISACPO.MX", "VESTA.MX", "WALMEX.MX")

descarga <- BatchGetSymbols(tickers = tickers,
                            first.date = primera_fecha,
                            last.date = ultima_fecha,
                            freq.data = frecuencia)

precios <- descarga$df.tickers

#Descargamos el IPC suponiendo que es nuestro portafolio de mercado

ticker_ipc <- "^MXX"
descarga_ipc <- BatchGetSymbols(tickers = ticker_ipc,
                                first.date = primera_fecha,
                                last.date = ultima_fecha,
                                freq.data = frecuencia)

precios_ipc <- descarga_ipc$df.tickers

df_precios_graf <- precios %>%
  dplyr::select(ref.date, price.adjusted, ticker)

names(df_precios_graf)[c(1,2)] <- c("Fecha", "Precio")

#Grafica de precios

df_precios_graf %>% ggplot() +
  geom_line(aes(x = Fecha, y = Precio, color = ticker))+
  theme_light()

#Verificación de las empresas descargadas

empresas <- precios %>% dplyr::select(ticker) %>% unique()
empresas <- empresas$ticker
empresas

#Acomodamos la información arrojada

df_precios <- precios %>%
  dplyr::select(ref.date, price.adjusted, ticker) %>%
  pivot_wider(names_from = ticker, values_from = price.adjusted)

names(df_precios)[1] <- "Fecha"

num_acciones <- length(names(df_precios)) -1
nombres_acciones <- names(df_precios)[2:(num_acciones+1)]
nombres_genericos <- paste0("accion_", 1:num_acciones)
nombres_originales <- names(df_precios)[2:(num_acciones+1)]
names(df_precios)[2:(num_acciones+1)] <- nombres_genericos

for(i in 1:num_acciones){
  columna <- as.name(paste0("accion_",i))
  vector_precios <- dplyr::pull(df_precios[columna], columna)
  log_rendimientos <- c()
  for(j in 2:length(vector_precios)){
    log_rendimientos[j] <- log(vector_precios[j]/vector_precios[j-1])
  }
  df_precios <- df_precios %>% mutate(!! as.name(paste0("rendimiento_",i)) := log_rendimientos)
}

#Guardar los log-rendimientos de cada acción por fecha y sin fecha

df_rendims_acciones <- df_precios %>% dplyr::select(Fecha, starts_with("rendimiento_")) %>%
  drop_na()

df_rendimientos <- df_precios %>% dplyr::select(starts_with("rendimiento_")) %>% drop_na()

#Guardar la correspondencia del nombre de las acciones con el nombre genérico

df_nombres <- data.frame(nombres_originales = nombres_originales,
                         nombres_genericos = nombres_genericos,
                         rendims_genericos = names(df_rendimientos))

#Obtenemos la matriz de varianzas y covarianzas de todas las acciones consideradas

matriz_var_cov <- var(df_rendimientos)
matriz_correl <- cor(df_rendimientos)

#Se obtiene el vector de rendimientos esperados en todas las acciones consideradas

vector_medias <- cbind(lapply(df_rendimientos, FUN = mean, na.rm = TRUE))

#Vector donde se alojan los rendimientos esperados de cada acción

vector_rend_esp <- as.numeric(vector_medias)

#Graficamos 

library(corrplot)
corrplot(matriz_correl, method="circle")

#Armamos un portafolio de tamaño 32

num_instrumentos_permitidos <- 32
num_total_instrumentos <- nrow(matriz_var_cov)
combinaciones <- combn(1:num_total_instrumentos,
                       num_instrumentos_permitidos)

combinaciones %>% str()

rendims_mv <- c()
varianzas_mv <- c()
activos_mv <- c()
for(i in 1:ncol(combinaciones)){
  subcjto <- combinaciones[,i]
  lista_activos <- glue::glue_collapse(subcjto, sep = ",")
  sub_esperados <- vector_rend_esp[subcjto]
  sub_matriz_varianzas <- matriz_var_cov[subcjto,subcjto]
  alpha_0 <- solve(sub_matriz_varianzas,
                   rep(1, times =nrow(sub_matriz_varianzas)))
  
  alpha_mv <- alpha_0/sum(alpha_0)
  rend_mv <- alpha_mv %*% sub_esperados
  var_mv <- alpha_mv %*% sub_matriz_varianzas %*% alpha_mv
  rendims_mv <- c(rendims_mv, rend_mv)
  varianzas_mv <- c(varianzas_mv, var_mv)
  activos_mv <- c(activos_mv, lista_activos)
}

df_mv <- data.frame(rendimiento = rendims_mv,
                    varianza = varianzas_mv,
                    componentes = activos_mv)

df_mv %>% head()

#Obtenemos el portafolio de mayor rendimiento y menor varianza posible

mejor_portafolio <- df_mv %>%
  dplyr::arrange(desc(rendimiento),varianza) %>%
  head(n=1)

mejor_portafolio

#Graficamos la frontera eficiente

mejores_activos <- mejor_portafolio$componentes %>%
  strsplit(., ",") %>%
  unlist() %>% as.numeric()
df_mejores_activos <- df_nombres[mejores_activos,]
seleccionados <- df_mejores_activos$nombres_originales

seleccionados

simbolos <- seleccionados
descarga_precios <- simbolos %>%
  tq_get(get = "stock.prices", from = "2018-01-01", to =today()) %>%
  group_by(symbol)


stock_returns_alta <- descarga_precios %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "daily",
               type = "log",
               col_rename = "returns")

stock_returns_ancha <- stock_returns_alta %>%
  spread(key = symbol, value = returns)

stock_returns_ancha %>% head(n=10)

# Se guarda el vector de medias y la matriz de covarianzas

stock_returns_ts <- stock_returns_ancha %>% tk_xts()

stock_returns_ts <-na.omit(stock_returns_ts)
rendim_medios <- colMeans(stock_returns_ts)
covMat <- cov(stock_returns_ts)
rendim_varianzas <- diag(covMat)
corrMat <- cor(stock_returns_ts)

ggcorrplot(corrMat, hc.order = TRUE, type = "lower",
           lab = TRUE) + ggtitle("Matriz de correlaciones")

#Se especifican las caracteristicas del portafolio con restricciones

portaf <- portfolio.spec(assets = colnames(stock_returns_ts))
portaf <- add.objective(portfolio = portaf, type = "risk", name = "var")
portaf <- add.constraint(portaf, type = "box", min = 0.01, max = 0.5)

#Para encontrar el portafolio con restricciones de ponderacion

library(DEoptim)

portaf_optimo_restric <- optimize.portfolio(R=stock_returns_ts,
                                            portfolio = portaf,
                                            optimize_method = "DEoptim")

ponder_optimas_restric <- portaf_optimo_restric$weights

# Portafolio de mínima varianza sin restricciones

alpha_0 <- solve(covMat, rep(1, times =nrow(covMat)))
alpha_mv <- alpha_0/sum(alpha_0)

# Rendimiento portafolio mínima varianza sin restricciones

rend_mv <- alpha_mv %*% rendim_medios
var_mv <- alpha_mv %*% covMat %*% alpha_mv
desv_est_mv <- sqrt(var_mv)

# Rendimiento portafolio mínima varianza con restricciones W>0.1

alpha_mv_rest <- ponder_optimas_restric
rend_mv_rest <- alpha_mv_rest %*% rendim_medios
var_mv_rest <- alpha_mv_rest %*% covMat %*% alpha_mv_rest
desv_est_mv_rest <- sqrt(var_mv_rest)

df_ponder_mv <- alpha_mv %>% data.frame()
names(df_ponder_mv) <- "ponderaciones_mv"
df_ponder_mv <- tibble::rownames_to_column(df_ponder_mv, "ticker")

df_ponder_mv_rest <- alpha_mv_rest %>% data.frame()
names(df_ponder_mv_rest) <- "ponderaciones_mv_rest"
df_ponder_mv_rest <- tibble::rownames_to_column(df_ponder_mv_rest, "ticker")

df_ponderaciones_mv <- left_join(df_ponder_mv,
                                 df_ponder_mv_rest, by = "ticker")

df_ponderaciones_mv

#df_ponderaciones_mv %>% xtable()
#Tabla de la media y desviación con y sin restricción

metricas_mv <- c(rend_mv, desv_est_mv)
metricas_mv_rest <- c(rend_mv_rest, desv_est_mv_rest)
tipo_portafolio <- c("Sin restricciones", "Con restricciones")

df_resumen_port <- rbind(metricas_mv,metricas_mv_rest) %>% data.frame()
row.names(df_resumen_port) <- NULL
df_resumen_port <- df_resumen_port %>% dplyr::mutate(tipo = tipo_portafolio)

df_resumen_port <- df_resumen_port %>% dplyr::select(tipo, X1, X2)
names(df_resumen_port)[2:3] <- c("media", "desv_estandar")
df_resumen_port

#Tabla de los tickers

df_resumen_acts_indiv <- rbind(rendim_medios, rendim_varianzas) %>%
  t() %>% data.frame() %>% tibble::rownames_to_column(., "tipo") %>%
  mutate(desv_estandar = sqrt(rendim_varianzas)) %>%
  dplyr::select(tipo, rendim_medios, desv_estandar)

names(df_resumen_acts_indiv)[2] <- "media"

df_resumen_acts_indiv

#Tabla de los tickers con media y desviación, y restricciones

df_resumen_con_port <- rbind(df_resumen_acts_indiv, df_resumen_port)
df_resumen_con_port

#Tabla para dibujar la frontera eficiente

inferior = min(df_resumen_con_port$media)
superior = max(df_resumen_con_port$media)
mallado <- seq(from = inferior, to = superior, length=50)
objetivo_medias <- c()
objetivo_desv <- c()
for(r_obj in mallado){
  A <- cbind(rep(1, times = length(rendim_medios)), rendim_medios)
  objeto_optimizacion <- solve.QP(Dmat = 2*covMat,
                                  dvec = rendim_medios,
                                  Amat=A, bvec=c(1, r_obj),
                                  meq=2)
  alpha_obj <- objeto_optimizacion$solution
  rend_obj <- alpha_obj %*% rendim_medios
  objetivo_medias <- c(objetivo_medias, rend_obj)
  var_obj <- alpha_obj %*% covMat %*% alpha_obj
  objetivo_desv <- c(objetivo_desv, sqrt(var_obj))
}
df_mallado <- data.frame(media = objetivo_medias,
                         desv_estandar = objetivo_desv)


df_resumen_con_port %>% ggplot() +
  geom_point(data = df_mallado, aes(x = desv_estandar, y = media),
             
             color = "blue", size = 0.5) +
  
  geom_point(aes(x = desv_estandar, y = media, color = tipo)) +
  geom_label_repel(aes(x = desv_estandar, y = media, label = tipo),
                   
                   box.padding = 0.5, point.padding = 0.6, 
                   size = 2.5, segment.color = 'grey50') +
  ggtitle("Diagrama Desviación Estándar & Media") +
  xlab("Desviación estándar") +
  ylab("Media") +
  theme_light() +
  theme(legend.position = "none")

#De la pagina de Banxico obtenemos el historico de las tasas cetes

historico_cete <- read.csv("/Users/Arturo/Desktop/Cetes_enero18-nov24.csv")

# Ajustar el rendimiento libre de riesgo para datos semanales
r_f <- ((mean(historico_cete$Valores) / 100 + 1)^(7 / 360)) - 1  # Convertir r_f diario a semanal

#r_f <- 0.0003
alpha_0 <- solve(covMat, rendim_medios - r_f)
alpha_T <- alpha_0/sum(alpha_0)
rend_T <- alpha_T %*% rendim_medios
var_T <- alpha_T %*% covMat %*% alpha_T
desv_T <- sqrt(var_T)
renglon_tangencia <- c(NA, rend_T, desv_T)
df_resumen_con_port <- rbind(df_resumen_con_port, renglon_tangencia)
df_resumen_con_port$tipo[nrow(df_resumen_con_port)] <- "Tangencia"

df_resumen_con_port

#Grafica incluyendo el portafolio de tangencia

df_resumen_con_port %>% ggplot() +
  geom_point(data = df_mallado, aes(x = desv_estandar, y = media),
             
             color = "blue", size = 0.1) +
  
  geom_point(aes(x = desv_estandar, y = media, color = tipo)) +
  geom_label_repel(aes(x = desv_estandar, y = media, label = tipo),
                   
                   box.padding = 0.5, point.padding = 0.6,
                   size = 2.5, segment.color = 'grey50') +
  ggtitle("Diagrama Desviación Estándar & Media") +
  xlab("Desviación estándar") +
  ylab("Media") +
  theme_light() +
  theme(legend.position = "none")

#Agregamos la SML

df_resumen_con_port %>% ggplot() +
  geom_point(data = df_mallado, aes(x = desv_estandar, y = media),
             
             color = "blue", size = 0.1) +
  
  geom_point(aes(x = desv_estandar, y = media, color = tipo)) +
  geom_segment(aes(x = 0, y = r_f, xend = desv_T, yend = rend_T),
               
               linetype="dotted",
               color = "brown", size=0.4) +
  
  geom_label_repel(aes(x = desv_estandar, y = media, label = tipo),
                   
                   box.padding = 0.5, point.padding = 0.6,
                   size = 2.5, segment.color = 'grey50') +
  ggtitle("Diagrama Desviación Estándar & Media") +
  xlab("Desviación estándar") +
  ylab("Media") +
  theme_light() +
  theme(legend.position = "none")


#_______________________________________________________________________________

#Regresión PCA

#Normalización de datos
preproc <- preProcess(df_rendimientos, method = c("center", "scale"))
rendimientos_norm <- predict(preproc, df_rendimientos)

#Se define la variable respuesta y predictores
Y <- rendimientos_norm$rendimiento_1
X <- rendimientos_norm %>% dplyr::select(-rendimiento_1)
df_modelo <- data.frame(Y, X)

#Se ajusta el modelo PCR con validación cruzada
set.seed(123)
modelo_pcr <- pcr(Y ~ ., data = df_modelo, scale = FALSE, validation = "CV")

#Elección del número óptimo de componentes 
rmse_cv <- RMSEP(modelo_pcr)$val["CV", 1, ]
plot(rmse_cv, type = "b", pch = 16, col = "darkblue",
     xlab = "Número de componentes", ylab = "RMSEP (CV)",
     main = "Curva de validación cruzada")

abline(v = which.min(rmse_cv), col = "red", lty = 2)
points(which.min(rmse_cv), min(rmse_cv), col = "red", pch = 19)

ncomp_optimo <- which.min(rmse_cv)

cat("Número óptimo de componentes (mínimo visual RMSEP):", ncomp_optimo, "\n")

#Separación del conjunto de entrenamiento y prueba
n <- nrow(df_rendimientos)
idx_train <- sample(1:n, size = floor(0.7 * n))  # 70% para entrenamiento
train <- df_rendimientos[idx_train, ]
test <- df_rendimientos[-idx_train, ]

#Variable respuesta simulada: rendimiento_1
y_train <- train$rendimiento_1
y_test <- test$rendimiento_1

#Matriz X de predictores (sin la variable respuesta)
X_train <- train[, -1]
X_test <- test[, -1]

#Se vuelve a ajustar el modelo con datos de entrenamiento
modelo_final <- pcr(y_train ~ ., data = X_train, scale = TRUE, validation = "CV")

#Se usa el número óptimo de componentes encontrado antes
predicciones <- predict(modelo_final, newdata = X_test, ncomp = ncomp_optimo)

#Evaluación del modelo
rmse_final <- sqrt(mean((y_test - predicciones)^2))
cat("RMSE final con", ncomp_optimo, "componentes:", rmse_final, "\n")

#Extraer los loadings del modelo
loadings_pcr <- loadings(modelo_pcr)

#Mostrar los loadings de los componentes utilizados
cat("\nPonderaciones (loadings) de los primeros", ncomp_optimo, "componentes:\n")
print(loadings_pcr[, 1:ncomp_optimo])


#_______________________________________________________________________________

#Librerías
library(pls)
library(dplyr)
library(caret)

#Usando la matriz de rendimientos: df_rendimientos

# Normalización

preproc <- preProcess(df_rendimientos, method = c("center", "scale"))
rend_norm <- predict(preproc, df_rendimientos)

# Transponer para tener una fila por activo
rend_norm_t <- t(rend_norm)  # 32 filas (activos), muchas columnas (días)

# PCA sobre activos
pca_res <- prcomp(rend_norm_t, center = TRUE, scale. = TRUE)

# Scores PCA
scores_pca <- data.frame(pca_res$x[, 1:2])  # 32 observaciones (activos)

# K-means
set.seed(123)
k <- 4
kmeans_res <- kmeans(scores_pca, centers = k, nstart = 25)

# Agregar nombres y clusters
scores_pca$cluster <- factor(kmeans_res$cluster)
scores_pca$activo <- df_nombres$nombres_originales

# Porcentaje explicado
var_exp <- summary(pca_res)$importance[2, 1:2] * 100
eje_x_label <- paste0("PC1 (", round(var_exp[1], 1), "%)")
eje_y_label <- paste0("PC2 (", round(var_exp[2], 1), "%)")

# Plot
ggplot(scores_pca, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_text_repel(aes(label = activo), size = 3, max.overlaps = 15) +
  labs(title = "Clusters K-means sobre Activos (PCA de rendimientos)",
       subtitle = paste("K =", k),
       x = eje_x_label,
       y = eje_y_label) +
  theme_minimal() +
  theme(legend.position = "right")


#Cluster 1:
#Muchas de estas empresas (ALSEA, Liverpool, Chdraui, GENTERA) dependen de cuánto 
#y cómo la gente elige gastar su dinero. No son productos esenciales, 
#o bien están atados a crédito y confianza del consumidor.

#A diferencia del clúster anterior, estas empresas sí dependen más del ritmo de 
#la economía: si baja el ingreso o hay inflación, la gente va menos al restaurante, 
#compra menos en Liverpool, o deja de pagar préstamos (GENTERA).

#BOLSAA, GENTERA y Q enfrentan riesgos financieros y regulatorios más específicos 
#(riesgo de crédito, tasas de interés, siniestros, regulaciones bursátiles), 
#lo que puede provocar rendimientos que no se comportan como los de consumo básico.



#Cluster 2:
#BBAJIOO y GFNORTEO son instituciones financieras, por lo tanto comparten 
#exposición a tasas de interés, ciclos crediticios, política monetaria, y riesgo sistémico.

#Las tres empresas responden al entorno macroeconómico nacional: inflación, 
#tasas, crecimiento económico.




#Cluster 3: 
#Son empresas que venden productos de consumo recurrente o necesario (alimentos, bebidas, higiene). 
#La gente los compra incluso en recesión.
#Muchas son típicamente etiquetadas como acciones defensivas. 
#No dependen tanto del ciclo económico, lo que las hace más estables en rendimiento.



#Cluster 4:

#Muchas de estas empresas están ligadas a sectores intensivos en capital y 
#altamente correlacionados con la actividad macroeconómica, como la construcción,
#manufactura, infraestructura, logística o minería.

#Varias de estas compañías tienen operaciones internacionales o ingresos en dólares 
#(CEMEX, GCC, ORBIA, GMEXICO), lo cual introduce sensibilidad al tipo de cambio 
#y ciclos internacionales.

#Estos sectores tienden a mostrar mayor volatilidad en sus rendimientos, 
#ya que dependen de factores como la inversión pública/privada, tasas de interés, 
#precios de materias primas, demanda externa.






