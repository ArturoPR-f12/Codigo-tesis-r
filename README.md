# Codigo-tesis-r
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

#Descargamos la información de las acciones de interes

primera_fecha <- "2018-01-01"
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

