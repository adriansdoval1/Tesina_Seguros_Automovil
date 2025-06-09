### Fórmulas y gráficas Auxiliares
## 3.1 Árbol de Regresión

# Cargar librerías
library(rpart)
library(rpart.plot)
library(ggplot2)

# Cargar datos y renombrar variables
datos <- mtcars
names(datos)[names(datos) == "wt"] <- "Peso"
names(datos)[names(datos) == "hp"] <- "Caballos de Fuerza"

# Crear el modelo de árbol de regresión
modelo <- rpart(mpg ~ Peso + `Caballos de Fuerza`, data = datos, method = "anova")

# Graficar el árbol con nombres legibles
rpart.plot(modelo, main = "Árbol de regresión: Millas por Galón de Gasolina", type = 3, extra = 101)


# Crear una cuadrícula para predecir
wt_seq <- seq(min(datos$Peso), max(datos$Peso), length.out = 200)
hp_seq <- seq(min(datos$`Caballos de Fuerza`), max(datos$`Caballos de Fuerza`), length.out = 200)
grid <- expand.grid(Peso = wt_seq, `Caballos de Fuerza` = hp_seq)

# Predecir mpg en cada punto de la cuadrícula
grid$pred <- predict(modelo, newdata = grid)

# Graficar
ggplot() +
  geom_contour(data = grid, aes(x = Peso, y = `Caballos de Fuerza`, z = pred), color = "black") +
  geom_point(data = datos , aes(x = Peso, y = `Caballos de Fuerza`), color = "blue", size = 2) +
  labs(
    title = "Regiones de decisión de un árbol de regresión (sin relleno)",
    x = "Peso del auto",
    y = "Caballos de fuerza"
  ) +
  theme_minimal()


## Diagrama

library(DiagrammeR)




grViz("
digraph xgboost_process {
  
  graph [layout = dot, rankdir = TB]
  node [shape = rectangle, style = filled, fillcolor = lightblue, fontname = Helvetica, fontsize = 12]

  datos        [label = '📦 Datos de Entrada\n(Ej: Variables Independientes y Variables Dependiente)']
  arbol1       [label = '🌳 Árbol 1\nPredicción inicial']
  residuales1  [label = '🔁 Calcular Residuales\n Árbol 1']
  arbol2       [label = '🌳 Árbol 2\nResiduales']
  residuales2  [label = '🔁 Calcular Residuales\n Árbol 2']
  arbol3       [label = '🌳 Árbol 3\nResiduales']
  residuales3  [label = '🔁 Calcular Residuales\n Árbol 3']
  suma         [label = '➕ Suma Ponderada de Predicciones']
  pred_final   [label = '✅ Predicción final\nModelo optimizado']

  datos -> arbol1
  datos -> arbol2
  datos -> arbol3
  arbol1 -> residuales1
  residuales1 -> suma
  residuales1 -> arbol2
  arbol2 -> residuales2
  residuales2 -> suma
  residuales2 -> arbol3
  arbol3 -> residuales3 
  residuales3 -> suma
  suma -> pred_final
  
      { rank = same; arbol1;arbol2 }
      { rank = same; arbol1;arbol3 }
}
")



# Cargar paquetes necesarios
library(xgboost)
library(ggplot2)
library(dplyr)

# Usar mtcars: predeciremos 'mpg' (consumo) usando el resto como predictores
data(mtcars)

# Convertir a matriz y separar variables
X <- as.matrix(mtcars[,c(4, 6)])  # Predictores (sin mpg)
y <- mtcars$mpg               # Variable respuesta

# Crear estructura de datos para xgboost
dtrain <- xgb.DMatrix(data = X, label = y)


# Entrenar un modelo simple
modelo <- xgboost(
  data = dtrain,
  objective = "reg:squarederror",  # Para regresión
  nrounds = 50,                    # Número de iteraciones
  verbose = 0                      # Silencia la salida
)



# Obtener la importancia de las variables
importancia <- xgb.importance(model = modelo)

# Mostrar tabla (opcional)
print(importancia)

# Graficar importancia
xgb.plot.importance(importancia, top_n = 10)


## Quantile Plot

library(data.table)
library(ggplot2)

# Simular datos
set.seed(123)
n <- 1000
dt <- data.table(
  exposure = runif(n, 0.5, 1.5),
  predicted = runif(n, 100, 1000)  # valor estimado
)

# Generar valor real con algo de variación respecto al estimado
dt[, actual := predicted * rlnorm(n, meanlog = 0, sdlog = 0.3)]

# Calcular prima pura real y estimada
dt[, actual_pure := actual / exposure]
dt[, predicted_pure := predicted / exposure]

# Crear deciles (10 cuantiles) basados en la prima estimada
dt[, quantile := cut(predicted_pure,
                     breaks = quantile(predicted_pure, probs = seq(0, 1, 0.2)),
                     include.lowest = TRUE, labels = FALSE)]

# Calcular promedio por cuantil
summary_dt <- dt[, .(
  promedio_real = mean(actual_pure),
  promedio_estimado = mean(predicted_pure)
), by = quantile]

# Formato largo para ggplot
plot_dt <- melt(summary_dt, id.vars = "quantile",
                variable.name = "tipo", value.name = "prima_pura")

# Graficar
ggplot(plot_dt, aes(x = quantile, y = prima_pura, color = tipo)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Gráfico de Cuantiles: Valor Real vs Estimado",
       x = "Cuantil (según prima estimada)",
       y = "Prima pura promedio",
       color = "Tipo de valor") +
  scale_color_manual(values = c("promedio_real" = "red", "promedio_estimado" = "blue"),
                     labels = c("Valor Real", "Valor Estimado")) +
  theme_minimal()

library(ggplot2)
library(cowplot)

# Two example plots
p1 <- ggplot(mtcars, aes(wt, mpg)) + 
  geom_point() + 
  ggtitle("Plot 1")

p2 <- ggplot(mtcars, aes(hp, mpg)) + 
  geom_point() + 
  ggtitle("Plot 2")

# Create a title as a ggdraw() object
title <- ggdraw() + 
  draw_label("Overall Title for the Grid of Plots", fontface = 'bold', size = 16, x = 0.5, hjust = 0.5) 

# Combine title and plots
plot_grid(title,
          plot_grid(p1, p2, labels = c("A", "B")),  # inner grid
          ncol = 1,
          rel_heights = c(0.1, 1))  # title takes 10% of the height


library(data.table)
library(ggplot2)
library(cowplot)

# Simular datos
set.seed(123)
n <- 1000
dt <- data.table(
  exposure = runif(n, 0.5, 1.5),
  model_a = runif(n, 100, 1000),
  model_b = runif(n, 100, 1000)
)

# Valor real generado con base en model_a (pero con ruido)
dt[, actual := model_a * rlnorm(n, meanlog = 0, sdlog = 0.3)]
dt[, actual_pure := actual / exposure]
dt[, model_a_pure := model_a / exposure]
dt[, model_b_pure := model_b / exposure]

# FUNCION para crear gráfico por modelo
cuantiles_plot <- function(dt, col_pred, col_real = "actual_pure", exposure = "exposure") {
  # Crear cuantiles
  dt[, quantile := cut(get(col_pred),
                       breaks = quantile(get(col_pred), probs = seq(0, 1, 0.1), na.rm = TRUE),
                       include.lowest = TRUE, labels = FALSE)]
  
  # Calcular promedio por cuantil
  summary_dt <- dt[, .(
    promedio_real = mean(get(col_real)),
    promedio_estimado = mean(get(col_pred))
  ), by = quantile]
  
  # Calcular lift
  lift_val <- summary_dt[quantile == 10, promedio_real] - summary_dt[quantile == 1, promedio_real]
  
  # Dar formato largo para ggplot
  plot_dt <- melt(summary_dt, id.vars = "quantile",
                  variable.name = "tipo", value.name = "prima_pura")
  
  # Gráfico
  p <- ggplot(plot_dt, aes(x = quantile, y = prima_pura, color = tipo)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    scale_color_manual(values = c("promedio_real" = "red", "promedio_estimado" = "blue"),
                       labels = c("Valor Real", "Valor Estimado")) +
    labs(x = "Cuantil (según prima estimada)",
         y = "Prima pura promedio",
         color = "Tipo",
         subtitle = paste("Lift:", round(lift_val, 2))) +
    theme_minimal()
  
  return(p)
}

# Crear los dos gráficos
p1 <- cuantiles_plot(dt, "model_a_pure") + ggtitle("Modelo A")
p2 <- cuantiles_plot(dt, "model_b_pure") + ggtitle("Modelo B")

# Crear título general
title <- ggdraw() + 
  draw_label("Gráfico de Cuantiles: Comparación de Modelos",
             fontface = 'bold', size = 16, x = 0.5, hjust = 0.5) 
#+  theme(plot.margin = margin(0, 0, 10, 0))

# Juntar título y gráficos
plot_grid(title,
          plot_grid(p1, p2, labels = c("A", "B")),
          ncol = 1,
          rel_heights = c(0.1, 1))




library(data.table)
library(ggplot2)
library(cowplot)

# Simular datos
set.seed(123)
n <- 1000
dt <- data.table(
  exposure = runif(n, 0.5, 1.5),
  model_a = runif(n, 100, 1000),
  model_b = runif(n, 100, 1000),
  model_c = runif(n, 100, 1000)
)

# Valor real basado en model_a + ruido
dt[, actual := model_a * rlnorm(n, meanlog = 0, sdlog = 0.3)]
dt[, actual_pure := actual / exposure]
dt[, model_a_pure := model_a / exposure]
dt[, model_b_pure := model_b / exposure]
dt[, model_c_pure := model_c / exposure]

# Función para gráfico de cuantiles
cuantiles_plot <- function(dt, col_pred, show_legend = TRUE) {
  dt[, quantile := cut(get(col_pred),
                       breaks = quantile(get(col_pred), probs = seq(0, 1, 0.1), na.rm = TRUE),
                       include.lowest = TRUE, labels = FALSE)]
  
  summary_dt <- dt[, .(
    promedio_real = mean(actual_pure),
    promedio_estimado = mean(get(col_pred))
  ), by = quantile]
  
  lift_val <- summary_dt[quantile == 10, promedio_real] - summary_dt[quantile == 1, promedio_real]
  
  plot_dt <- melt(summary_dt, id.vars = "quantile",
                  variable.name = "tipo", value.name = "prima_pura")
  
  p <- ggplot(plot_dt, aes(x = quantile, y = prima_pura, color = tipo)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    scale_color_manual(values = c("promedio_real" = "red", "promedio_estimado" = "blue"),
                       labels = c("Valor Real", "Valor Estimado")) +
    labs(x = "Cuantil", y = "Prima pura promedio",
         subtitle = paste("Lift:", round(lift_val, 2))) +
    theme_minimal(base_size = 12) +
    theme(legend.position = if (show_legend) "bottom" else "none")
  
  return(p)
}

# Crear los 3 gráficos
p1 <- cuantiles_plot(dt, "model_a_pure", show_legend = FALSE) + ggtitle("Modelo A")
p2 <- cuantiles_plot(dt, "model_b_pure", show_legend = FALSE) + ggtitle("Modelo B")
p3 <- cuantiles_plot(dt, "model_c_pure", show_legend = TRUE) + ggtitle("Modelo C")

# Extraer leyenda desde p3
legend <- get_legend(p3)

# Quitar leyenda de p3 para graficar sin repetición
p3 <- p3 + theme(legend.position = "none")

# Título general
title <- ggdraw() + 
  draw_label("Gráfico de Cuantiles: Comparación de 3 Modelos",
             fontface = 'bold', size = 16, x = 0.5, hjust = 0.5) 
 #+ theme(plot.margin = margin(0, 0, 10, 0))

# Juntar todo
full_plot <- plot_grid(
  title,
  plot_grid(p1, p2, p3, ncol = 3, labels = c("A", "B", "C")),
  legend,
  ncol = 1,
  rel_heights = c(0.1, 1, 0.08)
)

print(full_plot)



library(ggplot2)
library(cowplot)

# Ejemplo de dos gráficos con márgenes reducidos
grafico_1 <- ggplot(mtcars, aes(mpg, wt)) +
  geom_point() +
  ggtitle("Gráfico 1") 

grafico_2 <- ggplot(mtcars, aes(hp, qsec)) +
  geom_point() +
  ggtitle("Gráfico 2") 
 

# Combinar con rel_widths y sin espacio visual extra
plot_grid(
  grafico_1,
  grafico_2,
  nrow = 1,
  align = "v",
  rel_widths = c(0.1, 0.1),       # mismo ancho
  axis = "b"                  # comparte eje inferior si es útil
)


library(ggplot2)
library(cowplot)

# Crear tres gráficos de ejemplo
grafico_1 <- ggplot(mtcars, aes(mpg, wt)) + geom_point() + ggtitle("Gráfico 1")
grafico_2 <- ggplot(mtcars, aes(hp, qsec)) + geom_point() + ggtitle("Gráfico 2")
grafico_3 <- ggplot(mtcars, aes(drat, disp)) + geom_point() + ggtitle("Gráfico 3")

# Crear un espacio vacío para equilibrar la grilla
espacio_vacio <- plot_spacer()

# Usar plot_grid con 2 columnas y 2 filas
final_plot <- plot_grid(
  grafico_1, grafico_2,
  grafico_3, espacio_vacio,
  ncol = 2,
  align = "hv"
)

# Mostrar
final_plot


library(ggplot2)

p1 <- ggplot(mtcars) + geom_point(aes(mpg, disp)) + theme(plot.margin = unit(c(0,30,0,0), "pt"))
p2 <- ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear))

#p1 + plot_spacer() + p2


# To have more control over spacing, you can use the `plot.margin`
# parameter for `theme()` on each individual plot.

(p1 + theme(plot.margin = unit(c(0,30,0,0), "pt"))) +
  (p2 + theme(plot.margin = unit(c(0,0,0,30), "pt")))




library(ggplot2)
library(cowplot)
library(patchwork)

# Crear tres gráficos de ejemplo
grafico_A <- ggplot(mtcars, aes(mpg, wt)) +
  geom_point() +
  ggtitle("Gráfico A")

grafico_B <- ggplot(mtcars, aes(hp, qsec)) +
  geom_point() +
  ggtitle("Gráfico B")

grafico_C <- ggplot(mtcars, aes(drat, disp)) +
  geom_point() +
  ggtitle("Gráfico C")

# Gráfico centrado abajo usando plot_spacer para balancear
plot_grid(
  grafico_A, grafico_B,
  plot_spacer(), grafico_C,
  ncol = 2,
  rel_heights = c(1, 1),    # misma altura para filas
  align = "hv"
)


library(ggplot2)
library(cowplot)

# Gráficos de ejemplo
grafico_1 <- ggplot(mtcars, aes(mpg, wt)) + geom_point() + ggtitle("Gráfico A")
grafico_2 <- ggplot(mtcars, aes(hp, qsec)) + geom_point() + ggtitle("Gráfico B")
grafico_3 <- ggplot(mtcars, aes(drat, disp)) + geom_point() + ggtitle("Gráfico C")

# Fila superior: dos gráficos
fila_superior <- plot_grid(grafico_1, grafico_2, ncol = 2)

# Fila inferior: gráfica centrada (entre dos espacios vacíos)
fila_inferior <- plot_grid(
  plot_spacer()+ theme_nothing(), grafico_3, plot_spacer()+ theme_nothing(),
  ncol = 3,
  rel_widths = c(1, 2, 1)  # C entra al centro, más grande, centrada perfectamente
)

# Juntar ambas filas
plot_grid(
  fila_superior,
  fila_inferior,
  ncol = 1,
  rel_heights = c(1, 1)
)

##########

library(xgboost)
library(DiagrammeR)
library(dplyr)

data(mtcars)

# Usaremos 'mpg' como variable dependiente
X <- as.matrix(mtcars[c(1, 4, 6)] %>% select(-mpg))
y <- mtcars$mpg

# Convertimos a DMatrix
dtrain <- xgb.DMatrix(data = X, label = y)

params <- list(
  objective = "reg:squarederror",  # Regresión
  max_depth = 2,
  eta = 0.1
)

modelo_xgb <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 3
)

xgb.plot.tree(model = modelo_xgb, trees = 1)
xgb.plot.tree(model = modelo_xgb, trees = 2)

####
library(xgboost)
library(DiagrammeR)

# Datos y modelo
data(mtcars)
X <- as.matrix(mtcars[, -1])
y <- mtcars$mpg
dtrain <- xgb.DMatrix(data = X, label = y)

params <- list(objective = "reg:squarederror", eta = 0.1, max_depth = 3)
model <- xgb.train(params, dtrain, nrounds = 20)

# Importancia de variables
importance_matrix <- xgb.importance(feature_names = colnames(X), model = model)

# Normalizamos el Gain para escalas de color/tamaño (0-1)
norm_gain <- importance_matrix$Gain / max(importance_matrix$Gain)

# Creamos nodos con tamaño proporcional a la importancia
nodes_code <- ""
y_pos <- 0
for(i in seq_len(nrow(importance_matrix))) {
  size <- 10 + 30 * norm_gain[i]  # tamaño nodo de 10 a 40
  color_intensity <- round(255 - 200 * norm_gain[i])  # color va de claro a oscuro
  color <- sprintf("#%02X%02X%02X", 255, color_intensity, color_intensity)  # rojo a rosa
  
  nodes_code <- paste0(nodes_code,
                       sprintf("node%d [label=\"%s\\n%.3f\" width=%.2f height=0.5 style=filled fillcolor=\"%s\" shape=box fixedsize=true pos=\"0,%.1f!\"]\n",
                               i, importance_matrix$Feature[i], importance_matrix$Gain[i], size/10, color, -y_pos))
  
  y_pos <- y_pos + 1.5
}

# Grafo con sólo nodos, alineados verticalmente (usando pos con rankdir=LR para simular barra horizontal)
graph_code <- paste0("
graph importance {
  graph [rankdir=LR splines=false nodesep=0.3 ranksep=0.5]
  node [fontname=Arial fontsize=10 fixedsize=true]
  
  ", nodes_code, "
}
")

# Mostrar el gráfico
grViz(graph_code)

####

modelo <- lm(mpg ~ wt + hp, data = mtcars)

# Muestra todas las gráficas diagnósticas
par(mfrow = c(2, 2))
plot(modelo)


######

library(xgboost)
library(rpart)
library(dplyr)
library(knitr)
library(data.table)

# Cargar datos
data(mtcars)

# Seleccionar predictores y variable objetivo
X <- as.matrix(mtcars[, c("wt", "hp")])  # Predictores
y <- mtcars$mpg                          # Respuesta

# Crear objeto DMatrix para XGBoost
dtrain <- xgb.DMatrix(data = X, label = y)

modelo_xgb <- xgboost(
  data = dtrain,
  objective = "reg:squarederror",
  nrounds = 10,
  verbose = 0,
  eta = 0.5, 
  max_depth = 1,
  gamma = 0
)

# Predicciones con XGBoost
pred_xgb <- predict(modelo_xgb, X)

# Convertir a data.frame para rpart
df <- mtcars[, c("mpg", "wt", "hp")]

# Ajustar modelo rpart
modelo_rpart <- rpart(mpg ~ wt + hp, data = df)

# Predicciones con rpart
pred_rpart <- predict(modelo_rpart, newdata = df)


# Crear tabla de comparación
resultados <- data.frame(
  Real = y,
  XGBoost = round(pred_xgb, 2),
  RPart = round(pred_rpart, 2)
)

# Mostrar tabla
kable(resultados, caption = "Comparación de predicciones: XGBoost vs rpart")



































