# ----------------------------------------------------------
# Topic modeling comentarios canales libertarios de Youtube
# Equipo Tupamaros: Diego Ceciliano Díaz, Santiago Colín, Aldo García, Sergio Guerrero, Santiago González 
# 30 de noviembre del 2025
# ----------------------------------------------------------

# ---------------- Cargar librerías ----------------

library(tidyverse)     # Limpieza y manipulación
library(openxlsx)      # Exportar Excel
library(tidytext)      # Tokenizar texto
library(readxl)        # Leer Excel
library(topicmodels)   # Modelos LDA
library(tm)            # Stopwords español
library(ggplot2)       # Gráficos estadísticos
library(wordcloud)     # Nube de palabras
library(RColorBrewer)  # Paletas de color
library(igraph)        # Redes de texto
library(ggraph)        # Graficar grafos
library(tidyr)         
library(slam)          

# ---------------- Comienza el procesamiento del Excel ----------------

# Cargar los comentarios de los videos de tres creadores
df_laje   <- read_excel("Datos/Comentarios/comentarios_videos_laje_decena.xlsx")
df_danann <- read_excel("Datos/Comentarios/comentarios_videos_danann_decena.xlsx")
df_peluca <- read_excel("Datos/Comentarios/comentarios_videos_peluca_decena.xlsx")

#Combinar en un solo data_frame
comentarios_todos <- bind_rows(
  df_laje,
  df_danann,
  df_peluca
)

# Verificar nombres de columnas
names(comentarios_todos)

# Usar la columna textOriginal
echterText <- comentarios_todos %>%
  mutate(texto = as.character(textOriginal))

# Limpieza: acentos, minúsculas, puntuación y emojis
echterText <- echterText %>%
  mutate(
    texto = str_replace_all(
      texto, c( 
        "á" = "a",
        "é" = "e",
        "í" = "i",
        "ó" = "o",
        "ú" = "u",
        "Á" = "A",
        "É" = "E",
        "Í" = "I",
        "Ó" = "O",
        "Ú" = "U",
        "ü" = "u",
        "Ü" = "U",
        "ñ" = "n",
        "Ñ" = "N"
      ))) 

# Ahora eliminamos los signos de puntuación 
echterText <- echterText %>%  
  mutate(
    texto = str_to_lower(texto), 
    texto = str_replace_all(texto, "[[:punct:]]", " "), 
    texto = str_squish(texto)
  )

# Eliminamos números que puedan generar ruido en nuestro análisis
echterText <- echterText %>%
  mutate(
    texto = str_replace_all(texto, "[0-9]+", " "),
    texto = str_squish(texto)
  )

# Eliminamos urls que sean molestas para el análisis
echterText <- echterText %>%
  mutate(
    texto = str_replace_all(texto, "https?://\\S+|www\\.\\S+", " "),
    texto = str_squish(texto)
  )

# Eliminamos terminología ligada a sitios web o enlaces de internet
echterText <- echterText %>%
  mutate(
    texto = str_remove_all(texto, "\\b(www|http|https|watch|youtube|xqedg|quot|href|com|amp|t|br|nrqw)\\b"),
    texto = str_squish(texto)
  )

# Eliminamos emojis
echterText <- echterText %>%
  mutate(
    texto = str_replace_all(texto, "[\\p{So}\\p{Cn}]", " "),
    texto = str_squish(texto)
  )

echterText <- echterText %>%
  mutate(doc_id = row_number())


# Verificar poniendo los top 10 
head(echterText$texto, 10)

# Stopwords en español
stopwords_es <- stopwords("spanish")

# Lista palabras de ruido
ruidos <- c(
  # Nombres propios comunes en los comentarios
  "jaja", "xd", "like", "video", "canal", "suscribete",
  "laje", "agustin", "adela", "micha", "milei", "javier",
  
  # Palabras comunes sin contenido relevante
  "hace", "cada", "yo", "los", "sin", "q", "era", "todos", "algo",
  "esta", "esa", "todo", "porque", "tu", "ese", "tiene", "son", "las",
  "o", "si", "mas", "solo", "v", "b", "asi", "tan", "siento", "como",
  "del", "mi", "su", "sus", "una", "para", "pero", "es", "ser", "ver", "que",
  "de", "a", "el", "la", "y", "en", "no", "me", "lo", "un", "se",
  "por", "con", "le", "al", "cuando", "te", "eso", "este", "ya",
  "donde", "cual", "quien", "fue", "sido", "estoy", "estas",
  "hay", "aca", "alla", "ahi", "entonces", "pues", "ahora", "despues",
  "antes", "luego", "siempre", "nunca", "tambien", "ni", "tus", "nuestro",
  "nuestros", "nuestra", "nuestras", "vos", "usted", "ustedes", "ellos", "ellas",
  "otro", "otros", "otra", "otras", "mismo", "misma", "mismos", "mismas",
  "tal", "tales", "tanto", "tanta", "tantos", "tantas", "mucho", "mucha",
  "muchos", "muchas", "poco", "poca", "pocos", "pocas", "muy", "menos",
  "mejor", "peor", "mayor", "menor", "gran", "grande", "pequeno",
  "bien", "mal", "aqui",
  
  # Verbos auxiliares
  "va", "voy", "vas", "van", "ir", "ido", "iba", "iban",
  "he", "has", "ha", "hemos", "han", "haber", "habido",
  "soy", "eres", "somos", "estar", "estado",
  "tengo", "tienes", "tenemos", "tienen", "tener", "tenido",
  "hago", "haces", "hacemos", "hacen", "hacer", "hecho",
  "digo", "dices", "dice", "decimos", "dicen", "decir", "dicho",
  "puedo", "puedes", "puede", "podemos", "pueden", "poder", "podido",
  "quiero", "quieres", "quiere", "queremos", "quieren", "querer", "querido",
  "doy", "das", "da", "damos", "dan", "dar", "dado",
  "veo", "ves", "vemos", "ven", "visto",
  
  # Pronombres y artículos extra
  "esto", "esos", "esas", "aquel", "aquella", "aquellos", "aquellas",
  "mio", "mia", "mios", "mias", "tuyo", "tuya", "tuyos", "tuyas",
  "suyo", "suya", "suyos", "suyas",
  
  # Conectores / preposiciones
  "sobre", "bajo", "entre", "desde", "hasta", "hacia", "mediante",
  "durante", "contra", "segun", "tras", "excepto", "salvo",
  
  # Saludos / ruido emocional
  "gracias", "excelente", "buena", "bueno", "saludos", "hola", "jajaja",
  "ojala", "porfa", "porfavor", "favor", "saludo", "abrazo", "adelante", "siga",
  "dije", "digan",
  
  # Ruido general español
  "estan", "están", "aun", "aún", "ademas", "además",
  "toda", "etc", "vez", "veces", "año", "años", "ano", "anos",
  "creo", "parece", "alguien", "deja", "encanta", "puede", "sabe",
  "parte", "tema", "temas", "hoy", "dia", "día", "tiempo", "momento",
  "cosa", "cosas", "igual", "forma", "unica", "única", "cualquier",
  "primer", "primera", "segundo", "tercero",
  
  # Noise / typos
  "fnxlzcu", "hww", "ooooila",
  
  # Proper nouns irrelevantes
  "abigail", "agus", "sra", "señora", "senora", "lucas", "lima",
  "peru", "dea", "proape",
  
  # Ruido en inglés / alemán
  "weiter", "einem", "mann",
  
  # Expresiones multi-palabra (las filtrarás en bigrams)
  "anos atrás", "abigail gracias", "abigail dios", "gracias abigail",
  "estan haciendo", "encanta escuchar", "sra abigail", "tres semanas",
  "temas abordas", "mpios lucas", "dos veces", "excelente analisis",
  "seria interesante", "buen programa", "siga adelante",
  "senora gracias", "excelente presentación", "excelente entrevista",
  "algun dia", "algun momento", "cualquier cosa",
  "hoy dia", "dio cuenta", "excelente analisis", "sigue adelante",
  "fuerte abrazo", "abrir flor", "doiorg", "doi org",
  "excelente exposición", "ningun momento", "tiempo igual",
  "dios hara", "dios hará", "unica forma", "estan quitando",
  "llevan anos", "dedo patética", "dolor oseo", "bache intente",
  "demos lugar",
  
  "hizo", "hace", "pasa", "paso", "ser", "seria", "todo", "todas",
  "claro", "debe", "deberia", "hablar", "hablo", "habla", "dije", "digo",
  "pregunta", "razon",
  
  
  
  "falta", "falta de", "pena", "vergüenza",
  "tema", "temas", 
  "totalmente", "claro", "pasa", "haciendolo", "haciendo",
  "ejemplo",
  "apoyo", "debe", "debería", "deberia",
  "sos", "sos un", "sos una",
  "quedo", "hizo", "trabajo",
  "plena", "parte", 
  "hizo", "habla", "hablar", "dijo", "dice"
)

# Tokenizar
tokens <- echterText %>%
  unnest_tokens(word, texto) %>%
  filter(!word %in% stopwords_es) %>%
  filter(!word %in% ruidos) %>%
  filter(!grepl("isimo$|isima$|isimos$|isimas$", word)) %>%
  filter(nchar(word) >= 3) %>%
  select(doc_id, word)

# Contar frecuencias
frecuencias <- tokens %>%
  count(word, sort = TRUE)

# Seleccionar top 20
top20 <- head(frecuencias, 20)
print(top20)

# Gráfico de barras
ggplot(top20, aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "#3B82F6", width = 0.7) +
  coord_flip() +
  labs(
    title = "Top 20 términos más frecuentes\nComentarios de creadores libertarios",
    x = "Término",
    y = "Frecuencia",
    caption = "Elaboración propia (Tupamaros) con datos de YouTube"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.5
    ),
    axis.title.y = element_text(
      margin = margin(r = 10)
    ),
    axis.title.x = element_text(
      margin = margin(t = 10)
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(
      size = 10,
      hjust = 1,
      color = "gray40"
    ),
    axis.text = element_text(color = "gray20")
  )

# Nube de palabras
wordcloud(
  words = frecuencias$word,
  freq = frecuencias$n,
  min.freq = 5,  # Aumenté el mínimo para reducir ruido
  max.words = 100,
  colors = brewer.pal(8, "Dark2"),
  random.order = FALSE
)

# BIGRAMAS MEJORADOS
# Crear bigramas
bigrams <- echterText %>%
  unnest_tokens(bigram, texto, token = "ngrams", n = 2)

# Separar en dos columnas
bigrams_sep <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Filtrar con criterios más estrictos
bigrams_filt <- bigrams_sep %>%
  filter(!word1 %in% stopwords_es,
         !word2 %in% stopwords_es,
         !word1 %in% ruidos,
         !word2 %in% ruidos,
         nchar(word1) >= 3,  
         nchar(word2) >= 3,
         !is.na(word1), 
         !is.na(word2))

bigram_counts <- bigrams_filt %>%
  count(word1, word2, sort = TRUE) %>%
  filter(n >= 5)  

# Ver los top bigramas
print(head(bigram_counts, 20))

# Obtener términos del top 16 para el grafo
top15_terms <- tokens %>%
  count(word, sort = TRUE) %>%
  slice_head(n = 15) %>%
  pull(word)

# Filtrar bigramas donde al menos una palabra esté en el top 15
bigram_top15 <- bigram_counts %>%
  filter(word1 %in% top15_terms | word2 %in% top15_terms) %>%
  filter(n >= 10)  

# Visualización de red de bigramas mejorada
graph <- graph_from_data_frame(bigram_top15)

# Red de bigramas 
set.seed(123)

ggraph(graph, layout = "fr") +
  geom_edge_link(
    aes(edge_alpha = n, edge_width = n),
    edge_colour = "gray75",
    show.legend = FALSE
  ) +
  geom_node_point(
    color = "#3B82F6",
    size = 5,
    alpha = 0.9
  ) +
  geom_node_text(
    aes(label = name),
    repel = TRUE,
    size = 3.5,
    color = "gray15",
    fontface = "bold"
  ) +
  theme_void() +
  labs(
    title = "Red de bigramas\nTérminos más frecuentes",
    caption = "Elaboración propia (Tupamaros) con datos de YouTube"
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.5
    ),
    plot.caption = element_text(
      size = 10,
      hjust = 1,
      color = "gray40",
      margin = margin(t = 10)
    )
  )


# Convert bigrams into single-token strings with "_"
bigrams_unidos <- bigrams_filt %>%
  mutate(bigram_token = paste(word1, word2, sep = "_")) %>%
  count(bigram_token, sort = TRUE)

# View top bigram tokens
head(bigrams_unidos, 30)


# Add bigrams as "_"-joined tokens to the existing tokens object
tokens <- tokens %>%
  bind_rows(
    bigrams_filt %>% 
      transmute(word = paste(word1, word2, sep = "_"))
  )


# Contar frecuencias nuevamente
frecuencias <- tokens %>%
  count(word, sort = TRUE)

# Seleccionar top 20 nuevamente
top20 <- head(frecuencias, 20)
print(top20)


# Nube de palabras con bigramas como tokens
wordcloud(
  words = frecuencias$word,
  freq = frecuencias$n,
  min.freq = 5,  # Aumenté el mínimo para reducir ruido
  max.words = 100,
  colors = brewer.pal(8, "Dark2"),
  random.order = FALSE
)

# ---------------- Topic models ----------------

# Crear documento-término matriz (DTM) desde los tokens limpios
dtm_data <- tokens %>%
  count(doc_id, word) %>%
  cast_dtm(doc_id, word, n)

# Eliminar documentos vacíos
row_sums <- slam::row_sums(dtm_data)
cat("Documentos vacíos iniciales:", sum(row_sums == 0), "\n")
dtm_data <- dtm_data[row_sums > 0, ]

dtm_filtered <- removeSparseTerms(dtm_data, 0.999)  
cat("Dimensiones después de filtrar:", dim(dtm_filtered), "\n")

# Eliminar documentos vacíos después del filtrado
row_sums_after <- slam::row_sums(dtm_filtered)
cat("Documentos vacíos después:", sum(row_sums_after == 0), "\n")
dtm_filtered <- dtm_filtered[row_sums_after > 0, ]

# Verificar que tenemos suficientes datos
if(nrow(dtm_filtered) < 10) {
  stop("Muy pocos documentos. Necesitas al menos 10 comentarios con contenido válido.")
}

if(ncol(dtm_filtered) < 10) {
  stop("Muy pocos términos únicos. Revisa tu lista de palabras de ruido.")
}

# Probar distintos valores de k
k_values <- 2:5
perplejidades <- c()

for (k in k_values) {
  set.seed(1234)
  modelo_temp <- LDA(
    dtm_filtered,
    k = k,
    method = "Gibbs",
    control = list(
      seed = 1234,
      burnin = 300,
      iter = 700,
      thin = 50
    )
  )
  
  loglik <- logLik(modelo_temp)
  perplejidad <- exp(-loglik / sum(dtm_filtered))
  perplejidades <- c(perplejidades, perplejidad)
}

df_perp <- data.frame(k = k_values, perplejidad = perplejidades)

# Seleccionar k mínimo
k_optimo <- df_perp$k[which.min(df_perp$perplejidad)]
cat("\n>>> K ÓPTIMO POR PERPLEJIDAD:", k_optimo, "\n")

# Reemplazar el valor final de k por el óptimo
k_topics <- k_optimo

cat("\nUsando k =", k_topics, "tópicos\n")

# Fijar semilla global
set.seed(1234)

# Configuración del modelo
lda_model <- LDA(
  dtm_filtered,
  k = k_topics,
  method = "Gibbs",
  control = list(
    seed = 1234,
    burnin = 500,      
    iter = 1000,       
    thin = 50,         
    best = TRUE,       
    verbose = 50       
  )
)


# ---------------- Extraer y analizar tópicos ----------------

# Extraer términos por tópico (beta)
terms_per_topic <- terms(lda_model, 10)  #top 10 por tópico

# Mostrar términos por tópico
for(i in 1:k_topics) {
  cat("\nTÓPICO", i, ":\n")
  cat(paste(terms_per_topic[, i], collapse = ", "), "\n")
}

# Crear vector vacío
topic_labels <- rep(NA, k_topics)

# Nombres de los tópicos
topic_labels[1] <- "Entrevistas"
topic_labels[2] <- "Gobierno argentino"
topic_labels[3] <- "Familia tradicional"
topic_labels[4] <- "Feminismo"
topic_labels[5] <- "Aborto"

topic_names <- data.frame(
  topic = 1:k_topics,
  nombre_topico = topic_labels
)

# Extraer matriz beta (probabilidades término-tópico)
beta_matrix <- posterior(lda_model)$terms

# Convertir a formato tidy manualmente
topics_beta <- data.frame()
for(i in 1:k_topics) {
  topic_data <- data.frame(
    topic = i,
    term = colnames(beta_matrix),
    beta = beta_matrix[i, ]
  )
  topics_beta <- rbind(topics_beta, topic_data)
}

# Agregar nombre del tópico
topics_beta <- topics_beta %>%
  left_join(topic_names, by = "topic") %>%
  arrange(topic, desc(beta))

# Top 10 términos por tópico
top_terms <- topics_beta %>%
  group_by(topic, nombre_topico) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, desc(beta))

# Mostrar resultados detallados
for(i in 1:k_topics) {
  cat("\n=== Tópico:", topic_labels[i], "(con probabilidades) ===\n")
  topic_terms <- top_terms %>%
    filter(topic == i) %>%
    select(term, beta)
  print(topic_terms, n = 10)
}

# Visualización de términos por tópico
lib_como_top <- top_terms %>%
  mutate(term = reorder_within(term, beta, nombre_topico)) %>%
  ggplot(aes(beta, term, fill = nombre_topico)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ nombre_topico, scales = "free") +
  scale_y_reordered() +
  labs(
    title = paste("Top 10 términos por tópico (k =", k_topics, ")"),
    x = "Beta (probabilidad del término en el tópico)",
    y = NULL
  ) +
  theme_minimal()

# Análisis de comentarios por tópico

# Extraer gamma (probabilidades documento-tópico)
gamma_matrix <- posterior(lda_model)$topics

# Convertir a formato tidy manualmente
docs_gamma <- gamma_matrix %>%
  as.data.frame() %>%
  rownames_to_column("document") %>%
  pivot_longer(
    cols = -document,
    names_to = "topic",
    values_to = "gamma"
  ) %>%
  mutate(topic = as.numeric(topic))

# Asignar cada comentario al tópico dominante
docs_topico_principal <- docs_gamma %>%
  group_by(document) %>%
  slice_max(gamma, n = 1) %>%
  ungroup() %>%
  mutate(document = as.numeric(document))

# Obtener los IDs de documentos que quedaron en dtm_filtered
docs_ids_validos <- as.numeric(rownames(dtm_filtered))

# Unir con datos originales
comentarios_con_topico <- echterText %>%
  mutate(doc_id = row_number()) %>%
  filter(doc_id %in% docs_ids_validos) %>%
  left_join(docs_topico_principal, by = c("doc_id" = "document")) %>%
  left_join(topic_names, by = "topic")

# Ver distribución de comentarios por tópico
tabla_distribucion <- comentarios_con_topico %>%
  count(topic, sort = TRUE) %>%
  left_join(topic_names, by = "topic")

print(tabla_distribucion)

# Gráfico: número de comentarios por tópico (distribución)
lib_com_k <- ggplot(tabla_distribucion, aes(x = nombre_topico, y = n, fill = nombre_topico)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = paste("Número de comentarios por tópico (k =", k_topics, ")"),
    x = "Tópico",
    y = "Número de comentarios"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

# Ver algunos ejemplos de comentarios por tópico
for(i in 1:k_topics) {
  cat("\n=== Comentarios Representativos por tópico",
      topic_labels[i], "===\n")
  
  ejemplos <- comentarios_con_topico %>%
    filter(topic == i) %>%
    arrange(desc(gamma)) %>%
    head(3) %>%
    select(texto, gamma)
  
  print(ejemplos)
}

# Heatmap de términos por tópico
topic_word_matrix <- topics_beta %>%
  group_by(topic, nombre_topico) %>%
  slice_max(beta, n = 15) %>%
  ungroup()

ggplot(topic_word_matrix, aes(x = nombre_topico, y = reorder(term, beta), fill = beta)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Heatmap: Probabilidad de términos por tópico",
    x = "Tópico",
    y = "Término",
    fill = "Beta"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

# Wordcloud por tópico
for (i in 1:k_topics) {
  cat("\n=== Wordcloud por tópico:", topic_labels[i], "===\n")
  
  terminos_topico <- topics_beta %>%
    filter(topic == i) %>%
    slice_max(order_by = beta, n = 50)
  
  wordcloud(
    words = terminos_topico$term,
    freq = terminos_topico$beta,
    min.freq = min(terminos_topico$beta),
    max.words = 50,
    random.order = FALSE,
    colors = brewer.pal(8, "Dark2")
  )
}

# Escribir Excel de comentarios con tópico
comentarios_topicos_texto_id <- comentarios_con_topico %>%
  select((ncol(.)-3):ncol(.))

write.xlsx(
  list(
    "TopTerms" = top_terms,
    "ComentariosConTopico" = comentarios_con_topico,
    "Distribucion" = tabla_distribucion
  ),
  file = "resultados_comentarios_libertarios_topicmodel.xlsx",
  overwrite = TRUE
)
