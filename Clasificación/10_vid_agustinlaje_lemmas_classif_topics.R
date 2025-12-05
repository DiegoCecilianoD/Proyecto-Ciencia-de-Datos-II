library(rvest)
library(openxlsx)
library(tidyverse)
library(httr)
library(jsonlite)
library(ggimage)
library(inegiR)
library(spotifyr)
library(tuber)
library(ellmer)
library(tidytext)       # Análisis de texto
library(readxl)         # Leer archivos Excel
library(topicmodels)    # Modelos LDA
library(tm)             # Text mining
library(ggplot2) 
library(udpipe)
library(SnowballC)
library(widyr)
library(stringr)
library(stringi)
library(wordcloud)
library(RColorBrewer)
library(randomForest)
library(caret)
library(e1071)
library(xgboost)
library(nnet)



# --- 1. Parámetros ----
archivo <- "comentarios_videos_laje_decena.xlsx"
columna_texto <- "textOriginal"
total_comentarios <- Inf

# --- 2. Cargar y limpiar ----
comentarios <- readxl::read_xlsx(archivo) %>%
  head(total_comentarios) %>%
  rename(texto = !!sym(columna_texto)) %>%
  mutate(
    texto = stri_trans_general(texto, "Latin-ASCII"), # elimina acentos
    texto = str_to_lower(texto),
    texto = str_replace_all(texto, "[[:punct:]]", " "),
    texto = str_replace_all(texto, "[0-9]+", " "),
    texto = str_replace_all(texto, "https?://\\S+|www\\.\\S+", " "),
    texto = str_remove_all(texto, "\\b(www|http|https|watch|youtube|xqedg|quot|href|com|amp|t|br|nrqw)\\b"),
    texto = str_replace_all(texto, "[\\p{So}\\p{Cn}]", " "), # emojis
    texto = str_squish(texto)
  ) %>%
  mutate(ID = 1:nrow(.)) %>%
  relocate(ID, .before = texto)

# --- 3. Modelo UDPipe ----
modelo <- udpipe_download_model(language = "spanish")
udmodel <- udpipe_load_model(modelo$file_model)

# --- 4. Anotación y lematización ----
anotado <- udpipe_annotate(udmodel, x = comentarios$texto, doc_id = comentarios$ID)
anotado <- as_tibble(anotado) %>%
  select(doc_id, token, lemma, upos) %>%
  mutate(lemma = stri_trans_general(lemma, "Latin-ASCII")) %>%  # asegúrate de quitar acentos de nuevo
  filter(!is.na(lemma))

# --- 5. Stopwords y ruidos ----
stopwords_es <- stopwords("es")

ruidos <- c(
  # Nombres propios comunes en los comentarios
  "jaja", "xd", "like", "video", "canal", "suscribete", 
  "laje", "agustin", "adela", "micha", "milei", "javier",
  
  # Palabras muy comunes sin contenido relevante
  "hace", "cada", "yo", "los", "sin", "q", "era", "todos", "algo", 
  "esta", "esa", "todo", "porque", "tu", "ese", "tiene", "son", "las", 
  "o", "si", "mas", "solo", "v", "b", "asi", "tan", "siento", "como", 
  "del", "mi", "su", "sus", "una", "para", "pero", "es", "ser", "ver", "que", 
  "de", "a", "el", "la", "y", "en", "no", "me", "lo", "un", "se", 
  "los", "por", "con", "le", "al", "cuando", "te", "eso", "este", "ya",
  "donde", "cual", "quien", "quien", "fue", "sido", "estoy", "estas",
  "hay", "aca", "alla", "ahi", "entonces", "pues", "ahora", "despues",
  "antes", "luego", "siempre", "nunca", "tambien", "ni", "tus", "nuestro",
  "nuestros", "nuestra", "nuestras", "vos", "usted", "ustedes", "ellos", "ellas",
  "otro", "otros", "otra", "otras", "mismo", "misma", "mismos", "mismas",
  "tal", "tales", "tanto", "tanta", "tantos", "tantas", "mucho", "mucha", 
  "muchos", "muchas", "poco", "poca", "pocos", "pocas", "muy", "mas",
  "menos", "mejor", "peor", "mayor", "menor", "gran", "grande", "pequeno",
  "bien", "mal", "ahi", "alla", "aqui", "aca",
  
  # Verbos auxiliares y comunes
  "va", "voy", "vas", "van", "ir", "ido", "iba", "iban",
  "he", "has", "ha", "hemos", "han", "haber", "habido",
  "soy", "eres", "somos", "son", "estar", "estado",
  "tengo", "tienes", "tenemos", "tienen", "tener", "tenido",
  "hago", "haces", "hace", "hacemos", "hacen", "hacer", "hecho",
  "digo", "dices", "dice", "decimos", "dicen", "decir", "dicho",
  "puedo", "puedes", "puede", "podemos", "pueden", "poder", "podido",
  "quiero", "quieres", "quiere", "queremos", "quieren", "querer", "querido",
  "doy", "das", "da", "damos", "dan", "dar", "dado",
  "veo", "ves", "vemos", "ven", "visto",
  
  # Pronombres y artículos adicionales
  "esto", "esos", "esas", "aquel", "aquella", "aquellos", "aquellas",
  "mio", "mia", "mios", "mias", "tuyo", "tuya", "tuyos", "tuyas",
  "suyo", "suya", "suyos", "suyas",
  
  # Conectores y preposiciones
  "sobre", "bajo", "entre", "desde", "hasta", "hacia", "mediante",
  "durante", "contra", "segun", "sin", "tras", "excepto", "salvo"
)

# --- 6. Filtrar lemmas útiles ----
tokens <- anotado %>%
  filter(!lemma %in% stopwords_es) %>%
  filter(!lemma %in% ruidos) %>%
  filter(!grepl("isimo$|isima$|isimos$|isimas$", lemma)) %>%
  filter(nchar(lemma) >= 3)

# --- 7. Contar y visualizar ----
frecuencias <- tokens %>%
  count(lemma, sort = TRUE)

top20 <- head(frecuencias, 20)
print(top20)

# Gráfico de barras
ggplot(top20, aes(x = reorder(lemma, n), y = n)) +
  geom_col(fill = "#e60073") +
  coord_flip() +
  labs(title = "Top 20 lemas más frecuentes - Comentarios Laje",
       x = "Lema", y = "Frecuencia") +
  theme_minimal()

# Nube de palabras
set.seed(123)
wordcloud(
  words = frecuencias$lemma,
  freq = frecuencias$n,
  min.freq = 5,
  max.words = 100,
  colors = brewer.pal(8, "Dark2"),
  random.order = FALSE
)



###### GUARDADO DE COMENTARIOS LIMPIOS#####



# Guardar el resultado en un Excel
openxlsx::write.xlsx(
  comentarios %>% select(ID, texto),
  file = "comentarios_limpios_decena_videos_laje.xlsx",
  asTable = TRUE
)



#Clasificación con Ollama 

library(httr2)
library(jsonlite)



comentarios_clasif_llama_laje <- readxl::read_xlsx("comentarios_limpios_decena_videos_laje.xlsx") %>% 
  distinct(ID, texto) %>% 
  drop_na(texto)


evaluar <- function(comentario){
  
  prompt_text <- str_c(
    "¿Considerarías que el siguiente comentario tiene un discurso anti-feminista o patriarcal?
    Anti-feminista incluye: negar que existe discriminación de género estructural, usar 'ideología de género' como crítica, decir que el feminismo busca privilegios, cuestionar que los femicidios son violencia machista, atacar el lenguaje inclusivo, rechazar la identidad de género autopercibida, o celebrar el cierre de instituciones de género.
    Feminista incluye: reconocer el patriarcado como problema, defender políticas de género, visibilizar violencia machista, apoyar movimientos como Ni Una Menos, defender la Ley de Identidad de Género, o mencionar brecha salarial como discriminación.
    Este comentario estaba en el video de un YouTuber argentino reconocido. Solo menciona si es 'Anti-feminista' o 'Feminista'. No des explicaciones adicionales. No uses símbolos de puntuación.
    
    Comentario: '", comentario, "'"
  )
  
  resp <- request("http://localhost:11434/api/generate") |>
    req_body_json(list(
      model = "qwen2.5:1.5b",
      prompt = prompt_text,
      stream = FALSE
    )) |>
    req_perform()
  
  result <- resp_body_json(resp)
  return(result$response)
}


bolsa <- tibble()
archivo_salida <- paste0("clasificacion_feminismo_llama_", Sys.Date(), ".xlsx")

for(c in seq_along(comentarios_clasif_llama_laje$texto)){
  tibble_respuesta <- tibble(
    ID = comentarios_clasif_llama_laje$ID[c],
    texto = comentarios_clasif_llama_laje$texto[c],
    Clasificacion = evaluar(comentarios_clasif_llama_laje$texto[c])
  )
  
  bolsa <- rbind(bolsa, tibble_respuesta)
  print(str_c("Listo: ", c, " / ", nrow(comentarios_clasif_llama_laje)))
  
  
  if(c %% 5 == 0 || c == nrow(comentarios_clasif_llama_laje)){
    openxlsx::write.xlsx(bolsa, archivo_salida)
    print(str_c("Progreso guardado en ", archivo_salida))
  }
}




##### Modelos de Clasificación (prueba sin Clasificacion de totalidad de comentarios)#####





comentarios_modelo_forest <- readxl::read_xlsx("clasificacion_feminismo_llama_laje_test.xlsx") %>%
  rename(id = ID,
         textOriginal = texto,
         clasificacion = Clasificacion)


stop_words <- tibble(palabras = tm::stopwords("spanish"))


comentarios_modelo_forest_token_uni <- comentarios_modelo_forest %>%
  unnest_tokens(output = "palabras", input = textOriginal, token = "words") %>%
  filter(!(palabras %in% c("amp", "br", "href", "https", "www.youtube.com"))) %>%
  filter(!str_detect(palabras, "\\d")) %>%
  anti_join(stop_words) %>%
  group_by(id, clasificacion, palabras) %>%
  count() %>%
  ungroup()


comentarios_modelo_forest_token_bi <- comentarios_modelo_forest %>%
  unnest_tokens(output = "palabras", input = textOriginal, token = "ngrams", n = 2) %>%
  separate(palabras, into = c("p1", "p2"), sep = "\\s", remove = FALSE) %>%
  filter(!is.na(p1) & !is.na(p2)) %>%
  filter(!(p1 %in% stop_words$palabras) & !(p2 %in% stop_words$palabras)) %>%
  filter(!str_detect(p1, "\\d") & !str_detect(p2, "\\d")) %>%
  select(id, clasificacion, palabras) %>%
  group_by(id, clasificacion, palabras) %>%
  count() %>%
  ungroup()


comentarios_modelo_forest_token <- bind_rows(comentarios_modelo_forest_token_uni,
                                             comentarios_modelo_forest_token_bi) %>%
  bind_tf_idf(term = palabras, document = id, n = n) %>%
  arrange(id, -tf_idf)

cat("Total de términos únicos (unigramas + bigramas):",
    length(unique(comentarios_modelo_forest_token$palabras)), "\n\n")


matriz_dtm <- comentarios_modelo_forest_token %>%
  cast_dtm(document = id,
           term = palabras,
           value = n,
           weighting = tm::weightTfIdf) %>%
  removeSparseTerms(sparse = 0.95)

cat("Dimensiones de la matriz DTM:", dim(matriz_dtm), "\n\n")


X <- as_tibble(as.matrix(matriz_dtm)) %>%
  mutate(across(everything(), ~replace(., is.na(.) | is.nan(.) | is.infinite(.), 0)))

categorias <- comentarios_modelo_forest_token %>%
  select(id, clasificacion) %>%
  distinct() %>%
  arrange(id)

y <- factor(categorias$clasificacion, levels = unique(categorias$clasificacion))


set.seed(666)
indices_train <- createDataPartition(y, p = 0.7, list = FALSE)

X_train <- X[indices_train, ]      
y_train <- y[indices_train]        
X_test  <- X[-indices_train, ]     
y_test  <- y[-indices_train]       



cat("Train:", nrow(X_train), "\nTest:", nrow(X_test),
    "\nFeatures:", ncol(X_train), "\n")
print(table(y_train))
print(table(y_test))


mdl_rf <- randomForest(x = X_train, y = y_train, ntree = 100)


pred_rf <- predict(mdl_rf, X_test)
print(confusionMatrix(pred_rf, y_test))
