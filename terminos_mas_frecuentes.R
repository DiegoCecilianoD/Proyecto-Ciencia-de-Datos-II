# ----------------------------------------------------------
# Términos más frecuentes creadores analizados
# Equipo Tupamaros: Diego Ceciliano Díaz, Santiago Colín, Aldo García, Sergio Guerrero, Santiago González 
# 30 de noviembre del 2025
# ----------------------------------------------------------

library(tidyverse)
library(tidytext)
library(SnowballC)
library(udpipe)
library(readxl)
library(tm)
library(stringr)
library(dplyr)

# Cargar datos
comentarios_videos_gelatina <- read_excel("Datos/Comentarios/comentarios_youtube_gelatina_decena.xlsx")
comentarios_videos_cenital <- read_excel("Datos/Comentarios/comentarios_youtube_cenitalgrande_decena.xlsx")
comentarios_youtube_futurorockfm <- read_excel("Datos/Comentarios/comentarios_youtube_futurorockanrolleros_decena.xlsx")
comentarios_videos_laje <- read_excel("Datos/Comentarios/comentarios_videos_laje_decena.xlsx")
comentarios_videos_peluca <- read_excel("Datos/Comentarios/comentarios_videos_peluca_decena.xlsx")
comentarios_videos_danann <- read_excel("Datos/Comentarios/comentarios_videos_danann_decena.xlsx")

comentarios_todos <- bind_rows(
  comentarios_videos_gelatina,
  comentarios_videos_cenital,
  comentarios_youtube_futurorockfm,
  comentarios_videos_laje,
  comentarios_videos_peluca,
  comentarios_videos_danann
)

# Verificar nombres de columnas
names(comentarios_todos)


# Usar la columna textOriginal
echterText <- comentarios_todos %>%
  mutate(texto = as.character(texto))

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


# Ahora agrupamos términos

ruidos <- c(
  # Nombres propios comunes en los comentarios
  "jaja", "xd", "like", "video", "canal", "suscribete",
  "laje", "agustin", "ofelia", "fernandez", "milei", "javier",
  "julia", "Rosemberg", "gustavo", "cordera", "pedro", "rosemblat",
  "kirchner", "cristina", "nestor", "ari", "lijalad", "juan", "manuel",
  "karina", "miley", "ramos", "mejia", "mirta", "legran", "santos", "vargas",
  "santiago", "caputo", "jose", "luis", "taylor", "swift", "patricia", "burrich",
  "gelatina",
  
  
  
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
  "hizo", "habla", "hablar", "dijo", "dice", "digamos", "habia","tipo", "manera", "tenes", "mira",
  "dos", "recien","lado", "fijate", "tenia","llama", "nueva", "dos", "incluso", "traves", 
  "sino", "nacio", "partir", "ningun", "dentro", "lugar", "dio", "miren", "decia", "algun", "viene",
  "alguna", "nadie", "medio", "casa", "respecto", "importante", 
  "quisieras_colaborar", "maria_julia", "efectivamente", "supuesto", "ayer",
  "hacerlo_uniendote", "importantes_librerias", "pudieras_colaborar", "campanita_musica",
  "patreon_siguiendo", "hijos", "sabes", "cuenta", 
  "patreon_siguiendo", "hacerlo_uniendote", "comentario_suscribiendote", "sabes", "buenos_dias",
  "juan_pablo", "habian_encontrado", "titulado_generacion", "boton_unirse", "importantes_librerias", 
  "musica_bienvenidos", "maria_julia", "maria", "julia", "patreon", "siguiendo", "hacerlo", "uniendote",
  "suscribiendote", "punto", "buenos_dias", "buenos", "dias", "podes_encontrarlo", 
  "podes", "encontrarlo", "etcetera_etcetera", "etcetera", "reciente_libro", "reciente", "libro",
  "importantes", "librerias", "libreria", "donaciones_libres", "donaciones", "libres", "encuentra", "disponible",
  "encuentra_disponible", "quisieras", "marcela", "pagano", "pablo", "anduesa", "barbara", "dirroco", "poner",
  "demas", "monton", "pudieras", "habian", "boton", "unirse", "musica", "campanita",
  
  # Diminutivos comunes
  "poquito", "poquita", "poquitos", "poquitas",
  "muchito", "muchita", "muchitos", "muchitas",
  "ratito", "ratitos", "ratita", "ratitas",
  "momentito", "momentitos",
  "ahorita", "ahoritita",
  "cosita", "cositas",
  "amiguito", "amiguita", "amiguitos", "amiguitas",
  "chiquito", "chiquita", "chiquitos", "chiquitas",
  "niñito", "niñita", "niñitos", "niñitas",
  "pueblito", "pueblitos",
  "casita", "casitas",
  "perrito", "perrita", "perritos", "perritas",
  "gatito", "gatita", "gatitos", "gatitas",
  "abuelito", "abuelita", "abuelitos", "abuelitas",
  "papito", "mamita",
  "hermanito", "hermanita", "hermanitos", "hermanitas",
  "noviecita", "noviecitos",
  "besito", "besitos",
  "cafecito", "cafecitos",
  "tiempito", "tiempitos",
  "boludito", "boluditos",
  
  # Expresiones argentinas coloquiales
  "che", "boludo", "boluda", "boludeces",
  "quilombo", "quilombito",
  "laburo", "laburito",
  "bondi", "colectivo",
  "mina", "minita",
  "pibe", "piba", "pibito", "pibita",
  "chabón", "chabona",
  "guita", "manguito", "mangos",
  "fiaca", "fiacón",
  "morfi", "morfar",
  "birra", "birrita",
  "matecito", "mate",
  "asado", "asadito",
  "locura", "locurita",
  "piola", "repiola",
  "copado", "copadito",
  "trucho", "truchito",
  "macana", "macanita",
  "cacho", "cachito",
  "bondiola", "choripán",
  "quilombazo", "quilombito",
  "re", "recontra", "altísimo", "altísima",
  "posta", "altísimamente",
  "bancá", "aguante", "aguantá",
  "dale", "andá", "vení",
  "uh", "epa", "ojo", "pucha"
  
)

diccionario_insultos_espectropolitico <- c(
  # Izquierda
  "izquierda" = "izquierda",
  "zurdo" = "izquierda", "zurdos" = "izquierda", "zurda" = "izquierda", "zurdas" = "izquierda",
  "comunista" = "izquierda", "comunistas" = "izquierda",
  "socialista" = "izquierda", "socialistas" = "izquierda",
  "progre" = "izquierda", "progres" = "izquierda",
  "progresista" = "izquierda", "progresistas" = "izquierda",
  "globalista" = "izquierda", "globalistas" = "izquierda",
  "woke" = "izquierda", "wokes" = "izquierda",
  "peronista" = "izquierda", "peronistas" = "izquierda",
  "planero" = "izquierda", "planeros" = "izquierda",
  "planera" = "izquierda", "planeras" = "izquierda",
  "choriplanero" = "izquierda", "choriplaneros" = "izquierda",
  "choriplanera" = "izquierda", "choriplaneras" = "izquierda",
  "orco" = "izquierda", "orcos" = "izquierda",
  "orca" = "izquierda", "orcas" = "izquierda",
  "rojo" = "izquierda", "rojos" = "izquierda",
  "roja" = "izquierda", "rojas" = "izquierda",
  "kuka" = "izquierda", "kukas" = "izquierda",
  
  # Derecha
  "derecha" = "derecha",
  "facho" = "derecha", "fachos" = "derecha",
  "fascista" = "derecha", "fascistas" = "derecha",
  "conservador" = "derecha", "conservadores" = "derecha",
  "libertario" = "derecha", "libertarios" = "derecha",
  "gorila" = "derecha", "gorilas" = "derecha",
  "patriarcado" = "derecha",
  
  # Género
  "género" = "género",
  "feminista" = "género", "feministas" = "género",
  "trans" = "género",
  "trava" = "género", "travesti" = "género", "trave" = "género", "travesa" = "género",
  "machona" = "género", "marimacho" = "género",
  "torta" = "género",
  "puto" = "género", "maricón" = "género", "loca" = "género",
  "no binarie" = "género", "les niñes" = "género", "les boludes" = "género",
  "ideología de género" = "género", "moda zurda" = "género",
  "se creen especiales" = "género", "eso no existe" = "género"
)

diccionario_insultos_espectropolitico <- tibble(
  palabra = names(diccionario_insultos_espectropolitico),
  categoria = unname(diccionario_insultos_espectropolitico)
)

# Tokenizar
tokens <- echterText %>%
  unnest_tokens(word, texto) %>%
  filter(!word %in% stopwords_es) %>%
  filter(!word %in% ruidos) %>%
  filter(!grepl("isimo$|isima$|isimos$|isimas$", word)) %>%
  filter(nchar(word) >= 3) %>%
  select(doc_id, word)

# Gráficos 

tokens_categorizados <- tokens %>%
  left_join(diccionario_insultos_espectropolitico, by = c("word" = "palabra"))

tokens_categorizados %>%
  filter(!is.na(categoria)) %>%
  count(categoria, word, sort = TRUE)


# Contar frecuencias
frecuencias <- tokens_categorizados %>%
  count(word, sort = TRUE)

# Seleccionar top 20
top20 <- head(frecuencias, 20)
print(top20)


ggplot(top20, aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 20 términos más frecuentes",
    x = "Término",
    y = "Frecuencia"
  )

# Frecuencia de términos por categoría
tokens_categorizados %>%
  filter(categoria == "género") %>%
  count(word, sort = TRUE) %>%
  slice_max(n, n = 20) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "darkorchid") +
  coord_flip() +
  labs(title = "Términos asociados a género", x = "Palabra", y = "Frecuencia")

tokens_categorizados %>%
  filter(categoria == "derecha") %>%
  count(word, sort = TRUE) %>%
  slice_max(n, n = 20) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "darkblue") +
  coord_flip() +
  labs(title = "Términos asociados a la ultraderecha", x = "Palabra", y = "Frecuencia")

tokens_categorizados %>%
  filter(categoria == "izquierda") %>%
  count(word, sort = TRUE) %>%
  slice_max(n, n = 20) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "darkred") +
  coord_flip() +
  labs(title = "Términos asociados a la izquierda", x = "Palabra", y = "Frecuencia")

library(wordcloud)
library(RColorBrewer)

wordcloud(
  words = frecuencias$word,
  freq = frecuencias$n,
  min.freq = 3,
  max.words = 100,
  colors = brewer.pal(8, "Dark2")
)

library(readxl)
library(dplyr)
library(ggplot2)

# === Cargar tus bases ===
comentarios_videos_gelatina <- read_excel("Datos/Comentarios/comentarios_youtube_gelatina_decena.xlsx")
comentarios_videos_cenital <- read_excel("Datos/Comentarios/comentarios_youtube_cenitalgrande_decena.xlsx")
comentarios_youtube_futurorockfm <- read_excel("Datos/Comentarios/comentarios_youtube_futurorockanrolleros_decena.xlsx")

comentarios_videos_laje <- read_excel("Datos/Comentarios/comentarios_videos_laje_decena.xlsx")
comentarios_videos_peluca <- read_excel("Datos/Comentarios/comentarios_videos_peluca_decena.xlsx")
comentarios_videos_danann <- read_excel("Datos/Comentarios/comentarios_videos_danann_decena.xlsx")

# === Agrupar y contar ===
primeros_3 <- bind_rows(
  comentarios_videos_gelatina,
  comentarios_videos_cenital,
  comentarios_youtube_futurorockfm
)

ultimos_3 <- bind_rows(
  comentarios_videos_laje,
  comentarios_videos_peluca,
  comentarios_videos_danann
)

conteo <- tibble(
  Grupo = c("Oposición", "Libertarios"),
  Observaciones = c(nrow(primeros_3), nrow(ultimos_3))
)

# === Gráfica con estilo Tupamaros ===
g_tupamaros <- conteo %>%
  ggplot(aes(x = reorder(Grupo, Observaciones), y = Observaciones)) +
  geom_col(fill = "darkorchid") +
  geom_text(aes(label = Observaciones),
            vjust = -0.3, color = "black", size = 4) +
  labs(
    title = "Comparación de cantidad de comentarios",
    x = "Grupo",
    y = "Número de observaciones",
    caption = "Elaboración propia (Tupamaros) con datos de YouTube"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.text = element_text(color = "gray20"),
    plot.caption = element_text(size = 10, hjust = 1, color = "gray40")
  )

g_tupamaros

