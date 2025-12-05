# Instalar paquetes si no los tienes
install.packages(c("httr", "jsonlite", "tidyverse"))


# Video 1 ----
# Cargar paquetes
library(httr)
library(jsonlite)
library(openxlsx)
library(dplyr)

# Definir parámetros
api_key <- "AIzaSyByRMY9SwSejyv-q82bHN0TDoDey4EStqs"  # Reemplaza con tu clave real
video_id <- "NbSTG9dIZFw"

get_all_youtube_comments <- function(video_id, api_key) {
  base_url <- "https://www.googleapis.com/youtube/v3/commentThreads"
  all_comments <- data.frame()
  next_page <- NULL
  
get_all_youtube_comments <- function(video_id, api_key) {
  base_url <- "https://www.googleapis.com/youtube/v3/commentThreads"
  all_comments <- data.frame()
  next_page <- NULL
  repeat {
    params <- list(
      part = "snippet",
      videoId = video_id,
      key = api_key,
      maxResults = 100,
      textFormat = "plainText"
    )
    if (!is.null(next_page)) {
      params$pageToken <- next_page
    }
    
    response <- GET(url = base_url, query = params)
    if (status_code(response) != 200) {
      stop("Error en la solicitud: ", status_code(response))
    }
    
    content_json <- content(response, "text", encoding = "UTF-8")
    data <- fromJSON(content_json, flatten = TRUE)
    
    if (length(data$items) == 0) break
    
    comments <- data$items %>%
      select(
        autor = snippet.topLevelComment.snippet.authorDisplayName,
        texto = snippet.topLevelComment.snippet.textDisplay,
        fecha = snippet.topLevelComment.snippet.publishedAt
      )
    
    all_comments <- bind_rows(all_comments, comments)
    
    if (!is.null(data$nextPageToken)) {
      next_page <- data$nextPageToken
      Sys.sleep(0.5)  # Evita sobrecargar la API
    } else {
      break
    }
  }
  return(all_comments)
}}

# Mostrar resultados
print(head(comentarios))

# Guardar en un Excel
#output_file <- "comentarios_youtube.xlsx"

#write.xlsx(comentarios, output_file)

#message("✅ Archivo Excel generado: ", output_file)

#return(comentarios)

# Define la ruta completa
output_folder <- "~/PROFESIONAL TEC/5° Semestre/Ciencia de datos para la toma de decisiones II/RETO C.D II/Datos"
output_file <- file.path(output_folder, "comentarios_youtube_gelatinazos.xlsx")

# Guarda el archivo
write.xlsx(comentarios, output_file)

# Video 2 ----
# Cargar paquetes necesarios
library(httr)
library(jsonlite)
library(dplyr)
library(openxlsx)

# Definir función para extraer todos los comentarios
get_all_youtube_comments <- function(video_id, api_key) {
  base_url <- "https://www.googleapis.com/youtube/v3/commentThreads"
  all_comments <- data.frame()
  next_page <- NULL
  
  repeat {
    params <- list(
      part = "snippet",
      videoId = video_id,
      key = api_key,
      maxResults = 100,
      textFormat = "plainText"
    )
    if (!is.null(next_page)) {
      params$pageToken <- next_page
    }
    
    response <- GET(url = base_url, query = params)
    if (status_code(response) != 200) {
      stop("Error en la solicitud: ", status_code(response))
    }
    
    content_json <- content(response, "text", encoding = "UTF-8")
    data <- fromJSON(content_json, flatten = TRUE)
    
    if (length(data$items) == 0) break
    
    comments <- data$items %>%
      select(
        autor = snippet.topLevelComment.snippet.authorDisplayName,
        texto = snippet.topLevelComment.snippet.textDisplay,
        fecha = snippet.topLevelComment.snippet.publishedAt
      )
    
    all_comments <- bind_rows(all_comments, comments)
    
    if (!is.null(data$nextPageToken)) {
      next_page <- data$nextPageToken
      Sys.sleep(0.5)
    } else {
      break
    }
  }
  
  return(all_comments)
}

# Parámetros
api_key <- "AIzaSyByRMY9SwSejyv-q82bHN0TDoDey4EStqs"
video_id <- "b8ici-zENFQ"

# Ejecutar extracción
comentarios <- get_all_youtube_comments(video_id, api_key)

# Mostrar primeros comentarios
print(head(comentarios))

# Guardar en Excel
output_folder <- "~/PROFESIONAL TEC/5° Semestre/Ciencia de datos para la toma de decisiones II/RETO C.D II/Datos"
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
output_file <- file.path(output_folder, "comentarios_youtube_gelatina2.xlsx")
write.xlsx(comentarios, output_file)
message("✅ Archivo Excel generado en: ", output_file)


# Video 3 ----

# Cargar paquetes necesarios
library(httr)
library(jsonlite)
library(dplyr)
library(openxlsx)

# Definir función para extraer todos los comentarios
get_all_youtube_comments <- function(video_id, api_key) {
  base_url <- "https://www.googleapis.com/youtube/v3/commentThreads"
  all_comments <- data.frame()
  next_page <- NULL
  
  repeat {
    params <- list(
      part = "snippet",
      videoId = video_id,
      key = api_key,
      maxResults = 100,
      textFormat = "plainText"
    )
    if (!is.null(next_page)) {
      params$pageToken <- next_page
    }
    
    response <- GET(url = base_url, query = params)
    if (status_code(response) != 200) {
      stop("Error en la solicitud: ", status_code(response))
    }
    
    content_json <- content(response, "text", encoding = "UTF-8")
    data <- fromJSON(content_json, flatten = TRUE)
    
    if (length(data$items) == 0) break
    
    comments <- data$items %>%
      select(
        autor = snippet.topLevelComment.snippet.authorDisplayName,
        texto = snippet.topLevelComment.snippet.textDisplay,
        fecha = snippet.topLevelComment.snippet.publishedAt
      )
    
    all_comments <- bind_rows(all_comments, comments)
    
    if (!is.null(data$nextPageToken)) {
      next_page <- data$nextPageToken
      Sys.sleep(0.5)
    } else {
      break
    }
  }
  
  return(all_comments)
}

# Parámetros
api_key <- "AIzaSyByRMY9SwSejyv-q82bHN0TDoDey4EStqs"
video_id <- "R1OF4FO7J-A"

# Ejecutar extracción
comentarios <- get_all_youtube_comments(video_id, api_key)

# Mostrar primeros comentarios
print(head(comentarios))

# Guardar en Excel
output_folder <- "~/PROFESIONAL TEC/5° Semestre/Ciencia de datos para la toma de decisiones II/RETO C.D II/Datos"
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
output_file <- file.path(output_folder, "comentarios_youtube_gelatina3.xlsx")
write.xlsx(comentarios, output_file)
message("✅ Archivo Excel generado en: ", output_file)


# Video 4 ----

# Cargar paquetes necesarios
library(httr)
library(jsonlite)
library(dplyr)
library(openxlsx)

# Definir función para extraer todos los comentarios
get_all_youtube_comments <- function(video_id, api_key) {
  base_url <- "https://www.googleapis.com/youtube/v3/commentThreads"
  all_comments <- data.frame()
  next_page <- NULL
  
  repeat {
    params <- list(
      part = "snippet",
      videoId = video_id,
      key = api_key,
      maxResults = 100,
      textFormat = "plainText"
    )
    if (!is.null(next_page)) {
      params$pageToken <- next_page
    }
    
    response <- GET(url = base_url, query = params)
    if (status_code(response) != 200) {
      stop("Error en la solicitud: ", status_code(response))
    }
    
    content_json <- content(response, "text", encoding = "UTF-8")
    data <- fromJSON(content_json, flatten = TRUE)
    
    if (length(data$items) == 0) break
    
    comments <- data$items %>%
      select(
        autor = snippet.topLevelComment.snippet.authorDisplayName,
        texto = snippet.topLevelComment.snippet.textDisplay,
        fecha = snippet.topLevelComment.snippet.publishedAt
      )
    
    all_comments <- bind_rows(all_comments, comments)
    
    if (!is.null(data$nextPageToken)) {
      next_page <- data$nextPageToken
      Sys.sleep(0.5)
    } else {
      break
    }
  }
  
  return(all_comments)
}

# Parámetros
api_key <- "AIzaSyByRMY9SwSejyv-q82bHN0TDoDey4EStqs"
video_id <- "DBbyI99TtIM"

# Ejecutar extracción
comentarios <- get_all_youtube_comments(video_id, api_key)

# Mostrar primeros comentarios
print(head(comentarios))

# Guardar en Excel
output_folder <- "~/PROFESIONAL TEC/5° Semestre/Ciencia de datos para la toma de decisiones II/RETO C.D II/Datos"
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
output_file <- file.path(output_folder, "comentarios_youtube_gelatina4.xlsx")
write.xlsx(comentarios, output_file)
message("✅ Archivo Excel generado en: ", output_file)


# Video 5 ----

library(httr)
library(jsonlite)
library(dplyr)
library(openxlsx)

# Definir función para extraer todos los comentarios
get_all_youtube_comments <- function(video_id, api_key) {
  base_url <- "https://www.googleapis.com/youtube/v3/commentThreads"
  all_comments <- data.frame()
  next_page <- NULL
  
  repeat {
    params <- list(
      part = "snippet",
      videoId = video_id,
      key = api_key,
      maxResults = 100,
      textFormat = "plainText"
    )
    if (!is.null(next_page)) {
      params$pageToken <- next_page
    }
    
    response <- GET(url = base_url, query = params)
    if (status_code(response) != 200) {
      stop("Error en la solicitud: ", status_code(response))
    }
    
    content_json <- content(response, "text", encoding = "UTF-8")
    data <- fromJSON(content_json, flatten = TRUE)
    
    if (length(data$items) == 0) break
    
    comments <- data$items %>%
      select(
        autor = snippet.topLevelComment.snippet.authorDisplayName,
        texto = snippet.topLevelComment.snippet.textDisplay,
        fecha = snippet.topLevelComment.snippet.publishedAt
      )
    
    all_comments <- bind_rows(all_comments, comments)
    
    if (!is.null(data$nextPageToken)) {
      next_page <- data$nextPageToken
      Sys.sleep(0.5)
    } else {
      break
    }
  }
  
  return(all_comments)
}

# Parámetros
api_key <- "AIzaSyByRMY9SwSejyv-q82bHN0TDoDey4EStqs"
video_id <- "gazb7ed3T78"

# Ejecutar extracción
comentarios <- get_all_youtube_comments(video_id, api_key)

# Mostrar primeros comentarios
print(head(comentarios))

# Guardar en Excel
output_folder <- "~/PROFESIONAL TEC/5° Semestre/Ciencia de datos para la toma de decisiones II/RETO C.D II/Datos"
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
output_file <- file.path(output_folder, "comentarios_youtube_gelatina5.xlsx")
write.xlsx(comentarios, output_file)
message("✅ Archivo Excel generado en: ", output_file)



# video 6 ----

library(httr)
library(jsonlite)
library(dplyr)
library(openxlsx)

# Definir función para extraer todos los comentarios
get_all_youtube_comments <- function(video_id, api_key) {
  base_url <- "https://www.googleapis.com/youtube/v3/commentThreads"
  all_comments <- data.frame()
  next_page <- NULL
  
  repeat {
    params <- list(
      part = "snippet",
      videoId = video_id,
      key = api_key,
      maxResults = 100,
      textFormat = "plainText"
    )
    if (!is.null(next_page)) {
      params$pageToken <- next_page
    }
    
    response <- GET(url = base_url, query = params)
    if (status_code(response) != 200) {
      stop("Error en la solicitud: ", status_code(response))
    }
    
    content_json <- content(response, "text", encoding = "UTF-8")
    data <- fromJSON(content_json, flatten = TRUE)
    
    if (length(data$items) == 0) break
    
    comments <- data$items %>%
      select(
        autor = snippet.topLevelComment.snippet.authorDisplayName,
        texto = snippet.topLevelComment.snippet.textDisplay,
        fecha = snippet.topLevelComment.snippet.publishedAt
      )
    
    all_comments <- bind_rows(all_comments, comments)
    
    if (!is.null(data$nextPageToken)) {
      next_page <- data$nextPageToken
      Sys.sleep(0.5)
    } else {
      break
    }
  }
  
  return(all_comments)
}

# Parámetros
api_key <- "AIzaSyByRMY9SwSejyv-q82bHN0TDoDey4EStqs"
video_id <- "DpQ7WIK9TeM"

# Ejecutar extracción
comentarios <- get_all_youtube_comments(video_id, api_key)

# Mostrar primeros comentarios
print(head(comentarios))

# Guardar en Excel
output_folder <- "~/PROFESIONAL TEC/5° Semestre/Ciencia de datos para la toma de decisiones II/RETO C.D II/Datos"
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
output_file <- file.path(output_folder, "comentarios_youtube_gelatina6.xlsx")
write.xlsx(comentarios, output_file)
message("✅ Archivo Excel generado en: ", output_file)

# video 7 ----

library(httr)
library(jsonlite)
library(dplyr)
library(openxlsx)

# Definir función para extraer todos los comentarios
get_all_youtube_comments <- function(video_id, api_key) {
  base_url <- "https://www.googleapis.com/youtube/v3/commentThreads"
  all_comments <- data.frame()
  next_page <- NULL
  
  repeat {
    params <- list(
      part = "snippet",
      videoId = video_id,
      key = api_key,
      maxResults = 100,
      textFormat = "plainText"
    )
    if (!is.null(next_page)) {
      params$pageToken <- next_page
    }
    
    response <- GET(url = base_url, query = params)
    if (status_code(response) != 200) {
      stop("Error en la solicitud: ", status_code(response))
    }
    
    content_json <- content(response, "text", encoding = "UTF-8")
    data <- fromJSON(content_json, flatten = TRUE)
    
    if (length(data$items) == 0) break
    
    comments <- data$items %>%
      select(
        autor = snippet.topLevelComment.snippet.authorDisplayName,
        texto = snippet.topLevelComment.snippet.textDisplay,
        fecha = snippet.topLevelComment.snippet.publishedAt
      )
    
    all_comments <- bind_rows(all_comments, comments)
    
    if (!is.null(data$nextPageToken)) {
      next_page <- data$nextPageToken
      Sys.sleep(0.5)
    } else {
      break
    }
  }
  
  return(all_comments)
}

# Parámetros
api_key <- "AIzaSyByRMY9SwSejyv-q82bHN0TDoDey4EStqs"
video_id <- "IUFfsoEagFs"

# Ejecutar extracción
comentarios <- get_all_youtube_comments(video_id, api_key)

# Mostrar primeros comentarios
print(head(comentarios))

# Guardar en Excel
output_folder <- "~/PROFESIONAL TEC/5° Semestre/Ciencia de datos para la toma de decisiones II/RETO C.D II/Datos"
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
output_file <- file.path(output_folder, "comentarios_youtube_gelatina7.xlsx")
write.xlsx(comentarios, output_file)
message("✅ Archivo Excel generado en: ", output_file)


# video 8 ----

library(httr)
library(jsonlite)
library(dplyr)
library(openxlsx)

# Definir función para extraer todos los comentarios
get_all_youtube_comments <- function(video_id, api_key) {
  base_url <- "https://www.googleapis.com/youtube/v3/commentThreads"
  all_comments <- data.frame()
  next_page <- NULL
  
  repeat {
    params <- list(
      part = "snippet",
      videoId = video_id,
      key = api_key,
      maxResults = 100,
      textFormat = "plainText"
    )
    if (!is.null(next_page)) {
      params$pageToken <- next_page
    }
    
    response <- GET(url = base_url, query = params)
    if (status_code(response) != 200) {
      stop("Error en la solicitud: ", status_code(response))
    }
    
    content_json <- content(response, "text", encoding = "UTF-8")
    data <- fromJSON(content_json, flatten = TRUE)
    
    if (length(data$items) == 0) break
    
    comments <- data$items %>%
      select(
        autor = snippet.topLevelComment.snippet.authorDisplayName,
        texto = snippet.topLevelComment.snippet.textDisplay,
        fecha = snippet.topLevelComment.snippet.publishedAt
      )
    
    all_comments <- bind_rows(all_comments, comments)
    
    if (!is.null(data$nextPageToken)) {
      next_page <- data$nextPageToken
      Sys.sleep(0.5)
    } else {
      break
    }
  }
  
  return(all_comments)
}

# Parámetros
api_key <- "AIzaSyByRMY9SwSejyv-q82bHN0TDoDey4EStqs"
video_id <- "bX7LRWx7wEY"

# Ejecutar extracción
comentarios <- get_all_youtube_comments(video_id, api_key)

# Mostrar primeros comentarios
print(head(comentarios))

# Guardar en Excel
output_folder <- "~/PROFESIONAL TEC/5° Semestre/Ciencia de datos para la toma de decisiones II/RETO C.D II/Datos"
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
output_file <- file.path(output_folder, "comentarios_youtube_gelatina8.xlsx")
write.xlsx(comentarios, output_file)
message("✅ Archivo Excel generado en: ", output_file)


# video 9 ----

library(httr)
library(jsonlite)
library(dplyr)
library(openxlsx)

# Definir función para extraer todos los comentarios
get_all_youtube_comments <- function(video_id, api_key) {
  base_url <- "https://www.googleapis.com/youtube/v3/commentThreads"
  all_comments <- data.frame()
  next_page <- NULL
  
  repeat {
    params <- list(
      part = "snippet",
      videoId = video_id,
      key = api_key,
      maxResults = 100,
      textFormat = "plainText"
    )
    if (!is.null(next_page)) {
      params$pageToken <- next_page
    }
    
    response <- GET(url = base_url, query = params)
    if (status_code(response) != 200) {
      stop("Error en la solicitud: ", status_code(response))
    }
    
    content_json <- content(response, "text", encoding = "UTF-8")
    data <- fromJSON(content_json, flatten = TRUE)
    
    if (length(data$items) == 0) break
    
    comments <- data$items %>%
      select(
        autor = snippet.topLevelComment.snippet.authorDisplayName,
        texto = snippet.topLevelComment.snippet.textDisplay,
        fecha = snippet.topLevelComment.snippet.publishedAt
      )
    
    all_comments <- bind_rows(all_comments, comments)
    
    if (!is.null(data$nextPageToken)) {
      next_page <- data$nextPageToken
      Sys.sleep(0.5)
    } else {
      break
    }
  }
  
  return(all_comments)
}

# Parámetros
api_key <- "AIzaSyByRMY9SwSejyv-q82bHN0TDoDey4EStqs"
video_id <- "RG6gQayuftY"

# Ejecutar extracción
comentarios <- get_all_youtube_comments(video_id, api_key)

# Mostrar primeros comentarios
print(head(comentarios))

# Guardar en Excel
output_folder <- "~/PROFESIONAL TEC/5° Semestre/Ciencia de datos para la toma de decisiones II/RETO C.D II/Datos"
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
output_file <- file.path(output_folder, "comentarios_youtube_gelatina9.xlsx")
write.xlsx(comentarios, output_file)
message("✅ Archivo Excel generado en: ", output_file)

# Video 10 ----

library(httr)
library(jsonlite)
library(dplyr)
library(openxlsx)

# Definir función para extraer todos los comentarios
get_all_youtube_comments <- function(video_id, api_key) {
  base_url <- "https://www.googleapis.com/youtube/v3/commentThreads"
  all_comments <- data.frame()
  next_page <- NULL
  
  repeat {
    params <- list(
      part = "snippet",
      videoId = video_id,
      key = api_key,
      maxResults = 100,
      textFormat = "plainText"
    )
    if (!is.null(next_page)) {
      params$pageToken <- next_page
    }
    
    response <- GET(url = base_url, query = params)
    if (status_code(response) != 200) {
      stop("Error en la solicitud: ", status_code(response))
    }
    
    content_json <- content(response, "text", encoding = "UTF-8")
    data <- fromJSON(content_json, flatten = TRUE)
    
    if (length(data$items) == 0) break
    
    comments <- data$items %>%
      select(
        autor = snippet.topLevelComment.snippet.authorDisplayName,
        texto = snippet.topLevelComment.snippet.textDisplay,
        fecha = snippet.topLevelComment.snippet.publishedAt
      )
    
    all_comments <- bind_rows(all_comments, comments)
    
    if (!is.null(data$nextPageToken)) {
      next_page <- data$nextPageToken
      Sys.sleep(0.5)
    } else {
      break
    }
  }
  
  return(all_comments)
}

# Parámetros
api_key <- "AIzaSyByRMY9SwSejyv-q82bHN0TDoDey4EStqs"
video_id <- "RDHYxU_ZFtU"

# Ejecutar extracción
comentarios <- get_all_youtube_comments(video_id, api_key)

# Mostrar primeros comentarios
print(head(comentarios))

# Guardar en Excel
output_folder <- "~/PROFESIONAL TEC/5° Semestre/Ciencia de datos para la toma de decisiones II/RETO C.D II/Datos"
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
output_file <- file.path(output_folder, "comentarios_youtube_gelatina10.xlsx")
write.xlsx(comentarios, output_file)
message("✅ Archivo Excel generado en: ", output_file)


