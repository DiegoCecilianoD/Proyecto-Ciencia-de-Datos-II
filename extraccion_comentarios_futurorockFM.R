# Instalar paquetes si no los tienes
install.packages(c("httr", "jsonlite", "tidyverse"))


# Video 1 ----
# Cargar paquetes
library(httr)
library(jsonlite)
library(openxlsx)
library(dplyr)

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
    
    content_json <- httr::content(response, as = "text", encoding = "UTF-8")
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
video_id <- "nRF-h0IIbZY"

# Ejecutar extracción
comentarios <- get_all_youtube_comments(video_id, api_key)

# Mostrar primeros comentarios
print(head(comentarios))

# Guardar en Excel
output_folder <- "~/PROFESIONAL TEC/5° Semestre/Ciencia de datos para la toma de decisiones II/RETO C.D II/Datos"
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
output_file <- file.path(output_folder, "comentarios_youtube_futurorockFM.xlsx")
write.xlsx(comentarios, output_file)
message("✅ Archivo Excel generado en: ", output_file)



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
    
    content_json <- httr::content(response, as = "text", encoding = "UTF-8")
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
video_id <- "d817kg-Af84"

# Ejecutar extracción
comentarios <- get_all_youtube_comments(video_id, api_key)

# Mostrar primeros comentarios
print(head(comentarios))

# Guardar en Excel
output_folder <- "~/PROFESIONAL TEC/5° Semestre/Ciencia de datos para la toma de decisiones II/RETO C.D II/Datos"
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
output_file <- file.path(output_folder, "comentarios_youtube_futurorockFM2.xlsx")
write.xlsx(comentarios, output_file)
message("✅ Archivo Excel generado en: ", output_file)

# Video 3 ----

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
    
    content_json <- httr::content(response, as = "text", encoding = "UTF-8")
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
video_id <- "dFyBG2deHyc"

# Ejecutar extracción
comentarios <- get_all_youtube_comments(video_id, api_key)

# Mostrar primeros comentarios
print(head(comentarios))

# Guardar en Excel
output_folder <- "~/PROFESIONAL TEC/5° Semestre/Ciencia de datos para la toma de decisiones II/RETO C.D II/Datos"
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
output_file <- file.path(output_folder, "comentarios_youtube_futurorockFM3.xlsx")
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
    
    content_json <- httr::content(response, as = "text", encoding = "UTF-8")
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
video_id <- "Ro5spZuCEX4"

# Ejecutar extracción
comentarios <- get_all_youtube_comments(video_id, api_key)

# Mostrar primeros comentarios
print(head(comentarios))

# Guardar en Excel
output_folder <- "~/PROFESIONAL TEC/5° Semestre/Ciencia de datos para la toma de decisiones II/RETO C.D II/Datos"
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
output_file <- file.path(output_folder, "comentarios_youtube_futurorockFM4.xlsx")
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
    
    content_json <- httr::content(response, as = "text", encoding = "UTF-8")
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
video_id <- "qNTxAbcLZp8"

# Ejecutar extracción
comentarios <- get_all_youtube_comments(video_id, api_key)

# Mostrar primeros comentarios
print(head(comentarios))

# Guardar en Excel
output_folder <- "~/PROFESIONAL TEC/5° Semestre/Ciencia de datos para la toma de decisiones II/RETO C.D II/Datos"
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
output_file <- file.path(output_folder, "comentarios_youtube_futurorockFM5.xlsx")
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
    
    content_json <- httr::content(response, as = "text", encoding = "UTF-8")
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
video_id <- "CXbOJkELgaI"

# Ejecutar extracción
comentarios <- get_all_youtube_comments(video_id, api_key)

# Mostrar primeros comentarios
print(head(comentarios))

# Guardar en Excel
output_folder <- "~/PROFESIONAL TEC/5° Semestre/Ciencia de datos para la toma de decisiones II/RETO C.D II/Datos"
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
output_file <- file.path(output_folder, "comentarios_youtube_futurorockFM6.xlsx")
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
    
    content_json <- httr::content(response, as = "text", encoding = "UTF-8")
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
video_id <- "El41R1GepK8"

# Ejecutar extracción
comentarios <- get_all_youtube_comments(video_id, api_key)

# Mostrar primeros comentarios
print(head(comentarios))

# Guardar en Excel
output_folder <- "~/PROFESIONAL TEC/5° Semestre/Ciencia de datos para la toma de decisiones II/RETO C.D II/Datos"
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
output_file <- file.path(output_folder, "comentarios_youtube_futurorockFM7.xlsx")
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
    
    content_json <- httr::content(response, as = "text", encoding = "UTF-8")
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
video_id <- "bzDtCCyy_7s"

# Ejecutar extracción
comentarios <- get_all_youtube_comments(video_id, api_key)

# Mostrar primeros comentarios
print(head(comentarios))

# Guardar en Excel
output_folder <- "~/PROFESIONAL TEC/5° Semestre/Ciencia de datos para la toma de decisiones II/RETO C.D II/Datos"
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
output_file <- file.path(output_folder, "comentarios_youtube_futurorockFM8.xlsx")
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
    
    content_json <- httr::content(response, as = "text", encoding = "UTF-8")
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
video_id <- "A43_yGYHlW8"

# Ejecutar extracción
comentarios <- get_all_youtube_comments(video_id, api_key)

# Mostrar primeros comentarios
print(head(comentarios))

# Guardar en Excel
output_folder <- "~/PROFESIONAL TEC/5° Semestre/Ciencia de datos para la toma de decisiones II/RETO C.D II/Datos"
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
output_file <- file.path(output_folder, "comentarios_youtube_futurorockFM9.xlsx")
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
    
    content_json <- httr::content(response, as = "text", encoding = "UTF-8")
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
video_id <- "hkQBtJnK2-0"

# Ejecutar extracción
comentarios <- get_all_youtube_comments(video_id, api_key)

# Mostrar primeros comentarios
print(head(comentarios))

# Guardar en Excel
output_folder <- "~/PROFESIONAL TEC/5° Semestre/Ciencia de datos para la toma de decisiones II/RETO C.D II/Datos"
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
output_file <- file.path(output_folder, "comentarios_youtube_futurorockFM10.xlsx")
write.xlsx(comentarios, output_file)
message("✅ Archivo Excel generado en: ", output_file)


