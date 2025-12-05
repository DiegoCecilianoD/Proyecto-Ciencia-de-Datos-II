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
video_id <- "53upuyNeYY4"

# Ejecutar extracción
comentarios <- get_all_youtube_comments(video_id, api_key)

# Mostrar primeros comentarios
print(head(comentarios))

# Guardar en Excel
output_folder <- "~/PROFESIONAL TEC/5° Semestre/Ciencia de datos para la toma de decisiones II/RETO C.D II/Datos"
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
output_file <- file.path(output_folder, "comentarios_youtube_cenital.xlsx")
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
video_id <- "xpdAsc3iE2c"

# Ejecutar extracción
comentarios <- get_all_youtube_comments(video_id, api_key)

# Mostrar primeros comentarios
print(head(comentarios))

# Guardar en Excel
output_folder <- "~/PROFESIONAL TEC/5° Semestre/Ciencia de datos para la toma de decisiones II/RETO C.D II/Datos"
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
output_file <- file.path(output_folder, "comentarios_youtube_cenital2.xlsx")
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
video_id <- "vpLRiv9-GQc"

# Ejecutar extracción
comentarios <- get_all_youtube_comments(video_id, api_key)

# Mostrar primeros comentarios
print(head(comentarios))

# Guardar en Excel
output_folder <- "~/PROFESIONAL TEC/5° Semestre/Ciencia de datos para la toma de decisiones II/RETO C.D II/Datos"
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
output_file <- file.path(output_folder, "comentarios_youtube_cenital3.xlsx")
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
video_id <- "Kte_b3bDJik"

# Ejecutar extracción
comentarios <- get_all_youtube_comments(video_id, api_key)

# Mostrar primeros comentarios
print(head(comentarios))

# Guardar en Excel
output_folder <- "~/PROFESIONAL TEC/5° Semestre/Ciencia de datos para la toma de decisiones II/RETO C.D II/Datos"
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
output_file <- file.path(output_folder, "comentarios_youtube_cenital4.xlsx")
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
video_id <- "LbGHBorZTSw"

# Ejecutar extracción
comentarios <- get_all_youtube_comments(video_id, api_key)

# Mostrar primeros comentarios
print(head(comentarios))

# Guardar en Excel
output_folder <- "~/PROFESIONAL TEC/5° Semestre/Ciencia de datos para la toma de decisiones II/RETO C.D II/Datos"
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
output_file <- file.path(output_folder, "comentarios_youtube_cenital5.xlsx")
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
video_id <- "a8Bn5n_tOWo"

# Ejecutar extracción
comentarios <- get_all_youtube_comments(video_id, api_key)

# Mostrar primeros comentarios
print(head(comentarios))

# Guardar en Excel
output_folder <- "~/PROFESIONAL TEC/5° Semestre/Ciencia de datos para la toma de decisiones II/RETO C.D II/Datos"
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
output_file <- file.path(output_folder, "comentarios_youtube_cenital6.xlsx")
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
video_id <- "b6OAeWBeUeU"

# Ejecutar extracción
comentarios <- get_all_youtube_comments(video_id, api_key)

# Mostrar primeros comentarios
print(head(comentarios))

# Guardar en Excel
output_folder <- "~/PROFESIONAL TEC/5° Semestre/Ciencia de datos para la toma de decisiones II/RETO C.D II/Datos"
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
output_file <- file.path(output_folder, "comentarios_youtube_cenital7.xlsx")
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
video_id <- "n1M_XjLVWUA"

# Ejecutar extracción
comentarios <- get_all_youtube_comments(video_id, api_key)

# Mostrar primeros comentarios
print(head(comentarios))

# Guardar en Excel
output_folder <- "~/PROFESIONAL TEC/5° Semestre/Ciencia de datos para la toma de decisiones II/RETO C.D II/Datos"
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
output_file <- file.path(output_folder, "comentarios_youtube_cenital8.xlsx")
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
video_id <- "EFVo3JABb5k"

# Ejecutar extracción
comentarios <- get_all_youtube_comments(video_id, api_key)

# Mostrar primeros comentarios
print(head(comentarios))

# Guardar en Excel
output_folder <- "~/PROFESIONAL TEC/5° Semestre/Ciencia de datos para la toma de decisiones II/RETO C.D II/Datos"
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
output_file <- file.path(output_folder, "comentarios_youtube_cenital9.xlsx")
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
video_id <- "RlqIMS7roX4"

# Ejecutar extracción
comentarios <- get_all_youtube_comments(video_id, api_key)

# Mostrar primeros comentarios
print(head(comentarios))

# Guardar en Excel
output_folder <- "~/PROFESIONAL TEC/5° Semestre/Ciencia de datos para la toma de decisiones II/RETO C.D II/Datos"
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
output_file <- file.path(output_folder, "comentarios_youtube_cenital10.xlsx")
write.xlsx(comentarios, output_file)
message("✅ Archivo Excel generado en: ", output_file)
