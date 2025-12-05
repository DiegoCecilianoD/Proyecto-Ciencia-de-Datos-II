#!/usr/bin/env Rscript

# Extractor de subtítulos de YouTube en R
# Requiere: yt-dlp instalado en el sistema (se puede instalar en mac con brew)

# ⏺ Instalación de yt-dlp
# 
# 🍎 macOS
# 
Opción 1: Con Homebrew (recomendado)
 brew install yt-dlp
# 
# Opción 2: Con pip
# pip3 install yt-dlp
# 
# Opción 3: Descarga directa
# sudo curl -L https://github.com/yt-dlp/yt-dlp/releases/latest/download/yt-dlp -o /usr/local/bin/yt-dlp
# sudo chmod a+rx /usr/local/bin/yt-dlp
# 
# 🪟 Windows
# 
Opción 1: Con winget (Windows 10/11)
winget install yt-dlp
# 
# Opción 2: Con Chocolatey
# choco install yt-dlp
# 
# Opción 3: Con pip
# pip install yt-dlp
# 
# Opción 4: Descarga directa
# 1. Descarga el .exe de: https://github.com/yt-dlp/yt-dlp/releases/latest
# 2. Coloca yt-dlp.exe en una carpeta (ejemplo: C:\Program Files\yt-dlp\)
# 3. Agrega esa carpeta al PATH del sistema
# 
# ---
#   ✅ Verificar instalación
# 
# En ambos sistemas, verifica con:
#   yt-dlp --version
# 
# Si ves un número de versión, ya está instalado correctamente
# 
# Usa Homebrew en Mac y winget en Windows por ser las opciones más sencillas.

# ¿Tiene algún límite? 
#   
#   ⏺ Límites de yt-dlp y el script
# 
# ✅ NO tiene límites de:
#   
#   - Número de videos: Puedes descargar subtítulos de tantos videos como quieras
# - Duración del video: Funciona con videos de cualquier duración
# - Tipo de cuenta: No necesitas cuenta de YouTube ni suscripción Premium
# - Costo: Es completamente gratis
# 
# ⚠️ SÍ tiene limitaciones:
#   
#   1. Disponibilidad de subtítulos
# - Solo funciona si el video tiene subtítulos (manuales o automáticos)
# - Si el video no tiene subtítulos, no podrá extraer nada
# 2. Rate limiting de YouTube
# - Si haces demasiadas peticiones muy rápido (cientos en minutos), YouTube puede bloquearte temporalmente
# - Solución: Esperar unos minutos o usar un delay entre descargas
# 3. Videos privados o restringidos
# - No funciona con videos privados
# - Videos con restricción de edad pueden requerir cookies de sesión
# 4. Idioma específico
# - Si pides un idioma que no existe en el video, dará error
# - El script ahora te muestra los idiomas disponibles primero
# 
# 💡 Recomendación para uso masivo
# 
# Si vas a procesar muchos videos, agrega un delay:
#   
#   videos <- c("URL1", "URL2", "URL3", ...)
# 
# lapply(videos, function(v) {
#   extract_youtube_subtitles(v, language = "es")
#   Sys.sleep(2)  # Espera 2 segundos entre cada video
# })
# 
# En resumen: No hay límites prácticos para uso normal, solo ten cuidado de no bombardear YouTube con cientos de
# peticiones por minuto.


library(jsonlite)

#' Extractor de subtítulos de YouTube
#'
#' @description Clase para extraer subtítulos de videos de YouTube usando yt-dlp
YouTubeSubtitleExtractor <- function() {

  #' Extrae el video ID de una URL de YouTube
  #' @param url URL del video
  #' @return Video ID o NULL si no se encuentra
  extract_video_id <- function(url) {
    patterns <- c(
      "(?:v=|/)([0-9A-Za-z_-]{11}).*",
      "youtu\\.be/([0-9A-Za-z_-]{11})",
      "embed/([0-9A-Za-z_-]{11})",
      "shorts/([0-9A-Za-z_-]{11})"
    )

    for (pattern in patterns) {
      if (grepl(pattern, url, perl = TRUE)) {
        match <- regmatches(url, regexec(pattern, url, perl = TRUE))[[1]]
        if (length(match) > 1) {
          return(match[2])
        }
      }
    }
    return(NULL)
  }

  #' Valida si la URL es de YouTube
  #' @param url URL a validar
  #' @return TRUE si es válida, FALSE si no
  validate_youtube_url <- function(url) {
    video_id <- extract_video_id(url)
    return(!is.null(video_id) && nchar(video_id) == 11)
  }

  #' Obtiene información de subtítulos disponibles
  #' @param url URL del video de YouTube
  #' @return Lista con información de subtítulos
  get_available_subtitles <- function(url) {
    if (!validate_youtube_url(url)) {
      stop("URL de YouTube inválida")
    }
    
    # Agregar --no-warnings para suprimir advertencias
    cmd <- sprintf('yt-dlp --dump-json --quiet --no-warnings "%s"', url)
    
    tryCatch({
      result <- system(cmd, intern = TRUE)
      info <- fromJSON(paste(result, collapse = ""))
      
      return(list(
        video_id = info$id,
        subtitles = if(is.null(info$subtitles)) list() else info$subtitles,
        automatic_captions = if(is.null(info$automatic_captions)) list() else info$automatic_captions,
        title = if(is.null(info$title)) "Unknown" else info$title
      ))
    }, error = function(e) {
      stop(paste("Error al obtener información del video:", e$message))
    })
  }

  #' Lista idiomas disponibles para subtítulos
  #' @param url URL del video de YouTube
  #' @return Vector con códigos de idioma disponibles
  list_available_languages <- function(url) {
    info <- get_available_subtitles(url)
    languages <- c()

    if (length(info$subtitles) > 0) {
      languages <- c(languages, names(info$subtitles))
    }

    if (length(info$automatic_captions) > 0) {
      languages <- c(languages, names(info$automatic_captions))
    }

    return(sort(unique(languages)))
  }

  #' Extrae subtítulos de un video de YouTube
  #' @param url URL del video de YouTube
  #' @param language Código de idioma (por defecto "es")
  #' @param output_format Formato de salida (por defecto "srt")
  #' @param output_dir Directorio de salida (por defecto "./subtitles")
  #' @param save_txt Guardar también como archivo .txt (por defecto TRUE)
  #' @return Lista con rutas de archivos generados
  extract_subtitles <- function(url, language = "es", output_format = "srt",
                               output_dir = "./subtitles", save_txt = TRUE) {
    if (!validate_youtube_url(url)) {
      stop("URL de YouTube inválida")
    }

    # Crear directorio de salida si no existe
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    # Obtener información del video
    video_info <- get_available_subtitles(url)
    video_id <- video_info$video_id
    video_title <- video_info$title

    # Sanitizar título para nombre de archivo
    safe_title <- gsub("[^[:alnum:]_-]", "_", video_title)
    safe_title <- substr(safe_title, 1, 100)  # Limitar longitud

    # Nombre de archivo único basado en video ID
    base_filename <- sprintf("%s_%s", safe_title, video_id)
    output_template <- file.path(output_dir, paste0(base_filename, ".%(ext)s"))

    # Construir comando yt-dlp
    cmd <- sprintf(
      'yt-dlp --write-subs --write-auto-subs --skip-download --sub-langs "%s" --sub-format "%s" --quiet -o "%s" "%s"',
      language, output_format, output_template, url
    )

    cat("Descargando subtítulos...\n")

    tryCatch({
      # Ejecutar comando
      system(cmd, intern = FALSE)

      # Buscar archivo generado con el nombre específico
      srt_file <- file.path(output_dir, sprintf("%s.%s.%s", base_filename, language, output_format))

      if (!file.exists(srt_file)) {
        stop(sprintf("No se encontraron subtítulos en idioma '%s' para este video", language))
      }

      cat("Subtítulos descargados exitosamente\n")
      result <- list(original = srt_file)

      # Generar archivo .txt si se solicita
      if (save_txt && output_format == "srt") {
        content <- read_subtitles(srt_file)
        text <- srt_to_text(content)

        txt_file <- file.path(output_dir, sprintf("%s.%s.txt", base_filename, language))
        writeLines(text, txt_file, useBytes = TRUE)
        cat("Archivo de texto generado:", txt_file, "\n")
        result$txt <- txt_file
      }

      return(result)

    }, error = function(e) {
      stop(paste("Error al extraer subtítulos:", e$message))
    })
  }

  #' Lee el contenido de un archivo de subtítulos
  #' @param file_path Ruta del archivo de subtítulos
  #' @return Contenido del archivo como texto
  read_subtitles <- function(file_path) {
    if (!file.exists(file_path)) {
      stop("El archivo no existe")
    }
    return(readLines(file_path, warn = FALSE, encoding = "UTF-8"))
  }

  #' Convierte subtítulos SRT a texto plano
  #' @param srt_content Contenido SRT como vector de líneas
  #' @return Texto plano sin marcas de tiempo
  srt_to_text <- function(srt_content) {
    # Filtrar líneas que no son números ni marcas de tiempo
    text_lines <- srt_content[!grepl("^\\d+$", srt_content) &
                             !grepl("\\d{2}:\\d{2}:\\d{2},\\d{3} --> \\d{2}:\\d{2}:\\d{2},\\d{3}", srt_content) &
                             srt_content != ""]

    return(paste(text_lines, collapse = " "))
  }

  # Retornar lista de funciones
  return(list(
    extract_video_id = extract_video_id,
    validate_youtube_url = validate_youtube_url,
    get_available_subtitles = get_available_subtitles,
    list_available_languages = list_available_languages,
    extract_subtitles = extract_subtitles,
    read_subtitles = read_subtitles,
    srt_to_text = srt_to_text
  ))
}

#' Función principal configurable para extraer subtítulos
#' @param video_url URL del video de YouTube
#' @param language Código de idioma (por defecto "es")
#' @param output_format Formato de salida (por defecto "srt")
#' @param output_dir Directorio de salida (por defecto "./subtitles")
#' @param show_preview Mostrar preview del contenido (por defecto TRUE)
#' @param preview_length Número de caracteres para preview (por defecto 500)
#' @param save_txt Guardar también como archivo .txt (por defecto TRUE)
#' @return Lista con rutas de archivos generados
extract_youtube_subtitles <- function(video_url,
                                    language = "es",
                                    output_format = "srt",
                                    output_dir = "./subtitles",
                                    show_preview = TRUE,
                                    preview_length = 500,
                                    save_txt = TRUE) {

  extractor <- YouTubeSubtitleExtractor()

  tryCatch({
    cat("\n========================================\n")
    cat("Extrayendo subtítulos de YouTube\n")
    cat("========================================\n")
    cat("URL:", video_url, "\n")
    cat("Idioma:", language, "| Formato:", output_format, "\n")

    # Mostrar idiomas disponibles
    cat("\nObteniendo idiomas disponibles...\n")
    available_languages <- extractor$list_available_languages(video_url)
    cat("Idiomas disponibles:", paste(available_languages, collapse = ", "), "\n\n")

    # Verificar si el idioma solicitado está disponible
    if (!(language %in% available_languages)) {
      warning(sprintf("El idioma '%s' podría no estar disponible. Intente con: %s",
                     language, paste(head(available_languages, 3), collapse = ", ")))
    }

    # Extraer subtítulos
    output_files <- extractor$extract_subtitles(video_url, language, output_format, output_dir, save_txt)

    cat("\n--- ARCHIVOS GENERADOS ---\n")
    cat("Subtítulos SRT:", output_files$original, "\n")
    if (!is.null(output_files$txt)) {
      cat("Texto plano:", output_files$txt, "\n")
    }

    # Mostrar preview del contenido
    if (show_preview && output_format == "srt") {
      content <- extractor$read_subtitles(output_files$original)
      text <- extractor$srt_to_text(content)

      cat("\n--- PREVIEW DEL CONTENIDO ---\n")
      preview <- substr(text, 1, preview_length)
      cat(preview)
      if (nchar(text) > preview_length) {
        cat("...")
      }
      cat("\n")
      cat(sprintf("\nTotal de caracteres: %d\n", nchar(text)))
    }

    cat("\n========================================\n")
    cat("Extracción completada exitosamente\n")
    cat("========================================\n\n")

    return(invisible(output_files))

  }, error = function(e) {
    cat("\n========================================\n")
    cat("ERROR:", e$message, "\n")
    cat("========================================\n\n")
    return(NULL)
  })
}

#' Función principal para usar desde línea de comandos
main <- function() {
  args <- commandArgs(trailingOnly = TRUE)

  if (length(args) < 1) {
    cat("\n========================================\n")
    cat("Extractor de Subtítulos de YouTube\n")
    cat("========================================\n")
    cat("Uso: Rscript youtube_subtitles_extractor.R <URL_YOUTUBE> [idioma] [formato]\n\n")
    cat("Argumentos:\n")
    cat("  URL_YOUTUBE : URL del video de YouTube (requerido)\n")
    cat("  idioma      : Código de idioma (opcional, por defecto: es)\n")
    cat("  formato     : Formato de salida (opcional, por defecto: srt)\n\n")
    cat("Ejemplo:\n")
    cat("  Rscript youtube_subtitles_extractor.R 'https://youtube.com/watch?v=VIDEO_ID'\n")
    cat("  Rscript youtube_subtitles_extractor.R 'https://youtube.com/watch?v=VIDEO_ID' en\n")
    cat("  Rscript youtube_subtitles_extractor.R 'https://youtube.com/watch?v=VIDEO_ID' es srt\n")
    cat("========================================\n\n")
    quit(status = 1)
  }

  url <- args[1]
  language <- if(length(args) > 1) args[2] else "es"
  output_format <- if(length(args) > 2) args[3] else "srt"

  result <- extract_youtube_subtitles(url, language, output_format)
  if (is.null(result)) {
    quit(status = 1)
  }
}

# Ejecutar main solo si se corre directamente desde línea de comandos
if (!interactive() && length(sys.frames()) == 0) {
  main()
}

# Ejemplo: 
# Video seleccionado: https://www.youtube.com/watch?v=g5vbhA0zVfk
# (Hay que tener una carpeta llamada "subtitles")

# Cambiar URL en esta parte
extract_youtube_subtitles(video_url = "https://youtu.be/69R3Mtoip9Y?si=tprzC-DSSU5-sEmt",
                          language = "es",
                          output_format = "srt",
                          output_dir = "./subcenital",
                          show_preview = TRUE,
                          preview_length = 500,
                          save_txt = TRUE)

