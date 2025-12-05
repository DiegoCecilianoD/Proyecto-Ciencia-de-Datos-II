library(openxlsx)

# Obtener lista de todos los archivos .txt en la carpeta
archivos_txt <- list.files(path = "Datos/Subtítulos/subcentinal", 
                           pattern = "\\.txt$", 
                           full.names = TRUE)

# Leer cada archivo y crear un dataframe
lista_subtitulos <- list()

for(i in seq_along(archivos_txt)) {
  # Leer contenido del archivo
  contenido <- readLines(archivos_txt[i], warn = FALSE)
  texto_completo <- paste(contenido, collapse = " ")
  
  # Crear fila con nombre del archivo y contenido
  lista_subtitulos[[i]] <- data.frame(
    nombre_archivo = basename(archivos_txt[i]),
    video = gsub("\\.txt$", "", basename(archivos_txt[i])),
    texto = texto_completo,
    stringsAsFactors = FALSE
  )
}

# Combinar todo en un solo dataframe
subtitulos_consolidados <- do.call(rbind, lista_subtitulos)

# Guardar en Excel
openxlsx::write.xlsx(subtitulos_consolidados, "subtitulos_cenital.xlsx")

# Ver resumen
print(subtitulos_consolidados[, c("nombre_archivo", "video")])