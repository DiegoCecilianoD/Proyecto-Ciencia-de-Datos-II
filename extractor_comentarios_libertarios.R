# Youtube 

# https://www.youtube.com/watch?v=3PV0Rra8NkI
# Añadir ligas de Youtube a usar

# ¡No usar esta información!

# yt_oauth(app_id     = "",
#   app_secret = "")

# Ejemplo de video
# 3comentarios_video <- tuber::get_all_comments(video_id = "3PV0Rra8NkI")

# Opción amigable con las APIs: 
# comentarios_video <- tuber::get_comment_threads(
# filter = c(video_id = "9ZyFkQv1bKo"),
# part = "snippet"
# )

# openxlsx::write.xlsx(comentarios_video, "comentarios_video_laje.xlsx")

# Info
# video_info <- tuber::get_video_details(video_id = "3PV0Rra8NkI")