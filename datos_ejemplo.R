
library(readxl)
datos<- read_excel("logs_curso.xlsx")




library(tidyverse)
library(lubridate)

datos<- datos %>%
  mutate(
    Hora_dt = as.POSIXct(Hora, format = "%d/%m/%y, %H:%M:%S", tz = "Europe/Madrid"),
    Fecha   = as.Date(Hora_dt),
    Hour    = hour(Hora_dt),
    # Día de la semana con abreviaturas personalizadas
    Weekday_num = wday(Fecha, week_start = 1),  # lunes=1, domingo=7
    Weekday = factor(
      Weekday_num,
      levels = 1:7,
      labels = c("Lun","Mar","Mie","Jue","Vie","Sab","Dom"),
      ordered = TRUE
    ),
    Origen = factor(Origen)
  )


datos <- datos %>%
  rename(
    hora            = Hora,
    usuario         = `Nombre completo del usuario`,
    usuario_afectado= `Usuario afectado`,
    contexto        = `Contexto del evento`,
    componente      = Componente,
    evento          = `Nombre evento`,
    descripcion     = Descripción,
    origen          = Origen,
    ip              = `Dirección IP`
  )


# Extraer nombres únicos de los alumnos
alumnos <- data.frame(
  usuario = unique(datos$usuario)
)

# Verificar
head(alumnos)
nrow(alumnos)   # número total de alumnos distintos



library(tidyverse)

fecha_inicio <- as.Date("2025-09-01")
fecha_hoy <- Sys.Date()
max_dias <- as.integer(fecha_hoy - fecha_inicio + 1)

# Días distintos por alumno
dias_por_alumno <- datos %>%
  group_by(usuario) %>%
  summarise(dias_distintos = n_distinct(Fecha),
            clics = n(),
            .groups = "drop") %>%
  mutate(
    pct_dias = dias_distintos / max_dias * 100,
    clics_por_dia = clics / dias_distintos
  )

# Máximos
max_clics <- max(dias_por_alumno$clics)
max_clics_por_dia <- max(dias_por_alumno$clics_por_dia)

# Añadir porcentajes relativos
dias_por_alumno <- dias_por_alumno %>%
  mutate(
    pct_clics = clics / max_clics * 100,
    pct_clics_por_dia = clics_por_dia / max_clics_por_dia * 100
  )

