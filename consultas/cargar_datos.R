nuevos_nombres <<- c("tipo", "gestion", "clase", "categoria", "subcategoria", "mes", "cantidad")
colores <<- c("#FFC502", "#073767", "#FF5733", "#33C1FF")

# Definir los nombres de los archivos y sus rutas relativas
archivos <<- c("CC.xlsx", "RD.xlsx", "SR.xlsx", "UJ.xlsx", "FC.xlsx")
ruta_base <<- "excel/"

lista_datos <<- cargar_y_renombrar_datos(ruta_base, archivos, nuevos_nombres)

orden_meses <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")
orden_meses_montos <- append(orden_meses, "agui", after = 11)
orden_meses_montos <- append(orden_meses_montos, "jun_reint", after = 6)