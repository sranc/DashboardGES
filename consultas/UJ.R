UJ_procesos <- lista_datos$UJ %>%
  filter(clase %in% c("coactivo social")) %>%  
  filter(categoria %in% c("recuperacion (cobros indebidos y otras acreencias)","recuperacion (aportes devengados)")) %>% 
  filter(subcategoria %in% c("montos recuperados")) %>%  
  select(-tipo,-clase, -subcategoria) %>% 
  mutate(categoria = case_when(
    categoria == "recuperacion (cobros indebidos y otras acreencias)" ~ "cobros indebidos",
    categoria == "recuperacion (aportes devengados)" ~ "aportes devengados" 
  )) %>% 
  mutate(mes = factor(mes, levels = orden_meses_montos),
         cantidad = as.numeric(cantidad)) 

UJ_penales <- lista_datos$UJ %>% 
  filter(categoria == "penal") %>% 
  mutate(mes = factor(mes, levels = orden_meses_montos),
         cantidad = as.numeric(cantidad)) 

categorias_uj <<- unique(UJ_procesos$categoria)
categorias_uj <<- c("todos",categorias_uj)
# Crear valores en minúscula para el procesamiento en el servidor
categorias_labels_uj <<- tools::toTitleCase(categorias_uj)  # Convertir a Título
