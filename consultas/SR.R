SR_cantidad <- lista_datos$SR %>%
  filter(tipo %in% c("cantidad")) %>%  
  filter(clase %in% c("planilla")) %>% 
  mutate(mes = factor(mes, levels = orden_meses_montos),
         cantidad = as.numeric(cantidad))  %>% 
  select(-tipo,-clase)

categorias <<- unique(SR_cantidad$categoria)
# Crear valores en minúscula para el procesamiento en el servidor
categorias_labels <<- tools::toTitleCase(categorias)  # Convertir a Título

subcategorias <<- unique(SR_cantidad$subcategoria)
subcategorias <<- c("todos", subcategorias)
# Crear valores en minúscula para el procesamiento en el servidor
subcategorias_labels <<- tools::toTitleCase(subcategorias)  # Convertir a Título


SR_monto <- lista_datos$SR %>%
  filter(tipo %in% c("monto")) %>%  
  filter(clase %in% c("planilla")) %>% 
  mutate(mes = factor(mes, levels = orden_meses_montos),
         cantidad = as.numeric(cantidad))  %>% 
  select(-tipo,-clase)

SR_genero <- lista_datos$SR %>%
  filter(tipo %in% c("cantidad","monto")) %>%  
  filter(subcategoria %in% c("masculino","femenino")) %>% 
  filter(categoria %in% c("titular","derechohabiente")) %>% 
  mutate(mes = factor(mes, levels = orden_meses_montos),
         cantidad = as.numeric(cantidad))  %>% 
  select(-tipo,-clase) %>% 
  group_by(gestion, categoria,subcategoria) %>% 
  slice_max(order_by = as.numeric(mes), n = 1, with_ties = FALSE) %>% 
  ungroup()

SR_anexo <- lista_datos$SR %>%
  filter(tipo %in% c("estadistica")) %>%  
  filter(categoria %in% c("impedimento","hospitalizacion","fallecimiento")) %>% 
  mutate(mes = factor(mes, levels = orden_meses_montos),
         cantidad = as.numeric(cantidad))  %>% 
  select(-tipo,-clase) %>% 
  group_by(gestion,mes) %>% 
  summarize(cantidad = sum(cantidad), .groups = 'drop')

SR_departamento <- lista_datos$SR %>% 
  filter(clase == "departamento_regional") %>% 
  filter(tipo %in% c("cantidad","monto")) %>% 
  select(-clase) %>% 
  group_by(tipo, gestion, categoria, subcategoria) %>% 
  slice_tail(n = 1) %>% 
  mutate(categoria = ifelse(categoria == "beni", "el beni", categoria)) %>% 
  mutate(categoria = str_to_title(categoria),
         categoria = ifelse(categoria == "Potosi", "Potosí", categoria)) %>%
  ungroup()

SR_departamento_mes <- lista_datos$SR %>% 
  filter(clase == "departamento_regional") %>% 
  filter(tipo %in% c("cantidad","monto")) %>% 
  select(-clase) %>% 
  mutate(mes = factor(mes, levels = orden_meses_montos)) %>%
  mutate(categoria = ifelse(categoria == "beni", "el beni", categoria)) %>% 
  mutate(categoria = str_to_title(categoria),
         categoria = ifelse(categoria == "Potosi", "Potosí", categoria)) %>%
  group_by(tipo,gestion,categoria, subcategoria,mes) %>% 
  summarize(cantidad = sum(cantidad), .groups = 'drop') %>% 
  pivot_wider(names_from = tipo, values_from = cantidad) %>% 
  arrange(gestion, subcategoria, mes)