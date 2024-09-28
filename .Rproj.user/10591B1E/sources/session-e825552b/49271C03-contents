FC_empresas <- lista_datos$FC %>% 
  filter(subcategoria %in% c("pequeñas","privada","publica")) %>% 
  select(-tipo,-clase)

FC_comportamiento <- lista_datos$FC %>% 
  filter(subcategoria %in% c("fiscalizaciones","recuperacion bs")) %>% 
  select(-tipo,-clase, -subcategoria)
