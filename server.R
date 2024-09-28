server <- function(input, output, session) {
  tamano_num <- 13
  valor1 <- reactive({
    cantidad <- totales_cantidad %>%
      filter(gestion == input$gestion) %>%
      select(cantidad) %>%
      pull()  # Extrae el valor como un vector
    
    # Formatear el número con separadores de miles y dos decimales
    prettyNum(cantidad, big.mark = ",", decimal.mark = ".", nsmall = 0)
  })
  valor2 <- reactive({
    cantidad <- totales_monto %>% 
      filter(gestion == input$gestion) %>% 
      select(cantidad) %>% 
      pull()  # Extrae el valor como un vector
    
    # Formatear el número con separadores de miles y dos decimales
    prettyNum(cantidad, big.mark = ",", decimal.mark = ".", nsmall = 0)
  })  
  valor3 <- reactive({
    cantidad <- SR_cantidad %>% 
      filter(gestion == input$gestion) %>% 
      filter(categoria %in% c("regular","pago a domicilio")) %>% 
      group_by(categoria, subcategoria) %>% 
      slice_tail(n = 1) %>% 
      group_by(gestion) %>% 
      summarize(cantidad = sum(cantidad), .groups = 'drop') %>% 
      select(cantidad) %>% 
      pull()
    prettyNum(cantidad, big.mark = ",", decimal.mark = ".", nsmall = 0)
  })  
  valor4 <- reactive({
    cantidad <- SR_monto %>% 
      filter(gestion == input$gestion) %>% 
      filter(categoria %in% c("regular","pago a domicilio")) %>% 
      group_by(categoria, subcategoria) %>%
      mutate(cantidad = cumsum(cantidad)) %>% 
      slice_tail(n = 1) %>% 
      group_by(gestion) %>% 
      summarize(cantidad = sum(cantidad), .groups = 'drop') %>% 
      mutate(cantidad = round(cantidad/1000000,0)) %>% 
      select(cantidad) %>% 
      pull()
    paste0(prettyNum(cantidad, big.mark = ",", decimal.mark = ".", nsmall = 0)," M")
  })   
  valor5 <- reactive({
    cantidad <- UJ_procesos %>% 
      filter(gestion == input$gestion) %>% 
      group_by(gestion, categoria) %>% 
      summarize(cantidad = round(sum(cantidad),0), .groups = 'drop') %>%
      group_by(gestion) %>% 
      summarize(cantidad = round(sum(cantidad),0), .groups = 'drop') %>%
      select(cantidad) %>% 
      pull()
    if (is.null(cantidad) || length(cantidad) == 0 || cantidad == "") {
      "Sin datos"
    } else {
      paste0(prettyNum(cantidad, big.mark = ",", decimal.mark = ".", nsmall = 0)," M")
    }
  })   
  valor6 <- reactive({
    cantidad <- FC_empresas %>% 
      filter(gestion == input$gestion) %>% 
      filter(categoria == "sector") %>% 
      group_by(categoria) %>% 
      summarize(cantidad = sum(cantidad), .groups = 'drop') %>% 
      select(cantidad) %>% 
      pull()
    if (is.null(cantidad) || length(cantidad) == 0 || cantidad == "") {
      "Sin datos"
    } else {
      paste0(prettyNum(cantidad, big.mark = ",", decimal.mark = ".", nsmall = 0))
    }
  }) 
  
  output$widget1 <- renderValueBox({
    valueBox(
      value = valor1(),
      subtitle = "N° Beneficiarios CC",
      icon = icon("chart-line"),
      color = "aqua"
    )
  })
  
  output$widget2 <- renderValueBox({
    valueBox(
      value = paste0(valor2()," M Bs"),
      subtitle = "Monto Desembolsado CC ",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$widget3 <- renderValueBox({
    valueBox(
      value = valor3(),
      subtitle = "N° Beneficiarios SR",
      icon = icon("user-check"),
      color = "yellow"
    )
  })
  
  output$widget4 <- renderValueBox({
    valueBox(
      value = valor4(),
      subtitle = "Monto Desembolsado SR",
      icon = icon("piggy-bank"),
      color = "purple"
    )
  })
  
  output$widget5 <- renderValueBox({
    val <- valor5()
    valueBox(
      value = val,
      subtitle = "Monto recuperado UJ",
      icon = icon("wallet"),
      color = "red"
    )
  })
  
  output$widget6 <- renderValueBox({
    val <- valor6()
    valueBox(
      value = if (is.null(val) || length(val) == 0 || val == "") {
        "Sin datos"
      } else {
        val
      },
      subtitle = "Cant.Empresas Fiscalizadas",
      icon = icon("money-bill-wave"),
      color = "blue"
    )
  })
  
# DAtos reactivos para los graficos CC
  datos_cantidad <- reactive({
    CC_cantidad %>% 
      filter(if (input$compensacion == "Todos") TRUE else subcategoria == tolower(input$compensacion)) %>%
      pivot_wider(names_from = subcategoria, values_from = cantidad, names_prefix = "cant_") %>% 
      mutate(total = rowSums(across(starts_with("cant_")), na.rm = TRUE)) %>% 
      select(gestion, mes, any_of(c("cant_global", "cant_mensual")), total) %>%
      filter(gestion == input$gestion) %>%
      filter(as.numeric(mes) >= input$mes[1] & as.numeric(mes) <= input$mes[2]) %>% 
      mutate(mes = str_to_title(mes))
  })
  
  datos_monto <- reactive({
    
    CC_monto %>%
      filter(if (input$compensacion == "Todos") TRUE else subcategoria == tolower(input$compensacion)) %>%
      pivot_wider(names_from = subcategoria, values_from = cantidad, names_prefix = "mont_") %>%
      mutate(total = rowSums(across(starts_with("mont_")), na.rm = TRUE)) %>%
      # Reemplazar NA por 0 solo en las columnas que existen
      mutate(across(starts_with("mont_"), ~replace_na(., 0))) %>%
      mutate(total = replace_na(total, 0)) %>%
      # Seleccionar columnas condicionalmente si existen
      select(gestion, mes, any_of(c("mont_global", "mont_mensual")), total) %>%
      arrange(gestion, mes) %>%
      filter(gestion == input$gestion) %>%
      filter(as.numeric(mes) >= input$mes[1] & as.numeric(mes) <= input$mes[2]) %>% 
      mutate(mes = str_to_title(mes))
  })
  
  datos_genero <- reactive({
    cc_genero %>%
      filter(gestion == input$gestion) 
  })
  
  datos_altas <- reactive({
    CC_altas %>%
      filter(gestion == input$gestion) %>% 
      mutate(categoria = case_when(categoria == "afp futuro" ~ "AFP Futuro", 
                                   categoria == "afp prevision" ~ "AFP Previsión", 
                                   categoria == "gestora publica" ~ "Gestora Pública",
                                   TRUE ~ categoria)) 
  })
  
  datos_inicio_emitidos <- reactive({
    CC_inicio_emitidos %>%
      mutate(categoria = str_replace_all(categoria, " ", "_")) %>%
      pivot_wider(names_from = categoria, values_from = cantidad, names_prefix = "cant_") %>%
      filter(gestion == input$gestion) %>% 
      filter(as.numeric(mes) >= input$mes[1] & as.numeric(mes) <= input$mes[2]) %>% 
      mutate(mes = str_to_title(mes))
  })
  
  datos_departamento_cc <- reactive({
    CC_departamento_mes %>% 
      filter(gestion == input$gestion) %>% 
      select(-gestion) %>% 
      mutate(mes = sapply(as.character(mes), mes_selector))
  })
  
# DAtos reactivos para los graficos SR
  
  observe({
    # Verificar si el usuario deselecciona todas las opciones
    if (length(input$categorias_check) == 0) {
      # Si no hay nada seleccionado, restablecer la última selección válida
      last_selection <- isolate(input$categorias_check)
      if (length(last_selection) == 0) {
        last_selection <- c("regular", "pago a domicilio")  # O proporcionar un valor por defecto
      }
      updateCheckboxGroupInput(session, "categorias_check", selected = last_selection)
    } 
  })
  
  cantidad_sr <- reactive({
    SR_cantidad %>% 
      filter(gestion == input$gestion) %>%
      filter(categoria %in% input$categorias_check) %>% 
      filter(if (input$tipo_sr == "todos") TRUE else subcategoria == input$tipo_sr) %>%
      select(-categoria) %>% 
      pivot_wider(names_from = subcategoria, values_from = cantidad, values_fn = list(cantidad = sum), names_prefix = "cant_") %>% 
      mutate(total = rowSums(across(starts_with("cant_")), na.rm = TRUE)) %>%
      filter(as.numeric(mes) >= input$mes[1] & as.numeric(mes) <= input$mes[2]) %>% 
      mutate(mes = str_to_title(mes))
  })
  
  monto_sr <- reactive({
    SR_monto %>% 
      filter(gestion == input$gestion) %>%
      filter(categoria %in% input$categorias_check) %>% 
      filter(if (input$tipo_sr == "todos") TRUE else subcategoria == input$tipo_sr) %>%
      select(-categoria) %>% 
      pivot_wider(names_from = subcategoria, values_from = cantidad, values_fn = list(cantidad = sum), names_prefix = "cant_") %>% 
      mutate(across(starts_with("cant_"), ~ round(.x / 1000000, 0))) %>% 
      mutate(across(starts_with("cant_"), cumsum)) %>% 
      mutate(total = rowSums(across(starts_with("cant_")), na.rm = TRUE)) %>%
      filter(as.numeric(mes) >= input$mes[1] & as.numeric(mes) <= input$mes[2]) %>% 
      mutate(mes = str_to_title(mes))
  })
  
  monto_cantidad_sr <- reactive({
    cantidad <- cantidad_sr() %>% 
      select(mes,total) %>% 
      rename(cant_total = total) 
    monto <- monto_sr() %>% 
      select(mes,total) %>% 
      rename(mont_total = total) 
    combined_data <- left_join(cantidad, monto, by = "mes") %>% 
      mutate(mes = str_to_title(mes))

  })
  
  genero_sr <- reactive({
    SR_genero %>% 
      filter(gestion == input$gestion) %>% 
      filter(if (input$tipo_sr == "todos") TRUE else categoria == input$tipo_sr) %>% 
      group_by(gestion,subcategoria,mes) %>% 
      summarize(cantidad = sum(cantidad), .groups = 'drop')
  })
  
  anexo_sr <- reactive({
    SR_anexo %>% 
      filter(gestion == input$gestion) %>% 
      filter(as.numeric(mes) >= input$mes[1] & as.numeric(mes) <= input$mes[2]) %>% 
      mutate(mes = str_to_title(mes))
  })
  
  datos_departamento_sr <- reactive({
    SR_departamento_mes %>% 
      filter(gestion == input$gestion) %>% 
      select(-gestion) %>% 
      mutate(mes = sapply(as.character(mes), mes_selector)) %>% 
      mutate(subcategoria = str_to_title(subcategoria)) %>%
      rename(Municipio = subcategoria, Mes = mes, Cantidad = cantidad, Monto = monto) 
  })

# DAtos reactivos para los graficos UJ
  
  indebido_devengado <- reactive({
    UJ_procesos %>% 
      filter(gestion == input$gestion) %>%
      filter(if (input$tipo_empresa == "todos") TRUE else categoria == input$tipo_empresa) %>% 
      mutate(categoria = gsub(" ", "_", categoria)) %>% 
      pivot_wider(names_from = categoria, values_from = cantidad, values_fn = list(cantidad = sum), names_prefix = "cant_") %>% 
      mutate(total = rowSums(across(starts_with("cant_")), na.rm = TRUE)) %>%
      filter(as.numeric(mes) >= input$mes[1] & as.numeric(mes) <= input$mes[2]) %>% 
      mutate(mes = str_to_title(mes))
  })
  
  penales <- reactive({
    UJ_penales %>% 
      filter(gestion == input$gestion) %>% 
      filter(as.numeric(mes) >= input$mes[1] & as.numeric(mes) <= input$mes[2]) %>% 
      mutate(mes = str_to_title(mes))
  })
  
  # DAtos reactivos para los graficos fc
  
  empresa <- reactive({
    subcategorias_necesarias <- if (input$filtro_empresa == "tamaño") {
      c("cant_pequeñas", "cant_grandes")
    } else {
      c("cant_privada", "cant_publica")
    }
    
    resultado_pivot <- FC_empresas %>% 
      filter(gestion == input$gestion) %>%
      filter(categoria == input$filtro_empresa) %>% 
      select(-categoria) %>% 
      pivot_wider(names_from = subcategoria, 
                  values_from = cantidad, 
                  values_fn = list(cantidad = sum), 
                  names_prefix = "cant_") 
    for (col_name in subcategorias_necesarias) {
      if (!col_name %in% names(resultado_pivot)) {
        resultado_pivot[[col_name]] <- 0  # Añade la columna con todos los valores establecidos a 0
      }
    }
    
    resultado_pivot  %>%
      mutate(across(any_of(subcategorias_necesarias), ~ replace_na(., 0), .names = "{.col}")) %>% 
      mutate(total = rowSums(across(starts_with("cant_")), na.rm = TRUE)) %>% 
      mutate(mes = str_to_title(mes))
  })
  
  comportamiento <- reactive({
    FC_comportamiento %>% 
      filter(gestion == input$gestion) %>% 
      mutate(categoria = gsub(" ", "_", categoria)) %>% 
      pivot_wider(names_from = categoria, 
                  values_from = cantidad, 
                  values_fn = list(cantidad = sum), 
                  names_prefix = "cant_") %>% 
      mutate(cant_recuperacion_bs = round(cant_recuperacion_bs/1000000,2)) %>% 
      mutate(mes = str_to_title(mes))
  })
  
  # graficos para CC
  
  output$barras_gestion <- renderEcharts4r({
    datos <- datos_cantidad()
    num_meses <- if (length(unique(datos$mes)) > 7) 45 else 0
    # Crear gráfico de barras con total acumulado
    grafico <- datos %>%
      e_charts(mes)
    
    # Verificar si mont_global está presente y agregarlo al gráfico si es así
    if ("cant_global" %in% colnames(datos)) {
      grafico <- grafico %>%
        e_bar(cant_global, 
              stack = "grp", 
              name = "Global", 
              itemStyle = list( color = "#FFC502",
                                borderRadius = 5,
                                shadowColor = "black",
                                shadowBlur = 2), 
              label = list(show = FALSE,
                           fontWeight = "bold", 
                           formatter  =  htmlwidgets::JS("
                            function(params){
                              var value = params.value[1];
                              var parts = value.toString().split('.');
                              parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                              return parts.join('.');
                            }
                            ")),
              legend = FALSE)
    }
    
    # Verificar si mont_mensual está presente y agregarlo al gráfico si es así
    if ("cant_mensual" %in% colnames(datos)) {
      grafico <- grafico %>%
        e_bar(cant_mensual, 
              stack = "grp", 
              name = "Mensual", 
              itemStyle = list(color = "#073767",
                               borderRadius = 5,
                               shadowColor = "black",
                               shadowBlur = 5),
              label = list(show = FALSE,
                           fontWeight = "bold", 
                           formatter  = htmlwidgets::JS("
                            function(params){
                              var value = params.value[1];
                              var parts = value.toString().split('.');
                              parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                              return parts.join('.');
                            }
                            ")),
              legend = FALSE)
    }
    
    # Agregar la línea del total
    grafico <- grafico %>%
      e_line(total, 
             itemStyle = list(width=10, color = "#202C33"), 
             name = "Total",
             symbol = "diamond", 
             symbolSize = 10, 
             label = list(show = TRUE, 
                          position = "top", 
                          fontWeight = "bold", 
                          fontSize = tamano_num,
                          rotate = num_meses,
                          formatter  =  htmlwidgets::JS("
                              function(params){
                              var value = params.value[1];
                              var parts = value.toString().split('.');
                              parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                              return parts.join('.');
                            }
                              ")),
             lineStyle = list(width = 2)) %>% 
      e_tooltip(trigger = "axis") %>%  # Tooltip al pasar el mouse
      e_legend(show = TRUE, bottom = 0) %>%
      e_x_axis( type = "category") %>%
      e_y_axis( axisLabel = list(rotate = 60)) 
    
    # Renderizar el gráfico
    grafico
  })
  
  output$barras_monto <- renderEcharts4r({
    datos <- datos_monto()
    num_meses <- if (length(unique(datos$mes)) > 7) 45 else 0
    
    # Crear gráfico de barras dinámicamente según las columnas presentes
    grafico <- datos %>%
      e_charts(mes)
    
    # Verificar si mont_global está presente y agregarlo al gráfico si es así
    if ("mont_global" %in% colnames(datos)) {
      grafico <- grafico %>%
        e_bar(mont_global, 
              stack = "grp", 
              name = "Global", 
              itemStyle = list( color = "#FFC502",
                                borderRadius = 5,
                                shadowColor = "black",
                                shadowBlur = 2), 
              label = list(show = FALSE,
                           fontWeight = "bold", 
                           formatter  =  htmlwidgets::JS("
                            function(params){
                              var value = params.value[1];
                              var parts = value.toString().split('.');
                              parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                              return parts.join('.');
                            }
                            ")),
              legend = TRUE)
    }
    
    # Verificar si mont_mensual está presente y agregarlo al gráfico si es así
    if ("mont_mensual" %in% colnames(datos)) {
      grafico <- grafico %>%
        e_bar(mont_mensual, 
              stack = "grp", 
              name = "Mensual", 
              itemStyle = list(color = "#073767",
                               borderRadius = 5,
                               shadowColor = "black",
                               shadowBlur = 5),
              label = list(show = FALSE,
                           fontWeight = "bold", 
                           formatter  = htmlwidgets::JS("
                            function(params){
                              var value = params.value[1];
                              var parts = value.toString().split('.');
                              parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                              return parts.join('.');
                            }
                            ")),
              legend = TRUE)
    }
    
    # Agregar la línea del total
    grafico <- grafico %>%
      e_line(total, 
             itemStyle = list(width=10, color = "#202C33"), 
             name = "Total",
             symbol = "diamond", 
             symbolSize = 10, 
             label = list(show = TRUE, 
                          position = "top", 
                          fontWeight = "bold", 
                          fontSize = tamano_num,
                          rotate = num_meses,
                          formatter  =  htmlwidgets::JS("
                              function(params){
                              var value = params.value[1];
                              var parts = value.toString().split('.');
                              parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                              return parts.join('.');
                            }
                              ")),
             lineStyle = list(width = 2)) %>% 
      e_tooltip(trigger = "axis") %>%  # Tooltip al pasar el mouse
      e_legend(show = TRUE, bottom = 0) %>%
      e_x_axis(type = "category") 
    
    # Renderizar el gráfico
    grafico
  })
  
  output$pie_genero <- renderEcharts4r({
    datos <- datos_genero()
    datos %>%
      e_charts(subcategoria) %>%
      e_pie(cantidad,label = list(
        formatter = htmlwidgets::JS("function(params) {
              return params.percent + '%';
            }"),
        position = "inside",
        fontStyle = "oblique",
        fontWeight = "bold"),
        color = c('#FFA500','#073767')) %>% 
      e_tooltip() %>%
      e_legend(orient = "vertical", 
               show = FALSE,
               right = "1%", 
               padding = c(10),
               textStyle = list(fontWeight = "bold", 
                                fontSize = 8)) %>% 
      e_title(text = paste0("Mes: ",mes_selector(unique(as.character(datos$mes)))), 
              left = "center", 
              top = "1%",
              textStyle = list(fontWeight = "bold")) %>% 
      e_grid(height = "10%", top = "10%")
  })
  
  output$barras_alta <- renderEcharts4r({
    datos <- datos_altas()
    
    datos %>% 
      e_charts(categoria) %>% 
      e_bar(cantidad, 
            stack = "grp", 
            seriesindex = ~categoria, 
            itemStyle = list( borderRadius = 5,
                              shadowColor = "black",
                              shadowBlur = 2), 
            label = list(show = TRUE,
                         fontWeight = "bold", 
                         fontSize = tamano_num + 2,
                         formatter  =  htmlwidgets::JS("
                            function(params){
                              var value = params.value[0];
                              var parts = value.toString().split('.');
                              parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                              return parts.join('.');
                            }
                            ")),
            legend = TRUE) %>% 
      e_legend(show = FALSE,
               bottom = 0) %>% 
      e_tooltip(trigger = "axis") %>%
      e_flip_coords() %>% 
      e_y_axis(axisLabel = list(rotate = 70))
  })
  
  output$lineas_inicio_emitido <- renderEcharts4r({
    datos <- datos_inicio_emitidos()
    
    datos %>% 
      e_charts(mes) %>%
      e_line(cant_inicio_tramite,
             name = "Inicio Tramite", 
             itemStyle = list(color = "#FFC502"),
             label = list(show = TRUE, fontSize = tamano_num),
             lineStyle = list(width = 2)) %>%
      e_line(cant_certificados_emitidos, 
             name = "Certificados Emitidos",
             itemStyle = list(color = "#073767"),
             label = list(show = TRUE, fontSize = tamano_num, position = "bottom"),
             lineStyle = list(width = 2)) %>%
      e_tooltip(trigger = "axis") %>%  
      e_legend(show = TRUE, bottom = 0) %>%
      e_x_axis(type = "category") %>%
      e_y_axis(axisLabel = list(rotate = 70)) 
  })
  
  selected_dept <- reactiveVal(NULL)
  
  output$mapa_bolivia <- renderLeaflet({
    datos <- datos_departamento_cc()
    bd_unida <<- merge(bolivia, datos, by.x = "woe_name", by.y = "subcategoria", all = TRUE)
    bd_unida <<- st_as_sf(bd_unida)
    
    output$info <- renderUI({
      
    })
    
    leaflet(data = bd_unida, options = leafletOptions(zoomControl = FALSE, dragging = FALSE,scrollWheelZoom = FALSE)) %>%
      addPolygons(
        fillColor = "#073767",
        weight = 1,
        color = "black",
        opacity = 1,
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          color = "white",
          weight = 2,
          bringToFront = TRUE
        ),
        layerId = ~woe_name  # Agrega el ID de capa para identificar los departamentos
      ) 
  })
  
  # Observador para capturar clics en el mapa
  observeEvent(input$mapa_bolivia_shape_click, {
    click_data <- input$mapa_bolivia_shape_click  # Captura los datos del clic
    
    # Extrae el departamento seleccionado a partir del ID de capa
    depto_seleccionado <- click_data$id
    
    # Filtra los datos del departamento seleccionado
    info_depto <- datos_departamento_cc() %>% filter(subcategoria == depto_seleccionado)
    
    # Si hay datos, obtenemos los meses únicos
    if (nrow(info_depto) > 0) {
      # Obtener meses únicos
      meses_disponibles <- unique(info_depto$mes)
      
      # Selecciona el último mes por defecto
      ultimo_mes <- tail(meses_disponibles, n = 1)
      
      # Actualiza el selectInput dinámico y selecciona el último mes por defecto
      output$info <- renderUI({
          selectInput("mes_seleccionado", "Selecciona Mes:", choices = meses_disponibles, selected = ultimo_mes)
      })
      
      # Actualizar el valor reactivo del departamento seleccionado
      prev_selected <- selected_dept()  # Recupera el departamento previamente seleccionado
      
      # Cambia el color del departamento previamente seleccionado de vuelta al color original
      if (!is.null(prev_selected) && prev_selected != depto_seleccionado) {
        leafletProxy("mapa_bolivia") %>%
          addPolygons(
            data = bd_unida %>% filter(woe_name == prev_selected),  # Filtrar el departamento previo
            fillColor = "#073767",  # Color original
            weight = 1,
            color = "black",
            opacity = 1,
            fillOpacity = 0.7,
            highlightOptions = highlightOptions(
              color = "white",
              weight = 2,
              bringToFront = TRUE
            ),
            layerId = ~woe_name
          )
      }
      
      # Cambia el color del nuevo departamento seleccionado
      leafletProxy("mapa_bolivia") %>%
        addPolygons(
          data = bd_unida %>% filter(woe_name == depto_seleccionado),  # Solo el departamento seleccionado
          fillColor = "#FFC502",  # Cambia el color del departamento clicado
          weight = 2,
          color = "black",
          opacity = 1,
          fillOpacity = 0.7,
          highlightOptions = highlightOptions(
            color = "white",
            weight = 2,
            bringToFront = TRUE
          ),
          layerId = ~woe_name
        )
      
      # Actualiza la variable reactiva con el nuevo departamento seleccionado
      selected_dept(depto_seleccionado)
      
      # Actualiza los datos del departamento para el mes seleccionado por defecto
      updateSelectInput(session, "mes_seleccionado", selected = ultimo_mes)
      
      # Filtrar y actualizar los datos en el HTML de acuerdo al mes seleccionado (por defecto el último)
      output$info_html <- renderUI({
        datos_filtrados <- info_depto %>% filter(mes == ultimo_mes)
        HTML(paste0(
          "<div style='text-align:center; font-size:30px; font-weight:bold;'> ", datos_filtrados$subcategoria[1], "</div>",
          "<p style='font-size:18px;'><strong>Monto desembolsado: </strong>", format(datos_filtrados$monto[1], big.mark = ".", decimal.mark = ","), " M Bs</p>",
          "<p style='font-size:18px;'><strong >N° de beneficiarios: </strong>", format(datos_filtrados$cantidad[1], big.mark = ".", decimal.mark = ","), "</p>"
        ))
      })
    } else {
      output$info <- renderUI({
        HTML("<div>No hay datos disponibles para el departamento seleccionado.</div>")
      })
    }
  })
  
  # Observador para actualizar los datos cuando se selecciona un mes
  observeEvent(input$mes_seleccionado, {
    # Filtra los datos según el departamento seleccionado y el mes
    depto_seleccionado <- selected_dept()
    info_depto <- datos_departamento_cc() %>%
      filter(subcategoria == depto_seleccionado, mes == input$mes_seleccionado)
    
    # Actualiza la salida HTML con los datos filtrados por el mes seleccionado
    output$info_html <- renderUI({
      if (nrow(info_depto) > 0) {
        HTML(paste0(
          "<div style='text-align:center; font-size:30px; font-weight:bold;'> ", info_depto$subcategoria[1], "</div>",
          "<p style='font-size:18px;'><strong>Monto desembolsado: </strong>", format(info_depto$monto[1], big.mark = ".", decimal.mark = ","), " M Bs</p>",
          "<p style='font-size:18px;'><strong >N° de beneficiarios: </strong>", format(info_depto$cantidad[1], big.mark = ".", decimal.mark = ","), "</p>"
        ))
      } else {
        HTML("<div>No hay datos disponibles para el mes seleccionado.</div>")
      }
    })
  })
  
  
  
# graficos para sistema de reparto
  
  output$beneficiarios_sr <- renderEcharts4r({
    datos <- cantidad_sr()
    num_meses <- if (length(unique(datos$mes)) > 7) 45 else 0
    
    # Crear gráfico de barras dinámicamente según las columnas presentes
    grafico <- datos %>%
      e_charts(mes)
    
    # Verificar si mont_global está presente y agregarlo al gráfico si es así
    if ("cant_titular" %in% colnames(datos)) {
      grafico <- grafico %>%
        e_bar(cant_titular, 
              stack = "grp", 
              name = "Titular", 
              itemStyle = list( color = "#FFC502",
                                borderRadius = 5,
                                shadowColor = "black",
                                shadowBlur = 2), 
              label = list(show = FALSE,
                           fontWeight = "bold", 
                           formatter  =  htmlwidgets::JS("
                            function(params){
                              var value = params.value[1];
                              var parts = value.toString().split('.');
                              parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                              return parts.join('.');
                            }
                            ")),
              legend = TRUE)
    }
    
    # Verificar si mont_mensual está presente y agregarlo al gráfico si es así
    if ("cant_derechohabiente" %in% colnames(datos)) {
      grafico <- grafico %>%
        e_bar(cant_derechohabiente, 
              stack = "grp", 
              name = "Derechohabiente", 
              itemStyle = list(color = "#073767",
                               borderRadius = 5,
                               shadowColor = "black",
                               shadowBlur = 5),
              label = list(show = FALSE,
                           fontWeight = "bold", 
                           formatter  = htmlwidgets::JS("
                            function(params){
                              var value = params.value[1];
                              var parts = value.toString().split('.');
                              parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                              return parts.join('.');
                            }
                            ")),
              legend = TRUE)
    }
    
    # Agregar la línea del total
    grafico <- grafico %>%
      e_line(total, 
             itemStyle = list(width=10, color = "#202C33"), 
             name = "Total",
             symbol = "diamond", 
             symbolSize = 10, 
             label = list(show = TRUE, 
                          position = "top", 
                          fontWeight = "bold", 
                          fontSize = tamano_num,
                          rotate = num_meses,
                          formatter  =  htmlwidgets::JS("
                              function(params){
                              var value = params.value[1];
                              var parts = value.toString().split('.');
                              parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                              return parts.join('.');
                            }
                              ")),
             lineStyle = list(width = 2)) %>% 
      e_tooltip(trigger = "axis") %>%  # Tooltip al pasar el mouse
      e_legend(show = TRUE, bottom = 0) %>%
      e_x_axis( type = "category")
    
    # Renderizar el gráfico
    grafico
  })
  
  output$montos_sr <- renderEcharts4r({
    datos <- monto_sr()
    num_meses <- if (length(unique(datos$mes)) > 7) 45 else 0
    
    # Crear gráfico de barras dinámicamente según las columnas presentes
    grafico <- datos %>%
      e_charts(mes)
    
    # Verificar si mont_global está presente y agregarlo al gráfico si es así
    if ("cant_titular" %in% colnames(datos)) {
      grafico <- grafico %>%
        e_bar(cant_titular, 
              stack = "grp", 
              name = "Titular", 
              itemStyle = list( color = "#FFC502",
                                borderRadius = 5,
                                shadowColor = "black",
                                shadowBlur = 2), 
              label = list(show = FALSE,
                           fontWeight = "bold", 
                           formatter  =  htmlwidgets::JS("
                            function(params){
                              var value = params.value[1];
                              var parts = value.toString().split('.');
                              parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                              return parts.join('.');
                            }
                            ")),
              legend = TRUE)
    }
    
    # Verificar si mont_mensual está presente y agregarlo al gráfico si es así
    if ("cant_derechohabiente" %in% colnames(datos)) {
      grafico <- grafico %>%
        e_bar(cant_derechohabiente, 
              stack = "grp", 
              name = "Derechohabiente", 
              itemStyle = list(color = "#073767",
                               borderRadius = 5,
                               shadowColor = "black",
                               shadowBlur = 5),
              label = list(show = FALSE,
                           fontWeight = "bold", 
                           formatter  = htmlwidgets::JS("
                            function(params){
                              var value = params.value[1];
                              var parts = value.toString().split('.');
                              parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                              return parts.join('.');
                            }
                            ")),
              legend = TRUE)
    }
    
    # Agregar la línea del total
    grafico <- grafico %>%
      e_line(total, 
             itemStyle = list(width=10, color = "#202C33"), 
             name = "Total",
             symbol = "diamond", 
             symbolSize = 10, 
             label = list(show = TRUE, 
                          position = "top", 
                          fontWeight = "bold", 
                          fontSize = tamano_num,
                          rotate = num_meses,
                          formatter  =  htmlwidgets::JS("
                              function(params){
                              var value = params.value[1];
                              var parts = value.toString().split('.');
                              parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                              return parts.join('.');
                            }
                              ")),
             lineStyle = list(width = 2)) %>% 
      e_tooltip(trigger = "axis") %>%  # Tooltip al pasar el mouse
      e_legend(show = TRUE, bottom = 0) %>%
      e_x_axis(type = "category") 
    
    # Renderizar el gráfico
    grafico
  })
  
  output$monto_beneficiario_sr <- renderEcharts4r({
    datos <- monto_cantidad_sr()
    datos %>% 
      e_charts(mes) %>% 
      e_bar(cant_total, 
            name = "Cantidad", 
            itemStyle = list(color =  "#073767",
                             borderRadius = 10,
                             shadowColor = "black",
                             shadowBlur = 5),
            stack = "grp") %>% 
      e_labels(show = FALSE,
               position = "inside", 
               fontWeight = "bold", 
               fontSize = 16,
               formatter  =  htmlwidgets::JS("
                function(params){
                  var value = params.value[1];
                  var parts = value.toString().split('.');
                  parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                  return parts.join('.');
                }
                ")) %>% 
      e_line(mont_total, 
             name = "Monto", 
             itemStyle = list(width=10, color =  "#FFC502"), 
             symbol = "diamond", 
             symbolSize = 12, 
             label = list(show = FALSE, 
                          position = "top", 
                          fontWeight = "bold", 
                          color =  "#FFC502",
                          fontSize = tamano_num,
                          formatter  =  htmlwidgets::JS("
                              function(params){
                                var value = params.value[1];
                                var parts = value.toString().split('.');
                                parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                                return parts.join('.');
                              }
                              ")),
             lineStyle = list(width = 3),
             y_index = 1) %>%
      # Agrega tooltips
      e_tooltip() %>%
      # Agrega función de descarga de imagen
      e_tooltip(trigger = "axis") %>%
      # Configura leyenda
      e_legend(orient = "horizontal", top = "bottom", padding = c(0,0)) %>% 
      e_animation(duration = 1500) 
  })
  
  output$genero_sr <- renderEcharts4r({
    datos <- genero_sr()
    datos %>%
      e_charts(subcategoria) %>%
      e_pie(cantidad,
            label = list(formatter = htmlwidgets::JS("function(params) {
                                                     return params.percent + '%';
                                                     }"),
                         position = "inside",
                         fontStyle = "oblique",
                         fontWeight = "bold"),
            color = c('#FFA500','#073767')) %>% 
      e_tooltip() %>%
      e_legend(orient = "vertical", 
               show = FALSE,
               right = "1%", 
               padding = c(10),
               textStyle = list(fontWeight = "bold", 
                                fontSize = 8)) %>% 
      e_title(text = paste0("Mes: ",mes_selector(unique(as.character(datos$mes)))), 
              left = "center", 
              top = "1%",
              textStyle = list(fontWeight = "bold")) %>% 
      e_grid(height = "10%", top = "10%")
  })
  
  output$anexo_sr <- renderEcharts4r({
    datos <- anexo_sr()
    
    datos %>% 
      e_charts(mes) %>% 
      e_pie(cantidad, 
            roseType = "radius",
            label = list(formatter = htmlwidgets::JS("function(params) {
                                                     return params.percent + '%';
                                                     }"),
                         position = "inside",
                         fontStyle = "oblique",
                         fontWeight = "bold")) %>% 
      e_tooltip() %>% 
      e_legend(bottom = 0,
               show = TRUE)
  })
  
  selected_dept_sr <- reactiveVal(NULL)
  
  output$mapa_bolivia_sr <- renderLeaflet({
    datos <- datos_departamento_sr()
    bd_unida <<- merge(bolivia, datos, by.x = "woe_name", by.y = "categoria", all = TRUE)
    bd_unida <<- st_as_sf(bd_unida)
    
    output$info1 <- renderUI({})
    output$tabla_municipios <- renderTable({})
    
    leaflet(data = bd_unida, options = leafletOptions(zoomControl = FALSE, dragging = FALSE,scrollWheelZoom = FALSE)) %>%
      addPolygons(
        fillColor = "#073767",
        weight = 1,
        color = "black",
        opacity = 1,
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          color = "white",
          weight = 2,
          bringToFront = TRUE
        ),
        layerId = ~woe_name  # Agrega el ID de capa para identificar los departamentos
      ) 
  })
  
  # Observador para capturar clics en el mapa
  observeEvent(input$mapa_bolivia_sr_shape_click, {
    click_data <- input$mapa_bolivia_sr_shape_click  # Captura los datos del clic
    
    # Extrae el departamento seleccionado a partir del ID de capa
    depto_seleccionado <- click_data$id
    
    # Encuentra los datos del departamento seleccionado en el data frame
    info_depto <- datos_departamento_sr() %>% filter(categoria == depto_seleccionado)
    
    if (nrow(info_depto) > 0) {
      # Obtener meses únicos del departamento seleccionado
      meses_disponibles <- unique(info_depto$Mes)
      
      # Selecciona el último mes como valor predeterminado
      ultimo_mes <- tail(meses_disponibles, n = 1)
      
      # Cambiar el color del departamento en el mapa
      
      prev_selected <- selected_dept_sr()  # Recupera el departamento previamente seleccionado
      if (!is.null(prev_selected)) {
        leafletProxy("mapa_bolivia_sr") %>%
          addPolygons(
            data = bd_unida %>% filter(woe_name == prev_selected),  # Filtrar el departamento previo
            fillColor = "#073767",  # Color original
            weight = 1,
            color = "black",
            opacity = 1,
            fillOpacity = 0.7,
            highlightOptions = highlightOptions(
              color = "white",
              weight = 2,
              bringToFront = TRUE
            ),
            layerId = ~woe_name
          )
      }
      
      # Cambiar el color del nuevo departamento seleccionado
      leafletProxy("mapa_bolivia_sr") %>%
        addPolygons(
          data = bd_unida %>% filter(woe_name == depto_seleccionado),  # Solo el departamento seleccionado
          fillColor = "#FFC502",  # Cambiar color del departamento seleccionado
          weight = 2,
          color = "black",
          opacity = 1,
          fillOpacity = 0.7,
          highlightOptions = highlightOptions(
            color = "white",
            weight = 2,
            bringToFront = TRUE
          ),
          layerId = ~woe_name
        )
      
      # Renderizar el selectInput, HTML y tabla juntos
      output$info1 <- renderUI({
          # SelectInput para los meses
          selectInput("mes_seleccionado_sr", "Selecciona Mes:", choices = meses_disponibles, selected = ultimo_mes)
      })
      
      # Filtrar y actualizar los datos en el HTML de acuerdo al mes seleccionado (por defecto el último)
      output$dep_data_sr <- renderUI({
        datos_filtrados <- info_depto %>%
          group_by(categoria, Mes) %>% 
          summarize(cantidad = sum(Cantidad), monto = sum(Monto), .groups = 'drop') %>% 
          filter(Mes == ultimo_mes)
        HTML(paste0(
          "<div style='text-align:center; font-size:30px; font-weight:bold;'> ", datos_filtrados$categoria[1], "</div>",
          "<p style='font-size:18px;'><strong>Monto desembolsado: </strong>", format(datos_filtrados$monto[1], big.mark = ".", decimal.mark = ","), " M Bs</p>",
          "<p style='font-size:18px;'><strong>N° de beneficiarios: </strong>", format(datos_filtrados$cantidad[1], big.mark = ".", decimal.mark = ","), "</p>"
        ))
      })
      
      # Filtrar y actualizar la tabla con el último mes seleccionado
      output$tabla_municipios <- renderTable({
        info_depto %>% 
          filter(Mes == ultimo_mes) %>% 
          select(-categoria, -Mes) %>% 
          mutate(Cantidad = format(round(Cantidad,0),big.mark = ".", decimal.mark = ","), Monto = format(round(Monto, 2), big.mark = ".", decimal.mark = ","))
      }, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'lrr')
      
      selected_dept_sr(depto_seleccionado)  # Actualiza el departamento seleccionado
      
      
    } else {
      output$info1 <- renderUI({
        HTML("<div>No hay datos disponibles para el departamento seleccionado.</div>")
      })
    }
  })
  
  # Observador para actualizar los datos y la tabla al seleccionar un mes
  observeEvent(input$mes_seleccionado_sr, {
    depto_seleccionado <- selected_dept_sr()  # Recuperar el departamento seleccionado
    
    # Actualiza la información del HTML según el mes seleccionado
    info_depto <- datos_departamento_sr() %>%
      filter(categoria == depto_seleccionado) %>% 
      group_by(categoria, Mes) %>% 
      summarize(cantidad = sum(Cantidad),monto = sum(Monto), .groups = 'drop') %>% 
      filter(Mes == input$mes_seleccionado_sr)
    
    output$dep_data_sr <- renderUI({
      if (nrow(info_depto) > 0) {
        HTML(paste0(
          "<div style='text-align:center; font-size:30px; font-weight:bold;'> ", info_depto$categoria[1], "</div>",
          "<p style='font-size:18px;'><strong>Monto desembolsado: </strong>", format(info_depto$monto[1], big.mark = ".", decimal.mark = ","), " M Bs</p>",
          "<p style='font-size:18px;'><strong>N° de beneficiarios: </strong>", format(info_depto$cantidad[1], big.mark = ".", decimal.mark = ","), "</p>"
        ))
      } else {
        HTML("<div>No hay datos disponibles para el mes seleccionado.</div>")
      }
    })
    
    output$tabla_municipios <- renderTable({
      datos_departamento_sr() %>% 
        filter(categoria == depto_seleccionado) %>% 
        filter(Mes == input$mes_seleccionado_sr) %>% 
        select(-categoria, -Mes) %>% 
        mutate(Cantidad = format(round(Cantidad,0),big.mark = ".", decimal.mark = ","), Monto = format(round(Monto, 2), big.mark = ".", decimal.mark = ","))
    }, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'lrr')
    
  })
  
  # graficos para UNIDAD JURIDICA
  
  output$indebidos_devengados <- renderEcharts4r({
    datos <- indebido_devengado()
    
    num_meses <- if (length(unique(datos$mes)) > 7) 45 else 0
    # Crear gráfico de barras con total acumulado
    grafico <- datos %>%
      e_charts(mes)
    
    # Verificar si mont_global está presente y agregarlo al gráfico si es así
    if ("cant_cobros_indebidos" %in% colnames(datos)) {
      grafico <- grafico %>%
        e_bar(cant_cobros_indebidos, 
              stack = "grp", 
              name = "Cobros Indebidos", 
              itemStyle = list( color = "#FFC502",
                                borderRadius = 5,
                                shadowColor = "black",
                                shadowBlur = 2), 
              label = list(show = FALSE,
                           fontWeight = "bold", 
                           formatter  =  htmlwidgets::JS("
                            function(params){
                              var value = params.value[1];
                              var parts = value.toString().split('.');
                              parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                              return parts.join('.');
                            }
                            ")),
              legend = FALSE)
    }
    
    # Verificar si mont_mensual está presente y agregarlo al gráfico si es así
    if ("cant_aportes_devengados" %in% colnames(datos)) {
      grafico <- grafico %>%
        e_bar(cant_aportes_devengados, 
              stack = "grp", 
              name = "Aportes Devengados", 
              itemStyle = list(color = "#073767",
                               borderRadius = 5,
                               shadowColor = "black",
                               shadowBlur = 5),
              label = list(show = FALSE,
                           fontWeight = "bold", 
                           formatter  = htmlwidgets::JS("
                            function(params){
                              var value = params.value[1];
                              var parts = value.toString().split('.');
                              parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                              return parts.join('.');
                            }
                            ")),
              legend = FALSE)
    }
    
    # Agregar la línea del total
    grafico <- grafico %>%
      e_line(total, 
             itemStyle = list(width=10, color = "#202C33"), 
             name = "Total",
             symbol = "diamond", 
             symbolSize = 10, 
             label = list(show = TRUE, 
                          position = "top", 
                          fontWeight = "bold", 
                          fontSize = tamano_num,
                          rotate = num_meses,
                          formatter  =  htmlwidgets::JS("
                              function(params){
                              var value = params.value[1];
                              var parts = value.toString().split('.');
                              parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                              return parts.join('.');
                            }
                              ")),
             lineStyle = list(width = 2)) %>% 
      e_tooltip(trigger = "axis") %>%  # Tooltip al pasar el mouse
      e_legend(show = TRUE, bottom = 0) %>%
      e_x_axis( type = "category") %>%
      e_y_axis( axisLabel = list(rotate = 60)) 
    
    # Renderizar el gráfico
    grafico
  })
  
  output$penales_uj <- renderEcharts4r({
    datos <- penales()
    
    num_meses <- if (length(unique(datos$mes)) > 7) 45 else 0
    # Crear gráfico de barras con total acumulado
    datos %>%
      e_charts(mes) %>%
        e_bar(cantidad, 
              stack = "grp", 
              name = "Recuperación",
              itemStyle = list(color = "#073767",
                               borderRadius = 5,
                               shadowColor = "black",
                               shadowBlur = 5),
              label = list(show = FALSE,
                           fontWeight = "bold", 
                           formatter  = htmlwidgets::JS("
                            function(params){
                              var value = params.value[1];
                              var parts = value.toString().split('.');
                              parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                              return parts.join('.');
                            }
                            ")),
              legend = FALSE) %>% 
      e_tooltip(trigger = "axis") %>%  # Tooltip al pasar el mouse
      e_legend(show = TRUE, bottom = 0) %>%
      e_x_axis( type = "category") %>%
      e_y_axis( axisLabel = list(rotate = 60)) 
  })
  
  # graficos para FISCALIZACION
  
  output$empresas <- renderEcharts4r({
    datos <- empresa()
    names <- colnames(datos)
    new_names <- names
    new_names[3] <- "cantidad_1"
    new_names[4] <- "cantidad_2"
    colnames(datos) <- new_names
    
    names <- tools::toTitleCase(gsub("^cant_", "", names))
    
    num_meses <- if (length(unique(datos$mes)) > 7) 45 else 0
    
    grafico <- datos %>%
      e_charts(mes)
    
    # Verificar si mont_global está presente y agregarlo al gráfico si es así
    if ("cantidad_1" %in% colnames(datos)) {
      grafico <- grafico %>%
        e_bar(cantidad_1, 
              stack = "grp", 
              name = names[3], 
              itemStyle = list( color = "#FFC502",
                                borderRadius = 5,
                                shadowColor = "black",
                                shadowBlur = 2), 
              label = list(show = FALSE,
                           fontWeight = "bold", 
                           formatter  =  htmlwidgets::JS("
                            function(params){
                              var value = params.value[1];
                              var parts = value.toString().split('.');
                              parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                              return parts.join('.');
                            }
                            ")),
              legend = TRUE)
    }
    
    # Verificar si mont_mensual está presente y agregarlo al gráfico si es así
    if ("cantidad_2" %in% colnames(datos)) {
      grafico <- grafico %>%
        e_bar(cantidad_2, 
              stack = "grp", 
              name = names[4], 
              itemStyle = list(color = "#073767",
                               borderRadius = 5,
                               shadowColor = "black",
                               shadowBlur = 5),
              label = list(show = FALSE,
                           fontWeight = "bold", 
                           formatter  = htmlwidgets::JS("
                            function(params){
                              var value = params.value[1];
                              var parts = value.toString().split('.');
                              parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                              return parts.join('.');
                            }
                            ")),
              legend = TRUE)
    }
    
    # Agregar la línea del total
    grafico <- grafico %>%
      e_line(total, 
             itemStyle = list(width=10, color = "#202C33"), 
             name = "Total",
             symbol = "diamond", 
             symbolSize = 10, 
             label = list(show = TRUE, 
                          position = "top", 
                          fontWeight = "bold", 
                          fontSize = tamano_num,
                          rotate = num_meses,
                          formatter  =  htmlwidgets::JS("
                              function(params){
                              var value = params.value[1];
                              var parts = value.toString().split('.');
                              parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                              return parts.join('.');
                            }
                              ")),
             lineStyle = list(width = 2)) %>% 
      e_tooltip(trigger = "axis") %>%  # Tooltip al pasar el mouse
      e_legend(show = TRUE, bottom = 0) %>%
      e_x_axis( type = "category")
    
    # Renderizar el gráfico
    grafico
    
  }) 
  
  output$comportamientos <- renderEcharts4r({
    datos <- comportamiento()
    datos %>% 
      e_charts(mes) %>% 
      e_bar(cant_fiscalizaciones, 
            name = "Fiscalizaciones", 
            itemStyle = list(color =  "#073767",
                             borderRadius = 10,
                             shadowColor = "black",
                             shadowBlur = 5),
            stack = "grp") %>% 
      e_labels(show = FALSE,
               position = "inside", 
               fontWeight = "bold", 
               fontSize = 16,
               formatter  =  htmlwidgets::JS("
                function(params){
                  var value = params.value[1];
                  var parts = value.toString().split('.');
                  parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                  return parts.join('.');
                }
                ")) %>% 
      e_line(cant_recuperacion_bs, 
             name = "Recuperación", 
             itemStyle = list(width=10, color =  "#FFC502"), 
             symbol = "diamond", 
             symbolSize = 12, 
             label = list(show = FALSE, 
                          position = "top", 
                          fontWeight = "bold", 
                          color =  "#FFC502",
                          fontSize = tamano_num,
                          formatter  =  htmlwidgets::JS("
                              function(params){
                                var value = params.value[1];
                                var parts = value.toString().split('.');
                                parts[0] = parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                                return parts.join('.');
                              }
                              ")),
             lineStyle = list(width = 3),
             y_index = 1) %>%
      # Agrega tooltips
      e_tooltip() %>%
      # Agrega función de descarga de imagen
      e_tooltip(trigger = "axis") %>%
      # Configura leyenda
      e_legend(orient = "horizontal", top = "bottom", padding = c(0,0)) %>% 
      e_animation(duration = 1500) 
  })
}
