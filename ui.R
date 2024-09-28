library(leaflet)
#########################################################################################################
##########################################      HEADER     ##############################################
#########################################################################################################

header <- dashboardHeader(title = "SENASIR")

#########################################################################################################
##########################################     SIDEBAR     ##############################################
#########################################################################################################

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Visualización", tabName = "visualizacion", icon = icon("chart-bar")),
    
    # SelectInput para seleccionar la gestión
    selectInput("gestion", "Seleccione la Gestión:", 
                choices = c("2021", "2022", "2023", "2024"),
                selected = "2024"),
    
    # SliderInput para seleccionar el mes
    sliderInput("mes", "Seleccione el Mes:",
                min = 1, max = 14, value = c(1, 14), step = 1),
    
    # Nuevo menú contraíble para Compensación
    menuItem("Ccompensación \n Cotizaciones", icon = icon("balance-scale"),
             selectInput("compensacion", "Seleccione la Compensación:", 
                         choices = c("Todos", "Global", "Mensual"),
                         selected = "Todos")
    ),
    # Nuevo menú contraíble para sistema de reparto
    menuItem("Sistema de Reparto", icon = icon("cogs"),
             checkboxGroupInput("categorias_check", "Selecciona Categorías:",
                                choices = setNames(categorias, categorias_labels),
                                selected = c("regular", "pago a domicilio")),
             selectInput("tipo_sr", "Seleccione el tipo:", 
                         choices = setNames(subcategorias, subcategorias_labels),
                         selected = "todos")
    ),
    # Nuevo menú contraíble para JURIDICA
    menuItem("Juridica", icon = icon("gavel"),
             selectInput("tipo_empresa", "Seleccione:", 
                         choices = setNames(categorias_uj, categorias_labels_uj),
                         selected = "todos")
    ),
    menuItem("Fiscalización", icon = icon("search"),
             selectInput("filtro_empresa", "Seleccione:", 
                         list(
                           "Por Tamaño" = "tamaño",
                           "Por Sector" = "sector"
                         ),
                         selected = "tamaño")
    )
    
  )
)

#########################################################################################################
##########################################       BODY      ##############################################
#########################################################################################################
custom_css <- "
  .box {
    margin-bottom: 5px;  /* Reducir el margen inferior */
  }
  .row {
    margin-bottom: 5px;  /* Reducir el margen entre filas */
  }
  .skin-blue .main-header .logo {
    background-color: #1B2D4B; /* Azul */
  }
  .skin-blue .main-header .navbar {
    background-color: #1B2D5B; /* Azul */
  }
  .skin-blue .main-sidebar {
    background-color: #1B2D5B; 
  }

 /* Estilos para fijar el header y el sidebar */
@media (min-width: 960px) {
  .main-header {
    position: fixed;
    z-index: 999;
    width: 100%; /* Asegurar que el header cubre toda la anchura */
    top: 0; /* Asegurar que está en la parte superior */
    left: 0; /* Alinear a la izquierda de la pantalla */
  }

  .main-sidebar {
    position: fixed;
    top: 0px; /* Dejar espacio para el header */
    left: 0;
    bottom: 0;
    height: auto;
    overflow-y: auto; /* Habilita el desplazamiento dentro del sidebar si es necesario */
    z-index: 998; /* Menor que el header para mantener el foco visual */
  }

 .content-wrapper {
    margin-top: 50px; /* Ajustar según la altura del header */
    margin-left: 230px; /* Ajustar según el ancho del sidebar */
    overflow-x: hidden; /* Ocultar el desbordamiento horizontal */
  }

 .main-sidebar {
    height: 100vh; /* Altura completa de la ventana de visualización */
    overflow-y: auto; /* Habilita el desplazamiento dentro del sidebar si es necesario */
 }
}

  .box-primary {
    border-top-color: #1B2D5B; /* Azul */
  }
  .content-wrapper, .right-side {
    background-color: #F0F0F0; /* Blanco */
  }
  .main-header .logo {
    color: #D6BD48 !important;  /* Cambia el color del título */
    font-weight: bold;          /* Opción para hacer el texto más visible */
  }

  .small-box .inner h3 {
        font-size: 24px;  /* Cambia este valor al tamaño que desees */
        padding-top: 10px;
        padding-bottom: 10px;
  }
  .small-box .inner p {
        font-size: 11px;  /* Ajusta el tamaño de la fuente del subtítulo */
        font-weight:bold;
        color:black;
  }

 /* Cambiar el color de fondo para las pestañas no activas */
  .nav-tabs-custom > .nav-tabs > li {
     background-color: #ddd; /* Gris claro para pestañas no activas */
  }

 /* Estilos CSS para el footer */
        .footer {
          width: 100%;
          background-color: #1B2D5B; /* Color de fondo del footer */
          color: white; /* Color del texto */
          text-align: center;
          padding: 10px 0;
          border-top: 1px solid #e7e7e7; /* línea de separación opcional */
        }
  #mapa_bolivia {
    background-color: white;
  }

  #mapa_bolivia_sr {
    background-color: white;
  }

"

body <- dashboardBody(
  useShinyjs(),  # Inicializa shinyjs
  tags$head(tags$style(HTML(custom_css))),
  
  fluidRow(
    valueBoxOutput("widget1", width = 2),
    valueBoxOutput("widget2", width = 2),
    valueBoxOutput("widget3", width = 2),
    valueBoxOutput("widget4", width = 2),
    valueBoxOutput("widget5", width = 2),
    valueBoxOutput("widget6", width = 2)
  ),
  fluidRow(
    tabBox(
      title = "",
      id = "tabset1", width = 12,
      
      tabPanel("COMPENSACIÓN DE COTIZACIONES",
               fluidRow(
                 box(title = "N° DE BENEFICIARIOS EN CURSO DE PAGO", status = "primary", solidHeader = FALSE, width = 6,
                     echarts4rOutput("barras_gestion", height = "350px") %>% withSpinner(color = "#0dc5c1")
                 ),
                 box(title = "MONTO DESEMBOLSADO EN MILLONES DE BOLIVIANOS", status = "warning", solidHeader = FALSE, width = 6,
                     echarts4rOutput("barras_monto", height = "350px") %>% withSpinner(color = "#0dc5c1")
                 )
               ),
               fluidRow(
                 box(title = "N° DE BENEFICIARIOS POR GÉNERO", status = "primary", solidHeader = FALSE, width = 3,
                     echarts4rOutput("pie_genero", height = "350px") %>% withSpinner(color = "#0dc5c1")
                 ),
                 box(title = "INICIO DE TRAMITES VS CERTIFICADOS EMITIDOS", status = "success", solidHeader = FALSE, width = 5,
                     echarts4rOutput("lineas_inicio_emitido", height = "350px") %>% withSpinner(color = "#0dc5c1")
                     
                 ),
                 box(title = "ALTAS ", status = "info", solidHeader = FALSE, width = 4,
                     echarts4rOutput("barras_alta", height = "350px") %>% withSpinner(color = "#0dc5c1")
                 )
               ),
               fluidRow(
                 box(title = "DEPARTAMENTO", status = "danger", solidHeader = FALSE, width = 8,
                     leafletOutput("mapa_bolivia", height = "700px") %>% withSpinner(color = "#0dc5c1")
                 ),
                 box(title = "", status = "warning", solidHeader = FALSE, width = 4,
                     uiOutput("info") %>% withSpinner(color = "#0dc5c1"),
                     htmlOutput("info_html")
                 )
               )
      ),
      
      tabPanel("SISTEMA DE REPARTO",
               fluidRow(
                 box(title = "N° DE BENEFICIARIOS DE RENTAS Y PENSIONES VITALICIAS", status = "primary", solidHeader = FALSE, width = 6,
                     echarts4rOutput("beneficiarios_sr", height = "350px") %>% withSpinner(color = "#0dc5c1")
                 ),
                 box(title = "MONTO DESEMBOLSADO EN MILLONES DE BOLIVIANOS", status = "primary", solidHeader = FALSE, width = 6,
                     echarts4rOutput("montos_sr", height = "350px") %>% withSpinner(color = "#0dc5c1")
                 )
               ),
               fluidRow(
                 box(title = "N° DE BENEFICIARIOS POR GÉNERO", status = "primary", solidHeader = FALSE, width = 3,
                     echarts4rOutput("genero_sr", height = "350px") %>% withSpinner(color = "#0dc5c1")
                 ),
                 box(title = "N° DE BENEFICIARIOS vs MONTO DESEMBOLSADO EN MILLONES", status = "primary", solidHeader = FALSE, width = 5,
                     echarts4rOutput("monto_beneficiario_sr", height = "350px") %>% withSpinner(color = "#0dc5c1")
                     
                 ),
                 box(title = "FORMULARIO ÚNICO DE AUTORIZACIÓN DE PAGO (ANEXO 15) ", status = "primary", solidHeader = FALSE, width = 4,
                     echarts4rOutput("anexo_sr", height = "350px") %>% withSpinner(color = "#0dc5c1")
                 )
               ),
               fluidRow(
                 box(title = "DEPARTAMENTO", status = "danger", solidHeader = FALSE, width = 8,
                     leafletOutput("mapa_bolivia_sr", height = "700px") %>% withSpinner(color = "#0dc5c1")
                 ),
                 box(title = "", status = "warning", solidHeader = FALSE, width = 4,
                     uiOutput("info1", height = "100px") ,
                     # HTML que muestra la información del departamento
                     htmlOutput("dep_data_sr"),
                     div(style = "overflow-x: auto; overflow-y: hidden; width: 100%;", 
                         tableOutput("tabla_municipios") 
                     )
                 )
               )
      ),
      
      tabPanel("JURIDICA Y FISCALIZACIÓN",
               fluidRow(
                 box(title = "COBROS INDEBIDOS Y APORTES DEVENGADOS", status = "primary", solidHeader = FALSE, width = 6,
                     echarts4rOutput("indebidos_devengados", height = "350px") %>% withSpinner(color = "#0dc5c1")
                 ),
                 box(title = "PROCESOS PENALES", status = "primary", solidHeader = FALSE, width = 6,
                     echarts4rOutput("penales_uj", height = "350px") %>% withSpinner(color = "#0dc5c1")
                 )
               ),
               fluidRow(
                 box(title = "NÚMERO DE FISCALIZACIONES A EMPRESAS", status = "primary", solidHeader = FALSE, width = 6,
                     echarts4rOutput("empresas", height = "350px") %>% withSpinner(color = "#0dc5c1")
                 ),
                 box(title = "COMPORTAMIENTO EN N° DE FISCALIZACIONES Y MONTO DE RECUPERACIÓN", status = "primary", solidHeader = FALSE, width = 6,
                     echarts4rOutput("comportamientos", height = "350px") %>% withSpinner(color = "#0dc5c1")
                 )
               )
      )
    )
  ),
  tags$div(class = "footer", "© 2024 SENASIR - Todos los Derechos Reservados - Elaborado por el Área de Planificación")
)

#########################################################################################################
##########################################       PAGE      ##############################################
#########################################################################################################
ui <- dashboardPage(header, sidebar, body)
