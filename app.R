#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

##En primer lugar, cargamos los paquetes necesarios


## Shiny es un paquete R que facilita la creación de aplicaciones web interactivas directamente desde R. 
## Puede alojar aplicaciones independientes en una página web o incrustarlas en documentos R Markdown o 
## crear paneles. También puede ampliar sus aplicaciones Shiny con temas CSS, widgets html y acciones de JavaScript.

library(shiny)

## VennDiagram incluye un conjunto de funciones para generar diagramas de Venn y Euler de alta resolución. 
## Incluye el manejo de varios casos especiales, incluido el escalado de dos casos y una amplia personalización 
## de la forma y estructura de la trama.

library(VennDiagram)

## Shinythemes incluye numerosas apariencias para nuestra aplicacion Shiny.

library(shinythemes)


## La shiny app consta de dos partes: la UI y el server. 

## Definimos en primer lugar la UI de la aplicación.

ui <- fluidPage(
    
     theme = shinytheme("cerulean"),
    
    ## Elegimos un título para la aplicación
      
       titlePanel("Diagrama de Venn"),
    
    
    ## La función sidebarLayout diseña una barra lateral y un area principal.
    
    sidebarLayout(
        ## Obtenemos así una barra lateral con una entrasa de control deslizante para el número de contenedores. Estos corresponderían
        ## con los imputs.
        sidebarPanel(
            textAreaInput("genesset1", "Genes set 1", value = " ", rows = 5, placeholder = "Copy the gene set here"),
            textInput("set1", "Enter genes set name"),
            textAreaInput("genesset2", "Genes set 2", value = " ", rows = 5, placeholder = "Copy the gene set here"),
            textInput("set2", "Enter genes set name"),
            downloadButton("imageVenn2", "Download the plot as an image")
        ),
        ## Mostramos un plot de la distribución generada.
        
        ## MainPanel continene los outputs
        mainPanel(
            h3("Venn Diagram for 2 genes sets"),
            plotOutput("VennDiagram2")
        )
        
    ),
    
    ## La función tag$hr añade una linea horizontal en la página web
    
    tags$hr(),
    
    ## La función sidebarLayout diseña una barra laetarl y un area principal.
    sidebarLayout(
        ## Obtenemos así una barra lateral con una entrasa de control deslizante para el número de contenedores. Estos corresponderían
        ## con los imputs.
        sidebarPanel(
            textAreaInput("genes1", "Genes set 1", value = " ", rows = 5, placeholder = "Copy the gene set here"),
            textInput("set3", "Enter genes set name"),
            textAreaInput("genes2", "Genes set 2", value = " ", rows = 5, placeholder = "Copy the gene set here"),
            textInput("set4", "Enter genes set name"),
            textAreaInput("genes3", "Genes set 3", value = " ", rows = 5, placeholder = "Copy the gene set here"),
            textInput("set5", "Enter genes set name"),
            downloadButton("imageVenn3", "Download the plot as an image")
        ),
        ## Mostramos un plot de la distribución generada.
        
        ## MainPanelcontinene los outputs
        mainPanel(
            ## h3 incorpora una cabecera en el plot con el nombre que le demos.
            h3("Venn Diagram for 3 genes sets"),
            plotOutput("VennDiagram3")
        )
    ),
    
    ## La función tag$hr añade una linea horizontal en la página web
    
    tags$hr(),
    
    ## La función sidebarLayout diseña una barra laetarl y un area principal.
    sidebarLayout(
        ## Obtenemos así una barra lateral con una entrasa de control deslizante para el número de contenedores. Estos corresponderían
        ## con los imputs.
        sidebarPanel(
            textAreaInput("data1", "Genes set 1", value = " ", rows = 5, placeholder = "Copy the gene set here"),
            textInput("set6", "Enter genes set name"),
            textAreaInput("data2", "Genes set 2", value = " ", rows = 5, placeholder = "Copy the gene set here"),
            textInput("set7", "Enter genes set name"),
            textAreaInput("data3", "Genes set 3", value = " ", rows = 5, placeholder = "Copy the gene set here"),
            textInput("set8", "Enter genes set name"),
            textAreaInput("data4", "Genes set 4", value = " ", rows = 5, placeholder = "Copy the gene set here"),
            textInput("set9", "Enter genes set name"),
            downloadButton("imageVenn4", "Download the plot as an image")
        ),
        ## Mostramos un plot de la distribución generada.
        
        ## MainPanelcontinene los outputs
        mainPanel(
            ## h3 incorpora una cabecera en el plot con el nombre que le demos.
            h3("Venn Diagram for 4 genes sets"),
            plotOutput("VennDiagram4")
        )
    )
    
    
)

## El siguiente paso es feniri la segunda parte de la app: el server.

server <- function(input, output) {
    
    ## Para crear cadenas vamos a utilizar la funcion strsplit que divide los elementos
    ## de un vector de caracetres x en subcadenas de acuerdo con las coincidencias con la subcadena dividida dentro de ellos.
    
    ## A fin de simplificar la lista, vamos a utilizar la función unlist.
    
    ##Vamos a utilizar expresiones reactivas. Las expresiones reactivas permiten cuando controlar la actualización de las distintas partes de la aplicación
    ##evitando así calculos innecesarios que pueden relentizar la aplicación.
    
    a = reactive(unique(unlist(strsplit(x = input$genesset1, split = "\n"))))
    b = reactive(unique(unlist(strsplit(x = input$genesset2, split = "\n"))))
    ab = reactive(unique(intersect(a(), b())))
    a2 = reactive(unique(unlist(strsplit(x = input$genes1, split = "\n"))))
    b2 = reactive(unique(unlist(strsplit(x = input$genes2, split = "\n"))))
    ab2 = reactive(unique(intersect(a2(), b2())))
    d = reactive(unique(unlist(strsplit(x = input$genes3, split = "\n"))))
    ad = reactive(unique(intersect(a2(), d())))
    bd = reactive(unique(intersect(b2(), d())))
    abd = reactive(unique(intersect(intersect(a2(), b2()), d())))
    a3 = reactive(unique(unlist(strsplit(x = input$data1, split = "\n"))))
    b3 = reactive(unique(unlist(strsplit(x = input$data2, split = "\n"))))
    ab3 = reactive(unique(intersect(a3(), b3())))
    d2 = reactive(unique(unlist(strsplit(x = input$data3, split = "\n"))))
    ad2 = reactive(unique(intersect(a3(), d2())))
    bd2 = reactive(unique(intersect(b3(), d2())))
    abd2 = reactive(unique(intersect(intersect(a3(), b3()), d2())))
    e = reactive(unique(unlist(strsplit(x = input$data4, split = "\n"))))
    ae = reactive(unique(intersect(a3(), e())))
    be = reactive(unique(intersect(b3(), e())))
    ed = reactive(unique(intersect(e(), d2())))
    abe = reactive(unique(intersect(intersect(a3(), b3()), e())))
    aed = reactive(unique(intersect(intersect(a3(), e()), d2())))
    bed = reactive(unique(intersect(intersect(b3(), e()), d2())))
    abed = reactive(unique(intersect(intersect(a3(), b3()), intersect(e(), d2()))))
    
    set1 = reactive(input$set1)
    set2 = reactive(input$set2)
    set3 = reactive(input$set3)
    set4 = reactive(input$set4)
    set5 = reactive(input$set5)
    set6 = reactive(input$set6)
    set7 = reactive(input$set7)
    set8 = reactive(input$set8)
    set9 = reactive(input$set9)
    
    output$VennDiagram2 = renderPlot({draw.pairwise.venn(length(a()), length(b()), length(ab()), col = c("red", "blue"), fill = c("red", "blue"), alpha = rep(0.25, 2), category = c(set1(), set2()), 
                                                         cat.cex = rep(2,2), print.mode = c("raw", "percent"))})
    
    output$VennDiagram3 = renderPlot ({draw.triple.venn(length(a2()), length(b2()), length(d()), length(ab2()), length(bd()), length(ad()), length(abd()),
                                                        col = c("red", "blue", "green"), fill = c("red", "blue", "green"), alpha = rep(0.25, 3),category = c(set3(), set4(), set5()), cat.cex = rep(2,3), print.mode = c("raw", "percent"))})
    
    output$VennDiagram4 = renderPlot ({draw.quad.venn(length(a3()), length(b3()), length(d2()), length(e()), length(ab3()), length(ad2()), length(ae()), length(bd2()), length(be()), length(ed()), length(abd2()), length(abe()), length(aed()), length(bed()), length(abed()),   
                                                      col = c("red", "blue", "green", "yellow"), fill = c("red", "blue", "green", "yellow"), alpha = rep(0.25, 4), category = c(set6(), set7(), set8(), set9()), cat.cex = rep(2,4), print.mode = c("raw", "percent"))})
    output$imageVenn2 <- downloadHandler(
       filename = "VennDiagram.png",
       content = function(file) {
            png(file)
            print(draw.pairwise.venn(length(a()), length(b()), length(ab()), col = c("red", "blue"), fill = c("red", "blue"), alpha = rep(0.25, 2), category = c(set1(), set2()), 
                                     cat.cex = rep(2,2)))
            dev.off()
        }
    )
    output$imageVenn3 <- downloadHandler(
        filename = "VennDiagram.png",
        content = function(file) {
            png(file)
            print(draw.triple.venn(length(a2()), length(b2()), length(d()), length(ab2()), length(bd()), length(ad()), length(abd()),
                                   col = c("red", "blue", "green"), fill = c("red", "blue", "green"), alpha = rep(0.25, 3),category = c(set3(), set4(), set5()), cat.cex = rep(2,3)))
            dev.off()
        }
    )
    output$imageVenn4 <- downloadHandler(
      filename = "VennDiagram.png",
      content = function(file) {
        png(file)
        print(draw.quad.venn(length(a3()), length(b3()), length(d2()), length(e()), length(ab3()), length(ad2()), length(ae()), length(bd2()), length(be()), length(ed()), length(abd2()), length(abe()), length(aed()), length(bed()), length(abed()),   
                             col = c("red", "blue", "green", "yellow"), fill = c("red", "blue", "green", "yellow"), alpha = rep(0.25, 4), category = c(set6(), set7(), set8(), set9()), cat.cex = rep(2,4)))
        dev.off()
      }
    )
}



## Por ultimo, corremos la aplicacion.

shinyApp(ui = ui, server = server)

