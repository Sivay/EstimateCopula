#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

####### Instalation des différents packets nécéssaires : 
    list.of.packages <- c("shiny","shinydashboard","DT")
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    if(length(new.packages)) install.packages(new.packages)
    lapply(list.of.packages,function(x){library(x,character.only=TRUE)}) 
    
    
    

####### UI
ui <- ui <- dashboardPage(
  dashboardHeader(title = "Estimtion de copules bivariées", disable=TRUE),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Input des data", tabName="InputData", icon=icon("dashboard")),
      menuItem("Mesures de dépendances", tabName="MesureDep", icon=icon("th")),
      menuItem("Choix de la copule", tabName="ChoixCopule", icon=icon("th")),
      menuItem("Résultats", tabName="Resultat", icon=icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName="InputData",
        fluidRow(
          #Selector for file upload
          column(4,
            column(6,
                  fileInput('datafile', 'Choisissez un fichier csv', accept=c('text/csv', 'text/comma-separated-values,text/plain'))
            ),
            column(6,
                  selectInput("selectRows","Selectionnez les deux colonnes corespondant aux données à modéliser :","",multiple=TRUE)
            ),
            span(textOutput("isBivariate"), style="color:red")
          ),
          column(8,
                 p("Summary du data.frame : "),
                 verbatimTextOutput("resume",placeholder = TRUE)
          )
        ),
        fluidRow(
          column(1),
          column(10,
              h2("Vos données sont les suivantes :"),
              dataTableOutput("filetable")
          ),
          column(1)
        )
      ),
      tabItem(tabName="MesureDep"
      ),
      tabItem(tabName="ChoixCopule"
      ),
      tabItem(tabName="Resultat"
      )
    )
  )
)

####### Serveur
server <- function(input, output, session) {
  
  ############### Partie récupération des data et gestion du nombre de variables. 
  
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    return(data.frame(read.csv(infile$datapath)))
  })                    # Correspond au fichier uploader par l'utilisateur 
  data <- reactive({
    if(is.null(filedata)){
      return(NULL)
    }
    else{
        return(filedata()[,input$selectRows])
    }
  })                        # Corepond au data.frame uploader l=par l'utilisateur avec colonnes selectionées.
  OKnombreCol <- reactive({
    if(is.null(input$datafile)){
      return(NULL)
    } else {
      return(ncol(data()) == 2)
    }
  })                 # Check que le nombre de colonne est le bon.
  output$isBivariate <- renderText({
    if(!is.null(input$datafile)){
      if (!OKnombreCol()){
        return("Le programme ne traite que des copules bivariées, merci de chosir ci-contre les deux colonnes que vous souhaitez utiliser")
      }
      else{
        return("Vos données sont bien de dimention 2, Vous pouvez passer à l'onglet suivant.")
      }
    }
  })        # Permet d'afficher un message rouge si les données ne sont pas bivariées.
  output$filetable <- DT::renderDataTable({
    DT::datatable(data(),options=list(pageLength = 10 ))
  })     # Permet d'output les data sous forme de dataTable.
  observe({
    updateSelectInput(session,"selectRows",choices=names(filedata()),selected=names(filedata()))
  })                                 # permet d'udater le selectInput qui choisis les colonnes du data.frame a prendre.
  output$resume <- renderPrint({
    summary(data())
  })
  ############### Partie Calculs et graphique des différente smesures de dépendances.
  
  
  
  
   set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })

}

####### Running the app.
shinyApp(ui = ui, server = server)

