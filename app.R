#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

####### Instalation des différents packets nécéssaires : 
    list.of.packages <- c("shiny","shinydashboard","DT","fitdistrplus")
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    if(length(new.packages)) install.packages(new.packages)
    lapply(list.of.packages,function(x){library(x,character.only=TRUE)}) 
    
    
    

####### UI
ui <- ui <- dashboardPage(
  dashboardHeader(title = "Estimtion de copules bivariées", disable=TRUE),
  dashboardSidebar(
    sidebarMenuOutput("Menu")
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName="InputData",
        fluidRow(
          column(3,h5(tags$b("1. Choississez un fichier CSV de données"))),
          column(3,h5(tags$b("2. Selectionnez 2 colonnes à modéliser"))),
          column(3,h5(tags$b("3. Renomez eventuellement les colonnes"))),
          column(3,h5(tags$b("4. Petit summary de vos données ")))
        ),
        fluidRow(
          column(9,
            column(4,
                  fileInput(inputId = 'datafile',label = "",accept=c('text/csv', 'text/comma-separated-values,text/plain'))
            ),
            column(4,
                  selectInput(inputId = "selectRows",label="",choices = "",multiple=TRUE)
            ),
            column(4,
                   uiOutput(outputId = "renamer")
            ),
            span(textOutput("isBivariate"), style="color:red")
          ),
          column(3,
                 verbatimTextOutput("resume",placeholder = TRUE)
          )
        ),
        h2("Vos données sont les suivantes :"),
        fluidRow(
          column(1),
          column(10,dataTableOutput("finalDataOut")),
          column(1)
        )
      ),
      tabItem(tabName="MesureDep",
        fluidRow(
          column(4,
                 
            h5(tags$b("Summary de vos données ")),
            verbatimTextOutput("resume2",placeholder = TRUE),
            fluidRow(
              column(8,
                     h5(tags$b("Coeffiencients de corélation linéaire de Pearson :")),
                     verbatimTextOutput("pearsonr",placeholder = TRUE),
                     h5(tags$b("Tau de Kendall")),
                     verbatimTextOutput("kendaltau",placeholder = TRUE),
                     h5(tags$b("Rho de Spearman")),
                     verbatimTextOutput("spearmanrho",placeholder = TRUE)
              ),
              column(4,
                     textOutput("correlations"),
                     div(id="SurvieDiv",
                      checkboxInput("Survie", "Passer en copule Comonotone", value = FALSE, width = NULL)
                     )
              )
            ),
            h5(tags$b("Kolmogorov-Smirnov Test on raw data")),
            verbatimTextOutput("ksTest",placeholder = TRUE),
            h5(tags$b("Kolmogorov-Smirnov Test on standardized data")),
            verbatimTextOutput("ksTest01",placeholder = TRUE)
            
          ),
          column(8,
            column(6,
              h5(tags$b("A propos de la première marginale...")),
             plotOutput("hist1"),
             plotOutput("descdistX"),
             plotOutput("qqplot")
            ),
            column(6,
              h5(tags$b("A propos de la seconde marginale...")),
              plotOutput("hist2"),
              
              plotOutput("descdistY")
            )
          )
        )
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
  })                            # Correspond au fichier uploader par l'utilisateur 
  dataSelected <- reactive({
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
      return(ncol(dataSelected()) == 2)
    }
  })                         # Check que le nombre de colonne est le bon.
  output$isBivariate <- renderText({
    if(!is.null(input$datafile)){
      if (!OKnombreCol()){
        return("Le programme ne traite que des copules bivariées, merci de chosir ci-contre les deux colonnes que vous souhaitez utiliser")
      }
      else{
        return("Vos données sont bien de dimention 2, vous pouvez passer à l'onglet suivant.")
      }
    }
  })                # Permet d'afficher un message rouge si les données ne sont pas bivariées.
  output$filetable <- DT::renderDataTable({
    DT::datatable(dataSelected(),options=list(pageLength = 10 ))
  })         # Permet d'output les data sous forme de dataTable.
  observe({
    updateSelectInput(session,"selectRows",choices=names(filedata()),selected=names(filedata()))
  })                                         # Permet d'udater le selectInput qui choisis les colonnes du data.frame a prendre.
  output$resume <- renderPrint({
    summary(data())
  })                    # Output d'un summary du data.table.
  output$renamer <- renderUI({
    lapply(colnames(dataSelected()),function(i){
      textInput(paste0("col_",i),i,i)
    })
  })                      # Permet de renommer les coloones de la data
  data <- reactive({
    Data <- dataSelected()
    DataNew1<-Data
    
    for ( i in names(input) ){
      
      if(grepl(pattern = "col_",i)){
        colnames(DataNew1)[which(colnames(DataNew1)==substr(i,5,nchar(i)))]=input[[i]]
      }
      
    }
    
    return(DataNew1)
  })                                # Le dataset final qu'on utilisera s'apelle du coup "data"
  output$finalDataOut <- renderDataTable({
    DT::datatable(data(),options=list(pageLength = 10 ))      
  })          # On sort un output final du dataset.
  output$resume2 <- renderPrint(summary(data()))         # On sort le résumé une deuxiemme fois pour pouvoir le récupérer une deuxiemme fois.
  
  DataIsImputed <- reactive({
    if(!is.null(input$datafile)){if(OKnombreCol()){
      return(TRUE)
    } else {return(FALSE)}} 
      else {return(FALSE)}
  })                       # Permet de condition certaines actions au fait que les data soit déja imputées.
  
  ############### Partie Calculs et graphique des différente smesures de dépendances.
  
  x <- reactive({
    x = data()[,1]
    names(x) = names(data())[1]
    return(x)
  })
  y <- reactive({
    y = data()[,2]
    names(y) = names(data())[2]
    return(y)
  })
  nomX <- reactive({
    nomX = names(data())[1]
  })
  nomY <- reactive({
    nomY = names(data())[2]
  })
  
  
  output$hist1 <- renderPlot({
    hist(x(), main=paste0(c("Histograme de ",nomX())),xlab=nomX())
  })                      # histograme de X
  output$hist2 <- renderPlot({
    hist(y(), main=paste0(c("Histograme de ",nomY())),xlab=nomY())
  })                      # hitograme de Y
  output$qqplot <- renderPlot({
    qplot(x(),y(),xlab=nomX(),ylab=nomY(),main=paste0(c("QQ-Plot")))
  })                     # QQPlot de X,Y
  output$ksTest <- renderPrint({
    ks.test(x(),y())
  })                    # ks.test de X,Y
  output$ksTest01 <- renderPrint({
    ks.test((x() - mean(x()))/sd(x()),(y() - mean(y()))/sd(y()))
  })                  # ks.test après standardisation
  output$pearsonr <- renderPrint({
    cor(x(),y())
  })                  # retour du coef de corr de pearson
  output$kendaltau <- renderPrint({
    cor(x(),y(),method="kendall")
  })                 # tau de kendall
  output$spearmanrho <- renderPrint({
    cor(x(),y(),method="spearman")
  })               # rho de spearman
  corelType <- reactive({
    if(DataIsImputed()){
      if (cor(x(),y()) >= 0 && cor(x(),y(),method="kendall") >= 0 && cor(x(),y(),method="spearman") >= 0 ){
        return("Positive")
      } else {
        if(cor(x(),y()) <= 0 && cor(x(),y(),method="kendall") <= 0 && cor(x(),y(),method="spearman") <= 0 ){
          return("Negative")
        } else {
          if(cor(x(),y()) == 0 && cor(x(),y(),method="kendall") == 0 && cor(x(),y(),method="spearman") == 0 ){
            return("Nulle")
      }}}
    }
  })                           # Définit le type de corélation entre les 3 variables. 
  output$correlations <- renderText({
    if (corelType() == "Positive"){ return("Nous avons clairement une corrélation positive entre les deux variables") }
    if (corelType() == "Negative"){ return("Nous avons clairement une corrélation négative entre les deux variables") }
    if (corelType() == "Nulle"){ return("La correlation à l'air nulle entre les eux variables") }
  })               # output une phrase correspondant à ce type. 
  output$descdistX <- renderPlot({
    descdist(x(),boot=50)
  })                  # plot de decision de loi
  output$descdistY <- renderPlot({
    descdist(y(),boot=50)
  })                  # plot de decision de loi
  output$survie <- reactive({
    input$Survie
  })
  observe({
    if (DataIsImputed()){
      if (corelType() == "Positive" || corelType() == "Nulle") {
        shinyjs::disable("SurvieDiv")
      } else {
        shinyjs::enable("Surviediv")
      }
    }
  })
  output$Menu <- renderMenu({
    
    firstmenu = sidebarMenu(
      menuItem("Input des data", tabName="InputData", icon=icon("dashboard"))
    )
    secondmenu =sidebarMenu(
      menuItem("Input des data", tabName="InputData", icon=icon("dashboard")),
      menuItem("Mesures de dépendances", tabName="MesureDep", icon=icon("th")),
      menuItem("Choix de la copule", tabName="ChoixCopule", icon=icon("th")),
      menuItem("Résultats", tabName="Resultat", icon=icon("th"))
    )
    
    if (!is.null(input$datafile)){
      if(OKnombreCol()){ return(secondmenu) } 
      else { return(firstmenu) }
    } else { return(firstmenu) }
  })            # Fabrication du menu 
  
  
  
  
  

}


####### Running the app.
shinyApp(ui = ui, server = server)

