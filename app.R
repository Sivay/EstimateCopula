#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

####### Instalation des différents packets nécéssaires : 
    .list.of.packages <- c("shiny","shinydashboard","DT","fitdistrplus","CDVine","asbio","copula","VineCopula","shinyjs")
    .new.packages <- .list.of.packages[!(.list.of.packages %in% installed.packages()[,"Package"])]
    if(length(.new.packages)) install.packages(.new.packages)
    lapply(.list.of.packages,function(x){library(x,character.only=TRUE)}) 
    
    
# Coucou c'est MIKA
# bonjour mike 
#reponse à oscar
########
ui <- ui <- dashboardPage(
  
  dashboardHeader(title = "Estimtion de copules bivariées", disable=TRUE),
  dashboardSidebar(
    sidebarMenuOutput("Menu")
  ),
  dashboardBody(
    useShinyjs(),
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
            column(4,fileInput(inputId = 'datafile',label = "",accept=c('text/csv', 'text/comma-separated-values,text/plain'))),
            column(4,selectInput(inputId = "selectRows",label="",choices = "",multiple=TRUE)),
            column(4,uiOutput(outputId = "renamer")),
            span(textOutput("isBivariate"), style="color:red")
          ),
          column(3,verbatimTextOutput("resume",placeholder = TRUE))
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
          column(5,
            h4(tags$b("Petit résumé du couple...")),
            verbatimTextOutput("resume2",placeholder = TRUE),
            fluidRow(
              h5(tags$b("    ")),
              column(6,
                h5(tags$b("Corélation linéaire de Pearson :")),
                verbatimTextOutput("pearsonr",placeholder = TRUE)
              ),
              column(6,
                textOutput("correlations"),
                div(id="SurvieDiv",
                  checkboxInput("Survie", "Passer en copule Comonotone", value = FALSE, width = NULL)
                )
              )
            ),
            fluidRow(
              column(6,
                h5(tags$b("Tau de Kendall")),
                verbatimTextOutput("kendaltau",placeholder = TRUE)

              ),
              column(6,
                h5(tags$b("Rho de Spearman")),
                verbatimTextOutput("spearmanrho",placeholder = TRUE)
              )
            ),
            h5(tags$b("Kolmogorov-Smirnov Test on raw data")),
            verbatimTextOutput("ksTest",placeholder = TRUE),
            h5(tags$b("Kolmogorov-Smirnov Test on standardized data")),
            verbatimTextOutput("ksTest01",placeholder = TRUE),
            h5(tags$b("Test de dependence des extrèmes (basé sur la Kendall's Distribution)")),
            verbatimTextOutput("evTestK1",placeholder = TRUE)
            
          ),
          column(7,
            column(6,
              h4(tags$b("A propos de la première marginale...")),
              plotOutput("hist1"),
              fluidRow(
                column(6, checkboxInput("XisDiscrete", "Variable discrete ?", value = FALSE)),
                column(6, numericInput("Xboot", "# of Bootstrap Value", 50, min = 0, max = 500, step = 1))
              ),
              plotOutput("descdistX")
            ),
            column(6,
              h4(tags$b("A propos de la seconde marginale...")),
              plotOutput("hist2"),
              fluidRow(
                column(6, checkboxInput("YisDiscrete", "Variable discrete ?", value = FALSE)),
                column(6, numericInput("Yboot", "# of Bootstrap Value", 50, min = 0, max = 500, step = 1))
              ),
              plotOutput("descdistY")
            )
          )
        ),
        fluidRow(
          column(4,
                 h5(tags$b("Quantile-Quantile Plot")),
                 plotOutput("qqplot")
                 ),
          column(4,
                 h5(tags$b("Chi-plot")),
                 plotOutput("chiPlot")
          ),
          column(4,
                 h5(tags$b("Rank-Rank Plot")),
                 plotOutput("RankRankPlot")
          )
        )
      ),
      tabItem(tabName="ChoixCopule",
              h2("Choix automatiuque d'une copule :"),
              fluidRow(
                column(1),
                column(10,dataTableOutput("AutoChoose")),
                column(1)
              )
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
    qplot(x(),y(),xlab=nomX(),ylab=nomY())
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
          } else {
          return(NULL)  
          }
        }}
    }
  })                           # Définit le type de corélation entre les 3 variables. 
  output$correlations <- renderText({
    if (corelType() == "Positive"){ return("Nous avons clairement une corrélation positive entre les deux variables") }
    if (corelType() == "Negative"){ return("Nous avons clairement une corrélation négative entre les deux variables") }
    if (corelType() == "Nulle"){ return("La correlation à l'air nulle entre les eux variables") }
  })               # output une phrase correspondant à ce type. 
  output$descdistX <- renderPlot({
    if(input$Xboot < 10){
      descdist(x(),discrete = input$XisDiscrete)
    } else {
      descdist(x(),boot=input$Xboot,discrete = input$XisDiscrete)
    }
  })                  # plot de decision de loi
  output$descdistY <- renderPlot({
    if (input$Yboot < 10){
      descdist(y(),discrete = input$YisDiscrete)
    } else {
      descdist(y(),boot=input$Yboot,discrete = input$YisDiscrete)
    }
    
  })                  # plot de decision de loi
  survie <- reactive({
    input$Survie
  })
  
  observe({
    if (DataIsImputed()){
      if (corelType() == "Negative") {
        shinyjs::show("SurvieDiv")
        updateCheckboxInput()
      } else {
        shinyjs::hide("SurvieDiv")
      }
    }
  })
  
  
  output$chiPlot <- renderPlot({
    #BiCopChiPlot(x(),y())
    chi.plot(x(),y())
  })
  
  output$RankRankPlot <- renderPlot({
    n = length(x()+1)
    plot(rank(x())/n,rank(y())/n,main="Rank-Rank plot")
  })
  
  
  output$evTestK1 <- renderPrint({
    evTestK(cbind(x(),y()))
  })
  
  
  Bicopsel <- reactive({
     aic <- BiCopSelect(pobs(x()),pobs(y()), selectioncrit="AIC", se=TRUE)
     bic <- BiCopSelect(pobs(x()),pobs(y()), selectioncrit="BIC", se=TRUE)
     llh <- BiCopSelect(pobs(x()),pobs(y()), selectioncrit="logLik", se=TRUE)
     return(list(aic,bic,llh))
  })
  
  output$AutoChoose <- renderDataTable({
    # 
    # bicopsel$aic$familyname
    # bicopsel$aic$par
    # bicopsel$par2
    # bicopsel$aic$npars # number of parameters 
    # bicopsel$aic$se # standard error for 1rst parameter
    # bicolsom$aic$se2 # standard error for 2nd parameters
    # bicopsel$aic$logLik
    # $AIC
    # $BIC
    # $emptau
    # p.value.indeptest
    
    rez <- Bicopsel()[[1]]
    AIC <- c("AIC",rez$familyname,rez$par,rez$se,rez$par2,rez$se2,rez$logLik,rez$AIC,rez$BIC,rez$emptau,rez$p.value.indeptest)
    
    rez <- Bicopsel()[[2]]
    BIC <- c("BIC",rez$familyname,rez$par,rez$se,rez$par2,rez$se2,rez$logLik,rez$AIC,rez$BIC,rez$emptau,rez$p.value.indeptest)
    
    
    rez <- Bicopsel()[[3]]
    logLik <- c("logLik",rez$familyname,rez$par,rez$se,rez$par2,rez$se2,rez$logLik,rez$AIC,rez$BIC,rez$emptau,rez$p.value.indeptest)

    rez <- rbind(AIC,BIC,logLik)
    colnames(rez) <- c("Critère de test",
                    "Nom de famille",
                   "Paramètre 1",
                   "Se(Par1)",
                   "Paramètre 2",
                   "Se(Par2)",
                   "Log-likelyhood",
                   "AIC",
                   "BIC",
                   "Tau de kendall emp.",
                   "P.value ( indep )")

    return(data.frame(rez))
    
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

