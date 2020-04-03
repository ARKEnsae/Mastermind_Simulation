
function(input, output, session) {

  ########## Fonctions servant aux couleurs des billes
  
  updateBoulesY <- function(input, output, n) {
    
    for (i in 1:8){
      requete <- sprintf("
      output$guesscell%i <- renderText({
        if(n>=%i){
          js$drawCircle('guesscell%ijs', colorsYInput()[%i])
        }else{
          js$clearCircle('guesscell%ijs')
        }
      })
      ", i, i, i, i, i)
      eval(parse(text = requete))
    }

      # output$guesscell1 <- renderText({
      # 
      #   js$drawCircle('guesscell1js',  colorsYInput()[1])
      # })
      output
    }
  
  updateBoulesIter <- function(input, output, n) {
    for (i in 1:8){ ## new : input$methode!='q3'
      requete <- sprintf("
      output$itercell%i <- renderText({
      if(n>=%i & input$methode!='q3'){
        js$drawCircle('itercell%ijs',  colorsIterInput()[%i])
      }else{
        js$clearCircle('itercell%ijs')
      }
    })
      ", i, i, i, i, i)
      eval(parse(text = requete))
    }
    # output$itercell1 <- renderText({
    #   if(n>=1 & input$iter2>1){
    #     js$drawCircle('itercell1js',  colorsIterInput()[1])
    #   }else{
    #     js$clearCircle('itercell1js')
    #   }
    # })
    # 
    output
  }
  
  updateBoulesXstar <- function(input, output, n) {
    
    
    for (i in 1:8){
      requete <- sprintf("
                   output$xstarcell%i <- renderText({
                        if(n>=%i){
              js$drawCircle('xstarcell%ijs', colorsXstarInput()[%i])
    }else{
                         js$clearCircle('xstarcell%ijs')
    }
  })
                         ", i, i, i, i, i)
      eval(parse(text = requete))
    }
    
    output
}
  
  
  
  yInput <- eventReactive(c(input$tirage,input$m,input$n,remiseInput(),input$methode), {
    y <- initialiser_y(m=input$m,n=input$n, avec_remise = remiseInput())
    return(y)
  }, ignoreNULL = FALSE)
  
  # xstarInput <-reactive({
  #   xstar <- (modeleInput()$x_star_hat_liste)[[input$iter2]]
  #   return(xster)
  # })
  # 
  # iterBoulesInput <-reactive({
  #   xstar <- (modeleInput()$x_star_hat_liste)[[input$iter2]]
  #   return(xster)
  # })
  
#  colorsIterInput <- eventReactive(c(input$tirage,input$iter2), {
  colorsIterInput <-reactive({
      
    if(!is.null(input$iter2)){
      matrice <- (modeleInput()$P_hat_liste)[[input$iter2]]
      prop <- meilleure_proposition(matrice)
      couleurs <- colors[prop]
      return(couleurs)
    }
  })
  
  colorsXstarInput <- reactive({
    if(!is.null(input$iter2)){
    prop <- (modeleInput()$x_star_hat_liste)[[input$iter2]]
    couleurs <- colors[prop]
    return(couleurs)
    }
  })
  
  
  colorsYInput <- eventReactive(yInput(), {
    couleurs <- colors[yInput()]
    return(couleurs)
  }, ignoreNULL = FALSE)
 
  output$iter <- renderUI({
    default_value <- 1
    # Si on veut commencer par défaut à la simulation de convergence
    # if(!is.null(modeleInput()$indice_stop))
    #   default_value <- modeleInput()$indice_stop
    sliderInput("iter2", label=NULL,min = 1, max = input$maxIters, value = default_value,step = 1,animate = animationOptions(interval = 700,playButton = icon('play', "fa-1x"),pauseButton = icon('pause', "fa-1x"))) 
  })
  
  output$N0 <- renderUI({
    numericInput("N", "N", value = C*input$n*input$m, 
                 step=10, 
                 min = 0, max = 2*C*input$n*input$m)
  })
  
  
  # modeleInput <- eventReactive(c(input$lancer_modele,input$tirage,input$n,input$m), {
  modeleInput<- reactive({
    
    if(input$methode!="q3"){
      modele <- lancer_algorithme(y=yInput(),n=input$n, 
                                  m=input$m,
                                  N = input$N, maxIters=input$maxIters,
                                  rho = input$rho, alpha = input$alpha,
                                  poids_blanc = 1, poids_noir = 2,
                                  smoothing = input$smoothing, C=C, d=input$d, avec_remise = remiseInput(),stop_d=FALSE)
    } else{
      modele <-  lancer_algorithme_hamming(y=yInput(), n=input$n, m=input$m, N = input$N, maxIters = input$maxIters,rho = input$rho, alpha = input$alpha,poids_blanc = 1, poids_noir = 2, smoothing = TRUE, C=C, d=input$d, stop_d=FALSE)
    }

    modele <<- modele 
    
    return(modele)
    
  })
  
  remiseInput <- reactive({
    if(input$methode=="q1"){
      bool <- TRUE
    } else{
      bool <- FALSE
    }
    return(bool)
  })
  
    
  vhatQ1Q2Input <- eventReactive(c(input$tirage,input$n,input$m,input$iter2,input$methode), {
  
    if(!is.null(input$iter2) & input$methode!="q3"){ 
        return(
          dessiner_histo(modeleInput()$P_hat_liste,input$iter2,colors[1:input$m])
        )

    }
    
  }, ignoreNULL = FALSE)
  
  vhatQ3Input <- eventReactive(c(input$tirage,input$n,input$m,input$iter2,input$methode), {
    
    if(!is.null(input$iter2) & input$methode=="q3"){ 
      return("hello")
    }
    
  }, ignoreNULL = FALSE)
  
  output$vhatQ1Q2 <- renderPlot({
    vhatQ1Q2Input()
  })
  
  output$vhatQ3 <- renderUI({
    fixedRow(
      br(),
      br(),
      h4("lambda : "),
      p(round((modeleInput()$lambda_hat_liste)[[input$iter2]],4)),
      h4("x* : "),
      
      fixedRow(style = "background-color:#ffffff;padding-top:10px;",
               
               column(1, htmlOutput(xstarcell1)),
               column(1, htmlOutput(xstarcell2)),
               column(1, htmlOutput(xstarcell3)),
               column(1, htmlOutput(xstarcell4)),
               column(1, htmlOutput(xstarcell5)),  
               column(1, htmlOutput(xstarcell6)),
               column(1, htmlOutput(xstarcell7)),
               column(1, htmlOutput(xstarcell8))
      ),
      
      fixedRow(style = "background-color:#ffffff;",
               useShinyjs(),
               extendShinyjs(text = jsDrawCircle),
               extendShinyjs(text = jsClearCircle),
               column(1,
                      tags$canvas(id = 'xstarcell1js',
                                  width = canvas_width,
                                  height = canvas_height
                      )
               ),                                             
               column(1,
                      tags$canvas(id = 'xstarcell2js',
                                  width = canvas_width,
                                  height = canvas_height
                      )
               ),                                                                    
               column(1,
                      tags$canvas(id = 'xstarcell3js',
                                  width = canvas_width,
                                  height = canvas_height
                      )
               ),  
               column(1,
                      tags$canvas(id = 'xstarcell4js',
                                  width = canvas_width,
                                  height = canvas_height
                      )
               ), 
               
               column(1,
                      tags$canvas(id = 'xstarcell5js',
                                  width = canvas_width,
                                  height = canvas_height
                      )
               ), 
               column(1,
                      tags$canvas(id = 'xstarcell6js',
                                  width = canvas_width,
                                  height = canvas_height
                      )
               ),
               column(1,
                      tags$canvas(id = 'xstarcell7js',
                                  width = canvas_width,
                                  height = canvas_height
                      )
               ),                         
               column(1,
                      tags$canvas(id = 'xstarcell8js',
                                  width = canvas_width,
                                  height = canvas_height
                      )
               )
      )
      
    )
    
    
     })
  
  output$vhat <- renderUI({
    if(input$methode!="q3"){
      plotOutput("vhatQ1Q2", width = "100%", height = "300px")
    } else{
      uiOutput("vhatQ3")
    }
    
  })
  
  # Textes
  output$titre_iter <- renderText({
    texte <- paste0(
        "</br>",
      ifelse(!is.null(modeleInput()$indice_stop),
             paste0("Convergence à l'itération n°", modeleInput()$indice_stop),
             "Non convergence"),  
      " (d=",input$d,")</br></br>",
    "<b>Itération n°",input$iter2,"</b>"
    )
    return(texte)
  })
  
  output$texte_iter <- renderText({
    if(!is.null(input$iter2)){
      
      texte <- paste0(
        "Gamma : ",round((modeleInput()$gammas_hat)[input$iter2],3),"</br>",
        "Smax : ", round((modeleInput()$s_max)[input$iter2],3),"</br>")
        
        if(input$methode!="q3"){
          p_min_max = p_min_max(modeleInput()$P_hat_liste[[input$iter2]])
 texte <- paste0(texte,"min : ", round(p_min_max$min,4)," / ",
                 "max_min : ", round(p_min_max$max_min,4),"</br>",
                  "max : ", round(p_min_max$max,4)," / ",
                  "min_max : ", round(p_min_max$min_max,4),
                 "</br>","Meilleure proposition :"
 )
          
        } else{
          
        }
      
      return(texte)
    }
  
  })
  
 
  output$tableau <- function() {
    if(input$methode=="q3"){
      bool = FALSE
    } else{
      bool = TRUE
    }
    mise_en_forme_tableau(modele=modeleInput(),matriciel=bool)
  }
  
  #Fonctionnalité "animation" 
  # FilmOnInput <- reactive({
  #   if(input$film){
  #     if(!is.null(input$iter2)){
  #       input$iter2
  #     }
  #   }
  # })
  # 
  # observeEvent({ 
  #   FilmOnInput()
  #     }, { 
  #   delay(1000, updateSliderInput(session,inputId="iter2", value = (input$iter2+1)))
  # 
  #   })
    
 

  ##### Mises à jours automatiques
  
  
  # Quand on change n ou m, on : 
  # - reaffiche les boules de y
  # - met iter=1
  observeEvent({
    input$m
    input$n
    }, {
    updateBoulesY(input=input, output=output,n=input$n)
    updateSliderInput(session,inputId="iter2", value = 1)
  })
  
  observeEvent({
    input$iter2
    yInput()
  }, {
    updateBoulesIter(input=input, output=output,n=input$n)
    updateBoulesXstar(input=input, output=output,n=input$n)
  })
  
  
  
  }


