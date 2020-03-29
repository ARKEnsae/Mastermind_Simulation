
function(input, output, session) {
  
  
  
  ########## Fonctions servant aux couleurs des billes
  
  updateBoulesY <- function(input, output, n) {
      
      output$guesscell1 <- renderText({
       
        js$drawCircle('guesscell1js',  colorsInput()[1])
      })
      
      output$guesscell2 <- renderText({
        if(n>=2){
          js$drawCircle('guesscell2js', colorsInput()[2])  
        }else{
          js$clearCircle('guesscell2js')
        }
      })
      output$guesscell3 <- renderText({
        if(n>=3){
          js$drawCircle('guesscell3js',  colorsInput()[3]) 
        }else{
          js$clearCircle('guesscell3js')
        }
        
      })
      output$guesscell4 <- renderText({
        if(n>=4){
          js$drawCircle('guesscell4js',  colorsInput()[4])
        }else{
          js$clearCircle('guesscell4js')
        }
        
      })
   
      output$guesscell5 <- renderText({
        if(n>=5){
          js$drawCircle('guesscell5js',  colorsInput()[5])
        }else{
          js$clearCircle('guesscell5js')
        }
        
      })
      
      output$guesscell6 <- renderText({
        if(n>=6){
          js$drawCircle('guesscell6js',  colorsInput()[6])
        }else{
          js$clearCircle('guesscell6js')
        }
        
      })
      output
    }
  
  updateBoulesIter <- function(input, output, n) {
    
    
    output$itercell1 <- renderText({
      if(input$iter2>1){
        js$drawCircle('itercell1js',  colorsPropInput()[1])
      }else{
        js$clearCircle('itercell1js')
      }
    })
    
    output$itercell2 <- renderText({
      if(n>=2 &input$iter2>1){
        js$drawCircle('itercell2js', colorsPropInput()[2])  
      }else{
        js$clearCircle('itercell2js')
      }
    })
    output$itercell3 <- renderText({
      if(n>=3 & input$iter2>1){
        js$drawCircle('itercell3js',  colorsPropInput()[3]) 
      }else{
        js$clearCircle('itercell3js')
      }
      
    })
    output$itercell4 <- renderText({
      if(n>=4 & input$iter2>1){
        js$drawCircle('itercell4js',  colorsPropInput()[4])
      }else{
        js$clearCircle('itercell4js')
      }
      
    })
    
    output$itercell5 <- renderText({
      if(n>=5 & input$iter2>1){
        js$drawCircle('itercell5js',  colorsInput()[5])
      }else{
        js$clearCircle('itercell5js')
      }
      
    })
    
    output$itercell6 <- renderText({
      if(n>=6 & input$iter2>1){
        js$drawCircle('itercell6js',  colorsInput()[6])
      }else{
        js$clearCircle('itercell6js')
      }
      
    })
    output
  }
  
  
  yInput <- eventReactive(c(input$tirage,input$m,input$n,input$avec_remise), {
    y <- initialiser_y(m=input$m,n=input$n, avec_remise = input$avec_remise)
    return(y)
  }, ignoreNULL = FALSE)
  
  colorsPropInput <- eventReactive(c(input$tirage,input$iter2), {
    matrice <- (matricesInput()$P_hat_liste)[[input$iter2]]
    prop <- matrice_to_proposition(matrice)
    couleurs <- colors[prop]
    return(couleurs)
  })
  
  colorsInput <- eventReactive(yInput(), {
    couleurs <- colors[yInput()]
    return(couleurs)
  }, ignoreNULL = FALSE)
 
  output$iter <- renderUI({
    sliderInput("iter2", label=NULL,min = 1, max = input$maxIters, value = 1,step = 1) 
  })
  
  output$N0 <- renderUI({
    numericInput("N", "N", value = C*input$n*input$m, 
                 step=10, 
                 min = 0, max = 2*C*input$n*input$m)
  })
  
  
  

  
  # matricesInput <- eventReactive(c(input$lancer_modele,input$tirage,input$n,input$m), {
  matricesInput<- reactive({
    
    liste_matrices <- lancer_algorithme(y=yInput(),n=input$n, 
                                        m=input$m,
                                        N = input$N, maxIters=input$maxIters,
                                        rho = input$rho, alpha = input$alpha,
                                        poids_blanc = 1, poids_noir = 2,
                                        smoothing = input$smoothing, C=C, d=input$d, avec_remise = input$avec_remise)
    
    modele2 <<- liste_matrices
    
    return(liste_matrices)
    
  })
  
  
  graphInput <- eventReactive(c(input$tirage,input$n,input$m,input$iter2), {
    return(
      dessiner_histo(matricesInput()$P_hat_liste,input$iter2,colors[1:input$m])
      )
  }, ignoreNULL = FALSE)
  
  output$graph <- renderPlot({
    graphInput()
  })
  
  # Textes
  output$titre_iter <- renderText({
    texte <- paste0(
        "</br>",
      ifelse(!is.null(matricesInput()$indice_stop),
             paste0("Convergence à l'itération n°", matricesInput()$indice_stop),
             "Non convergence"),  
      " (d=",input$d,")</br></br>",
    "<b>Itération n°",input$iter2,"</b>"
    )
    return(texte)
  })
  
  output$texte_iter <- renderText({
    
    if(input$iter2>1){
      p_min_max = p_min_max(matricesInput()$P_hat_liste[[input$iter2]])
      texte <- paste0(
        "Gamma : ",round((matricesInput()$gammas_hat)[input$iter2],3),"</br>",
        "Smax : ", round((matricesInput()$s_max)[input$iter2],3),"</br>",
        "min : ", round(p_min_max$min,4)," / ",
        "max_min : ", round(p_min_max$max_min,4),"</br>",
        "max : ", round(p_min_max$max,4)," / ",
        "min_max : ", round(p_min_max$min_max,4),"</br>",
        
      #  "Meilleur score :",(matricesInput()$meilleur_scores)[input$iter2],
        "Meilleure proposition :"
      )
    } else{
      texte <- "</br></br></br></br></br></br></br>"
      
    }

  
    return(texte)
  })
  
 
  output$tableau <- function() {
    
    mise_en_forme_tableau(matricesInput())
  }
  
  #Fonctionnalité "animation"
  FilmOnInput <- reactive({
    if(input$film){
      if(!is.null(input$iter2)){
        input$iter2
      }
    }
  })

  observeEvent({ 
    FilmOnInput()
      }, { 
    delay(1000, updateSliderInput(session,inputId="iter2", value = (input$iter2+1)))

    })
    
 

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
  }, {
    updateBoulesIter(input=input, output=output,n=input$n)
  })
  
  
  
  }


