
fluidPage(
  chooseSliderSkin(skin="Modern",color = "#333333"), #couleur des slider
  #header
  HTML("<nav class='navbar' style='background-color: #e7e7e7;'>"),
  div(class='col-sm-2',
      div(class='pull-right',
          tags$a(
            img(src="img/LOGO-ENSAE_appli.png",height = 100, style="margin-left: 40px;margin-top:10px;margin-bottom:10px;"),
            href="https://github.com/ARKEnsae/",
            target="_blank"))),
  div(class='container-fluid col-md-8',
      div(
        h2(style='color: black; text-align:center;', "Mastermind et permutations"),
        h4(style='color: black; text-align:center;', "Projet Simulation et Monte Carlo")
      )),
  HTML("</nav>"),
  
  navbarPage(title="Menu",
             selected = "Simulations", 
             windowTitle="Mastermind et permutations", 
             includeCSS("www/template_mastermind.css"),
             tabPanel("Simulations",
                      sidebarLayout(position = "right",
                                    fluidRow(column(width = 4,wellPanel(id="sidebar", 
                                                                        tags$div(id = "module_css",
                                                                                 fluidRow(
                                                                                   column(6,numericInput("m", "Couleurs (m)", value = 6,  min = 1, max = 10)),
                                                                                   column(6,numericInput("n", "Billes (n)", value = 4,                                         
                                                                                                         min = 1, max = 8))
                                                                                   ),
                                                                                 
                                                                                 fluidRow(        
                                  
             column(5,HTML("Combinaison</br>cachée (y) : ")),            
             column(7,actionButton("tirage", "Changer\ncombinaison"))                        
             ),                     
                                                            
                                                                               
                                  
                                  fixedRow(style = "background-color:#ecf0f1;padding-top:10px;",
                                    
                                           column(1, htmlOutput(guesscell1)),
                                           column(1, htmlOutput(guesscell2)),
                                           column(1, htmlOutput(guesscell3)),
                                           column(1, htmlOutput(guesscell4)),
                                           column(1, htmlOutput(guesscell5)),  
                                           column(1, htmlOutput(guesscell6)),
                                           column(1, htmlOutput(guesscell7)),
                                           column(1, htmlOutput(guesscell8))
                                           ),
                                  
                                  fixedRow(style = "background-color:#ecf0f1;",
                                           useShinyjs(),
                                           extendShinyjs(text = jsDrawCircle),
                                           extendShinyjs(text = jsClearCircle),
                                           column(1,
                                                  tags$canvas(id = 'guesscell1js',
                                                              width = canvas_width,
                                                              height = canvas_height
                                                  )
                                           ),                                             
                                           column(1,
                                                  tags$canvas(id = 'guesscell2js',
                                                              width = canvas_width,
                                                              height = canvas_height
                                                  )
                                           ),                                                                    column(1,
                                                                                                                        tags$canvas(id = 'guesscell3js',
                                                                                                                                    width = canvas_width,
                                                                                                                                    height = canvas_height
                                                                                                                        )
                                           ),  
                                           column(1,
                                                  tags$canvas(id = 'guesscell4js',
                                                              width = canvas_width,
                                                              height = canvas_height
                                                  )
                                           ), 
                                           
                                           column(1,
                                                  tags$canvas(id = 'guesscell5js',
                                                              width = canvas_width,
                                                              height = canvas_height
                                                  )
                                           ), 
                                           column(1,
                                                  tags$canvas(id = 'guesscell6js',
                                                              width = canvas_width,
                                                              height = canvas_height
                                                  )
                                           ),
                                           column(1,
                                                  tags$canvas(id = 'guesscell7js',
                                                              width = canvas_width,
                                                              height = canvas_height
                                                  )
                                           ),
                                           column(1,
                                                  tags$canvas(id = 'guesscell8js',
                                                              width = canvas_width,
                                                              height = canvas_height
                                                  )
                                           )
                                           
                                           
                                  ),
                                  
                                                                                
          # switchInput(inputId = "avec_remise",
          #                value = TRUE,label="remise",
          #                size="normal",
          #                onStatus = "dreamrs",
          #                onLabel = "avec",
          #                offLabel = "sans"),
          h3("Paramètres du modèle"),
          hr(),
          prettyRadioButtons(inputId = "methode",
                        label = "Méthode",
                        shape = "curve",
                        outline = FALSE,
                        thick = FALSE,
                        fill = FALSE,
                        plain = FALSE,
                        animation = "smooth",
                        icon = icon("check-circle"),
                        inline = FALSE,
                        choices = c("Tirage avec remise" = "q1",
                         "Tirage sans remise" = "q2",
                         "Loi avec distance de Hamming" = "q3")),
          
          fluidRow(
            column(8,sliderInput("maxIters", "Nombre d'itérations (maxIters)",min = 1, max = 50, value = 20)),
            column(4,numericInput("d", "d", value = 5, step=1,  min = 2, max = 10))
          ),
            
          #sliderInput("maxIters", "Nombre d'itérations (maxIters)",min = 0, max = 50, value = 20),
          fluidRow(
            column(4,uiOutput("N0")), 
            column(4,numericInput("rho", "rho", value = 0.1, 
                                  step=0.01, 
                                  min = 0, max = 1)),                            column(4,numericInput("alpha", "alpha", value = 0.7,
                                                                                                       step=0.1,                                                            min = 0, max = 1))                            ),
          switchInput(inputId = "smoothing", value = TRUE,
                      label="smoothing",size="normal",onStatus = "dreamrs"), 
          actionButton("lancer_modele", "Lancer modèle")
          
          
          
                                                                        )))),
                                  mainPanel(
                                      tabsetPanel(type = "tabs",
                                                  tabPanel("Résultats",
                                  
fluidRow(
column(6,
       
      # switchInput(inputId = "film", value = FALSE, label="film",size="small",onStatus = "dreamrs"), 
       htmlOutput("titre_iter"),
       uiOutput("iter"), 
        uiOutput("texte_iter"),
                ### début boules
                fixedRow(style = "background-color:#ffffff;padding-top:10px;",
                         
                         column(1, htmlOutput(itercell1)),
                         column(1, htmlOutput(itercell2)),
                         column(1, htmlOutput(itercell3)),
                         column(1, htmlOutput(itercell4)),
                         column(1, htmlOutput(itercell5)),  
                         column(1, htmlOutput(itercell6)),
                         column(1, htmlOutput(itercell7)),
                         column(1, htmlOutput(itercell8))
                ),
                
                fixedRow(style = "background-color:#ffffff;",
                         useShinyjs(),
                         extendShinyjs(text = jsDrawCircle),
                         extendShinyjs(text = jsClearCircle),
                         column(1,
                                tags$canvas(id = 'itercell1js',
                                            width = canvas_width,
                                            height = canvas_height
                                )
                         ),                                             
                         column(1,
                                tags$canvas(id = 'itercell2js',
                                            width = canvas_width,
                                            height = canvas_height
                                )
                         ),                                                                    
                         column(1,
                                tags$canvas(id = 'itercell3js',
                                            width = canvas_width,
                                            height = canvas_height
                                )
                         ),  
                         column(1,
                                tags$canvas(id = 'itercell4js',
                                            width = canvas_width,
                                            height = canvas_height
                                )
                         ), 
                         
                         column(1,
                                tags$canvas(id = 'itercell5js',
                                            width = canvas_width,
                                            height = canvas_height
                                )
                         ), 
                         column(1,
                                tags$canvas(id = 'itercell6js',
                                            width = canvas_width,
                                            height = canvas_height
                                )
                         ),
                         column(1,
                                tags$canvas(id = 'itercell7js',
                                            width = canvas_width,
                                            height = canvas_height
                                )
                         ),                         
                         column(1,
                                tags$canvas(id = 'itercell8js',
                                            width = canvas_width,
                                            height = canvas_height
                                )
                         )
                                         )
       
       
       
       ),
column(6,
       uiOutput("vhat"),
       
       ### début boules 1
       
       
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
       ),
       
       ### fin boules   1
       
       
       
       )
)
                                                           
                                                  ),
                                             tabPanel("Tableau",
                                                      
                                      tableOutput("tableau")
                                                      
                                                      )
                                      )
                                    ))),

              tabPanel("Code",
         
                       htmlOutput("TODO1")
         
              ),
             tabPanel("À propos",
                      
                      htmlOutput("TODO2")

             )
  )
  
)
 

 
