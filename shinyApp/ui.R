
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
             windowTitle="windowtitle", 
             includeCSS("www/template_mastermind.css"),
             tabPanel("Simulations",
                    sidebarLayout(position = "right",
                                    fluidRow(column(width = 4,wellPanel(id="sidebar", 
                                                                        tags$div(id = "module_css",
                                                                                fluidRow(
                                                                                  column(6,numericInput("m", "Couleurs (m)", value = 6, 
                                  min = 0, max = 10)),                            column(6,numericInput("n", "Billes (n)", value = 4,                                         min = 0, max = 6))                            ),
                                                                                HTML("Combinaison cachée (y) : "),
                                  
                                  fixedRow(style = "background-color:#ecf0f1;padding-top:10px;",
                                    
                                           column(1, htmlOutput(guesscell1)),
                                           column(1, htmlOutput(guesscell2)),
                                           column(1, htmlOutput(guesscell3)),
                                           column(1, htmlOutput(guesscell4)),
                                           column(1, htmlOutput(guesscell5)),  
                                           column(1, htmlOutput(guesscell6))
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
                                           )                                                              ),
                                  
                                                                                
          actionButton("tirage", "Changer combinaison"),
          br(),
          h3("Paramètres du modèles"),
          hr(),
          
          sliderInput("maxIters", "Nombre d'itérations",min = 0, max = 50, value = 20),
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
       
       fluidRow(
         column(8,
                htmlOutput("titre_iter"),
                uiOutput("iter")), 
         column(4, br(), 
                switchInput(inputId = "film", value = FALSE, label="film",size="small",onStatus = "dreamrs")) #marche pas
       ),
       
        uiOutput("texte_iter"),
    
                
                ### début boules
                fixedRow(style = "background-color:#ffffff;padding-top:10px;",
                         
                         column(1, htmlOutput(itercell1)),
                         column(1, htmlOutput(itercell2)),
                         column(1, htmlOutput(itercell3)),
                         column(1, htmlOutput(itercell4)),
                         column(1, htmlOutput(itercell5)),  
                         column(1, htmlOutput(itercell6))
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
                         )
                )
       
       
       
       ),
column(6,plotOutput("graph", width = "100%", height = "300px"))
)
                                                           
                                                  )#,
                                             #tabPanel("Tableau",)
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
 

 
