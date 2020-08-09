tabPanelGradeQuestions <- tabPanel('Grade questions', 
           
           # Generate a row with a sidebar
           sidebarLayout(
             
             # Define the sidebar with one input
             sidebarPanel(
               fluidRow(style = 'color:darkred;', column(width = 3,
                               actionButton('previousp', '<', class = 'btn', width = '100%')
               ), column(width = 6, style = 'text-align:center',
                         selectInput("person", 'Student #', 
                                     choices=1, selected = 1, width = '100%')
               ), column(width = 3, style = 'text-align:left',
                         actionButton('nextp', '>', class = 'btn', width = '100%')
               )),
               
               hr(style = 'height:2px; background-color:darkblue;'), 
               
               fluidRow(column(width = 3,
                            actionButton('previousq', '<', class = 'btn', width = '100%')
               ), column(width = 6, style = 'text-align:center',
               selectInput("question", 'Question #', 
                           choices=1, selected = 1, width = '100%')
               ), column(width = 3, style = 'text-align:left',
               actionButton('nextq', '>', class = 'btn', width = '100%')
               )),
               fluidRow(column(6,
                               numericInput('points_earned', 'Points earned', value = 4)
               ), column(6,
                         shinyjs::disabled(numericInput('points_possible', 'Points possible', value = 4))
               )),
               
               textAreaInput('comment', 'Question comment'),
               fluidRow(column(width = 6,
                    actionButton('grade', 'Grade', class = 'btn btn-primary', width = "100%")
                  ), column(width = 6,
                 actionButton('delete','Delete', class = 'btn btn-danger', width = "100%")
                  )
               ),
               hr(),
               fluidRow(
                 selectInput('advance', 'After clicking Grade, where should we go?', 
                             choices = c('Next question', 'Next person', 'None'),
                             selected = 'Next person')
               )
            ),
             
             mainPanel(
               htmlOutput("user_question")
             )
          )
)

