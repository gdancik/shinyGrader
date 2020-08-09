tabPanelFinal <- tabPanel('Final comments', 
           
           # Generate a row with a sidebar
           sidebarLayout(
             
             # Define the sidebar with one input
             sidebarPanel(
               fluidRow(style = 'color:darkred;', column(width = 3,
                               actionButton('previouspFinal', '<', class = 'btn', width = '100%')
               ), column(width = 6, style = 'text-align:center',
                         selectInput("personFinal", 'Student #', 
                                     choices=1, selected = 1, width = '100%')
               ), column(width = 3, style = 'text-align:left',
                         actionButton('nextpFinal', '>', class = 'btn', width = '100%')
               )),
               
               textAreaInput('final-comment', 'Assignment comment'),
               fluidRow(column(width = 6,
                    actionButton('add-comment', 'Add comment', class = 'btn btn-primary', width = "100%")
                  ), column(width = 6,
                 actionButton('delete-comment','Delete comment', class = 'btn btn-danger', width = "100%")
                  )
               )
            ),
             
             mainPanel(
               htmlOutput("user_assignment")
             )
          )
)

