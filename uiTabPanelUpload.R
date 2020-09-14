 tabPanelUpload <- tabPanel('Assignment upload',
        
    # Generate a row with a sidebar
    sidebarLayout(

    # Define the sidebar with one input
    sidebarPanel(

      fluidRow(column(4,
                      shinyDirButton('dir1', 'Select a directory', 'Directory selection',
                                     style = 'display:inline-block')
      ), column(6, style = 'margin-left:20px;', 
                checkboxInput('chkOverwrite', 'Overwrite graded files', value = FALSE)
      )),

      div(id = 'num_questions_div',
        numericInput('num_questions', 'Number of questions', value = NULL)
      )
    ),

    # Create a spot for the barplot
    mainPanel(
        #htmlOutput("user_question"),
        #dataTableOutput('gradebook')
    
        htmlOutput('pointValueHeader'),

        fluidRow(column(width = 3), column(width = 6,
            tableOutput('questionPoints')
        ))
    )
))

