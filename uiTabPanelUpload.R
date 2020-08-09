 tabPanelUpload <- tabPanel('Assignment upload',
        
    # Generate a row with a sidebar
    sidebarLayout(

    # Define the sidebar with one input
    sidebarPanel(

      fluidRow(column(4,
                      shinyDirButton('dir1', 'Select a directory', 'Directory selection',
                                     style = 'display:inline-block')
      ), column(6,
                checkboxInput('chkOverwrite', 'Automatic overwrite', value = TRUE)
      )),

      numericInput('num_questions', 'Number of questions', value = 2)

    ),

    # Create a spot for the barplot
    mainPanel(
        #htmlOutput("user_question"),
        #dataTableOutput('gradebook')
        h2('Question point values'),
        fluidRow(column(width = 3), column(width = 6,
            tableOutput('questionPoints')
        ))
    )
))

