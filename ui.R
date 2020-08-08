
# Rely on the 'WorldPhones' dataset in the datasets
# package (which generally comes preloaded).
library(shinyjs)
library(shinyFiles)
library(shinythemes)

# Use a fluid Bootstrap layout
fluidPage(

  inlineCSS(list(
    #"body" = "background-color: yellow",
    "#shiny-notification-panel" = "width: 33% !important; bottom: 10% !important;",
    "#shiny-notification-graded_exists" = "border: 1px solid black;",
    "#previousq, #nextq, #previousp, #nextp"= "margin-top:22px;"
    
  )), 
  
  # Give the page a title
 # titlePanel("Help Me Grade"),
useShinyjs(),
 #shinythemes::themeSelector(),
  navbarPage('Help me grade', inverse = TRUE, theme = shinytheme("cerulean"),
    id = 'tab',
    tabPanel('Assignment upload',
        
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
  )),
  
  tabPanel('Grade Questions', 
           
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
  ),
  
  tabPanel('View assignment / final grade',
           htmlOutput('user_assignment')
  ),

  tabPanel('Gradebook', 
           tableOutput('gradebook'))
  )
)


