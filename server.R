library(stringr)
library(DT)

###################################################
# The main reactive framework is 
#   assignment(files, fileNum, changed)
# 
# getAssignment - depends on all of above
# getQuestion - getAssignment for input$question
#######################################


cross <- '&#10060'
exclamation <- '&#10071'
correct <- '&#9989'

getQuestion <- function(s, num) {
  s2 <- strsplit(s,'<pre class="r">')[[1]]
  num <- paste0('### Question ', num)
  g <- grep(num, s2)
  paste0('<pre class="r">', s2[g])
}


beginTotalGradeComment <- '\n<!-- BEGIN TOTAL GRADE COMMENT --->\n'
endTotalGradeComment <- '\n<!-- END TOTAL GRADE COMMENT --->\n'

# Define a server for the Shiny app
function(input, output, session) {

  observeEvent(input$tab, {
    print('clicked tab')
    
    if (input$tab == 'Gradebook') {
      print(assignment$gradebook)
    }
    
  })
  
  
  volumes <- c(Home = fs::path_home(), 
               Desktop = "/Users/dancikg/Desktop/r_grading", getVolumes()())
  
  shinyDirChoose(
    input,
    'dir1',
    updateFreq = 0,
    session = session,
    defaultRoot = 'Desktop',
  #  defaultPath = "Desktop",
    roots = volumes
  )
                 
    
  shinyFileChoose(
    input,
    'file1',
    updateFreq = 0,
    session = session,
    defaultRoot = NULL,
    defaultPath = "/Desktop/r_grading/assignment",
    roots = volumes
  )
  
  observeEvent( input$dir1, {
    dir_selected <- parseDirPath(volumes, input$dir1)
    if (length(dir_selected) == 0) {
      return()
    }
    
    question_file <- paste0(dir_selected, '/questions.csv')
    if (file.exists(question_file)) {
      r <- read.csv(paste0(dir_selected, '/questions.csv'), header = FALSE)
      
      print(r)
      assignment$question_points <- r$V2
      if (!all(r$V1 == 1:nrow(r))) {
        stop("Questions must be numbers 1 - n")
      }
      assignment$num_questions <- nrow(r)
      
    } else {
      stop("Error: missing questions.csv")
    }
    
    cat('update to: ', assignment$num_questions)
    updateTextInput(session, 'num_questions', 'Number of questions', assignment$num_questions)
    
    updateQuestionInput(1)
    
    cat('selected directory: ', dir_selected, '\n')
    files <- Sys.glob(paste0(dir_selected, '/*.html'))
    
    s <- unlist(sapply(files, validateFormat))
    
    msg <- paste0(s, collapse = "</br>")
   
    if (!is.null(msg) && msg != "") {
      
      output$user_question <- renderUI({
        HTML("<h3 style = 'color:red'>Assignment format not valid</h3>", 
             "<p>", msg, "</p")
      })
      return()
    }
    
    s <- unlist(sapply(files, getGradedFileName))
    d <- getGradedFileName(files[1], TRUE)
    dir.create(d, showWarnings = FALSE)
    
    
    cat("check is: ", input$chkOverwrite, '\n')
    #scan(what = character())
    
    if (input$chkOverwrite) {
      f <- Sys.glob(paste0(d,'/*'))
      if (length(f) > 0) {
        showNotification(HTML('<h3>Note: Deleting files in graded directory</h3>'),
                         id = 'deleting-files', duration = 7, type = "message")
        file.remove(f)
      }
    }
    
    # set up for copying files
    copies <- sapply(files, getGradedFileName)
    assignment$files <- sample(copies)
    assignment$fileNum <- 1
    assignment$changed <- TRUE
    
    # create gradebook (do this first so we can fill it in for existing files)
    m <- matrix(NA, nrow = length(copies), ncol = assignment$num_questions)
    d <- data.frame(m)
    rownames(d) <- basename(assignment$files)
    colnames(d)<- paste0("Q", 1:assignment$num_questions)
    d <- cbind(d, total = 0)
    assignment$gradebook <- d
    
    found_graded <- FALSE
    # check if any have been (partially) graded
    for ( s1 in s) {
      if (!file.exists(s1)) {
        next
      }
      r <- readAssignment(s1)
      g <- getGradesFromString(r)
    
      if (nrow(g) > 0 && !is.na(g[1,1])) {
        found_graded <- TRUE
        setGradebook(s1, g[,2], g[,3])
      }
    }
    
    if (found_graded) {
      showNotification(HTML('<h3>Note: Graded files exist</h3> 
                            <p> At least one file is (partially) graded. 
                            This file will not be overwritten. To re-grade the original 
                            file, delete your "graded" directory and re-load the files.</p>'),
                       id = 'graded_exists', duration = 7, type = "message")
    } else if (any(sapply(s, file.exists))) {
      showNotification(HTML('<h3>Note: Files exist </h3>
                            <p> Files exist in the "graded" directory, but have
                            not yet been graded; these files will be overwritten'),
                       duration = 7, type = "message", id = 'graded_exists')
    }
    
    # create copies of all files -- but do NOT overwrite existing ones
    for (i in 1:length(copies)) {
      readAssignment(files[i], copies[i])
    }
    
  })

  # get graded file name or directory
  getGradedFileName <- function(f, dir = FALSE) {
    b <- basename(f)
    d <- basename(dirname(f))
    
    db <- paste0('/',d, '/', b)
    
    if (dir) {
      return(gsub(paste0(db, '$'), paste0('/graded/'), f))
    }
    g <- gsub(paste0(db, '$'), paste0('/graded/',b), f)
    gsub('\\.html$', '_graded.html', g)
    
  }

  
  # questions must be numeric (1, 2, etc)    
  validateFormat <- function(f) {
    r <- readAssignment(f, NULL)
    questions <- as.numeric(str_match_all(r, "### Question (\\d+)")[[1]][,2])
    
    msg <- NULL
    n <- assignment$num_questions
    missing <- setdiff(1:n, questions)
    extra <- setdiff(questions, 1:n)
    
    if (length(missing) > 0) {
      msg <- paste0(msg, "</br>missing questions: ", paste(missing, collapse = ","))
    }
    
    if (length(extra) > 0) {
      msg <- paste0(msg, "</br>extra questions found: ", paste(extra, collapse = ","))
    }

    if (!is.null(msg)) {
      msg <- paste0("<b>", basename(f), "</b>", msg, "</br></br>")  
    }
    
    msg
    
  }
  
  updateQuestionInput <- function(selected){
    updateSelectInput(session, "question", "Question:",
                      1:length(assignment$question_points), 
                      selected = selected)
  }
  
  observeEvent(assignment$fileNum, {
  
    updateSelectInput(session, 'person', "Student:",
                    1:length(assignment$files), selected = assignment$fileNum)
    
    updateSelectInput(session, 'personFinal', "Student:",
                      1:length(assignment$files), selected = assignment$fileNum)
    
  })
  
  observeEvent(input$person, {
    assignment$fileNum <- as.integer(input$person)
  })

  observeEvent(input$personFinal, {
    assignment$fileNum <- as.integer(input$personFinal)
  })
  
  
  
 
  updatePointsPossible <- function(points, update_earned = TRUE) {
    updateNumericInput(session, 'points_possible', 'Points possible', value = points)
    if (update_earned) {
      updateNumericInput(session, 'points_earned', 'Points earned', value = points)
    }
  }
  
  # reads the assignment from file 'f' and returns its text
  # copying only occurs if file does not exist, and 
  # will add appropriate .answer-correct, .answer-wrong, and #total-grade styles,
  # as well as a total grade div after the first div
  readAssignment <- function(f, copy_file = NULL) {
    
    cat('reading: ', f, '...\n')
    s <- readLines(f)
    assignment_orig <- paste0(s, collapse = '\n')
    
     if (!is.null(copy_file) && !file.exists(copy_file)) {
      assignment_orig <- paste(assignment_orig, "\n\n<style>\n.answer-correct span{\ncolor:green;\nfont-weight:normal;\n}\n",
                               "\n.answer-wrong span {\ncolor:red;\nfont-weight:normal;\n}\n",
                               "</style>")

      # get first div
      s <-  str_extract(assignment_orig, '<div\\b[^>]*>[\\s\\S]*?</div>')
      print(s)
      
      assignment_orig <- gsub(s, paste0(s, '\n', 
                                        '<div id = "total-grade-div" style = "color:blue; border: 1px solid blue; padding:10px; margin-bottom:10px;">',
                                        '<h2 id = "total-grade">Grade Here</h2>',
                                        beginTotalGradeComment, endTotalGradeComment,
                                        '</div>'),
                                        assignment_orig)
      
      write(assignment_orig, file = copy_file)
    }
    
     invisible(assignment_orig)
     
  }
  
  assignment <- reactiveValues(files = NULL, fileNum = NULL, changed = TRUE, 
                               gradebook = NULL,
                               questions_points = NULL, 
                               num_questions = NULL)
  
  

  # nextQuestion with offset = 
  #  1 for next question
  #  -1 for previous
  # NULL to go to first question
  nextQuestion <- function(offset) {

    selected = 1
    if (!is.null(offset)) {
      selected = as.integer(input$question) + offset
    }
    
    if (selected > input$num_questions) {
      showNotification('You are past the last question; starting over at question #1',
                       duration = 7)
      
      selected = 1
    }
    
    updateQuestionInput(selected)
    
    cat('update with points: ', assignment$question_points[selected], '...\n')
    updatePointsPossible(assignment$question_points[selected])
    
    }
  
  
  # go to next or previous person, depending on offset; 
  #   if past the end, go to next question for first person
  nextPerson <- function(offset) {
    
    num <- assignment$fileNum + offset
    
    # if passed the end, next question for first person
    if (num > length(assignment$files)) {
      num <- 1
      nextQuestion(1)
    } else if (num < 1) {
      shinyjs::alert("already at first person")
      return()
    }
    
    assignment$fileNum <- num
    

  }
  
  # get assignment using reactive values files[fileNum]
  getAssignment <- reactive({
    assignment$changed
    assignment$changed <- FALSE
    readAssignment(currentFile())
  })
  
  # get the current question
  question <- reactive({
    cat('getting question...')
    if (is.null(assignment$files)) {
      return ("")
    }
    
    a <- getAssignment()
    getQuestion(a, input$question)
    
  })

  output$user_question <- renderUI({
    HTML(paste0('<h3> Student #', assignment$fileNum, ', Question ', 
                input$question, ', ', 
                num_graded_string(currentFile()), '</h3>'), question())
  }) 
  
  output$user_assignment <- renderUI({
    HTML(paste0('<h3> Student #', assignment$fileNum, 
                ', ', 
                num_graded_string(currentFile()), '</h3>'), getAssignment())
    
    
  })

  
  observe( {
      if (is.null(assignment$gradebook)) {
        return()
      }
    
      print('re-render gradebook...')
    
      output$gradebook <- renderTable(
        assignment$gradebook,rownames = TRUE
      )
  })
  
  observe({
    if (is.null(assignment$num_questions)) {
      return()
    }
    output$questionPoints <- renderTable(
        data.frame(question_number = 1:assignment$num_questions,
                             points = assignment$question_points), 
                             rownames = FALSE
        )
  })
  
  currentFile <- reactive({
    assignment$files[assignment$fileNum]
  })
  
  observeEvent(input$grade,{
    
   if (is.na(input$points_possible) | is.na(input$points_earned)) {
      shinyjs::alert('at least one input is not valid')
      return()
    }
    
    g <- grade(getAssignment(), input$question, input$points_earned, input$points_possible, input$comment)
    
    write(g, file = currentFile())
    
   # nextQuestion(1)
    assignment$changed <- TRUE
    #setGradebook(currentFile(), input$question, input$points_earned)  
    
    if (input$advance == 'Next question') {
      nextQuestion(1)
    } else if (input$advance == "Next person") {
      nextPerson(1)
    }
  })
  
  observeEvent(input$`add-comment`,{
    
    if (trimws(input$`final-comment`) == '') {
      return()
    }
    
    g <- addComment(getAssignment(), input$`final-comment`)
    
    write(g, file = currentFile())
    
    assignment$changed <- TRUE
    
  })
  
  
  
  observeEvent(input$nextq, {
    nextQuestion(1)
  })

  observeEvent(input$previousq, {
    nextQuestion(-1)
  })
  
  observeEvent(input$delete, {
    
    g1 <- question()
    
    g2 <- gsub('<p class = \"answer-correct\".*?</p>', '', g1)
    g2 <- gsub('<p class = \"answer-wrong\".*?</p>', '', g2)
    x <- gsub(g1, g2, getAssignment(), fixed = TRUE)
  
    assignment$changed <- TRUE
    
    setGradebook(currentFile(), input$question, NA) 
    x <- addTotal(currentFile(), x)  
    write(x, file = currentFile())
    
  })
  
  observeEvent(input$`delete-comment`, {
    
    x <- str_extract(getAssignment(), totalGradeComment())
    x <- gsub(x, totalGradeComment(mycomment = ''), getAssignment(), fixed = TRUE)
    
    assignment$changed <- TRUE
    write(x, file = currentFile())
    
  })
  
  
  
  
  observeEvent( c(input$nextp, input$nextpFinal),{
                   cat("next person")
    nextPerson(1)
  }, ignoreInit = TRUE)
  
  observeEvent(c(input$previousp, input$previouspFinal), {
    nextPerson(-1)
  }, ignoreInit = TRUE)
  
  source('gradebook.R', local = TRUE)
  
  grade <- function(s, num, earned, possible, comment = '') {
    
    print('grading..')
    q = paste0('### Question ', num)
    
    id <- paste0('q', num, '-graded')
    
    emoji = correct 
    color <- 'green'
    class <- 'answer-correct'
    if (earned != possible) {
      emoji = cross
      color <- 'red'
      class <- 'answer-wrong'
    }
    
    print('replacing...')
    #repl = paste0(q, '\n')
    repl <- ''
    repl = paste0(repl, '<p class = "', class, '" id = "', id, '" style = "color:',color, '; background-color:white; padding:5px; border: solid 1px;">', 'Question ', num)
    repl = paste0(repl, ' -- [', earned, ' / ', possible, ' points] ', emoji)
    
    print('check comment..')
    
    if (comment != '') {
      repl = paste0(repl, '\n\n' , comment)
    }
    
    repl = paste(repl, '</p>')
    repl = paste0(repl, q)
    
    s <- gsub(q, repl, s)
    
    setGradebook(currentFile(), input$question, input$points_earned)  
    s <- addTotal(currentFile(), s)  
    assignment$change <- TRUE
    return(s)
  }

  # return regex for totalGrade comment
  totalGradeComment <- function(x, mycomment = '[\\s\\S]*?') {
    paste0(beginTotalGradeComment, mycomment, endTotalGradeComment)
  }
  
  addComment <- function(s, comment) {
    
    print('commenting ..')
   
    x <- str_extract(s, totalGradeComment())
    t <- totalGradeComment(mycomment = comment)
    
    s <- gsub(x, totalGradeComment(mycomment = comment), s, fixed = TRUE)
    
    assignment$change <- TRUE
    return(s)
  }
  
  
    
  # adds total to file; 
  # replaces total-grade element of txt with total grade
  addTotal <- function(f, txt) {
    
    current <- str_extract(txt, '<h2 id = "total-grade">[\\s\\S]*?</h2>')
    repl <- paste0('<h2 id = "total-grade">', 
                   assignment$gradebook[basename(f),'total'], '</h2>')
    
    gsub(current, repl, txt)      

  }
  
}


