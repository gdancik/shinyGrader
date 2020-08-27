if (!require(stringr)) install.packages('stringr')

library(stringr)

###################################################
# The main reactive framework is 
#   assignment(files, fileNum, changed)
# 
# getAssignment - depends on all of above
# getQuestion - getAssignment for input$question
##################################################

R_ASSIGNMENT <- NULL
cross <- '&#10060'
exclamation <- '&#10071'
correct <- '&#9989'
#questionStr <- '### Question '
questionStr <- 'Question '
#questionStr <- '# '

R_QUESTION_STR <- '# '
#R_QUESTION_STR <- '### Question '


getQuestion <- function(s, num) {
  
  if (R_ASSIGNMENT) {
    s2 <- strsplit(s,'<pre class="r">')[[1]]
    num <- paste0(R_QUESTION_STR, num)
    g <- grep(num, s2)
    return(paste0('<pre class="r">', s2[g]))
  } else {
    
    
    #graded_div <- paste0('<div id = "q', num, '-graded"[\\s\\S]*?</div>')
    
    #gd <- str_extract(s, graded_div)
    
    pattern <- paste0('\n.*', questionStr, num, '[\\s\\S]*?\n(?=(.*',questionStr, '))')
    
    # get match
    m <- str_match(s, pattern)
    
    if (is.na(m[1,1])) {
      pattern <- paste0('\n.*', questionStr, num, '[\\s\\S]*</body>')
      m <- str_match(s, pattern)  
    }
    
    #if (is.na(gd)) {
      gd <- ''
    #}
    
    paste0('</br>', gd, '</br>', m[1,1])
    m[1,1]
    
  }
}


beginTotalGradeComment <- '\n<!-- BEGIN TOTAL GRADE COMMENT --->\n'
endTotalGradeComment <- '\n<!-- END TOTAL GRADE COMMENT --->\n'

# Define a server for the Shiny app
function(input, output, session) {

  shinyjs::hide('tab')
  shinyjs::disable('num_questions_div')
  
  output$pointValueHeader <- renderUI({
    
    list(
      h2('Upload your assignments'),
      p('Select a directory containing the assignments to upload.',
        'All HTML files in the directory will be uploaded.',
        'Note that your directory must contain a questions.csv file',
        'containing the point values for each question')
    )
  })
  
  
  observeEvent(input$tab, {
    #print('clicked tab')
    
    if (input$tab == 'Grade questions') {
      
#      <link rel="stylesheet" type="text/css" href="shinythemes/css/cerulean.min.css"/>
        
        #shinyjs::runjs("$('head').append('<link rel=\"stylesheet\" type=\"text/css\" href=\"shinythemes/css/cerulean.min.css\">');")
      
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
   # defaultPath = "Desktop",
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
      
      #print(r)
      assignment$question_points <- r$V2
      if (!all(r$V1 == 1:nrow(r))) {
        showNotification(HTML('<h3>Error: Invalid questions.csv format</h3><p>Questions must be numbered from 1 - n</p>'),
                         duration = NULL, type = 'warning')
        return()
      }
      assignment$num_questions <- nrow(r)
      
    } else {
      showNotification(HTML('<h3>Error: Missing questions.csv</h3><p>You must have a questions.csv file in the assignments folder</p>'),
                       duration = NULL, type = 'warning')
      return()
    }
    
    
    # reset global variables
    R_ASSIGNMENT <<- NULL
    questionStr <<- 'Question '
    
    cat('update to: ', assignment$num_questions)
    updateTextInput(session, 'num_questions', 'Number of questions', assignment$num_questions)
    
    updateQuestionInput(1)
    
    #cat('selected directory: ', dir_selected, '\n')
    files <- Sys.glob(paste0(dir_selected, '/*.html'))
    
    s <- unlist(sapply(files, validateFormat))
    
    msg <- paste0(s, collapse = "</br>")
    
    cat('msg: ', msg, '\n')
   
    if (!is.null(msg) && msg != "") {
      
      output$user_question <- renderUI({
        HTML("<h3 style = 'color:red'>Assignment format not valid</h3>", 
             "<p>", msg, "</p")
      })
      
      shinyjs::show('tab')
      return()
    }
    
    s <- unlist(sapply(files, getGradedFileName))
    d <- getGradedFileName(files[1], TRUE)
    dir.create(d, showWarnings = FALSE)
    
    
    #cat("check is: ", input$chkOverwrite, '\n')
    #scan(what = character())
    
    if (input$chkOverwrite) {
      f <- Sys.glob(paste0(d,'/*'))
      if (length(f) > 0) {
        showNotification(HTML('<h3>Deleting files in graded directory</h3>',
                              '<p>Files are deleted when overwrite option is checked</p>'),
                         id = 'deleting-files', duration = 7, type = "error")
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
    
    output$pointValueHeader <- renderUI({
      
      list(
        h2('Question point values'),
        p('The question and point values uploaded from questions.csv ',
          'are given below'), br()
        )
    })
    
    
  })

  # get graded file name or directory
  getGradedFileName <- function(f, dir = FALSE) {
    b <- basename(f)
    d <- basename(dirname(f))

    r <- paste0(dirname(f), '/graded/')
    if (dir) {
      return(r)
    } 
    
    shinyjs::show('tab')
    
    g <- paste0(r,b)
    gsub('\\.html$', '_graded.html', g)
    
    # db <- paste0('/',d, '/', b)
    # 
    # if (dir) {
    #   return(gsub(paste0(db, '$'), paste0('/graded/'), f))
    # }
    # g <- gsub(paste0(db, '$'), paste0('/graded/',b), f)
    # gsub('\\.html$', '_graded.html', g)
    
  }

  
  # questions must be numeric (1, 2, etc)    
  validateFormat <- function(f) {
    r <- readAssignment(f, NULL)
    questions <- as.numeric(str_match_all(r, paste0(questionStr, "(\\d+)"))[[1]][,2])
    
    cat('questions: ', questions, '\n')
    
    msg <- NULL
    n <- assignment$num_questions
    
    save(r, questions, n, questionStr, file = 'a.RData')
    
    cat('n: ', n, '\n')
    
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
  
  resetComment <- function() {
    updateTextAreaInput(session, 'comment', 'Question comment', '')
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
    
    #cat('reading: ', f, '...\n')
    s <- readLines(f)
    
    if (is.null(R_ASSIGNMENT)) {
      R_ASSIGNMENT <<- any(grepl('<pre class="r">', s) )
      if (R_ASSIGNMENT) {
        questionStr <<- R_QUESTION_STR
      }
    }
    
    
    assignment_orig <- paste0(s, collapse = '\n')
    
    
     if (!is.null(copy_file) && !file.exists(copy_file)) {
      assignment_orig <- paste(assignment_orig, "\n\n<style>\n.answer-correct span{\ncolor:green;\nfont-weight:normal;\n}\n",
                               "\n.answer-wrong span {\ncolor:red;\nfont-weight:normal;\n}\n",
                               "</style>")

      # get first div
      
      if (R_ASSIGNMENT) {
        s <-  str_extract(assignment_orig, '<div\\b[^>]*>[\\s\\S]*?</div>')
      } else {
        s <- str_extract(assignment_orig, '<body.*?>')
      }
      
      assignment_orig <- gsub(s, paste0(s, '\n', 
                                        '<div id = "total-grade-div" style = "color:blue; border: 1px solid blue; padding:10px; margin-bottom:10px;">',
                                        '<h2 id = "total-grade">Grade Here</h2><h3>',
                                        beginTotalGradeComment, endTotalGradeComment,
                                        '</h3></div>'),
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
  nextQuestion <- function(offset, notify = TRUE) {

    selected = 1
    if (!is.null(offset)) {
      selected = as.integer(input$question) + offset
    }
    
    if (selected > input$num_questions) {
      if (notify) {
        showNotification(HTML('<h3> This was the last question</h3><p>Starting over at question #1</p>'),
                       duration = 7, type = 'message')
      }
      selected = 1
    }
    
    updateQuestionInput(selected)
    
    #cat('update with points: ', assignment$question_points[selected], '...\n')
    updatePointsPossible(assignment$question_points[selected])
    
    resetComment()
    
    }
  
  
  # go to next or previous person, depending on offset; 
  #   if past the end, go to next question for first person
  nextPerson <- function(offset, notify = TRUE) {
    
    num <- assignment$fileNum + offset
    
    # if passed the end, next question for first person
    if (num > length(assignment$files)) {
        if (notify) {
          showNotification(HTML('<h3>This was the last student</h3><p>Starting over at person #1</p>'),
                         duration = 7, type = 'message')
        }
        selected = 1
        num <- 1
        if (input$tab != 'Final comments') nextQuestion(1, notify)
    } else if (num < 1) {
      shinyjs::alert("already at first person")
      return()
    }
    
    assignment$fileNum <- num
    
    resetComment()
    

  }
  
  # get assignment using reactive values files[fileNum]
  getAssignment <- reactive({
    assignment$changed
    assignment$changed <- FALSE
    readAssignment(currentFile())
  })
  
  divCorrectRE <- function(num) {
    paste0('<div id = "q', num, '+-graded" class = \"answer-correct\".*?</div></br>')
  }
  
  divWrongRE <- function(num) {  
    paste0('<div id = "q', num, '+-graded" class = \"answer-wrong\".*?</div></br>')
  }
  
  # get the current question
  question <- reactive({
    #cat('getting question...')
    if (is.null(assignment$files)) {
      return ("")
    }
    
    a <- getAssignment()
    g <- getQuestion(a, input$question)
  
    p1 <- divCorrectRE(input$question)
    p2 <- divWrongRE(input$question)
    
    if (grepl(p1, g) || grepl(p2, g)) {
        shinyjs::disable('grade')
      shinyjs::enable('delete')
    } else {
      shinyjs::enable('grade')
      shinyjs::disable('delete')
    }
    
    g
    
  })

  studentHeader <- function(include_question = TRUE) {
    p <- paste0('<h3 style = "color: #317eac;"> Student #', assignment$fileNum)
    if (include_question) {
      p <- paste0(p, ', Question ', 
                  input$question#, ', ', 
                  #num_graded_string(currentFile())
      )
    }
                
    qs <- getQuestionsThatNeedGrading()
    
    if (length(qs) > 0) {
      p <- paste0(p, '<span style = "font-size:80%;"> (You still need to grade questions: ', formatVector(qs, 5), ')</span>')
    } else {
      p <- paste0(p, '<span style = "font-size:80%;"> (Grading for this student complete)</span>')
    }
    
    more <- ''
    if (!R_ASSIGNMENT) {
      more <- '</br>'
    }
    paste(p, '</h3>', more)
  }
  
  output$user_question <- renderUI({
    HTML(studentHeader(), question())
  }) 
  
  output$user_assignment <- renderUI({
    
    cmt <- getCurrentTotalGradeComment()
    
    if (is.null(cmt)) {
      shinyjs::enable('add-comment')
      shinyjs::disable('delete-comment')
    } else {
      shinyjs::disable('add-comment')
      shinyjs::enable('delete-comment')
    }
    
    HTML(studentHeader(include_question = FALSE), getAssignment())
    # HTML(paste0('<iframe height = "500px" width = "500px" src = "file://', 
    #      currentFile(), '"></iframe>'))
    # 
    
    
  })

  
  observe( {
      if (is.null(assignment$gradebook)) {
        return()
      }
    
      #print('re-render gradebook...')
    
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
    
    updatePointsPossible(assignment$question_points[1])
  })
  
  currentFile <- reactive({
    assignment$files[assignment$fileNum]
  })
  
  observeEvent(input$grade,{
    
   if (is.na(input$points_possible) | is.na(input$points_earned)) {
      shinyjs::alert('at least one input is not valid')
      return()
    }
    
    g <- grade(getAssignment(), input$question, input$points_earned, 
               input$points_possible, emojiize(input$comment))
    
    write(g, file = currentFile())
    
    assignment$changed <- TRUE
    
    notify <- TRUE
    
    if (length(getStudentsThatNeedMoreGrading()) == 0) {
      showNotification(HTML('<h3>All assignments have been graded</h3><p>You should now add final comments<p>'), 
                       id = 'grading-complete', duration = 7, type = 'message')
      notify <- FALSE
    }
    
    if (input$advance == 'Next question') {
      nextQuestion(1, notify)
    } else if (input$advance == "Next person") {
      nextPerson(1, notify)
    }
    
    
  })
  
  observeEvent(input$`add-comment`,{
    
    if (trimws(input$`final-comment`) == '') {
      return()
    }
    
    g <- addComment(getAssignment(), emojiize(input$`final-comment`))
    
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
    a <- getAssignment()
    
    p1 <- divCorrectRE(input$question)
    p2 <- divWrongRE(input$question)
 
    g2 <- gsub(p1, '', g1)
    g2 <- gsub(p2, '', g2)
    
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
                   #cat("next person")
    nextPerson(1)
  }, ignoreInit = TRUE)
  
  observeEvent(c(input$previousp, input$previouspFinal), {
    nextPerson(-1)
  }, ignoreInit = TRUE)
  
  source('gradebook.R', local = TRUE)
  
  emojiize <- function(x) {
    x <- gsub(':smile:', '&#128512;', x)
    x <- gsub(':frown:', '&#128533;', x)
    x
  }
  
  grade <- function(s, num, earned, possible, comment = '') {
    
    
    #print('grading..')
    q = paste0(questionStr, num)
    
    id <- paste0('q', num, '-graded')
    
    emoji = correct 
    color <- 'green'
    class <- 'answer-correct'
    if (earned != possible) {
      emoji = cross
      color <- 'red'
      class <- 'answer-wrong'
    }
    
    #print('replacing...')
    #repl = paste0(q, '\n')
    repl <- ''
    repl = paste0(repl, '<div id = "', id, '" class = "', class, '" style = "color:',color, '; background-color:white; padding:5px; border: solid 1px;">', 'Question ', num)
    repl = paste0(repl, ' -- [', earned, ' / ', possible, ' points] ', emoji)
    
    #print('check comment..')
    
    if (comment != '') {
      if (R_ASSIGNMENT) {
          repl = paste0(repl, '\n\n' , comment)
      } else {
          repl <- paste0(repl, '</br></br>', comment)
      }
    }
    
    repl = paste(repl, '</br></div></br>')
    repl = paste0(repl, q)
    
    #cat('replacing with:\n', repl, '...\n')
    
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
    
    #print('commenting ..')
   
    x <- str_extract(s, totalGradeComment())
    t <- totalGradeComment(mycomment = comment)
    
    s <- gsub(x, totalGradeComment(mycomment = comment), s, fixed = TRUE)
    
    assignment$change <- TRUE
    return(s)
  }
  
  
    
  # adds total to file; 
  # replaces total-grade element of txt with total grade
  addTotal <- function(f, txt) {
    
    num <- assignment$gradebook[basename(f),'total']
    den <- sum(assignment$question_points)
    
    current <- str_extract(txt, '<h2 id = "total-grade">[\\s\\S]*?</h2>')
    repl <- paste0('<h2 id = "total-grade"> Total grade: ', 
                   num, '/',den,' (', round(num/den*100),  '%)</h2>')
    
    gsub(current, repl, txt, fixed = TRUE)      

  }
  
  formatVector <- function(x, n) {
    if (length(x) > n) {
      x <- x[1:n]
      x <- c(x, '...')
    }
    paste(x, collapse = ',')
  }
  
  # returns student numbers for assignments that need grading
  getStudentsThatNeedMoreGrading <- function() {
    (1:nrow(assignment$gradebook))[apply(is.na(assignment$gradebook), 1, any)]
  }
  
  # get questions for current student
  getQuestionsThatNeedGrading <- function() {
    (1:ncol(assignment$gradebook))[is.na(assignment$gradebook[assignment$fileNum,])]
  }
  
  output$needsGrading <- renderUI({
    x <- getStudentsThatNeedMoreGrading()
    if (length(x) == 0) {
      return(h3('All assignments have been graded', style = 'color:darkred;'))
    }
    x <- formatVector(paste0('Student #', x), 5)
    h3(paste('Needs grading: ', x), style = 'color:darkred;')
  })
 
  getCurrentTotalGradeComment <- function() {
    tt <- totalGradeComment(mycomment = '([\\s\\S]*?)')
    g <- getAssignment()
    
    s <- str_match(g, tt)
    
    if (is.na(s[1]) || ncol(s) < 2 || trimws(s[1,2]) == '') {
      return(NULL)
    }
    return(s[1,2])
  }
  
   
}


