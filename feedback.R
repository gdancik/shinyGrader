# creates feedback list for Q1 - Qn, and 'final'
create_feedback <- function(n) {
  feedback <- vector("list", n+1)
  names(feedback)[1:n] <- paste0('Q', 1:n)
  names(feedback)[n+1] <- 'final'
  feedback
}


get_feedback_index <- function(myfeedback, qnum) {
  q <- paste0('Q', qnum)
  if (!q %in% names(myfeedback)) {
    stop('feedback for ', q, ' not found')
  }
  q
}

# returns TRUE if feedback for qnum exists
feedback_exists <- function(myfeedback, qnum, feedback) {
  q <- get_feedback_index(myfeedback, qnum)
  fb <- sapply(myfeedback[[q]], `[[`,2)
  return (feedback %in% fb)
}


# adds feedback for qnum
add_feedback <- function(myfeedback, qnum, points, feedback) {
  
  q <- get_feedback_index(myfeedback, qnum)
  if (feedback_exists(myfeedback, qnum, feedback)) {
    return(myfeedback)
  }
  
  new_fb <- c(points = points, feedback = feedback)
  if (is.null(myfeedback[[q]])) {
    myfeedback[[q]] <- list(new_fb)
  } else {
    myfeedback[[q]] <- append(myfeedback[[q]], list(new_fb))
  }
  myfeedback
}


format_feedback <- function(myfeedback, qnum) {
  q <- get_feedback_index(myfeedback, qnum)
  #sapply(feedback[[q]], function(xx) paste(xx$points, '-', xx$feedback))
  sapply(myfeedback[[q]], function(xx) paste0(xx, collapse = ' - '))
}

# feedback <- create_feedback(3)
# feedback <- add_feedback(feedback, 1, 10, "This is a test")
# feedback <- add_feedback(feedback, 1, 3, "Wrong")
# feedback <- add_feedback(feedback, 1, 3, "Wronged")
# feedback <- add_feedback(feedback, 1, 3, "Wrong")
