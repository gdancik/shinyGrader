# gradebook related functions, that use the 
# assignment$gradebook reactive

setGradebook <- function(f,q,val) {
#   cat('setting gb with', f, ':', q,' and ', val, '...')
    assignment$gradebook[basename(f),as.integer(q)] <- as.integer(val)  
    assignment$gradebook$total <- rowSums(assignment$gradebook[,1:(ncol(assignment$gradebook)-1), drop = FALSE],
                                          na.rm = TRUE)
 #   cat('done')
}

num_graded_string <- function(f) {
  if (is.null(f)) {
    return()
  }
  n1 <- sum(!is.na(assignment$gradebook[basename(f),-1 ]))
  paste0('Num graded: ', n1, '/', ncol(assignment$gradebook) -1)
}

getGradesFromString <- function(x) {
  if (length(x) > 1) {
    stop('x must have length 1 in getGradesFromString')
  }
  str_match_all(x, 'Question (\\d+) -- \\[(\\d+) / \\d+ points\\]')[[1]]
}


