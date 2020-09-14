library(ggplot2)

# 1) What is 2 + 5

2 + 5

# 2) What is your name?

cat("Garrett")


r <- readLines('/Users/dancikg/Desktop/r_grading/assignment/test.html')
s <- paste0(r, collapse = '\n')

q1 <- '<pre class="r">[\\s\\S]+?# 1\\)[\\s\\S]+?# 3\\)'

pattern <- '<pre class="r">[\\s\\S]+?# 1\\)[\\s\\S]+?# 3\\)'
f <- str_extract(s, pattern)
f


str_extract(s, '(?:.(?!<pre class=\"r\">))+# 3\\)')

str_extract(s, '(?:.(?!<pre class="r">))+3\\)')
