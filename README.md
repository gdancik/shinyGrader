# shinyGrader
*shinyGrader* is a web-based tool for grading assignments submitted in the form of HTML files. While designed for grading of R/HTML notebooks, *shinyGrader* also works with Word files converted to HTML. 

## Installation instructions

1. Install R for your system, available from https://cran.r-project.org/

2. Install R Studio Desktop for your system, available from https://rstudio.com/products/rstudio/download/#download

3. Open R Studio and install the package *shiny* by typing the following in the R console:

`install.packages('shiny')`

## Launching *shinyGrader*

1. To launch *shinyGrader* from R Studio, type the following in the R console:

`library(shiny)`
 `runGitHub('shinyGrader', 'gdancik')`

This will download and open *shinyGrader* in your web browser. The first time you run *shinyGrader*, additional packages will be installed, which may take a minute.

## Using *shinyGrader*

1. Put submissions for the assignment to be graded in a folder.

2. In this folder, create a file named *questions.csv* which contains the point value for each question. This file should contain two columns, separated by a comma, and no header. For example, 

`1,3
2,5
`
would indicate that the file contains 2 questions, which should have the format 'Question 1' and 'Question 2' in the assignment.

3.  A new folder, named *graded*, is created in this directory that will contain the graded assignments. From the *Grade questions* tab, click on *Grade* to add the graded score and an optional comment above the current question.

4. From the 'Final comments' tab, you can view the entire assignment and add additional comments next to the total score for each assignment.

5. The 'Gradebook' tab contains the gradebook which can be exported (coming soon).
