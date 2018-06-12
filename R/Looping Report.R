library(knitr)
library(markdown)
library(rmarkdown)

# Looping by trainer
for (trainers in names) {
  rmarkdown::render('/Users/mishn/Documents/testing/layout.Rmd',  # file 2
                    output_file =  paste(trainers, '_', Sys.Date(), ".docx", sep=''), 
                    output_dir = '/Users/mishn/Documents/testing/')
}

# Looping by course
for (course in courses) {
  rmarkdown::render('/Users/mishn/Documents/testing/layout.Rmd',  # file 2
                    output_file =  paste(courses, '_', Sys.Date(), ".docx", sep=''), 
                    output_dir = '/Users/mishn/Documents/testing/')
}
