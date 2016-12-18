library(extrafont)

#http://blog.revolutionanalytics.com/2012/09/how-to-use-your-favorite-fonts-in-r-charts.html
loadfonts(device = "win")

Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.16/bin/gswin64c.exe")

source("functions.R")

today_date <- as.character(format(Sys.Date(), "%d %b %Y"))

#http://reed.edu/data-at-reed/software/R/markdown_multiple_reports.html
for (i in 15:15){
  course_dataset <- dataset[dataset$A.2..Select.the.name.of.your.Erasmus.Mundus.Joint.Master.Degree..EMJMD.. == tenormore$Course[i],]

  rmarkdown::render('./Reports/report_script.Rmd',  # file 2
                    output_format = "pdf_document",
                    output_file =  paste(as.character(tenormore$Course[i]), '_', Sys.Date(), ".pdf", sep=''), 
                    output_dir = './Reports/courses')
 # embed_fonts(paste(as.character(tenormore$Course[i]), '_', Sys.Date(), ".pdf", sep=''))
}
