prepare_for_arxiv <- function() {
  
  # assumes you have done the incantation after generating the tex files of
  # pdflatex paper; biber paper; pdflatex paper
  
  if (!dir.exists("arxiv")) dir.create("arxiv")
  
  zip_name <- "arxiv/arxiv-submission.zip"
  tex_files <-list.files(pattern = "tex$|bbl$")
  figures <- list.files("./figures/", recursive = TRUE, full.names = TRUE)
  
  
  files_to_zip <-c(tex_files, figures)
  
  if (file.exists(zip_name)) {
    file.remove(zip_name)
  }
  
  zip(zipfile = zip_name, files = files_to_zip)
  
}