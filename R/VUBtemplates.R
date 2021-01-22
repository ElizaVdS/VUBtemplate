#' VUBtemplates
#'
#' Package with VUB templates
#'
#' @name VUBtemplates
#' @docType package
NULL


#  Internal functions
#  Utilities for article()
#  Largely inspired from the rticles package
#' @keywords internal
find_file <- function (template, file) {
  template <- system.file("rmarkdown", "templates", template, file, package = "VUBtemplate")
  if (template == "") {
    stop("Couldn't find template file ", template, "/", file, call. = FALSE)
  }
  return(template)
}
#' @keywords internal
find_resource <- function (template, file) {
  return(find_file(template, file.path("resources", file)))
}
#' @keywords internal
inherit_pdf_document <- function (...) {
  fmt <- rmarkdown::pdf_document(...)
  fmt$inherits <- "pdf_document"
  return(fmt)
}

#' Article
#'
#' Formatting an article in th VUB style
#'
#' The function is called by the Markdown model Article
#'
#' @param ... Optional arguments passed on to \code{\link{pdf_document}}
#' @param md_extensions Markdown Extensions, cf. \code{\link{pdf_document}}
#'
#' @export
article <- function (..., md_extensions = c("-autolink_bare_uris")) {
  inherit_pdf_document(..., template = find_resource("article", "template.tex"), md_extensions = md_extensions, citation_package = "natbib")
}

#' Knit
#'
#' Creation of all the documents from the models
#'
#' Used to verify that the models function well
#'
#' @param destination Destionation file of the documents
#'
#' @name Knit
NULL


#' @rdname Knit
#' @export
KnitAll <- function (destination="docs") {
  KnitArticle(destination=destination)
  TricoterPresentation(destination=destination)
  TricoterOuvrage(destination=destination)
  TricoterMemo(destination=destination)
}

#' @rdname Knit
#' @export
KnitArticle <- function (destination="docs") {
  # Preparation
  knitr_table_format <- options("knitr.table.format")
  OriginalWD <- getwd()
  tmpdir <- tempdir()
  # Article
  setwd(tmpdir)
  unlink("article", recursive=TRUE)
  rmarkdown::draft("article", template="article", package="VUBtemplate", edit=FALSE)
  setwd("article")
  # Knit to HTML
  options(knitr.table.format='html')
  rmarkdown::render(input="article.Rmd",
                    output_format=bookdown::html_document2(theme="sandstone", toc=TRUE, toc_float=TRUE),
                    output_dir=destination)
  # Knit to pdf
  options(knitr.table.format='latex')
  rmarkdown::render(input="article.Rmd",
                    output_format=bookdown::pdf_book(base_format=EcoFoG::article),
                    output_dir=destination)
  # Copy to destination
  docsDirs <- list.dirs(path=destination, full.names=TRUE, recursive=TRUE)
  dir.create(paste(OriginalWD, "/", destination, sep=""), showWarnings=FALSE)
  dir.create(paste(OriginalWD, "/", destination, "/article", sep=""), showWarnings=FALSE)
  if (length(docsDirs) > 0) {
    sapply(paste(OriginalWD, "/", destination, "/article/", docsDirs, sep=""), dir.create, showWarnings=FALSE)
    docsFiles <- list.files(destination, full.names=TRUE, recursive=TRUE)
    file.copy(from=docsFiles, to=paste(OriginalWD, "/", destination, "/article/", docsFiles, sep=""), overwrite=TRUE)
  }
  # Clean up
  setwd(OriginalWD)
  unlink(paste(tmpdir, "/article", sep=""), recursive=TRUE)
  options(knitr.table.format=knitr_table_format)
}

