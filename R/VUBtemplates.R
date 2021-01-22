#' VUBtemplates
#'
#' Package with VUB templates
#'
#' @name Article
#' @docType package
NULL


#  Internal functions
#  Utilities for article()
#  Largely inspired from the rticles package
#' @keywords internal
find_file <- function (template, file) {
  template <- system.file("rmarkdown", "templates", template, file, package = "Article")
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
  inherit_pdf_document(..., template = find_resource("article", "VUB_Article.tex"), md_extensions = md_extensions, citation_package = "natbib")
}
