#' @name copyTemplate
#' @export copyTemplate
#' 
#' @title Copy Analysis File Template to a Directory
#' @description Various templates are provided as a part of \code{CCFproprietary}
#'   that fit the needs of various individuals or departments.  \code{copyTemplate}
#'   allows the user to copy these templates into a project directory 
#'   without having to manually 'Save As' a file.
#'   
#' @param templatename The file name of the template.  The file extension should be left off.  Note
#'   that there may be up to four templates for each template name, depending on the
#'   extension chosen in \code{ext}.
#' @param renameAs A character string giving the new name of the file in the 
#'   new location.
#' @param copyToDir Location to which the template should be copied.
#' @param listTemplate Logical.  If \code{TRUE}, a list of all available 
#'   templates will be written to the console.  This does not interrupt the 
#'   normal process of the function.
#' @param ext The file extension for the template.
#' @param ... Additional arguments to pass to \code{file.copy}.
#' 
#' @author Benjamin Nutter
#' 
#' @examples
#' \dontrun{
#' copyTemplate("nutter", "01-StudyDesign",
#'              copyToDir="s_plus")
#' }

copyTemplate <- function(templateName, renameAs,
                         copyToDir=getwd(),
                         listTemplates=FALSE, 
                         ext=c("rmd", "rnw", "rhtml", "tex"),
                         ...){
  ext <- match.arg(ext, c("rmd", "rnw", "rhtml", "tex"))
  
  templateName <- paste0(templateName, ".", ext)
  templates <- list.files(system.file("extdata", package="Playground"))
  templateName <- grep(templateName, templates, value=TRUE, ignore.case=TRUE)
  
  if (listTemplates){
    print(list.files(system.file("extdata", package="Playground"))  )
  }
  
  copyFrom <- system.file(file.path("extdata", templateName), package="Playground")
  copyTo <- file.path(copyToDir, paste0(renameAs, ".", ext))
  
  status <- file.copy(copyFrom, copyTo, ...)
  if (!status){
    if (file.exists(copyTo))
      add.msg <- paste0("\nThere already exists a file named '", copyTo, "'\n",
                        "If you wish to overwrite this file, use 'overwrite=TRUE'")
    else add.msg <- ""
  }
  else add.msg <- ""
  
  message(paste0("The template was ", 
                 if(status) "successfully" else "unsuccessfully", 
                 " copied from \n",
                 copyFrom,
                 "\nto\n",
                 copyTo,
                 add.msg))
  
}
