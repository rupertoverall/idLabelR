#' Read a spreadsheet.
#'
#' A helper utility to handle reading of spreadsheet files.
#'
#' This function should not need to be called directly by the user.
#'
#' The code used to read the file depends on the filetype. The filetype is
#' determined by the extension given in the \code{file} parameter or can be
#' overridden by the \code{filetype} parameter. For more information about file
#' reading parameters, have a look at the documentation for the respective
#' packages: \code{\link[readxl]{read_excel}} for reading \code{.xlsx} and
#' \code{.xls} files. \code{\link[readODS]{read_ods}} for reading \code{.ods}
#' files. This is the Open Document standard used by software such as OpenOffice
#' and LibreOffice. \code{\link[utils]{read.csv}} for reading \code{.csv} files.
#' If \code{filetype = 'csv2'}, then \code{\link[utils]{read.csv2}} is used
#' (where commas are used as decimal separators as is common in Europe).
#' \code{\link[utils]{read.delim}} for reading \code{.tsv}, \code{.tab},
#' \code{.txt} files. This is just tab-delimited text.
#'
#' @param file The name of the file to be read.
#' @param filetype The filetype of the input spreadsheet. Microsoft Excel
#'   (\code{.xlsx} and \code{.xls}) and Open Document Format (\code{.ods}; as
#'   used by OpenOffice/LibreOffice for example) are supported, as are comma-
#'   and tab-delimited formats (\code{.csv}, \code{.tsv}/\code{.txt}). The
#'   default behaviour is to guess the format from the file extension. If this
#'   does not work or you know better than the machine, then this option can be
#'   used to specify the filetype.
#' @param ... Additional parameters passed to the file reader. The parameters
#'   accepted depend on the input filetype.
#'
#' @seealso \code{\link[idlabelR]{make_ids}}.
#'
#' @importFrom tools file_path_sans_ext
#' @importFrom readxl read_excel read_xls read_xlsx
#' @importFrom readODS read_ods
#' @importFrom utils read.csv read.csv2 read.delim
#'
#' @export
read_db = function(file, filetype = "auto", ...){
	df = NULL

	if(filetype == "auto"){
		ext = tolower(tools::file_ext(file))
		if(!ext %in% c("csv", "csv2", "tsv", "tab", "txt", "excel", "xls", "xlsx", "ods")){
			stop("stickR cannot match your file to a readable filetype.")
		}
		filetype = ext
	}

	if(filetype == "csv"){
		df = utils::read.csv(file, ...)
	}else if(filetype == "csv2"){
		df = utils::read.csv2(file, ...)
	}else if(filetype == "tsv" | filetype == "tab" | filetype == "txt"){
		df = utils::read.delim(file, ...)
	}else if(filetype == "excel"){
		df = readxl::read_excel(file, ...)
	}else if(filetype == "xls"){
		df = readxl::read_xls(file, ...)
	}else if(filetype == "xlsx"){
		df = readxl::read_xlsx(file, ...)
	}else if(filetype == "ods"){
		df = readODS::read_ods(file, ...)
	}

	attr(df, "filetype") = filetype
	return(df)
}
