#' Write a spreadsheet with IDs.
#'
#' A helper utility to handle writing of spreadsheet files.
#'
#' This function should not need to be called directly by the user.
#'
#' The code used to write the file depends on the filetype. The filetype is
#' determined by the extension given in the \code{file} parameter or can be
#' overridden by the \code{filetype} parameter. For more information about file
#' writing parameters, have a look at the documentation for the respective
#' packages: \code{\link[openxlsx]{write.xlsx}} for writing \code{.xlsx} files.
#' Note that an input filetype of \code{.xls} will cause a \code{.xlsx} file to
#' be written (as \code{.xls} is not supported).
#' \code{\link[readODS]{write_ods}} for writing \code{.ods} files. This is the
#' Open Document standard used by software such as OpenOffice and LibreOffice.
#' \code{\link[utils]{write.csv}} for writing \code{.csv} files. If
#' \code{filetype = 'csv2'}, then \code{\link[utils]{write.csv2}} is used (where
#' commas are used as decimal separators as is common in Europe).
#' \code{\link[utils]{write.table}} for writing \code{.tsv}, \code{.tab},
#' \code{.txt} files. This is just tab-delimited text. The same file extension
#' used for the input file will be used to write the output.
#'
#' @param df A \code{\link{data.frame}} containing the experiment data.
#' @param file The name of the file to be written to.
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
#' @importFrom openxlsx write.xlsx
#' @importFrom readODS write_ods
#' @importFrom utils write.csv write.csv2 write.table
#'
#' @export
write_db = function(df, file, filetype, ...){

	file = tools::file_path_sans_ext(file)
	filetype = attr(df, "filetype")

	if(filetype == "csv"){
		utils::write.csv(df, paste0(file, "_IDs", ".csv"), ...)
	}else if(filetype == "csv2"){
		utils::write.csv2(df, paste0(file, "_IDs", ".csv"), ...)
	}else if(filetype == "tsv" | filetype == "tab" | filetype == "txt"){
		utils::write.table(df, paste0(file, "_IDs.", filetype), row.names = FALSE, quote = FALSE)
	}else if(filetype == "xls"){
		message("stickR cannot write to '.xls' files. An '.xlsx' file will be created instead.")
		openxlsx::write.xlsx(df, paste0(file, "_IDs", ".xlsx"), ...)
	}else if(filetype == "xlsx"){
		openxlsx::write.xlsx(df, paste0(file, "_IDs", ".xlsx"), ...)
	}else if(filetype == "ods"){
		readODS::write_ods(df, paste0(file, "_IDs", ".ods"), ...)
	}
}
