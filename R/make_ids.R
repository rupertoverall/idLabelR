#' Generate randomised IDs.
#'
#' A utility to create randomised IDs from an experiment database/spreadsheet.
#'
#' This function creates a new spreadsheet file in the same format as the input
#' file (note the exception about \code{.xls} below). This new file will be
#' named as the input file with "_IDs" appended to the name.
#'
#' There is no support for writing to the obsolete \code{.xls} format so the
#' modified files from \code{.xls} input will be written as \code{.xlsx} with a
#' message.
#'
#' The label format is designed to allow for an experiment name (ideally a short
#' abbreviation), a randomised number and a control character/checksum. Not all
#' of these components are necessary (a minimum requirement is only the
#' randomised number---but this is not recommended). For maximum compatibility
#' (with R variable names and many spreadsheet tools for example), it is
#' recommended that IDs begin with a letter rather than a number. The format
#' uses 3 special characters: \itemize{ \item \code{?} is the randomised number.
#' Adjacent symbols will be collapsed, but it is possible (not particularly
#' recommended) to have the number appear multiple times. \item \code{@} is a
#' control character (letters that may be ambiguously written or confused with
#' digits are not used) that is useful as a control to distinguish
#' easily-confused ID numbers. \item \code{#} is a checksum character. This is
#' not yet implemented. }
#'
#' @param file A spreadsheet file containing experimental metadata. It is
#'   important that information for individual subjects is in rows.
#' @param range The range of ID numbers. The default will number from 1 to a
#'   maximum that is 20\% more than the total number of subjects in the
#'   spreadsheet (this allows some additional subjects to be added at a later
#'   date if necessary). If you want to use a larger number of subjects in the
#'   future (which should be randomised together) or if the numbering should not
#'   start at 1, then this option can be used to override the default.
#' @param format A string specifying the components making up the ID. See
#'   Details to learn how to customise this.
#' @param filetype The filetype of the input spreadsheet. Microsoft Excel
#'   (\code{.xlsx} and \code{.xls}) and Open Document Format (\code{.ods}; as
#'   used by OpenOffice/LibreOffice for example) are supported, as are comma-
#'   and tab-delimited formats (\code{.csv}, \code{.tsv} or \code{.txt}). The
#'   default behaviour is to guess the format from the file extension. If this
#'   does not work or you know better than the machine, then this option can be
#'   used to specify the filetype.
#' @param seed The seed used to randomise the IDs. There is no real reason that
#'   this should need to be changed, but you can if you want to.
#' @param ... Additional parameters passed to the file reader. The parameters
#'   accepted depend on the input filetype.
#'
#' @return Invisibly returns the randomised IDs (in the order they appear in the
#'   spreadsheet) as a character vector.
#'
#' @seealso \code{\link[idLabelR]{read_db}}, or \code{\link[idLabelR]{write_db}}
#'   for additional information on the readable/writeable file formats.
#'
#' @examples
#' require(idLabelR)
#'
#' example_spreadsheet = system.file("extdata", "Example.txt", package = "idLabelR")
#' ids = make_ids(file = example_spreadsheet)
#'
#' @export
make_ids = function(file, range = c(0, 0), format = "X.?.@", filetype = "auto", seed = 1, ...){
	df = read_db(file, ...)
	filetype = attr(df, "filetype")

	if(all(range == 0)){
		range = c(1, ceiling(nrow(df) * 1.2))
	}else if(any(is.na(as.numeric(range)))){
		stop("An invalid range was specified.")
	}else if(length(range) == 1){
		range = c(1, as.numeric(range))
	}else{
		range = range(as.numeric(range))
	}

	if(abs(diff(range)) < nrow(df)){
		stop("Not enough unique ID numbers have been specified.")
	}
	set.seed(seed)
	id.numbers = sample(min(range):max(range))
	id.letters = rep(c("a", "b", "c", "d", "e", "f", "g", "h", "k", "m", "n", "p", "q"), length = length(id.numbers))

	if(length(grep("\\?", format)) == 0){
		stop("There is no ID number specified in the format")
	}else if(length(grep("\\?", format)) > 1){
		warning("There are multiple ID numbers specified in the format")
	}

	ids = as.character(sapply(1:nrow(df), function(n){
		gsub("\\@+", id.letters[n], gsub("\\?+", id.numbers[n], format))
	}))

	df = cbind(IDs = ids, df)
	attr(df, "filetype") = filetype
	write_db(df, file = file, filetype = filetype, ...)

	invisible(ids)
}
