#' Get a label layout.
#'
#' A utility to retrieve pre-formatted label layout configurations.
#'
#' This function allows easy access to label layout configurations for a range of vendor's label formats. More layouts are being continually added and users are welcome to submit their own. See the package website for more information on the available configurations
#'
#' @param label.type A character string describing the label vendor, product number and output page size.
#'
#' @seealso \code{\link[idLabelR]{make_pdf}}.
#'
#' @examples
#' require(idLabelR)
#'
#' example_spreadsheet = system.file("extdata", "Example.txt", package = "idLabelR")
#' ids = make_ids(example_spreadsheet)
#' layout = get_layout("cryobabies_LCRY_1700_A4")
#' make_pdf(ids, layout)
#'
#' @export
get_layout = function(label.type){
	layout = NULL
	if(label.type == "cryobabies_LCRY_1700_A4"){
		layout = list(
			# Positions on the page of the label centres.
			label.x = c(33.2, 68.9, 104.9, 140.2, 175.9),
			label.y = c(13.3, 30.7, 46.4, 62.2, 77.9, 93.7, 109.4, 125.1, 140.9, 156.6, 172.4, 188.1, 203.8, 219.6, 235.3, 251.1, 266.8),
			page.width = 210,
			page.height = 297,
			units = "mm",
			text.font = 2, # Bold.
			text.size = 1, # 100% default size.
			sublabel.font = 1, # Normal.
			sublabel.size = 0.5, # 50% default size.
			sublabel.offset = 2, # Distance below ID text.
			# Optional date-of-printing timestamp position.
			timestamp.x = 210 - 15,
			timestamp.y = 17,
			timestamp.size = 0.75
		)
	}

	if(label.type == "Herma_4333_A4"){
		layout = list(
			# Positions on the page of the label centres.
			label.x = c(20.5, 48.5, 76.5, 104.5, 132.5, 160.5, 188.5),
			label.y = c(18, 28, 38, 48, 58, 68, 78, 88, 98, 108, 118, 128, 138, 148, 158, 168, 178, 188, 198, 208, 218, 228, 238, 248, 258, 268, 278),
			page.width = 210,
			page.height = 297,
			units = "mm",
			text.font = 2, # Bold.
			text.size = 0.8, # 80% default size.
			sublabel.font = 1, # Normal.
			sublabel.size = 0.5, # 50% default size.
			sublabel.offset = 2, # Distance below ID text.
			# Optional date-of-printing timestamp position.
			timestamp.x = 210 - 15,
			timestamp.y = 17,
			timestamp.size = 0.75
		)
	}

	if(is.null(layout)) warning("There is no available label configuration for '", label.type, "'.")
	return(layout)
}

##########

