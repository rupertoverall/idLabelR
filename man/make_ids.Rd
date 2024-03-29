% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_ids.R
\name{make_ids}
\alias{make_ids}
\title{Generate randomised IDs.}
\usage{
make_ids(
  file,
  range = c(0, 0),
  format = "X.?.#",
  filetype = "auto",
  seed = 1,
  ...
)
}
\arguments{
\item{file}{A spreadsheet file containing experimental metadata. It is
important that information for individual subjects is in rows.}

\item{range}{The range of ID numbers. The default will number from 1 to a
maximum that is 20\% more than the total number of subjects in the
spreadsheet (this allows some additional subjects to be added at a later
date if necessary). If you want to use a larger number of subjects in the
future (which should be randomised together) or if the numbering should not
start at 1, then this option can be used to override the default.}

\item{format}{A string specifying the components making up the ID. See
Details to learn how to customise this. The default is "X.?.#".}

\item{filetype}{The filetype of the input spreadsheet. Microsoft Excel
(\code{.xlsx} and \code{.xls}) and Open Document Format (\code{.ods}; as
used by OpenOffice/LibreOffice for example) are supported, as are comma-
and tab-delimited formats (\code{.csv}, \code{.tsv}, \code{.tab} or
\code{.txt}). The default behaviour is to guess the format from the file
extension. If this does not work or you know better than the machine, then
this option can be used to specify the filetype.}

\item{seed}{The seed used to randomise the IDs. There is no real reason that
this should need to be changed, but you can if you want to.}

\item{...}{Additional parameters passed to the file reader. The parameters
accepted depend on the input filetype.}
}
\value{
Invisibly returns the randomised IDs (in the order they appear in the
  spreadsheet) as a character vector.
}
\description{
A utility to create randomised IDs from an experiment database/spreadsheet.
}
\details{
This function creates a new spreadsheet file in the same format as the input
file (note the exception about \code{.xls} below). This new file will be
named as the input file with "_IDs" appended to the name.

There is no support for writing to the obsolete \code{.xls} format so the
modified files from \code{.xls} input will be written as \code{.xlsx} with a
message.

The label format is designed to allow for an experiment name (ideally a short
abbreviation), a randomised number and a control character/checksum. Not all
of these components are necessary (a minimum requirement is only the
randomised number---but this is not recommended). For maximum compatibility
(with R variable names and many spreadsheet tools for example), it is
recommended that IDs begin with a letter rather than a number. The format
uses 3 special characters: \itemize{ \item \code{?} is the randomised number.
Adjacent symbols will be collapsed, but it is possible (not particularly
recommended) to have the number appear multiple times. \item \code{@} is a
control character (letters that may be ambiguously written or confused with
digits are not used) that is useful as a control to distinguish
easily-confused ID numbers. \item \code{#} is a checksum character generated
using the Verhoeff algorithm. }
}
\examples{
require(idLabelR)

example_spreadsheet = system.file("extdata", "Example.txt", package = "idLabelR")
ids = make_ids(file = example_spreadsheet, format = "MyExpt.?.#")

}
\seealso{
\code{\link[idLabelR]{read_db}}, or \code{\link[idLabelR]{write_db}}
  for additional information on the readable/writeable file formats.
}
