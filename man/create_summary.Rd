% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MPI_create.R
\name{create_summary}
\alias{create_summary}
\title{Create Summary}
\usage{
create_summary(input, path_type, recursive)
}
\arguments{
\item{input}{Input type of either a bin file path, MPI path or an MPI object.}

\item{path_type}{The file type within the folder to create summary for.}

\item{recursive}{TRUE applies the operation to all nested elements.}
}
\value{
Data frame of bin file or MPI summary.
}
\description{
Create Summary
}
\details{
Function to create a summary of key information of a bin file or MPI path.
}
