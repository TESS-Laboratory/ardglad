% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ard-glad-urls.R
\name{ard_glad_urls}
\alias{ard_glad_urls}
\alias{print.ard_glad_urls_umd}
\alias{print.ard_glad_urls_aws}
\title{Generate URLs for GLAD ARD data}
\usage{
ard_glad_urls(
  aoi,
  start_date,
  end_date = NULL,
  reproj = TRUE,
  src = c("umd", "aws")
)

\method{print}{ard_glad_urls_umd}(x, ...)

\method{print}{ard_glad_urls_aws}(x, ...)
}
\arguments{
\item{aoi}{an sf or sfc object representing the area of interest.}

\item{start_date}{a character string representing the start date.}

\item{end_date}{a character string representing the end date. if NULL, only
the start date is used and tiles for a single 16-day interval are returned.}

\item{reproj}{a logical value indicating whether to reproject the input if
it is not in the WGS84 CRS.}

\item{src}{a character string indicating the source of the data. Options are
"umd" (University of Maryland) and "aws" (Amazon Web Services).}

\item{x}{an ard_glad_urls object}

\item{...}{not used}
}
\value{
An ard_glad_urls object (an s3 class), essentially a simple named
list of character vectors containing the URLs for the requested data.
Names correspond to the 16-day interval range.
}
\description{
Generate URLs for GLAD ARD data

print method for ard_glad_urls objects

print method for ard_glad_urls objects
}
\details{
Each item in the returned list comprises the tile(s) for a given 16-day
interval.
}
\concept{GLAD ARD urls}
