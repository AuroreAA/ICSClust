% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{select_plot}
\alias{select_plot}
\alias{select_plot.default}
\alias{select_plot.data.frame}
\alias{select_plot.ICS_crit}
\title{Plot of the Generalized Kurtosis Values of the ICS Transformation}
\usage{
select_plot(object, ...)

\method{select_plot}{default}(
  object,
  select = NULL,
  scale = FALSE,
  screeplot = TRUE,
  type = c("dots", "lines"),
  width = 0.2,
  color = "grey",
  alpha = 0.3,
  size = 3,
  ...
)

\method{select_plot}{data.frame}(
  object,
  type = c("dots", "lines"),
  width = 0.2,
  color = "grey",
  alpha = 0.3,
  ...
)

\method{select_plot}{ICS_crit}(
  object,
  type = c("dots", "lines"),
  width = 0.2,
  color = "grey",
  alpha = 0.3,
  size = 3,
  screeplot = TRUE,
  ...
)
}
\arguments{
\item{object}{an object inheriting from class \code{"ICS"} and containing
results from an ICS transformation or from class \code{"ICS_crit"}.}

\item{\dots}{additional arguments are currently ignored.}

\item{select}{an integer, character, or logical vector specifying for
which components to extract the generalized kurtosis values, or
\code{NULL} for extracting the generalized kurtosis values of all
components.}

\item{scale}{a logical indicating whether to scale the generalized
kurtosis values to have product 1 (defaults to \code{FALSE}).}

\item{screeplot}{boolean. If \code{TRUE} a plot of the generalized kurtosis
values is drawn. Otherwise it is context specific to the \code{ICS_crit} object.
For "med" criterion, the differences between the kurtosis values and the
median are plotted in absolute values. For "discriminatory" the
discriminatory power associated to the evaluated combinations are drawn.}

\item{type}{either \code{"dots"} or \code{"lines"} for the type of plot.}

\item{width}{the width for shading the selected components in case an
\code{ICS_crit} object is given.}

\item{color}{the color for shading the selected components in case an
\code{ICS_crit} object is given.}

\item{alpha}{the transparency for shading the selected components in case
an \code{ICS_crit} object is given.}

\item{size}{size of the points. Only relevant for "discriminatory" criteria.}
}
\value{
An object of class \code{"ggplot"} (see \code{\link[ggplot2:ggplot]{ggplot2::ggplot()}}).
}
\description{
Extracts the generalized kurtosis values of the components obtained via an
ICS transformation and draws either a screeplot or a specific plot for a
given criterion. If an object of class \code{"ICS_crit"} is given, then the
selected components are shaded on the plot.
}
\examples{
X <- iris[,-5]
out <- ICS(X)

# on an ICS object
select_plot(out)
select_plot(out, type = "lines")

# on an ICS_crit object 
# median criterion
out_med <- med_crit(out, nb_select = 1, select_only = FALSE)
select_plot(out_med, type = "lines")
select_plot(out_med, screeplot = FALSE, type = "lines", 
color = "lightblue")

# discriminatory criterion
out_disc <- discriminatory_crit(out, clusters = iris[,5], 
 select_only = FALSE)
select_plot(out_disc)


}
\author{
Andreas Alfons and Aurore Archimbaud
}
