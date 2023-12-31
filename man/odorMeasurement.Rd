% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/odorMeasurement.R
\name{odorMeasurement}
\alias{odorMeasurement}
\title{R6 Class representing a measurement}
\description{
R6 Class representing a measurement

R6 Class representing a measurement
}
\details{
R6 Class representing a measurement

For details ...
}
\examples{

rnorm(100)

}
\author{
Kornelius Rohmeyer \email{rohmeyer@small-projects.de}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{meta_data}}{meta data}

\item{\code{data_wide}}{data in wide format}

\item{\code{data_long}}{data in long format}

\item{\code{baseline_data}}{baseline data as given in the header. Perhaps not that useful, see the discussion about falling response values.}

\item{\code{change}}{...}

\item{\code{name}}{...}

\item{\code{start}}{...}

\item{\code{stop}}{...}

\item{\code{start_probe}}{...}

\item{\code{stop_probe}}{...}

\item{\code{base_line_models}}{...}

\item{\code{corrected_data_long}}{...}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-odorMeasurement-new}{\code{odorMeasurement$new()}}
\item \href{#method-odorMeasurement-model_baseline}{\code{odorMeasurement$model_baseline()}}
\item \href{#method-odorMeasurement-correct_for_baseline}{\code{odorMeasurement$correct_for_baseline()}}
\item \href{#method-odorMeasurement-calculate_response}{\code{odorMeasurement$calculate_response()}}
\item \href{#method-odorMeasurement-calculate_features}{\code{odorMeasurement$calculate_features()}}
\item \href{#method-odorMeasurement-clone}{\code{odorMeasurement$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-odorMeasurement-new"></a>}}
\if{latex}{\out{\hypertarget{method-odorMeasurement-new}{}}}
\subsection{Method \code{new()}}{
Create a eNoseMeasurement object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{odorMeasurement$new(
  meta_data = NULL,
  data_wide = NULL,
  data_long = NULL,
  baseline_data = NULL,
  name = NULL,
  start = NULL,
  stop = NULL,
  start_probe = NULL,
  stop_probe = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{meta_data}}{meta data}

\item{\code{data_wide}}{data in wide format}

\item{\code{data_long}}{data in long format}

\item{\code{baseline_data}}{baseline data as given in the header. Perhaps not that useful, see the discussion about falling response values.}

\item{\code{name}}{...}

\item{\code{start}}{...}

\item{\code{stop}}{...}

\item{\code{start_probe}}{...}

\item{\code{stop_probe}}{...}

\item{\code{change}}{...}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new `eNoseMeasurement` object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-odorMeasurement-model_baseline"></a>}}
\if{latex}{\out{\hypertarget{method-odorMeasurement-model_baseline}{}}}
\subsection{Method \code{model_baseline()}}{
Model the baseline (default: as a polynomial of degree 2)
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{odorMeasurement$model_baseline(
  polynomial_degree = 2,
  use_data_after_measurement_for_baseline = TRUE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{polynomial_degree}}{Degree of polynomial to fit. Default is 2.}

\item{\code{use_data_after_measurement_for_baseline}}{Should the data after the exposure to the odor also be used?
Default is TRUE to reduce extrapolation errors. If there are still major effects from the odor after the measurement, it could make sense to set this to FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-odorMeasurement-correct_for_baseline"></a>}}
\if{latex}{\out{\hypertarget{method-odorMeasurement-correct_for_baseline}{}}}
\subsection{Method \code{correct_for_baseline()}}{
Correct the data by subtracting the baseline.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{odorMeasurement$correct_for_baseline()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-odorMeasurement-calculate_response"></a>}}
\if{latex}{\out{\hypertarget{method-odorMeasurement-calculate_response}{}}}
\subsection{Method \code{calculate_response()}}{
...
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{odorMeasurement$calculate_response()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-odorMeasurement-calculate_features"></a>}}
\if{latex}{\out{\hypertarget{method-odorMeasurement-calculate_features}{}}}
\subsection{Method \code{calculate_features()}}{
...
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{odorMeasurement$calculate_features()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-odorMeasurement-clone"></a>}}
\if{latex}{\out{\hypertarget{method-odorMeasurement-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{odorMeasurement$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
