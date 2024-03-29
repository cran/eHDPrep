% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/semantic_enrich.R
\name{metavariable_info}
\alias{metavariable_info}
\title{Compute Metavariable Information}
\usage{
metavariable_info(graph, mode = "in", IC_threshold = 0)
}
\arguments{
\item{graph}{Graph containing ontological and dataset nodes. Must be in
\code{\link[tidygraph:tidygraph]{tidygraph}} format or coercible to this format.}

\item{mode}{Character constant specifying the directionality of the edges.
One of: "in" or "out".}

\item{IC_threshold}{Metavariables with IC less than this value will be
omitted from output. Default = 0 (no omission).}
}
\value{
A modified graph object with additional node attributes pertaining to their
  status as a metavariable.
}
\description{
Calculates attributes for each node in a graph object pertaining to their
suitability and rank as metavariables; primarily if they are the most
informative common ancestor (see \code{\link{node_IC_zhou}}) of a set of
nodes representing a dataset variable.
}
\details{
The added attributes are:
\describe{\item{min_dist_to_var}{Integer. The minimum distance of an ontology
node in the graph to a node representing a dataset variable.}
\item{is_metavariable}{Logical. If the node has at least two descendants in
the graph which represent dataset variables.}
\item{variable_descendants}{List. The names of variables of which a node is
an ancestor.}
\item{variable_set}{Integer. An identifier for the unique set of descendants
in the graph which represent dataset variables. The assigned
number corresponds to the order in which a unique set was identified when
scanning through the node table.}
\item{highest_IC}{Logical. If the node possesses the highest information
content of all other nodes which are common ancestors of the same variable
set. Information content is expected to have been calculated in
\code{\link{join_vars_to_ontol}}.}
}
}
\examples{
data(example_ontology)
require(magrittr)
example_ontology \%>\%
join_vars_to_ontol(example_mapping_file, root = "root") -> joined_ontol

metavariable_info(joined_ontol)
}
\seealso{
\code{\link{node_IC_zhou}}

Other semantic enrichment functions: 
\code{\link{join_vars_to_ontol}()},
\code{\link{metavariable_agg}()},
\code{\link{metavariable_variable_descendants}()}
}
\concept{semantic enrichment functions}
