# Seek out feedback arc sets in my org-roam graph


# NOTE: for "exact_ip", recompile igraph with GLPK support. First:
#     apt install build-essential gfortran libglpk40 libglpk-dev libxml2-dev
# https://r.igraph.org/articles/installation-troubleshooting#cannot-compile-igraph-from-sources-on-linux
install.packages("igraph")

library(tidyverse)
library(conflicted)
library(igraph)

# this file was produced from an elisp function I have
g <- graph_from_data_frame(
  read_tsv("/tmp/org-roam-digraph.tsv"),
  directed = TRUE
)

# so many... too greedy algo?
fas1 <- feedback_arc_set(g, algo = "approx_eades")
fas1

# incomputable... https://lists.gnu.org/archive/html/help-glpk/2013-12/msg00007.html
fas2 <- feedback_arc_set(g, algo = "exact_ip")
fas2


lisp_data <- str_c("(\"", as_ids(fas1), "\")") |>
  str_replace("\\|", "\" . \"") |>
  str_flatten("\n ") |>
  (function(x) {
    str_c("(", x, ")")
  })()
# (\(x) str_c("(", x, ")"))() |>


write_file(lisp_data, "/tmp/feedback_arcs.el")
