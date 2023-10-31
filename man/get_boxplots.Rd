\name{get_boxplots}
\alias{get_boxplots}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{get_boxplots}
\description{
This function generates boxplots for m/z features. The input intensity matrix could be transformed
or non-transformed intensities. Sample labels in the class labels file should be
in the same order as the intensity matrix or feature table.
}
\usage{
get_boxplots(X = NA, Y = NA, feature_table_file, parentoutput_dir,
                 class_labels_file, boxplot.col.opt = c("journal",
                 "npg", "nejm", "jco", "lancet", "custom1",
                 "brewer.RdYlBu", "brewer.RdBu", "brewer.PuOr",
                 "brewer.PRGn", "brewer.PiYG", "brewer.BrBG",
                 "brewer.Set2", "brewer.Paired", "brewer.Dark2",
                 "brewer.YlGnBu", "brewer.YlGn", "brewer.YlOrRd",
                 "brewer.YlOrBr", "brewer.PuBuGn", "brewer.PuRd",
                 "brewer.PuBu", "brewer.OrRd", "brewer.GnBu",
                 "brewer.BuPu", "brewer.BuGn", "brewer.blues", "black",
                 "grey65", "terrain", "rainbow", "heat", "topo"),
                 alphacol = 1, newdevice = TRUE, cex.plots = 0.8,
                 replace.by.NA = FALSE, pairedanalysis = FALSE,
                 filename = "", ylabel = "Intensity", xlabel = NA,
                 alphabetical.order = FALSE, name = NA, add.jitter =
                 FALSE, add.pvalues = FALSE, class.levels = NA,
                 fill.plots = TRUE, connectpairedsamples = FALSE,
                 boxplot.type = "ggplot", study.design =
                 c("multiclass", "onewayanova", "twowayanova",
                 "onewayanovarepeat", "twowayanovarepeat"),
                 multiple.figures.perpanel = FALSE, ggplot.type1 =
                 TRUE, replace.outliers = FALSE, plot.height = 8,
                 plot.width = 8, extra_text = NA, group_by_mat = NA,
                 position_dodge_width = 0.75, numnodes = 2,
                 hightlight.points = FALSE, ref.group.val = FALSE,
                 facet.nrow = 1, facet.ncol = NULL, ylim.val = NA,
                 remove.xaxis.labels = FALSE, boxplot.lwd = 1,
                 axis.ticks.size = 1.5, axis.line.size = 2, jitter.size
                 = 3, boxplot.strip.position = "top", legend.all.pages
                 = TRUE, pval.reporting.method = "none",
                 replace.by.median = FALSE, pval.table.report = FALSE,
                 subset.factor1 = NA, subset.factor2 = NA,
                 asterisk.size = 6, posthoc.multcomp.test =
                 "bonferroni", ...)
}
\arguments{
    \item{feature_table_file}{
Feature table that includes the mz, retention time, and measured intensity in each sample 
for each analyte. The first 2 columns should be the mz and time. The remaining columns
should correspond to the samples in the class labels file with each column including the intensity profile
of a sample. 
Full path required. Eg: C:/My Documents/test.txt
The feature table should be in a tab-delimited format. An example of the input file is provided under the
"example" folder.
}
  \item{parentoutput_dir}{
Provide full path of the folder where you want the results to be written.
Eg: C:/My Documents/ProjectA/results/
}
  \item{class_labels_file}{
File with class labels information for each sample. Samples should be in the same order
as in the feature table. Please use the same format as in the example folder.
}
\item{sample.col.opt}{
Color scheme for PCA and heatmap sample axis 
eg: "heat" or "topo"
}
\item{alphacol=0.3}{
Color scaling parameter
eg:0.3
}
}
\value{
Creates a PDF with boxplots for each m/z feature.
}
\author{Karan Uppal <kuppal2@emory.edu>}