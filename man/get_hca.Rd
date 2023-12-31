\name{get_hca}
\alias{get_hca}
\title{get_hca}
\description{
This function performs two-way hierarchical clustering analysis and generates a heatmap showing
the clustering results. The input intensity matrix could be transformed
or non-transformed intensities. Sample labels in the class labels file should be
in the same order as the intensity matrix or feature table.
}
\usage{
get_hca(feature_table_file = NA, parentoutput_dir,
                 class_labels_file = NA, X = NA, Y = NA,
                 heatmap.col.opt = "RdBu", cor.method = "spearman",
                 is.data.znorm = FALSE, analysismode =
                 "classification", sample.col.opt = c("journal", "npg",
                 "nejm", "jco", "lancet", "custom1", "brewer.RdYlBu",
                 "brewer.RdBu", "brewer.PuOr", "brewer.PRGn",
                 "brewer.PiYG", "brewer.BrBG", "brewer.Set2",
                 "brewer.Paired", "brewer.Dark2", "brewer.YlGnBu",
                 "brewer.YlGn", "brewer.YlOrRd", "brewer.YlOrBr",
                 "brewer.PuBuGn", "brewer.PuRd", "brewer.PuBu",
                 "brewer.OrRd", "brewer.GnBu", "brewer.BuPu",
                 "brewer.BuGn", "brewer.blues", "black", "grey65",
                 "terrain", "rainbow", "heat", "topo"), plots.width =
                 8, plots.height = 8, plots.res = 600, plots.type =
                 "cairo", alphacol = 1, hca_type = "two-way", newdevice
                 = FALSE, input.type = "intensity", mainlab = "",
                 cexRow = 0.5, cexCol = 0.5, plot.bycluster = FALSE,
                 color.rows = TRUE, similarity.matrix = "correlation",
                 deepsplit = 4, minclustsize = 2, mergeCutHeight =
                 0.05, num_nodes = 2, alphabetical.order = FALSE,
                 pairedanalysis = FALSE, cutree.method = c("dynamic",
                 "default"), study.design = c("multiclass",
                 "onewayanova", "twowayanova", "onewayanovarepeat",
                 "twowayanovarepeat"), labRow.value = FALSE,
                 labCol.value = FALSE, power_val = 6, row.col.opt =
                 "journal", show.silhouette = FALSE, cexLegend = 0.7,
                 ylab_text = "", xlab_text = "", heatmap_file =
                 "heatmap")
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
\item{heatmap.col.opt}{
Color scheme for HCA hetmap eg: "RdBu", "topo", "heat", or "terrain"
}
\item{cor.method}{
Correlation method. Options: "person" or "spearman". Default: "spearman"
}
  \item{is.data.znorm}{
Is the data already auto-scaled or z-scaled? eg: TRUE or FALSE
}
\item{analysismode}{
"classification" for group-wise comparison (case vs control) or
"regression" for continuous response variables. Default: "classification"
}
\item{sample.col.opt}{
Color scheme for PCA and heatmap sample axis 
eg: "heat" or "topo" or "rainbow"
}
  \item{plots.width}{
Width of the tiff file. eg: 2000
}
  \item{plots.height}{
Height of the tiff file. eg: 2000
}
  \item{plots.res}{
Resolution of the tiff file. eg: 300
}
\item{alphacol}{
Color scaling parameter
eg:0.3
}
\item{hcatype}{
Color scaling parameter
eg:"two-way" or "one-way"
}
\item{similarity.matrix}{
Correlation or topological overlap matrix 
eg:"correlation" or "TOM"
}
}
\value{
Heatmap from Two-way hierarchical clustering analysis;
Intensity matrix in the same order as the dendrograms in
heatmap; 
Sample cluster labels
}
\author{Karan Uppal <kuppal2@emory.edu>}