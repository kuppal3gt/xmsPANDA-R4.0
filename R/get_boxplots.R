get_boxplots <-
function(X=NA,Y=NA,feature_table_file,parentoutput_dir,class_labels_file,
                       boxplot.col.opt=c("journal", "npg", "nejm", "jco", "lancet", "custom1", "brewer.RdYlBu", "brewer.RdBu", "brewer.PuOr", 
                                         "brewer.PRGn", "brewer.PiYG", "brewer.BrBG", "brewer.Set2", "brewer.Paired", "brewer.Dark2", "brewer.YlGnBu", "brewer.YlGn",
                                         "brewer.YlOrRd", "brewer.YlOrBr", "brewer.PuBuGn", "brewer.PuRd", "brewer.PuBu", "brewer.OrRd", "brewer.GnBu", "brewer.BuPu",
                                         "brewer.BuGn", "brewer.blues", "black", "grey65", "terrain", "rainbow", "heat", "topo"),
                       alphacol=1,newdevice=TRUE,cex.plots=0.4,replace.by.NA=FALSE,pairedanalysis=FALSE,
                       filename="",ylabel="Intensity",xlabel=NA,alphabetical.order=FALSE,name=NA,
                       add.jitter=FALSE,add.pvalues=FALSE,class.levels=NA,fill.plots=TRUE,
                       connectpairedsamples=FALSE,boxplot.type="ggplot",
                       study.design=c("multiclass","onewayanova","twowayanova","onewayanovarepeat",
                                      "twowayanovarepeat"),
                       multiple.figures.perpanel=FALSE,ggplot.type1=TRUE,replace.outliers=FALSE,
                       plot.height=8,plot.width=8,
                       extra_text=NA,group_by_mat=NA,position_dodge_width=0.75,
                       numnodes=2,hightlight.points=FALSE,ref.group.val=FALSE,facet.nrow=1,facet.ncol=NULL,
                       ylim.val=NA,remove.xaxis.labels=FALSE,boxplot.lwd=1,axis.ticks.size=1.5,axis.line.size=2,jitter.size=3,boxplot.strip.position ="top",legend.all.pages=TRUE,
                       pval.reporting.method="none",replace.by.median=FALSE,pval.table.report=FALSE,subset.factor1=NA,subset.factor2=NA,
                       asterisk.size=6,posthoc.multcomp.test="bonferroni",...)
{
  
  parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
  match_col.opt=match(boxplot.col.opt,c("journal","npg","nejm","jco","lancet","custom1","brewer.RdYlBu","brewer.RdBu","brewer.PuOr","brewer.PRGn","brewer.PiYG","brewer.BrBG",
                                       "brewer.Set2","brewer.Paired","brewer.Dark2","brewer.YlGnBu","brewer.YlGn","brewer.YlOrRd","brewer.YlOrBr","brewer.PuBuGn",
                                       "brewer.PuRd","brewer.PuBu",
                                       "brewer.OrRd","brewer.GnBu","brewer.BuPu","brewer.BuGn","brewer.blues","black","grey65","terrain","rainbow","heat","topo"))
  
  match_col.opt=length(which(is.na(match_col.opt)==TRUE))
  
  if(pval.reporting.method=="none"){
    pval.table.report=FALSE
  }
  
  if(length(match_col.opt)<1){
    
    boxplot.col.opt=boxplot.col.opt[1]
  }else{
    
    if(length(grep(boxplot.col.opt,pattern="brewer."))>1){
      
      boxplot.col.opt=boxplot.col.opt[1]
    }
  }
  
  #color.palette=get_hexcolors_for_palettes(color.palette=color.palette,alpha.col=alpha.col[1])
  
  if(length(boxplot.col.opt)==1){
    boxplot.col.opt=tolower(boxplot.col.opt)
    boxplot.col.opt=get_hexcolors_for_palettes(color.palette=boxplot.col.opt[1],alpha.col=alphacol[1])
  }
#suppressWarnings(suppressMessages(
  res<-get_boxplots_child(X=X,Y=Y,feature_table_file=feature_table_file,parentoutput_dir=parentoutput_dir,class_labels_file=class_labels_file,boxplot.col.opt,
                          alphacol=alphacol,newdevice=newdevice,cex.plots=cex.plots,
                     replace.by.NA=replace.by.NA,pairedanalysis=pairedanalysis,filename=filename,ylabel=ylabel,xlabel=xlabel,
                     alphabetical.order=alphabetical.order,name=name,add.jitter=add.jitter,add.pvalues=add.pvalues,class.levels=class.levels,fill.plots=fill.plots,
                     connectpairedsamples=connectpairedsamples,boxplot.type=boxplot.type,study.design=study.design,
                     multiple.figures.perpanel=multiple.figures.perpanel,ggplot.type1=ggplot.type1,
                     replace.outliers=replace.outliers,plot.height=plot.height,
                     plot.width=plot.width,extra_text=extra_text,group_by_mat=group_by_mat,
                     position_dodge_width=position_dodge_width,numnodes=numnodes,
                     hightlight.points=hightlight.points,ref.group.val=ref.group.val,facet.nrow=facet.nrow,facet.ncol=facet.ncol,ylim.val=ylim.val,
                     remove.xaxis.labels=remove.xaxis.labels,boxplot.lwd=boxplot.lwd,axis.ticks.size=axis.ticks.size,
                     axis.line.size=axis.line.size,jitter.size=jitter.size, boxplot.strip.position = boxplot.strip.position,
                     legend.all.pages=legend.all.pages,pval.reporting.method=pval.reporting.method,replace.by.median=replace.by.median,pval.table.report=pval.table.report,
                     subset.factor1=subset.factor1,subset.factor2=subset.factor2,asterisk.size=asterisk.size,posthoc.multcomp.test=posthoc.multcomp.test,...)
                     #))
  
  if(newdevice==TRUE){
    
    try(dev.off(),silent=TRUE)
  }
  
  try(unlink("Rplots.pdf"),silent=TRUE)
  return(res)
}
