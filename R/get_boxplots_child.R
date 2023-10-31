get_boxplots_child <-
function(X,Y,feature_table_file,parentoutput_dir,class_labels_file,boxplot.col.opt="journal",alphacol=0.3,newdevice=TRUE,cex.plots=0.8,replace.by.NA=FALSE,
                             pairedanalysis=FALSE,filename="",ylabel="Intensity",xlabel=NA,
                             alphabetical.order=FALSE,name=NA,add.jitter=TRUE,add.pvalues=TRUE,class.levels=NA,fill.plots=FALSE,connectpairedsamples=FALSE,
                             boxplot.type="ggplot",
                             study.design=c("multiclass","onewayanova","twowayanova","onewayanovarepeat",
                                                                  "twowayanovarepeat"),
                             multiple.figures.perpanel=TRUE,
                             ggplot.type1=TRUE,replace.outliers=FALSE,plot.height=8,plot.width=8,
                             extra_text=NA,group_by_mat=NA,position_dodge_width=0.75,
                             numnodes=2,hightlight.points=FALSE,ref.group.val=FALSE,facet.nrow=NULL,facet.ncol=NULL,
                             ylim.val=NA,remove.xaxis.labels=FALSE,boxplot.lwd=1,axis.ticks.size=1.5,axis.line.size=2,jitter.size=2,boxplot.strip.position = "top",legend.all.pages=TRUE,
                             pval.reporting.method="anova",replace.by.median=FALSE,pval.table.report=FALSE,subset.factor1=NA,subset.factor2=NA,asterisk.size=6,posthoc.multcomp.test="bonferroni",...)
{
  options(warn=-1)
  analysistype=study.design[1]
  
  paireddesign=NA
  
  if(boxplot.type=="ggplot"){
    suppressMessages(library(ggplot2))
  }else{
    suppressMessages(library(ggpubr))
  }
  
 # multiple.figures.perpanel=TRUE
  
  if(typeof(X)=="logical"){
    data_matrix<-read.table(feature_table_file,sep="\t",header=TRUE,stringsAsFactors=FALSE,check.names=FALSE)
  }else{
    X<-as.data.frame(X)
    
    data_matrix<-X
  }
  if(typeof(Y)=="logical"){
    classlabels<-read.table(class_labels_file,sep="\t",header=TRUE)
  }else{
    Y<-as.data.frame((Y))
    classlabels<-Y
  }
  rm(X)
  rm(Y)
  cnames<-colnames(data_matrix)
  cnames<-tolower(cnames)
  
  check_names<-grep(cnames,pattern="^name$")
  names_with_mz_time<-{}
  
  dir.create(parentoutput_dir,showWarnings = FALSE)
  setwd(parentoutput_dir)
 
  if(length(check_names)>0){
    
    #Name is present
    if(check_names==1){
      
      check_names1<-grep(cnames,pattern="^mz$")
      check_names2<-grep(cnames,pattern="^time$")
      
      
      if(length(check_names1)<1 & length(check_names2)<1){
        mz<-seq(1.00001,nrow(data_matrix)+1,1)
        time<-seq(1.01,nrow(data_matrix)+1,1.00)
        check_ind<-gregexpr(cnames,pattern="^name$")
        check_ind<-which(check_ind>0)
        data_matrix<-as.data.frame(data_matrix)
        
        Name<-as.character(data_matrix[,check_ind])
        
        data_matrix<-cbind(mz,time,data_matrix[,-check_ind])
        names_with_mz_time=cbind(Name,mz,time)
        
        names_with_mz_time<-as.data.frame(names_with_mz_time)
        data_matrix<-as.data.frame(data_matrix)
        
        #write.table(names_with_mz_time,file="Stage1/Name_mz_time_mapping.txt",sep="\t",row.names=FALSE)
        
        write.table(names_with_mz_time,file="Name_mz_time_mapping.txt",sep="\t",row.names=FALSE)
        
        
      }else{
        
        if(length(check_names1)>0 & length(check_names2)>0){
          
          check_ind<-gregexpr(cnames,pattern="^name$")
          check_ind<-which(check_ind>0)
          Name<-as.character(data_matrix[,check_ind])
          data_matrix<-data_matrix[,-check_ind]
          names_with_mz_time=cbind(Name,data_matrix$mz,data_matrix$time)
          colnames(names_with_mz_time)<-c("Name","mz","time")
          names_with_mz_time<-as.data.frame(names_with_mz_time)
          data_matrix<-as.data.frame(data_matrix)
          write.table(names_with_mz_time,file="Name_mz_time_mapping.txt",sep="\t",row.names=FALSE)
        }
      }
      
    }
  }else{
    
    
    check_names1<-grep(cnames[1],pattern="^mz$")
    check_names2<-grep(cnames[2],pattern="^time$")
    if(length(check_names1)<1 || length(check_names2)<1){
      stop("Invalid feature table format. The format should be either Name in column A or mz and time in columns A and B. Please check example files.")
    }else{
      
      names_with_mz_time<-cbind(paste(data_matrix$mz,data_matrix$time,sep="_"),data_matrix$mz,data_matrix$time)
    }
  }
  
  
  

  
  data_m<-data_matrix[,-c(1:2)]
  
  data_m<-as.matrix(data_m)
  
  mzvec<-data_matrix[,1]
  timevec<-data_matrix[,2]
  goodfeats<-data_m			
  rm(data_m)
  
  #library(extrafont)
  #loadfonts()
  
  multiple.groups=FALSE
  
  #c("multiclass","onewayanova","twowayanova","onewayanovarepeat","twowayanovarepeat")
  
  if(study.design=="onewayanovarepeat" | study.design=="twowayanovarepeat" | study.design=="twowayrepeat" | study.design=="onewayrepeat"){
    
    pairedanalysis=TRUE
  }
  
 # print(head(classlabels))
  
  if(pairedanalysis==TRUE){
    
    print("Using column 2 as subject identifiers")
    paireddesign=classlabels[,c(1,2)]
    
    classlabels<-classlabels[,-c(2)]
  }
  
  pairedanalysis=FALSE
  paireddesign=NA
  
  
  if(dim(classlabels)[2]>2){
    
    
    if(study.design=="twowayanova" | study.design=="twowayanovarepeat" | study.design=="twoway" | study.design=="twowayrepeat"){
      # print("More than two columns found in the class labels file. ")
      
      
      if(alphabetical.order==FALSE){
        classlabels[,2] <- factor(classlabels[,2], levels=unique(classlabels[,2]))
        
        classlabels[,3] <- factor(classlabels[,3], levels=unique(classlabels[,3]))
      }
      
      Class<-paste(classlabels[,2],":",classlabels[,3],sep="") #classlabels[,2]:classlabels[,3]
      
      multiple.groups=TRUE
      
    }else{
      
      if(study.design=="onewayanova" | study.design=="onewayanovarepeat" | study.design=="oneway"){
        #     print("More than two columns found in the class labels file. ")
        
        if(alphabetical.order==FALSE){
          classlabels[,2] <- factor(classlabels[,2], levels=unique(classlabels[,2]))
          
          
        }
        
        Class<-classlabels[,2] #classlabels[,2]:classlabels[,3]
        
        
      }else{
        
        if(alphabetical.order==FALSE){
          classlabels[,2] <- factor(classlabels[,2], levels=unique(classlabels[,2]))
          
          
        }
        Class<-classlabels[,2]
        
      }
      
    }
    
  }else{
    if(alphabetical.order==FALSE){
      classlabels[,2] <- factor(classlabels[,2], levels=unique(classlabels[,2]))
      
    }
    Class<-classlabels[,2]
  }
  
  
 # par(mfrow=c(2,2),family="sans",cex=cex.plots)
  
  if(alphabetical.order==FALSE){
    Class <- factor(Class, levels=unique(Class))
  }
#  save(Class,classlabels,file="Class.Rda")
  
  class_levels<-levels(as.factor(Class))
  
  class_labels_levels<-levels(as.factor(Class))
  ordered_labels<-Class
  
  class_label_alphabets<-paste("C",1:length(class_labels_levels),sep="") #c("A","B","C","D","E","F","G","H","I","J","K","L","M")
  
  if(is.na(boxplot.col.opt)==TRUE){
    
    col_vec<-rep(c("white"),length(class_labels_levels))
    boxplot.col.opt<-col_vec
  }
  
  #save(boxplot.col.opt,class_labels_levels,file="coldebug.Rda")
  
  if(boxplot.col.opt=="default"){
    
    col_vec<-c("#CC0000","#AAC000","blue","mediumpurple4","mediumpurple1","blueviolet","cornflowerblue","cyan4","skyblue",
               "darkgreen", "seagreen1", "green","yellow","orange","pink", "coral1", "palevioletred2",
               "red","saddlebrown","brown","brown3","white","darkgray","aliceblue",
               "aquamarine","aquamarine3","bisque","burlywood1","lavender","khaki3","black")
    
  }else{ 
    if(boxplot.col.opt=="topo"){
      #col_vec<-topo.colors(256) #length(class_labels_levels)) 
      
      #col_vec<-col_vec[seq(1,length(col_vec),)]
      
      col_vec <- topo.colors(length(class_labels_levels), alpha=alphacol)
    }else{
      if(boxplot.col.opt=="heat"){
        #col_vec<-heat.colors(256) #length(class_labels_levels))
        
        col_vec <- heat.colors(length(class_labels_levels), alpha=alphacol)
      }else{
        if(boxplot.col.opt=="rainbow"){
          #col_vec<-heat.colors(256) #length(class_labels_levels))
          col_vec<-rainbow(length(class_labels_levels), start = 0, end = alphacol)
          
          #col_vec <- heat.colors(length(class_labels_levels), alpha=alphacol)
        }else{
          
          if(boxplot.col.opt=="terrain"){
            #col_vec<-heat.colors(256) #length(class_labels_levels))
            #col_vec<-rainbow(length(class_labels_levels), start = 0, end = alphacol)
            
            col_vec <- cm.colors(length(class_labels_levels), alpha=alphacol)
          }else{
            
            if(boxplot.col.opt=="colorblind"){
              #col_vec <-c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")
              # col_vec <- c("#0072B2", "#E69F00", "#009E73", "gold1", "#56B4E9", "#D55E00", "#CC79A7","black")
              
              if(length(class_labels_levels)<9){
                
                col_vec <- c("#0072B2", "#E69F00", "#009E73", "#56B4E9", "#D55E00", "#CC79A7", "#E64B35FF", "grey57")
                
              }else{
                
                #   col_vec<-colorRampPalette(brewer.pal(10, "RdBu"))(length(class_labels_levels))
                col_vec<-c("#0072B2", "#E69F00", "#009E73", "#56B4E9", "#D55E00", "#CC79A7","#E64B35B2", "#4DBBD5B2","#00A087B2","#3C5488B2","#F39B7FB2","#8491B4B2","#91D1C2B2","#DC0000B2","#7E6148B2",
                           "#374E55B2","#DF8F44B2","#00A1D5B2","#B24745B2","#79AF97B2","#6A6599B2","#80796BB2","#0073C2B2","#EFC000B2", "#868686B2","#CD534CB2","#7AA6DCB2","#003C67B2","grey57")
                
              }
              
              
            }else{
              
              check_brewer<-grep(pattern="brewer",x=boxplot.col.opt[1])
              
              if(length(check_brewer)>0){
                
                boxplot.col.opt=gsub(x=boxplot.col.opt,pattern="brewer.",replacement="")
                col_vec <- colorRampPalette(brewer.pal(10, boxplot.col.opt))(length(class_labels_levels))
                
              }else{
                
                if(boxplot.col.opt=="journal"){
                  
                  col_vec<-c("#0072B2", "#E69F00", "#009E73", "#56B4E9", "#D55E00", "#CC79A7","#E64B35FF","#3C5488FF","#F39B7FFF",
                             "#8491B4FF","#91D1C2FF","#DC0000FF","#B09C85FF","#5F559BFF",
                             "#808180FF","#20854EFF","#FFDC91FF","#B24745FF",
                             
                             "#374E55FF","#8F7700FF","#5050FFFF","#6BD76BFF",
                             "#E64B3519","#4DBBD519","#631879E5","grey75")
                  if(length(class_labels_levels)<8){
                    col_vec<-c("#0072B2", "#E69F00", "#009E73", "#56B4E9", "#D55E00", "#CC79A7","grey75")
                    
                    #col_vec2<-brewer.pal(n = 8, name = "Dark2")
                    
                  }else{
                    if(length(class_labels_levels)<=28){
                      # col_vec<-c("#0072B2", "#E69F00", "#009E73", "#56B4E9", "#D55E00", "#CC79A7", "grey75","#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666","#1B9E77", "#7570B3", "#E7298A", "#A6761D", "#666666", "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
                      
                      col_vec<-c("#0072B2", "#E69F00", "#009E73", "#56B4E9", "#D55E00", "#CC79A7","#E64B35FF","#3C5488FF","#F39B7FFF",
                                 "#8491B4FF","#91D1C2FF","#DC0000FF","#B09C85FF","#5F559BFF",
                                 "#808180FF","#20854EFF","#FFDC91FF","#B24745FF",
                                 
                                 "#374E55FF","#8F7700FF","#5050FFFF","#6BD76BFF", "#8BD76BFF",
                                 "#E64B3519","#9DBBD0FF","#631879E5","#666666","grey75")
                      
                    }else{
                      
                      colfunc <-colorRampPalette(c("#0072B2", "#E69F00", "#009E73", "#56B4E9", "#D55E00", "#CC79A7","grey75"));col_vec<-colfunc(length(class_labels_levels))
                      
                      col_vec<-col_vec[sample(col_vec)]
                      
                      
                    }
                  }
                  
                  
                  
                }else{
                  #col_vec <-boxplot.col.opt
                  #col_vec <- rep(col_vec,length(class_labels_levels))
                  
                 
                  
                  if(length(boxplot.col.opt)==1){
                    
                    col_vec <-rep(boxplot.col.opt,length(class_labels_levels))
                    
                  }else{
                    
                    #length(boxplot.col.opt)>=length(class_labels_levels)
                    
                    #if(length(boxplot.col.opt)>=length(class_labels_levels)){
                      
                      col_vec <-boxplot.col.opt
                      col_vec <- rep(col_vec,length(class_labels_levels))
                      
                      
                    #}else{
                     # colfunc <-colorRampPalette(boxplot.col.opt);col_vec<-colfunc(length(class_labels_levels))
                    #}
                    
                  }
                  
                }
                
              }
              
            }
          }
          
          
        }
        
      }
      
    }	
  }
  
  
  
  ordered_labels={}
  num_samps_group<-new("list")
  num_samps_group[[1]]<-0
  groupwiseindex<-new("list")
  groupwiseindex[[1]]<-0
  
  
  for(c in 1:length(class_labels_levels))
  {
    
    classlabels_index<-which(Class==class_labels_levels[c])
    #ordered_labels<-c(ordered_labels,as.character(classlabels[classlabels_index,2]))
    num_samps_group[[c]]<-length(classlabels_index)
    groupwiseindex[[c]]<-classlabels_index
  }
  
  sampleclass<-{}
  patientcolors<-{}
  
  if(length(mzvec)>4){
    max_per_row<-3
    
    
    par_rows<-ceiling(9/max_per_row)
    
  }else{
    max_per_row<-length(mzvec)
    par_rows<-1
  }
  
  
  #name=goodfeats_name,
  #class_labels_levels<-paste("x",seq(1,length(class_labels_levels)),sep="")
  
  file_ind<-0
  boxplots_fname<-paste(filename,".pdf",sep="")
  #tiff(boxplots_fname, width=plots.width,height=plots.height,res=plots.res, compression="lzw")
  
  #tiff(boxplots_fname, width=2000,height=3000,res=plots.res, compression="lzw")
  
  if(newdevice==TRUE){ # & boxplot.type=="simple"){
    pdf(boxplots_fname) #,width=plot.width,height=plot.height)
  }
  #par(mfrow=c(par_rows,max_per_row))
  ###save(goodfeats,class_labels_levels,file="debug1.Rda")
  
  
  #plot(0:10, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
  
  
  # text(5,9,"Description:",font=2,col="blue")
  
  #text(5, 8, "This PDF includes boxplots of individual variables for each group.\n The box represents the interquartile range,
   #    the whiskers represent the 1.5 +/- IQR range,\n and the bold horizontal line represents the median.
    #   \n\n\n Note: The panels are grouped by factor 1 (e.g. group)\n or factor 2 (e.g. timepoint).",cex=1.5,font=2)
  
    # text(5, 7, "The figures include: ")
  if(is.na(extra_text)==FALSE){
    
    #plot(0:10, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
    
    
    # text(5,9,"Description:",font=2,col="blue")
    
    #text(5, 8, extra_text,cex=1.5,font=2)
    #text(extra_text)
  }
  
  temp_dm<-cbind(as.character(Class),(t(goodfeats)))
  temp_dm<-as.data.frame(temp_dm)
  colnames(temp_dm)<-c("Class",rownames(goodfeats))
  
  #keeps the class order same as in the input file; avoids arrangement by alphabetical order
  if(alphabetical.order==FALSE){
    temp_dm$Class <- factor(temp_dm$Class, levels=unique(temp_dm$Class))
    class_levels<-levels(as.factor(temp_dm$Class))
  }else{
    
    class_levels<-levels(as.factor(temp_dm$Class))
  }
  
  if(is.na(class.levels)==FALSE){
    
    match_test<-match(class.levels,levels(as.factor(temp_dm$Class)))
    if(length(which(is.na(match_test)==TRUE))<1){
      
      if(alphabetical.order==FALSE){
        temp_dm$Class <- factor(temp_dm$Class, levels=unique(class.levels))
      }
    }else{
      stop(paste("User defined class.levels ", paste(class.levels,sep=" ",collapse=""), " do not match the levels in the class labels matrix, ",paste(class_levels,sep=" ",collapse=""),sep=""))    
    }
  }
  
  #par(mfrow=c(1,1),family="sans",cex=cex.plots)
  theme_set(theme_gray(base_size = 25, base_family = 'sans' ))
  
  if(boxplot.type=="simple"){
    
    #lapply(1:dim(goodfeats)[1],function(m)
    for(m in 1:dim(goodfeats)[1])
    {
      
      if(m%%9==0){
        
        file_ind<-file_ind+1
        boxplots_fname<-paste("boxplots_file",file_ind,".tiff",sep="")
        
      }
      
      round_mzval<-mzvec[m] #sprintf("%.4f",mzvec[m])
      
      round_timeval<-timevec[m] #sprintf("%.1f",timevec[m])
      
      if(is.na(name[1])==TRUE){
        
        if(length(check_names)>0){
          if(check_names==1){
            
            mzname<-as.character(names_with_mz_time[m,1])
          }else{
            
            mzname<-paste("mz_time: ",round_mzval,"_",round_timeval,sep="")
          }
          
        }else{
          
          mzname<-paste("mz_time: ",round_mzval,"_",round_timeval,sep="")
        }
      }else{
        
        mzname=as.character(name[m])
      }
      
      t1<-table(sampleclass)
      cur_d<-new("list")
      feat_vec<-{}
      class_vec<-{}
      
      for(c in 1:length(class_labels_levels))
      {
        num_samps_group[[1]]<-t1[1]
        cvec<-as.vector(t(goodfeats[m,c(groupwiseindex[[c]])]))
        
        if(replace.outliers==TRUE){
           cvec<-replace_outliers(cvec,replace.by.NA,replace.by.median=replace.by.median)
        }
        cur_d[[c]]<-cvec
        feat_vec<-c(feat_vec,cvec)
        class_vec<-c(class_vec,rep(class_labels_levels[c],length(which(Class==class_labels_levels[c]))))
        
      }
      
      #w <- 0.1
      #        par(omd=c(0, 1-w, 0, 1),cex.main=0.7)
      
      #save(cur_d,class_labels_levels,goodfeats,groupwiseindex,file="cur_d.Rda")
      
      boxplot(cur_d,ylab=ylabel,main=mzname,xaxt="n",cex.main=0.7,col="white") #,ylim=range(pretty(c(0,max_yval))))
      
      for(i in 1:length(class_labels_levels)){
        axis(side=1,at=c(i),labels=class_labels_levels[i], col=col_vec[i],cex.axis=cex.plots,srt=90)
        
        
      }
      
      # (legend(par('usr')[2], par('usr')[4], bty='n', xpd=NA,class_labels_levels, col = col_vec[1:length(class_labels_levels)],pch = rep(19,length(col_vec[1:length(class_labels_levels)])), pt.cex = 0.6, title = "Class",cex=0.8))
      
    }
    #})
    
  }else{
    
    cl<-parallel::makeCluster(getOption("cl.cores", numnodes))
    clusterEvalQ(cl,library(ggplot2))
    clusterEvalQ(cl,library(rstatix))
    clusterEvalQ(cl,library(dplyr))
    clusterEvalQ(cl,library(gridExtra))
    clusterExport(cl,"replace_outliers")
    clusterExport(cl,"get_pval_boxplots")
    

    
    clusterExport(cl,"diffexplmtwowayanova")
    
  #  plot_res<-lapply(1:dim(goodfeats)[1],function(m,mzvec,timevec,check_names,name,class_labels_levels,sampleclass,col_vec,goodfeats,pairedanalysis,connectpairedsamples,boxplot.type,
   #                                                                                       ggplot.type1,group_by_mat,cex.plots,boxplot.col.opt,add.jitter,add.pvalues,fill.plots,multiple.figures.perpanel,ylim.val,pval.reporting.method)
                                                  
    plot_res<-parLapply(cl,1:dim(goodfeats)[1],function(m,mzvec,timevec,check_names,name,class_labels_levels,sampleclass,col_vec,goodfeats,pairedanalysis,connectpairedsamples,boxplot.type,
                                                       ggplot.type1,group_by_mat,cex.plots,boxplot.col.opt,add.jitter,add.pvalues,fill.plots,multiple.figures.perpanel,ylim.val,
                                                      pval.reporting.method,ref.group.val,subset.factor1,subset.factor2,asterisk.size,posthoc.multcomp.test)
      
    #  plot_res<-lapply(1:dim(goodfeats)[1],function(m,mzvec,timevec,check_names,name,class_labels_levels,sampleclass,col_vec,goodfeats,pairedanalysis,connectpairedsamples,boxplot.type,
     #                                                     ggplot.type1,group_by_mat,cex.plots,boxplot.col.opt,add.jitter,add.pvalues,fill.plots,multiple.figures.perpanel,ylim.val,
      #                                                    pval.reporting.method,ref.group.val,subset.factor1,subset.factor2,asterisk.size,posthoc.multcomp.test)
    {
      pval_res={}
      
      print(class_labels_levels)
      
      if(m%%9==0){
        
        file_ind<-file_ind+1
        boxplots_fname<-paste("boxplots_file",file_ind,".tiff",sep="")
        
      }
      
     round_mzval<-mzvec[m] #sprintf("%.4f",mzvec[m])
      
     round_timeval<-timevec[m] #sprintf("%.1f",timevec[m])
      
      if(is.na(name[1])==TRUE){            
        
        if(length(check_names)>0){	
          if(check_names==1){
            
            mzname<-as.character(names_with_mz_time[m,1])
          }else{  
            
            mzname<-paste("mz_time: ",round_mzval,"_",round_timeval,sep="")
          }
          
        }else{
          
          mzname<-paste("mz_time: ",round_mzval,"_",round_timeval,sep="")
        }
      }else{
        
        mzname=as.character(name[m])
      }
     
   #  save(class_labels_levels,goodfeats,groupwiseindex,file="test2.Rda")
     
      if(length(class_labels_levels)>=2)
      {
        if(length(class_labels_levels)>=1)
        {
          t1<-table(sampleclass)
          cur_d<-new("list")
          feat_vec<-{}
          class_vec<-{}
          sid_vec<-{}
          
         # print("DOING THIS2!!!")
          fname1<-paste(m,".Rda",sep="")
       # save(goodfeats,m,class_labels_levels,groupwiseindex,t1,num_samps_group,file=fname1)
           for(c in 1:length(class_labels_levels))
          {
            num_samps_group[[1]]<-t1[1]
            cvec<-as.vector(t(goodfeats[m,c(groupwiseindex[[c]])]))
            if(replace.outliers==TRUE){
              cvec<-as.numeric(as.character(cvec))
            cvec<-replace_outliers(cvec,replace.by.NA,replace.by.median=replace.by.median)
            }
            cur_d[[c]]<-cvec
            feat_vec<-c(feat_vec,cvec)
            class_vec<-c(class_vec,rep(class_labels_levels[c],length(which(Class==class_labels_levels[c]))))
            sid_vec<-c(sid_vec,names((goodfeats[m,c(groupwiseindex[[c]])])))  
          }
          
          
          temp_dm<-cbind(as.character(sid_vec),as.character(class_vec),as.vector(feat_vec))
          
          temp_dm2<-temp_dm #[,c(1,(m+1))]
          temp_dm2<-as.data.frame(temp_dm2)
          colnames(temp_dm2)<-c("SID","Class","Feature")
          temp_dm2$Feature<-as.numeric(as.character(temp_dm2$Feature))
          
          sum_yval1=summary(temp_dm2$Feature,na.rm=TRUE)
          max_yval1=max(temp_dm2$Feature,na.rm=TRUE)+(sum_yval1[5]-sum_yval1[2])
          
          if(alphabetical.order==FALSE){
            temp_dm2$Class <- factor(temp_dm2$Class, levels=unique(temp_dm2$Class))
          }
          
          fname1<-paste("temp_dm2",mzname,"A1.Rda")
          
         #save(temp_dm2,group_by_mat,file="temp_dm2.Rda")
       #   save(Class,class.levels,temp_dm2,alphabetical.order,file=fname1)
          Class<-temp_dm2
          
          if(is.na(class.levels)==FALSE){
            
            match_test<-match(class.levels,levels(as.factor(temp_dm2$Class)))
            if(length(which(is.na(match_test)==TRUE))<1){
              
              if(alphabetical.order==FALSE){
                temp_dm2$Class <- factor(temp_dm2$Class, levels=unique(class.levels))
              }
            }else{
              stop(paste("User defined class.levels ", paste(class.levels,sep=" ",collapse=""), " do not match the levels in the class labels matrix, ",paste(class_levels,sep=" ",collapse=""),sep=""))
            }
          }
          
          
          if(multiple.groups==TRUE){
            Factor1<-gsub(temp_dm2$Class,pattern=":([\\w|\\W])*",replacement="",perl=TRUE)
            Factor2<-gsub(temp_dm2$Class,pattern="([\\w|\\W])*:",replacement="",perl=TRUE)
            
       
            fname1<-paste("temp_dm2",mzname,"A.Rda")
            #save(Class,Factor1,Factor2,temp_dm2,file=fname1)
            temp_dm2<-cbind(temp_dm2,Factor1,Factor2)
            
            temp_dm2<-as.data.frame(temp_dm2)
            
            if(alphabetical.order==FALSE){
              temp_dm2$Factor1 <- factor(temp_dm2$Factor1, levels=unique(Factor1))
              
              temp_dm2$Factor2 <- factor(temp_dm2$Factor2, levels=unique(Factor2))
            }
            
          }
         #save(temp_dm2,group_by_mat,file="d1.Rda")
          if(is.na(group_by_mat)==FALSE){
          
            colnames(group_by_mat)<-c("SID","GroupBy")
            
              save(temp_dm2,group_by_mat,file="d1.Rda")
             temp_dm2<-merge(temp_dm2,group_by_mat,by="SID")
          }
          
          if(pairedanalysis==TRUE && connectpairedsamples==TRUE){
            
            
            colnames(paireddesign)<-c("SID","SubjectID")
            temp_dm2<-merge(temp_dm2,paireddesign,by="SID")
          }
          
          
          
          #  par(mfrow=c(2,2),family="sans",cex=cex.plots)
          
          w <- 0.1
          par(omd=c(0, 1-w, 0, 1),cex.main=0.7)
          
          #    print(cex)
          #save(temp_dm2,cur_d,mzname,boxplot.col.opt,col_vec,ylabel,Class,goodfeats,class_labels_levels,cex.plots,boxplot.type,multiple.groups,add.jitter,add.pvalues,fill.plots,file="temp_dm2.Rda")
          
          if(boxplot.type=="simple"){
            
            
            boxplot(cur_d,ylab=ylabel,main=mzname,xaxt="n",cex.main=0.7,col="white") #,ylim=range(pretty(c(0,max_yval))))
            
            for(i in 1:length(class_labels_levels)){
              axis(side=1,at=c(i),labels=class_labels_levels[i], col=col_vec[i],cex.axis=cex.plots,srt=45)
              
              
            }
            
            (legend(par('usr')[2], par('usr')[4], bty='n', xpd=NA,class_labels_levels, col = col_vec[1:length(class_labels_levels)],pch = rep(19,length(col_vec[1:length(class_labels_levels)])), pt.cex = 0.6, title = "Class",cex=0.8))
            
            
            #boxplot(cur_d)
          }else{
            
           # facet.nrow=1
            
            fname1<-paste("temp_dm2",mzname,".Rda")
        
            if(pairedanalysis==TRUE && connectpairedsamples==TRUE){    
                if(is.na(paireddesign)==FALSE){
                  
                  temp_dm2$SubjectID=as.numeric(as.factor(temp_dm2$SubjectID))
                }
            }
           #fname1<-paste()
           #save(temp_dm2,file="temp_dm2.Rda")
           
           if(is.na(subset.factor1)==FALSE){
             
             temp_dm2<-subset(temp_dm2,Factor1%in%c(subset.factor1))
           }
           
           if(is.na(subset.factor2)==FALSE){
             
             temp_dm2<-subset(temp_dm2,Factor2%in%c(subset.factor2))
           }
           
           
            #if(add.pvalues==FALSE && fill.plots==TRUE){
            if(TRUE){
              suppressMessages(library(ggplot2))
              
              
              if(multiple.groups==FALSE){
                #p <- ggplot(temp_dm2, aes(y=as.numeric(Feature),x=Class,fill=Class)) + labs(title=mzname)
                
                if(is.na(group_by_mat)==FALSE){
                  #if(ggplot.type1==TRUE){
                  p <- ggplot(temp_dm2, aes(y=as.numeric(Feature),x=Class,fill=Class)) +labs(title=mzname) + facet_wrap(~GroupBy, scale="free_x",nrow=facet.nrow,ncol=facet.ncol)  
                    
                  #}
                }else{
                  p <- ggplot(temp_dm2, aes(y=as.numeric(Feature),x=Class,fill=Class)) + labs(title=mzname)
                  
                }
                
        
                }else{
                            
                          if(is.na(group_by_mat)==TRUE){
                            #p <- ggplot(temp_dm2, aes(y=as.numeric(Feature),x=Factor1,fill=Factor2)) + labs(title=mzname) + facet_wrap(~Factor2, scale="free")  
                           
                          if(is.na(ggplot.type1)==FALSE){
                           if(ggplot.type1==TRUE){
                             
                             #print("DOing this")
                             
                          #  save(temp_dm2,mzname,facet.nrow,facet.ncol,file="debugbox.Rda")
                          
                              p <- ggplot(temp_dm2, aes(y=as.numeric(Feature),x=Factor1,fill=Factor1)) +labs(title=mzname) + facet_wrap(~Factor2, scale="free_x",nrow=facet.nrow,ncol=facet.ncol,strip.position = boxplot.strip.position)  
                            }else{
                              
                              p <- ggplot(temp_dm2, aes(y=as.numeric(Feature),x=Factor2,fill=Factor2)) +labs(title=mzname) + facet_wrap(~Factor1, scale="free_x",nrow=facet.nrow,ncol=facet.ncol,strip.position = boxplot.strip.position)  
                              
                              
                            }
                          }else{
                            p <- ggplot(temp_dm2, aes(y=as.numeric(Feature),x=Factor1,fill=Factor2)) +labs(title=mzname)
                          }
                            
                          }else{
                          #  save(temp_dm2,mzname,facet.nrow,facet.ncol,group_by_mat,file="debugbox.Rda")
                              if(is.na(ggplot.type1)==FALSE){
                              if(ggplot.type1==TRUE){
                                #fill=factor(GroupBy)#aes(color=factor(GroupBy)) geom_point(aes(group=SubjectID))+geom_line(aes(group=SubjectID))+
                              
                                #change: 1052022
                              #  p <- ggplot(temp_dm2, aes(y=as.numeric(Feature),x=Factor1,fill=GroupBy)) +labs(title=mzname) + facet_wrap(~Factor2, scale="free_x",nrow=facet.nrow,ncol=facet.ncol)  
                              
                                
                                  p <- ggplot(temp_dm2, aes(y=as.numeric(Feature),x=Factor1,fill=Factor1)) +labs(title=mzname) + facet_grid(GroupBy~Factor2, scale="free",switch="x") #,nrow=facet.nrow,ncol=facet.ncol)  
                                
                              }else{
                                 
                                p <- ggplot(temp_dm2, aes(y=as.numeric(Feature),x=Factor2,fill=GroupBy)) +labs(title=mzname) + facet_wrap(~Factor1, scale="free_x",nrow=facet.nrow,ncol=facet.ncol)  
                                
                              }
                              }else{
                                
                                p <- ggplot(temp_dm2, aes(y=as.numeric(Feature),x=Factor1,fill=GroupBy)) +labs(title=mzname)
                              }
                            
                         }
                
                
                }
            
            if(is.na(boxplot.col.opt)==FALSE){
              
              p=p+stat_boxplot(geom='errorbar',width=0.15)
              
              if(boxplot.col.opt=="white"){
                p<-p + geom_boxplot(lwd=boxplot.lwd)
              }else{
                geom_col_vec=(col_vec[1:length(class_labels_levels)])
                
                p<-p + geom_boxplot(alpha=alphacol,outlier.shape=NA,lwd=boxplot.lwd) #,colour=geom_col_vec)
                
              }
            }
              
              
           fname_c<-paste("d2",m,".Rda",sep="")
           #save(p,temp_dm2,file=fname_c)
              
              if(pairedanalysis==TRUE)
                {
                
                if(connectpairedsamples==TRUE){
                  
                  if(is.na(ggplot.type1)==FALSE){
                    if(ggplot.type1==TRUE){
                    p=p+geom_line(aes(Factor1, as.numeric(Feature),fill=factor(GroupBy),group=SubjectID),
                              position = position_dodge2(position_dodge_width))
                    }else{
                      p=p+geom_line(aes(Factor2, as.numeric(Feature),fill=factor(GroupBy),group=SubjectID),
                                    position = position_dodge2(position_dodge_width))
                      
                    }
                  }else{
                    p=p+geom_line(aes(Factor1, as.numeric(Feature),fill=factor(GroupBy),group=SubjectID),
                                  position = position_dodge2(position_dodge_width))
                    
                  }
                }
        
        #         p<-p+geom_line(aes(y  = as.numeric(Feature), x = Factor1)) #, group = SubjectID))
                  
                       if(add.jitter==TRUE){
                         
                         #p<-p+geom_jitter(aes(colour=factor(GroupBy)))
                         #position = position_jitterdodge(),
                        if(is.na(ggplot.type1)==FALSE){
                               if(ggplot.type1==TRUE){
                                          p=p+ geom_point(aes(Factor1, as.numeric(Feature),fill=GroupBy),
                                          shape=21, #factor(gsub(temp_dm2$SID,pattern="[a-z|A-Z|0-9]*_",replacement="")),
                                          position = position_dodge(position_dodge_width))
                               }else{
                                           p=p+ geom_point(aes(Factor2, as.numeric(Feature),fill=GroupBy),
                                                 shape=21, #factor(gsub(temp_dm2$SID,pattern="[a-z|A-Z|0-9]*_",replacement="")),
                                                 position = position_dodge(position_dodge_width))
                                 
                               }
                        }else{
                          p=p+ geom_point(aes(Factor1, as.numeric(Feature),fill=GroupBy),
                                          shape=21, #factor(gsub(temp_dm2$SID,pattern="[a-z|A-Z|0-9]*_",replacement="")),
                                          position = position_dodge(position_dodge_width))
                          
                        }
                         
                         if(highlight.points==TRUE){
                           #p=p+geom_point(data=subset(df.2, highlight),aes(x=variable, y=value), color="red", size=5)
                         }
                    
                      }
                #  p <- ggplot(temp_dm2, aes(y=as.numeric(Feature),x=Factor1,fill=Factor1)) +
              }
              else
                {
                  
                 # print("DOING THIS")
              if(add.jitter==TRUE){
                
                if(multiple.groups==FALSE){
                  
                      p=p+ geom_point(aes(Class, as.numeric(Feature),fill=Class),
                                      shape=21, #factor(gsub(temp_dm2$SID,pattern="[a-z|A-Z|0-9]*_",replacement="")),
                                      position = position_dodge(position_dodge_width),size=jitter.size)
                }else{
                #p<-p+geom_jitter()
                if(is.na(ggplot.type1)==FALSE){
                      if(ggplot.type1==TRUE){
                        p=p+ geom_point(aes(Factor1, as.numeric(Feature),fill=Factor1),
                                        shape=21, #factor(gsub(temp_dm2$SID,pattern="[a-z|A-Z|0-9]*_",replacement="")),
                                        position = position_dodge(position_dodge_width),size=jitter.size)
                      }else{
                        p=p+ geom_point(aes(Factor2, as.numeric(Feature),fill=Factor2),
                                        shape=21, #factor(gsub(temp_dm2$SID,pattern="[a-z|A-Z|0-9]*_",replacement="")),
                                        position = position_dodge(position_dodge_width),size=jitter.size)
                        
                      }
                }else{
                  p=p+ geom_point(aes(Factor1, as.numeric(Feature),fill=Factor1),
                                  shape=21, #factor(gsub(temp_dm2$SID,pattern="[a-z|A-Z|0-9]*_",replacement="")),
                                  position = position_dodge(position_dodge_width),size=jitter.size)
                  
                }
                
                }
              }
              
                }
              
              
              if(add.pvalues==TRUE){
                
                fname_pval<-paste(mzname,"pval.Rda",sep="")
               # save(temp_dm2,file=fname_pval)
                
                suppressMessages(library(ggpubr))
                var.equal.bool=TRUE
                pool.sd.bool=FALSE
                if(pval.reporting.method=="Student.t.test"){
                  
                  var.equal.bool=TRUE
                  pool.sd.bool=FALSE
                  pval.reporting.method="t.test"
                }else{
                  
                  if(pval.reporting.method=="Welch.t.test"){
                    
                    var.equal.bool=FALSE
                    pool.sd.bool=FALSE
                    pval.reporting.method="t.test"
                  }else{
                    
                    
                    if(pval.reporting.method=="t.test"){
                      
                      var.equal.bool=FALSE
                      pool.sd.bool=FALSE
                      pval.reporting.method="t.test"
                    }
                  }
                }
              
               # max_yval1=max_yval1*0.5
                if(multiple.groups==FALSE){
                  p<-p + stat_compare_means(data=temp_dm2,aes(group = Class),size = 5*cex.plots,label = "p.format",
                                            label.x = 0.5, label.y = max_yval1,size = 5*cex.plots,method=pval.reporting.method)
                }else{
                  
                
                 
                  if(pairedanalysis==FALSE){
                    
                  # save(temp_dm2,p,var.equal.bool,pool.sd.bool,ref.group=ref.group.val,file="temp_dm2_file.Rda")
                    pval_res<-try(get_pval_boxplots(temp_dm2,pval.reporting.method = pval.reporting.method,ref.group=ref.group.val,pool.sd.bool=pool.sd.bool,
                                                    var.equal.bool=var.equal.bool,p.adjust.method = posthoc.multcomp.test),silent=TRUE)
                    
                    #,p.adjust.method = posthoc.multcomp.test
                    
                    if(is(pval_res,"try-error"))
                    {
                      pval_res<-NA
                          
                    }
                    
                    print("done")
                   
                    #label = "p.format",
                    if(ggplot.type1==TRUE){
                      
                      if(is.na(ref.group.val)==TRUE){
                        ref.group.val<-unique(temp_dm2$Factor1)[1] 
                      }else{
                        if(ref.group.val==FALSE){
                          
                          p<-p + stat_compare_means(data=temp_dm2,aes(group = Factor1),label = "p.format",
                                                    size = 4*cex.plots,method=pval.reporting.method)
                        }else{
                          
                          if(pval.reporting.method=="wilcox.test"){
                          p<-p + stat_compare_means(data=temp_dm2,aes(group = Factor1),label = "p.signif",
                                                    size = asterisk.size*cex.plots,ref.group = ref.group.val,hide.ns=T,method="wilcox.test",
                                                    method.args=list("paired"=FALSE))
                          
                          #stat_compare_means(label = "p.signif", method = "wilcox.test", paired = F, hide.ns = T, show.legend = F, size = 8, label.y = 0.7)
                          
                          
                          }else{
                           # p<-p + stat_compare_means(data=temp_dm2,aes(group = Factor1),label = "p.format",
                            #                          size = 2*cex.plots,ref.group = ref.group.val)
                            if(pval.reporting.method=="kruskal.test"){
                              p<-p + stat_compare_means(data=temp_dm2,aes(group = Factor1),
                                                       size = asterisk.size*cex.plots,hide.ns=T,method="kruskal.test",label = "p.signif")
                        
                              
                            }else{
                              
                              if(pval.reporting.method=="t.test"){
                               
                                p<-p + stat_compare_means(data=temp_dm2,aes(group = Factor1),
                                                          size = asterisk.size*cex.plots,ref.group = ref.group.val,hide.ns=T,method="t.test",label = "p.signif",
                                                          method.args=list("var.equal"=var.equal.bool,"paired"=FALSE))
                                
                                
                              }
                              
                            }
                            
                            
                          }
                        }
                      }
                    }else{
                      if(is.na(ref.group.val)==TRUE){
                        ref.group.val<-unique(temp_dm2$Factor2)[1] 
                      }else{
                        if(ref.group.val==FALSE){
                          
                          p<-p + stat_compare_means(data=temp_dm2,aes(group = Factor1),label = "p.format",
                                                    size = 5*cex.plots,method=pval.reporting.method)
                        }else{
                          p<-p + stat_compare_means(data=temp_dm2,aes(group = Factor1),label = "p.format",
                                                    size = 5*cex.plots,ref.group = ref.group.val,method=pval.reporting.method)
                        }
                      }
                    p<-p + stat_compare_means(data=temp_dm2,aes(group = Factor2),label = "p.format",
                                              size = 5*cex.plots,ref.group = ref.group.val,method=pval.reporting.method)
                    }
                  }else{
                    
                    if(ggplot.type1==TRUE){
                    p<-p + stat_compare_means(data=temp_dm2,aes(group = Factor1),label = "p.format",
                                              size = 5*cex.plots,method=pval.reporting.method)
                    }else{
                    p<-p + stat_compare_means(data=temp_dm2,aes(group = Factor2),paired=TRUE,label = "p.format",
                                              size = 5*cex.plots,method=pval.reporting.method)
                    }
                  }
                }
              }
            #  p=p+geom_line(aes(group=SubjectID))
             # save(p,ylabel,cex.plots,col_vec,class_labels_levels,file="p.Rda")
              #,margin=margin(30,0,0,0)
              #,margin=margin(0,30,0,0)
              
              if(is.na(group_by_mat)==FALSE){
                
                p<-p+ labs(y=ylabel) + theme_bw() + theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=5), panel.spacing=unit(1.25,"lines"),
                  axis.line = element_line(colour = "black",size=1),
                  axis.text= element_text(size=12*cex.plots,family="sans",colour = "black"),
                  axis.title.x=element_text(size=13*cex.plots,face="bold",colour = "black"),
                  axis.title.y=element_text(size=13*cex.plots,face="bold",colour = "black"),
                  plot.title = element_text(hjust = 0.5,size=16*cex.plots),
                  
                  legend.background = element_rect(color = "black", fill = "white"),
                  strip.background=element_rect(colour="white",fill="white"),
                #  strip.text.x = element_text(size = 13*cex.plots, colour = "black"), 
                  #strip.text.y = element_text(size = 13*cex.plots, colour = "black"), 
                strip.text.x = element_text(size = 11*cex.plots, colour = "black"), 
                strip.text.y = element_text(size = 11*cex.plots, colour = "black",angle=0), 
                  strip.text = element_text(face="bold"),strip.placement = "outside") + scale_fill_manual(values=(col_vec[1:length(class_labels_levels)])) + scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
                
                
              }else{
              p<-p+ labs(y=ylabel) + theme_bw() + theme(panel.border = element_blank(), 
                                                        panel.grid.major = element_blank(),
                                                        panel.grid.minor = element_blank(),
                                                        panel.spacing=unit(1.25,"lines"),
                                                        axis.line = element_line(colour = "black",size=1),
                                                        axis.text= element_text(size=12*cex.plots,family="sans",colour = "black"),
                                                        axis.title.x=element_text(size=13*cex.plots,face="bold",colour = "black"),
                                                        axis.title.y=element_text(size=13*cex.plots,face="bold",colour = "black"),
                                                        plot.title = element_text(hjust = 0.5,size=16*cex.plots),
                                                        
                                                        legend.background = element_rect(color = "black", fill = "white"),
                                                        strip.background=element_rect(colour="white",fill="white"),
                                                        strip.text.x = element_text(size = 11*cex.plots, colour = "black"), 
                                                        strip.text.y = element_text(size = 11*cex.plots, colour = "black",angle=0), 
                                                       strip.text = element_text(face="bold"),strip.placement = "outside") + scale_fill_manual(values=(col_vec[1:length(class_labels_levels)])) + scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
              }
              p=p + theme(legend.text = element_text(size=10*cex.plots),legend.key.size = unit(3,"line"),legend.background = element_rect(color = NA),legend.title = element_blank()) #+ theme_set(theme_gray(base_size = 20, base_family = 'Font Name' ))
              
        #      
         
              if(remove.xaxis.labels==TRUE){
                
                p=p+theme(axis.text.x=element_blank(),
                      axis.ticks.x=element_blank())
              }
              
              
              p=p+theme(
                axis.ticks = element_line(size = axis.ticks.size, color="black") , 
                axis.ticks.length = unit(.5, "cm"), axis.line = element_line(size = axis.line.size, linetype=1))
              
              if(is.na(xlabel)==FALSE){
               
                  p=p+labs(x=xlabel)
              }
             
             # p=p+theme(axis.text.x = element_text(angle = 45, hjust = 1,margin = margin(t = 20)))
                    
            # p=p+theme(axis.title.x = element_text(vjust=-0.5)) #opts(axis.title.x = theme_text(vjust=-0.5))
              
              p=p+theme(legend.position = "none")
              
              p=p+scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
              
           #   if(facet.placement=="outside"){
           #     p=p+theme(strip.placement = "outside")
                
            #  }
              
              ##addhere
              fname1=paste("boxplot",mzname,".Rda")
             # save(p,col_vec,ylabel,class_labels_levels,cex.plots,file=fname1)
            #  ggpubr::ggexport(p,filename =fname1,width=unit(plot.width-0.5, "in"),
             #                  height=unit(plot.height-0.5, "in"),res=600)
              
            }
            else{
              
              suppressMessages(library(ggpubr))
              #if(boxplot.col.opt=="white" | boxplot.col.opt=="journal")
              {
                
                
                geom_col_vec=(col_vec[1:length(class_labels_levels)])
                
                if(boxplot.col.opt=="white" || length(unique(geom_col_vec))==1){
                  #geom_col_vec<-c("white")
                  
                  geom_col_vec<-NULL
                  
                  
                  if(multiple.groups==FALSE){
                    if(add.jitter==TRUE){
                      p<-ggboxplot(temp_dm2,x="Class",y="Feature",palette=geom_col_vec,add="jitter",title=mzname)
                    }else{
                      p<-ggboxplot(temp_dm2,x="Class",y="Feature",palette=geom_col_vec,title=mzname)
                      
                    }
                  }else{
                    
                    if(pairedanalysis==TRUE && connectpairedsamples==TRUE){
                      
                      if(add.jitter==TRUE){
                        p<-ggpaired(temp_dm2,x="Factor1",y="Feature",palette=geom_col_vec,add="jitter",title=mzname,line.color = "gray", line.size = 0.4)
                      }else{
                        p<-ggpaired(temp_dm2,x="Factor1",y="Feature",palette=geom_col_vec,title=mzname,line.color = "gray", line.size = 0.4)
                        
                      }
                      
                    }else{
                      if(add.jitter==TRUE){
                        p<-ggboxplot(temp_dm2,x="Factor1",y="Feature",palette=geom_col_vec,add="jitter",title=mzname)
                      }else{
                        p<-ggboxplot(temp_dm2,x="Factor1",y="Feature",palette=geom_col_vec,title=mzname)
                        
                      }
                    }
                  }
                  
                }else{
                  
                  
                  if(multiple.groups==FALSE){
                    if(add.jitter==TRUE){
                      p<-ggboxplot(temp_dm2,x="Class",y="Feature",color="Class",palette=geom_col_vec,add="jitter",title=mzname)
                    }else{
                      p<-ggboxplot(temp_dm2,x="Class",y="Feature",color="Class",palette=geom_col_vec,title=mzname)
                      
                    }
                  }else{
                    
                    if(pairedanalysis==TRUE && connectpairedsamples==TRUE){
       #               print(head(temp_dm2))
                      
                      if(add.jitter==TRUE){
                        p<-ggpaired(temp_dm2,x="Factor1",y="Feature",color="Factor2",palette=geom_col_vec,add="jitter",title=mzname,line.color = "gray", line.size = 0.4)
                      }else{
                        p<-ggpaired(temp_dm2,x="Factor1",y="Feature",color="Factor2",palette=geom_col_vec,title=mzname,line.color = "gray", line.size = 0.4)
                        
                      }
                      
                    }else{
                      if(add.jitter==TRUE){
                        p<-ggboxplot(temp_dm2,x="Factor1",y="Feature",color="Factor2",palette=geom_col_vec,add="jitter",title=mzname)
                      }else{
                        p<-ggboxplot(temp_dm2,x="Factor1",y="Feature",color="Factor2",palette=geom_col_vec,title=mzname)
                        
                      }
                    }
                  }
                }
                
                
                if(add.pvalues==TRUE){
                  
                  if(multiple.groups==FALSE){
                    p<-p + stat_compare_means(size = 5*cex.plots,label = "p.format",label.x = 0.5, label.y = max_yval1,size = 5*cex.plots)
                  }else{
                    if(pairedanalysis==FALSE){
                      #label = "p.format",
                      
                      p<-p + stat_compare_means(aes(group = Factor1),label = "p.format",label.x = 0.5, label.y = max_yval1,size = 5*cex.plots)
                      p<-p + stat_compare_means(aes(group = Factor2),label = "p.format",size = 5*cex.plots)
                    }else{
                      
                      p<-p + stat_compare_means(aes(group = Factor1),label = "p.format",label.x = 0.5, label.y = max_yval1,size = 5*cex.plots)
                      p<-p + stat_compare_means(aes(group = Factor2),paired=TRUE,label = "p.format",size = 5*cex.plots)
                    }
                  }
                }
              }
              
              
              p=p + font("axis.text", size = 12*cex.plots, color = "black") + font("axis.title", size = 13*cex.plots, color = "black")
              p=p + font("legend.text", size = 12*cex.plots, color = "black") #theme(legend.position = "right")
              p=p+theme(plot.title = element_text(hjust = 0.5,size=16*cex.plots),legend.position = "right") + labs(y=ylabel) + theme(axis.text.x = element_text(angle = 45, hjust = 1,vjust=-0.5))
              
              
            } 
            
            
           # p=p+scale_y_continuous(labels = scales::number)
            
            
            if(is.na(ylim.val)==FALSE){
              
              p=p+ylim(ylim.val[1], ylim.val[2])
            }
            
            return(list("plot_res"=p,"pval_res"=pval_res))
            #print(p)
          }
          
          
          
        }
        
        
        
      }
    },mzvec,timevec,check_names,name,class_labels_levels,sampleclass,col_vec,goodfeats,pairedanalysis,connectpairedsamples,boxplot.type,
    ggplot.type1,group_by_mat,cex.plots,boxplot.col.opt,add.jitter,add.pvalues,
    fill.plots,multiple.figures.perpanel,ylim.val,pval.reporting.method,ref.group.val,subset.factor1,subset.factor2,asterisk.size,posthoc.multcomp.test)
    
  #  save(plot_res,multiple.figures.perpanel,plot.height,plot.width,file="boxplot_plot.Rda")
      stopCluster(cl)
      
      if(add.pvalues==FALSE){
        
        pval.reporting.method="none"
      }
      
      #pval_res<-plot_res$pval_res
      #plot_res<-plot_res$plot_res
    if(boxplot.type=="ggplot"){
      
      suppressMessages(library(ggpubr))
      suppressMessages(library(gridExtra))
      suppressMessages(library(gtable))
        if(length(plot_res)>0){
          if(multiple.figures.perpanel==FALSE){
           
            mytheme <- gridExtra::ttheme_default(
              core = list(fg_params=list(cex = 1.2)),
              colhead = list(fg_params=list(cex = 1.3)),
              rowhead = list(fg_params=list(cex = 1.3)))
           
            if(pval.reporting.method=="twowayanova"){ 
            res<-lapply(seq(1,length(plot_res),1),function(i){
              
              p1=plot_res[[i]][[1]]
              mytheme <- gridExtra::ttheme_default(
                core = list(fg_params=list(cex = 1.2)),
                colhead = list(fg_params=list(cex = 1.3)),
                rowhead = list(fg_params=list(cex = 1.3)))
              tg1<-tableGrob(round(plot_res[[i]][[2]],5),theme = mytheme)
              
              #using the buffer group as reference\n
              
              title <- textGrob(paste("Two-way ANOVA with Tukey's HSD post-hoc comparisons\nFactor1 - Treatment; Factor2 - Time; Factor1:Factor2 - Interaction",sep=""),gp=gpar(fontsize=18))
              padding <- unit(5,"mm")
              
              text_high<-""
              
              text_high<-"Statistical significance is based on two-way ANOVA with Tukey's HSD post-hoc comparisons. Only post-hoc comparisons with the buffer group are shown here."
              p1=p1+ labs(caption = text_high)+theme(plot.caption = element_text(size=9*cex.plots,hjust=0,vjust=(0)))
              
              
              #tg1A<-gtable_add_grob(tg1,"Pairwise comparisons",1,1,1,ncol(tg1))
              tg1A <- gtable_add_rows(
                tg1, 
                heights = grobHeight(title) + padding,
                pos = 0)
              
              tg1A <- gtable_add_grob(
                tg1A, 
                title, 
                1, 1, 1, ncol(tg1A),clip='off')
              
              if(i==1){
                figure<-ggpubr::ggarrange(p1,tg1A,ncol = 2, nrow = 1,heights=c(plot.height),width=c(plot.width)) #,align = c("hv")) #,legend="right")
                
              }else{
                if(legend.all.pages==TRUE){
                  figure<-ggpubr::ggarrange(p1,tg1A,ncol = 2, nrow = 1,heights=c(plot.height),width=c(plot.width)) #,align = c("hv")) #,legend="right",align = c("hv"))
                  
                }else{
                  
                  figure<-ggpubr::ggarrange(p1,tg1A,ncol = 2, nrow = 1,heights=c(plot.height),width=c(plot.width)) #,align = c("hv")) #,legend="none",align = c("hv"))
                }    
              }
              return(figure)
            })
            }else{
             
              if(pval.reporting.method!="none"){
                res<-lapply(c(1,seq(1,length(plot_res),1)),function(i){
                  
                  p1=plot_res[[i]][[1]]
                  mytheme <- gridExtra::ttheme_default(
                    core = list(fg_params=list(cex = 1)),
                    colhead = list(fg_params=list(cex = 1)),
                    rowhead = list(fg_params=list(cex = 1)))
                  tg1<-tableGrob(round(plot_res[[i]][[2]],5),theme = mytheme)
                  
                  #using the buffer group as reference\n
                  
                  title <- textGrob(paste(""),gp=gpar(fontsize=18))
                  padding <- unit(5,"mm")
                  text_high<-""
                  text_high<-""
                  if(pval.reporting.method=="wilcox.test"){
                    text_high<-paste("The asterisks represent p-values from pairwise Wilcoxon rank-sum test results using the ",ref.group.val," group as reference.\nSee the pvalues.table_* file for all pairwise comparisons.",sep="")
                  }else{
                    if(pval.reporting.method=="kruskal.test"){
                      text_high<-"The asterisks represent p-values from Kruskal-Wallis one-way ANOVA. \nSee the pvalues.table for Dunn's post-hoc comparisons."
                    }else{
                      
                      if(pval.reporting.method=="t.test"){
                        text_high<-paste("The asterisks represent p-values from pairwise Welch's t-test results using the ",ref.group.val," group as reference.\nSee the pvalues.table_* file for all pairwise comparisons.",sep="")
                      }
                      
                    }
                  }
                  p1=p1+ labs(caption = text_high)+theme(plot.caption = element_text(size=9*cex.plots,hjust=0,vjust=(0)))
                  
                  
                  #tg1A<-gtable_add_grob(tg1,"Pairwise comparisons",1,1,1,ncol(tg1))
                  tg1A <- gtable_add_rows(
                    tg1, 
                    heights = grobHeight(title) + padding,
                    pos = 0)
                  
                  tg1A <- gtable_add_grob(
                    tg1A, 
                    title, 
                    1, 1, 1, ncol(tg1A),clip='off')
                  
                  
                  if(i==1){
                    figure<-ggpubr::ggarrange(p1,tg1A,ncol = 1, nrow = 1,heights=c(plot.height),width=c(plot.width),legend="right") #align = c("hv"),
                    
                  }else{
                    if(legend.all.pages==TRUE){
                      figure<-ggpubr::ggarrange(p1,tg1A,ncol = 1, nrow = 1,heights=c(plot.height),width=c(plot.width),legend="right") #,align = c("hv")) #,legend="right",align = c("hv"))
                      
                    }else{
                      
                      figure<-ggpubr::ggarrange(p1,tg1A,ncol = 1, nrow = 1,heights=c(plot.height),width=c(plot.width),legend="right") #,align = c("hv")) #,legend="none",align = c("hv"))
                    }    
                  }
                  return(figure)
                }) 
                
                  
                  res2<-lapply(seq(1,length(plot_res),1),function(i){
                    
                    p1=plot_res[[i]][[1]]
                    mytheme <- gridExtra::ttheme_default(
                      core = list(fg_params=list(cex = 1.1)),
                      colhead = list(fg_params=list(cex = 1.1)),
                      rowhead = list(fg_params=list(cex = 1.1)))
                    tg1<-tableGrob(round(plot_res[[i]][[2]],5),theme = mytheme)
                    
                    #using the buffer group as reference\n
                    
                    title <- textGrob(paste(""),gp=gpar(fontsize=18))
                    padding <- unit(5,"mm")
                    text_high<-""
                    if(pval.reporting.method=="wilcox.test"){
                      text_high<-paste("The asterisks represent p-values from pairwise Wilcoxon rank-sum test results using the ",ref.group.val," group as reference.\nSee the pvalues.table_* file for all pairwise comparisons.",sep="")
                    }else{
                      if(pval.reporting.method=="kruskal.test"){
                        text_high<-"The asterisks represent p-values from Kruskal-Wallis one-way ANOVA. \nSee the pvalues.table for Dunn's post-hoc comparisons."
                      }else{
                        
                        if(pval.reporting.method=="t.test"){
                        text_high<-paste("The asterisks represent p-values from pairwise Welch's t-test results using the ",ref.group.val," group as reference.\nSee the pvalues.table_* file for all pairwise comparisons.",sep="")
                        }
                        
                      }
                    }
                    p1=p1+ labs(caption = text_high)+theme(plot.caption = element_text(size=9*cex.plots,hjust=0,vjust=(0)))
                    
                    
                    #tg1A<-gtable_add_grob(tg1,"Pairwise comparisons",1,1,1,ncol(tg1))
                    tg1A <- gtable_add_rows(
                      tg1, 
                      heights = grobHeight(title) + padding,
                      pos = 0)
                    
                    tg1A <- gtable_add_grob(
                      tg1A, 
                      title, 
                      1, 1, 1, ncol(tg1A),clip='off')
                    
                    if(i==1){
                      figure<-ggpubr::ggarrange(p1,tg1A,ncol = 2, nrow = 1,heights=c(plot.height),width=c(plot.width)) #align = c("hv"),
                      
                    }else{
                      if(legend.all.pages==TRUE){
                        figure<-ggpubr::ggarrange(p1,tg1A,ncol = 2, nrow = 1,heights=c(plot.height),width=c(plot.width)) #,align = c("hv")) #,legend="right",align = c("hv"))
                        
                      }else{
                        
                        figure<-ggpubr::ggarrange(p1,tg1A,ncol = 2, nrow = 1,heights=c(plot.height),width=c(plot.width)) #,align = c("hv")) #,legend="none",align = c("hv"))
                      }    
                    }
                    return(figure)
                  }) 
                
              }else{
                
                
                res<-lapply(seq(1,length(plot_res),1),function(i){
                  
                  p1=plot_res[[i]][[1]]
                  mytheme <- gridExtra::ttheme_default(
                    core = list(fg_params=list(cex = 0.75)),
                    colhead = list(fg_params=list(cex = 1)),
                    rowhead = list(fg_params=list(cex = 1)))
                  
                  
                  if(i==1){
                    figure<-ggpubr::ggarrange(p1,ncol = 1, nrow = 1,heights=c(plot.height),width=c(plot.width),legend="right") #align = c("hv"),
                    
                  }else{
                    if(legend.all.pages==TRUE){
                      figure<-ggpubr::ggarrange(p1,ncol =1, nrow = 1,heights=c(plot.height),width=c(plot.width),legend="right") #,align = c("hv")) #,legend="right",align = c("hv"))
                      
                    }else{
                      
                      figure<-ggpubr::ggarrange(p1,ncol = 1, nrow = 1,heights=c(plot.height),width=c(plot.width),legend="right") #,align = c("hv")) #,legend="none",align = c("hv"))
                    }    
                  }
                  return(figure)
                })  
              }
                
            }
                
            
          }
          
          }else{
            res<-lapply(seq(1,length(plot_res),4),function(i){
              p1=plot_res[[i]]
              p1=plot_res[[i]]
              p2={}
              p3={}
              p4={}
              
              if((i+1)<length(plot_res)){
                p2=plot_res[[i+1]]
              }
              if((i+2)<length(plot_res)){
                p3=plot_res[[i+2]]
              }
              if((i+3)<length(plot_res)){
                
                p4=plot_res[[i+3]]
              }
              figure<-ggarrange(p1,p2,p3,p4,ncol = 2, nrow = 2,heights=c(4,4),width=c(6,6),legend=FALSE,align = c("hv"))
              # gg##save(res,file="t.pdf")
              #figure<-ggarrange(p1,ncol = 2, nrow = 2,heights=c(4,4),width=c(6,6),legend=FALSE,align = c("hv"))
              return(figure)
            })
            
          }
          
        
          
      
      library(ggpubr)
     # library(cowplot)
      feature.names<-names_with_mz_time[,c(1)]
      names(plot_res)<-feature.names
      
      if(pval.table.report==TRUE){
      pval_table<-lapply(1:length(plot_res),function(j){as.data.frame(t(plot_res[[j]][[2]]))})
      
      pval_table<-ldply(pval_table,rbind)
      #pval_table<-round(pval_table,5)
      rownames(pval_table)<-names(plot_res)
      pval_table<-cbind(names(plot_res),pval_table)
      
      pval_table<-cbind(colnames(pval_table[,-c(1)]),t(pval_table[,-c(1)]))
      
      pval_table<-as.data.frame(pval_table)
      
      colnames(pval_table)<-c("Comparison",names(plot_res))
      
      boxplots_fname1=gsub(boxplots_fname,pattern=".pdf$",replacement="")
      
      fname_temp<-paste("pvalues.table_",boxplots_fname1,".txt",sep="")
      write.table(pval_table,file=fname_temp,sep="\t",row.names=FALSE)
      
      # save(res,plot_res,feature.names,file="res_with_pvalues.Rda")
      }
      #if(FALSE)
      {
        res<-lapply(1:length(res),function(x){
          return(res[[x]][[1]])
          # print(res[[x]][[1]])
        })
        
      }
     res<-append(res, ggpubr::get_legend(plot_res[[1]][[1]]))
      #res[[length(res)+1]][[1]] <- ggpubr::get_legend(plot_res[[i]])
      
      ggpubr::ggexport(res,filename =boxplots_fname,width=unit(plot.width-0.5, "in"),
                       height=unit(plot.height-0.5, "in"))
        if(FALSE)
          {
            if(pval.table.report==TRUE){
        res2<-lapply(1:length(res2),function(x){
          return(res2[[x]][[1]])
          # print(res[[x]][[1]])
        })
        boxplots_fname2<-paste("pvalues",boxplots_fname,sep="")
        
       # ggpubr::ggexport(res2,filename =boxplots_fname2,width=unit(plot.width-0.5, "in"),
        #                 height=unit(plot.height-0.5, "in"))
        
        #save(res2,name,file="res_with_pvalues.Rda")
        }
        }
   # ggpubr::ggexport(res,filename =boxplots_fname)
        
      
    }
      
  }
 
  if(newdevice==TRUE){
    try(dev.off(boxplots_fname),silent=TRUE)
  }else{
    try(dev.off(boxplots_fname),silent=TRUE)
  }
  try(unlink("Name_mz_time_mapping.txt"))

  #par(mfrow=c(1,1))
  options(warn=0)
  return(res)
}
