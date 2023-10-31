get_pval_boxplots <-
function(df,pval.reporting.method="wilcox.test",p.adjust.method="bonferroni",ref.group="Buffer",pool.sd.bool=TRUE,var.equal.bool=TRUE){
  
  stat_table1=compare_means(Feature~Factor1,data=df,ref.group=ref.group,group.by = "Factor2",method="wilcox.test")
  stat_table1A<-stat_table1[,c("Factor2","group1","group2","p.format")]
  colnames(stat_table1A)<-c("TimePoint","Group1","Group2","p.value")
  
  #summary(stat_table1)
  
  stat_table1B<-{}
  j=1
  comparison_list<-{}
 
  for(j in 1:nrow(stat_table1A))
  {
    
    group1_mat <- subset(df,Factor1==stat_table1A$Group1[1] & Factor2==stat_table1A$TimePoint[1])
    group2_mat <- subset(df,Factor1==stat_table1A$Group2[j] & Factor2==stat_table1A$TimePoint[j])
    if(FALSE){
      sum_group1<-round(summary(as.numeric(as.character(group1_mat$Feature))),3)
      sum_group2<-round(summary(as.numeric(as.character(group2_mat$Feature))),3)
      sum_group1_text<-paste(sum_group1[3]," (",sum_group1[2],"-",sum_group1[5],")",sep="")
      sum_group2_text<-paste(sum_group2[3]," (",sum_group2[2],"-",sum_group2[5],")",sep="")
      
      sum_group1_text<-paste(mean(as.numeric(as.character(group1_mat$Feature)),na.rm=TRUE), " (",sd(as.numeric(as.character(group1_mat$Feature)),na.rm=TRUE),")",sep="")
      sum_group2_text<-paste(mean(as.numeric(as.character(group2_mat$Feature)),na.rm=TRUE), " (",sd(as.numeric(as.character(group2_mat$Feature)),na.rm=TRUE),")",sep="")
      
      x=c(as.numeric(as.character(group1_mat$Feature)))
      y=c(as.numeric(as.character(group2_mat$Feature)))
      #print(t.test(x,y))
      stat_table1B<-rbind(stat_table1B,cbind(stat_table1A[j,],sum_group1_text,sum_group2_text))
    }
    comparison_list<-c(comparison_list,paste(group2_mat$Factor1,":",group2_mat$Factor2,"-",group1_mat$Factor1,":",group1_mat$Factor2,sep=""))
  }
  
   for(j in 1:nrow(stat_table1A))
  {
        
           group1_mat <- subset(df,Factor1==stat_table1A$Group1[1] & Factor2==stat_table1A$TimePoint[j])
          group2_mat <- subset(df,Factor1==stat_table1A$Group2[j] & Factor2==stat_table1A$TimePoint[j])
          if(FALSE){
          sum_group1<-round(summary(as.numeric(as.character(group1_mat$Feature))),3)
          sum_group2<-round(summary(as.numeric(as.character(group2_mat$Feature))),3)
          sum_group1_text<-paste(sum_group1[3]," (",sum_group1[2],"-",sum_group1[5],")",sep="")
          sum_group2_text<-paste(sum_group2[3]," (",sum_group2[2],"-",sum_group2[5],")",sep="")
          
          sum_group1_text<-paste(mean(as.numeric(as.character(group1_mat$Feature)),na.rm=TRUE), " (",sd(as.numeric(as.character(group1_mat$Feature)),na.rm=TRUE),")",sep="")
          sum_group2_text<-paste(mean(as.numeric(as.character(group2_mat$Feature)),na.rm=TRUE), " (",sd(as.numeric(as.character(group2_mat$Feature)),na.rm=TRUE),")",sep="")
          
          x=c(as.numeric(as.character(group1_mat$Feature)))
          y=c(as.numeric(as.character(group2_mat$Feature)))
          #print(t.test(x,y))
          stat_table1B<-rbind(stat_table1B,cbind(stat_table1A[j,],sum_group1_text,sum_group2_text))
        }
    comparison_list<-c(comparison_list,paste(group2_mat$Factor1,":",group2_mat$Factor2,"-",group1_mat$Factor1,":",group1_mat$Factor2,sep=""))
  }
  
 
  
  for(j in 2:nrow(stat_table1A))
  {
    
    group1_mat <- subset(df,Factor1==stat_table1A$Group1[1] & Factor2==stat_table1A$TimePoint[1])
    group2_mat <- subset(df,Factor1==stat_table1A$Group1[j] & Factor2==stat_table1A$TimePoint[j])
    if(FALSE){
      sum_group1<-round(summary(as.numeric(as.character(group1_mat$Feature))),3)
      sum_group2<-round(summary(as.numeric(as.character(group2_mat$Feature))),3)
      sum_group1_text<-paste(sum_group1[3]," (",sum_group1[2],"-",sum_group1[5],")",sep="")
      sum_group2_text<-paste(sum_group2[3]," (",sum_group2[2],"-",sum_group2[5],")",sep="")
      
      sum_group1_text<-paste(mean(as.numeric(as.character(group1_mat$Feature)),na.rm=TRUE), " (",sd(as.numeric(as.character(group1_mat$Feature)),na.rm=TRUE),")",sep="")
      sum_group2_text<-paste(mean(as.numeric(as.character(group2_mat$Feature)),na.rm=TRUE), " (",sd(as.numeric(as.character(group2_mat$Feature)),na.rm=TRUE),")",sep="")
      
      x=c(as.numeric(as.character(group1_mat$Feature)))
      y=c(as.numeric(as.character(group2_mat$Feature)))
      #print(t.test(x,y))
      stat_table1B<-rbind(stat_table1B,cbind(stat_table1A[j,],sum_group1_text,sum_group2_text))
    }
    comparison_list<-c(comparison_list,paste(group2_mat$Factor1,":",group2_mat$Factor2,"-",group1_mat$Factor1,":",group1_mat$Factor2,sep=""))
  }
  
  temp_df<-df[,c("Feature","Factor1","Factor2")]
  cnames1<-colnames(temp_df)
  cnames1[1]<-"Response"
  colnames(temp_df)<-cnames1
  
 # save(temp_df,file="temp_df.Rda")
  
  if(pval.reporting.method=="twowayanova"){
  res1<-diffexplmtwowayanova(temp_df)
  }else{
    if(pval.reporting.method=="kruskal.test"){
     
      factor2_levels1<-levels(factor(temp_df$Factor2,levels=unique(temp_df$Factor2)))
      
      res1<-lapply(1:length(factor2_levels1),function(i){
        
        temp_df1<-temp_df[which(temp_df$Factor2==factor2_levels1[i]),]
        
        main_res<-kruskal.test(temp_df1$Response~temp_df1$Factor1)
        resdunn<-dunn_test(data=temp_df1,formula=Response~Factor1,p.adjust.method = p.adjust.method) #dunnTest(temp_df1$Response~temp_df1$Factor1,method="none")
        
        temp_res<-c(main_res$p.value,resdunn$p.adj)
        
        main_res<-as.data.frame(temp_res)
        
        rnames_vec<-c("Across all groups",paste(resdunn$group1," - ",resdunn$group2,sep=""))
        
        
        rownames(main_res)<-paste("(Day ",factor2_levels1[i],") ",rnames_vec,sep="")
        
        
        return(main_res)
      })
      
      res1<-bind_rows(res1)
      res1<-na.omit(res1)
      if(p.adjust.method=="none"){
      colnames(res1)<-"p.value"
      }else{
        
        colnames(res1)<-paste("p.adjusted (",p.adjust.method,")",sep="")
      }
      
    }else{
      
      if(pval.reporting.method=="wilcox.test"){
        
        factor1_levels1<-levels(factor(temp_df$Factor1,levels=unique(temp_df$Factor1)))
        factor2_levels1<-levels(factor(temp_df$Factor2,levels=unique(temp_df$Factor2)))
        
        #within day comparisons
        res1<-lapply(1:length(factor2_levels1),function(i){
          
          temp_df1<-temp_df[which(temp_df$Factor2==factor2_levels1[i]),]
          
          main_res<-pairwise.wilcox.test(x=temp_df1$Response,g=temp_df1$Factor1,p.adjust.method = p.adjust.method)
          
          #pw_res<-dunnTest(temp_df1$Response~temp_df1$Factor1,method="bh")
          
          rnames_vec<-{}
          
          for(j in 1:ncol(main_res$p.value)){
            
            rnames_vec<-c(rnames_vec,paste(colnames(main_res$p.value)[j]," - ",rownames(main_res$p.value),sep=""))
          }
          main_res<-as.vector(round(main_res$p.value,4))
          names(main_res)<-paste("(Day ",factor2_levels1[i],") ",rnames_vec,sep="")
          main_res<-as.data.frame(main_res)
          rownames(main_res)<-paste("(Day ",factor2_levels1[i],") ",rnames_vec,sep="")
          #return(list("mainpvalues"=main_res,"posthoc"={}))
          return(main_res)
        })
        
        res1A<-bind_rows(res1)
        res1A<-na.omit(res1A)
        
        ref_group=temp_df[which(temp_df$Factor2==factor2_levels1[1] & temp_df$Factor1==factor1_levels1[1]),]
        ref_group$Factor1<-paste(ref_group$Factor2,":",ref_group$Factor1,sep="")
        #day1 comparisons
        res1<-lapply(1:length(factor2_levels1),function(i){
          
          temp_df1<-temp_df[which(temp_df$Factor2==factor2_levels1[i]),]
          
          temp_df1<-rbind(ref_group,temp_df1)
         # temp_df1$Factor1<-levels(factor(temp_df1$Factor1,levels=unique(temp_df1$Factor1)))
          
          main_res<-pairwise.wilcox.test(x=temp_df1$Response,g=temp_df1$Factor1,p.adjust.method = p.adjust.method)
          
          #pw_res<-dunnTest(temp_df1$Response~temp_df1$Factor1,method="bh")
          
          rnames_vec<-{}

          #for(j in 1:ncol(main_res$p.value)){
            
            rnames_vec<-paste("(Day ", colnames(main_res$p.value)[1],") - (Day ",factor2_levels1[i],") ",rownames(main_res$p.value),sep="")
          #}
          main_res<-as.vector(round(main_res$p.value[,1],4))
                #names(main_res)<-paste("(Day ",factor2_levels[i],") ",rnames_vec,sep="")
                main_res<-as.data.frame(main_res)
                rownames(main_res)<- rnames_vec #paste("(Day ",factor2_levels[i],") ",rnames_vec,sep="")
         # return(list("mainpvalues"=main_res,"posthoc"={}))
          return(main_res)
        })
        
        res1B<-bind_rows(res1)
        res1B<-na.omit(res1B)
        
        res1<-rbind(res1A,res1B)
        
        if(p.adjust.method=="none"){
          colnames(res1)<-"p.value"
        }else{
          
          colnames(res1)<-paste("p.adjusted (",p.adjust.method,")",sep="")
        }
      }else{
        
        
        if(pval.reporting.method=="t.test"){
          
          factor1_levels1<-levels(factor(temp_df$Factor1,levels=unique(temp_df$Factor1)))
          factor2_levels1<-levels(factor(temp_df$Factor2,levels=unique(temp_df$Factor2)))
          
          #within day comparisons
          #pool.sd.bool
          res1<-lapply(1:length(factor2_levels1),function(i){
            
            temp_df1<-temp_df[which(temp_df$Factor2==factor2_levels1[i]),]
            temp_df1$Response<-as.numeric(as.character(temp_df1$Response))
            main_res<-pairwise.t.test(x=temp_df1$Response,g=temp_df1$Factor1,p.adjust.method = p.adjust.method,pool.sd=pool.sd.bool,var.equal = var.equal.bool)
            
         
            
            rnames_vec<-{}
            
            for(j in 1:ncol(main_res$p.value)){
              
              rnames_vec<-c(rnames_vec,paste(colnames(main_res$p.value)[j]," - ",rownames(main_res$p.value),sep=""))
            }
            main_res<-as.vector(round(main_res$p.value,4))
            names(main_res)<-paste("(Day ",factor2_levels1[i],") ",rnames_vec,sep="")
            main_res<-as.data.frame(main_res)
            rownames(main_res)<-paste("(Day ",factor2_levels1[i],") ",rnames_vec,sep="")
            #return(list("mainpvalues"=main_res,"posthoc"={}))
            return(main_res)
          })
          
          res1A<-bind_rows(res1)
          res1A<-na.omit(res1A)
          
          ref_group=temp_df[which(temp_df$Factor2==factor2_levels1[1] & temp_df$Factor1==factor1_levels1[1]),]
          ref_group$Factor1<-paste(ref_group$Factor2,":",ref_group$Factor1,sep="")
          #reference group comparisons
          res1<-lapply(1:length(factor2_levels1),function(i){
            
            temp_df1<-temp_df[which(temp_df$Factor2==factor2_levels1[i]),]
            
            temp_df1<-rbind(ref_group,temp_df1)
            # temp_df1$Factor1<-levels(factor(temp_df1$Factor1,levels=unique(temp_df1$Factor1)))
            
            main_res<-pairwise.t.test(x=temp_df1$Response,g=temp_df1$Factor1,p.adjust.method = p.adjust.method,pool.sd=pool.sd.bool,var.equal = var.equal.bool)
            
            #pw_res<-dunnTest(temp_df1$Response~temp_df1$Factor1,method="bh")
            
            rnames_vec<-{}
            
            #for(j in 1:ncol(main_res$p.value)){
            
            rnames_vec<-paste("(Day ", colnames(main_res$p.value)[1],") - (Day ",factor2_levels1[i],") ",rownames(main_res$p.value),sep="")
            #}
            main_res<-as.vector(round(main_res$p.value[,1],4))
            #names(main_res)<-paste("(Day ",factor2_levels[i],") ",rnames_vec,sep="")
            main_res<-as.data.frame(main_res)
            rownames(main_res)<- rnames_vec #paste("(Day ",factor2_levels[i],") ",rnames_vec,sep="")
            # return(list("mainpvalues"=main_res,"posthoc"={}))
            return(main_res)
          })
          
          res1B<-bind_rows(res1)
          res1B<-na.omit(res1B)
          
          res1<-rbind(res1A,res1B)
          
          if(p.adjust.method=="none"){
            colnames(res1)<-"p.value"
          }else{
            
            colnames(res1)<-paste("p.adjusted (",p.adjust.method,")",sep="")
          }
        }else{
          
          
          if(pval.reporting.method=="onewayanova"){
            res1<-diffexponewayanova(temp_df)
            
            
            posthoc_comp<-t(res1$posthoc)
            
            print(comparison_list)
            
            posthoc_comp<-posthoc_comp[which(rownames(posthoc_comp)%in%unique(comparison_list)),]
        
            
            final_res<-c(t(res1$mainpvalues),posthoc_comp)
          }
        }
      }
    }
    
  }
  
  if(pval.reporting.method=="twowayanova"){
  posthoc_comp<-t(res1$posthoc)
  
  print(comparison_list)
  
  posthoc_comp<-posthoc_comp[which(rownames(posthoc_comp)%in%unique(comparison_list)),]
  #View(unique(comparison_list))
  
  #View(posthoc_comp)
  
  save(temp_df,file="temp_df.Rda")
  
  final_res<-c(t(res1$mainpvalues),posthoc_comp)
  }else{
    final_res<-res1 #t(res1$mainpvalues)
  }
  final_res<-as.data.frame(t(final_res))
  final_res<-t(final_res)
  rnames1<-rownames(final_res)
  
  if(pval.reporting.method=="twowayanova"){
    rnames1[1:3]<-names(res1$mainpvalues)
  }
  
  rownames(final_res)<-rnames1
  colnames(final_res)<-"p.value"
  #rownames(final_res)<-c(names(res1$mainpvalues),rownames(posthoc_comp))
  return(final_res)
 # stat_table1B<-as.data.frame(stat_table1B)
  
  
}
