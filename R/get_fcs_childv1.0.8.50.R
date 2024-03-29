get_fcs_childv1.0.8.50 <-
function(metab_data,reference_sets,itrs=1000,fcs.min.hits=2,numnodes=2){
  
  
  
  # colnames(reference_sets)<-c("SetID","Name","XID")
  count.unique.formula.overlapsize=TRUE
  dup.feature.check=TRUE
  
  
  cnames1<-colnames(metab_data)
  
  cnames1[1:2]<-c("XID","Statistic")
  
  colnames(metab_data)<-cnames1
  
  
  set_size=table(reference_sets$SetID)
  set_size<-cbind(names(set_size),set_size)
  set_size<-as.data.frame(set_size)
  colnames(set_size)<-c("SetID","Total.Size")
  
  
  metab_data_sets<-merge(metab_data,reference_sets,by="XID")
  
  #save(metab_data_sets,metab_data,reference_sets,file="metab_data_sets.Rda")
  
  metab_data_sets<-metab_data_sets[order(metab_data_sets$SetID,metab_data_sets$Statistic,decreasing=TRUE),]
  
  if(count.unique.formula.overlapsize==TRUE){
    
    if(length(grep(colnames(metab_data_sets),pattern="Formula"))>0){
      xid_formula<-paste(metab_data_sets$SetID,"_",metab_data_sets$Formula,sep="")
      
      dup_check<-which(duplicated(xid_formula)==TRUE)
      if(length(dup_check)>0){
        metab_data_sets<-metab_data_sets[-dup_check,]
      }
    }
  }
  #  write.table(metab_data_sets,file="fcs_sets_with_statistic.txt",sep="\t",row.names=FALSE)
  metab_data_sets<-metab_data_sets[,c(1:4)]
  
  if(nrow(metab_data_sets)<1){
    
    metabset_res_mat<-{}
    
  }else{
    sid_list<-metab_data_sets$SetID;
    sid_list<-unique(sid_list)
    
    metab_data_sets$Statistic<-as.numeric(as.character(metab_data_sets$Statistic))
    
    
    set_statistic1=aggregate(metab_data_sets$Statistic,by=list(metab_data_sets$SetID),FUN=function(x){
      x<-as.numeric(as.character(x))
      max_mean<-max(mean(x[which(x>0)],na.rm=TRUE),mean(x[which(x<0)],na.rm=TRUE),na.rm=TRUE)
      
      resv<-c(agg.stat=sum(x,na.rm=TRUE)/length(x),zscore=sqrt(length(x))*sum(x,na.rm=TRUE)/length(x), maxmean=max_mean,numhits=length(x))
      resv<-round(resv,3)
      return(resv)
    })
    set_statistic1<-do.call(data.frame,set_statistic1)
    colnames(set_statistic1)=c("SetID","Agg.Statistic","Z.score","MaxMean","Num.Hits")
    
    set_statistic1$Z.score<-scale(set_statistic1$MaxMean)
    
    
    #metabset_res<-parLapply(cl,1:length(sid_list),function(c,dup.feature.check,fcs.method,fcs.permutation.type){
    z.test1 = function(x=NA,mu=NA,popvar=NA,z.score=NA){
      
      one.tail.p <- NULL
      
      if(is.na(z.score)==TRUE){
        z.score <- round((mean(x)-mu)/(popvar/sqrt(length(x))),3)
      }
      one.tail.p <- round(pnorm(abs(z.score),lower.tail = FALSE),3)
      
      
      return(one.tail.p)
    }
    if(dup.feature.check==TRUE){
      
      if(length(duplicated(paste(metab_data_sets$SetID,"_",metab_data_sets$mz,"_",metab_data_sets$time,sep=""))==TRUE)>0){
        metab_data_sets<-metab_data_sets[-which(duplicated(paste(metab_data_sets$SetID,"_",metab_data_sets$mz,"_",metab_data_sets$time,sep=""))==TRUE),]
      }
    }
    
    metab_data_sets<-merge(metab_data_sets,set_size,by="SetID")
    
    #save(metab_data_sets,file="prerand.Rda")
    
    cl<-makeCluster(max(numnodes,detectCores()*0.5))
    #   a=Sys.time()
    randmetabset_res<-parLapply(cl,1:itrs,function(i,metab_data_sets){
      
      set.seed(i)
      all <- sample(1:nrow(metab_data_sets),nrow(metab_data_sets))
      randmetab_data_sets<-metab_data_sets
      randmetab_data_sets$Statistic<-metab_data_sets$Statistic[all]
      randmetab_data_sets$XID<-metab_data_sets$XID[all]
      
      #randmetab_data_sets[,1]<-randkeggids
      #randmetab_data_sets[,2]<-randstat
      
      randset_statistic1=aggregate(randmetab_data_sets$Statistic,by=list(randmetab_data_sets$SetID),FUN=function(x){
        
        x<-as.numeric(as.character(x))
        
        max_mean<-max(mean(x[which(x>0)],na.rm=TRUE),mean(x[which(x<0)],na.rm=TRUE),na.rm=TRUE)
        
        
        resv<-c(agg.stat=sum(x,na.rm=TRUE)/length(x),zscore=sqrt(length(x))*sum(x,na.rm=TRUE)/length(x), maxmean=max_mean)
        
        resv<-round(resv,3)
        
        # resv<-as.data.frame(resv)
        return(resv)
      })
      
      return(randset_statistic1)
      
    },metab_data_sets=metab_data_sets)
    
    # b=Sys.time()
    #print(b-a)
    stopCluster(cl)
    
    randmetabset_res<-do.call(rbind, randmetabset_res)
    randmetabset_res<-do.call(data.frame, randmetabset_res)
    
    
    
    #checkhere
    
    colnames(randmetabset_res)<-c("SetID","Agg.Statistic","Z.score","MaxMean")
    
    randmetabset_res$Z.score<-randmetabset_res$MaxMean
    
    randset_maxmean=aggregate(randmetabset_res$MaxMean,by=list(randmetabset_res$SetID),FUN=function(x){
      
      x<-as.numeric(as.character(x))
      return(c(meanval=round(mean(x,na.rm=TRUE),3),sdval=round(sd(x,na.rm=TRUE),3)))
    })
    
    randset_maxmean<-do.call(data.frame,randset_maxmean)
    
    colnames(randset_maxmean)<-c("SetID","RandMeanMaxMean","RandSdMaxMean")
    
    
    set_statistic1<-merge(set_statistic1,randset_maxmean,by="SetID")
    
    MaxMean.Std<-round((set_statistic1$MaxMean-set_statistic1$RandMeanMaxMean)/set_statistic1$RandSdMaxMean,3)
    
    
    set_statistic1<-cbind(set_statistic1,MaxMean.Std)
    
    randmetabset_res<-merge(randmetabset_res,randset_maxmean,by="SetID")
    RandMaxMean.Std<-(randmetabset_res$MaxMean-randmetabset_res$RandMeanMaxMean)/randmetabset_res$RandSdMaxMean
    randmetabset_res<-cbind(randmetabset_res,round(RandMaxMean.Std,3))
    #set_statistic1A=merge(set_statistic1,set_size,by="SetID")
    #Norm.Agg.Statistic=set_statistic1A$Agg.Statistic/set_statistic1A$Size
    
    meanp.agg<-function (p)
    {
      keep <- (p >= 0) & (p <= 1)
      invalid <- sum(1L * keep) < 3
      if (invalid) {
        warning("Must have at least four valid p values")
        res <- list(z = NA_real_, p = NA_real_, validp = p[keep])
      }
      else {
        pi <- mean(p[keep])
        k <- length(p[keep])
        z <- (0.5 - pi) * sqrt(12 * k)
        if (k != length(p)) {
          warning("Some studies omitted")
        }
        res <- list(z = z, p = pnorm(z, lower.tail = FALSE),
                    validp = p[keep])
      }
      #class(res) <- c("meanp", "metap")
      res
    }
    
    #save(randmetabset_res,set_statistic1,file="pvaldebug.Rda")
    metabset_res1<-lapply(1:length(sid_list),function(c){
      
      
      cur_sid_rand<-randmetabset_res[which(randmetabset_res$SetID==sid_list[c]),]
      cur_sid_orig<-set_statistic1[which(set_statistic1$SetID==sid_list[c]),]
      if(nrow(cur_sid_orig)>0){
        pval_agg.stat=length(which(as.numeric(as.character(cur_sid_rand[,2]))>as.numeric(as.character(cur_sid_orig[2]))))/itrs
        
        z.score=as.numeric(as.character(cur_sid_orig[3]))
        
        pval_zscore=round(pnorm(abs(z.score),lower.tail = FALSE),3)
        
        #length(which(as.numeric(as.character(cur_sid_rand[,3]))>as.numeric(as.character(cur_sid_orig[3]))))/itrs
        pval_stdmaxmean=length(which(as.numeric(as.character(cur_sid_rand[,4]))>as.numeric(as.character(cur_sid_orig[4]))))/itrs
        
        #z.score=as.numeric(as.character(cur_sid_orig[7]))
        # pval_zscore=round(pnorm(abs(z.score),lower.tail = FALSE),3)
        
        #pval_stdmaxmean=round(pnorm(abs(z.score),lower.tail = FALSE),3)
        
        pval_meta=meanp.agg(c(pval_agg.stat,pval_zscore,pval_stdmaxmean))$p #,na.rm=TRUE)$p
        #zval=(0.5-meanp_val)*sqrt(12*3)
        #pval_meta <- round(pnorm((zval),lower.tail = FALSE),3)
        
        pval_mat<-c(as.character(sid_list[c]),round(pval_agg.stat,3),round(pval_zscore,3),round(pval_stdmaxmean,3),round(pval_meta,3))
        #   return(pval_mat)
      }
    })
    
    metabset_res_pvalmat<-ldply(metabset_res1,rbind)
    
    metabset_res_qvalmat<-apply(metabset_res_pvalmat[,-c(1)],2,function(x){p.adjust(x,method="BH")})
    
    metabset_res_pvalmat<-cbind(metabset_res_pvalmat,round(metabset_res_qvalmat,3))
    
    colnames(metabset_res_pvalmat)<-c("SetID","pval.Agg.Statistic","pval.Z.score","pval.MaxMean","pval.meta","FDR.Agg.Statistic","FDR.Z.score","FDR.MaxMean","FDR.meta")
    
    res<-merge(set_statistic1,metabset_res_pvalmat,by="SetID")
    
    res<-merge(res,set_size,by="SetID")
    
    #res$Z.score<-res$MaxMean.Std
    
    #save(res,file="reschild.Rda")
    
    return(res)
    
  }
}
