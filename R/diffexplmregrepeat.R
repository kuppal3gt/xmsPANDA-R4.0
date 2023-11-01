#
diffexplmregrepeat <-
function(dataA,subject_inf,modeltype="lme.RIRS",covar.matrix=NA){
  
  dataA<-as.data.frame(dataA)
  
  Subject<-subject_inf
  dataA<-cbind(dataA,subject_inf)
  save(dataA,covar.matrix,file="dataA.Rda")
  
  dataA$Response<-as.numeric(dataA$Response)
  dataA$Factor1<-as.numeric(dataA$Factor1)
  
  y <- "Response"
  x <- names(dataA)[!names(dataA) %in% c(y,"subject_inf")]
  mymodel <- as.formula(paste(y, paste(x, collapse="+"), sep="~"))
  
  save(dataA,covar.matrix,file="dataA.Rda")
  
  
  if(is.na(covar.matrix)==FALSE){
          if(modeltype=="lme.RI"){
            
            res <- lme(mymodel, random = ~ 1 | subject_inf, data=dataA,control=lmeControl(opt="optim"),correlation=corCompSymm(form=~1|subject_inf)) #,silent=TRUE)
          }else{
            if(modeltype=="lme.RIRS"){
              res <- try(lme(mymodel, random = ~ 1 + Factor1 | subject_inf, data=dataA,control=lmeControl(opt="optim"),correlation=corCompSymm(form=~1|subject_inf)),silent=TRUE)
            }else{
              if(modeltype=="nlme.RIRS"){
                res <- try(nlme(mymodel, random = ~ 1 + Factor1 | subject_inf, data=dataA,control=lmeControl(opt="optim"),correlation=corCompSymm(form=~1|subject_inf)),silent=TRUE)
              }else{
                if(modeltype=="nlme.RI"){
                  res <- nlme(mymodel, random = ~ 1 | subject_inf, data=dataA,control=lmeControl(opt="optim"),correlation=corCompSymm(form=~1|subject_inf)) #,silent=TRUE)
                }
              }
            }
            
            
          }
  }else{
    
    if(modeltype=="lme.RI"){
      
      res <- lme(as.numeric(Response) ~ as.numeric(Factor1), random = ~ 1 | subject_inf, data=dataA,control=lmeControl(opt="optim"),correlation=corCompSymm(form=~1|subject_inf)) #,silent=TRUE)
    }else{
      if(modeltype=="lme.RIRS"){
        res <- try(lme(as.numeric(Response) ~ as.numeric(Factor1), random = ~ 1 + Factor1 | subject_inf, data=dataA,control=lmeControl(opt="optim"),correlation=corCompSymm(form=~1|subject_inf)),silent=TRUE)
      }else{
        if(modeltype=="nlme.RIRS"){
          res <- try(nlme(as.numeric(Response) ~ as.numeric(Factor1), random = ~ 1 + Factor1 | subject_inf, data=dataA,control=lmeControl(opt="optim"),correlation=corCompSymm(form=~1|subject_inf)),silent=TRUE)
        }else{
          if(modeltype=="nlme.RI"){
            res <- nlme(as.numeric(Response) ~ as.numeric(Factor1), random = ~ 1 | subject_inf, data=dataA,control=lmeControl(opt="optim"),correlation=corCompSymm(form=~1|subject_inf)) #,silent=TRUE)
          }
        }
      }
      
      
    }
    
    
  }
  if(is(res,"try-error")){
    
    return(NA)
  }else{
    s1<-summary(res)
    
    s2=s1$tTable[2,c("p-value","Value","Std.Error","t-value")]
    return(s2)
  }
  
}
