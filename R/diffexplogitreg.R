#
diffexplogitreg <-
function(dataA){
  
  dataA<-as.data.frame(dataA)
  
  labels_1<-labels(as.factor(dataA$Factor1))
  
  
  dataA$Factor1<-replace(dataA$Factor1,which(dataA$Factor1==labels_1[1]),0)
  dataA$Factor1<-replace(dataA$Factor1,which(dataA$Factor1==labels_1[2]),1)
  
  a1 <- glm(dataA$Factor1 ~ .,family=binomial(logit),data=dataA) # aov(dataA$Response ~ .,data=dataA) # + chocolate$Factor1*chocolate$Factor2) 
  
  c1<-confint(a1,level=0.95)
  s1<-summary(a1)
  
  
  anova_res<-anova(a1)
  num_rows<-dim(anova_res)
 
  
  s1<-s1$coefficients
  s1<-s1[-c(1),]
  
  confint_lower<-s1[,1]-(1.96*s1[,2])
  confint_upper<-s1[,1]+(1.96*s1[,2])
  
  #print(anova_res)
  #
  return(list("mainpvalues"=s1[,4],"estimates"=s1[,1],"zstat"=s1[,3],"stderr"=s1[,2],"confint"=c(confint_lower,confint_upper)))
  
  
}
