pollutantmean <-function(directory, pollutant, id=1:332){
  tmean=NULL
  accesstt<-NULL
  for(i in id){
    fname<-sprintf("%03d.csv",i)
    fname<-paste(directory,fname,sep = "/")
    pollfile<-read.csv(fname)
    
    access<-pollfile[,pollutant]
    #if (pollutant=="sulfate"){
    #  access<-pollfile$sulfate
    #}
    accesstt<-append(accesstt,access)
  }
  print(mean(accesstt,na.rm = T))
}

complete <-function(directory, id=1:332){
  filenames<-list.files(directory, full.names=F)
  filenumbers<-as.numeric(sub("\\.csv$","",filenames))
  filenames<-na.omit(filenames[match(id, filenumbers)])
  listofstations<-lapply(file.path(directory,filenames), read.csv)
  num<-0
  id<-NULL
  nobs<-NULL
  for (i in listofstations){
    num<-num+1
    cc<-complete.cases(i)
    dd<-sum(cc)
    id<-append(id,num)
    nobs<-append(nobs,dd)
  }
  df<-data.frame(id,nobs)
  print(df)
}


corr <-function(directory, threshold=0){
  filenames<-list.files(directory,full.names=T)
  listofstations<-lapply(filenames, read.csv)
  vec=NULL
  for (i in listofstations){
    if (sum(complete.cases(i))>threshold){
      comp<-i[complete.cases(i),]
      vec<-append(vec,cor(comp$nitrate,comp$sulfate))
    }
  }
  vec
}