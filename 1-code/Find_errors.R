######SCRIPT TO FIND OUTLIERS#####

# method based on boxplot outliers based on difference values from consecutive points in temporal series-----------------------------------

#' Function to detect potential outliers
#'
#' @param data data frame
#' @param y_var response variable
#' @param sd_tresh treshold of standar error 
#'
#' @return data frame of potential outliers
#' @export
#'
#' @examples
find_outliers=function(data,y_var,x_var,seq_var){
  # 
  # data=prodAll%>%filter(Site=='PR' & IdGenotype=='GE06')
  # y_var='BunchMass'
  # seq_var='HarvestDate'
  
  
  detect0=data%>%
    # mutate(Year=year(get(x_var)))%>%
    filter(!is.na(get(y_var)))%>%
    select(Project,Site,Progeny,Treatment,TreeNb,NbLeaf,y_var,seq_var,x_var)%>%
    group_by(Project,Site,Progeny,TreeNb)%>%
    arrange(Project,Site,Progeny,TreeNb,get(seq_var))%>%
    mutate(lag1=ifelse(test = abs(as.numeric(get(seq_var)-lag(get(seq_var))))>0,yes = abs(get(y_var)-lag(get(y_var)))/abs(as.numeric(get(seq_var)-lag(get(seq_var)))),no = NA),
           lag2=ifelse(test = abs(as.numeric(get(seq_var)-lag(get(seq_var),n=2)))>0,yes = abs(get(y_var)-lag(get(y_var),n=2))/abs(as.numeric(get(seq_var)-lag(get(seq_var),n=2))),no = NA),
           lead1=ifelse(test = abs(as.numeric(get(seq_var)-lead(get(seq_var))))>0,yes = abs(get(y_var)-lead(get(y_var)))/abs(as.numeric(get(seq_var)-lead(get(seq_var)))),no = NA),
           lead2=ifelse(test = abs(as.numeric(get(x_var)-lead(get(x_var),n=2)))>0,yes = abs(get(y_var)-lead(get(y_var),n=2))/abs(as.numeric(get(seq_var)-lead(get(seq_var),n=2))),no = NA)
    )%>%
    ungroup()
  
  
  detect=merge(x = detect0,y = detect0%>%
                 group_by(Site)%>%
                 summarize(diff_mean=mean(c(lag1,lag2),na.rm=T),
                           diff_sd=sd(c(lag1,lag2),na.rm=T),
                           inf=quantile(x = c(lag1,lag2),probs = 0.25,na.rm=T)-1.5*IQR(c(lag1,lag2),na.rm=T),
                           sup=quantile(x = c(lag1,lag2),probs = 0.75,na.rm=T)+1.5*IQR(c(lag1,lag2),na.rm=T))
               ,all.x=T,all.y=F)%>%
    ungroup()%>%
  mutate(lag1_std=ifelse(lag1>sup | lag1<inf,1,0),
         lag2_std=ifelse(lag2>sup | lag2<inf,1,0),
         lead1_std=ifelse(lead1>sup | lead1<inf,1,0),
         lead2_std=ifelse(lead2>sup | lead2<inf,1,0))%>%
  group_by(Project,Site,Progeny,TreeNb,NbLeaf)%>%
  mutate(non_na_count =sum(!is.na(c(lag1_std,lag2_std,lead1_std,lead2_std)) & c(lag1_std,lag2_std,lead1_std,lead2_std)==1))%>%
    ungroup()%>%
    filter(non_na_count>1)%>%
    mutate(variable=y_var,
           type='outlier',
           value=get(y_var))%>%
    mutate(inf=quantile(x = value,probs = 0.25,na.rm=T)-1.5*IQR(value,na.rm=T), ##second 
           sup=quantile(x = value,probs = 0.75,na.rm=T)+1.5*IQR(value,na.rm=T))%>%
    filter(value>sup | value<inf)%>%
    ungroup()%>%
    select(Project,Site,Progeny,Treatment,TreeNb,NbLeaf,y_var,x_var,seq_var,variable,type,value)
  
  # colnames(detect)[colnames(detect)==x_var]='ObservationDate'
  print(paste(y_var,'----> Number of errors:',nrow(detect)))
  
  
  return(detect)
}


#' Get outlier form boxplot calculation on production
#'
#' @param data data fram
#' @param y_var response to get outliers
#'
#' @return data frame of outliers
#' @export
#'
#' @examples
find_outliers_prod=function(data,y_var,x_var){

  # data=prodAll
  # x_var='HarvestDate'
  # y_var='BunchMass'

  ##method based on boxplot outlier
  detect=data%>%
    filter(!is.na(get(y_var)))%>%
    select(Progeny,Treatment,TreeNb,NbLeaf,y_var,x_var)%>%
    group_by(Treatment)%>%
    mutate(inf=quantile(x = get(y_var),probs = 0.25,na.rm=T)-1.5*IQR(get(y_var),na.rm=T),
           sup=quantile(x = get(y_var),probs = 0.75,na.rm=T)+1.5*IQR(get(y_var),na.rm=T))%>%
    filter(get(y_var)>sup | get(y_var)<inf)%>%
    mutate(variable=y_var,
           type='outlier',
           value=get(y_var))%>%
    ungroup()%>%
    mutate(inf=quantile(x = value,probs = 0.25,na.rm=T)-1.5*IQR(value,na.rm=T), ##second 
           sup=quantile(x = value,probs = 0.75,na.rm=T)+1.5*IQR(value,na.rm=T))%>%
    filter(value>sup | value<inf)%>%
    select(Progeny,Treatment,TreeNb,NbLeaf,variable,type,value)
  colnames(detect)[colnames(detect)==x_var]='ObservationDate'
  print(paste(y_var,'----> Number of errors:',nrow(detect)))

  
  
  # ggplot()+
  #   geom_point(data=detect,aes(x=ObservationDate,y=value))
  # 
  # ggplot(data=detect,aes(y=value))+
  #   geom_boxplot()
  
  
  
  
  return(detect)
}
#' 
#' 
#' 
#' 
#' # method based on deviation from spline -----------------------------------
#' 
#' #' Function to detect potential outliers
#' #'
#' #' @param data data frame
#' #' @param x_var temporal variable (Date or MAP)
#' #' @param y_var response variable
#' #' @param sd_tresh treshold of standar error from spline model above which outlier are considered
#' #'
#' #' @return data frame of potential outliers
#' #' @export
#' #'
#' #' @examples
#' find_outliers2=function(data,x_var,y_var,sd_tresh=2){
#'   
#'   # data=LeafAll
#'   # sd_tresh=3
#'   # x_var='ObservationDate'
#'   # y_var='RachisLength'
#'   
#'   
#'   
#'   detect0=data%>%
#'     mutate(Year=year(ObservationDate))%>%
#'     filter(!is.na(get(y_var)) & !is.na(get(x_var)))%>%
#'     select(Site,Year,IdGenotype,TreeId,Plot,BlockNumber,LineNumber,TreeNumber,PhytomerNumber,y_var,ObservationDate)%>%
#'     group_by(TreeId)%>%
#'     mutate(time=seq(1,n()),
#'            nobs=n())%>%
#'     filter(nobs>3)%>%
#'     mutate(y_predict=smooth.spline(x = time,y = get(y_var),df = 2)$y,
#'            resid=get(y_var)-y_predict)%>%
#'     ungroup()
#'   
#'   detect=merge(detect0%>%
#'                  group_by(Site)%>%
#'                  summarize(resid_sd=sd(resid)),detect0)%>%
#'     ungroup()%>%
#'     mutate(residual_std=resid/resid_sd)%>%
#'     filter(abs(residual_std)>sd_tresh)%>%
#'     mutate(variable=y_var,
#'            type='outlier',
#'            value=get(y_var))%>%
#'     select(Site,IdGenotype,Plot,BlockNumber,LineNumber,TreeNumber,ObservationDate,PhytomerNumber,variable,type,value)
#'   
#'   print(paste(y_var,'----> Number of errors:',nrow(detect)))
#'   
#'   
#'   return(detect)
#' }
