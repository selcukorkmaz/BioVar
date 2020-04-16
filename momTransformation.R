
momTransformation <- function(data = NULL, time = NULL, measure = NULL, method ="mom"){
    
    medianTime = aggregate(data[,measure], list(data[,time]), median, na.rm=TRUE)
    
    splitData <- split(data, data[,time])
    
    momData = list()
    
    
    for(t in 1:length(splitData)){
      
      tmp = splitData[[t]]
      
      if(method == "mom"){
      
        tmp[,measure] = tmp[,measure]/medianTime[[t,2]]
      
      }else{
        
        tmp[,measure] = log(tmp[,measure]/medianTime[[t,2]])
        
        
      }
      momData[[t]] = tmp
      
    }
    
    momTransformedData = do.call(rbind.data.frame, momData)
    
    return(momTransformedData)

}

