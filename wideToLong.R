# data = read.table("~/Documents/GitHub/BV/data/RBC_data.txt", header = T, sep = "\t")
# head(data)
# 
# subject = "subject"
# timeRange = c("t1","t2","t3","t4","t5","t6","t7","t8","t9","t10")
# replicate = "replicate"
# analyte = "RBC"
# gender = "gender"
# time = "time"
# 
# 
# data = wideToLong(data, subject, gender, timeRange, replicate, analyte)
# head(data)
# dim(data)




wideToLong <- function(data, subject, gender, timeRange, replicate, analyte){

        
        splitSubject = split(data, data[,subject])
        
        splitList = lapply(splitSubject, FUN = function(x){
          
          x[,colSums(is.na(x))==0]
          
        })
        
        
        data = plyr::rbind.fill(lapply(splitList, as.data.frame))
        
        
        dataFull = reshape(data, varying = timeRange, direction = "long", sep = "")
        dataFull$'replicate.subject' = as.numeric(paste0(dataFull[,replicate], dataFull[,subject]))
         names(dataFull)[6] = analyte
        dataFull = dataFull[complete.cases(dataFull),]
        table(dataFull[,analyte] <=0)
        dataFull[dataFull[,analyte] <=0, analyte] = NA
        
        dataFull = dataFull[complete.cases(dataFull),]
        head(dataFull)
        dim(dataFull)
        
        tbl = table(dataFull[,subject])
        
        naSubject = tbl[!(tbl%%2==0)]
        naSubject2 = as.numeric(names(naSubject))
        
        if(length(naSubject2) >0){
          for(i in 1:length(naSubject2)){
            
            nas3 = dataFull[dataFull[,subject] == naSubject2[i],]
            
            tbl2 = table(nas3[,subject])
            
            nas4 = tbl2[!(tbl2%%2==0)]
            nas5 = as.numeric(names(nas4))
            
            for(j in 1:length(nas5)){
              
              dataFull[dataFull[,subject] == naSubject2[i] & dataFull[,time] == nas5[j], analyte] = NA
              
              
            }
            
          }
          
        }
        
        dataFull = dataFull[complete.cases(dataFull),]
        dataLast = dataFull[,c(subject, gender, "time", replicate, analyte)]
        
        return(dataLast)
        
        }
        