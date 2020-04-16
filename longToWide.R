data = read.table("~/Documents/GitHub/BV/data/example_data.txt", header = T, sep = "\t")
head(data)

splitData = split(data,data$Subject)
splitList = list()

for(i in 1:length(splitData)){
  
  tmp = splitData[[i]]
  
  splitTmp = split(tmp, tmp$Time)
  splitTmpList=list()
  
  for(j in 1:length(splitTmp)){
    
    stmp = splitTmp[[j]]
    
    s = stmp$Subject[[1]]
    g = stmp$Gender[[1]]
    t = stmp$Time[[1]]
    r1 =stmp$Measurement[[1]]
    r2 =stmp$Measurement[[2]]
    
    splitTmpList[[j]] = cbind.data.frame(r1,r2)
  }
  
  tmpList = do.call(rbind.data.frame, splitTmpList)
  splitList[[i]] = tmpList
}

dataNew = do.call(cbind.data.frame, splitList)
head(dataNew)

write.table(dataNew, "~/Documents/GitHub/BV/data/testData.txt", quote = F, row.names = F, sep = "\t")
