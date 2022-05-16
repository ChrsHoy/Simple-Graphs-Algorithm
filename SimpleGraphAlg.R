
{
  library(pracma)
  library(tidyverse)
  
  
  alph=5
  adjM<-matrix(0,nrow=alph,ncol=alph,byrow=FALSE)

  for(i in 2:alph)
  {
    q<-(round(rand(i,1),0))

    #print (q)
    for(j in 2:i)
    {
    adjM[j-1,i]<-q[j,1]
    adjM[i,j-1]=adjM[j-1,i]
    }
    #print(adjM)
  }
  adjS1<-c()

  
  for (rows in 1:(nrow(adjM)-1)) {
    for ( columns in (rows+1):ncol(adjM)) {
      adjS1<-append(adjS1,adjM[rows,columns])
      #Length buffering
    }  }

    print ((length(adjS1)))
  adjS2<-c()

  for(i in 1:(length(adjS1)))
    {adjS2[i]=adjS1[length(adjS1)-i+1]}

  

print(adjM)

print(adjS1)
print(adjS2)
alphSeq1<-round(rand(1,alph),1)*10
alphSeq2<-round(rand(1,alph),1)*10

alph_df<-data.frame(alphSeq1,alphSeq2)
alphCoord <- rbind(alphSeq1,alphSeq2)

plot(alphSeq1,alphSeq2)

m=(length(adjS1)+1)
aCounter =0
cCounter=0

for(i in 1:(nrow(adjM)-1)){
  bCounter=2
  bCounter=bCounter+cCounter
  for(j in bCounter:(ncol(adjM)))
  {
    aCounter=aCounter+1
    if((adjM[j,i]&adjM[i,j])==1)
      {
      segments(alphSeq1[i],alphSeq2[i],alphSeq1[j],alphSeq2[j])
      }
    }
    
  }

  cCounter=cCounter+1
  }

  #print(cCounter)



