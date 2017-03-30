
Z<-read.csv("naive_bayes_binary.csv")

# Spliting data into training and testing data

train=sample(1:nrow(Z),nrow(Z)/2)  
test= -train
training_data=Z[train,]
testing_data=Z[test,]

p_1=vector(mode="integer",length = 10)
p_2=vector(mode="integer",length = 10)
p_3=vector(mode="integer",length = 10)

#Building the model by calulating the individual probabilities 
for(i in 1:3){
  temp_data=training_data[training_data$V11==i,]
  r=nrow(temp_data)
  for(j in 1:10){
    if(i==1){
      p_1[j]=sum(temp_data[,j])/r
    }else if(i==2){
      p_2[j]=sum(temp_data[,j])/r
    }else{
      p_3[j]=sum(temp_data[,j])/r
    }
  }
}

result=vector(mode="integer",length = nrow(testing_data))

for(i in 1:nrow(testing_data)){
  p1=p2=p3=1
  temp_data=testing_data[i,1:10]

  for(j in 1:3){
    
    if(j==1){
      for(z in 1:10){
        if(temp_data[,z]==1){
          p1=p1*p_1[z]
        }else{
          p1=p1*(1-p_1[z])
        }
      }
      
    }else if(j==2){
      for(z in 1:10){
        if(temp_data[,z]==1){
          p2=p2*p_2[z]
        }else{
          p2=p2*(1-p_2[z])
        }
      }
      
    }else{
      for(z in 1:10){
        if(temp_data[,z]==1){
          p3=p3*p_3[z]
        }else{
          p3=p3*(1-p_3[z])
        }
      }
    }
    
  }
  
  if(p1>p2 & p1>p3){
    result[i]=1
  }
  if(p2>p1 & p2>p3){
    result[i]=2
  }
  if(p3>p1 & p3>p2){
    result[i]=3
  }
}


table(result, testing_data$V11,dnn=c("Prediction","Actual"))



