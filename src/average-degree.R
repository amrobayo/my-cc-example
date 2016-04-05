setwd("/Users/Antonio/Desktop/coding-challenge-master/data-gen")

library(rjson)
library(streamR)


x1= parseTweets("tweets.txt")

##since all the other columns are useless, I clean up the data frame with the following
##two lines of code
x1=x1[, -(2:8)]
x1= x1[,-(3:ncol(x1))]

ht ="#[A-Za-z0-9]+"

t1 = as.numeric(strptime(x1$created_at, format="%a %b %d %H:%M:%S %z %Y"))
# we use the following format="%a %b %d %H:%M:%S %z %Y" since it is the
#format that twitter follows in its 'created_at' field. 
# %a-Abbreviated weekday name in the current locale on this platform
# %b-Abbreviated month name in the current locale on this platform.
# %d-Day of the month as decimal number (01–31).
# %H-Hours as decimal number (00–23)
# %M-Minute as decimal number (00–59)
# %S-Second as integer (00–61), allowing for up to two leap-seconds (but POSIX-compliant implementations will ignore leap seconds)
# %z-Signed offset in hours and minutes from UTC, so -0800 is 8 hours behind UTC. 
# %Y-Year with century



##internally, time is stored as the number of seconds since 01/01/1970
##thus the time frame that tweets can come in is established by
##adding 60 to base
base=as.numeric(t1[1]) + 60
min= as.numeric(t1[1])
timestamps = numeric(0)
tcount = 2
avgs= numeric(0)
lengths = numeric(0)
hashtags = character(0)
counter=1
prev = 0

degree = function(y){
  sum=0
  if(length(y)>0){
    div = length(table(y))
    for(i in 1:div){
      sum = sum + table(y)[[i]]
    }
    ans = (sum/div)
    ans =as.numeric(format(round(ans, 2), nsmall = 2))
  }
  else{
    ans =0
  }
  return(ans)
}


evict = function(){
  if(!(is.na(lengths[counter])))
  {
  for(i in 1:lengths[counter]){
   hashtags<<- hashtags[-1]
    
  }
  counter<<- counter+1
  }
}


measure_length = function(n){
  result = n*(n-1)
  lengths <<- c(lengths, result)
  return(result)
  
}




for(i in 1:nrow(x1)){
  cat("Looking at tweet", i, "\n")
  
  m= gregexpr(ht, x1$text[i])
  t= regmatches(x1$text[i],m)
  
  if(t1[i]<min){
    avgs = c(avgs, prev)
  }
  
  else if(t1[i]<=base & length(t[[1]])<=1){
    avgs = c(avgs, prev)
  }
  
  else if(t1[i]<=base & length(t[[1]])>1){
    l = length(t[[1]])
        for (j in 1:l) {
          hashtags = c(hashtags, rep(t[[1]][j], times=l-1))
         
        }
        avgs = c(avgs, degree(hashtags))
        timestamps=c(timestamps, t1[i])
        prev= degree(hashtags)
        measure_length(l)
      }
  else if (t1[i]>base & length(t[[1]])<=1){
    avgs= c(avgs, prev)
    evict()
    
  }
  
  else if(t1[i]> base &length(t[[1]])>1){ 
    
    evict()
    
    l = length(t[[1]])
    for (j in 1:l) {
      hashtags = c(hashtags, rep(t[[1]][j], times=l-1))
    }
    avgs = c(avgs, degree(hashtags))
    measure_length(l)
    prev= degree(hashtags)
    
    min = timestamps[tcount]
    base = timestamps[tcount]+60
    tcount = tcount +1
    }
}
  
  


setwd("/Users/Antonio/Desktop/coding-challenge-master/tweet_output")

write(x=paste(avgs, "\n", sep=""),
      file="output.txt", append=TRUE)

# running test 
setwd("/Users/Antonio/Desktop/coding-challenge-master/insight_testsuite/temp/tweet_input")


x2 = parseTweets("tweets.txt")


for(i in 1:nrow(x2)){
  cat("Looking at tweet", i, "\n")
  
  m= gregexpr(ht, x1$text[i])
  t= regmatches(x1$text[i],m)
  
  if(t1[i]<min){
    avgs = c(avgs, prev)
  }
  
  else if(t1[i]<=base & length(t[[1]])<=1){
    avgs = c(avgs, prev)
  }
  
  else if(t1[i]<=base & length(t[[1]])>1){
    l = length(t[[1]])
    for (j in 1:l) {
      hashtags = c(hashtags, rep(t[[1]][j], times=l-1))
      
    }
    avgs = c(avgs, degree(hashtags))
    timestamps=c(timestamps, t1[i])
    prev= degree(hashtags)
    measure_length(l)
  }
  else if (t1[i]>base & length(t[[1]])<=1){
    avgs= c(avgs, prev)
    evict()
    
  }
  
  else if(t1[i]> base &length(t[[1]])>1){ 
    
    evict()
    
    l = length(t[[1]])
    for (j in 1:l) {
      hashtags = c(hashtags, rep(t[[1]][j], times=l-1))
    }
    avgs = c(avgs, degree(hashtags))
    measure_length(l)
    prev= degree(hashtags)
    
    min = timestamps[tcount]
    base = timestamps[tcount]+60
    tcount = tcount +1
  }
}


setwd("/Users/Antonio/Desktop/coding-challenge-master/insight_testsuite/temp/tweet_output")

write(x=paste(avgs, "\n", sep=""),
      file="output.txt", append=TRUE)