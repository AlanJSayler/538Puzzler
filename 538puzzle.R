#Puzzle from here: http://fivethirtyeight.com/features/what-if-robots-cut-your-pizza/
#My method: I think of the points around the circumference as a line between 0 and 1
#2 lines (enumerated by a pair of points on the "circumference") cross on the pizza
#iff the points are interleaved (i.e. exactly one point of pair a is between the points of b
#and vice versa). I generate a large number of "pizzas" and "cuts" (or pairs of point), divide
#them into groups of three cuts, and count the crossings. The number of slices of pizza is the 
#number of crossings +4


#count crosses in a dataframe of pairs of values
count_crosses = function(a){
  sum = 0
  for(i in 1:(nrow(a)-1)){
    for(j in (i+1):nrow(a)){
      sum = sum + crosses(a[i,],a[j,])
    }
  }
  return(sum)
}


#accepts a pair of values for both a and b, returns 1 iff the points of a are interleaved with the points of b
crosses = function(a,b){
  if(between(a,b[1]) + between(a,b[2]) == 1){
    return(1)
  }
  return(0)
}

#accepts a pair of a values and a b value, it returns 1 iff b is between the two elements of a and 0 otherwise
between = function(a,b){
  if(b> min(a) && b<max(a)){
    return(1)
  }
  return(0)
}


results = replicate(10000, count_crosses(data.frame(replicate(2,runif(3)))))
print(results)
#weird! I guess there is an average of 1 crossing of the cuts, to make an average of
#5 slices! Very fun puzzle!
  

