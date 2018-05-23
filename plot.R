reza_reps <- function(speed, cars, reps){
reza = function(speed,cars){
#s = speed limit
#n = number of cars on highway
d = sample(x = c(1:speed),size = cars,replace = TRUE)
for(i in 2:length(d)){
if(
d[i] > d[i-1]
){
d[i] <- d[i-1]
}
}
return(length(unique(d)))
}
return(replicate(reps, reza(speed,cars)))
}

plot(colMeans(apply(X = matrix(c(2:100)),MARGIN = 1, FUN = reza_reps, speed = 100000, reps = 100)), type = 'l') 
lines(colMeans(apply(X = matrix(c(2:100)),MARGIN = 1, FUN = reza_reps, speed = 10000, reps = 100))) 
lines(colMeans(apply(X = matrix(c(2:100)),MARGIN = 1, FUN = reza_reps, speed = 1000, reps = 100))) 
lines(colMeans(apply(X = matrix(c(2:100)),MARGIN = 1, FUN = reza_reps, speed = 100, reps = 100))) 
lines(colMeans(apply(X = matrix(c(2:100)),MARGIN = 1, FUN = reza_reps, speed = 50, reps = 100))) 
lines(colMeans(apply(X = matrix(c(2:100)),MARGIN = 1, FUN = reza_reps, speed = 40, reps = 100))) 
lines(colMeans(apply(X = matrix(c(2:100)),MARGIN = 1, FUN = reza_reps, speed = 30, reps = 100))) 
lines(colMeans(apply(X = matrix(c(2:100)),MARGIN = 1, FUN = reza_reps, speed = 20, reps = 100))) 
lines(colMeans(apply(X = matrix(c(2:100)),MARGIN = 1, FUN = reza_reps, speed = 10, reps = 100)))
lines(colMeans(apply(X = matrix(c(2:100)),MARGIN = 1, FUN = reza_reps, speed = 10, reps = 100)))
lines(colMeans(apply(X = matrix(c(2:100)),MARGIN = 1, FUN = reza_reps, speed = 5, reps = 100)))
lines(colMeans(apply(X = matrix(c(2:100)),MARGIN = 1, FUN = reza_reps, speed = 4, reps = 100)))
lines(colMeans(apply(X = matrix(c(2:100)),MARGIN = 1, FUN = reza_reps, speed = 3, reps = 100)))
lines(colMeans(apply(X = matrix(c(2:100)),MARGIN = 1, FUN = reza_reps, speed = 2, reps = 100)))
lines(colMeans(apply(X = matrix(c(2:100)),MARGIN = 1, FUN = reza_reps, speed = 1, reps = 100)))
lines(cumsum(1/c(1:10000)), col = 'red')

