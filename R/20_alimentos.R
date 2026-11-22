airquality
calc_stats <- function(x) {
  n    <- length(x)                      # tamanho ORIGINAL do vetor (com NA)
  xbar <- mean(x, na.rm = TRUE)
  Md   <- median(x, na.rm = TRUE)
  S2   <- sum((x - xbar)^2, na.rm = TRUE) / (n - 1)
  DMA  <- sum(abs(x - xbar), na.rm = TRUE) / (n - 1)
  S    <- sqrt(S2)
  AS2  <- 3 * (xbar - Md) / S
  list(S2 = S2, DMA = DMA, AS2 = AS2)
}

calc_stats(airquality$Temp)
m<- airquality$Ozone[airquality$Month==9]

calc_stats(m)

calc_stats(airquality$Ozone)



vector<- c(1,2,3,4,5)
x<- diff(vector)
x

vector[-1]
vector[-length(vector)]


x<- numeric(20)
x[1]<-2 
x[2]<-3

for(i in 3:20){
  x[i] <- x[i-1] + x[i-2] 
}
x
x[20]

x[-1]
x[-length(x)]

r<- x[-1]/x[-length(x)]
r
a<- abs(diff(r))
which(abs(diff(r))< 1e-7)[1]
a
a[16]
#r <- x[-1]/x[-length(x)]

#dif <- abs(diff(r))

#which(dif < 1e-7)[1]
#dif[16]

x<-numeric (20)
x[1]<-1
x[2]<-1
x[3]<-1
x[4]<-1

for(i in 5:20) {
  x[i]<- x[i-1] + x[i-2] + x[i-3] + x[i-4]
  
}
x
x[18]

r <- x[-1]/x[-length(x)]
r


dif <- abs(diff(r))[5:length(r)]
dif

k <- which(dif < 1e-6)[1]
k          # 16
k + 6 



x <- c(1, 2)
n <- 2
while (x[n] <= 10000) {
  n <- n + 1
  x[n] <- x[n - 1] + x[n - 2]/2
}
n


x<- numeric(100)
x[1]<- 1
x[2]<- 2
n<-2
while(x[n]<=10000){
  n<- n+1
  x[n]<- x[n-1]+(x[n-2]/2)
  print(x[n])
  
}
n


x<- c(1,2)
n<-2
for(i in 3:100){
  n<- n+1
  x[i]<- x[i-1]+(x[i-2]/2)
  print(x[i])
  if(x[i]>10000 ){
    x[i]
    break
    }
  
}

n
x[n]

a<-c(1:13)
a
b<-c(1:13)
b  
c<-c(1:13)
c
d<-c(1:13)
d  


m<-c(a,b,c,d)
m

m<- rep(1:13, times=4)
m
x<-numeric(2)
b<-numeric(1)
n<- 0
while(!b){
  n<-n+1
  x<- sample(m,2)
  b<- (x[1]==x[2])
  
}
x[1]

x
n

baraja<- rep(c("A",2:10,"J","Q","K"), times=4 )
baraja

w<- c()

set.seed(123)

m <- rep(c("A", 2:10, "J", "Q", "K"), times = 4)   # baralho de 52 cartas

n_rep <- 10000
tentativas <- numeric(n_rep)      # vetor para guardar os 10.000 resultados

for (i in 1:n_rep) {
  n <- 0                          # zera o contador a cada experimento
  repeat {
    n <- n + 1
    x <- sample(m, 2)             # duas cartas sem reposição
    if (x[1] == x[2]) break       # saiu par: termina este experimento
  }
  tentativas[i] <- n              # guarda quantas tentativas foram necessárias
}

head(tentativas)                  # os primeiros resultados
length(tentativas)                # 10000
mean(tentativas)     


r<- function (simula_moeda(), k, prob_cara=0,5, exibir=FALSE)
k<- c(2,3,4)
dado<- c("k","c")
numero<- 10
ten<- numeric (numero)
for(i in 1:3){
  k[i]
  print(k)
  for(i in 1:numero){
    n<- 0
    c<-0
    repeat{
      n<-n+1
      l<-sample(dado, 1)
      c<-c+1
      if(l[1]=="C"){
        c<- c+1
      }else{ c<-0}
      if(c==3)break
    }
  }
  
}
n
dado



k<- c(2,3,4)
dado<- c("k","c")
numero<- 10
k
ten<- numeric (numero)
for(i in 1:3){
  a<-k[i]
  for(i in 1:numero){
    n<- 0
    c<-0
    repeat{
      n<-n+1
      l<-sample(dado, 1)
      if(l[1]=="c")break   
    }
  }
}

