'''faça uma simulação usando solow com prog. Técnico em excel ou R com os seguintes valores:
s=0.25, 
d=0.05, 
n=0.02, 
alfa da cobb douglas=0.4, 
progresso técnico=0.01

Simule um aumento naw taxa de poupança para 0.4. 
Plote gráficos de série de tempo para y, ln y e Y/AL (y til)
'''


s <- 0.25 # tx de poupança inicial
s2 <- 0.4 # tx de poupança final
d <- 0.05 # depreciação
n <- 0.02 # tx de cresc pop
alpha <- 0.4 # alpha da cobb douglas
g <- 0.01 # tx de progresso técnico

k <- rep(NA,500)
y <- rep(NA, 500)
A <- rep(NA,500)

A[1] <- 1
k[1] <- 1

# equação acima da 2.9 do Jones e Vollrath
y[1] <- k[1]^alpha*A[1]^(1-alpha)


for (t in 2:500){
  A[t] <- A[1]*exp(g*t) # equação anterior a 2.8
  y[t] <- A[t]*(s/(n+g+d))^(alpha/(1-alpha)) # equação 2.13
  k[t] <- s*y[t]-(n+g+d)*k[t-1] # eq 2.12
}

plot.ts(y)

# SIMULANDO COM MUDANÇA DE TX DE POUPANÇA

k1 <- rep(NA,500)
y1 <- rep(NA,500)
savings <- rep(s2,500)

savings[1:250] <- s # muda a taxa de poupança quando t = 250
k1[1] <- 1
y1[1] <- k1[1]^alpha*A[1]^(1-alpha)

for (t in 2:500){
  
  A[t] <- A[1]*exp(g*t) 
  y1[t] <- A[t]*(savings[t-1]/(d+n+g))^(alpha/(1-alpha)) 
  k1[t] <- savings[t-1]*y1[t]-(n+g+d)*k1[t-1] 
  
}

plot.ts(y1)

install.packages('tidyverse')
install.packages('reshape2')

library(tidyverse)
library(reshape2)


data.frame(t = 1:500, y1, y) %>% melt(id = "t") %>% 
  ggplot(aes(x = t, y = value, colour = variable)) +
  geom_line() +
  ylab("y") +
  xlab("tempo") +
  theme(legend.title = element_blank())

data.frame(t = 1:500, log(y1), log(y)) %>% melt(id = "t") %>% 
  ggplot(aes(x = t, y = value, colour = variable)) +
  geom_line() +
  ylab("y") +
  xlab("tempo") +
  theme(legend.title = element_blank())

ytil <- y/A
ytil1 <- y1/A

data.frame(t = 1:500, ytil1, ytil) %>% melt(id = "t") %>% 
  ggplot(aes(x = t, y = value, colour = variable)) +
  geom_line() +
  ylab("y") +
  xlab("tempo") +
  theme(legend.title = element_blank())