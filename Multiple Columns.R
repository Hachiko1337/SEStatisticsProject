#install.packages ("tidyverse")
library(tidyverse)

w = read.csv("da.csv", TRUE, ",")
type = "BarPlot" #Pie or BarPlot
t="Female" #chose characteristics of first column
x=w$Sex.M.F==t #chose first column
tt="Rural" #chose characteristics of second column
y=w$U.R==tt    #chose second column
i=length(x)
contor=0
while(i>=1)
{
  if (x[i]==TRUE & y[i]==TRUE)
      contor=contor+1
  
  i=i-1
}
contor
rest=sum(x==TRUE)-contor
rest
vector=c(contor,rest)
lbls=c("Rural",tt)


if(type == "Pie")
{
  pie3D(vector, explode = 0, labels = lbls, theta = pi / 3, col=c("chartreuse", "blue4"), main = t, radius = 1, start = 2)
}
if(type == "BarPlot")
{maxim = max(vector) + (10 - max(vector) %% 10)
barplot(vector, main = t, xlim = c(0, maxim), horiz = TRUE, col=c("chartreuse", "blue4"),names.arg=c("Urban","Rural"),cex.names=0.8)
}
