#install.packages (c("plyr","plotrix"))
library (plyr)
library (plotrix)

table = read.csv("da.csv", TRUE, ",")

type = "Pie" #Pie or BarPlot 
var = c("Sex.M.F","U.R") #Name/Names of the column/columns as presented in "names(table)"

a = count(table, var)

var = gsub(".", " ", var, fixed=TRUE)
i = nrow(a)
colors = rainbow(i)

slices = NULL
lbls = NULL
while (i > 0)
{
  slices = c(slices, a[i, ncol(a)])
  j = ncol(a) - 1
  k = 1
  tmp = NULL
  while (k <= j)
  {
    tmp = paste(tmp, substr(a[i,k], 1, 100),"\n")
    k = k + 1
  }
  lbls = c(lbls, tmp)
  i = i - 1
}

clbls = lbls
pct = round(slices / sum(slices) * 100)
lbls = paste(lbls, pct)
lbls = paste(lbls, "%", sep = "") 

ok = FALSE
k = 1
while( k <= j)
{
  if(grepl(var[k],"Age"))
    ok = TRUE
  k = k + 1
}

if(ok == TRUE && ncol(a) < 3 )
{
  valmin = 1
  valmax = 10
  p = 1
  year = NULL
  ye = 0
  
  while(p < nrow(a))
  {
    while(a[p,1] >= valmin && a[p,1] < valmax && p < nrow(a))
    {
      ye = ye + a[p,2]
      p = p + 1
    }
    valmin = valmax
    valmax = valmax + 10
    year = c(year, ye)
    ye = 0
  }
  
  maxim = max(year) + (10 - max(year) %% 10)
  name = paste(seq(0, valmax - 20, by = 10), "-", seq(9, valmax - 10, by = 10), "\nyears ")
  
  if(type == "Pie")
  {
    name = gsub("\n", " ", name, fixed=TRUE)
    pct = round(year / sum(year) * 100)
    name = paste(name, "\n=", pct)
    name = paste(name, "%", sep = "") 
    
    j = 1
    while (j < length(year)) 
    {
      if(year[j] == 0)
      {
        name = name[-j]
        year = year[-j]
        j = j - 1
      }
      j = j + 1
    }  
  
  pie(year, labels = name, col = rainbow(length(year)), main = "Age Range", radius = 1)
  }
  
  if(type == "BarPlot")
    barplot(year, main = "Age Range", xlim = c(0, maxim), horiz = TRUE, col = rainbow(length(year)), las = 1, names.arg = name, cex.names = 0.8)
}

if(ok == TRUE && ncol(a) > 2)
  print("Can't do that!")

if(type == "Pie" && ok != TRUE)
{
  if(nrow(a) <= 4)
    pie3D(slices, explode = 0, labels = lbls, theta = pi / 3, col = colors, main = var, radius = 1, start = 1.5, labelcex = 1)
  
  if(nrow(a) > 4)
    pie(slices, labels = lbls, col = colors, main = var, radius = 1)
}

if(type == "BarPlot" && ok != TRUE)
  barplot(slices, main = var, xlim = c(0, max(slices) + (10 - max(slices) %% 10)), col = colors, horiz = TRUE, las = 1, names.arg = clbls, cex.names = 0.8)
