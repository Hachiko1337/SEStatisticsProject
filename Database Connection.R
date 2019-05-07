#install.packages (c("plyr","plotrix","jsonlite"))
library (plyr)
library (plotrix)
library (jsonlite)

input = "https://ro-medical-app.herokuapp.com/api/doctors/list"

df = fromJSON(input, flatten = TRUE)
listcols = colnames(df[lapply(colnames(df), function(x) class(df[,x])) == "list"])
for (col in listcols)
{
  df[col] = data.frame(unlist(lapply(df[,col], function(x) toString(unlist(x)))))
}
table = df


setwd("C:\\Users\\Valentin\\Documents\\IP")

file = "pacients-ours.csv"
#table = read.csv(file, TRUE, ",")

type = "BarPlot" #Pie or BarPlot 

contor = length(names(table))
tip = 1
while(tip <= 2)
{
  if(tip == 1)
    type = "Pie"
  if(tip == 2)
    type = "BarPlot"
  size = 2
  while(size <= length(table))
  {
    if(size == 2)
      var = c("Age","Sex.M.F")
    if(size >= 3)
      var = colnames(table[size])
    
    t = 1
    ok2 = TRUE
    while (t <= contor)
    {
      if (var == names (table[t]))
        ok2 = FALSE
      t = t + 1
    }
    
    w = FALSE
    k = 1
    while( k <= length(var))
    {
      if (grepl (var[k], "BestDoctor"))
        w = TRUE
      k = k + 1
    }
    if(file != "doctors.csv")
      w = FALSE
    
    if (ok2 == FALSE || w == TRUE)
    {
      if (w == TRUE)
      {
        a = count (table[9])
        var = gsub (".", " ", var, fixed = TRUE)
        copie = nrow (a)
        maxim = a [copie, 1]
        q = 1
        
        while (table[q, 9] != maxim)
          q = q + 1
        
        nume = table[q,2]
        nume = paste(nume, table[q, 3], sep = "")
        z=paste(var,"_",type,".png",sep="")
        png(filename=z ) 
        par(mar=c(5,6,4,1)+.1)
        pie (maxim, labels = maxim, col = "blue", main = var, radius = 1, xlab = nume, ylab = table[q, 6])
        dev.off()
      }
      if (w != TRUE)
      {
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
            tmp = paste(tmp, substr(a[i,k], 1, 100))
            k = k + 1
          }
          lbls = c(lbls, tmp)
          i = i - 1
        }
        
        clbls = lbls
        pct = round(slices / sum(slices) * 100)
        lbls = paste(lbls, pct, "%")
        #lbls = paste(lbls, "%", sep = "") 
        
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
            z=paste(var,"_",type,".png",sep="")
            png(filename=z )
            par(mar=c(5,6,4,1)+.1)
            pie(year, labels = name, col = rainbow(length(year)), main = "Age Range", radius = 1)
            dev.off()
          }
          
          if(type == "BarPlot")
          {
            z=paste(var,"_",type,".png",sep="")
            png(filename=z ) 
            par(mar=c(5,6,4,1)+.1)
            barplot(year, main = "Age Range", xlim = c(0, maxim), horiz = TRUE, col = rainbow(length(year)), las = 1, names.arg = name, cex.names = 0.8)
            dev.off()
          }
        }
        
        if(type == "Pie" && ok != TRUE)
        {
          if(nrow(a) <= 4)
          {
            z=paste(var,"_",type,".png",sep="")
            png(filename=z ) 
            par(mar=c(5,6,4,1)+.1)
            pie3D(slices, explode = 0, labels = lbls, theta = pi / 3, col = colors, main = var, radius = 1, start = 1.5, labelcex = 1)
            dev.off()
          }
    
          if(nrow(a) > 4)
          {
            z=paste(var,"_",type,".png",sep="")
            png(filename=z ) 
            pie(slices, labels = lbls, col = colors, main = var, radius = 1)
            dev.off()
          }
        }
        
        if(type == "BarPlot" && ok != TRUE)
        {
          z=paste(var,"_",type,".png",sep="")
          png(filename=z ) 
          par(mar=c(5,6,4,1)+.1)
          barplot(slices, main = var, xlim = c(0, max(slices) + (10 - max(slices) %% 10)), col = colors, horiz = TRUE, las = 1, names.arg = clbls, cex.names = 0.8)
          dev.off()
        }
        
        if(ok == TRUE && ncol(a) > 2)
          print("Age must be called alone!")
      }
      
      if(type != "Pie" && type != "BarPlot")
        print("Unknown Graphic Function!")
    }
    
    if(ok2 == TRUE && w != TRUE)
      print("Wrong Column. Please Insert a correct one!")
    
    size = size + 1
  }
  
  tip = tip + 1
}