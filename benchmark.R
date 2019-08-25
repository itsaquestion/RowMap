df = data.frame(a = c(1:3),b = c(2:4))
rowMap2 = function(x, .f, col_names = NULL, ...) {
  #NullCheck::stopNull(except ="col_names")
  tmp = vector("list", length = nrow(x))
  for (i in 1:nrow(x)) {
    tmp[[i]] = .f(x[i,])
  }
  ret = Reduce(rbind, tmp)
  if (!is.null(col_names)) { colnames(ret) = col_names }
  ret
}


n = 5000

df = data.frame(a = sample(0:9,n,replace = T),
                b = sample(0:9,n,replace = T),
                c = sample(0:9,n,replace = T))
library(xts)
df_xts = as.xts(df,order.by = as.Date(1:nrow(df)))

fun = function(y){
  data.frame(x=y$a,z=y$a+y$b)
}

f_dt = function(df){
  ret = rowMap(df,fun)
}

f_mu= function(df){
  ret2 = rowMap2(df,fun)
}

#f_ap(df)

library(microbenchmark)
mbm = microbenchmark(
  dt = f_dt(df),
  mu = f_mu(df),
  times = 10

)
mbm

