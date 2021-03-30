library(doSNOW)
library(foreach)

# funkcja dla odlegosci
getR<-function(j,i,k)
{
  r = sqrt((k-j)^2 + (1+i)^2)
  return(r)
}
# funkcja dla kata
getAngle<-function(j,i,k)
{
  if(k == j)
  {
    return(90.0)
  }
  if(k < j)
  {
    ii = j - k
    tg = i / ii
    rad = atan(tg)
    deg = (rad/pi)*180
    return (deg)
  }
  if(k > j)
  {
    ii = k - j
    tg = i / ii
    rad = atan(tg)
    deg = (rad/pi)*180
    return (deg)
  }
}

#wymiary
N=500
M=250 
#gestosci
rho1<-3500
rho2<-5500
#stala grawitacji
gamma<-6.67e-11
#macierz gestosci
rho<-matrix(nrow=M,ncol=N,rho1)
#generacja modelu
for(k in 1:3)
{
  x<-round(100+runif(1)*(N-200))
  z<-round(50+runif(1)*(M-100))
  for(i in 1:M)
  {
    for(j in 1:N)
    {
      if( sqrt((i-z)^2+(j-x)^2)<50)
        rho[i,j]<-rho2
    }
  }
}
# wyswietlenie modelu
image(t(apply(rho, 2, rev)), asp=0.5)
box()
# wektor na wyniki
g<-rep(0,N)


######################### WARIANT SZEREGOWY #########################
start <- Sys.time()
for(k in 100:400)
{
  suma<-0
  for(i in 1:M) 
  {
    for(j in 1:N)
    {
      beta = (i+1-i)/(j+1-j) 
      r = getR(j, i, k)
      r1 = getR(j, i+1, k)
      z = r1/r
      logarytm = log(z)
      a = getAngle(j, i, k)
      a1 = getAngle(j, i+1, k)
      suma <- suma + (rho[i,j] * beta * (logarytm - (a-a1)) )
    }
  }
  g[k] = 2 * gamma * suma;
}
stop <- Sys.time()

# czas wykonywania obliczen
print("wariant szeregowy: ")
stop - start
# filtracja zer z wektora g
g_filtered <- g[g > 0]
plot(g_filtered)
######################### ################# #########################



######################### WARIANT RÓWNOLEGY #########################

#stworzenie i zarejestrowanie klastra
cl<- makeCluster(3)
registerDoSNOW(cl)
#pomiar czasu
start<-Sys.time()

out<-foreach(k=seq(100,400,1)) %dopar%
  {
    suma<-0
    for(i in 1:M)
    {
      for(j in 1:N)
      {
        beta = (i+1-i)/(j+1-j)
        r = getR(j, i, k)
        r1 = getR(j, i+1, k)
        z = r1/r
        logarytm = log(z)
        a = getAngle(j, i, k)
        a1 = getAngle(j, i+1, k)
        suma <- suma + (rho[i,j] * beta * (logarytm - (a-a1)) )
        
      }
    }
    g <- 2 * gamma * suma
  }
#pomiar czasu
stop<-Sys.time()
#wypisanie roznicy czasu
print("wariant równolegly: ")
stop-start
#deklaracja macierzy
m<-matrix(nrow=10,ncol=1000,0)
#przypisanie do macierzy "rozwinietej listy"
m<-unlist(out)
#zwolnienie klastra
stopCluster(cl)
plot(m)
######################### ################# #########################