library(animation)
library(plotrix)

drawImage <- function(a, center_x, center_y, radius) 
{  
  x=1:nrow(a); y=x;
  #x=1:200;y=x;
  A=outer(x,y,function(x,y) sqrt(((x-center_x)^2)+((y-center_y)^2)))
  
  a<-ifelse((A<radius | a == 4),4,1.15)
  
  return(a)
  
}  # end drawImage

centers_x <- c (0,0,0)
centers_y <- c (0,0,0)

# do pomiary czasu
start <- Sys.time()

N = 200   # wymiar
h <- 5    # krok

# macierz dla kroku nastepnego i poprzedniego
Lnew <- matrix(nrow = N, ncol = N, 0)
L <- matrix(nrow = N, ncol = N, 0)

# model alfa
a <- matrix(0, 200, 200)
for(i in 1:3) 
{
  center_x <- round(runif(1, min=50, max=150))
  centers_x[i] <- center_x
  center_y <- round(runif(1, min=50, max=150))
  centers_y[i] <- center_y
  a <- drawImage(a, center_x, center_y, 50)
}

image(a)

# wartosc max(a) dla stabilnosci
a_max <- max(a)

# krok czasowy
dt <- h^2/(4*a_max)
t <- 0

# warunki brzegowe
L[,1] <- rep(0,N)
L[,N] <- rep(0, N)
L[1,] <- rep(0, N)
L[N,] <- rep(50, N)
Lnew <- L

# ilosc iteracji
niter <- 30000

# pasek postepu
prog_bar <- txtProgressBar(min = 0, max = niter, style = 3)

saveGIF({
  # pasek postepu
  stepi <- (-1)
  
  for (k in 1:niter) {
    t <- t + dt
    # pasek postêpu
    stepi <- stepi + 1
    setTxtProgressBar(prog_bar, stepi)
    
    # petla po wierszach i kolumnach
    for (i in 2:(N - 1)) {
      for (j in 2:(N - 1)) {
        Lnew[i, j] <- (1 - (4 * dt * a[i, j]) / (h ^ 2)) * L[i, j] + dt * a[i, j] *
          ((L[i - 1, j] + L[i+1, j] + L[i, j - 1] + L[i, j + 1]) / (h ^ 2))
      }
    }
    
    # gradient 0
    Lnew[, 1] <- L[, 2]
    Lnew[, N] <- L[, N - 1]
    
    # zapis co 100
    if (k %% 100 == 0) {
      Limg <- apply(L, 2, rev)
      image(t(Limg))
      text(0.2, 0.9, k)
      
      #dodanie okregow na model
      for (l in 1:3){ draw.ellipse(1-centers_x[l]/200, (centers_y[l]/200), a = 0.25, b = 0.25, nv = 100, lwd = 1, border = "darkblue") }
      
      box()
    } # if
  } # po k
}, interval = 0.1, movie.name = "anim_30000_cw3.gif") # saveGIF

# czas dzialania
stop <- Sys.time()
stop - start 
