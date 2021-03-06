
library(animation)

# parametry modelu
nz <- 500
nx <- 500
fpeak <- 30.0
dt <- 0.02 # krok czasowy
et <- 0.5
ds <- 1.0
# położenie źródła
xs <- nx / 2.0
zs <- 25
# ilość kroków czasowych dla sejsmogramu
nt <- et / dt + 1

# model1
V <- matrix(nrow = nz, ncol = nx, 2000)
for (i in 1:nz / 2) {
  for (j in 1:nx) {
    V[i, j] <- 1000
  }  
}

# macierze dla pól ciśnień w czasie t+1, t oraz t-1
p <- matrix(nrow = nz, ncol = nx, 0)
pm <- matrix(nrow = nz, ncol = nx, 0)
pp <- matrix(nrow = nz, ncol = nx, 0)

# TODO: trzeba znaleźć Vmax do warunku stabilności
vmax <- 2000

# dtr - realny krok próbkowania który jest dzielnikiem dt
dtr <- ds / (2.0 * vmax)
w2 <- 0
while (1) {
  w2 <- w2 + 1
  w1 <- dt / w2
  if (w1 <= dtr) {
    dtr <- w1
    break
  }
}

# pasek postępu
niter = et / dtr + 1
prog_bar <- txtProgressBar(min = 0, max = niter, style = 3)

kk <- 1   # ilość dtr na jedną dt
kkk <- 0  # ilość dt
k <- 1    # ilość dtr

seis<-matrix(nrow=nt,ncol=nx,0)
x = 1

saveGIF({
    while (1) {
    k <- k + 1
    kk <- kk + 1
    t <- k * dtr
    # pasek postepu
    setTxtProgressBar(prog_bar, k)
    
    # pętla główna modelowania
    for (i in 2:(nz -1)) {
        for (j in 2:(nx - 1)) {
        pp[i, j] = 2.0 * p[i, j] - pm[i, j] + ((dtr * dtr) / (ds * ds) * V[i, j] * V[i, j]) *
            (p[i + 1, j] + p[i - 1, j] + p[i, j + 1] + p[i, j - 1] - 4.0 * p[i, j])
        }
    }
    
    # źródło Rickera
    pp[zs, xs] <- pp[zs, xs] + exp(-(((pi * fpeak * (t - (1.0 / fpeak))) * (pi * fpeak * (t - (1.0 / fpeak)))))) *
        (1.0 - 2.0 * ((pi * fpeak * (t - (1.0 / fpeak))) * (pi * fpeak * (t - (1.0 / fpeak)))))
    
    ######################################################################################
    
    # transparent lewa - warunek brzegowy dla krawędzi lewej
    for (i in 1:nz) {
        pp[i, 1] = p[i, 1] + p[i, 2] - pm[i, 2] +
        V[i, 1] * (dtr / ds) * (p[i, 2] - p[i, 1] - (pm[i, 3] - pm[i, 2]))
    }

    # TODO: transparent prawa
        for (i in 1:nz) {
        pp[i, nz] = p[i, nz] + p[i, nz-1] - pm[i, nz-1] +
        V[i, nz] * (dtr / ds) * (p[i, nz-1] - p[i, nz] - (pm[i, nz-2] - pm[i, nz-1]))
    }
    
    # TODO: transparent góra
        for (j in 1:nz) {
        pp[1, j] = p[1, j] + p[2, j] - pm[2, j] +
        V[1, j] * (dtr / ds) * (p[2, j] - p[1, j] - (pm[3, j] - pm[2, j]))
    }
    # TODO: transparent dół
        for (j in 1:nz) {
        pp[nz, j] = p[nz, j] + p[nz-1, j] - pm[nz-1, j] +
        V[nz, j] * (dtr / ds) * (p[nz-1, j] - p[nz, j] - (pm[nz-2, j] - pm[nz-1, j]))
    }

    ######################################################################################
    
    # przejście o krok do przodu
    pm <- p
    p <- pp
    
    # warunek do zapisania próbki sejsmogramu - taki sam zrobić dla animacji!
    if (kk * dtr + dtr / 10.0 >= dt) {
        kk <- 0
        kkk <- kkk + 1 # zmienna kkk mówi o ilości zapisanych próbek sejsmogramu
        

        
    # zapisywanie wartości z górnej krawędzi modelu do sejsmogramu
    for(z in 1:nx){
      seis[x, z] <- pp[1, z]
    }
    x = x + 1
    
    # zapisanie animcaji 
    image(t(apply(pp, 2, rev)))
    text(0.2, 0.9, kkk * dt)

    # przerwanie po czasie
    if (kkk * dt > et)
      break
    }
    
    } # KONIEC PĘTLI GŁÓWNEJ
}, interval = 0.2, movie.name = "cw5.gif") # SAVEGIF



# wyrysowanie sejsmogramu
image(t(apply(seis, 2, rev)))

