#instlacja pakietu
#install.packages("animation")
#instlacja biblioteki do animacji "muchy" - rozk?ad Levy'ego
#install.packages("rmutil")
#za?adowanie pakietu
library(animation)
library(rmutil)
N=100
#inicjalizacja macierzy dla kroku nowego i przesz?ego
Lnew<-matrix(nrow=N,ncol=N,0)
L<-matrix(nrow=N,ncol=N,0)
#warunki brzegowe
L[,1]<-rep(0,N)
L[,N]<-rep(0,N)
L[1,]<-rep(0,N)
L[N,]<-rep(0,N)
#chcemy mie? warunki brzegowe te? w nowym kroku
Lnew<-L

#pozycja punktu w obszarze
pos<- round(runif(2)*(N-2)+1)

#blok do zapisu w animacji GIF o domy?lnych: interwale 1s i nazwie animation.gif
saveGIF(interval = 0.1, movie.name = "animation.gif", {
  
  #przypisanie punktu
  L[pos[1],pos[2]]<-1
  
  Limg <- apply(L, 2, rev)
  image(t(Limg))
  #dodanie w lewym g?rnym rogu numeru iteracji
  text(0.1,0.9,k)
  
  #p?tla po iteracjach (k)
  for (k in 1:1000)
  {
    
    # wyrażenie dla rozkładu Levy'ego
    
    #pom <- pos + round((2*rbinom(2,1,0.5)-1)*rlevy(2,m=0,s=1))
    pom<-pos+round(runif(2)*2-1)
    while(pom[1]<=0 || pom[1]>=N || pom[2]<=0 || pom[2]>= N)
    {
      #pom <- pos + round((2*rbinom(2,1,0.5)-1)*rlevy(2,m=0,s=1))
      pom<-pos+round(runif(2)*2-1)
    }
    
    #p?tla po wierszach (i) i kolumnach (j)
    for (i in 2:99){
      for (j in 2:99){
        Lnew[i,j]<-0.25*(L[i-1,j]+L[i+1,j]+L[i,j-1]+L[i,j+1])
      }
    }
    
    #przej?cie o krok do przodu w iteracji
    L<-Lnew
    pos<-pom
    L[pos[1],pos[2]]<-1
    
    #image po rotacji. apply zadaje funkcj? (tu rev) do kolejnych
    #kolumn (2) lub wierszy (1) dla zadanej macierzy (L)
    #rev odwraca kolejno??
    if(k%% 10 == 0) {
      Limg <- apply(L, 2, rev)
      image(t(Limg))
      #dodanie w lewym g?rnym rogu numeru iteracji
      text(0.1,0.9,k)
    }
    
  } #po k
}) #SaveGIF