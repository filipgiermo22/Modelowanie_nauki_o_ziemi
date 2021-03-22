#instlacja pakietu
install.packages("animation")
#załadowanie pakietu
library(animation)

#inicjalizacja macierzy dla kroku nowego i przeszłego
Lnew<-matrix(nrow=10,ncol=10,0)
L<-matrix(nrow=10,ncol=10,0)
#warunki brzegowe
L[,1]<-rep(0,10)
L[,10]<-rep(0,10)
L[1,]<-rep(0,10)
L[10,]<-rep(1,10)
#chcemy mieć warunki brzegowe też w nowym kroku
Lnew<-L
#wypisanie stanu pola w kroku 0
L
#blok do zapisu w animacji GIF o domyślnych: interwale 1s i nazwie animation.gif
saveGIF({
  
  Limg <- apply(L, 2, rev)
  image(t(Limg))
  #dodanie w lewym górnym rogu numeru iteracji
  text(0,1,0)
  
  #pętla po iteracjach (k)
  for (k in 1:100)
  {
    #pętla po wierszach (i) i kolumnach (j)
    for (i in 2:9){
      for (j in 2:9){
        Lnew[i,j]<-0.25*(L[i-1,j]+L[i+1,j]+L[i,j-1]+L[i,j+1])
      }
    }
    
    # WARUNEK STOPU: zaokrąglenie do 3 miejsc po przecinku i porównanie poprzedniej z następną
    
    if (all(round(L, digits = 3) == round(Lnew, digits = 3))) {
      break
    }
    
    #przejście o krok do przodu w iteracji
    L<-Lnew
    print(k)
    print(L)
    
    # zapisywanie co 10
    
    if(k %% 10 == 0) {
      Limg <- apply(L, 2, rev)
      image(t(Limg))
      #dodanie w lewym górnym rogu numeru iteracji
      text(0,1,k)
    }
    
  } #po k
}) #SaveGIF
