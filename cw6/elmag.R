###################################################################tutaj zaczynamy dane EM


data<-read.csv("Wyniki_em_2019.csv",header=TRUE,encoding="UTF-8")

#szerokoœci (latitude)
#usuniêcie "\" z koñca stringa 
pom0<-substr(data$Szerokoœæ.geograficzna,1,nchar(data$Szerokoœæ.geograficzna)-1)

#podzia³ na stopnie minuty sekundy 
pom1<-strsplit(pom0, "°" )
pom2<-strsplit(unlist(pom1), "' " )

#wszystko po kolei
lats<-as.numeric(unlist(pom2))

#pierwsze wartoœci stopnie, drugie minuty itp.
d<-lats[seq(1, length(lats), 3)]
m<-lats[seq(2, length(lats), 3)]
s<-lats[seq(3, length(lats), 3)]

#do decimal 
lat<-d+m/60+s/3600


#d³ugoœci (latitude)
#usuniêcie "\" z koñca stringa 
pom0<-substr(data$D³ugoœæ.geograficzna,1,nchar(data$D³ugoœæ.geograficzna)-1)

#podzia³ na stopnie minuty sekundy 
pom1<-strsplit(pom0, "°" )
pom2<-strsplit(unlist(pom1), "' " )

#wszystko po kolei
lons<-as.numeric(unlist(pom2))

#pierwsze wartoœci stopnie, drugie minuty itp.
d<-lons[seq(1, length(lats), 3)]
m<-lons[seq(2, length(lats), 3)]
s<-lons[seq(3, length(lats), 3)]

#do decimal 
lon<-d+m/60+s/3600


#od razu value chocia¿ nie bêdzie na razie potrzebne
value<-as.numeric(data$Wynik.pomiaru..V.m.)



# budujemy ramki danych dla Polski
dataP<-data.frame(longitude=lon,latitude=lat)
dataP$value<-value

#sprawdŸmy teraz mape

library(ggmap)
lat <- c(min(dataP$latitude)-0.05, max(dataP$latitude)+0.05)
lon <- c(min(dataP$longitude)-0.05, max(dataP$longitude)+0.05)
bbox <- make_bbox(lon,lat)
b<- get_map(bbox,maptype="terrain", source="osm",color="color",messaging = TRUE)
ggmap(b)+geom_point(data = dataP, aes(longitude,latitude))


length(dataP[,1])


#teraz czas obiekty przestrzenne
library(sf)

#wyczytanie danych z pliku shape
dzielnice<-st_read("dzielnice_Krakowa.shp")

#sprawdzamy czy wszystko ok i widzmy ¿e uk³ad odniesienia jest ETRS89 a chcemy mieæ WGS84 bo sandard dla czujników EM
dzielnice

#transformacja do WGS
dzielniceWGS<-st_transform(dzielnice,crs = 4326)

#plot (wszystkie cechy)
plot(dzielniceWGS)

#plot gemetrii bo tylko to nas interesuje 
plot(st_geometry(dzielniceWGS))

# na razie nie chcemy podzia³y na dzielnice wiêc je wszystkie ³¹czymy ze sob¹ 
krakowWGS<-st_union(st_geometry(dzielniceWGS))


# bibloteka sf dzia³a z "korowym" plot co pozwala unikaæ ggplot
plot(dataP[,1],dataP[,2],pch=19)
plot(krakowWGS,add=TRUE)




#pora na statystki przestrzenne 
library(spatstat)
library(rgdal)

#trzeba przejœæ ze sfery (lat lon) na p³ask¹ mapê w ukladzie utm (ma³opolska jest w 34N)
krakowUTM<-st_transform(krakowWGS,CRS("+proj=utm +zone=34N +datum=WGS84"))

# to samo z naszymi danymi o czujnikach 
data_spat<-data.frame(lon=dataP$longitude,lat=dataP$latitude,value=dataP$value)
coordinates(data_spat) <- ~lon+lat
proj4string(data_spat) <- CRS("+proj=longlat +datum=WGS84")
data_spat
#^ to jest obiet w uk³adzie sferycznym który mo¿na ju¿ automatycznie konwertowaæ

library(rgdal)
#konwersja 
data_UTM <- spTransform(data_spat, CRS("+proj=utm +zone=34N +datum=WGS84"))

# stworzenie obiektu ppp 2D
dataP_ppp<-ppp(x=data_UTM$lon,y=data_UTM$lat,window=as.owin(krakowUTM))

#stworzenie ppp z marks czyli z danymi w punktach (tu wartoœæ)
dataP_ppp_v<-ppp(x=data_UTM$lon,y=data_UTM$lat,marks=data_UTM$value,window=as.owin(krakowUTM))

#widzimy ze punktow jest 97 czyli dosz³y b³êdne adresy
data15_ppp

#porówanie plotow
plot (dataP_ppp,lwd=2)
plot (dataP_ppp_v,lwd=2)

#i plot na mapie


bbb<-st_bbox(krakowWGS)

bbox <- make_bbox(c(bbb[1],bbb[3]),c(bbb[2],bbb[4]))
bb<- get_map(bbox,maptype="terrain", source="stamen",color="color",messaging = TRUE,zoom=11)
ggmap(bb)+geom_point(data = dataP, aes(longitude,latitude))





#czy w danych jest patern czy s¹ w miarê losowo rozsiane - test kwadrantowy 
qt <- quadrat.test(dataP_ppp)
plot(qt)
qt
# co to oznacza -> mala liczba (poni¿ej alfa) oznacza ¿e nie ma spatial randomness 

#to samo tylko inaczej
# patrzymy na dystans do najbl¿eszego punktu i oczekujemy symetrycznego histogramu dla losowoœci
nn<-nndist(dataP_ppp)
hist(nn)


#to samo tylko inaczej 2
#dla skumulowanego prawdopodobieñstwa ¿e dystans jest dany lub mniejszy
G<-Gest(dataP_ppp)
plot(G,lwd=2)
#linia niebieska -> losowoœæ pozosta³e to empiryczne prawdopodobieñstwo w ró¿nych wariantach 
#ze wzglendu na poprawki zwi¹zane z tym ¿e mamy granice okna i czêœæ punktów mo¿e mieæ sztucznie
#zawy¿on¹ odleg³oœæ 

#niepewnoœæ -> krzywa powinna byæ w szarym 
env<-envelope(dataP_ppp,fun=Gest)
plot(env)

#funkcja K to liczba spodziewana liczba s¹siadów w danej odleg³oœci od punktu skalowana 
#intensywnoœci¹ (w uproszczeniu prawdop. punktu dla dystansu ->0)
#tylko jeden wariant poprawki "iso"
K<-Kest(dataP_ppp,correction="iso")
plot(K)

#tak naprawdê lepiej jest u¿ywaæ normalizowanego K czyli L (jak na wyk³adzie)
env<-envelope(dataP_ppp,fun=Lest)
plot(env)

#ale najlepiej jest zbadaæ rozk³ad prawdopodobieñstwa wyst¹pienia punktu w danym miejscu
ed<-density(dataP_ppp)
plot(ed)
points(dataP_ppp,pch="*",col="White")
plot(krakowUTM,add=TRUE,border="White",col="NA")

#mapa 
library(maptools)
library(automap)
library(raster)

#znowu konwersja :)
dataP_spdf<-as.SpatialPointsDataFrame.ppp(dataP_ppp_v)
spplot(dataP_spdf)
coordinates(dataP_spdf)


# Ordinary Kriging 
elev_auto <- autoKrige(marks ~ 1, input_data = dataP_spdf,model = "Mat")
plot(elev_auto)

plot(elev_auto$krige_output[1])
points(dataP_ppp_v,pch="*",col="White")
plot(Window(dataP_ppp_v),add=TRUE)


#zarys krakowa w odpowiednim formacie
bound<-st_as_sf(krakowUTM)  #st_convex_hull(krakowUTM)
plot(bound,add=TRUE)

#okreœlenie siatki punktów w których bêdzie dokonywana aproksymacja dowoln¹ metod¹, z jakim dystansem i ile punktów w dwóch kierunkach 
coord<-as.data.frame(st_coordinates(krakowUTM))
left_down<-c( min(coord$X), min(coord$Y))
#rozmiar oczka siatki w metrach
size<-c(100,100)

#wyliczenie ile puntów potrzeba w najwiêkszym wymiarze 
right_up<-c( max(coord$X), max(coord$Y))
points<- (right_up-left_down)/size 
num_points<-ceiling(points) 

#dodajemy wiêcej punktów ¿eby pokazaæ efekt crop
eps<-10
num_points<-num_points+eps

grid <- GridTopology(left_down, size,num_points )

# kowersja do odpowiedniego formatu w odpowiednim uk³adzie 
gridpoints <- SpatialPoints(grid, proj4string = CRS("+proj=utm +zone=34N +datum=WGS84"))
plot(gridpoints)

#przyciêcie do prostok¹t w którym mieœci siê Krakow to ju¿ jest w zasadzie zrobione wczeœniej w GridTopology
#ale zosta³o zepsute (+ eps) po to ¿eby pokazaæ efekt
cropped_gridpoints <- crop(gridpoints,bound )
plot(cropped_gridpoints,add=TRUE,col="Red")
plot(Window(dataP_ppp_v),add=TRUE)


# konwersja do SpatialPixels
spgrid <- SpatialPixels(cropped_gridpoints)
coordnames(spgrid) <- c("x", "y")
plot(spgrid)
plot(Window(dataP_ppp_v),add=TRUE)


elev_auto <- autoKrige(marks ~ 1, input_data = dataP_spdf,new_data=spgrid,model = "Mat")
plot(elev_auto$krige_output[1])
points(dataP_ppp_v,pch="*",col="White")
plot(krakowUTM,add=TRUE,border="White")



#spróbujmy zrobiæ to samo ale wykorzystuj¹c inny obiekt (spatial polygons) do dok³¹dnego przyciêcia siatki 
#bound jest w formacie sf. R potrafi u¿yæ go do przyciêcia siatki ale tylko w prostok¹t
#jesli chcemy przyci¹æ ³adnie do granic potrzeba innego formatu (kto by pomyœla³)
#kordynaty musz¹ byæ odfiltowane bo w pliku bound s¹ b³êdy w postaci 2 krótkich poligonów 
library(sp)
cor<-st_coordinates(bound)
cor
#wybieramy tylko pierwszy poligon bo reszta to œmieci
cor_f<-cor[cor[,3]==1,]
p = Polygon(cbind(cor_f[,1],cor_f[,2]))
ps = Polygons(list(p),1)
sps = SpatialPolygons(list(ps))
plot(sps,lwd=3)

#teraz jeszcze raz przycinamy i przekszta³camy tak jak poprzednio
#ale tym razem plikiem sps (dla pewnoœci mo¿na by³o dodaæ jeszcze uk³ad wspó³rzêdnych) 
cropped_gridpoints <- crop(gridpoints,sps)
plot(cropped_gridpoints,col="Red")
plot(Window(dataP_ppp_v),add=TRUE)


# konwersja do SpatialPixels
spgrid <- SpatialPixels(cropped_gridpoints)
coordnames(spgrid) <- c("x", "y")
plot(spgrid)
plot(Window(dataP_ppp_v),add=TRUE)


dataP_ppp_v$marks

elev_auto <- autoKrige(marks ~ 1, input_data = dataP_spdf,new_data=spgrid,model = "Mat")
plot(elev_auto$krige_output[1])
points(dataP_ppp_v,pch="*",col="White")

#zobaczmy b³¹d dopasowania 
plot(elev_auto$krige_output[3])
points(dataP_ppp_v,pch="*",col="White")


#czêsto chcemy wyrosowaæ zmienne ci¹g³e jako faktory np. male, œrednie, du¿e itp.
#elev_auto$krige_output$var1.pred to s¹ wartoœci ska³dowej z elev_auto$krige_output[1] 
#dla prostoty wprawdzamy zmienn¹ pomocnicz¹ zamiast pêtli stosujemy ifelse 
#produkuj¹c 3 stany  (musza byc conajmnie 3)
a<-elev_auto$krige_output$var1.pred
b<-rep("NA",length(a))
b<-ifelse(a<=0.3,1,b)
b<-ifelse(a>0.3 & a<=0.4,2,b)
b<-ifelse(a>0.4,3,b)
elev_auto$krige_output$var1.factor<-as.factor(b)
plot(elev_auto$krige_output[4])


