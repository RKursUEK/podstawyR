# Analiza skupień

library(mclust) # indeks Randa

# Przygotowanie zbioru danych
# zbiór iris bez ostatniej kolumny
iris.new<-iris[,!(names(iris)=="Species")]

# dodanie do nazwy obiektu łańcucha "kw_"
rownames(iris.new)<-paste("kw_",rownames(iris.new),sep="")

# obliczenie macierzy odległości
d<-dist(iris.new,method="euclidean")

# Analiza skupień
cat("\nZastosowanie metody najbliższego sąsiedztwa\n")
clust.res1<-hclust(d,method="single")

# wykres (dendrogram)
plot(clust.res1,hang = -1)

# Wykres osuwiska
plot(head(rev(clust.res1$height),15),type="l")

# przecięcie drzewa na wysokości h = 1,2
div1<-cutree(clust.res1,h=1.2)

cat("\nWyniki klasyfikacji po przecięciu drzewa na wysokości 1,2\n")
cat("\nLiczebność klas\n")
print(table(div1))

# informacja o sposobie przypisania obiektów
cat("\nPrzypisanie przykładowych obiektów\n")
print(head(div1,10))

# rzeczywiste przypisanie obiektów
s<-iris$Species

indRand1 <- adjustedRandIndex(s,div1)
cat("\nindRand1 =",indRand1,"\n")

# porównanie wyników rzeczywistych z obliczonymi
tb1 <- table(s,div1)
cat("\nTablica kontyngencji\n")
print(tb1)

# przecięcie drzewa w miejscu podziału na 3 klasy
cat("\nPodział drzewa na trzy klasy\n")

div2<-cutree(clust.res1,k=3)
cat("\nLiczebność klas\n")
print(table(div2))

indRand2 <- adjustedRandIndex(s,div2)
cat("\nindRand2 =",indRand2,"\n")

# porównanie wyników rzeczywistych z obliczonymi
tb2 <- table(s,div2)
cat("\nTablica kontyngencji\n")
print(tb2)


# analiza skupień za pomocą metody Warda
cat("\nZastosowanie metody Warda\n")
clust.res3<-hclust(d,method="ward.D2")

# przecięcie drzewa w miejscu podziału na 3 klasy
cat("\nPodział na trzy klasy\n")
div3<-cutree(clust.res3,k=3)

cat("\nLiczebność klas\n")
print(table(div3))

indRand3 <- adjustedRandIndex(s,div3)
cat("\nindRand3 =",indRand3,"\n")

# porównanie wyników rzeczywistych z obliczonymi
tb3 <- table(s,div3)
cat("\nTablica kontyngencji\n")
print(tb3)


# zastosowanie metody k - średnich
cat("\nZastosowanie metody k-średnich\n")
km.res <- kmeans(iris.new,3)

# wyświetlenie informacji o przynależności do klasy
print(head(km.res$cluster,10))

indRand4 <- adjustedRandIndex(s,km.res$cluster)
cat("\nindRand4 =",indRand4,"\n")

tb4 <- table(s,km.res$cluster)
cat("\nTablica kontyngencji\n")
print(tb4)







