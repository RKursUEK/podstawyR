#Tworzenie przykladowych modeli opartych na drzewach decyzyjnych

library(rpart)
library(rpart.plot)

# Budowa drzewa
# Specyfikacja modelu: zmienna_objaśniana ~ zmienne objaśniające
# Określenie ramki danych: data = ....
# Optymalizowane kryterium: indeks Giniego (domyślne ustawienie)

model1<-rpart(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data=iris)
# Wykres drzewa
rpart.plot(model1,main="Model1")
# Wyświetlenie szczegółowych informacji na temat drzewa
cat("\n\nInformacje dotyczące modelu: model1\n\n")
print(model1)

# Jawne określenie kryterium
model2<-rpart(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data=iris,parms=list(split="information"))
rpart.plot(model2,main="model2")
cat("\n\nInformacje dotyczące modelu: model2\n\n")
print(model2)

model3<-rpart(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data=iris,parms=list(split="gini"))
rpart.plot(model3,main="model3")
cat("\n\nInformacje dotyczące modelu: model3\n\n")
print(model3)

# sterowanie złożonością modelu
# minsplit – minimalna liczba elementów w węźle niezbędna do dokonania podziału
# cp (complexity parameter) – wymagana minimalna wartość polepszenia się miary jakości drzewa 
#     (o ile minimalnie musi się zmniejszyć miara niejednorodności)
model4<-rpart(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data=iris,parms=list(split="gini"),control=rpart.control(minsplit=1,cp=0.00001))
rpart.plot(model4,main="model4")
cat("\n\nInformacje dotyczące modelu: model4\n\n")
print(model4)


# utworzonie wektora z wartościami losowymi z przedziału 1:lw 
# (wartości całkowite, losowanie bez zwracania)
lw <- nrow(iris)
ind<-sample(1:lw)

n.ucz <- trunc(0.8 * lw)

# numery obserwacji zaliczonych do zbioru uczącego
ind.ucz<-ind[1:n.ucz]

# numery obserwacji zaliczonych do zbiotru testowego
ind.test<-ind[n.ucz+1:lw]

# tworzenie zbioru uczącego
iris.ucz<-iris[ind.ucz,]

# tworzenie zbioru testowego
iris.test<-iris[ind.test,]

# tworzenie drzewa przy wykorzystaniu zbioru uczącego
model5<-rpart(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data=iris.ucz,parms=list(split="gini"),control=rpart.control(minsplit=3))
rpart.plot(model5,main="model5")
cat("\n\nInformacje dotyczące modelu: model5\n\n")
print(model5)


# uruchomienie modelu dla danych uczących, na wyjściu identyfikator klasy
res.ucz<-predict(model5,newdata=iris.ucz,type="class")

# badanie poprawności działania modelu (na podstawie zbioru uczącego)
# budowa tablicy kontyngencji
# porównanie informacji pochodzących ze zbioru uczącego (iris.ucz$Species)
# z wynikiem działania modelu (res.ucz)

tb.ucz<-table(iris.ucz$Species,res.ucz)
cat("\n\nTabela kontyngencji\n\n")
print(tb.ucz)

# miara jakości modelu:
# iloraz liczby przypadkóW zaklasyfikowanych prawidłowo i liczby wszystkich przypadków
q.ucz <- sum(diag(tb.ucz))/sum(tb.ucz)
cat("\n\nJakość dla zbioru uczącego",q.ucz,"\n\n")


# uruchomienie modelu dla danych testowych, na wyjściu identyfikator klasy
res.test<-predict(model5,newdata=iris.test,type="class")

# badanie poprawności działania modelu (na podstawie zbioru testowego)
# budowa tablicy kontyngencji
# porównanie informacji pochodzących ze zbioru testowego (iris.test$Species)
# z wynikiem działania modelu (res.test)
tb.test<-table(iris.test$Species,res.test)
cat("\n\nTabela kontyngencji\n\n")
print(tb.test)

# miara jakości modelu:
# iloraz liczby przypadkóW zaklasyfikowanych prawidłowo i liczby wszystkich przypadków
q.test <- sum(diag(tb.test))/sum(tb.test)
cat("\n\nJakość dla zbioru testowego",q.test,"\n\n")

# uruchomienie modelu dla danych testowych, na wyjściu prawdopodobieństwo przynależności do klasy
res.test.prob<-predict(model5,newdata=iris.test,type="prob")

# wyświetlenie wyników dla 5 początkowych przypadków
cat("\n\nWyniki dla 5 początkowych przypadków\n\n")
print(head(res.test.prob),5)


