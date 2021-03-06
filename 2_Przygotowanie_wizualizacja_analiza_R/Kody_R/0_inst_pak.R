# install.packages
# Instalacja pakiet�w R z serwera CRAN pakiet ma nazw� rugarch (umo�liwia szacowanie parametr�w 
# jednowymiarowych modeli klasy GARCH i weryfikacj� za�o�e� tych modeli oraz symulacji z za�o�onego procesu GARCH)
# Pliki binarne pakietu instaluje si� jednokrotnie 
# (ewentualnie mo�na po jakim� czasie dokona� aktualizacji pakietu za pomoc� komendy update.packages)
install.packages("rugarch")

?install.packages # Pomoc dotycz�ca funkcji install.packages (mi�dzy innymi argumenty, kt�re nale�y poda� na wej�ciu tej funkcji)

# Za�adowanie pakietu do przestrzeni roboczej (pakiet �adujemy za ka�dym razy kiedy chcemy skorzysta� z jego funkcji)
library(rugarch)

# Wykaz funkcji w ramach pakietu wraz z informacji dotycz�c� ich argument�w, zwracanymi warto�ci oraz najcz�ciej
# kr�tkim przyk�adem wykorzystanie dla przyk�adowego zbioru danych do��czonego do pakietu
help(package = rugarch)

# Pomoc dotycz�ca funkcji ugarchfit umo�liwiaj�cej oszacowanie parmatr�w modeli klasy ARMA-GARCH metod� MNW albo quasi-MNW
?ugarchfit          
?ugarchspec # Pomoc dotycz�ca funkcji umo�liwiaj�cej specyfikacj� modelu z klasy ARMA-GARCH, 
            # kt�ra nast�pnie jest wykorzystywana jako wej��ie dla funkcji ugarchfit

# Je�eli chcemy zainstalowa� "masowo" pakiety z serwera CRAN odnosz�ce si� do danej dziedziny zastosowa�: CRAN Task Views
# W�r�d tych kategorii pakiet�w mamy m.in.: statystyka wielowymiarowa (Multivariate), ekonometria (Econometrics), 
# finanse (Finance), wnioskowanie bayesowskie (Bayesian), statystyka odporna (Robust)
# wykaz rozk�ad�w zmiennych jedno- i wielowymiarowych (Distributions), metody optymalizacyjne (Optimization), 
# numeryczne metody rozwi�zywania r�wna� r�niczkowych (DifferentialEquations), uczenie maszynowe (MachineLearning), 
# analiza skupie�, chemometria (ChemPhys), nauki spo�eczne (SocialSciences) i inne
# CRAN Task Views: https://cran.r-project.org/web/views/

# W celu "masowej" instalacji pakiet�w mo�emy skorzysta� z funkcji install.views z pakietu ctv (nazwa jest skr�tem o CRAN Task Views)
install.packages("ctv")
library(ctv)
install.views("Econometrics") # nazw� interesuj�cej nas grupy pakiet�w mo�emy sprawdzi� w CRAN Task Views: 
                              # https://cran.r-project.org/web/views/

# Istniej� tak�e inne serwery, na kt�rych zamieszczane s� pakiety R, np. serwer R-Forge: http://R-Forge.R-project.org
# instalacja pakietu CLAG (algorytm analizy skupie�) z repozytorium Rforge, wsakzujemy repos = "http://R-Forge.R-project.org"
install.packages("CLAG", repos="http://R-Forge.R-project.org")

# Mo�na zainstalowa� tak�e tzw. wersj� dewelopersk� pakietu z system�w takich jak Github (platforma deweloperska, 
# kt�ra ��czy w sobie funkcjonalno�� systemu zarz�dzania wersjami oraz inne narz�dzia umo�lwiaj�ce wygodne
# tworzenie kodu, w szczeg�lno�ci w grupach
# Instalacje pakiet�w z Github jest mo�liwa za pomoc� funkcji install_github z pakietu devtools
install.packages("devtools") # instalujemy pakiet devtools
library(devtools)            # �adujemy pakiet devtools
# Instalujemy pakiet DepthProc z Github - autorstwa zespo�u: Kosiorowski, Zawadzki, S�omczynski, Bocian, W�grzynkiewicz
# https://github.com/zzawadz/DepthProc
install_github("zzawadz/DepthProc")


#####################################################################################################
#####################################################################################################
#####################################################################################################

# Tworzenie funkcji w R, realizuj�cych wskazane przez nas procedury

arytm <- function(a = 2, b = 3) {
  if(!is.numeric(c(a,b)) | b == 0) stop("a or b are not numeric or b = 0")
  sum_res <- a + b
  diff_res <- a - b
  mult_res <- a * b
  div_res <- a/b
  list(sum = sum_res, diff = diff_res, prod = mult_res, quot = div_res)
}

arytm(a = 5, b = 7)
arytm(5, 7)         # Je�eli warto�ci argument�w, podawane s� w takiej samej kolejno�ci 
arytm(b = 7, a = 5) # je�eli wskazujemy warto�ci argument�w, w innej kolejno�ci, ni� argumenty zosta�y wskazane
                    # w definicji funkcji trzeba wskaza� nazw� argument�w
arytm("k", 2)       # zwraca b��d argument b, jest warto�ci� tekstow� (character)
arytm(3,0)          # zwraca b��d, b jest r�wne 0

arytm()             # Jako, �e nie zosta�y wskazane warto�ci argument�w, funkcja jest wywo�ywana dla warto�ci
                    # domy�lnych z definicji funkcji

# Wywo�anie funkcji w ramach procedury debugowania ("poszukiwania b��d�w"), 
# kolejne "podprecedury" w ciele funkcji s� wykonywane,
# dzi�ki czemu mo�na sprawdzi�, jakie s� warto�ci przyjmowane przez lokalne ("po�rednie") zmienne funkcji,
# przedstawiaj�ce wyniki cz�stkowe, kt�re sk�adaj� si� na ko�cowy wynik funkcji
debug(arytm) 
arytm(10, 2)        # Wywo�ujemy funkcj� arytm w trybie debugowania dla warto�ci arugment�w a i b odpowiednio 10 i 2
arytm(10,0)         # Wywo�ujemy funkcj� arytm w trybie debugowania dla warto�ci arugment�w a i b odpowiednio 10 i 0

# Wy��czenie uruchamiania funkcji w trybie debugowania
undebug(arytm)

################################################################################################################
################################################################################################################
################################################################################################################
# Przep�yw sterowania - funkcje z grupy apply, jako alternatywy dla p�tli w R

# sapply(vec, func) - wyznacza warto�� funkcji func jest wyznaczana, dla kazdego kolejnego elementu wektora 
# albo listy vec, zwraca wektor lub macierz wynik�w (mo�na uzyska� takie same wyniki, jak za pomoc� p�tli for)
# lapply(vec, func) - takie samo dzia�anie jak sapply, tylko zwracana jest lista wynik�w
# mapply(func, vec1, vec2) - wyznacza warto�ci funkcji func dla par i-tego elementu vec1 oraz i-tego elementu vec2,
# gdzie  vec1 i vec2 maj� taka sam� liczb� element�w, indeksowanych przez i: i=1,2,...,n, zwraca 
# wektor n-elementowy wynik�w (mo�na uzyska� takie same wyniki, jak w przypadku zagnie�d�onych p�tli for)
# outer(X, Y, func) - Wyznaczanie warto�ci funkcji func dla ka�dego (x, y) \in X x Y, zwraca macierz wynik�w
# o wymiarach (#X) x (#Y)
# replicate(nrep, func) - wyznacza warto�� funkcji func, nrep razy 


# Wykorzystanie funkcji sapply

# Generator LCG - parametry z Borland C/C++
lincongr<- function(a = 22695477, b = 1, p = 2^32, seed = 999, len = 10000){
  x <- rep(0, len + 1)
  x[1] <- seed
  val <- sapply(2:(len+1),  function(i) x[i]<<- (a * x[i-1] + b) %% p )/p
  plot(val, type = "l", main = "Series of the LCG pseudorandom numbers", col = "blue")
  hist(val, main = "Histogram of the LCG pseudorandom numbers", col = "red")
  val
}

lincongr() # ci�g liczb pseudolosowych z generatora LCG

# Symulacja realizacji stacjonarnego procesu AR(1) z wykorzystaniem sapply
ar1sim <- function(n = 1000, const = 2.5, ar1 = -.7, sd = .3, parvalTit = TRUE){
  if(abs(ar1)>=1 | sd <= 0) stop("Wrong parameters specified")
  eps <- rnorm(n+1, 0, sd)
  x <- NULL
  x[1] <- rnorm(1, const/(1-ar1), sd/(1-ar1^2)^.5)
  sapply(2:(n+1), function(i) x[i]<<- const + ar1*x[i-1] + eps[i])
  tit <- if(parvalTit){
    bquote("Simulation from the Stationary AR(1) process:" ~ y[t] == .(const) ~
             .(ifelse(ar1>=0, "+", "-")) ~ .(abs(ar1)) * y[t-1] + epsilon[t])
  }else{
    bquote("Simulation from the Stationary AR(1) process:" ~ y[t] == nu + phi * y[t-1] + epsilon[t])}
  plot(x[-1], type = "l", main = tit, col = "red", ylab = expression(y[t]))
  x[-1]
}

ar1sim()  # Szereg z symulacji, w tytule wykresu zapis modelu z warto�ciami parametr�w
ar1sim(parvalTit = FALSE)  # Szereg z symulacji, w tytule wykresu zapis modelu z symbolami parametr�w

# Wyznaczenie warto�ci dzia�a� arytmetycznych dla warto�ci x[i] oraz y[i], i = 1,...,n 
mapply(arytm, 1:100, 51:150)

# Wyznaczanie warto�ci funkcji f dla ka�dego (x, y) \in X x Y
outer(X = 1:5, Y = 5:10, function(x,y) x^2 + y^3)

# replicate: przyk�ad symulacji rozk�adu wskazanych estymator�w wybranego parametru (traktowanego jako ustalonego) 
# dla zadanego rozk�adu (z domy�lnymi warto�ciami parametr�w zaimplementowanymi w R), 
# dla okre�lonej wielko�ci pr�by, typu rozk�adu, zwraca warto�ci estymatora z kolejnych symulowanych pr�b,
# histogram rozk�adu warto�ci estymatora z na�o�on� funkcj� g�sto�ci oszacowan� z wykorzystaniem estymatora j�drowego
simestdist <- function(dist, nrep = 10000, n = 1000, estimators = c(mean, median)){
  simres <- sapply(estimators, function(est) replicate(nrep, est(dist(n))))
  nest <- ncol(simres)
  par(mfrow = c(nest %/% 2, 2))
  lapply(1:nest, 
         function(i) {hist(simres[,i], col = i + 1, freq = FALSE, xlab = expression(theta(x)), 
                           main = paste("Estimator", i, ", sample n =", n))
           lines(density(simres[,i]), col = "blue", lwd = 3)})
  simres}

(EVest <- simestdist(rnorm, n = 100, estimators = c(mean, median)))  # warto�� oczekiwana: 
# rozk�ady estymatora �redniej arytmetycznej i mediany z pr�by 
# n = 100 elementowej, z rozk�adu standyrozowanego normalnego
(SDest <- simestdist(rnorm, n = 100, estimators = c(sd, mad)))       # odchylenie standardowe: rozk�ady estymatora sd (odchylenia
# standardowego) i mad (median bezwgl�dnych odchyle� od mediany)

# Podstawowe statystyki opisowe dla rozk�adu symulacyjnego warto�ci estymator�w
install.packages("psych")
library(psych)
?describe
(desEVest <- round(t(describe(EVest)), 4))
(desSDest <- round(t(describe(SDest)), 4))

# Wyniki symulacji warto�ci estymator�w dla r�nych wielko�ci pr�by, jako kolejne elementy listy
lapply(10^(1:5), simestdist, dist = rnorm, nrep = 1000, estimators = c(mean, median))  

par(mfrow = c(1,1)) # jeden wykres na okno

############################################################################################################

?Distributions   # rozk�ady zaimplementowane w pakiecie stats (d - funkcja g�sto�ci, p - dystrybuanta,
                 # q - funkcja kwantylowa, r - generowanie liczb pseudolosowych)
# Wykaz rozk�ad�w w pakietach R - CRAN Task View - Distributions:
# https://cran.r-project.org/web/views/Distributions.html

# Uproszczona wersja rejection sampling
# f - target density, g - proposal density, 
# for all x: f(x) > 0 => g(x) > 0,
# for all x in supp: f(x) <= M * g(x)

rejectsampl <- function(n = 10000, targetDist = "beta", proposDist = "unif", M, 
                        parsTarget = list(shape1 = 3, shape2 = 6), parsProp = list(), histBreaks = "Sturges"){
  
  rpropos <- paste(c("r", proposDist), collapse = "")
  dtarget <- paste(c("d", targetDist), collapse = "")
  dpropos <- paste(c("d", proposDist), collapse = "")
  x <- do.call(rpropos, c(n = n, parsProp))
  f <- do.call(dtarget, append(list(x = x), parsTarget))
  g <- do.call(dpropos, append(list(x = x), parsProp))
  
  
  if(proposDist == "unif") {M <- max(f)
  if(length(parsProp)) M <- M * (parsProp$max - parsProp$min)
  }
  fMg <- f/(M*g)
  if(!all(fMg <= 1)) stop("M and proposal distribution wrongly specified")
  un <- runif(n)
  accept <- fMg > un
  sample <- x[accept]
  nAccept <- sum(accept)
  fracAccepted <- nAccept/n*100
  minx <- min(x)
  maxx <- max(x)
  hist(sample, main = paste("Rejection sampling - target: ", targetDist, ", proposal:", proposDist, sep =""), 
       freq = FALSE, col = "green", breaks = histBreaks)
  xseq <- seq(from = minx, to = maxx, length.out= 10000)
  lines(xseq, do.call(dtarget, append(list(x = xseq), parsTarget)), col = "red", lwd = 2)
  list(sample = sample, nAccepted = nAccept, nGen = n, fracAccepted = fracAccepted)
}

rejectsampl()

#############################################################################################################
#############################################################################################################
#############################################################################################################

# Wyra�enia matematyczne zapisane w formie drzew sk�adniowych R
# substitute - zwraca wierdzcho�ki drzewa sk�adniowego dla wyra�enia (expression)
e1 <- substitute(a+2*b)
length(e1)
typeof(e1)      # typ languange
e1
# Wierzcho�ki drzewa sk�adniowego od korzenia do li�ci
e1[[1]]         # "+"
e1[[2]]         # symbol "a"
e1[[3]]
typeof(e1[[3]]) # typ language
e1[[3]][[1]]    # "*"
e1[[3]][[2]]    # symbol "2"
e1[[3]][[3]]    # symbol "b"

##############################################################################################################
##############################################################################################################
##############################################################################################################
# Dla zainteresowanych

# Elementy przetwarzania r�wnoleg�ego - pakiet parallel
# "zr�wnoleglone" odpowiedniki sapply, lapply, mapply, odpowiednio parSapply, parLapply, clusterMap
library(parallel)
# (n.cores <- detectCores())  # okre�lenie liczby rdzeni
# cluster <- makePSOCKcluster(names=n.cores) # okre�lenie klastra obliczeniowego

#parLapply(cl=cluster, X, fun, ...) - odpowiednik sapply, wykonywany na wielu rdzeniach procesora
#parLapply(cl=cluster, X, fun, ...) - odpowiednik lapply, wykonywany na wielu rdzeniach procesora
#clusterMap(cl=cluster, fun, ...) - odpowiednik mapply, wykonywany na wielu rdzeniach procesora
# stopCluster(cl=cluster) - zako�czenie oblicze� r�wnoleg�ych za pomoc� klastra

# Przyk�ad zastosowania r�wnoleglej wersji lapply: parLapply 
# wyniki symulacji warto�ci estymator�w dla r�nych wielko�ci pr�by, jako kolejne elementy listy 
(n.cores <- detectCores())
cluster <- makePSOCKcluster(names=n.cores)
parLapply(cl=cluster, 10^(1:5), simestdist, dist = rnorm, nrep = 1000, estimators = c(mean, median))
stopCluster(cl=cluster)

# Pakiet rbenchmark umo�liwiaj�cy pomiar czasu systemowego dla wywo�ania funkcji
install.packages(rbenchmark) 
library(rbenchmark)
cluster <- makePSOCKcluster(names=detectCores())
benchmark(replications = 1,
          lapply(10^(1:4), simestdist, dist = rnorm, nrep = 1000, estimators = c(mean, median)),
          parLapply(cl=cluster, 10^(1:4), simestdist, dist = rnorm, nrep = 1000, estimators = c(mean, median))
)

stopCluster(cl=cluster)