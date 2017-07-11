# install.packages
# Instalacja pakietów R z serwera CRAN pakiet ma nazwê rugarch (umo¿liwia szacowanie parametrów 
# jednowymiarowych modeli klasy GARCH i weryfikacjê za³o¿eñ tych modeli oraz symulacji z za³o¿onego procesu GARCH)
# Pliki binarne pakietu instaluje siê jednokrotnie 
# (ewentualnie mo¿na po jakimœ czasie dokonaæ aktualizacji pakietu za pomoc¹ komendy update.packages)
install.packages("rugarch")

?install.packages # Pomoc dotycz¹ca funkcji install.packages (miêdzy innymi argumenty, które nale¿y podaæ na wejœciu tej funkcji)

# Za³adowanie pakietu do przestrzeni roboczej (pakiet ³adujemy za ka¿dym razy kiedy chcemy skorzystaæ z jego funkcji)
library(rugarch)

# Wykaz funkcji w ramach pakietu wraz z informacji dotycz¹c¹ ich argumentów, zwracanymi wartoœci oraz najczêœciej
# krótkim przyk³adem wykorzystanie dla przyk³adowego zbioru danych do³¹czonego do pakietu
help(package = rugarch)

# Pomoc dotycz¹ca funkcji ugarchfit umo¿liwiaj¹cej oszacowanie parmatrów modeli klasy ARMA-GARCH metod¹ MNW albo quasi-MNW
?ugarchfit          
?ugarchspec # Pomoc dotycz¹ca funkcji umo¿liwiaj¹cej specyfikacjê modelu z klasy ARMA-GARCH, 
            # która nastêpnie jest wykorzystywana jako wejœæie dla funkcji ugarchfit

# Je¿eli chcemy zainstalowaæ "masowo" pakiety z serwera CRAN odnosz¹ce siê do danej dziedziny zastosowañ: CRAN Task Views
# Wœród tych kategorii pakietów mamy m.in.: statystyka wielowymiarowa (Multivariate), ekonometria (Econometrics), 
# finanse (Finance), wnioskowanie bayesowskie (Bayesian), statystyka odporna (Robust)
# wykaz rozk³adów zmiennych jedno- i wielowymiarowych (Distributions), metody optymalizacyjne (Optimization), 
# numeryczne metody rozwi¹zywania równañ ró¿niczkowych (DifferentialEquations), uczenie maszynowe (MachineLearning), 
# analiza skupieñ, chemometria (ChemPhys), nauki spo³eczne (SocialSciences) i inne
# CRAN Task Views: https://cran.r-project.org/web/views/

# W celu "masowej" instalacji pakietów mo¿emy skorzystaæ z funkcji install.views z pakietu ctv (nazwa jest skrótem o CRAN Task Views)
install.packages("ctv")
library(ctv)
install.views("Econometrics") # nazwê interesuj¹cej nas grupy pakietów mo¿emy sprawdziæ w CRAN Task Views: 
                              # https://cran.r-project.org/web/views/

# Istniej¹ tak¿e inne serwery, na których zamieszczane s¹ pakiety R, np. serwer R-Forge: http://R-Forge.R-project.org
# instalacja pakietu CLAG (algorytm analizy skupieñ) z repozytorium Rforge, wsakzujemy repos = "http://R-Forge.R-project.org"
install.packages("CLAG", repos="http://R-Forge.R-project.org")

# Mo¿na zainstalowaæ tak¿e tzw. wersjê dewelopersk¹ pakietu z systemów takich jak Github (platforma deweloperska, 
# która ³¹czy w sobie funkcjonalnoœæ systemu zarz¹dzania wersjami oraz inne narzêdzia umo¿lwiaj¹ce wygodne
# tworzenie kodu, w szczególnoœci w grupach
# Instalacje pakietów z Github jest mo¿liwa za pomoc¹ funkcji install_github z pakietu devtools
install.packages("devtools") # instalujemy pakiet devtools
library(devtools)            # ³adujemy pakiet devtools
# Instalujemy pakiet DepthProc z Github - autorstwa zespo³u: Kosiorowski, Zawadzki, S³omczynski, Bocian, Wêgrzynkiewicz
# https://github.com/zzawadz/DepthProc
install_github("zzawadz/DepthProc")


#####################################################################################################
#####################################################################################################
#####################################################################################################

# Tworzenie funkcji w R, realizuj¹cych wskazane przez nas procedury

arytm <- function(a = 2, b = 3) {
  if(!is.numeric(c(a,b)) | b == 0) stop("a or b are not numeric or b = 0")
  sum_res <- a + b
  diff_res <- a - b
  mult_res <- a * b
  div_res <- a/b
  list(sum = sum_res, diff = diff_res, prod = mult_res, quot = div_res)
}

arytm(a = 5, b = 7)
arytm(5, 7)         # Je¿eli wartoœci argumentów, podawane s¹ w takiej samej kolejnoœci 
arytm(b = 7, a = 5) # je¿eli wskazujemy wartoœci argumentów, w innej kolejnoœci, ni¿ argumenty zosta³y wskazane
                    # w definicji funkcji trzeba wskazaæ nazwê argumentów
arytm("k", 2)       # zwraca b³¹d argument b, jest wartoœci¹ tekstow¹ (character)
arytm(3,0)          # zwraca b³¹d, b jest równe 0

arytm()             # Jako, ¿e nie zosta³y wskazane wartoœci argumentów, funkcja jest wywo³ywana dla wartoœci
                    # domyœlnych z definicji funkcji

# Wywo³anie funkcji w ramach procedury debugowania ("poszukiwania b³êdów"), 
# kolejne "podprecedury" w ciele funkcji s¹ wykonywane,
# dziêki czemu mo¿na sprawdziæ, jakie s¹ wartoœci przyjmowane przez lokalne ("poœrednie") zmienne funkcji,
# przedstawiaj¹ce wyniki cz¹stkowe, które sk³adaj¹ siê na koñcowy wynik funkcji
debug(arytm) 
arytm(10, 2)        # Wywo³ujemy funkcjê arytm w trybie debugowania dla wartoœci arugmentów a i b odpowiednio 10 i 2
arytm(10,0)         # Wywo³ujemy funkcjê arytm w trybie debugowania dla wartoœci arugmentów a i b odpowiednio 10 i 0

# Wy³¹czenie uruchamiania funkcji w trybie debugowania
undebug(arytm)

################################################################################################################
################################################################################################################
################################################################################################################
# Przep³yw sterowania - funkcje z grupy apply, jako alternatywy dla pêtli w R

# sapply(vec, func) - wyznacza wartoœæ funkcji func jest wyznaczana, dla kazdego kolejnego elementu wektora 
# albo listy vec, zwraca wektor lub macierz wyników (mo¿na uzyskaæ takie same wyniki, jak za pomoc¹ pêtli for)
# lapply(vec, func) - takie samo dzia³anie jak sapply, tylko zwracana jest lista wyników
# mapply(func, vec1, vec2) - wyznacza wartoœci funkcji func dla par i-tego elementu vec1 oraz i-tego elementu vec2,
# gdzie  vec1 i vec2 maj¹ taka sam¹ liczbê elementów, indeksowanych przez i: i=1,2,...,n, zwraca 
# wektor n-elementowy wyników (mo¿na uzyskaæ takie same wyniki, jak w przypadku zagnie¿d¿onych pêtli for)
# outer(X, Y, func) - Wyznaczanie wartoœci funkcji func dla ka¿dego (x, y) \in X x Y, zwraca macierz wyników
# o wymiarach (#X) x (#Y)
# replicate(nrep, func) - wyznacza wartoœæ funkcji func, nrep razy 


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

lincongr() # ci¹g liczb pseudolosowych z generatora LCG

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

ar1sim()  # Szereg z symulacji, w tytule wykresu zapis modelu z wartoœciami parametrów
ar1sim(parvalTit = FALSE)  # Szereg z symulacji, w tytule wykresu zapis modelu z symbolami parametrów

# Wyznaczenie wartoœci dzia³añ arytmetycznych dla wartoœci x[i] oraz y[i], i = 1,...,n 
mapply(arytm, 1:100, 51:150)

# Wyznaczanie wartoœci funkcji f dla ka¿dego (x, y) \in X x Y
outer(X = 1:5, Y = 5:10, function(x,y) x^2 + y^3)

# replicate: przyk³ad symulacji rozk³adu wskazanych estymatorów wybranego parametru (traktowanego jako ustalonego) 
# dla zadanego rozk³adu (z domyœlnymi wartoœciami parametrów zaimplementowanymi w R), 
# dla okreœlonej wielkoœci próby, typu rozk³adu, zwraca wartoœci estymatora z kolejnych symulowanych prób,
# histogram rozk³adu wartoœci estymatora z na³o¿on¹ funkcj¹ gêstoœci oszacowan¹ z wykorzystaniem estymatora j¹drowego
simestdist <- function(dist, nrep = 10000, n = 1000, estimators = c(mean, median)){
  simres <- sapply(estimators, function(est) replicate(nrep, est(dist(n))))
  nest <- ncol(simres)
  par(mfrow = c(nest %/% 2, 2))
  lapply(1:nest, 
         function(i) {hist(simres[,i], col = i + 1, freq = FALSE, xlab = expression(theta(x)), 
                           main = paste("Estimator", i, ", sample n =", n))
           lines(density(simres[,i]), col = "blue", lwd = 3)})
  simres}

(EVest <- simestdist(rnorm, n = 100, estimators = c(mean, median)))  # wartoœæ oczekiwana: 
# rozk³ady estymatora œredniej arytmetycznej i mediany z próby 
# n = 100 elementowej, z rozk³adu standyrozowanego normalnego
(SDest <- simestdist(rnorm, n = 100, estimators = c(sd, mad)))       # odchylenie standardowe: rozk³ady estymatora sd (odchylenia
# standardowego) i mad (median bezwglêdnych odchyleñ od mediany)

# Podstawowe statystyki opisowe dla rozk³adu symulacyjnego wartoœci estymatorów
install.packages("psych")
library(psych)
?describe
(desEVest <- round(t(describe(EVest)), 4))
(desSDest <- round(t(describe(SDest)), 4))

# Wyniki symulacji wartoœci estymatorów dla ró¿nych wielkoœci próby, jako kolejne elementy listy
lapply(10^(1:5), simestdist, dist = rnorm, nrep = 1000, estimators = c(mean, median))  

par(mfrow = c(1,1)) # jeden wykres na okno

############################################################################################################

?Distributions   # rozk³ady zaimplementowane w pakiecie stats (d - funkcja gêstoœci, p - dystrybuanta,
                 # q - funkcja kwantylowa, r - generowanie liczb pseudolosowych)
# Wykaz rozk³adów w pakietach R - CRAN Task View - Distributions:
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

# Wyra¿enia matematyczne zapisane w formie drzew sk³adniowych R
# substitute - zwraca wierdzcho³ki drzewa sk³adniowego dla wyra¿enia (expression)
e1 <- substitute(a+2*b)
length(e1)
typeof(e1)      # typ languange
e1
# Wierzcho³ki drzewa sk³adniowego od korzenia do liœci
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

# Elementy przetwarzania równoleg³ego - pakiet parallel
# "zrównoleglone" odpowiedniki sapply, lapply, mapply, odpowiednio parSapply, parLapply, clusterMap
library(parallel)
# (n.cores <- detectCores())  # okreœlenie liczby rdzeni
# cluster <- makePSOCKcluster(names=n.cores) # okreœlenie klastra obliczeniowego

#parLapply(cl=cluster, X, fun, ...) - odpowiednik sapply, wykonywany na wielu rdzeniach procesora
#parLapply(cl=cluster, X, fun, ...) - odpowiednik lapply, wykonywany na wielu rdzeniach procesora
#clusterMap(cl=cluster, fun, ...) - odpowiednik mapply, wykonywany na wielu rdzeniach procesora
# stopCluster(cl=cluster) - zakoñczenie obliczeñ równoleg³ych za pomoc¹ klastra

# Przyk³ad zastosowania równoleglej wersji lapply: parLapply 
# wyniki symulacji wartoœci estymatorów dla ró¿nych wielkoœci próby, jako kolejne elementy listy 
(n.cores <- detectCores())
cluster <- makePSOCKcluster(names=n.cores)
parLapply(cl=cluster, 10^(1:5), simestdist, dist = rnorm, nrep = 1000, estimators = c(mean, median))
stopCluster(cl=cluster)

# Pakiet rbenchmark umo¿liwiaj¹cy pomiar czasu systemowego dla wywo³ania funkcji
install.packages(rbenchmark) 
library(rbenchmark)
cluster <- makePSOCKcluster(names=detectCores())
benchmark(replications = 1,
          lapply(10^(1:4), simestdist, dist = rnorm, nrep = 1000, estimators = c(mean, median)),
          parLapply(cl=cluster, 10^(1:4), simestdist, dist = rnorm, nrep = 1000, estimators = c(mean, median))
)

stopCluster(cl=cluster)