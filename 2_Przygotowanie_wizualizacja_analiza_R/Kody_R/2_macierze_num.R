
.Machine     # Skr�towa specyfikacja sposobu reprezentacji warto�ci liczbowych w pami�ci (typy j�zyka C:
             # liczby ca�kowite: typ long - 4 albo 8 bajt�w (systemy 64 bitowe poza Windows),
             # typ longlong - 8 bajt�w; reprezantacja zmiennoprzecinkowa liczb rzeczywistych: brak typu single, 
             # typ double - 8 bajt�w (11 bit�w - wyk�adnik, 53 bity - mantysa oraz znak), 
             # typ long double 12 bajt�w (32 bitowa wersja R) albo 16 bajt�w (64-bitowa wersja R))
             # typ sprawdzamy za pomoc� typeof()
             # 32-bit integers and IEC 60559 floating-point (double precision) arithmetic
             # 8 bajtowy wska�nik do struktury C SEXP przechowuj�cej obiekty R (SEXP - podtypy: 
             # REALSXP - wektory numeryczne R, INTSXP - wektory liczb ca�kowitych, LGLSXP - wektory logiczne, 
             # STRSXP - wektor znakowy, CPLXSXP - wektor liczb zespolonych, VECSXP - lista i pozosta�e),
             # 
# Szczeg�y:  ?.Machine
#             https://stat.ethz.ch/R-manual/R-devel/library/base/html/zMachine.html
#             http://adv-r.had.co.nz/C-interface.html
#             http://adv-r.had.co.nz/memory.html#object-size

.Machine$double.eps      # epsilon maszynowy dla typu double (minimalna dodatnia liczba zmiennoprzecinkowa x, taka �e 1+x != 1)
.Machine$double.neg.eps  # epsilon maszynowy dla typu double (minimalna dodatnia liczba zmiennoprzecinkowa x, taka �e 1-x != 1)

library(pryr)
(v <- c(5, 7.5, 12.6))
address(v)                # Sprawdza adres wska�nika (do SEXP reprezentuj�cego dany obiekt R) w pami�ci RAM
sexp_type(v)
object_size(v)            # Rozmiar obiektu w bajtach


######################################################################################################################
######################################################################################################################
######################################################################################################################

sessionInfo()               # Informacje dotycz�ce sesji R: attached base packages (za�adowane pakiety bazowe),
                            # other attached packages (pozosta�e za�adowane pakiety)

?options
options()                   # Opcje globalne" m.in. digits - liczba wy�wietlanych cyfr wyniku, 
                            # defaultPackages - domy�lnie �adowane pakiety, scipen - preferencje dotycz�ce wy�wietlania
                            # wynik�w w notacji naukowej, b�d� sta�oprzecinkowej (liczba ca�kowita: dodatnia warto��
                            # preferncja notacji sta�oprzecinkowej, ujemna - preferencja notacji naukowej)
getOption("scipen", default = NULL)
?par
par()                       # Opcje dotycz�ce grafiki

format(10^-15, scientific = FALSE)   # Wy�wietlanie wyniku w postaci sta�oprzecinkowej
format(0.0025, scientific = TRUE)  # Wy�wietlanie wyniku w postaci naukowej

signif(0.00789, 1)        # Zaokr�glenie wyniku z dok�adno�ci� do pierwszej cyfry znacz�cej
round(0.00789, 1)         # Zaokr�glenie wyniku z dok�adno�ci� do jednege miejsca po przecinku

# Wywo�ywanie polece� systemowych i zwrot wynik�w do konsoli R
# system(command, intern = FALSE,
#        ignore.stdout = FALSE, ignore.stderr = FALSE,
#        wait = TRUE, input = NULL, show.output.on.console = TRUE,
#        minimized = FALSE, invisible = TRUE)

# Utworzenie po��czenia z plikiem
# polfile <- file(description = "", open = "", blocking = TRUE,
#      encoding = getOption("encoding"), raw = FALSE,
#      method = getOption("url.method", "default"))

# polurl <- url(description = "", open = "", blocking = TRUE,
#     encoding = getOption("encoding"),
#     method = getOption("url.method", "default"))

# open(polurl) # Otwarcie po��czenia
# close(polurl) # Zamkni�cie po��czenia

######################################################################################################################
######################################################################################################################
######################################################################################################################


# Generowanie liczb pseudolosowych (PRNG)

# Zwraca typ stosowanego generatora liczb pseudolosowych z jednowymiarowego rozk�adu jednostajnego 
# (domy�lnie generator Mersenne-Twister /generuj�cy liczby pierwsze Mersenne'a/)
# oraz procedury generowania liczb pseudolosowych z jendowymiarowego rozk�adu normalnego 
# (domy�lnie pr�bkowanie inwersyjne)
RNGkind()

RNGkind(kind = "Wichmann-Hill", normal.kind = NULL) # zmiana na generator Wichmana-Hilla
RNGkind(kind = "default") # Powr�t do domy�lnego genratora MT
?RNGkind # Informacje dotycz�ce dost�pnych generator�w liczb pseudolosowych (opr�cz MT19937, /32-bitow� d�ugo�� s�ow/,
# m.in. Super-Duper, Marsaglia-Multicarry, stosowany w arkuszu kalkulacyjnym generator Wichmanna-Hilla,
# Knuth-TAOCP)

.Random.seed[1:4]
# Ustalanie warto�ci ziarna generatora
set.seed(seed = 1234)

# Charakterystyki jako�ci generatora liczb pseudolowych (PRNG):
# - generator to najcz�ciej rekurencyjnie zadany deterministyczny ci�g liczbowy, tak wi�c przy ustalonych parametrach generatora 
#   (w szczeg�lno�ci wyrazu pocz�tkowego - ziarna /seed/), uzyskiwane ci�gi liczb b�d� identyczne, 
#   odmienne ci�gi liczbowe, uzyskuje si� najcz�ciej manipuluj�c warto�ci� ziarna, przy pozosta�ych parametrach
#   generatora niezmienionych
# - generowane ci�gi liczbowe s� okresowe - d�ugo�� okresu (cyklu) generatora to jedna z najwa�niejszych jego charakterystyk,
#   oczywi�cie d�u�szy cykl jest preferowany, generator MT - cykl wynosi 2^19937-1, LCG -  maksymalny cykl to (m-1), gdzie m to dzielnik,
#   dla funkcji modulo w formule okre�laj�cej warto�� kolejnego wyrazu ci�gu 
# - ocena niezale�no�ci kolejnych generowanych liczb w ramach sekwencji oraz zgodno�ci generowanych
#   pr�bek z rozk�adem jednosajnym

# Zestaw test�w statystycznych DieHard dla generator�w liczb pseudolosowych mo�na znale�� w pakiecie R o nazwie RDieHarder
library(RDieHarder)
help(package = "RDieHarder")


library(random)
# random package (Eddelbuettel, 2007) (which provides functions that
# access a non-deterministic random number generator (NDRNG) based on a physical source of randomness

set.seed(9999) # Ustawiamy ziarno generatora, jako liczb� ca�kowit� 9999
runif(10)     # Generujemy ci�g 10 liczb pseudlosowych z rozk�adu jednostajnych 

runif(10)   # ziarno uleg�o zmianie uzyskujemy innyc ci�g liczbowy

set.seed(9999)
runif(10) # Po ustaleniu tego samego ziarna, uzyskujemy identyczny ci�g liczbowy

(r1k <- runif(1000, 0, 1))
par(mfrow = c(1,1))
hist(r1k, main = "Histogram - rozk�ad jednostajny, n = 1000", col = "red", freq = FALSE, xlab = "x")

(r100k <- runif(10^5, 0, 1))
hist(r100k, main = "Histogram - rozk�ad jednostajny, n = 100000", col = "green", freq = FALSE, xlab = "x")

# Pr�bkowanie inwersyjne z rozk�adu normalnego - generowane warto�ci z rozk�adu jednostajnego zadanego  
# na przedziale [0,1], okre�laj� warto�ci dystrybuanty rozk�adu normalnego o okre�lonych parametrach (mu, sigma), 
# kt�re to warto�ci po podstawieniu do funkcji kwantylowej, czyli funkcji odwrotnej do dystrybuanty 
# rozk�adu normlanego o przyj�tych parametrach, b�d� warto�ciami pseudolosowymi z przyj�tego
# rozk�adu normalnego

qgen <- runif(10^4, 0, 1)
ngen <- qnorm(qgen , mean = 170, sd = 10) # Funkcja kwantylowa rozk�adu normalnego o parametrach (mu = 170, sigma = 10)
hist(ngen, main = "Pr�bkowanie inwersyjne - rozk�ad normalny (170, 10)", xlab = "x", col = "blue", freq = FALSE)
curve(dnorm(x, mean = 170, sd = 10), from = min(ngen), to = max(ngen), n = 10^5, add = TRUE, col = "red")

# ecdf - dystybuanta rozk�adu empirycznego
plot(ecdf(ngen[1:50]), main = "Histogram rozk�adu empirycznego podpr�by {x_i}, i = 1, ..., 50")

######################################################################################################################
######################################################################################################################
######################################################################################################################

### Wektory ###

# Wektory numeryczne
(a <- c(1, 7, 9, 5, rep(0, 3), 10, 14))
# print(a)
length(a)
class(a)
mode(a)
typeof(a)

(aint <- as.integer(a))
typeof(aint)
mode(aint)

typeof(5)
typeof(5L)

is.double(5)
is.integer(5L)

rev(a)
sort(a)
sort(a, decreasing = TRUE)
order(a)
a[2:5]
a[-(3:4)]
summary(a)
names(a) <- letters[1:length(a)]

# Liczby zespolone
(z <- 5 + 6i)
class(z)
Conj(z)
Mod(z)
abs(z)
Re(z)
Im(z)
Arg(z)

# Wektory liczb zespolonych
(x <- rnorm(5))
(y <- rnorm(5))
# Tworzenie wektora liczb zespolonych
(z1 <- complex(real = x, imaginary = y))
as.complex(x)
plot(z1, pch = 16, main = "P�aszczyzna zespolona")

# Pierwiastki wielomianu

# Pakiet polynom umo�liwia dzia�ania na wielomianach
if(!require(polynom)) install.packages("polynom")
library(polynom)
# w(x) = 4 + 3*x + 2*x^2 + 1*x^3 
(wiel <- polynomial(4:1))
# Wyznacza pierwiastki wielomianu (w funkcji solve z pakietu polynom  pierwiastki s� wyznaczane jako warto�ci w�asne
# macierzy stowarzyszonej, z kt�r� zwi�zany jest wielomian i uporz�dkowane rosn�co wed�ug ich cz�ci rzeczywistej):
# http://www.ams.org/journals/mcom/1995-64-210/S0025-5718-1995-1262279-2/S0025-5718-1995-1262279-2.pdf
# na stronie 3 mo�na znale�� posta� macierzy stowarzyszonej
(pierw <- solve(wiel)) 

# Dla sprawdzenia wyznaczamy posta� macierzy stowarzyszonej i wyznaczamy warto�ci w�asne
(compmat <- matrix(c(0,1,0,0,0,1, -4:-2), ncol = 3))
eigen(compmat)$values

# alternatyw� dla solve.polynomial jest funkcja polyroot z pakietu base, kt�ra implementuje algorytm 
# Jenkinsa-Trauba poszukiwania pierwiastk�w wielomian�w z zespolonymi (wariant CPOLY) i rzeczywistymi (wariant RPOLY)
# wsp�czynnikami (t�umaczenie na kod C procedury Fortrana autorstwa Ross, Ihaka); 
#na wej�ciu podajemy wektor wsp�czynnik�w, uporz�dkowany rosn�co wg wyk�adnika zmiennej wielomianu
polyroot(4:1)

# Wyznaczanie warto�ci wielomianu dla okre�lonej warto�ci argumentu (najpierw dokonuje si� konwersji obiektu polynomial 
# do klasycznej funkcji R)
wielfun <- as.function(wiel)
wielfun(c(0, 10, 20))  # Warto�ci wielomianu dla x = 0, 10, 20 

deriv(wiel)               # Wyznaczanie za pomoc� oblicze� symbolicznych pochodnej wielomianu
polynom::integral(wiel)   # Domy�lne granice ca�kowania to 0 i x
polynom::integral(wiel, limits = c(0, 7))
## Wyznaczanie posta� wielomianu w oparciu o jego pierwiastki
polynom::poly.calc(pierw)


# Wektory tekstowe
as.character(a)
(ba <- LETTERS[a + 1])
(tx <- c("czerwony", "zielony", "niebieski"))
class(tx)
is.character(tx)
paste(tx[1], tx[2], sep = "-")

# Wektory czynnikowe (factor)

# Nieuporz�dkowane kategorie
(ftx <- factor(rep(tx, 10), ordered = FALSE))
is.ordered(ftx)

# Uporz�dkowane kategorie
war <- c("niski", "�redni", "wysoki")
(stan <- war[floor(runif(100, 1, 4))])
(d <- factor(stan, levels = war, ordered = TRUE))
levels(d)
unclass(d)
is.ordered(d)

# Wektory logiczne
(e <- as.logical(a))

# Operatory logiczne &, |, &&, ||, ==, !=, <, <=, >, >=, any, all

(vl <- a > 5 & a < 10)
any(vl); all(vl)
!vl

######################################################################################################################
######################################################################################################################
######################################################################################################################

### Macierze ###

# Dzia�ania na macierzach realizowane s� w R przez funkcje biblioteki BLAS (Basic Linear Algebra Subprograms) Fortrana:
# poziom 1 - operacje wektor * wektor, poziom 2 - operacje macierz * wektor, poziom 3 - operacje macierz * macierz

# Funkcje biblioteki BLAS wykorzystuj� funkcje biblioteki LAPACK, implementuj�cej procedury numeryczne 
# algebry liniowej (m.in. dekompozycje macierzy)


(B <- matrix(1:25, ncol = 5))
args(matrix)
B[2, 3]
B[4:5, ]
B[, 2:3]
B[, -(2:3)]
nrow(B)
ncol(B)
dim(B) # Wymiary macierzy B
colMeans(B)
rowMeans(B)
colSums(B)
rowSums(B)
t(B) # Transpozycja macierzy B
det(B) # Wyznacznik macierzy B


#wyznacz warto�� funkcji (tutaj sumy) dla ka�dego wiersza macierzy
apply(B, 1, sum)

#wyznaczanie warto�ci funkcji (tutaj sumy) dla ka�dej kolumny macierzy
apply(B, 2, sum)

# wyznacz warto�� zadanej funkcji w oparciu o warto�ci z danej kolumny macierzy (kolejno dla ka�dej z kolumn)
apply(B, 2, function (x) mean(x) + 5)

# Tworzenie macierzy diagonalnej
# macierz jednostkowa stopnia 5
diag(5)
# macierz diagonalna o elementach diagonalnych z wektora a
is.vector(a)
a
diag(a) 
# Wyci�ganie element�w diagonalnych z macierzy B
is.matrix(B)
B
diag(B)

# Tworzenie macierzy z elementami pseudolosowymi
F <- matrix(rnorm(100, 170, 15), ncol = 5)
G <- matrix(rnorm(15, 170, 15), nrow = 5)
H <- matrix(rnorm(100), ncol = 5)
dim(F)
dim(G)
dim(H)

# Iloczyn macierzy (Cauchy'ego)     %*%
(L <- F %*% G)
dim(L)

# Iloczyn Hadamarda      *
(K <- F * H)
dim(K)

# Iloczyn Kroneckera    %x%
(Z <- B %x% G)
dim(Z)
# alternatywnie
kronecker(B, G)

# Iloczyn skalarny      "crossprod"
(dotB <- crossprod(B)) # B'B
dim(dotB)
all.equal(dotB, t(B) %*% B)
(dotFH <- crossprod(F, H)) # F'H
all.equal(dotFH, t(F) %*% H)

# Iloczyny diadyczny (zewn�trzny)  "Outer product"
(outB <- tcrossprod(B)) # BB'
dim(outB)
(outFH <- tcrossprod(F, H)) # FH'
dim(outFH)

# Macierz kwadratowa stopnia 5 z elementami pseudolosowymi
(A <- (matrix(round(rnorm(25, 10, 2)), ncol = 5)))
dim(A)
det(A)
# Wyznacznik jest obliczany jako iloczyn element�w diagonalnych macierzy U, w ramach dekompozycji LU, 
# w macierzy L zak�ada si� jednostkowe elementy diaganalne dla identyfikowalno�ci
library(matrixcalc)
(decomplu <- matrixcalc::lu.decomposition(A))
(detA <- prod(diag(decomplu$U)))

(Ainv <- solve(A))

# Dekompozycja QR macierzy (procedura dqrdc2 z biblioteki LINPACK albo LAPACK, wykorzystuj�ca refleksj� Householdera)
(QR <- qr(A))
QR$rank
qr.Q(QR) # Wydobycie macierzy Q
qr.R(QR) # Wydobycie macierzy R
qr.X(QR)

(S <- crossprod(matrix(rnorm(25), ncol = 5))) # Macierz symetryczna, dodatnio okre�lona
# Dekompozycja Choleskiego (funkcja chol wykorzystuje procedury biblioteki LAPACK: DPOTRF oraz DPSTRF)
(L <- chol(S))

# Macierz odwrotna
(invS <- solve(S))
(idec <- chol2inv(L)) # Zwraca macierz odwrotn� do macierzy X=LL', przyjmuj�c jako argument macierz L z dekompozycji Choleskiego

identical(idec, invS) # Por�wnanie czy warto�ci s� identyczne (bez uwzgl�dnia b��du zwi�zanego z reprezentacj� zmiennoprzecinkow�)
all.equal(idec, invS) # Por�wnanie Uwzgl�dnia b��d zwi�zany z zastosowaniem reprezentacji / arytmetyki zmiennoprzecinkowej

(numerrunit <- S%*%invS)
zapsmall(numerrunit) # Zaokr�glenia do 0, warto�ci, kt�rych odst�pstwa od 0, wynikaj� z zastosowania do ich wyznaczania arytmetyki zmiennoprzecinkowej   

# "Kwadratowe" uk�ady r�wna� liniowych A * x = b - rozwi�zanie (dok�adnie jedno, gdy� zak�adamy, �e r(A) = r(A|b) = n)
print(A)
dim(A)
det(A)    # wyznacznik niezerowy, st�d wiadomo, �e rz�d r(A) = n = 5
(b <- matrix(9:5))

# W celu rozwi�zanie kwadratowego uk�adu r�wna� liniowych stosuje si� funkcj� solve, kt�ra wywo�uje procedur� biblioteki LAPACK: 
# - DGESV - rzeczywiste macierze parametr�w uk�adu (A, b) 
# - ZGESV - zespolone macierze parametr�w uk�ad�w (A, b)
# Procedury te wykorzystuj� dekompozycj� LU macierzy A, w celu wyznaczenia rozwi�zania uk�adu A * x = b <=> LU * x = b
(xast <- solve(A, b))

# "Prostok�tne" uk�ady r�wna� liniowych - rozwi�zanie (z wykorzystaniem dekompozycji QR macierzy parametr�w)

# Uk�ad nadokre�lony (brak rozwi�zania): r(A) /= r(A|b) - poszukuje si� wektora x* minimalizuj�cego 
# kryterium najmniejszych kwadrat�w: min ||Ax - b||
set.seed(1234)
(A <- matrix(runif(12), nrow = 4))
(b <- matrix(1:4))
dim(A)
ncol(A)             # liczba niewiadomych uk�adu
qr(A)$rank          # rz�d macierzy A
qr(cbind(A,b))$rank # rz�d macierzy rozszerzonej A|b

qr.solve(A, b)      # wynaczamy wektor x* minimalizuj�cy kryterium najmniejszych kwadrat�w

# Uk�ad niedookre�lony - rozwi�zaniem uk�adu jest podprzestrze� wektorowa wymiaru n - r, gdzie n jest liczb� 
# niewiadomych  uk�adu, natomiast  r(A) = r(A|b) = r < n, to rz�dzy macierzy parametr�w A i macierzy rozszerzonej A|b, kt�re s�
# takie same, r�wne r i mniejsze od liczby niewiadomych n
# zadanie sprowadza si� do okre�lania bazy tej podprzestrzeni wektorowej
set.seed(9999)
(A <- matrix(runif(12), nrow = 3))
(b <- matrix(1:3))
dim(A)
ncol(A)             # liczba niewiadomych uk�adu
qr(A)$rank          # rz�d macierzy A
qr(cbind(A,b))$rank # rz�d macierzy rozszerzonej A|b
  
qr.solve(A, b)  # Uzyskujemy jedno z rozwi�za� bazowych


#### Pseudoinwersja Moore'a-Penrose'a
if(!require(MASS)) install.packages("MASS")
library(MASS)

ginv(A) #Pseudoinwersja macierzy A

#### Macierz kwadratowa stopnia 5 z warto�ciami pseudolosowymi
set.seed(5678)
(M <- (matrix(round(rnorm(25, 10, 2)), ncol = 5)))

# Dekompozycja spektralna macierzy (zadanie w�asne: Ax = \lambda x) - funkcja eigen
# (funkcja eigen wykorzystuje procedury z biblioteki LAPACK: 
# - DSYEVR /dla rzeczywistej macierzy symetrycznej/, 
# - DGEEV /dla rzeczywistej macierzy niesymetrycznej/, 
# - ZHEEV /dla zespolonej macierzy hermitowskiej/
# - ZGEEV /dla zespolonej macierzy niehermitowskiej/ )

eigen(M, symmetric = FALSE)


# Uog�lnione zagadnienie w�asne Ax =\lambda Bx
library(geigen)
geigen(M, S)

eigen(solve(S) %*% M)

# Pakiet matrixcalc
if(!require(matrixcalc)) install.packages("matrixcalc")
library(matrixcalc)
help(package=matrixcalc)

is.square.matrix(M) # Czy macierz jest kwadratowa
is.square.matrix(S)

is.symmetric.matrix(M) # Czy macierz jest symetryczna
is.symmetric.matrix(S)

# Okre�lono��  (symetrycznej) macierzy /procedura sprawdza znaki warto�ci w�asnych wyznaczonych przez eigen/
is.positive.definite(S)     # dost�pna jest tak�e funkcja  is.positive.semi.definite  sprawdzaj�ca dodatni� p�okre�lono��
is.negative.definite(-S)     # dost�pna jest tak�e funkcja is.negative.semi.definite sprawdzaj�ca ujemn� p�okre�lono��

is.indefinite #? undM <- runif(9, 1:5)
is.diagonal.matrix(diag(rnorm(5)))

# vech - Halfvectorization
(halfv <- M[lower.tri(M, diag = TRUE)])
matrixcalc::vech(M)     # alternatywnie
# vec - Wektoryzacja
c(M)
matrixcalc::vec(M)      # alternatywnie

lower.triangle(M)       # Tworzy z danej macierzy, macierz dolnotr�jk�tn� przez zast�pienie zerami wszystkich element�w powy�ej diagonali  
upper.triangle(M)

# Pot�gowanie macierzy
matrix.power(M, k = 3)

# Dekompozycja LU (zak�ada si�, �e L ma jednostkowe elementy diagonalne, dekompozycja LU wyznaczana 
# za pomoc� metody Doolittle)
lu.decomposition(M)

# Normy macierzy rzeczywistych
# Pakiet base (procedury biblioteki LAPACK)
# normy operatorowe
norm(M, type = "1")     # norma ||X||_1,  p = 1
norm(M, type = "i")     # norma ||X||_\infty, p = \infty
norm(M, type = "2")     # norma spektralna ||X||_2, odpowiada najwi�kszej warto�ci osobliwej macierzy

# normy "po wsp�rz�dnych"
norm(M, type = "F")     # norma Frobeniusa (odpowiada normie L2 zwektoryzowanej macierzy)
norm(M, type = "M")     # norma maksimum max i,j |x_ij| odpowiada normie L1 zwektoryzowanej macierzy)

# Alternatywnie pakiet matrixcalc:
entrywise.norm(M, p = 1)
frobenius.norm(M)
spectral.norm(M)

# Implementacja popularnych metod numerycznych - pakiet pracma (Practical Numerical Math Functions)
library(pracma)
help(package = "pracma")

# charpoly - funkcja wyznaczaj�ca parametry wielomianu charakterystycznego dla macierzy A
# Wsp�czynniki wielomianu charakterystycznego dla zadania w�asnego wyznaczane s� za 
# pomoc� algorytmu Faddiejewa�LeVerriera  
charpoly(M)

# bisect metoda bisekcji - poszukiwani rzeczywistych pierwiastk�w funkcji ci�g�ej jednej zmiennej, 
# w zadanym przedziale warto�ci zmiennej [a,b]

# Przybli�enie warto�ci pierwiastka
fwielom <- function(x, p = 2, k = 5) x^p - k
# x \in [0,5], takie, �e x^2-5 = 0, daje przybli�enie numeryczne warto�ci pierwiastka z 5
bisect(fwielom, a = 0, b = 5, maxiter = 100, tol = NA)

#######################################################################################################
#######################################################################################################
#######################################################################################################

# R�niczkowanie symboliczne w R
# Wyra�enia matematyczne reprezentowane za pomcoc� drzew sk�adniowych
# Automatic differentiation (r�niczkowanie automatyczne, czasami te� nazywane r�niczkowanie algorytmiczne):
# wykorystuje regu�� �a�cuchow� r�biczkowania funkcji z�o�onych:
# wariant forward - w ramach regu�y �a�cuchowej r�niczkowania, procedura "przebiega" drzewo sk�adniowe od li�ci do
# wierzcho�ka,
# wariant backward - w ramach regu�y �a�cuchowej r�niczkowania, procedura "przebiega" drzewo sk�adniowe od wierzcho�ka
# do li�ci,
# literatura: Rall L.B., Automatic Differentiation: Techniques and Applications,

?expression  # klasa "expression" s�u�y do przechowywania wyra�e�

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

# D(expr, name) - R�niczkowanie symoliczne - pakiet base
# symboliczne r�niczkowanie funkcji jednej zmiennej (na wej�ciu obiekt "expression",
# na wyj�ciu zwraca obiekt "expression")
D(expression((exp(x^2+5)+cos(x))/x), name = "x")

# deriv(expr, ...) - symboliczne r�niczkowanie funkcji wielu zmiennych (na wej�ciu obiekt "expression",
# na wyj�ciu zwraca obiekt "expression" lub funkcj�)

(fncx <- expression((exp(x^2+5)+cos(x))/x))  # tworzymy obiekt "expression", przechowuj�cy funkcj� jednej zmiennej "x"
(dx <- deriv(fncx, namevec = "x", hessian = TRUE, func = TRUE))  # func = TRUE, �eby zwr�ci� funkcj�, a nie obiekt "expression"

dx(1:5)   # Funkcja zwraca warto�ci funkcji dla zadanych warto�ci argumentu, a warto�ci pierwszej i drugiej pochodnej
          # jako atrybuty "gradient", "hessian", kt�ry warto�ci mo�na wyci�gn�c:

attr(dx(x = 1:5), "gradient") # zwraca warto�� pierwszej pochodnej w dla x = 1, ..., 5
attr(dx(x = 1:5), "hessian")  # zwraca warto�� drugiej pochodnej w dla x = 1, ..., 5

(fncxy <- expression(sin(cos(x + y^2)))) # tworzymy obiekt "expression", przechowuj�cy funkcj� dw�ch zmiennych "x" i "y"
(dxy <- deriv(fncxy, c("x", "y"), hessian = TRUE, func = TRUE))

attr(dxy(x = 1.5, y = 2.5), "gradient") # zwraca warto�� pierwszej pochodnej w dla x = 8, 5, 10, y = 3, 7, 8
attr(dxy(x = 1.5, y = 2.5), "hessian")  # zwraca warto�� drugiej pochodnej w dla x = 8, 5, 10, y = 3, 7, 8


attr(dxy(x = c(8,5,10), y = c(3, 7 , 8)), "gradient") # zwraca warto�� pierwszej pochodnej w dla x = 8, 5, 10, y = 3, 7, 8
attr(dxy(x = c(8,5,10), y = c(3, 7 , 8)), "hessian")  # zwraca warto�� drugiej pochodnej w dla x = 8, 5, 10, y = 3, 7, 8


# R�niczkowanie symboliczne - pakiet Deriv
# Symboliczne wyznaczanie pochodnych funkcji wielu zmiennych (na wej�ciu podaje si� obiekt "function")
# Okre�lamy skalarn� funkcj� wielu zmiennych, w tym przypadku dw�ch zmiennych:
# f(x1, x2) = 100 * (x2 - x1 * x1)^2 + (1-x1)^2 (Funkcja Rosenbrocka - minimum globalne dla (x1,x2) = (1, 1))

library(Deriv)
# Deriv::Deriv
# Deriv::Simplify

fnc <- function(x) {
  100 * (x[2] - x[1] * x[1])^2 + (1 - x[1])^2
}
# Je�eli na wej�ciu funkcji podajemy wektor x, kt�rego elementami s� zmienne funkcji,
# to, aby wyznaczy� gradient funkcji wzgl�dem zmiennych, stosujemy zapis 
# okre�laj�cy "po�o�enie" zmiennych w wektorze x np. c(x = "1", x = "2"), nderiv = 1 oznacza, 
# �e chcemy wyznaczy� gradient (wektor pochodnych cza�tkowych pierwszego rz�du)
# na wyj�ciu uzyskujemy obiekt "function"
(fncgrad <- Deriv::Deriv(fnc, x = c(x = "1", x = "2"), nderiv = 1))

# Wyznaczenie hesjanu nderiv = 2
(fnches <- Deriv::Deriv(fnc, x = c(x = "1", x = "2"), nderiv = 2))

# Wyznaczanie jednoczesne gradientu i hesjanu nderiv = 1:2

(fncgh <-Deriv::Deriv(fnc, x = c(x = "1", x = "2"), nderiv = 1:2))

# Wyznaczenie warto�ci gradientu i hesjanu w punkcie x = [1, 2]
fncgh(c(1,2))

# Uproszczenie funkcji wynikowej
Deriv::Simplify

######################################################################################################################
######################################################################################################################
######################################################################################################################

# R�niczkowanie numeryczne - pakiet numDeriv
# Umo�liwia przybli�enie warto�ci gradientu, hesjanu w punkcie
library(numDeriv)
# Numeryczne przybli�enie warto�ci gradientu funkcji w punkcie 
# f - funkcja j�zyka R, x - wektor warto�ci, w kt�rych ma by� wyznaczona numeryczne przybli�enie gradientu
# metoda wyznaczania: method = c("Richardson", "simple", "complex"), "simple" f'(x) approx (f(x+h)-f(x-h))/2h, 
# "complex" (Lyness i Moler), "Richardson" - metoda ekstrapolacji Richardsona
# 
numDeriv::grad(fnc,  x = c(1, 2), method = "Richardson")
numDeriv::hessian(fnc,  x = c(1, 2), method = "Richardson")

######################################################################################################################
######################################################################################################################
######################################################################################################################

# Optymalizacja bezwarunkowa - metody numeryczne

# Domy�lnie optim dokonuje minimalizacji funkcji wielu zmiennych
# za pomoc� metod numerycznych: BFGS (method = "BFGS" lub method = "L-BFGS-B"), 
# gradient�w sprz�onych (method = "CG"), metoda Brenta (method = "Brent"), Neldera-Meada (method = "Nelder-Mead)
# symulowanego wy�arzania (SANN)
# par - punkt startowy dla algorytmu numerycznego
# fn - funkcja podlegaj�ca optymalizacji
# gr - gradient funkcji, podaje si� obekt function reprezentuj�cy gradient, je�eli gr = NULL, gradient jest przybli�any numerycznie
# hessian = TRUE, zwraca warto�� hesjanu (przbli�onego numerycznie) w punkcie z ostatniej iteracji
# control - lista zawieraj�ca parametry procedury numerycznej: trace - pokazuje wi�cej szczeg��w z dzia�ania
# algorytmu numerycznego, fnscale - odwrotno�� wsp�czynnik przez, kt�ry mno�ymy funkcj� przed poddaniem optymalizacji:
# 1/fnscale * f
# ujemna warto�� fnscale, sprawia, i� mamy maksymalizacj� "wyj�ciowej" funkcji f,
# parametry zatrzymania algorytmu numerycznego: maxit (domy�lnie 100), 
# reltol (progowa realtywna zmiana warto�ci funkcji, w danej iteracji, poni�ej kt�rej nast�puje zatrzymanie algorytmu,
# domy�lnie pierwiastek z epsilonu maszynowego)
# REPORT = 10, pokazuje warto�� funkcji w punkcie dla co 10-tej iteracji
# 
?optim

fnc  # Przypomnijmy posta� funkcji dw�ch zmiennych f(x1, x2) = 100 * (x2 - x1 * x1)^2 + (1-x1)^2
(res <- optim(par = c(0.2, 0.5), fn = fnc, gr = fncgrad, method = "BFGS"))

# optim z wyszczeg�lnieniem parametr�w procedury numeryczne: argumentowi control przypisano list� parametr�w
(resext<- optim(par = c(0.2, 0.5), fn = fnc, gr = fncgrad, method = "BFGS", hessian = TRUE,
      control = list(trace = 1, fnscale = 1, REPORT = 1, maxit = 100, reltol = sqrt(.Machine$double.eps))))

matrixcalc::is.positive.definite(resext$hessian)    # Potwierdzenie warunku wystarczaj�cego dla minimum lokalnego funkcji wielu zmiennych


# Maksymalizacja logarytmu funkcji wiarygodno�ci dla pr�by z rozk�adu normalnego za pomoc� metod numerycznych
# Maksimum w tym przypadku mo�na wyznaczy� analitycznie: 
# ^mu = n^-1 \sum_i=1^n (x_i), sigma^2 = n^-1 sum_i=1^n (x_i - ^mu)^2

loglik <- function(x, pars) {
  mu <- pars[1]
  logsigma2 <- pars[2]
  -sum(logsigma2 + (x - mu)^2/exp(logsigma2))}
(loglikgrad <- Deriv::Deriv(loglik, x = c(pars = "1", pars = "2"), nderiv = 1))

obs <- rnorm(100, 150, 4) # 100 obserwacji pseudolosowych z rozk�adu normalnego N(mu = 150, sigma = 4)
(parest <- optim(par = c(0,0), x = obs, fn = loglik, gr = loglikgrad, method = "BFGS", hessian = TRUE,
      control = list(trace = 1, fnscale = -1, REPORT = 1, maxit = 100)))

(muest <- parest$par[1])
(sigmaest <- exp(parest$par[2]))^.5

# Oszacowanie MNW parametr�w rozk�adu z wykorzystaniem numerycznych metod poszukiwania maksimum funkcji wiarygodno�ci
# funkcja fitdist z pakietu fitdistrplus
library(fitdistrplus)
?fitdistrplus::fitdist



######################################################################################################################
######################################################################################################################
######################################################################################################################

# Ca�kowanie numeryczne (jednowymiarowe) - metody kwadratur

# np. metoda Newtona-Cotesa: w tym metoda trapez�w: \sum_i=1^n (x_i- x_i-1)/2 (f(x_i-1) + f(x_i))

# W R zaimplementowano natomiast adaptacyjne metody kwadratur:
# https://en.wikipedia.org/wiki/Adaptive_quadrature
# Wykorzystuje si� metod� Gaussa�Kronroda z biblioteki Fortrana QUADPACK:
# https://en.wikipedia.org/wiki/QUADPACK

# funkcja integrate wykorzystuje procedury dqags oraz dqagi z biblioteki QUADPACK
ftointegr <- function(x) exp(x^2+5)
curve(ftointegr, from = 0, to = 3, n = 1000, col = "red", ylab = "f(x)", main =  expression(paste("f(x)=", exp(x^2+5))))
integrate(ftointegr, subdivisions = 100L, lower = 1, upper = 2)
integrate(dnorm, subdivisions = 100L, lower = 0, upper = 10, mean = 0, sd = 5) # X~ N(mu=5, sd = 10), P({5 <= X <= 10})
integrate(dnorm, subdivisions = 100L, lower = -Inf, upper = 10, mean = 0, sd = 5) # X~ N(mu=5, sd = 10), P({X <= 10}) = F(x = 10)
