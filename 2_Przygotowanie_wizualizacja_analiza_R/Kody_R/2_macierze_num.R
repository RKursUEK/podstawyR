
.Machine     # Skrótowa specyfikacja sposobu reprezentacji wartoœci liczbowych w pamiêci (typy jêzyka C:
             # liczby ca³kowite: typ long - 4 albo 8 bajtów (systemy 64 bitowe poza Windows),
             # typ longlong - 8 bajtów; reprezantacja zmiennoprzecinkowa liczb rzeczywistych: brak typu single, 
             # typ double - 8 bajtów (11 bitów - wyk³adnik, 53 bity - mantysa oraz znak), 
             # typ long double 12 bajtów (32 bitowa wersja R) albo 16 bajtów (64-bitowa wersja R))
             # typ sprawdzamy za pomoc¹ typeof()
             # 32-bit integers and IEC 60559 floating-point (double precision) arithmetic
             # 8 bajtowy wskaŸnik do struktury C SEXP przechowuj¹cej obiekty R (SEXP - podtypy: 
             # REALSXP - wektory numeryczne R, INTSXP - wektory liczb ca³kowitych, LGLSXP - wektory logiczne, 
             # STRSXP - wektor znakowy, CPLXSXP - wektor liczb zespolonych, VECSXP - lista i pozosta³e),
             # 
# Szczegó³y:  ?.Machine
#             https://stat.ethz.ch/R-manual/R-devel/library/base/html/zMachine.html
#             http://adv-r.had.co.nz/C-interface.html
#             http://adv-r.had.co.nz/memory.html#object-size

.Machine$double.eps      # epsilon maszynowy dla typu double (minimalna dodatnia liczba zmiennoprzecinkowa x, taka ¿e 1+x != 1)
.Machine$double.neg.eps  # epsilon maszynowy dla typu double (minimalna dodatnia liczba zmiennoprzecinkowa x, taka ¿e 1-x != 1)

library(pryr)
(v <- c(5, 7.5, 12.6))
address(v)                # Sprawdza adres wskaŸnika (do SEXP reprezentuj¹cego dany obiekt R) w pamiêci RAM
sexp_type(v)
object_size(v)            # Rozmiar obiektu w bajtach


######################################################################################################################
######################################################################################################################
######################################################################################################################

sessionInfo()               # Informacje dotycz¹ce sesji R: attached base packages (za³adowane pakiety bazowe),
                            # other attached packages (pozosta³e za³adowane pakiety)

?options
options()                   # Opcje globalne" m.in. digits - liczba wyœwietlanych cyfr wyniku, 
                            # defaultPackages - domyœlnie ³adowane pakiety, scipen - preferencje dotycz¹ce wyœwietlania
                            # wyników w notacji naukowej, b¹dŸ sta³oprzecinkowej (liczba ca³kowita: dodatnia wartoœæ
                            # preferncja notacji sta³oprzecinkowej, ujemna - preferencja notacji naukowej)
getOption("scipen", default = NULL)
?par
par()                       # Opcje dotycz¹ce grafiki

format(10^-15, scientific = FALSE)   # Wyœwietlanie wyniku w postaci sta³oprzecinkowej
format(0.0025, scientific = TRUE)  # Wyœwietlanie wyniku w postaci naukowej

signif(0.00789, 1)        # Zaokr¹glenie wyniku z dok³adnoœci¹ do pierwszej cyfry znacz¹cej
round(0.00789, 1)         # Zaokr¹glenie wyniku z dok³adnoœci¹ do jednege miejsca po przecinku

# Wywo³ywanie poleceñ systemowych i zwrot wyników do konsoli R
# system(command, intern = FALSE,
#        ignore.stdout = FALSE, ignore.stderr = FALSE,
#        wait = TRUE, input = NULL, show.output.on.console = TRUE,
#        minimized = FALSE, invisible = TRUE)

# Utworzenie po³¹czenia z plikiem
# polfile <- file(description = "", open = "", blocking = TRUE,
#      encoding = getOption("encoding"), raw = FALSE,
#      method = getOption("url.method", "default"))

# polurl <- url(description = "", open = "", blocking = TRUE,
#     encoding = getOption("encoding"),
#     method = getOption("url.method", "default"))

# open(polurl) # Otwarcie po³¹czenia
# close(polurl) # Zamkniêcie po³¹czenia

######################################################################################################################
######################################################################################################################
######################################################################################################################


# Generowanie liczb pseudolosowych (PRNG)

# Zwraca typ stosowanego generatora liczb pseudolosowych z jednowymiarowego rozk³adu jednostajnego 
# (domyœlnie generator Mersenne-Twister /generuj¹cy liczby pierwsze Mersenne'a/)
# oraz procedury generowania liczb pseudolosowych z jendowymiarowego rozk³adu normalnego 
# (domyœlnie próbkowanie inwersyjne)
RNGkind()

RNGkind(kind = "Wichmann-Hill", normal.kind = NULL) # zmiana na generator Wichmana-Hilla
RNGkind(kind = "default") # Powrót do domyœlnego genratora MT
?RNGkind # Informacje dotycz¹ce dostêpnych generatorów liczb pseudolosowych (oprócz MT19937, /32-bitow¹ d³ugoœæ s³ow/,
# m.in. Super-Duper, Marsaglia-Multicarry, stosowany w arkuszu kalkulacyjnym generator Wichmanna-Hilla,
# Knuth-TAOCP)

.Random.seed[1:4]
# Ustalanie wartoœci ziarna generatora
set.seed(seed = 1234)

# Charakterystyki jakoœci generatora liczb pseudolowych (PRNG):
# - generator to najczêœciej rekurencyjnie zadany deterministyczny ci¹g liczbowy, tak wiêc przy ustalonych parametrach generatora 
#   (w szczególnoœci wyrazu pocz¹tkowego - ziarna /seed/), uzyskiwane ci¹gi liczb bêd¹ identyczne, 
#   odmienne ci¹gi liczbowe, uzyskuje siê najczêœciej manipuluj¹c wartoœci¹ ziarna, przy pozosta³ych parametrach
#   generatora niezmienionych
# - generowane ci¹gi liczbowe s¹ okresowe - d³ugoœæ okresu (cyklu) generatora to jedna z najwa¿niejszych jego charakterystyk,
#   oczywiœcie d³u¿szy cykl jest preferowany, generator MT - cykl wynosi 2^19937-1, LCG -  maksymalny cykl to (m-1), gdzie m to dzielnik,
#   dla funkcji modulo w formule okreœlaj¹cej wartoœæ kolejnego wyrazu ci¹gu 
# - ocena niezale¿noœci kolejnych generowanych liczb w ramach sekwencji oraz zgodnoœci generowanych
#   próbek z rozk³adem jednosajnym

# Zestaw testów statystycznych DieHard dla generatorów liczb pseudolosowych mo¿na znaleŸæ w pakiecie R o nazwie RDieHarder
library(RDieHarder)
help(package = "RDieHarder")


library(random)
# random package (Eddelbuettel, 2007) (which provides functions that
# access a non-deterministic random number generator (NDRNG) based on a physical source of randomness

set.seed(9999) # Ustawiamy ziarno generatora, jako liczbê ca³kowit¹ 9999
runif(10)     # Generujemy ci¹g 10 liczb pseudlosowych z rozk³adu jednostajnych 

runif(10)   # ziarno uleg³o zmianie uzyskujemy innyc ci¹g liczbowy

set.seed(9999)
runif(10) # Po ustaleniu tego samego ziarna, uzyskujemy identyczny ci¹g liczbowy

(r1k <- runif(1000, 0, 1))
par(mfrow = c(1,1))
hist(r1k, main = "Histogram - rozk³ad jednostajny, n = 1000", col = "red", freq = FALSE, xlab = "x")

(r100k <- runif(10^5, 0, 1))
hist(r100k, main = "Histogram - rozk³ad jednostajny, n = 100000", col = "green", freq = FALSE, xlab = "x")

# Próbkowanie inwersyjne z rozk³adu normalnego - generowane wartoœci z rozk³adu jednostajnego zadanego  
# na przedziale [0,1], okreœlaj¹ wartoœci dystrybuanty rozk³adu normalnego o okreœlonych parametrach (mu, sigma), 
# które to wartoœci po podstawieniu do funkcji kwantylowej, czyli funkcji odwrotnej do dystrybuanty 
# rozk³adu normlanego o przyjêtych parametrach, bêd¹ wartoœciami pseudolosowymi z przyjêtego
# rozk³adu normalnego

qgen <- runif(10^4, 0, 1)
ngen <- qnorm(qgen , mean = 170, sd = 10) # Funkcja kwantylowa rozk³adu normalnego o parametrach (mu = 170, sigma = 10)
hist(ngen, main = "Próbkowanie inwersyjne - rozk³ad normalny (170, 10)", xlab = "x", col = "blue", freq = FALSE)
curve(dnorm(x, mean = 170, sd = 10), from = min(ngen), to = max(ngen), n = 10^5, add = TRUE, col = "red")

# ecdf - dystybuanta rozk³adu empirycznego
plot(ecdf(ngen[1:50]), main = "Histogram rozk³adu empirycznego podpróby {x_i}, i = 1, ..., 50")

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
plot(z1, pch = 16, main = "P³aszczyzna zespolona")

# Pierwiastki wielomianu

# Pakiet polynom umo¿liwia dzia³ania na wielomianach
if(!require(polynom)) install.packages("polynom")
library(polynom)
# w(x) = 4 + 3*x + 2*x^2 + 1*x^3 
(wiel <- polynomial(4:1))
# Wyznacza pierwiastki wielomianu (w funkcji solve z pakietu polynom  pierwiastki s¹ wyznaczane jako wartoœci w³asne
# macierzy stowarzyszonej, z któr¹ zwi¹zany jest wielomian i uporz¹dkowane rosn¹co wed³ug ich czêœci rzeczywistej):
# http://www.ams.org/journals/mcom/1995-64-210/S0025-5718-1995-1262279-2/S0025-5718-1995-1262279-2.pdf
# na stronie 3 mo¿na znaleŸæ postaæ macierzy stowarzyszonej
(pierw <- solve(wiel)) 

# Dla sprawdzenia wyznaczamy postaæ macierzy stowarzyszonej i wyznaczamy wartoœci w³asne
(compmat <- matrix(c(0,1,0,0,0,1, -4:-2), ncol = 3))
eigen(compmat)$values

# alternatyw¹ dla solve.polynomial jest funkcja polyroot z pakietu base, która implementuje algorytm 
# Jenkinsa-Trauba poszukiwania pierwiastków wielomianów z zespolonymi (wariant CPOLY) i rzeczywistymi (wariant RPOLY)
# wspó³czynnikami (t³umaczenie na kod C procedury Fortrana autorstwa Ross, Ihaka); 
#na wejœciu podajemy wektor wspó³czynników, uporz¹dkowany rosn¹co wg wyk³adnika zmiennej wielomianu
polyroot(4:1)

# Wyznaczanie wartoœci wielomianu dla okreœlonej wartoœci argumentu (najpierw dokonuje siê konwersji obiektu polynomial 
# do klasycznej funkcji R)
wielfun <- as.function(wiel)
wielfun(c(0, 10, 20))  # Wartoœci wielomianu dla x = 0, 10, 20 

deriv(wiel)               # Wyznaczanie za pomoc¹ obliczeñ symbolicznych pochodnej wielomianu
polynom::integral(wiel)   # Domyœlne granice ca³kowania to 0 i x
polynom::integral(wiel, limits = c(0, 7))
## Wyznaczanie postaæ wielomianu w oparciu o jego pierwiastki
polynom::poly.calc(pierw)


# Wektory tekstowe
as.character(a)
(ba <- LETTERS[a + 1])
(tx <- c("czerwony", "zielony", "niebieski"))
class(tx)
is.character(tx)
paste(tx[1], tx[2], sep = "-")

# Wektory czynnikowe (factor)

# Nieuporz¹dkowane kategorie
(ftx <- factor(rep(tx, 10), ordered = FALSE))
is.ordered(ftx)

# Uporz¹dkowane kategorie
war <- c("niski", "œredni", "wysoki")
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

# Dzia³ania na macierzach realizowane s¹ w R przez funkcje biblioteki BLAS (Basic Linear Algebra Subprograms) Fortrana:
# poziom 1 - operacje wektor * wektor, poziom 2 - operacje macierz * wektor, poziom 3 - operacje macierz * macierz

# Funkcje biblioteki BLAS wykorzystuj¹ funkcje biblioteki LAPACK, implementuj¹cej procedury numeryczne 
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


#wyznacz wartoœæ funkcji (tutaj sumy) dla ka¿dego wiersza macierzy
apply(B, 1, sum)

#wyznaczanie wartoœci funkcji (tutaj sumy) dla ka¿dej kolumny macierzy
apply(B, 2, sum)

# wyznacz wartoœæ zadanej funkcji w oparciu o wartoœci z danej kolumny macierzy (kolejno dla ka¿dej z kolumn)
apply(B, 2, function (x) mean(x) + 5)

# Tworzenie macierzy diagonalnej
# macierz jednostkowa stopnia 5
diag(5)
# macierz diagonalna o elementach diagonalnych z wektora a
is.vector(a)
a
diag(a) 
# Wyci¹ganie elementów diagonalnych z macierzy B
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

# Iloczyny diadyczny (zewnêtrzny)  "Outer product"
(outB <- tcrossprod(B)) # BB'
dim(outB)
(outFH <- tcrossprod(F, H)) # FH'
dim(outFH)

# Macierz kwadratowa stopnia 5 z elementami pseudolosowymi
(A <- (matrix(round(rnorm(25, 10, 2)), ncol = 5)))
dim(A)
det(A)
# Wyznacznik jest obliczany jako iloczyn elementów diagonalnych macierzy U, w ramach dekompozycji LU, 
# w macierzy L zak³ada siê jednostkowe elementy diaganalne dla identyfikowalnoœci
library(matrixcalc)
(decomplu <- matrixcalc::lu.decomposition(A))
(detA <- prod(diag(decomplu$U)))

(Ainv <- solve(A))

# Dekompozycja QR macierzy (procedura dqrdc2 z biblioteki LINPACK albo LAPACK, wykorzystuj¹ca refleksjê Householdera)
(QR <- qr(A))
QR$rank
qr.Q(QR) # Wydobycie macierzy Q
qr.R(QR) # Wydobycie macierzy R
qr.X(QR)

(S <- crossprod(matrix(rnorm(25), ncol = 5))) # Macierz symetryczna, dodatnio okreœlona
# Dekompozycja Choleskiego (funkcja chol wykorzystuje procedury biblioteki LAPACK: DPOTRF oraz DPSTRF)
(L <- chol(S))

# Macierz odwrotna
(invS <- solve(S))
(idec <- chol2inv(L)) # Zwraca macierz odwrotn¹ do macierzy X=LL', przyjmuj¹c jako argument macierz L z dekompozycji Choleskiego

identical(idec, invS) # Porównanie czy wartoœci s¹ identyczne (bez uwzglêdnia b³êdu zwi¹zanego z reprezentacj¹ zmiennoprzecinkow¹)
all.equal(idec, invS) # Porównanie Uwzglêdnia b³¹d zwi¹zany z zastosowaniem reprezentacji / arytmetyki zmiennoprzecinkowej

(numerrunit <- S%*%invS)
zapsmall(numerrunit) # Zaokr¹glenia do 0, wartoœci, których odstêpstwa od 0, wynikaj¹ z zastosowania do ich wyznaczania arytmetyki zmiennoprzecinkowej   

# "Kwadratowe" uk³ady równañ liniowych A * x = b - rozwi¹zanie (dok³adnie jedno, gdy¿ zak³adamy, ¿e r(A) = r(A|b) = n)
print(A)
dim(A)
det(A)    # wyznacznik niezerowy, st¹d wiadomo, ¿e rz¹d r(A) = n = 5
(b <- matrix(9:5))

# W celu rozwi¹zanie kwadratowego uk³adu równañ liniowych stosuje siê funkcjê solve, która wywo³uje procedurê biblioteki LAPACK: 
# - DGESV - rzeczywiste macierze parametrów uk³adu (A, b) 
# - ZGESV - zespolone macierze parametrów uk³adów (A, b)
# Procedury te wykorzystuj¹ dekompozycjê LU macierzy A, w celu wyznaczenia rozwi¹zania uk³adu A * x = b <=> LU * x = b
(xast <- solve(A, b))

# "Prostok¹tne" uk³ady równañ liniowych - rozwi¹zanie (z wykorzystaniem dekompozycji QR macierzy parametrów)

# Uk³ad nadokreœlony (brak rozwi¹zania): r(A) /= r(A|b) - poszukuje siê wektora x* minimalizuj¹cego 
# kryterium najmniejszych kwadratów: min ||Ax - b||
set.seed(1234)
(A <- matrix(runif(12), nrow = 4))
(b <- matrix(1:4))
dim(A)
ncol(A)             # liczba niewiadomych uk³adu
qr(A)$rank          # rz¹d macierzy A
qr(cbind(A,b))$rank # rz¹d macierzy rozszerzonej A|b

qr.solve(A, b)      # wynaczamy wektor x* minimalizuj¹cy kryterium najmniejszych kwadratów

# Uk³ad niedookreœlony - rozwi¹zaniem uk³adu jest podprzestrzeñ wektorowa wymiaru n - r, gdzie n jest liczb¹ 
# niewiadomych  uk³adu, natomiast  r(A) = r(A|b) = r < n, to rzêdzy macierzy parametrów A i macierzy rozszerzonej A|b, które s¹
# takie same, równe r i mniejsze od liczby niewiadomych n
# zadanie sprowadza siê do okreœlania bazy tej podprzestrzeni wektorowej
set.seed(9999)
(A <- matrix(runif(12), nrow = 3))
(b <- matrix(1:3))
dim(A)
ncol(A)             # liczba niewiadomych uk³adu
qr(A)$rank          # rz¹d macierzy A
qr(cbind(A,b))$rank # rz¹d macierzy rozszerzonej A|b
  
qr.solve(A, b)  # Uzyskujemy jedno z rozwi¹zañ bazowych


#### Pseudoinwersja Moore'a-Penrose'a
if(!require(MASS)) install.packages("MASS")
library(MASS)

ginv(A) #Pseudoinwersja macierzy A

#### Macierz kwadratowa stopnia 5 z wartoœciami pseudolosowymi
set.seed(5678)
(M <- (matrix(round(rnorm(25, 10, 2)), ncol = 5)))

# Dekompozycja spektralna macierzy (zadanie w³asne: Ax = \lambda x) - funkcja eigen
# (funkcja eigen wykorzystuje procedury z biblioteki LAPACK: 
# - DSYEVR /dla rzeczywistej macierzy symetrycznej/, 
# - DGEEV /dla rzeczywistej macierzy niesymetrycznej/, 
# - ZHEEV /dla zespolonej macierzy hermitowskiej/
# - ZGEEV /dla zespolonej macierzy niehermitowskiej/ )

eigen(M, symmetric = FALSE)


# Uogólnione zagadnienie w³asne Ax =\lambda Bx
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

# Okreœlonoœæ  (symetrycznej) macierzy /procedura sprawdza znaki wartoœci w³asnych wyznaczonych przez eigen/
is.positive.definite(S)     # dostêpna jest tak¿e funkcja  is.positive.semi.definite  sprawdzaj¹ca dodatni¹ pó³okreœlonoœæ
is.negative.definite(-S)     # dostêpna jest tak¿e funkcja is.negative.semi.definite sprawdzaj¹ca ujemn¹ pó³okreœlonoœæ

is.indefinite #? undM <- runif(9, 1:5)
is.diagonal.matrix(diag(rnorm(5)))

# vech - Halfvectorization
(halfv <- M[lower.tri(M, diag = TRUE)])
matrixcalc::vech(M)     # alternatywnie
# vec - Wektoryzacja
c(M)
matrixcalc::vec(M)      # alternatywnie

lower.triangle(M)       # Tworzy z danej macierzy, macierz dolnotrójk¹tn¹ przez zast¹pienie zerami wszystkich elementów powy¿ej diagonali  
upper.triangle(M)

# Potêgowanie macierzy
matrix.power(M, k = 3)

# Dekompozycja LU (zak³ada siê, ¿e L ma jednostkowe elementy diagonalne, dekompozycja LU wyznaczana 
# za pomoc¹ metody Doolittle)
lu.decomposition(M)

# Normy macierzy rzeczywistych
# Pakiet base (procedury biblioteki LAPACK)
# normy operatorowe
norm(M, type = "1")     # norma ||X||_1,  p = 1
norm(M, type = "i")     # norma ||X||_\infty, p = \infty
norm(M, type = "2")     # norma spektralna ||X||_2, odpowiada najwiêkszej wartoœci osobliwej macierzy

# normy "po wspó³rzêdnych"
norm(M, type = "F")     # norma Frobeniusa (odpowiada normie L2 zwektoryzowanej macierzy)
norm(M, type = "M")     # norma maksimum max i,j |x_ij| odpowiada normie L1 zwektoryzowanej macierzy)

# Alternatywnie pakiet matrixcalc:
entrywise.norm(M, p = 1)
frobenius.norm(M)
spectral.norm(M)

# Implementacja popularnych metod numerycznych - pakiet pracma (Practical Numerical Math Functions)
library(pracma)
help(package = "pracma")

# charpoly - funkcja wyznaczaj¹ca parametry wielomianu charakterystycznego dla macierzy A
# Wspó³czynniki wielomianu charakterystycznego dla zadania w³asnego wyznaczane s¹ za 
# pomoc¹ algorytmu Faddiejewa–LeVerriera  
charpoly(M)

# bisect metoda bisekcji - poszukiwani rzeczywistych pierwiastków funkcji ci¹g³ej jednej zmiennej, 
# w zadanym przedziale wartoœci zmiennej [a,b]

# Przybli¿enie wartoœci pierwiastka
fwielom <- function(x, p = 2, k = 5) x^p - k
# x \in [0,5], takie, ¿e x^2-5 = 0, daje przybli¿enie numeryczne wartoœci pierwiastka z 5
bisect(fwielom, a = 0, b = 5, maxiter = 100, tol = NA)

#######################################################################################################
#######################################################################################################
#######################################################################################################

# Ró¿niczkowanie symboliczne w R
# Wyra¿enia matematyczne reprezentowane za pomcoc¹ drzew sk³adniowych
# Automatic differentiation (ró¿niczkowanie automatyczne, czasami te¿ nazywane ró¿niczkowanie algorytmiczne):
# wykorystuje regu³ê ³añcuchow¹ ró¿biczkowania funkcji z³o¿onych:
# wariant forward - w ramach regu³y ³añcuchowej ró¿niczkowania, procedura "przebiega" drzewo sk³adniowe od liœci do
# wierzcho³ka,
# wariant backward - w ramach regu³y ³añcuchowej ró¿niczkowania, procedura "przebiega" drzewo sk³adniowe od wierzcho³ka
# do liœci,
# literatura: Rall L.B., Automatic Differentiation: Techniques and Applications,

?expression  # klasa "expression" s³u¿y do przechowywania wyra¿eñ

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

# D(expr, name) - Ró¿niczkowanie symoliczne - pakiet base
# symboliczne ró¿niczkowanie funkcji jednej zmiennej (na wejœciu obiekt "expression",
# na wyjœciu zwraca obiekt "expression")
D(expression((exp(x^2+5)+cos(x))/x), name = "x")

# deriv(expr, ...) - symboliczne ró¿niczkowanie funkcji wielu zmiennych (na wejœciu obiekt "expression",
# na wyjœciu zwraca obiekt "expression" lub funkcjê)

(fncx <- expression((exp(x^2+5)+cos(x))/x))  # tworzymy obiekt "expression", przechowuj¹cy funkcjê jednej zmiennej "x"
(dx <- deriv(fncx, namevec = "x", hessian = TRUE, func = TRUE))  # func = TRUE, ¿eby zwróci³ funkcjê, a nie obiekt "expression"

dx(1:5)   # Funkcja zwraca wartoœci funkcji dla zadanych wartoœci argumentu, a wartoœci pierwszej i drugiej pochodnej
          # jako atrybuty "gradient", "hessian", który wartoœci mo¿na wyci¹gn¹c:

attr(dx(x = 1:5), "gradient") # zwraca wartoœæ pierwszej pochodnej w dla x = 1, ..., 5
attr(dx(x = 1:5), "hessian")  # zwraca wartoœæ drugiej pochodnej w dla x = 1, ..., 5

(fncxy <- expression(sin(cos(x + y^2)))) # tworzymy obiekt "expression", przechowuj¹cy funkcjê dwóch zmiennych "x" i "y"
(dxy <- deriv(fncxy, c("x", "y"), hessian = TRUE, func = TRUE))

attr(dxy(x = 1.5, y = 2.5), "gradient") # zwraca wartoœæ pierwszej pochodnej w dla x = 8, 5, 10, y = 3, 7, 8
attr(dxy(x = 1.5, y = 2.5), "hessian")  # zwraca wartoœæ drugiej pochodnej w dla x = 8, 5, 10, y = 3, 7, 8


attr(dxy(x = c(8,5,10), y = c(3, 7 , 8)), "gradient") # zwraca wartoœæ pierwszej pochodnej w dla x = 8, 5, 10, y = 3, 7, 8
attr(dxy(x = c(8,5,10), y = c(3, 7 , 8)), "hessian")  # zwraca wartoœæ drugiej pochodnej w dla x = 8, 5, 10, y = 3, 7, 8


# Ró¿niczkowanie symboliczne - pakiet Deriv
# Symboliczne wyznaczanie pochodnych funkcji wielu zmiennych (na wejœciu podaje siê obiekt "function")
# Okreœlamy skalarn¹ funkcjê wielu zmiennych, w tym przypadku dwóch zmiennych:
# f(x1, x2) = 100 * (x2 - x1 * x1)^2 + (1-x1)^2 (Funkcja Rosenbrocka - minimum globalne dla (x1,x2) = (1, 1))

library(Deriv)
# Deriv::Deriv
# Deriv::Simplify

fnc <- function(x) {
  100 * (x[2] - x[1] * x[1])^2 + (1 - x[1])^2
}
# Je¿eli na wejœciu funkcji podajemy wektor x, którego elementami s¹ zmienne funkcji,
# to, aby wyznaczyæ gradient funkcji wzglêdem zmiennych, stosujemy zapis 
# okreœlaj¹cy "po³o¿enie" zmiennych w wektorze x np. c(x = "1", x = "2"), nderiv = 1 oznacza, 
# ¿e chcemy wyznaczyæ gradient (wektor pochodnych czaœtkowych pierwszego rzêdu)
# na wyjœciu uzyskujemy obiekt "function"
(fncgrad <- Deriv::Deriv(fnc, x = c(x = "1", x = "2"), nderiv = 1))

# Wyznaczenie hesjanu nderiv = 2
(fnches <- Deriv::Deriv(fnc, x = c(x = "1", x = "2"), nderiv = 2))

# Wyznaczanie jednoczesne gradientu i hesjanu nderiv = 1:2

(fncgh <-Deriv::Deriv(fnc, x = c(x = "1", x = "2"), nderiv = 1:2))

# Wyznaczenie wartoœci gradientu i hesjanu w punkcie x = [1, 2]
fncgh(c(1,2))

# Uproszczenie funkcji wynikowej
Deriv::Simplify

######################################################################################################################
######################################################################################################################
######################################################################################################################

# Ró¿niczkowanie numeryczne - pakiet numDeriv
# Umo¿liwia przybli¿enie wartoœci gradientu, hesjanu w punkcie
library(numDeriv)
# Numeryczne przybli¿enie wartoœci gradientu funkcji w punkcie 
# f - funkcja jêzyka R, x - wektor wartoœci, w których ma byæ wyznaczona numeryczne przybli¿enie gradientu
# metoda wyznaczania: method = c("Richardson", "simple", "complex"), "simple" f'(x) approx (f(x+h)-f(x-h))/2h, 
# "complex" (Lyness i Moler), "Richardson" - metoda ekstrapolacji Richardsona
# 
numDeriv::grad(fnc,  x = c(1, 2), method = "Richardson")
numDeriv::hessian(fnc,  x = c(1, 2), method = "Richardson")

######################################################################################################################
######################################################################################################################
######################################################################################################################

# Optymalizacja bezwarunkowa - metody numeryczne

# Domyœlnie optim dokonuje minimalizacji funkcji wielu zmiennych
# za pomoc¹ metod numerycznych: BFGS (method = "BFGS" lub method = "L-BFGS-B"), 
# gradientów sprzê¿onych (method = "CG"), metoda Brenta (method = "Brent"), Neldera-Meada (method = "Nelder-Mead)
# symulowanego wy¿arzania (SANN)
# par - punkt startowy dla algorytmu numerycznego
# fn - funkcja podlegaj¹ca optymalizacji
# gr - gradient funkcji, podaje siê obekt function reprezentuj¹cy gradient, je¿eli gr = NULL, gradient jest przybli¿any numerycznie
# hessian = TRUE, zwraca wartoœæ hesjanu (przbli¿onego numerycznie) w punkcie z ostatniej iteracji
# control - lista zawieraj¹ca parametry procedury numerycznej: trace - pokazuje wiêcej szczegó³ów z dzia³ania
# algorytmu numerycznego, fnscale - odwrotnoœæ wspó³czynnik przez, który mno¿ymy funkcjê przed poddaniem optymalizacji:
# 1/fnscale * f
# ujemna wartoœæ fnscale, sprawia, i¿ mamy maksymalizacjê "wyjœciowej" funkcji f,
# parametry zatrzymania algorytmu numerycznego: maxit (domyœlnie 100), 
# reltol (progowa realtywna zmiana wartoœci funkcji, w danej iteracji, poni¿ej której nastêpuje zatrzymanie algorytmu,
# domyœlnie pierwiastek z epsilonu maszynowego)
# REPORT = 10, pokazuje wartoœæ funkcji w punkcie dla co 10-tej iteracji
# 
?optim

fnc  # Przypomnijmy postaæ funkcji dwóch zmiennych f(x1, x2) = 100 * (x2 - x1 * x1)^2 + (1-x1)^2
(res <- optim(par = c(0.2, 0.5), fn = fnc, gr = fncgrad, method = "BFGS"))

# optim z wyszczególnieniem parametrów procedury numeryczne: argumentowi control przypisano listê parametrów
(resext<- optim(par = c(0.2, 0.5), fn = fnc, gr = fncgrad, method = "BFGS", hessian = TRUE,
      control = list(trace = 1, fnscale = 1, REPORT = 1, maxit = 100, reltol = sqrt(.Machine$double.eps))))

matrixcalc::is.positive.definite(resext$hessian)    # Potwierdzenie warunku wystarczaj¹cego dla minimum lokalnego funkcji wielu zmiennych


# Maksymalizacja logarytmu funkcji wiarygodnoœci dla próby z rozk³adu normalnego za pomoc¹ metod numerycznych
# Maksimum w tym przypadku mo¿na wyznaczyæ analitycznie: 
# ^mu = n^-1 \sum_i=1^n (x_i), sigma^2 = n^-1 sum_i=1^n (x_i - ^mu)^2

loglik <- function(x, pars) {
  mu <- pars[1]
  logsigma2 <- pars[2]
  -sum(logsigma2 + (x - mu)^2/exp(logsigma2))}
(loglikgrad <- Deriv::Deriv(loglik, x = c(pars = "1", pars = "2"), nderiv = 1))

obs <- rnorm(100, 150, 4) # 100 obserwacji pseudolosowych z rozk³adu normalnego N(mu = 150, sigma = 4)
(parest <- optim(par = c(0,0), x = obs, fn = loglik, gr = loglikgrad, method = "BFGS", hessian = TRUE,
      control = list(trace = 1, fnscale = -1, REPORT = 1, maxit = 100)))

(muest <- parest$par[1])
(sigmaest <- exp(parest$par[2]))^.5

# Oszacowanie MNW parametrów rozk³adu z wykorzystaniem numerycznych metod poszukiwania maksimum funkcji wiarygodnoœci
# funkcja fitdist z pakietu fitdistrplus
library(fitdistrplus)
?fitdistrplus::fitdist



######################################################################################################################
######################################################################################################################
######################################################################################################################

# Ca³kowanie numeryczne (jednowymiarowe) - metody kwadratur

# np. metoda Newtona-Cotesa: w tym metoda trapezów: \sum_i=1^n (x_i- x_i-1)/2 (f(x_i-1) + f(x_i))

# W R zaimplementowano natomiast adaptacyjne metody kwadratur:
# https://en.wikipedia.org/wiki/Adaptive_quadrature
# Wykorzystuje siê metodê Gaussa–Kronroda z biblioteki Fortrana QUADPACK:
# https://en.wikipedia.org/wiki/QUADPACK

# funkcja integrate wykorzystuje procedury dqags oraz dqagi z biblioteki QUADPACK
ftointegr <- function(x) exp(x^2+5)
curve(ftointegr, from = 0, to = 3, n = 1000, col = "red", ylab = "f(x)", main =  expression(paste("f(x)=", exp(x^2+5))))
integrate(ftointegr, subdivisions = 100L, lower = 1, upper = 2)
integrate(dnorm, subdivisions = 100L, lower = 0, upper = 10, mean = 0, sd = 5) # X~ N(mu=5, sd = 10), P({5 <= X <= 10})
integrate(dnorm, subdivisions = 100L, lower = -Inf, upper = 10, mean = 0, sd = 5) # X~ N(mu=5, sd = 10), P({X <= 10}) = F(x = 10)
