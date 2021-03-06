

#########################################################################################################
#########################################################################################################
#########################################################################################################
# Import danych z pliku csv (notowania WIG20, �r�d�o: https://stooq.pl/q/d/?s=wig20):

wig20 <- read.csv("https://stooq.pl/q/d/l/?s=wig20&i=d")
# Serwis stooq.pl ogranicza liczb� mo�liwych pobra� danych z tego samego adresu IP, w sytuacji, gdy mo�liwo��
# pobrania z serwisu zosta�a zablokowana mo�emy pobra� dane z dostarczonego pliku wig20_d.csv
# wig20 <- read.csv("wig20_d.csv")

View(wig20)
dim(wig20)
names(wig20)
library(xts)
(wig20xts <- xts(wig20[,-c(1,6)], order.by = as.POSIXct(wig20[,1])))
lw20 <- log(wig20xts)
head(lw20)
plot(lw20[, "Zamkniecie"], main = "Log closing prices time series of WIG20", xlab = "Time")

library(ggplot2)
ggplot(data = lw20, aes(x = time(lw20), y = Zamkniecie))  + geom_line() +
  labs(x = "Time", y = ("log WIG20"), title = "Log closing prices time series of WIG20" ) +
  theme(plot.title = element_text(hjust = 0.5))

dlw20 <- 100 * diff(lw20) # diff - wyznaczenie przyrost�w z logarytm�w (jednokresowych logarytmicznych st�p zwrotu)
plot(dlw20[, "Zamkniecie"], main = "Daily log returns time series of WIG20", xlab = "Time")
(lastyear <- lw20["2017/"])
matplot(lastyear, type = "l", xlab = "Time", ylab = "log(wig20)", main = "OHLC - Time series of log WIG20")

d3years <- dlw20["2015/", 4]    # Ograniczenie szeregu czasowego: pocz�wszy od roku 2015 do ostatniej realizacji w ramach szeregu
                                # format: dlw20["2015-03-02/2015-06-02", ]; dlw20["/1992-02", ]
dim(d3years)
plot(d3years, main = "Daily ")
(moveqwvar <- rollapply(d3years, width = 30, FUN = var))
plot(moveqwvar, main = "Equally weighted moving variance")

d3years_demean<- scale(d3years, center = TRUE, scale = FALSE)
library(MTS)
# Wskazanie ujemnej lambdy oznacza, �e parametr lambda jest szacowany MNW przy za�o�eniu normalnego rozk�adu st�p zwrotu
# warunkowego wzgl�dem przesz�o�ci, dodatnia warto�� lambda oznacza, �e zak�ada si� ustalon� warto�� tego parametru
(ewma <- EWMAvol(d3years_demean, lambda = 0.96))
plot(xts(ewma$Sigma.t, order.by = time(ewma$return)), main = "Equally weighted moving average variance or IGARCH(1,1) wih fixed parameters")


library(rugarch)
# Kod �r�d�owy biblioteki w C/C++ (g��wnie funkcje wykorzystywane w symulacji realizacji proces�w klasy ARFIMA-GARCH):
# https://github.com/cran/rugarch/tree/master/src
# oraz R (pozosta�e funkcje, w tym wrappery dla funkcji C): https://github.com/cran/rugarch/tree/master/R
# 
# modele klasy ARMA-GARCH, wnioskowanie cz�sto�ciowe (estymacja MNW albo quasi-MNW)
# variance model: model = c("sGARCH", "iGARCH"  "eGARCH", "gjrGARCH" "apARCH", "fGARCH, "csGARCH", "mcsGARCH", "realGARCH")
# distribution.model = c("norm", "std", "snorm", "sged", "sstd", "ghyp", "jsu")
(spec <- ugarchspec(mean.model = list(armaOrder = c(1,0), include.mean = FALSE,
                                           archm = FALSE, archpow = 1, arfima = FALSE, external.regressors = NULL,
                                           archex = FALSE),
                    variance.model = list(model = "sGARCH", garchOrder = c(1,1), 
                    submodel = NULL, external.regressors = NULL, variance.targeting = FALSE),
                    distribution.model = "std"))

# Specyfikacja modelu AR(1)-GARCH(1,1) z warunkowym rozk�adem t-Studenta (model AR bez sta�ej)

(fit <- ugarchfit(data = d3years, spec = spec))

# which = c("ask", 1:12, "all")
# plot(fit, which = "ask")
plot(fit, which = "all")

# Bootstrapowe przedzia�y predykcji dla ARMA-GARCH (Pascual et al. 2004, 2006) - ugarchboot;
# bootstrapowe pr�by dla standaryzowanych reszt modelu ARMA-GARCH 
# (nie bierze si� pod uwag� przyj�tego typu rozk�adu innowacji)
# method = c("Partial", "Full"): 
# "Full" - ma na celu uwzgl�dnienie niepewno�ci zwi�zanej z oszacowaniami parametr�w modelu ARMA-GARCH oraz zmienno�ci
# sk�adnik�w losowych - innowacji procesu:
# (szacuje si� parametry wyspecyfikowanego modelu, dla szereg�w y_t(b), t = 1, ..., T, kt�re uzyskuje si� 
# przy oszacowanych warto�ciach parametr�w dla zaobserwowanej pr�by, zast�puj�c reszty standaryzowane
# ich realizacjami bootstrapowymi; wykorzystuj�c oszacowania parametr�w modelu dla ka�dej kolejnej pr�by 
# y_t(b), b = 1, ..., n.bootpred, dla przysz�ych moment�w T+1, ..., T+n.ahead, wyznacza si� warto�ci prognozy 
# wariancji oraz y_t, wykorzystuj�c przy tym bootstrapowe realizacje standaryzowanych reszt - liczba
# bootstrapowych standaryzowanych reszt wynosi n.bootpred, tym samym dla ka�dego momentu czasowego t= T+1,...,T+n.ahead
# mamy n.bootfit * n.bootpred relizacji z bootstrapowego predyktywnego rozk�adu

# "Partial" - nie uwzgl�dnia niepewno�ci zwi�zanej z oszacowaniami parametr�w, dla moment�w t = T+1,...,T+n.ahead,
# przy generowaniu realizacji wariancji oraz y_t wykorzystuje si�, wy��cznie oszacowania parametr�w dla zaobserwowanej 
# pr�by oraz bootstrapowe realizacje standaryzowanych reszt, dla ka�dego momentu uzyskujemy n.bootpred 
# prognoz wariancji oraz warto�ci y_t

(bootp <- ugarchboot(fit, method = "Full", n.ahead = 50, n.bootpred = 100, n.bootfit = 100, verbose = TRUE))
slotNames(bootp)

# Realizacje (symulacje) y_t, t = T+1, ..., T+n.ahead z bootstrapowego rozk�adu predyktywnego
slotNames(bootp)
(forboot <- bootp@fseries)
dim(forboot) # (n.bootfit * n.bootpred) x n.ahead = (100 * 100) x 50

# Bootstrap dla sigma^2
(fsig <- bootp@fsigma)
dim(fsig) # (n.bootfit * n.bootpred) x n.ahead = (100 * 100) x 50

# Wykres wachlarzowy
library(fanplot)
plot(NULL, xlim = c(1, 50), ylim = range(forboot)*0.85, main = "Fanplot of the AR(1)-GARCH(1,1) bootstrap predictive distribution",
     ylab = "Predictive intervals")
# data.type = c("values", "simulations"), style = c("fan", "spaghetti", "boxplot", "boxfan")
fan(data = forboot, data.type="simulations", style = "fan", type = "interval", probs= c(0.05, 0.25, 0.5, 0.75, 0.95))

# Szacowanie modelu w oparciu o ruchome okno
(roll <- ugarchroll(spec, d3years, n.start = 200, refit.every = 100,
                  refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE,
                  VaR.alpha = c(0.01, 0.05), keep.coef = TRUE))
slotNames(roll)

# Op�nienie szeregu czasowego (y_t-k)
ytm1 <- lag(x = d3years, k = 1) 
head(d3years)
head(ytm1)

# Kolejne op�nienia szeregu wraz z wyj�ciowym szeregiem (y_t, y_t-1, ..., y_t-k)
k <- 3
ylags <- embed(d3years, dimension = k+1)
colnames(ylags) <- c("yt", paste("ytm", 1:3, sep = "")) 
head(ylags)
(ylagsxts <- xts(ylags, order.by = index(d3years)[-(1:3)]))

# Data systemowa, Czas systemowy
Sys.Date()
Sys.time()

# apply.daily, apply.weekly, period.apply - statystyki za tydzie�, miesi�c, kwartalne, roczne
(monthly_stats <- apply.monthly(d3years, FUN = mean))
(monthly_stats1 <- apply.monthly(d3years, 
                        FUN = function(x) c(volat = sd(x)*252^0.5, 
                        quantDisp = IQR(x)/(quantile(x, 0.75)+quantile(x, 0.25)))))

# endpoints
# to.minutes(x,k,...), to.minutes3(x,name,...), to.minutes5, to.minutes10, to.minutes15, to.minutes30
# to.hourly, to.daily, to.weekly, to.monthly

(endp <- endpoints(d3years,on = "months", k = 2)) # co dwa miesi�ce
d3years[endp,]

# to.period, to.monthly, to.weekly
to.period(d3years, period = "months", k = 2)  # co dwa miesi�ce (takie same wyniki jak w przypadku wykorzystania indeksowania 
                                              # w oparciu o endpoints)
to.monthly(d3years, OHLC = FALSE)
to.weekly(d3years, OHLC = FALSE)
to.weekly(d3years, OHLC = TRUE)

# Wi�cej mo�liwo�ci pakiet highfrequency - dane transakcyjne i z arkusza zlece� 
library(highfrequency)


########################################################################################################
# Import z plik�w zebranych w archiwum zip 
# (dane dzienne z rynk�w finansowych - BOSSA.pl: http://bossa.pl/notowania/metastock/)
# (dane transakcyjne /tickowe/ z rynk�w finansowych - BOSSA.pl: http://bossa.pl/notowania/pliki/intraday/metastock/)

# Wypisanie plik�w w katalogu

# list.files(path = ".", pattern = NULL, all.files = FALSE,
#            full.names = FALSE, recursive = FALSE,
#            ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
# Wypisanie podkatalog�w
# list.dirs(path = ".", full.names = TRUE, recursive = TRUE)


# Rozpakowanie pliku zip
temp <- tempfile()
download.file("http://bossa.pl/pub/indzagr/mstock/mstzgr.zip", temp)   # Pobieramy plik mstzgr.zip z notowaniami indeks�w gie�d �wiatowych

# list = TRUE - wypisuje zamiast �ci�ga� pliki z archiwum zip
(zawart <- unzip(zipfile = temp, files = "all", list = TRUE, overwrite = TRUE,
                 junkpaths = FALSE, unzip = "internal",
                 setTimes = FALSE))

(dopobr <- zawart[1:10, 1])

unzip(zipfile = temp, files = dopobr, list = FALSE, overwrite = TRUE,
      junkpaths = FALSE, exdir = "./pliki", unzip = "internal",
      setTimes = FALSE)

unlink(temp)

setwd("./pliki") # ustalenie podkatalogu pliki jako roboczego
(pliki <- list.files())
(listanot<- lapply(pliki, function(file) read.table(file = file, sep = ",", dec = ".", header = TRUE, stringsAsFactors = FALSE)))

setwd("..")  # ustalenie nadkatalogu jako katalogu roboczego

# POSIXct format: https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html
# Funkcja, kt�ra ��czy szeregi cen na zamkni�cie sesji, w wielowymiarowy szereg xts 
mergeToxts<- function(list_not, join = "inner", ...){
  data <- NULL
  for(i in 1:length(list_not)) {
    tmp <- xts(list_not[[i]][,6], order.by = as.POSIXct(as.character(list_not[[i]][,2]), format = "%Y%m%d"))
    data<- merge.xts(data, tmp, join = join, ...)
    colnames(data)[i] <- list_not[[i]][1,1]}
  data}

# Tworzy obiekt xls zawieraj�cy wielowymiarowy szereg czasowy notowa�, join = "inner", oznacza, �e wielowymiarowy 
# szereg obejmuje daty z iloczynu zbior�w dat dla jednowymiarowych szereg�w czasowych
(wielnot <- mergeToxts(list_not = listanot, join = "inner"))
# join = "outer" - szereg wielowymiarowy obejmuje notowania dla sumy zbior�w dat dla jednowymiarowych szereg�w 
# (wprawadza si� odpowiednio braki danych NA)
(wielnotout <- mergeToxts(list_not = listanot, join = "outer")) 

plot(as.ts(wielnot), main = "Notowania indeks�w wybranych gie�d �wiatowych")

index(wielnot) # Wyci�gni�cie indeks�w z obiektu xts

# Dzia�ania na zbiorach (pakiet base): union - suma mnogo�ciowa, intersect - iloczyn mnogo�ciowy, setdiff - r�nica zbior�w,
# setequal - r�wno�� zbior�w, is.element - przynale�no�� do zbioru

(Aset <- c("B", "G", "O", "L", "M")); (Bset <- c("L", "M", "Z", "E", "V"))
union(Aset, Bset)
intersect(Aset, Bset)
setdiff(Aset, Bset)
is.element("G", Aset)

#######################################################################################################
#######################################################################################################
#######################################################################################################

# Pobieranie danych z BDL GUS

install.packages("SmarterPoland")
library(SmarterPoland)

# Drzewo kategorii BDL GUS
pwyk <- getBDLtree()
View(pwyk)

# Wyszukiwanie zbior�w danych w�r�d kategorii BDL GUS, zawieraj�cych odniesienia do frazy "Szko�y wy�sze" 
wysz <- getBDLsearch("Szko�y wy�sze")
View(wysz)

# Pobieranie zbioru danych "Szko�y wy�sze" z metric_id = 1042
# Niestety udost�pnione API "MojePanstwo" od jakiego� czasu nie ma mo�liwo�ci pobierania danych BDL GUS
BDLszkol <- getBDLseries(metric_id = 1042)

####################################################################################################

# Pobieranie danych z repozytorium Eurostat
# pakiet eurostat: https://cran.r-project.org/web/packages/eurostat/vignettes/eurostat_tutorial.pdf
install.packages("eurostat")

# �adowanie pakietu
library(eurostat)
# library(rvest)

# Wykaz kategorii danych Eurostat
toc <- get_eurostat_toc()
View(toc)

# Wyszukiwanie zbior�w danych (lub ich folder�w) w bazie Eurostatu, 
# zawieraj�cych odniesienia do frazy "HICP" (zharmonizowany wska�nik cen konsumpcyjnych), 
# typ wyszukiwanych obiekt�w: type = c("dataset", "folder", "table", "all")
# przeszukiwana jest baza: http://ec.europa.eu/eurostat/data/database

wyszHICP <- search_eurostat("HICP", type = "all")
View(wyszHICP)
# Wyci�gni�cie numeru id z wynik�w wyszukiwania
(idnum <- wyszHICP$code[2])

# Zbiory danych s� pobierane z Eurostatu za pomoc� tzw. masowego pobierania (bulk download) albo JSON API
# (umo�liwiaj�cego filtrowanie pobieranych danych). Je�eli w funkcji get_eurostat, wska�emy tylko id tabeli
# skorzystamy z masowego pobierania, je�eli dodatkowo okre�limy filtry skorzystamy z JSON API.


# Pobieramy znaleziony zbi�r danych o id "prc_hicp_midx" za pomoc� funkcji get_eurostat:
# 
# type = c("code", "label") - rodzaj zwracanych nazw zmiennych: code - kod Eurostat, label - pe�na nazwa
# filter - lista ogranicze� z elementami: 
# geo = c("EU28", "FI") - kody Eurostatu odnosz�ce si� do jednostek terytorialnych 
# time, sinceTimePeriod = 2000 (dane od roku 2000 do ostatniego dost�pnego),
# lastTimePeriod = 10 - zwraca dane odnosz�ce si� do 10 ostatnich lat w zbiorze danych

# pobieranie masowe - bez filtracji (pobieramy z pe�nymi nazwami subindykator�w Eurostatu: type = "label")
hicpData <- get_eurostat(idnum, type = "label", time_format = "num",
                         select_time = NULL, cache = TRUE, update_cache = FALSE,
                         cache_dir = NULL, compress_file = TRUE,
                         stringsAsFactors = FALSE, keepFlags = FALSE)

dim(hicpData)
View(hicpData)

# pobieranie masowe - bez filtracji (pobieramy z kodami subindykator�w Eurostatu: type = "code")
hicpDataCode <- get_eurostat(idnum, type = "code", time_format = "num",
                         select_time = NULL, cache = TRUE, update_cache = FALSE,
                         cache_dir = NULL, compress_file = TRUE,
                         stringsAsFactors = FALSE, keepFlags = FALSE)

dim(hicpDataCode)
View(hicpDataCode)

# unit: "Index, 2005=100" odpowiada mu kod I05
# coicop: "All-items HICP" odpowiada mu kod CP00

# Usuni�cie danych z podr�cznego "schowka"
clean_eurostat_cache()

# Wykaz kraj�w z grup: eu_countries, ea_countries, efta_countries, candidate_countries
eu_countries
ea_countries
efta_countries
candidate_countries

# Filtracja - maksymalnie mo�na wskaza� 50 subindykator�w Eurostat
# w li�cie filters wskazujemy:
# sinceTimePeriod = 2000 - tutaj pobieramy dane od 2000 roku
# geo = c(eu_countries[, 1], "EU28", "CH") - poszczeg�lne kraje UE, 
hicpFiltered <- get_eurostat(idnum, type = "label", lang = "en", filters = list(sinceTimePeriod = 2000, 
                                                                                geo = c(eu_countries[, 1], "EU28", "CH"), unit="I05", coicop="CP00"), 
                             stringsAsFactors = default.stringsAsFactors())
View(hicpFiltered)

####################################################################################################

# Dane finansowe z Google Finance oraz dane makroekonomiczne dla USA z FRED 
# pakiet quantmod, funkcja getSymbols
library(quantmod) 
# Dane dzienne (OHLC) - akcje, indeksy (�r�d�o Google Finance)
getSymbols(Symbols = nms, src = "google", return.class = "data.frame", symbol.lookup = TRUE)
View(MSFT)
plot(as.ts(MSFT), main = "Microsoft Corporation OHLC")
plot(as.ts(AAPL), main = "Apple Inc. OHLC")

# Dane makroekonomiczne USA - FRED (tutaj inflacja CPI)
getSymbols("CPIAUCNS", src= "FRED", return.class = "xts")
plot(CPIAUCNS, main = "CPI USA")

#######################################################################################################
#######################################################################################################
#######################################################################################################

# Pobieranie tabel z html/xml
library(XML)
library(RCurl)
link <- "https://en.wikipedia.org/wiki/List_of_countries_by_income_equality"
(xData <- getURL(link))  # Otwiera dost�p do linku
(tabele <- readHTMLTable(xData, stringsAsFactors = FALSE))   # Odczytuje i zapisuje wszystkie tabele - warto�ci pomi�dzy znacznikami <table> </table>
length(tabele)   # Liczba odczytanych tabel
names(tabele)  # Nazwy tabel (Chcemy wybra� "Gini coefficient, before taxes and transfers[10]")

gini_before_tax <- tabele[[7]]
head(gini_before_tax)
(gini_before_tax<- gini_before_tax[-1,])  # Pierwszy wiersz tabeli jest pusty, wi�c go usuwamy
class(gini_before_tax[, 2]) # Kolumny ramki danych traktowane s� jako tekstowe

# Zamiana kolum ramki na typ numeric
gini_before_tax[, -1] <- sapply(2:(ncol(gini_before_tax)), function(i) as.numeric(gini_before_tax[, i]))
gini_before_tax
class(gini_before_tax[, 2])

# Pobieramy tabel� "Gini coefficient, after taxes and transfers[10]" 
gini_after_tax <- tabele[[10]]
head(gini_after_tax)
gini_after_tax<- gini_after_tax[-1,]
gini_after_tax[, -1] <- sapply(2:(ncol(gini_after_tax)), function(i) as.numeric(gini_after_tax[, i]))
gini_after_tax

########################################################################################################
########################################################################################################
########################################################################################################

library(openxlsx) # nie wymaga Java ani Pearl
# tylko xlsx
# Okre�lamy �cie�k� do pliku reg_es.xlsx
path <- choose.files() # uruchamia okno dialogowe wyboru pliku, do kt�rego �cie�ka zostanie przypisana do path
path
?read.xlsx
# xlsxFile - �cie�ka do skoroszytu xlsx, sheet - numer lub nazwa arkusza do pobrania, startRow - numer wiersza,
# od kt�rego rozpoczyna si� pobieranie danych , colNames = TRUE, w tabeli s� nag��wki, rowNames - tabela zawiera
# nazwy obserwacji, rows - wektor numeryczne, kt�rego elementy reprezentuj� numer kolumn do pobrania
przestrz <- read.xlsx(xlsxFile = path, sheet = 1, startRow = 1, colNames = TRUE,
                      rowNames = FALSE, rows = NULL, cols = NULL)
head(przestrz)

# Pozosta�e pakiety do importu danych z xlsx
# library(gdata) #Wymaga pearl
# read.xls()
# 
# library(xlsx)  # wymaga Java
# read.xlsx(); write.xlsx()

########################################################################################################
########################################################################################################
########################################################################################################

# Dane dotycz�ce szkolnictwa - BDL GUS

# Kopiujemy z pliku szkolnictwo_skrot_nazwy.xlsx do schowka nazwy jednostek i warto�ci zmiennych B1:PR24
(dataszk <- read.table("clipboard", sep = "\t", dec = ",", header = TRUE, stringsAsFactors = FALSE, row.names = 1,
                      na.strings = "-"))

View(dataszk)
datawoj <- dataszk[!grepl("Region", rownames(dataszk)) & rownames(dataszk) != "POLSKA", ]
datawoj <- datawoj[order(rownames(datawoj)),]
dim(datawoj)    # Wymiary uzyskanej ramki danych
colnames(datawoj)
(rownames(datawoj) <- tolower(rownames(datawoj)))  # zamiana nazw wierszy na ma�e litery
View(datawoj)  # Dane panelowe: 16 jednostek x 18 jednostek czasowych (reprezentacja danych tzw. "szeroka")

# dplyr - pakiet do manipulacji na ramkach danych zawiera m.in. funkcje: 
# select - wyb�r podzbioru zmiennych (kolumn) z ramki danych, filter - wyb�r podzbioru obserwacji (wierszy) w oparciu
# o warunki logiczne, mutate - tworzenie nowych zmiennych (kolumn), z przekszta�cenia istniej�cych zmiennych (kolumn)
# i wiele innych
library(dplyr)
help(package = "dplyr") # Wyszczeg�lnienie funkcji pakietu dplyr 

# Wyb�r podzbioru cech - select
# select(data, starts_with(string))
# select(data, ends_with(string))
# select(data, contains(string))
# select(data, matches(".t."))
# select(data, varname1, varname2)
# vars <- c("varname1", "varname2")
# select(iris, one_of(vars))

select(datawoj, starts_with("swo")) # Wybierz zmienne o nazwach rozpoczynaj�cych si� od "swo"
select(datawoj, un.std.1999:un.std.2016, wse.std.1999:wse.std.2016, wsz.std.1999:wsz.std.2016)

select(datawoj, matches(".16"))

(datawoj <- mutate(datawoj, wojew = rownames(datawoj)))

# tidyr - pakiet do czyszczenia danych, dla danych panelowych - zamiana postaci "szerokiej" na "w�sk�" i odwrotnie
library(tidyr) 
help(package = "tidyr") # Wyszczeg�lnienie funkcji pakietu

# Przej�cie do postaci "w�skiej" dla zmiennej wse.abs
wse <- gather(datawoj, key = year, value = wse.abs, wse.abs.1999:wse.abs.2016)
(wse <- select(wse, wojew, year, wse.abs))
(wse <- separate(data = wse, col = year, sep ="\\.", into = c("name1", "name2", "year"), remove = TRUE))
(wse <- select(wse, wojew, year, wse.abs))

# Wszystkie zmienne przej�cie do postaci "w�skiej" z wszystkimi zmiennymi zbioru danych
allvar <- gather(datawoj, key = year, value = values, -wojew)
allvar <- separate(data = allvar, col = year, sep ="\\.", into = c("name1", "name2", "year"), remove = TRUE)
(allvar <- unite(allvar, col = varname, name1, name2, sep = "."))
(allvar <- spread(allvar, key = varname, value = values))
View(allvar)

# filter - zwraca podzbi�r obserwacji spe�niaj�cych warunek logiczny dotycz�cy warto�ci zmiennej/zmiennych

# #  Wiele kryteri�w
# filter(data, x1 < 6 & x2 == 1)   # & - operator logiczny oraz
# filter(data, x1 < 6 | x2 == 1)   # | - operator logiczny lub
# # Podanie wielu warunk�w po przecinkach, oznacza ich koniunkcj� (czyli zapis jest r�wnoznaczny z zastosowaniem &)
# filter(data, x1 < 6, x2 == 1)

filter(allvar, year == 2016, swo.og� > 30)
# r�wnowa�nie filter(allvar, year == 2016 & swo.og� > 30)
filter(allvar, swo.abs > median(swo.abs) | swo.og� > 15) 

colnames(allvar)
# mutate - Tworzenie nowych zmiennych, z przekszta�cenia ju� istniej�cych w ramce danych 
# (nowa zmienna jest do��czana do ramki danych)
(dodzm <- mutate(allvar, abs.na.sw = swo.abs/swo.og�))
# transmute - tworzy now� zmienn�, lecz nie dodaje jej do ramki danych 
(bezzm <- transmute(allvar, abs.na.sw = swo.abs/swo.og�))

# Funkcje dla zmiennych ramki danych (w oparciu o warto�ci w kolumnach ramki danych)
summarise(allvar, mean(wse.abs), sd(wse.abs))

# Statystyki grupowe
library(moments) # m.in. funkcje umo�lwiaj�ce wyznaczenie wsp�czynnika asymetrii i kurtozy
summarise(group_by(allvar, wojew), mean(swo.abs), sd(swo.abs), skewness(swo.abs), kurtosis(swo.abs))
summarise(group_by(allvar, year), mean(swo.abs), sd(swo.abs),  skewness(swo.abs), kurtosis(swo.abs))

library(psych)
# Podstawowe statystyki opisowe
describe(select(allvar, - wojew, -year))
# Statystyki opisowe warunkowe wzgl�dem warto��i zmiennej dyskretnej
describeBy(allvar, group = "wojew")

(dane_2016 <- filter(allvar, year == 2016))
(techn_2016 <- select(dane_2016, starts_with("wst")))
  
# Kwantyle rozk�adu empirycznego
quantile(techn_2016[,1])
summary(techn_2016)

# Wsp�czynniki korelacji z pr�by dla par zmiennych
# method =  c("pearson", "kendall", "spearman")) - opowiednio wsp�czennik korelacji r Pearson, 
# wsp�czynnik korelacji rang Speramana, wsp�czynnik tau-Kendalla
(wsp_kor <- cor(techn_2016, method = "pearson"))

# alternative = c("two.sided", "less", "greater")
cor.test(techn_2016[,1], techn_2016[,2], alternative = "two.sided", method = "pearson",
         exact = NULL, conf.level = 0.95, continuity = FALSE)

library(reshape)
(cp <- melt(wsp_kor))

library(ggplot2)
ggplot(cp, aes(X1, X2, fill = value)) + geom_tile() + 
  scale_fill_gradient(low = "yellow",  high = "red") + ggtitle("Correlation Heatmap")

# Okre�lenie frakcji brakuj�cych danych dla poszczeg�lnych zmiennych oraz ich "struktury"
library(VIM)
aggr_plot <- aggr(allvar, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(data), cex.axis=.7, gap=3, ylab=c("Frakcja brakuj�cych danych","Struktura"))


# na.fail - zatrzymanie procedury w przypadku brak�w danych, na.omit - pomija bezpowrotnie brakuj�ce dane,
# na.exclude - pomija brakuj�ce dane, ale zachowuje informacj� o ich po�o�eniu, dzi�ki czemu np. w sytuacji predykcji,
# brakuj�ce przypadki s� uwzgl�dniane, na.pass - brak dzia�a� dotycz�cych brakuj�cych danych 
is.na(allvar)
complete.cases(allvar)
mean(complete.cases(allvar))

allvar[!complete.cases(allvar),]

# Imputacja brakuj�cych danych
# MCAR (Missing Completly at Random) - prawdopodobie�stwo wyst�pienia braku danych dla danej zmiennej nie zale�y od
# warto�ci zmiennych obserwowalnych i nieobserwowalnych
# MAR (Missing at Random) - prawdopodobie�stwo wyst�pienia braku danych dla danej zmiennej zale�y od warto�ci
# zmiennych obserwowalnych,
# MNAR (Missing not at Random) - przypadek, kt�ry nie jest MCAR ani MNAR, np. prawdopodobie�stwo wyst�pienia 
# braku danych dla danej zmiennej zale�y od warto�ci tej�e zmiennej

library(mice)
md.pattern(allvar)

##################################################################################################
##################################################################################################
##################################################################################################
# Zapisywanie danych w postaci pliku binarnego R (.RData)
ls() # zwraca nazwy wszystkich obiekty w przestrzeni roboczej
# Zapisywanie obiekt�w do pliku RData
# list - wektor nazw obiekt�w, kt�re chcemy zapisa�; list = ls() zapisuje wszystkie obiekty;
# file - �cie�ka i nazwa pliku z rozszerzeniem RData, w przypadku, gdy �cie�ka nie jest podana
# plik zapisywany jest w katalogu roboczym
save(list = ls() , file = "wszystkie.RData")
save(list = c("allvar"), file = "wybrane.RData")

# zapisany plik RData mo�na uruchomi� za pomoc� funkcji load
getwd()                        # Sprawdzamy katalog roboczy
load(file = "wybrane.RData")   # Je�eli plik RData, z kt�rego dane chcemy xa�adowa� znajduje si� w katalogu roboczym
                               # podajemy tylko nazw� pliku, w przeciwnym razie podajemy tak�e �cie�k� do pliku
                               # lub zmieniamy katalog roboczy
setwd(path)                    # zmiana katalogu roboczego, przy czym path to �cie�ka, np. path <- "C:\\Analizy"
setwd(choose.dir())            # to samo, tylko choose.dir otwiera okno dialogowe, kt�re umo�liwia wyb�r katalogu, do kt�rego
                               # �cie�ka jest przekazywana do setwd

# Eksport do pliku csv lub tabeli
(swyzsze <- select(allvar, starts_with("swo")))
write.csv(swyzsze, file = "swyzsze.csv", fileEncoding = "UTF-8")
write.table(swyzsze)

##################################################################################################
##################################################################################################
##################################################################################################
# Wizualizacja danych

# Wykorzystujemy dane z ramki danych allvar

# Grafika z pakietu graphics

# Histogram rozk�adu empirycznego
# breaks (do wyboru): wektor zaieraj�cy punkty podzia�u, nazwa funkcji wyznaczaj�cej punkty podzia�u, 
# pojedyncza warto�� wskazuj�ca liczb� klas, nazwa wskazuj�ca jedn� z metod podzia�u: "Sturges", "Scott",
# "Freedman-Diaconis"
# freq = TRUE - histogram cz�sto�ci (liczebno�ci), freq = FALSE - histogram g�sto�ci (liczebno��/d�ugo�� przedzia�u klasy)
hist(allvar$wse.abs, col = "blue", main = "Empiryczny rozk�ad liczby absolwent�w WSE", 
     sub = "Pr�ba po��czona: wojew�dztwa x lata", freq = FALSE, breaks = "Sturges", xlab = "Absolwenci WSE")
# J�drowy estymator g�sto�ci dla pr�by po��czonej: lata 1999-2016 x wojew�dztwa
plot(density(allvar$swo.abs), main = "J�drowy estymator g�sto�ci dla pr�by po��czonej: wojew�dztwa x lata")   
hist(allvar$wse.abs, plot = FALSE, breaks = "Sturges")
plot(ecdf(allvar$wse.abs), main = "Dystrybuanta rozk�adu empirycznego - pr�ba po��czona")

# Wykres ramka-w�sy dla pr�by po��czonej - znikoma "informacyjno��"
boxplot(allvar$swo.abs, horizontal = FALSE)

# Rozk�ad empiryczny liczby absolwent�w og�em wzgl�dem wojew�dztw za pomoc� wykresu ramka-w�sy
boxplot(split(allvar$swo.abs, allvar$wojew), horizontal = TRUE, main = "Liczba absolwent�w og�em wzgl�dem wojew�dztw",
        col = "lightblue")
# Rozk�ad empiryczny liczby absolwent�w og�em wzgl�dem lat za pomoc� wykresu ramka-w�sy
boxplot(split(allvar$swo.abs, allvar$year), horizontal = TRUE, main = "Liczba absolwent�w og�em wzgl�dem lat", col = "lightgreen")

(swyzog <- select(allvar, wojew, year, matches("swo.")))


(wojewlab <- as.numeric(as.factor(swyzog$wojew)))
(yearlab <- as.numeric(swyzog$year)%%1998)   # %% funkcja modulo: year mod 1998 
  
# Macierzowe wykresy rozrzutu (kategorie zmiennej dyskretnej - kolor znacznika)
pairs(swyzog[, -(1:2)], col = wojewlab, pch = 16, main = "Macierzowy wykres rozrzutu - kategorie: wojew�dztwa")
pairs(swyzog[, -(1:2)], col = yearlab, pch = 16, main = "Macierzowy wykres rozrzutu - kategorie: lata")

# Dyskretyzacja zmiennej ci�g�ej
(tuk5nx3 <- fivenum(swyzog[, 3]))  # 5 liczb Tukeya (minimum, lower-hinge, mediana, upper-hinge, maksimum)
(x3disc <- cut(swyzog[, 3], breaks = tuk5nx3, labels = NULL, include.lowest = TRUE,  right = TRUE, dig.lab = 3,
       ordered_result = TRUE))

(tuk5nx5 <- fivenum(swyzog[, 5]))  # 5 liczb Tukeya (minimum, lower-hinge, mediana, upper-hinge, maksimum)
(x5disc <- cut(swyzog[, 5], breaks = tuk5nx5, labels = NULL, include.lowest = TRUE,  right = TRUE, dig.lab = 3,
               ordered_result = TRUE))

(tb <- table(x5disc, useNA = "no"))  # Tablica liczebno�ci wyst�pie� kategorii
(ptb <- prop.table(tb))

# Tablice kontyngencji (tutaj dwudzielcze)
(tb2d <- table(x3disc, x5disc))
(ptb2d <- prop.table(tb2d, margin = 2)) # margin = 1 - kolumny sumuj� si� do 1, margin = 2 - wiersze sumuj� si� do 1

plot(ptb2d, main = "Graficzna reprezentacja tablicy dwudzielczej", col = "lightblue")

# Test niezale�no�ci chi-kwadrat dla dw�ch cech jako�ciowych
(chiwynik <- chisq.test(tb2d))
chiwynik$expected

# fisher.test(tb2d)
mcnemar.test(tb2d)

# Wiele zmiennych na jednym wykresie: matplot, matpoints, matlines

# Wyznaczmy warto�ci trzech funkcji wielomianowych, w punktach w ramach przyj�tej siatki
x <- seq(from = -1, to = 2, length = 20)
y1 <- 1.2 * x^3 - x^2 - 3.0*x + 2.0
y2 <- 1.5 * x^3 - 0.9 * x^2  - 2.0 * x + 1.0
y3 <- 0.7 * x^3 - 1.3*x^2 - 0.6 * x + 0.4

# Adnotacje na wykresach:
# https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/plotmath.html

(bindedpoints <- cbind(y1, y2, y3))  # ��czymy warto�ci funkcji wielomianowych w punktach siatki w macierz
matplot(x, bindedpoints , type = "l", lwd = 3, lty = 1:3, col = 1:3, xlim = c(-1, 2), ylim = c(-1, 5.5) ,
        xlab = "x", ylab = expression(w[i](x)), main = "Wykres wielomian�w")
legend("topleft", legend = c("w1", "w2", "w3"), lty = c(1,2,3), col = 1:3)


# Wykres funkcji matematycznych
f <- .5         # czestotliwosc
phi <- 2        # przesuniecie fazowe
curve(sin(2*pi*f*x + phi), from = -10, to = 10, n = 1000, type = "l", col = "red")

# Wykresy 3D

# Wykres powierzchni funkcji dw�ch zmiennych z = f(x,y) - funkcja persp
x <- seq(-1.95, 1.95, length = 30)
y <- seq(-1.95, 1.95, length = 35)
(z <- outer(x, y, function(a, b) a*b^2))  # Warto�ci funkcji w punktach dla ka�dej mo�liwej 
                                          # pary warto�ci x oraz y
(nrz <- nrow(z))
(ncz <- ncol(z))

# Paleta przej�cia kolor�w do niebieskiego do zielonego
(paleta <- colorRampPalette( c("blue", "green")))
# Wybieramy n = 100 kolor�w ze stworzonej palety
nbcol <- 100
(kol <- paleta(n = nbcol))

zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
facetcol <- cut(zfacet, nbcol)
persp(x, y, z, col = kol[facetcol], phi = 30, theta = -30, main = "Wykres skalarnej funkcji dw�ch zmiennych")

# Wykresy warstwicowe i konturowe filled.contour z pakietu graphics)
filled.contour(x, y, z, color = terrain.colors, plot.title = title(main = "Wykres konturowy dla funkcji z = xy^2",
              xlab = "x", ylab = "y"), 
              plot.axes = { axis(1, seq(-2, 2, by = 0.2))
                            axis(2,  seq(-2, 2, by = 0.2)) },
              key.title = title(main = "Warto�� z"), key.axes = axis(4, seq(-8, 8, by = 1)))


# Wykres rozrzutu 3D
library(scatterplot3d)
colnames(swyzog)
wojewlab
scatterplot3d(x = swyzog$swo.og� , y = swyzog$swo.std , z = swyzog$swo.abs, color = wojewlab, pch = 16,
              xlab = c("Szko�y wy�sze og�em"), ylab = "Liczba student�w", zlab = "Liczba absolwent�w",
              main = "Tr�jwymiarowy wykres rozrzutu")

##################################################################################################

# ggplot2 - sk�adnia dla tworzenia wykres�w, umozliwiaj�ca bardziej �cis�� kontrol� ich element�w 
# http://ggplot2.tidyverse.org/reference/
# Szczeg�y dotycz�ce tworzenia wykres�w z ggplot2:
# Wickham H., "ggplot2: Elegant Graphics for Data Analysis"

library(ggplot2)
help(package = "ggplot2")
# Histogram ggplot2: geom_histogram
# mapping = aes() - aestethics ("estetyki") - okre�lenie zmiennych, kt�rych warto�ci przedstawiane b�d� na wykresie:
# aes(x = zm1, y = zm2, col = zm3, size = zm4)
# labs - nazwy wykresu (title), osi (x, y)
# theme - specyfikacja wygl�du wykresu
ggplot(data = allvar) + geom_histogram(mapping = aes(x = wse.abs), binwidth = 500, color = "green", fill = "red") + 
  labs(title = "Empiryczny rozk�ad liczby absolwent�w WSE", subtitle = "Pr�ba po��czona: wojew�dztwa x lata", 
       x = "Absolwenci WSE", y = "liczebno��") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.5), face = "bold", colour = "green"),
        plot.subtitle = element_text(hjust = 0.5, face = "italic"),
        panel.background = element_rect(fill = "white", colour = "grey50"))

# Tworzenie szablonu wykresu (do kt�rego mo�na dodawa� kolejne elementy poprzez "+")
plottempl <- ggplot(data = allvar) + labs(x = "Absolwenci WSE")
plottempl + geom_density(aes(x = wse.abs)) + labs(y = "g�sto��") + labs(title = "J�drowy estymator g�sto�ci") 

# Zapisanie ostatniego wy�wietlonego wykresu do pliku
ggsave(filename = "wykres.pdf", plot = last_plot())

names(swyzog)

# Wykres rozrzutu
(scatt <- ggplot(data = swyzog) + geom_point(mapping = aes(x = swo.og�, y = swo.abs, col = wojew, size = swo.std)) +
  labs(title = "Wykres rozrzutu absolwenci szk� wy�szych vs. liczba szk� wy�szych",
       x = "szko�y wy�sze og�em", y = "absolwenci szk� wy�szych", colour="Wojew�dztwo", size = "Liczba student�w"))

# Wyg�adzanie: method= c("lm", "glm", "gam", "loess", "rlm"), wyb�r method = "auto" odpowiada wyborowi "loess"
# lm - KMNRL (KMNK), glm - uog�lniony model regresji liniowej (do wskazania funkcja przej�cia + typ rozk�adu 
# warunkowego), gam - uog�lnione modele addytywne, rlm - odporna regresja liniowa (M estymator, domyslnie
# wagi zadane przez funkcj� psi Hubera)
scatt + geom_smooth(mapping = aes(x = swo.og�, y = swo.abs), method = "auto", formula = y ~ x, se = TRUE)

# Skala logarytmiczna na wykresie rozrzutu (log10, sqrt, reverse)
scatt + scale_x_log10() + scale_y_log10()

# Wykres ramka-w�sy
# axis.text.x = element.blank() - usuni�cie nazw jednostek z osi x
ggplot(data=swyzog) + geom_boxplot(aes(x=wojew, y=swo.abs, fill=wojew)) + 
  labs(title = "Wykres ramka-w�sy", x = "wojew�dztwo", y = "absolwenci szk� wy�szych", fill = "wojew�dztwo") +
  theme(axis.text.x = element_blank(), plot.title = element_text(hjust = 0.5))

# Wykres szeregu czasowego
ggplot(data = d3years) + geom_line(mapping = aes(x = index(d3years), y = d3years))+
  labs(title = "Wykres dziennych logarytmicznych st�p zwrotu WIG20", x = "Czas", y = expression(y[t]))+
  theme(plot.title = element_text(hjust = 0.5))

# Skr�cona sk�adnia wykresu ggplot2 - qplot
qplot(x = swo.og�, y = swo.abs, data = swyzog, color = wojew, size= swo.std, geom = "point",
      main= "Wykres rozrzutu absolwenci szk� wy�szych vs. liczba szk� wy�szych")


# Narz�dzia graficzne z ggplot2 wykorzystane do prezentacji wynik�w analiz 
library(ggfortify)
# funkcja autoplot umo�liwia prezentacj� wynik�w analiz z pomoc� ggplot2 zamiast narz�dzi z pakietu graphics
# Graficzna weryfikacja za�o�e� KMNRL
autoplot(lm(Petal.Width~Petal.Length, data = iris), data = iris, colour = 'Species')
# Wykres pr�bkowej ACF/PACF (op�nienie wyra�one w sekundach 1d = 3600 s)
autoplot(stats::acf(d3years, plot = FALSE))
autoplot(stats::pacf(d3years, plot = FALSE))

# Wykres wielowymiarowego szeregu czasowego (jednowymiarowe szeregi sk�adowe na osobnych wykresach)
autoplot(wielnot[, 1:3])

# Szablony formatowania wykres�w
library(ggthemes)
# theme_base - wygl�d wykresu zbli�ony do tych z pakietu graphics
# theme_calc - wzorowany na wykresie z LibreOffice Calc
# theme_economist - wykres z czasopisma "The Economist"
# theme_excel - wykres z szarym t�em wzorowany na tych z MS Excel
# theme_few - wykres oparty na: Few, �Practical Rules for Using Color in Charts�
# theme_gdocs - wykres Google Docs
# theme_hc - wykres wzorowany na Highcharts JS
# theme_par- wykres wykorzystuj�cy bie��ce ustawienia opcji par()
# theme_solarized: a theme using the solarized color palette.
# theme_stata - wzorowany na wykresie z oprogramowania Stata
# theme_tufte - wykres minimalizuj�cy "zu�ycie tuszu", oparty na: Tufte, "The Visual Display of Quantitative Information"
# theme_wsj - wykres z "The Wall Street Journal"

# The Economist
scatt + geom_smooth(mapping = aes(x = swo.og�, y = swo.abs), method = "auto", formula = y ~ x, se = TRUE) +
  theme_economist()

# Ms Excel
scatt + geom_smooth(mapping = aes(x = swo.og�, y = swo.abs), method = "auto", formula = y ~ x, se = TRUE) +
  theme_excel()

# Google Docs
scatt + geom_smooth(mapping = aes(x = swo.og�, y = swo.abs), method = "auto", formula = y ~ x, se = TRUE) +
  theme_gdocs()
