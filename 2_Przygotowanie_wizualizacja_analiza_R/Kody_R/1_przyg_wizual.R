

#########################################################################################################
#########################################################################################################
#########################################################################################################
# Import danych z pliku csv (notowania WIG20, ¿ród³o: https://stooq.pl/q/d/?s=wig20):

wig20 <- read.csv("https://stooq.pl/q/d/l/?s=wig20&i=d")
# Serwis stooq.pl ogranicza liczbê mo¿liwych pobrañ danych z tego samego adresu IP, w sytuacji, gdy mo¿liwoœæ
# pobrania z serwisu zosta³a zablokowana mo¿emy pobraæ dane z dostarczonego pliku wig20_d.csv
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

dlw20 <- 100 * diff(lw20) # diff - wyznaczenie przyrostów z logarytmów (jednokresowych logarytmicznych stóp zwrotu)
plot(dlw20[, "Zamkniecie"], main = "Daily log returns time series of WIG20", xlab = "Time")
(lastyear <- lw20["2017/"])
matplot(lastyear, type = "l", xlab = "Time", ylab = "log(wig20)", main = "OHLC - Time series of log WIG20")

d3years <- dlw20["2015/", 4]    # Ograniczenie szeregu czasowego: pocz¹wszy od roku 2015 do ostatniej realizacji w ramach szeregu
                                # format: dlw20["2015-03-02/2015-06-02", ]; dlw20["/1992-02", ]
dim(d3years)
plot(d3years, main = "Daily ")
(moveqwvar <- rollapply(d3years, width = 30, FUN = var))
plot(moveqwvar, main = "Equally weighted moving variance")

d3years_demean<- scale(d3years, center = TRUE, scale = FALSE)
library(MTS)
# Wskazanie ujemnej lambdy oznacza, ¿e parametr lambda jest szacowany MNW przy za³o¿eniu normalnego rozk³adu stóp zwrotu
# warunkowego wzglêdem przesz³oœci, dodatnia wartoœæ lambda oznacza, ¿e zak³ada siê ustalon¹ wartoœæ tego parametru
(ewma <- EWMAvol(d3years_demean, lambda = 0.96))
plot(xts(ewma$Sigma.t, order.by = time(ewma$return)), main = "Equally weighted moving average variance or IGARCH(1,1) wih fixed parameters")


library(rugarch)
# Kod Ÿród³owy biblioteki w C/C++ (g³ównie funkcje wykorzystywane w symulacji realizacji procesów klasy ARFIMA-GARCH):
# https://github.com/cran/rugarch/tree/master/src
# oraz R (pozosta³e funkcje, w tym wrappery dla funkcji C): https://github.com/cran/rugarch/tree/master/R
# 
# modele klasy ARMA-GARCH, wnioskowanie czêstoœciowe (estymacja MNW albo quasi-MNW)
# variance model: model = c("sGARCH", "iGARCH"  "eGARCH", "gjrGARCH" "apARCH", "fGARCH, "csGARCH", "mcsGARCH", "realGARCH")
# distribution.model = c("norm", "std", "snorm", "sged", "sstd", "ghyp", "jsu")
(spec <- ugarchspec(mean.model = list(armaOrder = c(1,0), include.mean = FALSE,
                                           archm = FALSE, archpow = 1, arfima = FALSE, external.regressors = NULL,
                                           archex = FALSE),
                    variance.model = list(model = "sGARCH", garchOrder = c(1,1), 
                    submodel = NULL, external.regressors = NULL, variance.targeting = FALSE),
                    distribution.model = "std"))

# Specyfikacja modelu AR(1)-GARCH(1,1) z warunkowym rozk³adem t-Studenta (model AR bez sta³ej)

(fit <- ugarchfit(data = d3years, spec = spec))

# which = c("ask", 1:12, "all")
# plot(fit, which = "ask")
plot(fit, which = "all")

# Bootstrapowe przedzia³y predykcji dla ARMA-GARCH (Pascual et al. 2004, 2006) - ugarchboot;
# bootstrapowe próby dla standaryzowanych reszt modelu ARMA-GARCH 
# (nie bierze siê pod uwagê przyjêtego typu rozk³adu innowacji)
# method = c("Partial", "Full"): 
# "Full" - ma na celu uwzglêdnienie niepewnoœci zwi¹zanej z oszacowaniami parametrów modelu ARMA-GARCH oraz zmiennoœci
# sk³adników losowych - innowacji procesu:
# (szacuje siê parametry wyspecyfikowanego modelu, dla szeregów y_t(b), t = 1, ..., T, które uzyskuje siê 
# przy oszacowanych wartoœciach parametrów dla zaobserwowanej próby, zastêpuj¹c reszty standaryzowane
# ich realizacjami bootstrapowymi; wykorzystuj¹c oszacowania parametrów modelu dla ka¿dej kolejnej próby 
# y_t(b), b = 1, ..., n.bootpred, dla przysz³ych momentów T+1, ..., T+n.ahead, wyznacza siê wartoœci prognozy 
# wariancji oraz y_t, wykorzystuj¹c przy tym bootstrapowe realizacje standaryzowanych reszt - liczba
# bootstrapowych standaryzowanych reszt wynosi n.bootpred, tym samym dla ka¿dego momentu czasowego t= T+1,...,T+n.ahead
# mamy n.bootfit * n.bootpred relizacji z bootstrapowego predyktywnego rozk³adu

# "Partial" - nie uwzglêdnia niepewnoœci zwi¹zanej z oszacowaniami parametrów, dla momentów t = T+1,...,T+n.ahead,
# przy generowaniu realizacji wariancji oraz y_t wykorzystuje siê, wy³¹cznie oszacowania parametrów dla zaobserwowanej 
# próby oraz bootstrapowe realizacje standaryzowanych reszt, dla ka¿dego momentu uzyskujemy n.bootpred 
# prognoz wariancji oraz wartoœci y_t

(bootp <- ugarchboot(fit, method = "Full", n.ahead = 50, n.bootpred = 100, n.bootfit = 100, verbose = TRUE))
slotNames(bootp)

# Realizacje (symulacje) y_t, t = T+1, ..., T+n.ahead z bootstrapowego rozk³adu predyktywnego
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

# OpóŸnienie szeregu czasowego (y_t-k)
ytm1 <- lag(x = d3years, k = 1) 
head(d3years)
head(ytm1)

# Kolejne opóŸnienia szeregu wraz z wyjœciowym szeregiem (y_t, y_t-1, ..., y_t-k)
k <- 3
ylags <- embed(d3years, dimension = k+1)
colnames(ylags) <- c("yt", paste("ytm", 1:3, sep = "")) 
head(ylags)
(ylagsxts <- xts(ylags, order.by = index(d3years)[-(1:3)]))

# Data systemowa, Czas systemowy
Sys.Date()
Sys.time()

# apply.daily, apply.weekly, period.apply - statystyki za tydzieñ, miesi¹c, kwartalne, roczne
(monthly_stats <- apply.monthly(d3years, FUN = mean))
(monthly_stats1 <- apply.monthly(d3years, 
                        FUN = function(x) c(volat = sd(x)*252^0.5, 
                        quantDisp = IQR(x)/(quantile(x, 0.75)+quantile(x, 0.25)))))

# endpoints
# to.minutes(x,k,...), to.minutes3(x,name,...), to.minutes5, to.minutes10, to.minutes15, to.minutes30
# to.hourly, to.daily, to.weekly, to.monthly

(endp <- endpoints(d3years,on = "months", k = 2)) # co dwa miesi¹ce
d3years[endp,]

# to.period, to.monthly, to.weekly
to.period(d3years, period = "months", k = 2)  # co dwa miesi¹ce (takie same wyniki jak w przypadku wykorzystania indeksowania 
                                              # w oparciu o endpoints)
to.monthly(d3years, OHLC = FALSE)
to.weekly(d3years, OHLC = FALSE)
to.weekly(d3years, OHLC = TRUE)

# Wiêcej mo¿liwoœci pakiet highfrequency - dane transakcyjne i z arkusza zleceñ 
library(highfrequency)


########################################################################################################
# Import z plików zebranych w archiwum zip 
# (dane dzienne z rynków finansowych - BOSSA.pl: http://bossa.pl/notowania/metastock/)
# (dane transakcyjne /tickowe/ z rynków finansowych - BOSSA.pl: http://bossa.pl/notowania/pliki/intraday/metastock/)

# Wypisanie plików w katalogu

# list.files(path = ".", pattern = NULL, all.files = FALSE,
#            full.names = FALSE, recursive = FALSE,
#            ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
# Wypisanie podkatalogów
# list.dirs(path = ".", full.names = TRUE, recursive = TRUE)


# Rozpakowanie pliku zip
temp <- tempfile()
download.file("http://bossa.pl/pub/indzagr/mstock/mstzgr.zip", temp)   # Pobieramy plik mstzgr.zip z notowaniami indeksów gie³d œwiatowych

# list = TRUE - wypisuje zamiast œci¹gaæ pliki z archiwum zip
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
# Funkcja, która ³¹czy szeregi cen na zamkniêcie sesji, w wielowymiarowy szereg xts 
mergeToxts<- function(list_not, join = "inner", ...){
  data <- NULL
  for(i in 1:length(list_not)) {
    tmp <- xts(list_not[[i]][,6], order.by = as.POSIXct(as.character(list_not[[i]][,2]), format = "%Y%m%d"))
    data<- merge.xts(data, tmp, join = join, ...)
    colnames(data)[i] <- list_not[[i]][1,1]}
  data}

# Tworzy obiekt xls zawieraj¹cy wielowymiarowy szereg czasowy notowañ, join = "inner", oznacza, ¿e wielowymiarowy 
# szereg obejmuje daty z iloczynu zbiorów dat dla jednowymiarowych szeregów czasowych
(wielnot <- mergeToxts(list_not = listanot, join = "inner"))
# join = "outer" - szereg wielowymiarowy obejmuje notowania dla sumy zbiorów dat dla jednowymiarowych szeregów 
# (wprawadza siê odpowiednio braki danych NA)
(wielnotout <- mergeToxts(list_not = listanot, join = "outer")) 

plot(as.ts(wielnot), main = "Notowania indeksów wybranych gie³d œwiatowych")

index(wielnot) # Wyci¹gniêcie indeksów z obiektu xts

# Dzia³ania na zbiorach (pakiet base): union - suma mnogoœciowa, intersect - iloczyn mnogoœciowy, setdiff - ró¿nica zbiorów,
# setequal - równoœæ zbiorów, is.element - przynale¿noœæ do zbioru

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

# Wyszukiwanie zbiorów danych wœród kategorii BDL GUS, zawieraj¹cych odniesienia do frazy "Szko³y wy¿sze" 
wysz <- getBDLsearch("Szko³y wy¿sze")
View(wysz)

# Pobieranie zbioru danych "Szko³y wy¿sze" z metric_id = 1042
# Niestety udostêpnione API "MojePanstwo" od jakiegoœ czasu nie ma mo¿liwoœci pobierania danych BDL GUS
BDLszkol <- getBDLseries(metric_id = 1042)

####################################################################################################

# Pobieranie danych z repozytorium Eurostat
# pakiet eurostat: https://cran.r-project.org/web/packages/eurostat/vignettes/eurostat_tutorial.pdf
install.packages("eurostat")

# £adowanie pakietu
library(eurostat)
# library(rvest)

# Wykaz kategorii danych Eurostat
toc <- get_eurostat_toc()
View(toc)

# Wyszukiwanie zbiorów danych (lub ich folderów) w bazie Eurostatu, 
# zawieraj¹cych odniesienia do frazy "HICP" (zharmonizowany wskaŸnik cen konsumpcyjnych), 
# typ wyszukiwanych obiektów: type = c("dataset", "folder", "table", "all")
# przeszukiwana jest baza: http://ec.europa.eu/eurostat/data/database

wyszHICP <- search_eurostat("HICP", type = "all")
View(wyszHICP)
# Wyci¹gniêcie numeru id z wyników wyszukiwania
(idnum <- wyszHICP$code[2])

# Zbiory danych s¹ pobierane z Eurostatu za pomoc¹ tzw. masowego pobierania (bulk download) albo JSON API
# (umo¿liwiaj¹cego filtrowanie pobieranych danych). Je¿eli w funkcji get_eurostat, wska¿emy tylko id tabeli
# skorzystamy z masowego pobierania, je¿eli dodatkowo okreœlimy filtry skorzystamy z JSON API.


# Pobieramy znaleziony zbiór danych o id "prc_hicp_midx" za pomoc¹ funkcji get_eurostat:
# 
# type = c("code", "label") - rodzaj zwracanych nazw zmiennych: code - kod Eurostat, label - pe³na nazwa
# filter - lista ograniczeñ z elementami: 
# geo = c("EU28", "FI") - kody Eurostatu odnosz¹ce siê do jednostek terytorialnych 
# time, sinceTimePeriod = 2000 (dane od roku 2000 do ostatniego dostêpnego),
# lastTimePeriod = 10 - zwraca dane odnosz¹ce siê do 10 ostatnich lat w zbiorze danych

# pobieranie masowe - bez filtracji (pobieramy z pe³nymi nazwami subindykatorów Eurostatu: type = "label")
hicpData <- get_eurostat(idnum, type = "label", time_format = "num",
                         select_time = NULL, cache = TRUE, update_cache = FALSE,
                         cache_dir = NULL, compress_file = TRUE,
                         stringsAsFactors = FALSE, keepFlags = FALSE)

dim(hicpData)
View(hicpData)

# pobieranie masowe - bez filtracji (pobieramy z kodami subindykatorów Eurostatu: type = "code")
hicpDataCode <- get_eurostat(idnum, type = "code", time_format = "num",
                         select_time = NULL, cache = TRUE, update_cache = FALSE,
                         cache_dir = NULL, compress_file = TRUE,
                         stringsAsFactors = FALSE, keepFlags = FALSE)

dim(hicpDataCode)
View(hicpDataCode)

# unit: "Index, 2005=100" odpowiada mu kod I05
# coicop: "All-items HICP" odpowiada mu kod CP00

# Usuniêcie danych z podrêcznego "schowka"
clean_eurostat_cache()

# Wykaz krajów z grup: eu_countries, ea_countries, efta_countries, candidate_countries
eu_countries
ea_countries
efta_countries
candidate_countries

# Filtracja - maksymalnie mo¿na wskazaæ 50 subindykatorów Eurostat
# w liœcie filters wskazujemy:
# sinceTimePeriod = 2000 - tutaj pobieramy dane od 2000 roku
# geo = c(eu_countries[, 1], "EU28", "CH") - poszczególne kraje UE, 
hicpFiltered <- get_eurostat(idnum, type = "label", lang = "en", filters = list(sinceTimePeriod = 2000, 
                                                                                geo = c(eu_countries[, 1], "EU28", "CH"), unit="I05", coicop="CP00"), 
                             stringsAsFactors = default.stringsAsFactors())
View(hicpFiltered)

####################################################################################################

# Dane finansowe z Google Finance oraz dane makroekonomiczne dla USA z FRED 
# pakiet quantmod, funkcja getSymbols
library(quantmod) 
# Dane dzienne (OHLC) - akcje, indeksy (Ÿród³o Google Finance)
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
(xData <- getURL(link))  # Otwiera dostêp do linku
(tabele <- readHTMLTable(xData, stringsAsFactors = FALSE))   # Odczytuje i zapisuje wszystkie tabele - wartoœci pomiêdzy znacznikami <table> </table>
length(tabele)   # Liczba odczytanych tabel
names(tabele)  # Nazwy tabel (Chcemy wybraæ "Gini coefficient, before taxes and transfers[10]")

gini_before_tax <- tabele[[7]]
head(gini_before_tax)
(gini_before_tax<- gini_before_tax[-1,])  # Pierwszy wiersz tabeli jest pusty, wiêc go usuwamy
class(gini_before_tax[, 2]) # Kolumny ramki danych traktowane s¹ jako tekstowe

# Zamiana kolum ramki na typ numeric
gini_before_tax[, -1] <- sapply(2:(ncol(gini_before_tax)), function(i) as.numeric(gini_before_tax[, i]))
gini_before_tax
class(gini_before_tax[, 2])

# Pobieramy tabelê "Gini coefficient, after taxes and transfers[10]" 
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
# Okreœlamy œcie¿kê do pliku reg_es.xlsx
path <- choose.files() # uruchamia okno dialogowe wyboru pliku, do którego œcie¿ka zostanie przypisana do path
path
?read.xlsx
# xlsxFile - œcie¿ka do skoroszytu xlsx, sheet - numer lub nazwa arkusza do pobrania, startRow - numer wiersza,
# od którego rozpoczyna siê pobieranie danych , colNames = TRUE, w tabeli s¹ nag³ówki, rowNames - tabela zawiera
# nazwy obserwacji, rows - wektor numeryczne, którego elementy reprezentuj¹ numer kolumn do pobrania
przestrz <- read.xlsx(xlsxFile = path, sheet = 1, startRow = 1, colNames = TRUE,
                      rowNames = FALSE, rows = NULL, cols = NULL)
head(przestrz)

# Pozosta³e pakiety do importu danych z xlsx
# library(gdata) #Wymaga pearl
# read.xls()
# 
# library(xlsx)  # wymaga Java
# read.xlsx(); write.xlsx()

########################################################################################################
########################################################################################################
########################################################################################################

# Dane dotycz¹ce szkolnictwa - BDL GUS

# Kopiujemy z pliku szkolnictwo_skrot_nazwy.xlsx do schowka nazwy jednostek i wartoœci zmiennych B1:PR24
(dataszk <- read.table("clipboard", sep = "\t", dec = ",", header = TRUE, stringsAsFactors = FALSE, row.names = 1,
                      na.strings = "-"))

View(dataszk)
datawoj <- dataszk[!grepl("Region", rownames(dataszk)) & rownames(dataszk) != "POLSKA", ]
datawoj <- datawoj[order(rownames(datawoj)),]
dim(datawoj)    # Wymiary uzyskanej ramki danych
colnames(datawoj)
(rownames(datawoj) <- tolower(rownames(datawoj)))  # zamiana nazw wierszy na ma³e litery
View(datawoj)  # Dane panelowe: 16 jednostek x 18 jednostek czasowych (reprezentacja danych tzw. "szeroka")

# dplyr - pakiet do manipulacji na ramkach danych zawiera m.in. funkcje: 
# select - wybór podzbioru zmiennych (kolumn) z ramki danych, filter - wybór podzbioru obserwacji (wierszy) w oparciu
# o warunki logiczne, mutate - tworzenie nowych zmiennych (kolumn), z przekszta³cenia istniej¹cych zmiennych (kolumn)
# i wiele innych
library(dplyr)
help(package = "dplyr") # Wyszczególnienie funkcji pakietu dplyr 

# Wybór podzbioru cech - select
# select(data, starts_with(string))
# select(data, ends_with(string))
# select(data, contains(string))
# select(data, matches(".t."))
# select(data, varname1, varname2)
# vars <- c("varname1", "varname2")
# select(iris, one_of(vars))

select(datawoj, starts_with("swo")) # Wybierz zmienne o nazwach rozpoczynaj¹cych siê od "swo"
select(datawoj, un.std.1999:un.std.2016, wse.std.1999:wse.std.2016, wsz.std.1999:wsz.std.2016)

select(datawoj, matches(".16"))

(datawoj <- mutate(datawoj, wojew = rownames(datawoj)))

# tidyr - pakiet do czyszczenia danych, dla danych panelowych - zamiana postaci "szerokiej" na "w¹sk¹" i odwrotnie
library(tidyr) 
help(package = "tidyr") # Wyszczególnienie funkcji pakietu

# Przejœcie do postaci "w¹skiej" dla zmiennej wse.abs
wse <- gather(datawoj, key = year, value = wse.abs, wse.abs.1999:wse.abs.2016)
(wse <- select(wse, wojew, year, wse.abs))
(wse <- separate(data = wse, col = year, sep ="\\.", into = c("name1", "name2", "year"), remove = TRUE))
(wse <- select(wse, wojew, year, wse.abs))

# Wszystkie zmienne przejœcie do postaci "w¹skiej" z wszystkimi zmiennymi zbioru danych
allvar <- gather(datawoj, key = year, value = values, -wojew)
allvar <- separate(data = allvar, col = year, sep ="\\.", into = c("name1", "name2", "year"), remove = TRUE)
(allvar <- unite(allvar, col = varname, name1, name2, sep = "."))
(allvar <- spread(allvar, key = varname, value = values))
View(allvar)

# filter - zwraca podzbiór obserwacji spe³niaj¹cych warunek logiczny dotycz¹cy wartoœci zmiennej/zmiennych

# #  Wiele kryteriów
# filter(data, x1 < 6 & x2 == 1)   # & - operator logiczny oraz
# filter(data, x1 < 6 | x2 == 1)   # | - operator logiczny lub
# # Podanie wielu warunków po przecinkach, oznacza ich koniunkcjê (czyli zapis jest równoznaczny z zastosowaniem &)
# filter(data, x1 < 6, x2 == 1)

filter(allvar, year == 2016, swo.og³ > 30)
# równowa¿nie filter(allvar, year == 2016 & swo.og³ > 30)
filter(allvar, swo.abs > median(swo.abs) | swo.og³ > 15) 

colnames(allvar)
# mutate - Tworzenie nowych zmiennych, z przekszta³cenia ju¿ istniej¹cych w ramce danych 
# (nowa zmienna jest do³¹czana do ramki danych)
(dodzm <- mutate(allvar, abs.na.sw = swo.abs/swo.og³))
# transmute - tworzy now¹ zmienn¹, lecz nie dodaje jej do ramki danych 
(bezzm <- transmute(allvar, abs.na.sw = swo.abs/swo.og³))

# Funkcje dla zmiennych ramki danych (w oparciu o wartoœci w kolumnach ramki danych)
summarise(allvar, mean(wse.abs), sd(wse.abs))

# Statystyki grupowe
library(moments) # m.in. funkcje umo¿lwiaj¹ce wyznaczenie wspó³czynnika asymetrii i kurtozy
summarise(group_by(allvar, wojew), mean(swo.abs), sd(swo.abs), skewness(swo.abs), kurtosis(swo.abs))
summarise(group_by(allvar, year), mean(swo.abs), sd(swo.abs),  skewness(swo.abs), kurtosis(swo.abs))

library(psych)
# Podstawowe statystyki opisowe
describe(select(allvar, - wojew, -year))
# Statystyki opisowe warunkowe wzglêdem wartoœæi zmiennej dyskretnej
describeBy(allvar, group = "wojew")

(dane_2016 <- filter(allvar, year == 2016))
(techn_2016 <- select(dane_2016, starts_with("wst")))
  
# Kwantyle rozk³adu empirycznego
quantile(techn_2016[,1])
summary(techn_2016)

# Wspó³czynniki korelacji z próby dla par zmiennych
# method =  c("pearson", "kendall", "spearman")) - opowiednio wspó³czennik korelacji r Pearson, 
# wspó³czynnik korelacji rang Speramana, wspó³czynnik tau-Kendalla
(wsp_kor <- cor(techn_2016, method = "pearson"))

# alternative = c("two.sided", "less", "greater")
cor.test(techn_2016[,1], techn_2016[,2], alternative = "two.sided", method = "pearson",
         exact = NULL, conf.level = 0.95, continuity = FALSE)

library(reshape)
(cp <- melt(wsp_kor))

library(ggplot2)
ggplot(cp, aes(X1, X2, fill = value)) + geom_tile() + 
  scale_fill_gradient(low = "yellow",  high = "red") + ggtitle("Correlation Heatmap")

# Okreœlenie frakcji brakuj¹cych danych dla poszczególnych zmiennych oraz ich "struktury"
library(VIM)
aggr_plot <- aggr(allvar, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(data), cex.axis=.7, gap=3, ylab=c("Frakcja brakuj¹cych danych","Struktura"))


# na.fail - zatrzymanie procedury w przypadku braków danych, na.omit - pomija bezpowrotnie brakuj¹ce dane,
# na.exclude - pomija brakuj¹ce dane, ale zachowuje informacjê o ich po³o¿eniu, dziêki czemu np. w sytuacji predykcji,
# brakuj¹ce przypadki s¹ uwzglêdniane, na.pass - brak dzia³añ dotycz¹cych brakuj¹cych danych 
is.na(allvar)
complete.cases(allvar)
mean(complete.cases(allvar))

allvar[!complete.cases(allvar),]

# Imputacja brakuj¹cych danych
# MCAR (Missing Completly at Random) - prawdopodobieñstwo wyst¹pienia braku danych dla danej zmiennej nie zale¿y od
# wartoœci zmiennych obserwowalnych i nieobserwowalnych
# MAR (Missing at Random) - prawdopodobieñstwo wyst¹pienia braku danych dla danej zmiennej zale¿y od wartoœci
# zmiennych obserwowalnych,
# MNAR (Missing not at Random) - przypadek, który nie jest MCAR ani MNAR, np. prawdopodobieñstwo wyst¹pienia 
# braku danych dla danej zmiennej zale¿y od wartoœci tej¿e zmiennej

library(mice)
md.pattern(allvar)

##################################################################################################
##################################################################################################
##################################################################################################
# Zapisywanie danych w postaci pliku binarnego R (.RData)
ls() # zwraca nazwy wszystkich obiekty w przestrzeni roboczej
# Zapisywanie obiektów do pliku RData
# list - wektor nazw obiektów, które chcemy zapisaæ; list = ls() zapisuje wszystkie obiekty;
# file - œcie¿ka i nazwa pliku z rozszerzeniem RData, w przypadku, gdy œcie¿ka nie jest podana
# plik zapisywany jest w katalogu roboczym
save(list = ls() , file = "wszystkie.RData")
save(list = c("allvar"), file = "wybrane.RData")

# zapisany plik RData mo¿na uruchomiæ za pomoc¹ funkcji load
getwd()                        # Sprawdzamy katalog roboczy
load(file = "wybrane.RData")   # Je¿eli plik RData, z którego dane chcemy xa³adowaæ znajduje siê w katalogu roboczym
                               # podajemy tylko nazwê pliku, w przeciwnym razie podajemy tak¿e œcie¿kê do pliku
                               # lub zmieniamy katalog roboczy
setwd(path)                    # zmiana katalogu roboczego, przy czym path to œcie¿ka, np. path <- "C:\\Analizy"
setwd(choose.dir())            # to samo, tylko choose.dir otwiera okno dialogowe, które umo¿liwia wybór katalogu, do którego
                               # œcie¿ka jest przekazywana do setwd

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

# Histogram rozk³adu empirycznego
# breaks (do wyboru): wektor zaieraj¹cy punkty podzia³u, nazwa funkcji wyznaczaj¹cej punkty podzia³u, 
# pojedyncza wartoœæ wskazuj¹ca liczbê klas, nazwa wskazuj¹ca jedn¹ z metod podzia³u: "Sturges", "Scott",
# "Freedman-Diaconis"
# freq = TRUE - histogram czêstoœci (liczebnoœci), freq = FALSE - histogram gêstoœci (liczebnoœæ/d³ugoœæ przedzia³u klasy)
hist(allvar$wse.abs, col = "blue", main = "Empiryczny rozk³ad liczby absolwentów WSE", 
     sub = "Próba po³¹czona: województwa x lata", freq = FALSE, breaks = "Sturges", xlab = "Absolwenci WSE")
# J¹drowy estymator gêstoœci dla próby po³¹czonej: lata 1999-2016 x województwa
plot(density(allvar$swo.abs), main = "J¹drowy estymator gêstoœci dla próby po³¹czonej: województwa x lata")   
hist(allvar$wse.abs, plot = FALSE, breaks = "Sturges")
plot(ecdf(allvar$wse.abs), main = "Dystrybuanta rozk³adu empirycznego - próba po³¹czona")

# Wykres ramka-w¹sy dla próby po³¹czonej - znikoma "informacyjnoœæ"
boxplot(allvar$swo.abs, horizontal = FALSE)

# Rozk³ad empiryczny liczby absolwentów ogó³em wzglêdem województw za pomoc¹ wykresu ramka-w¹sy
boxplot(split(allvar$swo.abs, allvar$wojew), horizontal = TRUE, main = "Liczba absolwentów ogó³em wzglêdem województw",
        col = "lightblue")
# Rozk³ad empiryczny liczby absolwentów ogó³em wzglêdem lat za pomoc¹ wykresu ramka-w¹sy
boxplot(split(allvar$swo.abs, allvar$year), horizontal = TRUE, main = "Liczba absolwentów ogó³em wzglêdem lat", col = "lightgreen")

(swyzog <- select(allvar, wojew, year, matches("swo.")))


(wojewlab <- as.numeric(as.factor(swyzog$wojew)))
(yearlab <- as.numeric(swyzog$year)%%1998)   # %% funkcja modulo: year mod 1998 
  
# Macierzowe wykresy rozrzutu (kategorie zmiennej dyskretnej - kolor znacznika)
pairs(swyzog[, -(1:2)], col = wojewlab, pch = 16, main = "Macierzowy wykres rozrzutu - kategorie: województwa")
pairs(swyzog[, -(1:2)], col = yearlab, pch = 16, main = "Macierzowy wykres rozrzutu - kategorie: lata")

# Dyskretyzacja zmiennej ci¹g³ej
(tuk5nx3 <- fivenum(swyzog[, 3]))  # 5 liczb Tukeya (minimum, lower-hinge, mediana, upper-hinge, maksimum)
(x3disc <- cut(swyzog[, 3], breaks = tuk5nx3, labels = NULL, include.lowest = TRUE,  right = TRUE, dig.lab = 3,
       ordered_result = TRUE))

(tuk5nx5 <- fivenum(swyzog[, 5]))  # 5 liczb Tukeya (minimum, lower-hinge, mediana, upper-hinge, maksimum)
(x5disc <- cut(swyzog[, 5], breaks = tuk5nx5, labels = NULL, include.lowest = TRUE,  right = TRUE, dig.lab = 3,
               ordered_result = TRUE))

(tb <- table(x5disc, useNA = "no"))  # Tablica liczebnoœci wyst¹pieñ kategorii
(ptb <- prop.table(tb))

# Tablice kontyngencji (tutaj dwudzielcze)
(tb2d <- table(x3disc, x5disc))
(ptb2d <- prop.table(tb2d, margin = 2)) # margin = 1 - kolumny sumuj¹ siê do 1, margin = 2 - wiersze sumuj¹ siê do 1

plot(ptb2d, main = "Graficzna reprezentacja tablicy dwudzielczej", col = "lightblue")

# Test niezale¿noœci chi-kwadrat dla dwóch cech jakoœciowych
(chiwynik <- chisq.test(tb2d))
chiwynik$expected

# fisher.test(tb2d)
mcnemar.test(tb2d)

# Wiele zmiennych na jednym wykresie: matplot, matpoints, matlines

# Wyznaczmy wartoœci trzech funkcji wielomianowych, w punktach w ramach przyjêtej siatki
x <- seq(from = -1, to = 2, length = 20)
y1 <- 1.2 * x^3 - x^2 - 3.0*x + 2.0
y2 <- 1.5 * x^3 - 0.9 * x^2  - 2.0 * x + 1.0
y3 <- 0.7 * x^3 - 1.3*x^2 - 0.6 * x + 0.4

# Adnotacje na wykresach:
# https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/plotmath.html

(bindedpoints <- cbind(y1, y2, y3))  # £¹czymy wartoœci funkcji wielomianowych w punktach siatki w macierz
matplot(x, bindedpoints , type = "l", lwd = 3, lty = 1:3, col = 1:3, xlim = c(-1, 2), ylim = c(-1, 5.5) ,
        xlab = "x", ylab = expression(w[i](x)), main = "Wykres wielomianów")
legend("topleft", legend = c("w1", "w2", "w3"), lty = c(1,2,3), col = 1:3)


# Wykres funkcji matematycznych
f <- .5         # czestotliwosc
phi <- 2        # przesuniecie fazowe
curve(sin(2*pi*f*x + phi), from = -10, to = 10, n = 1000, type = "l", col = "red")

# Wykresy 3D

# Wykres powierzchni funkcji dwóch zmiennych z = f(x,y) - funkcja persp
x <- seq(-1.95, 1.95, length = 30)
y <- seq(-1.95, 1.95, length = 35)
(z <- outer(x, y, function(a, b) a*b^2))  # Wartoœci funkcji w punktach dla ka¿dej mo¿liwej 
                                          # pary wartoœci x oraz y
(nrz <- nrow(z))
(ncz <- ncol(z))

# Paleta przejœcia kolorów do niebieskiego do zielonego
(paleta <- colorRampPalette( c("blue", "green")))
# Wybieramy n = 100 kolorów ze stworzonej palety
nbcol <- 100
(kol <- paleta(n = nbcol))

zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
facetcol <- cut(zfacet, nbcol)
persp(x, y, z, col = kol[facetcol], phi = 30, theta = -30, main = "Wykres skalarnej funkcji dwóch zmiennych")

# Wykresy warstwicowe i konturowe filled.contour z pakietu graphics)
filled.contour(x, y, z, color = terrain.colors, plot.title = title(main = "Wykres konturowy dla funkcji z = xy^2",
              xlab = "x", ylab = "y"), 
              plot.axes = { axis(1, seq(-2, 2, by = 0.2))
                            axis(2,  seq(-2, 2, by = 0.2)) },
              key.title = title(main = "Wartoœæ z"), key.axes = axis(4, seq(-8, 8, by = 1)))


# Wykres rozrzutu 3D
library(scatterplot3d)
colnames(swyzog)
wojewlab
scatterplot3d(x = swyzog$swo.og³ , y = swyzog$swo.std , z = swyzog$swo.abs, color = wojewlab, pch = 16,
              xlab = c("Szko³y wy¿sze ogó³em"), ylab = "Liczba studentów", zlab = "Liczba absolwentów",
              main = "Trójwymiarowy wykres rozrzutu")

##################################################################################################

# ggplot2 - sk³adnia dla tworzenia wykresów, umozliwiaj¹ca bardziej œcis³¹ kontrolê ich elementów 
# http://ggplot2.tidyverse.org/reference/
# Szczegó³y dotycz¹ce tworzenia wykresów z ggplot2:
# Wickham H., "ggplot2: Elegant Graphics for Data Analysis"

library(ggplot2)
help(package = "ggplot2")
# Histogram ggplot2: geom_histogram
# mapping = aes() - aestethics ("estetyki") - okreœlenie zmiennych, których wartoœci przedstawiane bêd¹ na wykresie:
# aes(x = zm1, y = zm2, col = zm3, size = zm4)
# labs - nazwy wykresu (title), osi (x, y)
# theme - specyfikacja wygl¹du wykresu
ggplot(data = allvar) + geom_histogram(mapping = aes(x = wse.abs), binwidth = 500, color = "green", fill = "red") + 
  labs(title = "Empiryczny rozk³ad liczby absolwentów WSE", subtitle = "Próba po³¹czona: województwa x lata", 
       x = "Absolwenci WSE", y = "liczebnoœæ") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.5), face = "bold", colour = "green"),
        plot.subtitle = element_text(hjust = 0.5, face = "italic"),
        panel.background = element_rect(fill = "white", colour = "grey50"))

# Tworzenie szablonu wykresu (do którego mo¿na dodawaæ kolejne elementy poprzez "+")
plottempl <- ggplot(data = allvar) + labs(x = "Absolwenci WSE")
plottempl + geom_density(aes(x = wse.abs)) + labs(y = "gêstoœæ") + labs(title = "J¹drowy estymator gêstoœci") 

# Zapisanie ostatniego wyœwietlonego wykresu do pliku
ggsave(filename = "wykres.pdf", plot = last_plot())

names(swyzog)

# Wykres rozrzutu
(scatt <- ggplot(data = swyzog) + geom_point(mapping = aes(x = swo.og³, y = swo.abs, col = wojew, size = swo.std)) +
  labs(title = "Wykres rozrzutu absolwenci szkó³ wy¿szych vs. liczba szkó³ wy¿szych",
       x = "szko³y wy¿sze ogó³em", y = "absolwenci szkó³ wy¿szych", colour="Województwo", size = "Liczba studentów"))

# Wyg³adzanie: method= c("lm", "glm", "gam", "loess", "rlm"), wybór method = "auto" odpowiada wyborowi "loess"
# lm - KMNRL (KMNK), glm - uogólniony model regresji liniowej (do wskazania funkcja przejœcia + typ rozk³adu 
# warunkowego), gam - uogólnione modele addytywne, rlm - odporna regresja liniowa (M estymator, domyslnie
# wagi zadane przez funkcjê psi Hubera)
scatt + geom_smooth(mapping = aes(x = swo.og³, y = swo.abs), method = "auto", formula = y ~ x, se = TRUE)

# Skala logarytmiczna na wykresie rozrzutu (log10, sqrt, reverse)
scatt + scale_x_log10() + scale_y_log10()

# Wykres ramka-w¹sy
# axis.text.x = element.blank() - usuniêcie nazw jednostek z osi x
ggplot(data=swyzog) + geom_boxplot(aes(x=wojew, y=swo.abs, fill=wojew)) + 
  labs(title = "Wykres ramka-w¹sy", x = "województwo", y = "absolwenci szkó³ wy¿szych", fill = "województwo") +
  theme(axis.text.x = element_blank(), plot.title = element_text(hjust = 0.5))

# Wykres szeregu czasowego
ggplot(data = d3years) + geom_line(mapping = aes(x = index(d3years), y = d3years))+
  labs(title = "Wykres dziennych logarytmicznych stóp zwrotu WIG20", x = "Czas", y = expression(y[t]))+
  theme(plot.title = element_text(hjust = 0.5))

# Skrócona sk³adnia wykresu ggplot2 - qplot
qplot(x = swo.og³, y = swo.abs, data = swyzog, color = wojew, size= swo.std, geom = "point",
      main= "Wykres rozrzutu absolwenci szkó³ wy¿szych vs. liczba szkó³ wy¿szych")


# Narzêdzia graficzne z ggplot2 wykorzystane do prezentacji wyników analiz 
library(ggfortify)
# funkcja autoplot umo¿liwia prezentacjê wyników analiz z pomoc¹ ggplot2 zamiast narzêdzi z pakietu graphics
# Graficzna weryfikacja za³o¿eñ KMNRL
autoplot(lm(Petal.Width~Petal.Length, data = iris), data = iris, colour = 'Species')
# Wykres próbkowej ACF/PACF (opóŸnienie wyra¿one w sekundach 1d = 3600 s)
autoplot(stats::acf(d3years, plot = FALSE))
autoplot(stats::pacf(d3years, plot = FALSE))

# Wykres wielowymiarowego szeregu czasowego (jednowymiarowe szeregi sk³adowe na osobnych wykresach)
autoplot(wielnot[, 1:3])

# Szablony formatowania wykresów
library(ggthemes)
# theme_base - wygl¹d wykresu zbli¿ony do tych z pakietu graphics
# theme_calc - wzorowany na wykresie z LibreOffice Calc
# theme_economist - wykres z czasopisma "The Economist"
# theme_excel - wykres z szarym t³em wzorowany na tych z MS Excel
# theme_few - wykres oparty na: Few, “Practical Rules for Using Color in Charts”
# theme_gdocs - wykres Google Docs
# theme_hc - wykres wzorowany na Highcharts JS
# theme_par- wykres wykorzystuj¹cy bie¿¹ce ustawienia opcji par()
# theme_solarized: a theme using the solarized color palette.
# theme_stata - wzorowany na wykresie z oprogramowania Stata
# theme_tufte - wykres minimalizuj¹cy "zu¿ycie tuszu", oparty na: Tufte, "The Visual Display of Quantitative Information"
# theme_wsj - wykres z "The Wall Street Journal"

# The Economist
scatt + geom_smooth(mapping = aes(x = swo.og³, y = swo.abs), method = "auto", formula = y ~ x, se = TRUE) +
  theme_economist()

# Ms Excel
scatt + geom_smooth(mapping = aes(x = swo.og³, y = swo.abs), method = "auto", formula = y ~ x, se = TRUE) +
  theme_excel()

# Google Docs
scatt + geom_smooth(mapping = aes(x = swo.og³, y = swo.abs), method = "auto", formula = y ~ x, se = TRUE) +
  theme_gdocs()
