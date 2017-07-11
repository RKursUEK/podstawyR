

# Symulator danych z modelu KMNRL/ UMNRL (zak³adamy wiêc, ¿e regresory s¹ nielosowe)
# n -liczba obserwacji, k - liczba zmiennych objaœniaj¹cych, beta - wektor parametrów strukturalnych modelu KMNRL, 
# const - czy model uwzglêdnia parametr sta³ej, detcoef - wspó³czynnik determinacji modelu,
# sX - wariancje zmiennych X, errorType - typ reszt: spher_eps - reszty pochodz¹ z n-wymiarowego normalnego rozk³adu
# sferycznego, autocorr_eps - reszty generuje stacjonarny proces AR(1) /ar1_eps to parametr phi, |phi| < 1/,
# eps_t = phi * eps_t-1 + eta_t, {eta_t} ~ iin(0, sigma^2_eta), sigma^2_eta zosta³a tak dobrana, aby
# w rozk³adzie bezwarunkowym zmiennych, w ramach procesu sk³adników losowych {eps_t}
# wariancja by³a taka sama jak ta (przyjêto ozn. var_hom), która dla sferycznego rozk³adu wektora 
# sk³adników losowych, dawa³aby za³o¿ony poziom wspó³czynnika determinacji, tzn. sigma^2_eta = (1-ar1^2) * var_hom
      # reszty z procesu AR(1), generuje napisana funkcja corrErr
# 
# heterosc_eps - reszty pochodz¹ z procesu nieskorelowanych zmiennych, z heteroskedastyczn¹ wariancj¹,
# dwa warianty heteroskedastycznoœci: grupowa - group_heter = TRUE, 
# (subsobs_var - liczba jednostek w podgrupach o takiej samej wariancji sk³adnika losowego, wartoœæ wariancji
# sta³a dla jednostek poszczególnych grup, generowana jest z rozk³adu jednostajnego okreœlonego 
# na przedziale var_hom +/- vardisp_heter * var_hom

# wariancja sk³adników losowych zale¿na od wartoœci zmiennej niezale¿nej - indeksu wariancji, zak³ada siê 
# funkcje wielomianowe stopnia p_heter_fnc oraz wspó³czynniku k: var_eps(i) = var_hom + k *(i - (n+1)/2)^p, 
# k jest dobierane tak, aby dla ka¿dego i var_eps(i) > 0
      # heteroskedastyczne reszty, generuje napisana funkcja hetErr


simLinReg <- function(nreps = 1, n = 100, k = 5, detcoef = 0.9, seed = NULL, beta = NULL, const = TRUE,
                      CorX = NULL, sX = NULL, ndigRoundCorX = "no", errorType = "spher_eps" ,
                      ar1_eps = 0.7, group_heter = TRUE, vardisp_heter = 0.5, subsobs_var = 25, p_heter_fnc = 3, k_heter_fnc = NULL){
  if(nreps < 1) stop("nreps must be a positive, integer number")
  
  if(!is.null(seed)) set.seed(seed)
  if(is.null(beta)) {
    beta <- matrix(round(runif(k, -10, 10)))
    beta0 <- (2*rbinom(1, 1, .5)-1) * round(runif(1, 1, 10))
  } else if(const){
    beta0 <- beta[1]
    beta <- matrix(beta[-1])
  }
  if(is.null(sX)) sX <- round(runif(k,1, 5), 2)
  if(is.null(CorX)) {
    pdsmat <- crossprod(matrix(runif(k^2, -1, 1), ncol = k))
    Smhalf <- diag(pdsmat)^-.5
    if(ndigRoundCorX == "no") {CorX <- diag(Smhalf) %*% pdsmat %*% diag(Smhalf)
    }else{
      CorX <- round(diag(Smhalf) %*% pdsmat %*% diag(Smhalf), ndigRoundCorX)
    }
  }
  
  if(is.character(CorX) && CorX == "unit") CorX <- diag(k)
  if(is.null(sX)) {
    SigmaX <- CorX 
  } else {
    sXhalf <- diag(sX)^.5
    SigmaX <- sXhalf %*% CorX %*% sXhalf
  }
  
  X <- matrix(rnorm(n*k), ncol = k) %*% chol(SigmaX)
  yhat <- X %*% beta
  if(const) yhat <- beta0 + yhat 
  detvar <- t(beta) %*% SigmaX %*% beta
  if(const) detvar <- detvar + beta0^2
  eps_sd <- c(((1-detcoef)/ detcoef * detvar)^.5)
  if(nreps == 1){
    eps <- switch(errorType, spher_eps = rnorm(n, 0, eps_sd), autocorr_eps = corrErr(n = n, ar1 = ar1_eps,
                                                                                     eps_sd = eps_sd), heterosc_eps = hetErr(n = n, group = group_heter, vardisp = vardisp_heter, 
                                                                                                                             subsobs = subsobs_var, p = p_heter_fnc, k = k_heter_fnc, eps_sd = eps_sd) )
    y <- yhat + eps
  }else{
    y <- replicate(round(nreps), yhat[, 1] + switch(errorType, spher_eps = rnorm(n, 0, eps_sd), 
                                                    autocorr_eps = corrErr(n = n, ar1 = ar1_eps, eps_sd = eps_sd), 
                                                    heterosc_eps = hetErr(n = n, group = group_heter, vardisp = vardisp_heter, 
                                                                          subsobs = subsobs_var, p = p_heter_fnc, k = k_heter_fnc, eps_sd = eps_sd)))
  }
  if(const) beta <- rbind(beta0, beta)
  rownames(beta) <- NULL
  if(const) X <- cbind(1,X)
  XtX <- n * SigmaX
  if(const) {XtX <- cbind(0, XtX)
  XtX <- rbind(c(n, rep(0, k)), XtX)}
  parEstCov <- solve(XtX)*eps_sd^2
  XtXgen <- crossprod(X)
  detCorX <- det(CorX)
  if(const) detCorXgen <- det(cor(X[,-1])) else detCorXgen <- det(cor(X))
  epsilon <- y - yhat[,1]
  list(y = y, X = X, yhat = yhat, epsilon = epsilon, CorX = CorX, SigmaX = SigmaX, beta = beta, eps_sd = eps_sd, XtX = XtX, XtXgen = XtXgen,
       detCorX = detCorX, detCorXgen = detCorXgen,
       parEstCov = parEstCov, const = const)
}




# Funkcja pomocnicza (wykorzystywana przez funkcjê simLinReg) 
# umo¿liwiaj¹ca generowanie reszt modelu UMNRL ze stacjonarnego procesu AR(1) 
corrErr <- function(n = 100, ar1 = .7, eps_sd){
  if(abs(ar1) >= 1) stop("|ar1| >= 1 - Error Process is not stationary AR(1) Process")
  eps0 <- rnorm(1, 0,  eps_sd)
  eta_sd <- (1-ar1^2)^.5 * eps_sd
  eta <- rnorm(n, 0, eta_sd)
  eps <- eps0
  # epst <- sapply(1:n, function(i) {eps <<- ar1 * eps + eta[i]; eps})
  for(i in 1:n) eps[i+1] <- ar1 * eps[i] + eta[i]
  (eps <- eps[-1])
}

# Funkcja pomocnicza (wykorzystywana przez funkcjê simLinReg) 
# umo¿liwiaj¹ca generowanie reszt modelu UMNRL z procesu nieskorelowanych zmiennych o ró¿nej wariancji
# (sta³ej w podgrupach lub zadanej przez funkcjê niezale¿nej zmiennej indeksuj¹cej)
hetErr <- function(n = 100, group = TRUE, vardisp = 0.6, subsobs = 25, p = 2, k = NULL, eps_sd){
  if(group){
    if(subsobs > n) stop("Number of subgroups observations must be less than whole sample observations")
    
    nsub <- n%/%subsobs
    reps <- sort(rep(1:nsub, length.out = n))
    genhtscdtvar <- runif(nsub, eps_sd^2 * (1- vardisp), eps_sd^2 * (1+ vardisp))
    (eps_het <- rnorm(n, 0, genhtscdtvar[reps]^.5))
  } else {
    var_eps <- eps_sd^2
    tr <- abs(((1-n)/2)^-p) * var_eps
    if(is.null(k)) k <- 0.95*(2*rbinom(1,1,.5)-1)* tr
    if(abs(k) > tr) stop(paste("k should be in absolute value less than ", tr, sep =""))
    adds <-  k *((1:n)-(n+1)/2)^p
    if(p %% 2 == 0) adds[1:n < (n+1)/2] <- -adds[1:n < (n+1)/2]
    funhtscdtvar <- var_eps + adds
    (eps_het <- rnorm(n, 0, funhtscdtvar^.5))
  }
}


hetvar<- function(data, formula = y~. , lbls = NULL, subsobs = 25){
  if(is.null(lbls))
  { n <- nrow(data)
  nsub <- n%/%subsobs
  lbls <- sort(rep(1:nsub, length.out = n))}
  lmres <- c(sapply(1:nsub, function(i) lm(formula, data = data[lbls == i,])$residuals))
  var_lmres <- aggregate(lmres, list(lbls = lbls), FUN = var)
  colnames(var_lmres)[2] <- "group_var_eps"
  W <- diag(var_lmres[lbls, 2])
  list(group_eps_var = var_lmres, W = W, lmres = lmres, lbls = lbls)
}



# Statystyki opisowe wartoœci estymatorów KMNK dla danych symulowanych pochodz¹cych z modeli KMNRL/UMNRL
simcoefs <- function(simdata, regsubs = NULL){
  const <- simdata$const
  theor <- simdata$parEstCov
  y <- simdata$y
  Xsim <-  simdata$X
  cnames <- paste("X",1:(ncol(Xsim)-const), sep = "")
  if(const){
    cnames <- c("(Intercept)", cnames)
    X <- Xsim[, -1] 
    if(!is.null(regsubs)) X <- X[, regsubs]
    coefs <- t(sapply(1:ncol(y), function(i, ...) lm(y[,i]~X, ...)$coefficients))
  }else{
    X <- Xsim
    if(!is.null(regsubs)) X <- X[, regsubs]
    coefs <- t(sapply(1:ncol(y), function(i, ...) lm(y[,i]~X-1, ...)$coefficients))
  }
  coefquants<- apply(coefs,2, quantile, c(0.025, 0.975))
  coefMeans <- colMeans(coefs)
  covcoefs <- cov(coefs)
  dimnames(theor) <- list(cnames, cnames)
  nc <- ncol(coefs)
  plotnr <- nc %/%2
  par(mfrow = c(plotnr, 2))
  for(i in 1: nc) {hist(coefs[,i], freq = F, col = "green", xlab = paste("beta", i-1, sep = ""), 
                             main = paste("Histogram beta", i-1, sep = ""))}
  list(coefs = coefs, coefMeans = coefMeans, coefquantiles = coefquants, covCoefs = covcoefs, theor = theor)
}

# Funkcja przekszta³caj¹ca wyniki symulacji (y, X) z modelu KMNRL na ramkê danych
dflm <- function(simLin)  {
  y <- simLin$y
  nreps <- ncol(y)
  const <- simLin$const
  if(const) X <- simLin$X[,-1] else  X <- simLin$X
  if(nreps == 1) {
    df <- data.frame(cbind(y, X))
    colnames(df) <- c("y", paste("X",1:(ncol(X)), sep = ""))}
  else{
    cnames <- c("y", paste("X",1:(ncol(X)), sep = ""))
    df  <- lapply(1:nreps,function(i) {
      dfi <- data.frame(cbind(y[,i], X))
      colnames(dfi) <- cnames
      dfi})
  }
  df}

######################################################################################################################
######################################################################################################################
######################################################################################################################

# Wywo³ujemy funkcjê simLinReg z domyœlnymi wartoœciami argumentów, czyli generujemy jednokrotnie dane z modelu KMNK
# (spe³nione s¹ za³o¿enia twierdzenia Gaussa-Markowa o KMNRL):
# - model ze sta³¹ i 5 zmiennymi niezale¿nymi,
# - parametry strukturalne: wektor beta to wektor 6 wartoœci pseudolosowych (pierwszy element to sta³a modelu)
# - wspó³czynnik determinacji = 0,95
# - zgodnie z za³o¿eniami KMNRL: sferyczny rozk³ad normalny wektora sk³adników losowych modelu
# - struktura zale¿noœci liniowych zmiennych niezale¿nych: wygenerowana pseudlosowo macierz korelacji oraz wektor
#  wariancji zmiennych niezale¿nych,
# - wspó³czynnik determinacji = 0.90 (identyczna dla wszystkich jednostek wariancja sk³adnika losowego, jest dobierana
#   w taki sposób, aby uzyskaæ zadan¹ wartoœæ wspó³czynnika determinacji, przy ustlonej teoretycznej macierzy kowariancji 
#   zmiennych niezale¿nych modelu oraz ustalonych parametrach beta)
# - w symulacji zak³ada siê, ¿e wszystkie zmienne objaœniaj¹ce modelu maj¹ (teoretycznie) zerow¹ œredni¹
(wyn <- simLinReg())
names(wyn)

colMeans(wyn$X)

y <- wyn$y        # Wektor y Wygenrowanych wartoœci zmiennej zale¿nej 
X <- wyn$X[,-1]   # Macierz X wartoœci zmiennych objaœniaj¹cych

cov(X)            # Sprawdzamy strukturê liniowych zale¿noœci zmiennych objaœniaj¹cych w wygenerowanej próbie
cor(X)            # Sprawdzamy strukturê liniowych zale¿noœci zmiennych objaœniaj¹cych w wygenerowanej próbie

wyn$SigmaX        # Sprawdzamy teoretyczn¹ (za³o¿on¹ w symulacji) strukturê macierzy kowariancji dla zmiennych objaœniaj¹cych
wyn$CorX          # Sprawdzamy teoretyczn¹ (za³o¿on¹ w symulacji) strukturê macierzy korelacji dla zmiennych objaœniaj¹cych

wyn$detCorX       # Sprwadzamy wartoœæ wyznacznika dla macierzy korelacji zmiennych objaœniaj¹cych za³o¿onej w symulacji 
wyn$detCorXgen    # Sprwadzamy wartoœæ wyznacznika dla macierzy korelacji zmiennych objaœniaj¹cych z uzyskanej próby 
# Wartoœæ wyznacznika bliska 0 œwiadczy o silnej wspó³iniowoœci zmiennych objaœniajcych, wartoœæ równa 1 œwiadczy
# o braku zale¿noœci liniowych pomiêdzy zmiennymi objaœniaj¹cymi

# W oparciu o wygenerowan¹ próbê szacujemy wektor parametrów modelu KMNRL 
# z wykorzystaniem estymatora KMNK (minimalizuj¹cego kryterium najmniejszych kwadratów min_beta ||y - X * beta||^2
summary(lm(y ~ X))
# W rozwi¹zaniu problemu najmniejszych kwadratów  wykorzystuje siê dekompozycjê QR macierzy X
qr.solve(cbind(1, X), y)
wyn$beta   # Teoretyczne wartoœci parametrów modelu za³o¿one w symulacji

# Ustalamy wektor parametrów modelu KMNK, z którego bêdziemy jednokrotnie generowaæ dane:
# - model ze sta³¹ i 5 zmiennymi niezale¿nymi,
# - parametry strukturalne zadane: beta = [1,2,3,4,5,6], przy czym pierwszy element wektora to sta³a modelu
# - zgodnie z za³o¿eniami KMNRL: sferyczny rozk³ad normalny wektora sk³adników losowych modelu
# - struktura zale¿noœci liniowych zmiennych niezale¿nych: wygenerowana pseudlosowo macierz korelacji oraz diagonalna macierz
#  wariancji zmiennych niezale¿nych,
# - wspó³czynnik determinacji = 0.95 (identyczna dla wszystkich jednostek wariancja sk³adnika losowego, jest dobierana
#   w taki sposób, aby uzyskaæ zadan¹ wartoœæ wspó³czynnika determinacji, przy ustlonej teoretycznej macierzy kowariancji 
#   zmiennych niezale¿nych modelu oraz ustalonych parametrach beta)
(wyn1 <- simLinReg(nreps = 1, n = 100, k = 5, beta = 1:6, detcoef = 0.95, errorType = "spher_eps")) 

wyn1$beta
wyn1$detCorX
wyn1$detCorXgen

df1 <- dflm(wyn1)
(lm1 <- lm(y ~ ., data = df1))
names(lm1)
summary(lm(y ~ ., data = df1))
names(summary)

confint(lm1, level = 0.95)  # Przedzia³ ufnoœci dla oszacowañ wektora parametróW

# Variance Inflation Factor - ocena stopnia wspó³liniowoœci zmiennych objaœniaj¹cych (niezale¿nych modelu) w próbie
car::vif(lm1)

# Model KMNK
# nrep = 1000 powtórzeñ symulacji (zgodnie z za³o¿eniami KMNRL, macierz wartoœci zmiennych niezale¿nych X,
# nie ulega w kolejnych symulacjach zmianie, natomiast w ka¿dej symulacji uzyskuje siê pseudolosowe
# realizacje sk³adnika losowego)
(wyn2 <- simLinReg(nreps = 1000, n = 100, k = 5, beta = 1:6, CorX = NULL, detcoef = 0.95, 
                   errorType = "spher_eps", seed = 1234))

wyn2$beta           # Przyjête w symulacji wartoœci wektora beta
wyn2$detCorX        # Wyznacznik przyjêtej w symulacji macierz korelacji zmiennych objaœniaj¹cych
wyn2$detCorXgen     # Wyznacznik macierzy korelacji z próby wartoœci zmiennych objaœniaj¹cych

df2all <- dflm(wyn2)
(df2 <- df2all[[1]])

(lm2 <- lm(y ~ . , data = df2))
summary(lm2)

confint(lm2, level = 0.95)

# Variance Inflation Factor - ocena stopnia wspó³liniowoœci zmiennych objaœniaj¹cych (niezale¿nych modelu) w próbie
car::vif(lm2)


# Zjawisko randomizacji przy braku uwzglêdnienia istotnych regresorów w szacowanym modelu
# (wraz ze wzrostem próby spadek obci¹¿enia estymatorów parametrów przy nieuwzglêdnieniu istotnych regresorów)
summary(lm(y~ X1+ X2, data = df2))
(desc <- simcoefs(wyn2, regsubs = 1:2))

(wyn2large <-  simLinReg(nreps = 1000, n = 10000, k = 5, beta = 1:6, CorX = NULL, detcoef = 0.95,
                        errorType = "spher_eps", seed = 1234))

df2largeall <- dflm(wyn2large)
df2large <- df2largeall[[1]]

summary(lm(y~ X1+ X2, data = df2large))
(desclarge <- simcoefs(wyn2large, regsubs = 1:2))

# Porównanie œrednich wartoœci estymatorów dla symulacji
desc$coefMeans
desclarge$coefMeans


(wynrep1 <- simLinReg(nreps = 1000, n = 100, k = 5, beta = 1:6,  errorType = "spher_eps", seed = 1234)) 

(descrep1 <- simcoefs(wynrep1))

(wynrep2 <- simLinReg(nreps = 1000, n = 100, k = 5, beta = 1:6, CorX  = "unit",  errorType = "spher_eps", seed = 1234)) 


######################################################################################################################
######################################################################################################################
######################################################################################################################

# Model KMNRL przy braku wp³ywu na zmienn¹ objaœnian¹ podzbioru zmiennych niezale¿nych (beta = [1,2,3,4,0,0]')
# zmienne X4, X5 nie s¹ regresorami modelu 
(msub <- simLinReg(nreps = 1000, n = 100, k = 5, beta = c(1:4, rep(0,2)), seed = 1234, errorType = "spher_eps"))

(dfsub <- dflm(msub)[[1]])

# Model z uwzglêdnieniem wszystkich zmiennych niezale¿nych X1:X5
(lmsub <- lm(y~., data = dfsub))
summary(lmsub)

# Rosn¹ce uporz¹dkowanie submodeli wed³ug wartoœci kryterium BIC (inne mo¿liwoœci AIC, AICc, QAIC)
# preferowane s¹ ni¿sze wartoœci kryterium BIC
library(MuMIn)
options(na.action = "na.fail") 
MuMIn::dredge(lmsub, beta = "none", rank = "BIC")
# Najni¿sz¹ wartoœci¹ BIC charakteryzuje siê model ze sta³¹ i zmiennymi objaœniaj¹cymi X1:X3

# Estymacja Maximum a posteriori
library(glmnet)
# regresja LASSO (regularyzacja L1)
Xsub <- as.matrix(dfsub[,-1])
ysub <- as.matrix(dfsub[,1])
(lasso<-glmnet(Xsub, ysub , family = "gaussian", alpha = 1, standardize = TRUE))

cvlasso <- cv.glmnet(x = Xsub, y = ysub, alpha = 1, nfolds = 10)

plot(cvlasso)

cvlasso$lambda.min
cvlasso$lambda.1se

(min1se_cvlasso<-coef(cvlasso, s = "lambda.1se"))

######################################################################################################################
######################################################################################################################
######################################################################################################################

# Statystyczna wspó³liniowoœæ zmiennych objaœniaj¹cych

(CorX <- matrix(c(1,0.98, 0.2, 0.98, 1, 0.1, 0.2, 0.1, 1), ncol = 3))
library(matrixcalc)
is.singular.matrix(CorX)
is.symmetric.matrix(CorX)
is.positive.definite(CorX)

(mcolin <- simLinReg(nreps = 1000, n = 100, k = 3, CorX = CorX, beta = 1:4, seed = 1234, errorType = "spher_eps"))

#######################################
# regresja grzbietowa (regularyzacja L2)

(dfcolin <- dflm(mcolin)[[1]])

(lmcolin <- lm(y~., data = dfcolin))
summary(lmcolin)

Xcol <- as.matrix(dfcolin[,-1])
ycol <- as.matrix(dfcolin[,1])

(ridge<-glmnet(Xcol, ycol, family = "gaussian", alpha = 0, standardize = TRUE))

cvridge <- cv.glmnet(x = Xcol, y = ycol, alpha = 0, nfolds = 10)

plot(cvridge)

cvridge$lambda.min
cvridge$lambda.1se

(min1se_cvridge <- coef(cvridge, s = "lambda.1se"))

# Algebraiczna wspó³liniowoœæ zmiennych objaœniaj¹cych
##### 

(CorXalg <- matrix(c(1, 0.2, 0.2, 0.2, 1, 1, 0.2, 1, 1), ncol = 3))

library(matrixcalc)
is.singular.matrix(CorXalg)
is.symmetric.matrix(CorXalg)
is.positive.definite(CorXalg)
is.positive.definite(CorXalg[-3, -3])

# Uzyskamy b³¹d - macierz korelacji zmiennych objaœniaj¹cych CorXalg jest osobliwa
(mcolinalg <- simLinReg(nreps = 1, n = 100, k = 3, CorX = CorXalg, beta = 1:4, errorType = "spher_eps"))

# Usuwamy zmienn¹ X3, która jest wielokrotnoœci¹ X2 (za³ó¿my X3 = 7 * X2)
(mcolinalg <- simLinReg(nreps = 1, n = 100, k = 2, CorX = CorXalg[-3, -3], beta = 1:3, errorType = "spher_eps"))

(dfcolinalg <- dflm(mcolinalg))
dfcolinalg <- cbind(dfcolinalg, X3 = 7*dfcolinalg[,"X2"])
dfcolinalg[,"y"] <- dfcolinalg[,"y"] + 4 *  dfcolinalg[,"X2"]
dfcolinalg
cor(dfcolinalg)

(lmcolinalg <- lm(y~., data = dfcolinalg))
summary(lmcolinalg)

Xcolalg <- as.matrix(dfcolinalg[,-1])
ycolalg <- as.matrix(dfcolinalg[,1])


(ridgealg <- glmnet(Xcolalg, ycolalg, family = "gaussian", alpha = 0, standardize = TRUE))

cvridgealg <- cv.glmnet(x = Xcolalg, y = ycolalg, alpha = 0, nfolds = 10)

plot(cvridgealg)

cvridgealg$lambda.min
cvridgealg$lambda.1se

(min1se_cvridgealg <- coef(cvridgealg, s = "lambda.1se"))

######################################################################################################################
######################################################################################################################
######################################################################################################################

# Model UMNK ze sk³adnikami losowymi generowanymi przez stacjonarny proces AR(1) z parametrem ar1 = - 0.9

(arepsmod <- simLinReg(nreps = 1, n = 100, k = 5, detcoef = 0.9, beta = 1:6, const = TRUE, seed = 1234, errorType = "autocorr_eps",
                       ar1_eps = -0.9))

(dfareps <- dflm(arepsmod))

(lmareps<- lm(y~., data = dfareps))
(sumlmareps <- summary(lmareps))

par(mfrow = c(2,2))
plot(lmareps)

names(lmareps)
names(sumlmareps)

(armod_res <- lmareps$residuals)

par(mfrow = c(1,1))
plot.ts(armod_res, main = "Residuals time series plot", xlab = "t", ylab = "eps")
acf(armod_res, plot = T)
pacf(armod_res, plot = T)

# Test autokorelacji dla autokorelacji reszt rzêdzu 1 (AR(1))
lmtest::dwtest(lmareps, alternative = "two.sided")
lmtest::dwtest(lmareps, alternative = "less")


# Test Breuscha-Godfreya dla autokorelacji reszt wy¿szego rzêdu (AR(p)) 
# (statystyka testowa przy prawdziwoœci H0 ma rozk³ad asymptotyczny chi-kwadrat, w wersji dla próby skoñczonej stosuje siê rozk³ad F)
lmtest::bgtest(formula = lmareps, order = 1, type = "F")

library(sandwich)
vcov(lmareps)
(class_cov_betahat <- vcovHC(lmareps, type = "const"))
# Estymator macierzy kowariancji Neweya-Westa (HAC - Heteroskedasticity and Autocorrelation Consistent)
# zgodny w przypadku autokorelacji reszt
(hac_cov_betahat <- NeweyWest(lmareps, lag = 1))
library(lmtest)
coeftest(lmareps, vcov = class_cov_betahat )
coeftest(lmareps, vcov = hac_cov_betahat )

diag(class_cov_betahat)^0.5
diag(hac_cov_betahat)^0.5


(ar_eps_est_aic <- ar(armod_res, aic = TRUE, order.max = NULL, method = "ols", demean = FALSE))
names(ar_eps_est_aic)
(ar_eps_est <- ar(armod_res, order.max = 1, method = "ols", demean = FALSE))
ar_eps_est$order
(ro <- ar_eps_est$ar[[1]])
ar_eps_est$var.pred
ar_eps_est$resid # realizacje eta

n <- nrow(dfareps)
(ropow <- c(1, ro^(1:(n-1))))

(Omega <- sapply(0:(n-1), function(i) c(rev(ropow[-1]), ropow)[(n - i):(2*n - 1 - i)]))

ro_theor <- -0.9
(ropow_theor <- c(1, ro_theor^(1:(n-1))))
(Omega_theor <- sapply(0:(n-1), function(i) c(rev(ropow_theor[-1]), ropow_theor)[(n - i):(2*n - 1 - i)]))

# Omegainv <- matrix(0, n, n)
# vals <- c(-ro, 1+ ro^2, -ro,)
# for(i in 2:(n-1)) Omegainv[()]


# UMNRL - Model regresji ze skorelowanymi b³êdami (ar1)
library(MASS)

# estymacja UMNK
(gls_ar <- lm.gls(y~., data = dfareps, W = Omega_theor, inverse = TRUE))
names(gls_ar)
gls_ar$coefficients

(se2_gls <- var(gls_ar$residuals))
Omegainv_theor <- solve(Omega_theor)
Xw1 <- arepsmod$X
(gls_cov_beta <- se2_gls * solve(t(Xw1)%*% Omegainv_theor %*% Xw1))
coeftest(gls_ar, vcov = fgls_cov_beta)
confint(gls_ar, level = 0.95) #??


# estymacja EUMNK
(fgls_ar <- lm.gls(y~., data = dfareps, W = Omega, inverse = TRUE))
names(fgls_ar)
fgls_ar$coefficients

(se2_fgls <- var(fgls_ar$residuals))
Omegainv <- solve(Omega)
Xw1 <- arepsmod$X
# Macierz kowariancji estymatora EUMNK: cov(hatbeta) = se_(hat Omega)^2 * (X' (hat Omega)^{-1} X)^{-1}
(fgls_cov_beta <- se2_fgls * solve(t(Xw1)%*% Omegainv %*% Xw1))
coeftest(fgls_ar, vcov = fgls_cov_beta)
confint(fgls_ar, level = 0.95) # ?

# Sprawdzenie wyników estymacji EUMNK za pomoc¹ w³asnej procedury: betahat_Omega = (X' (hat Omega)^{-1} X)^{-1} ((X' (hat Omega)^{-1} y)
y <- arepsmod$y
(betaOmega <- solve(t(Xw1) %*% Omegainv %*% Xw1) %*% (t(Xw1) %*% Omegainv %*% y))


library(nlme)
# Restricted Maximum Likelihood (REML)
gls(y ~ ., data = dfareps, correlation=corARMA(p=1))

(ast_data <- data.frame(stats::filter(dfareps, filter = c(1, -ro), method = "convolution", sides = 1)))
colnames(ast_data) <- c("y", paste("X", 1:(ncol(ast_data)-1), sep = ""))

(Pgls <- lm(y~., data =ast_data[-1,]))
summary(Pgls)

######################################################################################################################
######################################################################################################################
######################################################################################################################

# Model UMNRL z heteroskedastycznoœci¹ reszt

# Przypadek grupowej heteroskedastycznoœci reszt
(grouphetmod <- simLinReg(nreps = 1, n = 100, k = 5, detcoef = 0.9, beta = 1:6, const = TRUE, seed = 1234, errorType = "heterosc_eps",
                          group_heter = TRUE, vardisp_heter = 0.5, subsobs_var = 25))

(dfgrouphet <- dflm(grouphetmod))

(lmgrouphet<- lm(y~., data = dfgrouphet))
(sumlmgrouphet<- summary(lmgrouphet))

par(mfrow = c(2,2))
plot(lmgrouphet)

(class_cov_betahat_gh <- vcovHC(lmgrouphet, type = "const"))
# Estymator macierz kowariancji HC (Heteroskedasticity-Consistent Covariance - zgodny w przypadku heteroskedastycznoœci reszt)
# cov(betahat) = se^2 * (X'X)^{-1} X' (hat Omega) X (X'X)^{-1}
(hc_cov_betahat_gh <- vcovHC(lmgrouphet, type = "HC"))

library(lmtest)
coeftest(lmgrouphet, vcov = class_cov_betahat_gh )
coeftest(lmgrouphet, vcov = hc_cov_betahat_gh ) 

(hetgroup <- hetvar(data = dfgrouphet , formula = y~. , lbls = NULL, subsobs = 25))
(W <- hetgroup$W)

# Estymator EUMNK w przypadku grupowej heteroskedastycznoœci reszt
(fgls_grouphet <- lm.gls(y~., data = dfgrouphet, W = W, inverse = TRUE))
names(fgls_grouphet)
fgls_grouphet$coefficients

(se2_gh <- var(fgls_grouphet$residuals))
Winvgh <- solve(W)
(Xw1gh <- grouphetmod$X)
# Macierz kowariancji estymatora EUMNK: cov(hatbeta) = se_(hat W)^2 * (X' (hat W)^{-1} X)^{-1}
(gh_cov_beta <- se2_gh * solve(t(Xw1gh)%*% Winvgh %*% Xw1gh))
coeftest(fgls_grouphet, vcov = gh_cov_beta)


# Przypadek heteroskedastycznoœci reszt zadanej przez funkcjê zmiennych niezale¿nych

(fnchetmod <- simLinReg(nreps = 1, n = 100, k = 5, detcoef = 0.9, beta = 1:6, const = TRUE, seed = 1234, errorType = "heterosc_eps",
                          group_heter = FALSE, p_heter_fnc = 3, k_heter_fnc = NULL))


(dffnchet <- dflm(fnchetmod))

(lmfnchet<- lm(y~., data = dffnchet))
(sumlmfnchet <- summary(lmfnchet))

# Test Breuscha-Pagana heteroskedastycznoœæ zadana przez funkcjê wielomianu trzeciego stopnia zmiennej indeksuj¹cej
t <- 1:nrow(dffnchet)
lmtest::bptest(lmfnchet, varformula = ~ poly(t, 3, raw = TRUE), studentize = TRUE)

(class_cov_betahat_fnch <- vcovHC(lmfnchet, type = "const"))
(hc_cov_betahat_fnch <- vcovHC(lmfnchet, type = "HC"))  # Hetroscedasticity consistent hat beta covariance estimator

library(lmtest)
coeftest(lmfnchet, vcov = class_cov_betahat_fnch)
coeftest(lmfnchet, vcov = hc_cov_betahat_fnch) 

# Wyznaczenie reszt KMNK
fnchetres <- lmfnchet$residuals
fnchetres_sq <- fnchetres^2

par(mfrow = c(2,2))
plot(lmfnchet)

par(mfrow = c(1,1))
plot(fnchetres_sq , main = "Square residuals vs. index", ylab = "Squared residuals")
ind <- 1:length(fnchetres_sq)

# Regresja loess (lokalna regresja wielomianowa ??)
(loc <- loess(fnchetres_sq  ~ ind - 1))
summary(loc)
predloc <- predict(loc) # Krzywa regresji loess
lines(predloc, col = "green")

# Przedzia³ ufnoœci dla regresji
# predict.lm(model,data.frame(x=q),interval='confidence', level=0.99)

(poly3 <- lm(fnchetres_sq ~ poly(ind,3, raw = TRUE))) # Regresja wielomianowa 3 stopnia kwadratów reszt (KMNK) na zmienn¹ indeks
summary(poly3)
(pred3 <- predict.lm(poly3)) # Krzywa regresji 
lines(pred3, col = "blue")    # Nak³adamy krzyw¹ regresji na wykres

(gamma <- poly3$coefficients) # Oszacowanie wektora parametrów gamma w regresji kwadratów reszt na zmienn¹ indeks
(gammaind <- pred3)
Wfnc <- diag(gammaind)

# Estymator EUMNK w przypadku heteroskedastycznoœci reszt zadanej przez funkcjê zmiennych niezale¿nych
(fgls_fnchet <- lm.gls(y~., data = dffnchet, W = Wfnc, inverse = TRUE))
names(fgls_fnchet)
fgls_fnchet$coefficients

(se2_fnch <- var(fgls_fnchet$residuals))
Winvfnch <- solve(Wfnc)
(Xw1fnch <- fnchetmod$X)
# Macierz kowariancji estymatora EUMNK: cov(hatbeta) = se_(hat W)^2 * (X' (hat W)^{-1} X)^{-1}
(fnch_cov_beta <- se2_fnch * solve(t(Xw1fnch)%*% Winvfnch %*% Xw1fnch))
coeftest(fgls_fnchet, vcov = fnch_cov_beta)


