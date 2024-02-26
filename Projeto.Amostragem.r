library(readxl)
require(dplyr)

dados <- read_excel("C:/Users/roger/Dropbox/My PC (LAPTOP-
2L8T1OSK)/Downloads/Dados Biblioteca.xlsx")

somatotal_peso <- c(260685,260206,232882,286239,229303)

somatotal_con <- 104
somatotal_pag <- 922601
somatotal_livros <- 2263
somatotal_peso <- sum(somatotal_peso)
M <- 1202
Nbarra <- somatotal_pag/somatotal_con
Nbarra
################################################################

######## EXERCÍCIO 1 ########
# BIOLOGICAS #
dados_bio <- dados %>% filter(Area == "bio")
attach(dados_bio)
sum_pag_bio <- sum(Paginas)
n_bio <- length(Paginas)
ni_bio <- c(rep(21,21),rep(33,33),rep(27,27),rep(26,26),rep(8,8))
ni_bio

ybarra_bio <- sum_pag_bio/n_bio
ybarra_bio
M <- 1202
m_bio <- 6
aux <- NULL

sr <- function(yi,yb,ni,m){
aux <- sum((yi-(ni*yb))^2)
return(aux/(m-1))
}
sr_bio <- sr(Paginas,ybarra_bio,ni_bio,m_bio)
sr_bio
var_ybarra_bio <- ((M-m_bio)/(M*m_bio*(Nbarra^2)))*sr_bio
var_ybarra_bio
lim_bio <- 1.96*sqrt(var_ybarra_bio)
lim_bio
lim_inf_bio <- ybarra_bio - lim_bio
lim_sup_bio <- ybarra_bio + lim_bio
IC_bio <- c(lim_inf_bio,lim_sup_bio)
IC_bio

# Exatas #
dados_exat <- dados %>% filter(Area == "exat")
attach(dados_exat)
sum_pag_exat <- sum(Paginas)
n_exat <- length(Paginas)
ni_exat <- c(rep(16,16),rep(35,35),rep(5,5),rep(18,18),rep(2,2),rep(28,28))
ni_exat
ybarra_exat <- sum_pag_exat/n_exat
ybarra_exat
M <- 1202

m_exat <- 6
aux <- NULL

sr <- function(yi,yb,ni,m){
aux <- sum((yi-(ni*yb))^2)
return(aux/(m-1))
}
sr_exat <- sr(Paginas,ybarra_exat,ni_exat,m_exat)
sr_exat
var_ybarra_exat <- ((M-m_exat)/(M*m_exat*(Nbarra^2)))*sr_exat
var_ybarra_exat
lim_exat <- 1.96*sqrt(var_ybarra_exat)
lim_exat
lim_inf_exat <- ybarra_exat - lim_exat
lim_sup_exat <- ybarra_exat + lim_exat
IC_exat <- c(lim_inf_exat,lim_sup_exat)
IC_exat

# Humanas #
dados_hum <- dados %>% filter(Area == "hum")
attach(dados_hum)
sum_pag_hum <- sum(Paginas)
n_hum <- length(Paginas)
ni_hum <- c(rep(14,14),rep(24,24),rep(22,22),rep(8,8),rep(23,23),rep(25,25),rep(24,24))
ni_hum
ybarra_hum <- sum_pag_hum/n_hum
ybarra_hum
M <- 1202
m_hum <- 7
aux <- NULL

sr <- function(yi,yb,ni,m){
aux <- sum((yi-(ni*yb))^2)
return(aux/(m-1))
}
sr_hum <- sr(Paginas,ybarra_hum,ni_hum,m_hum)
sr_hum
var_ybarra_hum <- ((M-m_hum)/(M*m_hum*(Nbarra^2)))*sr_hum
var_ybarra_hum
lim_hum <- 1.96*sqrt(var_ybarra_hum)
lim_hum
lim_inf_hum <- ybarra_hum - lim_hum
lim_sup_hum <- ybarra_hum + lim_hum
IC_hum <- c(lim_inf_hum,lim_sup_hum)
IC_hum
################################################################

######## EXERCÍCIO 2 ########

sp <- function(yi,yb,ni,m){
aux <- sum((yi-(ni*yb))^2)
return(aux/(m-1))
}
nbarra <- somatotal_livros/somatotal_con
# BILOGICAS #
attach(dados_bio)
prop_bio <- sum(`Idade binario`)
p_bio <- prop_bio/n_bio
p_bio
sp_bio <- sp(`Idade binario`,p_bio,ni_bio,m_bio)
sp_bio

var_p_bio <- ((M-m_bio)/(M*m_bio*(Nbarra^2)))*sp_bio
var_p_bio
lim_p_bio <- 1.96*sqrt(var_p_bio)
lim_p_bio
lim_inf_bio <- p_bio - lim_p_bio
lim_sup_bio <- p_bio + lim_p_bio
IC_p_bio <- c(lim_inf_bio,lim_sup_bio)
IC_p_bio

# Exatas #
attach(dados_exat)
prop_exat <- sum(`Idade binario`)
p_exat <- prop_exat/n_exat
p_exat
sp_exat <- sp(`Idade binario`,p_exat,ni_exat,m_exat)
sp_exat
var_p_exat <- ((M-m_exat)/(M*m_exat*(Nbarra^2)))*sp_exat
var_p_exat
lim_p_exat <- 1.96*sqrt(var_p_exat)
lim_p_exat
lim_inf_exat <- p_exat - lim_p_exat
lim_sup_exat <- p_exat + lim_p_exat
IC_p_exat <- c(lim_inf_exat,lim_sup_exat)
IC_p_exat
# Humanas #
attach(dados_hum)
prop_hum <- sum(`Idade binario`)
p_hum <- prop_hum/n_hum
p_hum
sp_hum <- sp(`Idade binario`,p_hum,ni_hum,m_hum)
sp_hum
var_p_hum <- ((M-m_hum)/(M*m_hum*(Nbarra^2)))*sp_hum
var_p_hum

lim_p_hum <- 1.96*sqrt(var_p_hum)
lim_p_hum
lim_inf_hum <- p_hum - lim_p_hum
lim_sup_hum <- p_hum + lim_p_hum
IC_p_hum <- c(lim_inf_hum,lim_sup_hum)
IC_p_hum
################################################################

######## EXERCÍCIO 3 ########
num_livros <- c(6,27,18,26,23,27,24,28,8,21,14,24,16,35,27,22,13)

attach(dados)
m4 <- somatotal_con
ybarra_t4 <- somatotal_livros/m4
total_estimado <- M*ybarra_t4
total_estimado
x <- dados$Paginas
y <- dados$Peso
n <- length(dados$Paginas)
ybarra_reg <- sum(dados$Peso)/n
xbarra_reg <- sum(dados$Paginas)/n
Xbarra_reg <- somatotal_pag/somatotal_livros
b_reg <- cov(x,y)/var(x)
Ybarra_reg <- ybarra_reg +b_reg*(Xbarra_reg-xbarra_reg)
Y_reg <- Ybarra_reg*total_estimado
Y_reg # em gramas
ro <- cov(x,y)/(sd(y)*sd(x))
var_ybarra_reg <- (1/n)*(1/(n-2))*var(y)*(1-ro^2)
var_ybarra_reg
var_reg <- (total_estimado^2)*var_ybarra_reg

limite_reg <- 1.96*sqrt(var_reg)
limite_reg
lim_inferior_reg <- Y_reg - limite_reg
lim_superior_reg <- Y_reg + limite_reg
IC_reg <- c(lim_inferior_reg,lim_superior_reg)
IC_reg

################################################################
######## EXERCÍCIO 4 ########
attach(dados)
total_estimado <- M*ybarra_t4
total_estimado
st <- function(yi,yt,m){
aux <- sum((yi-yt)^2)
aux
return(aux/(m-1))
}
st_total <- st(num_livros,ybarra_t4,17)
st_total
var_total <- ((M-m4)/(M*m4))*st_total*(M^2)
var_total
limite <- 1.96*sqrt(var_total)
limite
lim_inferior <- total_estimado- limite
lim_superior <- total_estimado + limite
IC <- c(lim_inferior,lim_superior)
IC