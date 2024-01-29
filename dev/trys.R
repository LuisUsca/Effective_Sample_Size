# rm(list = ls())
library(ggplot2)
library(dplyr)
library(magrittr)
library(openxlsx)
library(tidyr)#spread
library(irr)# corr intra clas icc

setwd("Documentos/Imarpe/Seguimiento")
setwd("F:/Imarpe/Seguimiento")
# Datos -------------------------------------------------------------------
# Lorna.xlsx Cabinza.xlsx Lisa.xlsx Chita.xlsx Lengudo.xlsx Machete.xlsx
# sp <- c("cabinza","lorna","coco","lisa","cachema","machete","pejerrey","chita","pintadilla","cabrilla","lenguado","trambollo")
datos <- read.xlsx(xlsxFile = "BD-Lorna.xlsx", sheet = "Biometrico")
dato <- datos %>% filter(ANIO %in% (2000:2020)) 
dato[, 30] <- round(as.numeric(dato[, 30]))# sapply(dato[, 29], as.numeric)
data_nm <- dato %>% group_by(ANIO, MES, DIA, LABORATORIO, EMBARCACION, ARTE) %>% summarise(n=sum(FREC_ABSOLUTA),m=length(LONGITUD))
# Mod Thonson -------------------------------------------------------------
# Mod Correlaciones -------------------------------------------------------
sp = "CABRILLA"
arte = "CORTINA" #CORTINA Y CERCO
data_nm %>% filter(ARTE==arte) %>% ggplot(aes(x=m,y=n)) + geom_point() # ver mejor relacion n y m
lab <- data_nm %>% filter(ARTE==arte,m%in%c(4:20),n>30)#lorna-cabinz>40, cachem-mache >30, coco-lisa >20, pintad-cabr>15
label <- lab %>% mutate(labs = paste(ANIO,MES,DIA,LABORATORIO,EMBARCACION,ARTE))

data <- dato %>% mutate(etiq = paste(ANIO,MES,DIA,LABORATORIO,EMBARCACION,ARTE)) %>% filter(etiq %in% unique(label$labs)) %>%
  select(ANIO,MES,DIA,LABORATORIO,RECURSO,EMBARCACION,ARTE,LONGITUD,FREC_ABSOLUTA,etiq)
#prueba de normalidad: seleccion de muestras
library(nortest)
dat_n <- NULL
n <- 1
for (i in unique(data$etiq)) {
  p <- data %>% filter(etiq==i)
  pp <- as.numeric(rep(p$LONGITUD,times=p$FREC_ABSOLUTA))
  if (shapiro.test(pp)$p.value >= 0.05){#shapiro<50, lillie>50
    dat_n[n] <- i}
  n = n+1
}
#datos de muestreo dist normal
nn <- na.omit(dat_n) # muestreos con dist normal
length(unique(nn))
normal <- unique((label %>% filter(labs %in% unique(nn)) %>% arrange(m))$labs)

write.csv(data_normal,"muestras_normal.csv",row.names = F)
### datos de validacion
# datos rango fijo
# pn <- c(60,70,80,90,100,120,150,200,250,300)
# pi <- c(0.1,0.2,0.3,0.3,0.4,0.5,0.4,0.5,0.2,0.1)
# dd <- NULL
# for (s in pn) {
#   d = rmultinom(1, size = s, prob = pi)
#   z = data.frame(seq(14,23),d,rep(s,length(d)))
#   colnames(z) <- c("LONGITUD","FREC_ABSOLUTA","N")
#   dd = bind_rows(dd,z)
# }
# dd%>%ggplot(aes(x=LONGITUD,y=FREC_ABSOLUTA)) + geom_col() + facet_grid(N~.)

##--------------------------comienza el bucle
#instrucion: activar bucle for para cada --> ### y ###

#datos normales con rangos
set.seed(1235)
# mean <- mean(rep(data$LONGITUD,times=data$FREC_ABSOLUTA))
datta <- NULL
q <- 0
#------------------------------------------------------------------simulacion
### datos validados
# vector <- unique(pros$EMBARCACION)
# for (s in vector) {
#   y <- pros %>% filter(EMBARCACION ==s)
### variando rangos
# r <- seq(1,3.6,by=.27)
# for (s in r) {
# b <- rnorm(300,mean,s)
# y <- data.frame(table(round(b)))
# colnames(y) <- c("LONGITUD","FREC_ABSOLUTA")
### variando tamaño
# for (s in pn) {
#   y <- dd %>% filter(N == s)
### variando modas
# m <- c(60,70,80,90,100,120,150,200,250,300)
# for (s in m) {
# bin <- rbinom(s, 1, 0.6)
# x <- rnorm(s, mean = 18, sd = 1) * bin + rnorm(s, mean = 21, sd = 1) * (1 - bin)
# y <- data.frame(table(round(x,0)))
# colnames(y) <- c("LONGITUD","FREC_ABSOLUTA")
###
#------------------------------------------------------------------estimacion
### muestras normales
h <- c(1,2,3,4,5,6,7,8,9,10) # cambiar id muestra cuando genera --Inf--
for (s in h) {
    y <- data %>% filter(etiq==normal[s])
    pro <- y %>% mutate(pro=FREC_ABSOLUTA/sum(FREC_ABSOLUTA))
    pro$fp <- round(pro$pro*1000,0)
    N_i <- rep(y$LONGITUD, times=y$FREC_ABSOLUTA) # poblacion
    # correlacion
    n_i <- seq(1,100,2)
    cor_1 <- vector(mode = "numeric")
    cor_1sd <- vector(mode = "numeric")
    p <- 1
    for (i in n_i) {
      cors_1 <- vector(mode = "numeric")
      for (j in 1:500) {
        x <- sample(N_i,size=i,replace = T) 
        dat <- data.frame(table(x)) # ordena
        pro_art <- data.frame(lon = pro$LONGITUD, fr=NA) 
        id_match <- match(pro_art$lon, dat$x)
        pro_art$fr <- dat$Freq[id_match]
        pro_art$fr_rel <- pro_art$fr/i
        id_na <- !is.na(pro_art$fr_rel)
        cors_1[j] <- cor(pro_art$fr_rel[id_na], pro$pro[id_na])
      }
    cor_1[p] <-   c(mean(cors_1, na.rm = T))
    cor_1sd[p] <- c(sd(cors_1, na.rm = T))
    p = p + 1
    }
dat1 <- data.frame(n_i, cor_1, cor_1sd, MC = rep(length(pro$LONGITUD),times=length(n_i)),
                   M=rep(sum(pro$FREC_ABSOLUTA),times=length(n_i)), arte=rep(arte,times=length(n_i)), sp=rep(sp, times=length(n_i)))
datta <- bind_rows(datta,dat1)
q = q+1
}

yy <- read.csv("Muestra/correlaciones_total.csv")
# yy <- read.csv("Muestra/correlaciones_sp.csv")

# colfunc <- colorRampPalette(c("yellow","yellow2","coral1","coral2","red4"))
colfunc <- colorRampPalette(c("green","yellow","red"))
  
p <- yy %>% group_by(n_i, sp, arte) %>% summarise(cors = round(mean(cor_1, na.rm = T),1)) %>% filter(!is.na(cors))%>% ggplot(aes(x=n_i, y=sp, height=1, width=5)) +
  geom_tile(colour="black", size=0.25, aes(fill=factor(cors))) + #scale_fill_viridis(direction=-1,discrete = TRUE,name="Nivel cor",option = "C")+
  scale_fill_manual(values = colfunc(10),name= expression(Cor*"  "*rho))+
  facet_grid(arte~.)+ scale_x_continuous(breaks = seq(from = 0, to = 100, by = 5)) + labs(x="Tamaño de muestra estimado",y="") + theme_bw()

ggsave("Muestra/latex/correlaciones_sps.pdf",p, height = 8, width = 10 , dpi = 320)
  
# graficando 
# library(paletteer)#scale_color_paletteer_d
# library(viridisLite)   # scale_fill_paletteer_d("nbapalettes::jazz_city",-1) +
library(viridis)#scale_fill_viridis_d
yy %>% mutate(m=cor_1-cor_1sd)%>% ggplot(aes(x=n_i,y=cor_1,color=intv)) + 
  geom_line(size=1) + geom_point(shape=21)+
  scale_x_continuous(breaks = seq(from = 0, to = 300, by = 20)) +
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.1)) +
  geom_errorbar(aes(ymin = cor_1-cor_1sd, ymax = cor_1+cor_1sd),width=4,color="blue4") +
  geom_hline(yintercept = 0.90, col = "darkred", lty = 4, lwd = .8) +
  geom_hline(yintercept = 0.75, col = "blue4", lty = 4, lwd = .8) +
  scale_color_viridis_d(direction = -1) +
  theme(legend.position = c(0.925, 0.415), legend.background = element_rect(fill = "white", color = "black")) +
  labs(x="Tamano de muestra", y="Correlacion") + guides(color=guide_legend(title="Rango - N")) 

ggsave("Muestra/latex/cabrilla_cortina_corr.pdf",p, height = 5, width = 10 , dpi = 320)


# valores tamaño minimo
tabla <- NULL
for (i in unique(datta$intv)) {
  xy = datta %>% filter(intv==i)
  val_9 = min((xy %>% filter(cor_1 >= 0.90))$n_i)
  val_7 = min((xy %>% filter(cor_1 >= 0.75))$n_i)
  dd = data.frame(n_9 = val_9, n_7 = val_7, muestra = i)
  tabla = bind_rows(tabla,dd)
}
datta <- datta %>% filter(!intv %in% c("9-6-21")) # filtrar si hay error y volver al bucle
# grafica tamaño minimo al 90 y 75% de correlacion segun muestra
p <- tabla %>% ggplot(aes(x=muestra)) + geom_col(aes(y=n_9),fill="cyan4",alpha=0.5) +  geom_col(aes(y=n_7),fill="tomato3") + 
  geom_hline(yintercept =mean(tabla$n_9),col = "darkred",lty = 4,lwd= .8) + geom_hline(yintercept =mean(tabla$n_7),col = "darkblue",lty = 4,lwd= .8)+
  scale_y_continuous(breaks = seq(from = 0, to = max(tabla$n_9), by = 10)) + labs(x="Muestreo",y="Tamaño minimo muestra")+
  annotate("text", x = 1, y = c(mean(tabla$n_9)+4,mean(tabla$n_7)+4), label = c(paste0("90%: ",round(mean(tabla$n_9))),paste0("75%: ",round(mean(tabla$n_7))))) +
  theme_bw()

ggsave("Muestra/latex/cabrilla_cortina_corr_est.pdf",p, height = 5, width = 10 , dpi = 320)


# Mod Multinomial ---------------------------------------------------------
arte = "CORTINA" #CORTINA Y CERCO
lugar = "CALLAO"
anio = unique(dato$ANIO)

data_nm %>% filter(ARTE==arte, LABORATORIO==lugar, ANIO %in% anio) %>% ggplot(aes(x=m,y=n)) + geom_point() + 
  scale_y_continuous(breaks = seq(from = 10, to = 400, by = 20)) + labs(x="Marcas de clase",y="Tamaño de muestreo")+
  scale_x_continuous(breaks = seq(from = 2, to = 40, by = 2))
lab <- data_nm %>% filter(ARTE==arte, m %in% c(5:20),n > 40)# filtrar segun rango y tamaño
label <- lab %>% mutate(labs = paste(ANIO,MES,DIA,LABORATORIO,EMBARCACION,ARTE))

# df <- dato %>% mutate(etiq = paste(ANIO,MES,DIA,LABORATORIO,EMBARCACION,ARTE), FPOND = round(PESO_CAPTURA*FREC_ABSOLUTA/PESO_MUESTRA)) %>% 
#   filter(etiq %in% unique(label$labs),ARTE==arte, LABORATORIO==lugar)
df <- dato %>% mutate(etiq = paste(ANIO,MES,DIA,LABORATORIO,EMBARCACION,ARTE)) %>% filter(etiq %in% unique(label$labs),ARTE==arte, LABORATORIO==lugar)

#####---PRIMER METODO
datas <- NULL

for (i in anio) {
  data <- df %>% filter(ANIO == i) %>% select(ANIO,MES,DIA,EMBARCACION,LONGITUD,FREC_ABSOLUTA)
  #data proporcion observada
  p_obs <- data %>% group_by(LONGITUD) %>% summarise(FREC_1 = sum(FREC_ABSOLUTA)) %>% mutate(PRO_1=FREC_1/sum(FREC_1))
  rango <- p_obs$LONGITUD
  N_i <- sum(p_obs$FREC_1)
  lmi <- min(p_obs$LONGITUD)
  lma <- max(p_obs$LONGITUD)
  mean <- round(mean(rep(p_obs$LONGITUD, times=p_obs$FREC_1)))
  sd <- sd(rep(p_obs$LONGITUD, times=p_obs$FREC_1))
  #data proporcion estimada
  pi <- dnorm(rango, mean = mean, sd = sd)
  fre <- rmultinom(1, size = N_i, prob = pi)
  p_est <- data.frame(LONGITUD_2 = rango, FREC_2=fre, PRO_2 = fre/sum(fre))
  # p_est %>% ggplot(aes(x=LONGITUD_2, y=PRO_2)) + geom_col() # solo para ver
  #estimacion tamaño primer metodo
  dat <- bind_cols(p_obs, p_est)
  dt <- data.frame(dat) %>% mutate(x = PRO_2*(1-PRO_2), y = abs(PRO_1 - PRO_2))
  N_mul <- sum(dt$x)/sum(dt$y)
  dd <- data.frame(anio=i, rango=paste(lmi,"_",lma), marca = length(rango),N_i = N_i, mean=mean,sd=sd,N_est=N_mul)
  datas = bind_rows(datas,dd)
}

datas %>% ggplot(aes(x=anio,y=N_est, col=rango)) + geom_point() +   #geom_text(aes(label = round(sd,1)), vjust = -2)+
  # facet_grid(anio ~ .)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#####---SEGUNDO METODO
etiqueta <- unique(df$etiq)
grupo <- NULL
p <- 1
for (i in etiqueta) {
  t <- df %>% filter(etiq==i)
  grupo <- c(grupo, paste(rep("v",length(t$ANIO)),p,sep=""))
  p = p + 1
  }
df$grupo <- grupo

tabla <- NULL
anio = unique(df$ANIO)

for (h in anio) {
dat2 <- df %>% filter(ANIO==h) %>% select(ANIO, MES, DIA,EMBARCACION, LONGITUD, FREC_ABSOLUTA, grupo)
M <- sum(dat2$FREC_ABSOLUTA) #total captura en el año y
m <- sum(dat2$FREC_ABSOLUTA) #total muestras en el año y
etiqueta <- unique(dat2$grupo)
n <-length(etiqueta)
dar <- NULL
for (i in etiqueta) {
  Mi <- (dat2 %>% filter(grupo == i) %>% summarise(n=sum(FREC_ABSOLUTA)))$n #numero peces en la lance t
  ddd <- dat2 %>% filter(grupo == i) %>% select(LONGITUD, FREC_ABSOLUTA)
  ui <- round(mean(rep(ddd$LONGITUD,times=ddd$FREC_ABSOLUTA))) # talla media en la muestra t
  datt <- data.frame(x=Mi*ui, Mi=Mi,lab=i)
  dar = bind_rows(dar,datt)
}
ry <- sum(dar$x)/M
M_h <- sum(dar$Mi)/n

dar3 <- NULL
for (i in etiqueta) {
  Mi <- (dat2 %>% filter(grupo == i) %>% summarise(n=sum(FREC_ABSOLUTA)))$n
  mi <- (dat2 %>% filter(grupo == i) %>% summarise(n=sum(FREC_ABSOLUTA)))$n 
  dt <- dat2 %>% filter(grupo == i) %>% mutate(x = (Mi/mi)*(LONGITUD-ry)^2, y = FREC_ABSOLUTA * (mean(LONGITUD) - ry)^2) %>%
    summarise(s1=sum(x),s2=sum(y))
  dar3 <- bind_rows(dar3,dt)
}
sig_2 <- sum(dar3$s1)/(M-1)
sig_2s <- sum(dar3$s2)/(M-1)#para frecuencias
#calculo de var(R) para muestra igual que la captura
long <- dat2 %>% select(LONGITUD,FREC_ABSOLUTA,grupo)
zz <- spread(data = long, key=grupo, value=FREC_ABSOLUTA)
zz[is.na(zz)] <- 0
hh <- icc(zz, model = "twoway", type = "agreement", unit = "single")
cor_h <- hh$value
M_hat <- mean((long %>% group_by(grupo) %>% summarise(nm = mean(FREC_ABSOLUTA)))$nm)
sig_m <- mean((long %>% group_by(grupo) %>% summarise(var = var(FREC_ABSOLUTA)))$var)
M_cor <- sum(long$FREC_ABSOLUTA)
sig_x <- var(rep(long$LONGITUD, times=long$FREC_ABSOLUTA))
varR <- sig_x*(1+(M_hat - 1 + sig_m/M_hat)*cor_h)/M_cor
#estimacion
sny1 <- sig_2/vry
sny2 <- sig_2s/vry
sny3 <- sig_2s/varR
tab <- data.frame(ry=ry,varR=varR, vry=vry, sig_2 = sig_2, sig_2s=sig_2s,sny1=sny1,sny2=sny2,sny3=sny3,corr=cor_h, ya=h)
tabla <- bind_rows(tabla,tab) 
}
tabla %>% ggplot() + geom_point(aes(x=sny2,y=sig_2))


#se selecciona var(R) y sig_2 para calcular deff y estimar meff en:
tabla1 <- NULL
for (h in anio) {
  dat2 <- df %>% filter(ANIO==h) %>% select(ANIO, MES, DIA,EMBARCACION, LONGITUD, FREC_ABSOLUTA,grupo)
  etiqueta <- unique(dat2$grupo)
  sig <- (tabla %>% filter(ya==h))$sig_2
  R <-  (tabla %>% filter(ya==h))$vry
  m <- sum(dat2$FREC_ABSOLUTA)
  n_i <- c(80,90,100,120,150,200)
  d_s <- NULL
  N_i <- rep(dat2$LONGITUD,times=dat2$FREC_ABSOLUTA)
  for (s in n_i) {
    d_p <- NULL
    for (p in 1:1000) {
      x <- sample(N_i, size=s, replace = T)
      d_p <- c(d_p, round(mean(x)))
    }
    var = var(d_p)
    deff = R/var
    ds <- data.frame(R_h = var, deff = deff, meff = m/deff, n_i = s, ya=h)
    d_s <- bind_rows(d_s, ds)
  }
  tabla1 <- bind_rows(tabla1, d_s)
}
tabla1 %>% ggplot(aes(x=ya, y= meff, colour= as.character(n_i))) + geom_point() + geom_line() + 
  scale_x_continuous(breaks = seq(from = 2000, to = 2021, by = 1)) +
  # scale_y_continuous(breaks = seq(from = 10, to = 300, by = 20)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#####---TERCER METODO
#funcion de estimacion de tamaño
D_M <- function(theta){
  -(log(gamma(theta)) - sum(log(gamma(dat$p_s * theta))) + sum((dat$p_s * theta - 1) * log(dat$p_o)))
}
#funcion de estimacion de modas
modas <- function(x){
  var <- gamlssMX(formula = x ~ 1,
                  data    = data.frame(x),
                  family  = RG, #RG, NO, GA
                  K       = 2,
                  control = MX.control(plot = FALSE))
  list(p_v = var$prob, mu = c(var$models[[1]]$mu.coefficients, var$models[[2]]$mu.coefficients), dv = var$G.deviance)
}

# sd_b <- NULL
# for (v in unique(df$grupo)) {
#   df_sd <- df %>% filter(grupo == v) %>% group_by(LONGITUD) %>% summarise(FREC=sum(FREC_ABSOLUTA))
#   x_d <- c((rep(df_sd$LONGITUD, times = df_sd$FREC)))
#   sd_b <- c(sd_b, sd(x_d))
#  }
# xd <- boxplot(sd_b, col="skyblue", frame.plot=F)
# sd_q <- mean(sd_b[!(sd_b %in% xd$out)])
# sd_q <- diff(quantile(sd_b,probs = c(0.1, 0.9)))
# sd_q <- quantile(sd_b, probs = 0.25)
# sd_q <- mean(sd_b)
# summary(sd_b)
library(multimode)
library(bbmle)
library(gamlss.mx)
set.seed(1234)
tabla2 <- NULL
anio = unique(df$ANIO)
for (h in anio) {
  dat3 <- df %>% filter(ANIO == h) %>% dplyr::select(ANIO, MES, DIA,EMBARCACION, LONGITUD, FREC_ABSOLUTA,grupo)
  h_i <- NULL
  for (i in unique(dat3$grupo)) {
    set.seed(1234)
    dt <- dat3 %>% filter(grupo == i) %>% group_by(LONGITUD) %>% summarise(FREC=sum(FREC_ABSOLUTA)) %>% mutate(p_o=FREC/sum(FREC))
    rango <- dt$LONGITUD
    mc_o <- length(rango)
    N_i <- sum(dt$FREC)
    x <- c((rep(dt$LONGITUD, times = dt$FREC)))
    vv <- locmodes(x, mod0 = 2, display = TRUE)
    # dd <- modetest(x, mod0=1,method="HY",B=500,submethod=NULL,n=NULL,tol=NULL)$p.value#mejor resultado
    skip_to_next <- FALSE
    tryCatch(dd <- modas(x), error = function(e) { skip_to_next <<- TRUE})
      if(skip_to_next) { next }     
    mean <- mean(x)
    sd <- sd(x)
    print(i)
    # if ((sum(vv$fvalue[c(1,3)]) > 0.5)==T) {#aumentar en cortina 0.01
    if (sort(dd$p_v)[1] <= 0.1) {#si es unimodal
      rag <- seq(min(rango), max(rango))
      mc_s <- length(rag)
      mat <- data.frame(TALLA = rag)
      mt <- merge(mat, dt, by.x = "TALLA", by.y = "LONGITUD", all.x = TRUE)
      mt[is.na(mt)] <- 0
      p_i <- dnorm(rag, mean = mean, sd = sd)
      fre <- c(rmultinom(1, size = N_i, prob = p_i))
      mts <- mt %>% mutate(FRE_2= fre, p_s= fre/sum(fre))
      mts[c("p_o","p_s")][mts[c("p_o","p_s")] == 0] <- 1.0e-8
      dat <- mts %>% mutate(phi = sqrt(N_i*p_o*(1-p_o)))
      skip_to_next <- FALSE
      tryCatch(val <- nlm(D_M, 1)$estimate, error = function(e) { skip_to_next <<- TRUE})
      if(skip_to_next) { next } 
      # val <- mle2(minuslogl = D_M, start = list(theta=1))@coef
      tt <- data.frame(ya = h, Dis = "Normal", sd = sd, Mi = i, N_est = val, Devianza = dd$dv, Ratio_mcni = mc_o/N_i, Pp_cv = sum(dat$phi)/N_i, mc = mc_o)
      h_i <- bind_rows(h_i, tt)
    } else {
      rag <- seq(min(rango), max(rango))
      mc_s <- length(rag)
      mat <- data.frame(TALLA = rag)
      mt <- merge(mat, dt, by.x = "TALLA", by.y = "LONGITUD", all.x = TRUE)
      mt[is.na(mt)] <- 0
      # p_i <- dnorm(rag, mean = mean, sd = sd/2)
      p_i <- dnorm(rag, mean = mean, sd = 1)#round(sd_q)
      fre <- c(rmultinom(1, size = N_i, prob = p_i))
      mts <- mt %>% mutate(FRE_2= fre, p_s= fre/sum(fre))
      mts[c("p_o","p_s")][mts[c("p_o","p_s")] == 0] <- 1.0e-8
      dat <- mts %>% mutate(phi = sqrt(N_i*p_o*(1-p_o)))
      skip_to_next <- FALSE
      tryCatch(val <- nlm(D_M, 1)$estimate, error = function(e) { skip_to_next <<- TRUE})
      if(skip_to_next) { next } 
      tt <- data.frame(ya = h, Dis = "Binom", sd = sd, Mi = i, N_est = val, Devianza = dd$dv, Ratio_mcni = mc_o/N_i, Pp_cv = sum(dat$phi)/N_i, mc = mc_o)
      h_i <- bind_rows(h_i, tt)
    }
  }
  tabla2 <- bind_rows(tabla2, h_i)
}
# write.csv(tabla2,"cabrilla_cortina3.csv",row.names = F)#sd2 1.5 1
tabla2 <- read.csv("Muestra/cabrilla_cortina3.csv")

library(GGally)
# ggpairs(tabla2[,c(2,4,5,6,7,8)])
p <- ggpairs(tabla2, columns = c(3,5,6,7,8,9), aes(color = Dis)) + scale_color_manual(values = c("gold3","gray1")) + scale_fill_manual(values = c("gold3","gray50"))

ggsave("Muestra/latex/cabrilla_cortina_cor.pdf",p, height = 5, width = 10 , dpi = 320)

p <- tabla2 %>% ggplot(aes(x=N_est,y=Devianza,label= round(Pp_cv,2))) + geom_point() + 
  geom_text(size = 3, position =position_dodge(1),vjust=-.5)+labs(x= "Tamaño de muestra estimado", y=" Devianza del modelo")

ggsave("Muestra/latex/cabrilla_cortina_dev.pdf",p, height = 5, width = 8 , dpi = 320)

p <- ggplot(tabla2, aes(x= Pp_cv, y= N_est)) + geom_point() + stat_smooth() + labs(x="Precision", y="Tamaño muestra estimado")

ggsave("Muestra/latex/cabrilla_cortina_ppcv.pdf",p, height = 5, width = 10 , dpi = 320)

m1 <- nls(N_est ~ a*Pp_cv^b, data=tabla2, start=list(a=1,b=0))

# lim <- diff(quantile(tabla2$Pp_cv, probs = c(0.1, 0.9)))
yd <- boxplot(tabla2$Pp_cv, col="skyblue", frame.plot=F)

lim <- quantile(tabla2$Pp_cv[!(tabla2$Pp_cv %in% yd$out)], probs = 0.1)

p <- tabla2 %>% ggplot() + geom_point(aes(x=Pp_cv, y=N_est), col="darkblue")+geom_line(aes(x=Pp_cv, y= predict(m1)), col="darkred") + 
  scale_y_continuous(breaks = seq(from = 0, to = 200, by = 10)) + geom_vline(xintercept = lim,lty=4,col="gray10",lwd=0.8) +
  labs(x="Precision", y="Tamaño muestra estimado")
ggsave("Muestra/latex/cabrilla_est_cortina.pdf",p, height = 5, width = 10 , dpi = 320)
# tabla2 %>% group_by(ya)%>%summarise(n=mean(N_est), sd = mean(sd))%>% ggplot(aes(x=ya,y=n, label=round(sd,2))) + geom_point(size=2, col="red") +
#   # geom_errorbar(aes(ymin = n - sd, ymax = n + sd),width = 0.4,color="blue4") +
#   geom_text(size = 3, position =position_dodge(1),vjust=-.5)+
#   scale_x_continuous(breaks = seq(from = 2000, to = 2021, by = 1)) +
#   scale_y_continuous(breaks = seq(from = 10, to = 100, by = 10)) +
#   labs(title = arte,x="", y="Tamaño muestra estimado") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# validacion
datos <- read.xlsx(xlsxFile = "validacion.xlsx")
dt <- datos %>% filter(Sexo==3) %>%mutate(etiq = paste0(Anio,Mes,Dia,Laboratorio,Recurso,Embarcacion,ZonaDePesca,Arte))
dt %>% group_by(etiq) %>% summarise(n=sum(Fr_abs),l=min(Longitud),L=max(Longitud))

etiqueta <- unique(dt$etiq)
grupo <- NULL
p <- 1
for (i in etiqueta) {
  t <- dt %>% filter(etiq==i)
  grupo <- c(grupo, paste(rep("v",length(t$Anio)),p,sep=""))
  p = p + 1
}
dt$grupo <- grupo

#prueba de normalidad: seleccion de muestras
library(nortest)
dat_n <- NULL
n <- 1
for (i in unique(dt$grupo)) {
  p <- dt %>% filter(grupo==i)
  pp <- as.numeric(rep(p$Longitud,times=p$Fr_abs))
  if (lillie.test(pp)$p.value >= 0.05){#shapiro<50, lillie>50
    dat_n[n] <- i}
  n = n+1
}
#datos de muestreo dist normal
nn <- na.omit(dat_n) # muestreos con dist normal
length(unique(nn))
normal <- unique((label %>% filter(labs %in% unique(nn)) %>% arrange(m))$labs)

set.seed(1235)
datta <- NULL
#------------------------------------------------------------------estimacion
### muestras normales
for (s in unique(dt$grupo)) {
  pro <- dt %>% filter(grupo==s) %>% mutate(pro=Fr_abs/sum(Fr_abs))
  pro$fp <- round(pro$pro*1000,0)
  N_i <- rep(pro$Longitud, times=pro$Fr_abs) # poblacion
  # correlacion
  n_i <- seq(1,150,5)
  cor_1 <- vector(mode = "numeric")
  cor_1sd <- vector(mode = "numeric")
  p <- 1
  for (i in n_i) {
    cors_1 <- vector(mode = "numeric")
    for (j in 1:500) {
      x <- sample(N_i,size=i,replace = T) 
      dat <- data.frame(table(x)) # ordena
      pro_art <- data.frame(lon = pro$Longitud, fr=NA) 
      id_match <- match(pro_art$lon, dat$x)
      pro_art$fr <- dat$Freq[id_match]
      pro_art$fr_rel <- pro_art$fr/i
      id_na <- !is.na(pro_art$fr_rel)
      cors_1[j] <- cor(pro_art$fr_rel[id_na], pro$pro[id_na])
    }
    cor_1[p] <-   c(mean(cors_1, na.rm = T))
    cor_1sd[p] <- c(sd(cors_1, na.rm = T))
    p = p + 1
  }
  dat1 <- data.frame(n_i, cor_1, cor_1sd, MC = rep(length(pro$LONGITUD),times=length(n_i)),intv=rep(unique(pro$grupo),times=length(n_i)),
                     M=rep(sum(pro$Fr_abs),times=length(n_i)), arte=rep(unique(pro$Arte),times=length(n_i)), sp=rep(unique(pro$Recurso), times=length(n_i)))
  datta <- bind_rows(datta,dat1)
}

colfunc <- colorRampPalette(c("yellow","yellow2","coral1","coral2","red4"))

p <- datta %>% group_by(n_i, sp, arte) %>% summarise(cors = round(mean(cor_1, na.rm = T),1)) %>% filter(!is.na(cors),n_i<100)%>% ggplot(aes(x=n_i, y=sp, height=1, width=5)) +
  geom_tile(colour="black", size=0.25, aes(fill=factor(cors))) + #scale_fill_viridis(direction=-1,discrete = TRUE,name="Nivel cor",option = "C")+
  scale_fill_manual(values = colfunc(10),name=expression(Cor*"  "*rho))+ facet_grid(arte~.)+ scale_x_continuous(breaks = seq(from = 0, to = 150, by = 10)) + labs(x="Tamaño de muestra",y="")#+

ggsave("Muestra/latex/correlaciones_val.pdf",p, height = 8, width = 10 , dpi = 320)



library(viridis)#scale_fill_viridis_d
datta %>% mutate(m=cor_1-cor_1sd)%>% ggplot(aes(x=n_i,y=cor_1,color=arte)) + 
  geom_line(size=0.7) + geom_point(shape=21)+
  scale_x_continuous(breaks = seq(from = 0, to = 300, by = 10)) +
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.1)) +
  geom_errorbar(aes(ymin = cor_1-cor_1sd, ymax = cor_1+cor_1sd),width=2,color="blue4") +
  geom_hline(yintercept = 0.90, col = "darkred", lty = 4, lwd = .5) +
  geom_hline(yintercept = 0.75, col = "blue4", lty = 4, lwd = .5) +
  scale_color_manual(values=c("darkred","blue4")) + facet_grid(sp~.)+
  theme(legend.position = "bottom", legend.background = element_rect(fill = "white", color = "black")) +
  labs(x="Tamano de muestra", y="Correlacion") + guides(color=guide_legend(title="Arte")) 

# ggsave("Muestra/latex/cabrilla_cortina_corr.pdf",p, height = 5, width = 10 , dpi = 320)

#distribucion frecuencia 
#lorna-cabinz>40, cachem-mache >30, coco-lisa >20, pintad-cabr>15
# sp <- c("cabinza","lorna","coco","lisa","cachema","machete","pejerrey","chita","pintadilla","cabrilla","lenguado","trambollo")
sp <- c("Cabinza","Cachema","Machete","Lisa")
sp <- c("Lorna","Coco","Pintadilla","Cabrilla")
sp <- c("Pejerrey")
arte <- c("CORTINA","CERCO")

tabla3 <- NULL
for (i in sp) {
  datos <- read.xlsx(xlsxFile =paste("BD-",i,".xlsx",sep = ""), sheet = "Biometrico")
  datos[, 30] <- round(as.numeric(datos[, 30]))#29 o 30
  dato <- datos %>% filter(ANIO %in% (2018), ARTE %in% arte) %>% mutate(etiq = paste0(ANIO,MES,DIA,LABORATORIO,EMBARCACION,ARTE)) %>% select(RECURSO,ARTE,LONGITUD,FREC_ABSOLUTA,etiq)
  dat <- dato %>% group_by(RECURSO,ARTE,etiq,LONGITUD) %>% summarise(FREC=sum(FREC_ABSOLUTA)) %>%
      mutate(pi=FREC/sum(FREC), phi = sqrt(sum(FREC)*pi*(1-pi)), cv=phi/(sum(FREC)*pi)) 
  dt <- dat %>% group_by(RECURSO,ARTE,LONGITUD) %>% summarise(frec = sum(FREC), pcv=mean(cv,na.rm=T)) %>% mutate(pro = frec/sum(frec,na.rm = T), rel=round(pro*1000))
  tabla3 <- bind_rows(tabla3,dt)
}

p <- tabla3 %>% ggplot(aes(x=LONGITUD, y=pro,label=round(pcv,1))) + geom_col(color="gray60", alpha=0.4) + 
  geom_errorbar(aes(x=LONGITUD, ymin=pro, ymax=pro+pcv/50), width=0.6, colour="darkred", size=.5) + 
  scale_y_continuous(breaks = seq(0, 1, 0.02)) + scale_x_continuous(breaks = seq(5, 50, 2)) +
  labs(x=NULL,y= "Frecuencia en miles") + facet_grid(RECURSO~ARTE, scales = "free") + theme_bw()

ggsave("Muestra/latex/dft_pejerrey.pdf",p, height = 8, width = 10 , dpi = 320)#pelagicos demersales


#ADICIONAL
datos <- read.csv("muestras_normal.csv")
sp <- c("CABINZA","CACHEMA","MACHETE","LISA")
sp <- c("LORNA","COCO","PINTADILLA","CABRILLA")
sp <- c("PEJERREY")
arte <- c("CORTINA","CERCO")

dat <- datos %>% group_by(RECURSO,ARTE,etiq,LONGITUD) %>% summarise(FREC=sum(FREC_ABSOLUTA)) %>%
    mutate(pi = FREC/sum(FREC), phi = sqrt(sum(FREC)*pi*(1-pi)), cv=phi/(sum(FREC)*pi)) 
dt <- dat %>% group_by(RECURSO,ARTE,LONGITUD) %>% summarise(frec = sum(FREC), pcv=mean(cv,na.rm=T)) %>% mutate(pro = frec/sum(frec,na.rm = T), rel=round(pro*1000))


p <- dt %>% ggplot(aes(x=LONGITUD, y=pro,label=round(pcv,1))) + geom_col(color="gray60", alpha=0.4) + 
  geom_errorbar(aes(x=LONGITUD, ymin=pro, ymax=pro+pcv/50), width=0.6, colour="darkred", size=.5) + 
  scale_y_continuous(breaks = seq(0, 1, 0.05)) + scale_x_continuous(breaks = seq(5, 50, 2)) +
  labs(x=NULL,y= "Proporci�n de tallas") + facet_grid(RECURSO~ARTE, scales = "free") + theme_bw()

ggsave("Muestra_normales.pdf",p, height = 8, width = 10 , dpi = 320)#pelagicos demersales


