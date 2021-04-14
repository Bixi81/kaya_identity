library(readr)
library(tidyr)
library(dplyr)
library(mgcv)

# 11. April 2021

# Länder

#Belgien	(BE)		Griechenland	(EL)		Litauen	(LT)		Portugal	(PT)
#Bulgarien	(BG)		Spanien	(ES)		Luxemburg	(LU)		Rumänien	(RO)
#Tschechien	(CZ)		Frankreich	(FR)		Ungarn	(HU)		Slowenien	(SI)
#Dänemark	(DK)		Kroatien	(HR)		Malta	(MT)		Slowakei	(SK)
#Deutschland	(DE)		Italien	(IT)		Niederlände	(NL)		Finnland	(FI)
#Estland	(EE)		Zypern	(CY)		Österreich	(AT)		Schweden	(SE)
#Irland	(IE)		Lettland	(LV)		Polen	(PL)

countries = c("BE","EL","LT","PT","BG","ES","LU","RO","CZ","FR","HU","SI","DK","HR","MT","SK","DE","IT","NL","FI","EE","CY","AT","SE","IE","LV","PL")

##########################################################
### Data

# Population
# DEMO_PJAN / Bevölkerung am 1. Januar nach Alter und Geschlecht
# Annual, Number, Total, Total
# https://ec.europa.eu/eurostat/databrowser/view/DEMO_PJAN__custom_799320/default/table?lang=de
pop = read_delim("C:/Users/User/Downloads/estat_demo_pjan_filtered.tsv.gz", "\t", escape_double = FALSE, na = ":", trim_ws = TRUE)
pop = separate(pop, 1, paste("code", 1:5, sep="_"), sep=",", extra="drop")
colnames(pop)[5] <- "country"
pop = pop %>% dplyr::select(country, paste(as.character(seq(1990,2018))))
pop = dplyr::filter(pop, country %in% countries)
pop = data.frame(cbind(country = pop$country, sapply(pop, function(x) gsub("[^0-9.-]", "", x) ) )[,-2])
pop[, 2:ncol(pop)] <- sapply(pop[, 2:ncol(pop)], as.numeric)

# GDP
# NAMA_10_GDP / B1GQ BIP zu Marktpreisen
# Current prices, million euro, Annual, Gross domestic product at market prices
# https://ec.europa.eu/eurostat/databrowser/view/NAMA_10_GDP__custom_799378/default/table?lang=de
gdp = read_delim("C:/Users/User/Downloads/estat_nama_10_gdp_filtered.tsv.gz", "\t", escape_double = FALSE, na = ":", trim_ws = TRUE)
gdp = separate(gdp, 1, paste("code", 1:4, sep="_"), sep=",", extra="drop")
colnames(gdp)[4] <- "country"
gdp = gdp %>% dplyr::select(country, paste(as.character(seq(1990,2018))))
gdp = dplyr::filter(gdp, country %in% countries)
gdp = data.frame(cbind(country = gdp$country, sapply(gdp, function(x) gsub("[^0-9.-]", "", x) ) )[,-2])
gdp[, 2:ncol(gdp)] <- sapply(gdp[, 2:ncol(gdp)], as.numeric)

# Primärenergieverbrauch
# NRG_IND_EFF  / [PEC2020-2030] Primärenergieverbrauch (Europa 2020-2030)  [MTOE] Millionen Tonnen Rohöleinheiten (TRÖE)
# Annual, Primary energy consumption (Europe 2020-2030), Million tonnes of oil equivalent (TOE)
# https://ec.europa.eu/eurostat/databrowser/view/NRG_IND_EFF__custom_799392/default/table?lang=de
eng = read_delim("C:/Users/User/Downloads/estat_nrg_ind_eff_filtered.tsv.gz", "\t", escape_double = FALSE, na = ":", trim_ws = TRUE)
eng = separate(eng, 1, paste("code", 1:4, sep="_"), sep=",", extra="drop")
colnames(eng)[4] <- "country"
eng = eng %>% dplyr::select(country, paste(as.character(seq(1990,2018))))
eng = dplyr::filter(eng, country %in% countries)
eng = data.frame(cbind(country = eng$country, sapply(eng, function(x) gsub("[^0-9.-]", "", x) ) )[,-2])
eng[, 2:ncol(eng)] <- sapply(eng[, 2:ncol(eng)], as.numeric)

# Treibhausgasemissionen nach Quellsektor (Quelle: EUA)
# ENV_AIR_GGE / Greenhouse gases (CO2, N2O in CO2 equivalent, CH4 in CO2 equivalent, HFC in CO2 equivalent, PFC in CO2 equivalent, SF6 in CO2 equivalent, NF3 in CO2 equivalent), Total (excluding LULUCF and memo items, including international aviation), Annual, Thousand tonnes
# Unit of measure: Thousand tonnes
# https://ec.europa.eu/eurostat/databrowser/view/ENV_AIR_GGE__custom_799405/default/table?lang=de
thg = read_delim("C:/Users/User/Downloads/estat_env_air_gge_filtered.tsv.gz", "\t", escape_double = FALSE, na = ":", trim_ws = TRUE)
thg = separate(thg, 1, paste("code", 1:5, sep="_"), sep=",", extra="drop")
colnames(thg)[5] <- "country"
thg = thg %>% dplyr::select(country, paste(as.character(seq(1990,2018))))
thg = dplyr::filter(thg, country %in% countries)
thg = data.frame(cbind(country = thg$country, sapply(thg, function(x) gsub("[^0-9.-]", "", x) ) )[,-2])
thg[, 2:ncol(thg)] <- sapply(thg[, 2:ncol(thg)], as.numeric)

### Kaya's identity (for Germany)
# F = P * G/P * E/G * F/E
# F = P * g   * e   * f

# F is global CO2 emissions from human sources
# P is global population
# G is world GDP
# E is global energy consumption

##########################################################
### Germany

p = pop[pop[1]=="DE",2:30]
g = gdp[gdp[1]=="DE",2:30] / pop[pop[1]=="DE",2:30]
e = eng[eng[1]=="DE",2:30] / gdp[gdp[1]=="DE",2:30]
f = thg[thg[1]=="DE",2:30] / eng[eng[1]=="DE",2:30] 

# Emissions can be calculated like...
f_cap = p * g * e * f
# Compare with past data
rbind(f_cap,thg[thg[1]=="DE",2:30])

### Plot German data

# GDP / POP
plot(t(g), xaxt = 'n', xlab="Jahr", main="GDP per capita (Germany)")
lines(t(g))
axis(1, at = seq(1, ncol(g)), labels = seq(1990, 2018))

# ENG / GDP
plot(t(e), xaxt = 'n', xlab="Jahr", main="Energy intensity of GDP (Germany)")
lines(t(e))
axis(1, at = seq(1, ncol(e)), labels = seq(1990, 2018))

# THG / ENG
plot(t(f), xaxt = 'n', xlab="Jahr", main="GHG intensity of Energy (Germany)")
lines(t(f))
axis(1, at = seq(1, ncol(f)), labels = seq(1990, 2018))

rm(p,g,e,f,f_cap)

##########################################################
### Aggregate EU-wide figures

eu = data.frame(
      cbind(
        pop = colSums(pop[,-1], na.rm = F),
        gdp = colSums(gdp[,-1], na.rm = F),
        eng = colSums(eng[,-1], na.rm = F),
        thg = colSums(thg[,-1], na.rm = F),
        y = seq(1990,2018)
        ))

p = eu$pop
g = eu$gdp / eu$pop
e = eu$eng / eu$gdp
f = eu$thg / eu$eng
f_cap = p * g * e * f

df = data.frame(cbind(p,g,e,f,y=seq(1990,2018)))

# GDP / POP
plot(g, xaxt = 'n', xlab="Jahr", main="GDP per capita (EU)")
lines(g)
axis(1, at = seq(1, nrow(eu)), labels = seq(1990, 2018))

# ENG / GDP
plot(e, xaxt = 'n', xlab="Jahr", main="Energy intensity of GDP (EU)")
lines(e)
axis(1, at = seq(1, nrow(eu)), labels = seq(1990, 2018))

# THG / ENG
plot(f, xaxt = 'n', xlab="Jahr", main="GHG intensity of Energy (EU)")
lines(f)
axis(1, at = seq(1, nrow(eu)), labels = seq(1990, 2018))

## Models (average future path)
m_p = lm(p~y,data=eu)
plot(eu$p, xaxt = 'n', xlab="Jahr", main="Population (EU) and Prediction")
lines(predict(m_p, newdata=eu), lty=2)
axis(1, at = seq(1, nrow(eu)), labels = seq(1990, 2018))

m_gdp = lm(gdp~y,data=eu)
plot(eu$gdp, xaxt = 'n', xlab="Jahr", main="GDP current prices (EU) and Prediction")
lines(predict(m_gdp, newdata=df), lty=2)
axis(1, at = seq(1, nrow(eu)), labels = seq(1990, 2018))

m_e = lm(e~poly(y,2),data=df[11:29,])
plot(df$e, xaxt = 'n', xlab="Jahr", main="ENG/GDP (EU) and Prediction")
lines(predict(m_e, newdata=df), lty=2)
axis(1, at = seq(1, nrow(eu)), labels = seq(1990, 2018))

m_f = lm(f~poly(y,2),data=df[11:29,])
plot(df$f, xaxt = 'n', xlab="Jahr", main="GHG/ENG (EU) and Prediction")
lines(predict(m_f, newdata=df), lty=2)
axis(1, at = seq(1, nrow(eu)), labels = seq(1990, 2018))

### Predictions
temp = data.frame(y=seq(1990,2050))
pred = data.frame(
        cbind(
          p = predict(m_p, newdata=temp),
          gdp = predict(m_gdp, newdata=temp),
          e = predict(m_e, newdata=temp),
          f = predict(m_f, newdata=temp)
        )
)

pred$y = seq(1990,2050)
pred$lage = lag(pred$e)
pred$lagf = lag(pred$f)

pred$e = ifelse(pred$e > pred$lage, min(pred$e), pred$e)
pred$f = ifelse(pred$f > pred$lagf, min(pred$f), pred$f)

pred = merge(pred,eu %>% dplyr::select(thg,y),by="y", all.x = T)
pred = merge(pred,eu %>% dplyr::select(gdp,y),by="y", all.x = T)
pred = merge(pred,eu %>% dplyr::select(pop,y),by="y", all.x = T)

# Alternative f (Assumtions post 2025)
pred$f2 = pred$f
for (y in seq(2025,2050)){
  pred$f2[pred$y==y] <- pred$f2[pred$y==y-1] * 0.995
}
pred$f2 = ifelse(pred$f2<500, 500, pred$f2)

# Alternative e (Assumtions post 2030)
pred$e2 = pred$e
for (y in seq(2030,2050)){
  pred$e2[pred$y==y] <- pred$e2[pred$y==y-1] * 0.99
}
pred$e2 = ifelse(pred$e2<0.00005, 0.0005, pred$e2)

# Alternative p (Assumtions post 2030)
pred$p2 = pred$p
for (y in seq(2030,2050)){
  pred$p2[pred$y==y] <- pred$p2[pred$y==y-1] * 0.998
}

# GHG projection
pred$ghg_pred = pred$p2 * (pred$gdp.x / pred$p) * pred$e2 * pred$f2
pred$diff1 = round(abs(pred$ghg_pred-pred$thg))
pred$diff2 = round(pred$diff1 / pred$thg,2)

# 1990 Baseline
b1990 = sum(thg[,2], na.rm = T) + 307254

pred$red = round(1 - (pred$thg / b1990),2)
pred$red_pred = round(1 - (pred$ghg_pred / b1990),2)

plot(pred$y, pred$e2) # Plot Assumptions e Energy / GDP
plot(pred$y, pred$f2) # Plot Assumptions f GHG Emissions / Energy
plot(pred$y, pred$p2) # Plot Assumptions p Population

# Plot expected reduction rel to 1990
plot(pred$y, pred$red_pred)
lines(pred$y, pred$red)
