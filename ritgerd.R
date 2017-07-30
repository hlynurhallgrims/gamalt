options("scipen" = 15, "digits" = 4) #Stillingar fyrir staðalform

# Keyra inn gögn fyrir VLF, Innflutning og Útflutning
# Keyra inn gögn fyrir Fjárútlát (Expenditure)
# Gögn þessi eru fengin úr
# World Bank World Development Indicators gagnagrunninum
# og af heimasíðu Hagstofu Íslands
wdi <- read.csv2("worldbank.csv"); #VLF, Innflutningur og Útflutningur
exp <- read.csv2("wdi_exp.csv"); #Expenditure

#Keyra inn upplýsingar um fjárlagaafgang/-halla.
#Gögn af heimasíðu Hagstofu Íslands
surplus <- read.csv2("sur_def.csv"); surplus_prev <- rep(NA, 52);
#Setja inn surplus_t-1 fyrir ár t
sur <- cbind(surplus, surplus_prev); sur[2:52,3] <- sur[1:51,2];

############
# Kafli 4 #
###########
## 4.2 ##

# Mynd 3
# Hagvöxtur og fjárútlátabreyting 1996 til 2007
plot(wdi$year[35:46], wdi$growth_perc[35:46], type = "l",
				main = "Hagvöxtur og fjárútlátabreyting 1996 til 2007",
				ylab = "Árleg %-breyting", xlim = c(1996,2007),
				xlab = "Ártal", ylim = c(-10,20));
par(new = T);
plot(exp$year[35:46], exp$exp_ann_growth[35:46], type = "l",
				col = "red", xlab = "", ylab = "", main = "", ylim = c(-10,20),
				lty = 2);
grid(nx = NULL, ny = 0, col = "blue");
legend(x = 1996, y = 20, legend = c("Hagvöxtur á ári (%)",
				    "Fjárútlát (%-breyting)"),
				    lty = c(1,2), col = c("black","red"));
abline(0, 0, col = "grey");
#Fylgni milli hagvaxtar og fjárútlátabreytinga 1996 til 2007
cor(wdi$growth_perc[35:46], exp$exp_ann_growth[35:46]);
#Fylgnistuðull, Pearson's r

#Mynd 4
plot(wdi$year, wdi$growth_perc, type = "l",
		 main = "Hagvöxtur og fjárútlátabreyting 1963 til 2013",
		 ylab = "Árleg %-breyting",
		 xlab = "Ártal", ylim = c(-10,20));
par(new = T);
plot(exp$year, exp$exp_ann_growth, type = "l", col = "red",
		 xlab = "", ylab = "", main = "", ylim = c(-10,20), lty = 2);
grid(nx = 55, ny = 0, col = "blue");

legend(x = 1985, y = 20, legend = c("Hagvöxtur á ári (%)",
																		"Fjárútlát (%-breyting)"),
																		lty = c(1,2), col = c("black","red"));
abline(0, 0, col = "grey");
#Fylgni milli hagvaxtar og fjárútlátabreytinga 1963 til 2013
cor(wdi$growth_perc, exp$exp_ann_growth);
#Fylgnistuðull, Pearson's r

##########
## 4.2 ##

#Hlöðum inn mFilter pakkann til að keyra Hodrick Prescott síun

library("mFilter");

#HP síun á VLF (GDP)
hp <- hpfilter(x = wdi$gdp, freq = 6.25, type = "lambda");
#Viðskiptakjör reiknuð út frá
#Verðvísutölum útflutnings og innflutnings
tot <- wdi$exp_PI / wdi$imp_PI * 100;
#HP síun á Terms of Trade
hptot <- hpfilter(x = tot, freq = 6.25, type = "lambda")

# Mynd 5
plot(wdi$year, wdi$gdp, type = "l",
		 main = "VLF Íslands og Framleiðslugeta samkvæmt HP-síun",
		 ylab = "USD", xlab = "Ártal", ylim = c(0,(2.5*10^10)), lty = 2)
par(new = T);
plot(wdi$year, hp$trend, type = "l", col = "red", xlab = "",
		 ylab = "",main = "", ylim = c(0, (2.5*10^10)));
grid(nx = 55, ny = 0, col = "blue");
legend(x = 1962, y = (2.5*10^10),
			 legend = c("VLF", "Framleiðslugeta skv. HP-síun"),
			 lty = c(2,1), col = c("black","red"));

# Mynd 6
plot(wdi$year, tot, type = "l",
		 main = "Viðskiptakjör og langtímaleytni viðskiptakjara samkvæmt HP-síun", 
		 ylab = "Viðskiptakjör (vísitala)",
		 xlab = "Ártal", ylim = c(70,120), lty = 2);
par(new = T);
plot(wdi$year, hptot$trend, type = "l", col = "red", xlab = "",
		 ylab = "",main = "", ylim = c(70, 120));
grid(nx = 55, ny = 0, col = "blue");
legend(x = 1967, y = 83, legend = c("Viðskiptakjör",
																		"Langtímaleytni skv. HP-síun"),
			 lty = c(2,1), col = c("black","red"));

#Log-frávik af GDP (aka GDP gap) og log-frávik af TOT (aka TOT gap)
gdpgap <- log(wdi$gdp) - log(hp$trend);
totgap <- log(tot) - log(hptot$trend);


# #Plotta GDPGAP
# # Ef þess er óskað þarf einfaldlega að "afkommenta" þennan hluta
# plot(wdi$year, gdpgap, type = "l");
# abline(0, 0, col="grey");
#
# #Plotta TOTGAP
# plot(wdi$year, totgap, type ="l");
# abline(0,0, col ="grey");

#For lúppa til að reikna út öll möguleg
#sveifluáhrifatímabil sem passa við tiltekna marktækni
tafla <- c(1, 2, 3,4); #Tafla sem skrifað er yfir í komandi lúppu
pValue <- 0.05; #Breyta þessu gildi til að ná fram marktækni
# miðað við annað p-gildi svo sem 0.1, 0.5, eða 0.01
for (i in 2:48) {
	for (j in (i + 4):52) {
		temp_LM <- lm(sur[i:j,2] ~ gdpgap[i:j,]
										+ totgap[i:j,]
										+ sur[i:j,3]);
		if (summary(temp_LM)$coefficients[2,4] < pValue) {
				print(paste("Fyrir",sur[i,1],
					    "sem byrjunarár og",
					    sur[j,1], "sem lokaár"));
			tafla <- rbind(tafla, c(sur[i,1], sur[j,1],
			summary(temp_LM)$coefficients[2,1],
			summary(temp_LM)$coefficients[2,4]));
		}
	}
}
tafla <- tafla[-1,]; tafla <- data.frame(tafla);
names(tafla) <- c("Byrjun", "Lok", "Beta", "P-gildi");
write.table(tafla, "table_from_for_loop.csv")

#For lúppa til að breyta í XY-gilda töflu sem hægt er að
#snyrta í Excel og plotta út frá án þess að nota ts.plot
lengja <- c("Ár", "Beta-gildi");
for (i in 1:(nrow(tafla))) {
 for (j in tafla[i,]$Byrjun:tafla[i,]$Lok) {
	 lengja <- rbind(lengja, c(j, tafla[i,]$Beta) );
	}
}

#Skrifa út CSV skjal
write.table(lengja, file = "umreiknad2.csv")
#Lesa inn CSV skjal fyrir sveifluáhrifa tímaraðirnar og snyrta til
cycle <- read.csv2('Cyclicality.csv')
cycle <- data.frame(cycle); nafnavector <- cycle[,1];
cycle <- cycle[,-1]; row.names(cycle) <- nafnavector;
#Lesa inn CSV skjal með meðaltölum
medaltal <- read.csv("average_beta.csv")

#Lesa inn CSV skjal sem brýtur tímaraðirnar niður
#í gildi sem birtast á scatterplotti
umreikn <- read.csv("cycle_scatter.csv")

# # Plotta og gera línulegt módel (margliðu).
# # Ef þess er óskað þarf einfaldlega að "afkommenta" þennan hluta
#plot(umreikn$y ~umreikn$x, ylim=c(-1.5,1.5), xlim=c(1960,2010),
# xlab="Ártal", ylab="Sveifluáhrif (beta)");
#fit <- lm(umreikn$y ~ poly(umreikn$x, 8)) #Talan hér segir til um veldisvísi
#lines(umreikn$x, fitted(fit), col='red', pch=20);
# # Hér endar línulega módelið

# Mynd 7
#Öll sveifluáhrifa tímabil við p-gildi < 0.05
ts.plot(cycle, gpars = list(xaxt = "n", col = rainbow(10),
	ylim = c(-1.5,1.5), xlim = c(-2,48),
	main = "Öll sveifluáhrifatímabil við p-gildi < 0.05",
	xlab = "Ártal", ylab = "Sveifluáhrif (beta)"));
grid(nx = 54, ny = 0, col = "grey");
par(new = T);
plot(medaltal$year, medaltal$average, type = "l", lwd = 0, ylim = c(-1.5,1.5),
		 xlim = c(1960, 2010), xlab = "", ylab = "");
abline(0, 0, col = "grey");

# Mynd 8
# Tímalína sveifluáhrifa
ts.plot(cycle, gpars = list(xaxt = "n", col = rainbow(10), ylim = c(-1.5,1.5),
	xlim = c(-2,48), main = "Tímalína sveifluáhrifa",
	xlab = "Ártal", ylab = "Sveifluáhrif (beta)"));
grid(nx = 54, ny = 0, col = "grey");
par(new = T);
plot(medaltal$year, medaltal$average, type = "l", ylim = c(-1.5,1.5),
		 xlim = c(1960, 2010), xlab = "", ylab = "");
abline(0, 0, col = "grey");

############
# Kafli 5 #
###########

# Mynd 9
#Sveifluáhrif og kjörfylgi
par(cex = 1);
par(cex.axis = 0.92);
plot(pearson_check$year_elected, pearson_check$beta, type = "l",
		 ylim = c(-1.5,1.5), xlim = c(1960, 2010), main = "Sveifluáhrif og kjörfylgi",
		 xlab = "", ylab = "");
grid(nx = 54, ny = 0, col = "blue");
legend(x = 1960, y = 1.5, legend = c("Sveifluáhrif Beta (vinstri ás)",
															 			 "Hlutfallsbr. fylgis. % (hægri ás) "),
			 lty = c(1,4), lwd = c(1,2), col = c("black","purple"));
abline(0, 0, col = "grey");
par(new = T);

plot(pearson_check$year_elected, pearson_check$F_percentage, type = "l",
		 lty = 4, lwd = 2, col = 'purple', xlim = c(1960,2010), ylim = c(-35,35),
		 axes = F, xlab = '', ylab = ''); axis(4);
#Fylgni milli sveifluáhrifa innan kjörtímabils og kjörfylgisbreytinga
cor(pearson_check$beta, pearson_check$F_percentage)
#Fylgnistuðull, Pearson's r

###Fin###
