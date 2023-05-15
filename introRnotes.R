library(tidyverse)

?mtcars
head(mtcars)
mtcars$qsec
qsec<-mtcars$qsec
#basic dataset 
sum(qsec)
length(qsec)
mean(qsec)
sd(qsec)
fluor<-as.numeric(c(9736,28,72,845,"Undetected", 549, 103))
summary(fluor)
mean(fluor)
mean(fluor, na.rm=T)
min(mtcars$qsec)
max(mtcars$qsec)
median(mtcars$qsec)
summary(qsec)
mean(trees)
?trees
summary(trees)
unique(trees$Height)
fluor
#returns unique
unique(trees$Height==76)
trees$Height==76
#remove values or data
rm(x)
rm(y)
class(trees)
a<-seq(from=0, to=100, by=10)
b<-seq_along(a)
plot(b~a)
a<-numeric(6)
class(a)
y=1:3
names(y)<-c("a","b","c")
y

hml <- c("high", "low", "medium", "low", "medium", "high")
class(hml)
#classing factors
hml<-factor(hml)
char<-as.character(rep(c("high", "medium", "low"), each = 100))
head(char)
# bytes in data
object.size(char)
object.size(factor(char))
as.integer(hml)
table(hml)
#ordering factors
hml<-factor(hml, levels=c("low", "medium", "high"), ordered=T)
table(hml)
hml[1]>hml[2]
#structure function - condensed version of data structure
str(hml)
hml<-factor(hml, levels=c("low", "medium", "high", "super high"), ordered=T)
hml
#storing this dataset as a backup
hml.bu<-hml
#add level
hml<-factor(hml, levels=c("super low", levels(hml), ordered=T))

#working with dataframes
Birds <- data.frame(Type = c("BirdofPrey", "BirdofPrey", "SongBird", 
                             "Shorebird", "Flightless", "SongBird", 
                             "Flightless"),
                    Num. = c(3, 15, 42, 8, 0, 29, 17),
                    Extinct = c(F,F,F,F,T,F,F), 
                    row.names = c("Eagle", "Hawk", "Bluebird", 
                                  "Egret", "Dodo", "Blue Jay", "Kiwi"))
Birds
class(Birds$Type)
class(Birds$Num.)
summary(Birds$Num.)
#working with lists
as.list(Birds)
#hard to work with and only do so when necessary, basically uneven data frames
list(letters = letters, 
     months=month.name,
     pi = pi,
     fives = seq(5,95,10))
#matrices all values have to be of the same class
?matrix
nine<-matrix(1:9, nrow=3, byrow=T,
             dimnames=list(Foo=LETTERS[1:3], Bar=month.abb[1:3]))
nine
data(BCI.env)
bci<-matrix(BCI.env$EnvHet)
array(BCI.env)
array(BCI.env$EnvHet, dim=c(2,5,5))
str(top300)
y<-top300[1:20, "n"]
str(y)
names(y) <-top300[1:20, "name"]
str(y)
y
y["Harper"]

#2/6/23 cont w/ top300 dataset indexing dataframes
y[c("Sophia", "Sofia")]
tail(y, 3)
log.vec<-c(F, F, rep(T, times=18))
y[log.vec]
y
y[c(T,F)]
#gives every other element of a vector or df
y[y==13066]
y[y>13066]
#sorts data to just the points you'd like
y[y!=13066]
#sorts when y doesnt equal 13066
y[y>median(y)]
#sorts when y is greater than the median
cnames <- c("Chloe", "Charlotte")
y[names(y) %in% cnames]
#can find the results within the larger dataset and returns them by using %in% "are found in"
islands
islands[c("Vancouver", "Victoria")]
sum(head(islands, 5))
str(islands)
mean(tail(islands, 43))
#mean of every other item in a group
mean(islands[c(F,T)])
mean(islands[seq(from=2, to=length(islands), by=2)])

#2/8/23
data(volcano)
dim(volcano)
volcano[1:6, 1:10]
mat<-matrix(1:4,2)
colnames(mat) <-c("A","B")
rownames(mat) <-c("X", "Y")
mat
mat["Y",]
#need , for rows, but not for columns
data(dune.env)
head(dune.env)
dune.env[c(1:3),]
data(varespec)
varespec[1:10, 1:5]
varespec[c(1:3), 1:8]
?varespec
varespec$Vaccviti
varespec$Cladrang
abs(varespec$Vaccviti-varespec$Cladrang)
data(varechem)
varechem
jeff <-c(varechem, varespec)
#can subtract from 2 different datasets, but need to be the exact same length
varespec[varechem$Humdepth<2,"Vaccviti"]-varespec[varechem$Humdepth<2,"Cladrang"]

#2/20/23
library(readxl)
Puromycin_data <- read_excel("~/Puromycin_data.xlsx")
Puromycin_data
#importing a file and skipping the first 41 rows, leaving 42 as the header row, 
#removing the top part, which was all random text
qPCR_data1 <- read_delim("C:/Users/ericr/Downloads/qPCR-data.txt", 
                               delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE, skip = 41)
qPCR_data1

#2/22/23
CT <- as.numeric(qPCR_data1$CT, na.rm=T)
summary(CT, na.rm=T)
save(qPCR_data1, file = "qPCR_data1", compress="gzip")

#2/27/23
library(ade4)
norm.dat<-rnorm(10000, 50, sd=10)
#(n, mean, sd)
hist(norm.dat, freq=F)
hist(norm.dat, xlim=(c(0,90)))
#can do hist on a matrix
hist(volcano, breaks=10)
hist(norm.dat)
lines(density(norm.dat))
data("msleep")
str(msleep)
#change to factor and keep in the data set
msleep$vore <- factor(msleep$vore)
str(msleep)
#the same below
boxplot(sleep_total~vore, msleep)
bp.data<-boxplot(msleep$sleep_total~msleep$vore)
#plot gives this data separately as a list
str(bp.data)
#gives all the information regarding a plot and 
#lets you change that info for all subsequent plots
par()
?par
par(fg="black", bg="white", cex=1)
boxplot(sleep_total~vore, msleep)
#plot 2 numerics and get a scatterplot
plot(sleep_total~log(bodywt), msleep)
#other way to do this and get axis labels to be scientific notation
plot(sleep_total~bodywt, msleep, log="x", col=msleep$vore)
legend("topright", legend=levels(msleep$vore), col=1:4, pch=1)
#col 0 is transparent, 1 black, etc.
plot(sleep_total~bodywt, msleep, log="x", col=msleep$vore)
legend("topright", legend=levels(msleep$vore), col=1:4, pch=1)

#3/1/23
library(ade4)
data(rhone)
plot(rhone$tab$pH~rhone$tab$air.temp, xlab="Air Temperature", 
     ylab="pH", main="Water and Air Temperature Correlation", 
     cex=1.5, bty="n", pch=2)
library(tidyverse)
data(msleep)
str(msleep)
#finds the mean of each of rem sleep as a function of order and aggregates them
rem_order <- aggregate(sleep_rem~order, msleep, mean)
head(rem_order)
#las makes horizontal labels, names adds the names, mar gives room for the labels themselves
par(mar=c(4,10,2,2), las=1)
barplot(rem_order$sleep_rem, horiz=T, names.arg = rem_order[,1])

data(VADeaths)
VADeaths
barplot(VAdeaths, beside=T)
library(RColorBrewer)
display.brewer.all()
brew.cols<-brewer.pal(3, "YlGnBu")
brew.cols
barplot(rem_order$sleep_rem, horiz=T, names.arg = rem_order[,1], col=brew.cols)

data(package="carData")
data("CanPop")
head(CanPop)
plot(CanPop$population~CanPop$year, fill=brew.cols)

#3/6/23 
sessionInfo()
let <- rep(LETTERS[1:4], times=4)
let
table(let)
ftable(let)

data(esoph)
str(esoph)
#observed numbers of each trait in a basic table
table(esoph[,c("agegp", "alcgp")])
etab <- table(esoph[,c("agegp", "tobgp")])
class(etab)
etab3d <- table(esoph[,c("agegp", "tobgp", "alcgp")])
plot(etab3d)
etab3d
ftab3d <- ftable(esoph[,c("agegp", "tobgp", "alcgp")])
#use for fish, lake, and pfas avg
ftab3d
plot(ftab3d)
ft.df<-as.data.frame(ftab3d)
ft.df
plot(ft.df)
dfmat <- as.data.frame.matrix(etab)
dfmat
plot(dfmat)
library(Hmisc)
describe(etab)
summary(1:1000)
summary(esoph)
describe(esoph)
?hmisc
methods(describe)
esoph.desc <- describe(esoph)
esoph.desc["agegp"]
esoph.desc["ncontrols"]
options(grType='plotly')
plot(esoph.desc[c("ncases", "ncontrols")])

#3/8/23
library(tidyverse)
library(Lahman)
HallOfFame
HOFd <- describe(HallOfFame[,c("votedBy", "inducted", "category")])
HOF <- table(HallOfFame[,c("votedBy", "inducted", "category")])
HOF
ftable(HOF)
plot(HOFd)
#filter by both voted by rows giving bbwaa and veterans and category voted by and inducted columns
HOFf <- ftable(HallOfFame[HallOfFame$votedBy %in% c("BBWAA", "Veterans"), 
                  c("category", "votedBy", "inducted")])
#aggregate allows for bringing together data by 1 function, 
#such as avg, sd, median, and makes a small table, and
#can add more columns w/ + to represent, gives interesting plot
warpbreaks
str(warpbreaks)
summary(warpbreaks)
?aggregate
woolbreaks <- aggregate(breaks~wool+tension, warpbreaks, FUN=mean)
library(carData)
head(Chile)
ChileSQ <- aggregate(statusquo ~region + education + sex, Chile, FUN=mean)
ChileSQ
plot(ChileSQ)

#binding data can use cbind or rbind
x.df <- data.frame(d=month.abb[1:8])
x.df
cbind(airquality[1:2,], airquality[11:12,])
rbind(airquality[1:2,], airquality[11:12,])

#3/20/23
#aggregating data
citation()
citation("ggplot2")
library(tidyverse)
data("diamonds")
#second one listed (cut) is the grouping variable, and need to do twice for the grouping variable,
#first needs to be numeric
diamondbycut <- aggregate(price~cut, diamonds, FUN=median)
diambycut <- aggregate(carat~cut, diamonds, FUN=median)
summary(diamondbycut)
plot(diambycut,diamondbycut)

#merging data
#need to merge by a common column
?merge
#all=T keeps everything even if not matched, all.x & all.y will keep all of one or other, F keeps least
#all=T gives NAs, F gives none but removes 
wq2 <- merge(wq, ec)
wq
ec
wq2
wq2 <- merge(wq, ec, all=T)
wq2
wq2 <- merge(wq, ec, all.x=T)
wq2
wq2 <- merge(wq, ec, all.y=T)
wq2
wq2 <- merge(wq, ec, by="month")
wq2
wq3 <- merge(wq, ec, by=1)
identical(wq2, wq3)

names(msh2)[3]<-"YEAR"
msh <- merge(msh1, msh2, all=F)
msh
msh_all <- merge(msh1, msh2, by = 1:3)
msh_all
table(msh1 [,1:3])
table(table(msh1 [,1:3]))
#table (table) gives the number for each, good for quick confirmation

#3/22/23
library(tidyverse)
library(reshape2)
library(plyr)
summarize(warpbreaks, 
          Avg = mean(breaks),
          Med = median(breaks),
          Dif = abs(Avg - Med))
ddply(warpbreaks,"wool", summarize,
      Avg = mean(breaks),
      Med = median(breaks),
      Dif = abs(Avg - Med))
ddply(warpbreaks,.(wool, tension), summarize,
      Avg = mean(breaks),
      Med = median(breaks),
      Dif = abs(Avg - Med))
ddply(warpbreaks,~wool+tension, summarize,
      Avg = mean(breaks),
      Med = median(breaks),
      Dif = abs(Avg - Med))
#can output lists if wanted, really ugly
dlply(warpbreaks,~wool+tension, summarize,
      Avg = mean(breaks),
      Med = median(breaks),
      Dif = abs(Avg - Med))
#arrays
daply(warpbreaks,~wool+tension, summarize,
      Avg = mean(breaks),
      Med = median(breaks),
      Dif = abs(Avg - Med))
#can input array to dataframe adply, mdply, ldply, etc. what you have to what you want is 
#first 2 letters
data(birthwt)

#3/27/23
library(MASS)
data("birthwt")
?summarize
?birthwt
head(birthwt)
ddply(birthwt, ~race + smoke, summarize,
      momwt = mean(lwt),
      birthwt = mean(bwt),
      SDmom = sd(lwt),
      SDbirth = sd(bwt),
      racecount = length(race),
      smokecount = length(smoke),
      physvisitmed = median(ftv))

#reshaping data
library(reshape2)
data("airquality")
#id are variables the identify what the sample is
aqm <- melt(airquality, id=c("Month", "Day"), na.rm = T)
aqm
# down~across notation
dcast(aqm, Month ~ variable, median)
#another ex, can use multiple grouping variables by adding together on the left of the ~
data("ChickWeight")
Chickmelt <- melt(ChickWeight, id=c("Time", "Chick", "Diet"), na.rm = F)
dcast(Chickmelt, Chick + Diet ~ Time, median)

#3/29/23
#conditional and loops

#if (logical statement) {body} 
?"if"

data(rivers)
is.numeric(rivers)
hist(log10(rivers), breaks=30)
abline(v=log10(median(rivers)), col=3, lwd=3)
#v is vertical line, and give it the x intercept
if (is.numeric(rivers)){
   median(rivers)
}
if (!is.numeric(rivers)){
   median(rivers)
}
#produces false response so nothing is output
x<-1:10
if (is.numeric(x)){median(rivers)}
x<-letters
if (is.numeric(x)){median(rivers)}
#no output
if (is.numeric(x)){median(rivers)} else {class(x)}
#have else statement that provides output for false data
x <- 1:10
if (is.numeric(x)){median(rivers)} else {class(x)}
#produces result from if, skips everything from else

x<-T
ifelse(x,1,2)
# ifelse(test, value to return if T, value to return if F)
?ifelse
lowercase <- T
ifelse(lowercase, letters, LETTERS)
lowercase <- c(T,F,T,F)
ifelse(lowercase, letters, LETTERS)

river.bin <- ifelse(rivers>median(rivers), "Long", "Short")
river.bin[1:10]
river.bin <- factor(river.bin,
                    levels=rev(unique(river.bin)), ordered=T)
str(river.bin)

?"for"
#for (each element in a vector) {do}
#for(blank in blank){if(blank){blank} else {blank}}
x<-1:10
for (i in x){
   cat(paste(i, ""))
}
#i inherits the value the first time through the loop, can use any object name but i is 
#common
riv.mean<-mean(rivers)
riv5<-rivers[1:5]
for (i in riv5){
   print(i - riv.mean)
}

x<-1
while (x<30){
   x<-x+1
   cat(paste(x, ""))
}
par()
#determine loop structure
for(i in par()){
   print(i)
}
#find all times where length of factors is > 3 in par 
for(i in par()){
      if (length(i) >3){
         print(i)
      }
}

#4/3/2023
library(ggplot2)
data(diamonds)
library(carData)
data("Chile")
summary(Chile)
library(ggpubr)
#alpha = transparency in geom point
ggplot(diamonds, aes(x=carat, y= price, color=clarity)) +
   geom_point(alpha = 0.1) +
   theme_classic2() +
  facet_wrap(~cut) +
  stat_smooth()

ggplot(movies, aes(x=rating, y=budget) +
  stat_smooth(method = "gam"))

movies1 <- subset(movies, budget > 10)
ggplot(movies1, aes(x=budget, y=rating)) +
  geom_point() +
  scale_x_log10()

#4/5/23
#Liz Carter guest lecture

#4/12/22
#calculate spp richness
library(vegan)
data(dune)
dune[1:5,1:5]
#where in comm is data greater than 0 and turning them to 1 to show presence and absence
#[comm>0] is logical expression
#omit comma in [,comm>0] bc it is a matrix
rich <- function(comm){
   comm[comm>0]<-1
   rowSums(comm)
}
rich(varespec)
#same as:
rich <- function(comm){
   apply(comm>0,1,sum)
}
rich(varespec)

library(car)
data("anscombe")

ans.lm <- lm(y3~x3, anscombe)
ans.lm
plot(y3~x3, anscombe, pch=16)
abline(ans.lm)
regLine(ans.lm)
summary(ans.lm)
fitted(ans.lm)
#values on regression line^^^
confint(ans.lm)
plot(ans.lm)

data(iris)
head(iris)
iris.lm <- lm(Sepal.Length~Petal.Length, iris)
plot(Sepal.Length~Petal.Length, iris, pch=16, col=iris$Species)
abline(iris.lm)
summary(iris.lm)
plot(iris.lm, col=iris$Species)

PFAS <- Data[,c(2,7,8)]
PFAS <- PFAS %>% 
  group_by(c(SUBSTANCE_NAME_ABBREVIATION)) %>% 
  mutate(row_id = row_number()) %>% 
  pivot_wider(id_cols = "SUBSTANCE_NAME_ABBREVIATION",  names_from = row_id,
              values_from = LAB_RESULT, 
              names_glue = "{.value}{row_id}") %>% 
  ungroup()
PFAS <- t(PFAS)
PFAS <- as.data.frame(PFAS)
PFAS <- PFAS %>%
  row_to_names(row_number = 1)
PFAS <- as.data.frame(sapply(PFAS, as.numeric))
?corr.test

ggplot(PFOS, aes(x=PFOS$`WEIGHT_MEAN (G)`, y=PFOS$`LENGTH_MEAN (MM)`)) +
  geom_point() +
  theme_classic() +
  theme(plot.title=element_text(face="bold", 
                                size=15, hjust=0.5)) +
  labs(title = "Length versus Weight",
       x = "Weight (g)",
       y = "Length (mm)") +
  geom_smooth(method = "lm", se = T)
?geom_smooth
