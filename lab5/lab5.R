install.packages('dplyr', dependencies = TRUE)
install.packages('ggplot2', dependencies = TRUE)

data("Orange")
help("Orange")

Orange
class(Orange)
str(Orange)
summary(Orange)
boxplot(Orange)
boxplot(Orange$circumference)

help.start()
help(diff)
RSiteSearch("diff")
example(diff)
help.search("Cars")
apropos("foot")
help("citFooter")

data()
library()

install.packages("Matrix")
exp(log(sin(1/2),base=7)-atan(sqrt(1+cos(0.05))))

x = c(1, 8, 2, 6, 3, 8, 5, 5, 5, 5) # створення вектора x
x

y<-x[x>=5 & x<=6] 
y
sum(y) # сума компонент вектора
prod(y) # добуток компонент вектора
diff(y) # різниця між сусідніми компонентами вектора.


z<-seq(-15,100,by=5) 
x<-replace(z,z%%10==0,c(0)) #заміна компонент вектора, які кратні 10, на 0
x
y<-x[x>mean(x)] #виділення тих компонент вектора, які більші за середнє значення вектора х.
y
sum(y) #сума компонент вектора у


vect<-c(-3,5,6,7,8,9,4,-4,-6,7,8,12,80,-5,0,6,21,-3,7,10) #створення вектора з парною кількістю елементів
M1<-matrix(vect,nrow=2) #створення з елементів вектору vect матриці з двома рядками.
M1
M2<-matrix(vect,ncol=2) #створення з елементів вектору vect матриці з двома стовпчиками
M2



vec <- numeric(18) #створення порожнього вектору з 18 елементів.
dim(vec)<-c(3,6) #задання розмірності для майбутньої матриці (3 рядка, 6 стовпчиків).
A <- edit(vec) #введення даних в матрицю з клавіатури
A
A*A #поелементне множення матриць



vect1<-c(0,3,2,9,-5,6,0,5,7,3,-3,0) #введення даних у вектор
B<-matrix(vect1,nrow = 3, ncol = 4, byrow = TRUE) #створення матриці з 3 рядками і 4 стовпчиками
B
C<-t(B) #транспонування матриці.
C
D<-B%*%C
D
A<-C%*%B
A

solve(D)


vec <- numeric(16)
dim(vec)<-c(4,4)
A <- edit(vec)
A
diag(A)<-replace(diag(A), diag(A)> mean(A), c(0)) #заміна діагональних елементів матриці, що більші за середнє значення елементів матриці, на 0
A
a<-c(0,3,-2)
a
y<-solve(A,a) #розв’язання лінійного рівняння.
y

vect<-c(-3,5,6,7,8,9,4,-4,-6,7,8,12,80,-5,0,6,21,-3,7,10) #створення вектора
vect
g<-quantile(vect,c(0,0.25,0.5,0.75,1)) # створення вектора, значеннями якого є квартілі вектора vect
g
u<-cut(vect,breaks=g,labels=c('Перший', 'Другий', 'Третій', 'Четвертий'),
       include.lowest=TRUE) #поділ на інтервали, кінцями інтервалів є квартілі, останній параметр дозволяє уникнути значення NA для -6
u

vik<-c(21,18,76,35,72,29,45,67,43,23,25,87,45,24,25) #створення вектора.
x<-sample(c(0,1),15,replace=TRUE)#створення вектора, який складається з 1 і 0 (всього 15).
x
mean(vik) #середній вік пацієнтів

x<-c(mean(Loblolly$height[Loblolly$age<10]),mean(Loblolly$height[Loblolly$age>10&Loblolly$age<16]),mean(Loblolly$height[Loblolly$age>15&Loblolly$age<21]),mean(Loblolly$height[Loblolly$age>20]))
# числовий вектор, який складається з середньої висоти дерева для таких категорій віку сосни: до 10 років, 10-15, 16-20, більше 20
x


h301<-Loblolly$height[Loblolly$Seed==301] #формування вектору даних, які відповідають висоті древ з насіння типу 301. 
a301<-Loblolly$age[Loblolly$Seed==301] #запис у вектор даних, які відповідають віку древ насіння типу 301
a315<-Loblolly$age[Loblolly$Seed==315] #запис у вектор даних, які відповідають віку древ насіння типу 315. 
h315<-Loblolly$height[Loblolly$Seed==315] #запис у вектор даних, які відповідають висоті древ з насіння типу 315
plot(a301,h301,type='l',lty=5,col='green',ylab='Висота дерева', xlab="Вік дерева", main="Графік залежності висоти дерева від віку") #побудова графіку залежності висоти дерев типу 301 від віку
lines(a315,h315, add=T,col = "violet") #побудова графіку залежності висоти дерев типу 315 від віку





library(dplyr)
library(ggplot2)

flats<-read.csv("flats.csv", stringsAsFactors = FALSE, encoding = "UTF-8")


class(flats)
str(flats)

?read.csv

flats <- read.csv("flats.csv", stringsAsFactors=FALSE, dec= ",")
str(flats)

dim(flats)
head(flats)
tail(flats)
names((flats))


str(flats)
summary(flats)

glimpse(flats)

count(flats, Місто)


flats %>%
count(Місто) %>%
arrange(n)


flats %>%
filter(Місто != "Києво-Святошинський") %>%
filter(Кімнат == 3) %>%
count(Місто) %>%
arrange(desc(n)) # arrange – сортування, desc – спадаючий порядок


flats %>%
filter(Кімнат == 2) %>%
filter(Місто != "Києво-Святошинський") %>%
count(Місто) %>%
arrange(desc(n))


ggplot(flats, aes(x=Кімнат)) +geom_bar(fill="lightblue",col="grey") +   ylab('Кількість')

p <- ggplot(flats, aes(x=Загальна_площа)) +  geom_bar(fill="lightblue", col="grey") + ylab('Кількість')
p

library(ggplot2)
ggplot(flats, aes(x=Загальна_площа, y=Ціна)) +  geom_point()





