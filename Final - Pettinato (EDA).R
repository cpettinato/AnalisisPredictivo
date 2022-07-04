#getwd()
setwd("C:/Users/camil/OneDrive/Documentos/2022/ITBA/2do Cuatrimestre 2020/Analisis Predictivo/Examenes/Final")

#ANALISIS EXPLORATORIO DE DATOS
library(readr)
library(dplyr)
ReadingHabit <- read_csv("ReadingHabit.csv")
View(ReadingHabit)
head(ReadingHabit,5)
str(ReadingHabit)
names(ReadingHabit)
summary(ReadingHabit)
libros = ReadingHabit

head(libros)


#Ver cuantos registros vacios tengo
sapply(libros, function(x) sum(is.na(x))) 


#Respuestas Posibles a las Preguntas
sex = libros %>% distinct(Sex, .keep_all = TRUE)
View(sex)
#Genero = female, male

race = libros %>% distinct(Race, .keep_all = TRUE)
View(race)
#Raza: Refused, Native American/American Indian, Mixed race, Asian or Pacific Islander,Black or African-American, White, Don't knok, Other

marital_status = libros %>% distinct(`Marital status?`,.keep_all = TRUE)
View(marital_status)
#Estado civil = Divorced, Married, Never been married, Widowed, Don't know, Living with a partner, Single, Separated

education = ReadingHabit %>% distinct(Education, .keep_all = TRUE)
View(education)
#Educacion = College graduate, High school graduate, High school incomplete, Some college, no 4-year degree, Post-graduate training/professional school after college, Technical, trade or vocational school AFTER high school, None, Don't know

employement = ReadingHabit %>% distinct(Employement,.keep_all = TRUE)
View(employement)
#Empleo = Retired, Employed full-time, Employed part-time, Have own business/self-employed, Student, Other, Not employed for pay, Disabled

income = libros %>% distinct(Incomes,.keep_all = TRUE)
View(income)
#income = $20,000 to under $30,000, Less than $10,000, $40,000 to under $50,000, $10,000 to under $20,000, $50,000 to under $75,000, $100,000 to under $150,000, $75,000 to under $100,000, 9$100,000 to under $150,000 (ver si hay solo uno o muchos), $30,000 to under $40,000, Refused

ultimo =  libros %>% distinct(`Last book you read, you.`,.keep_all = TRUE)
View(ultimo)
#ultimo libro = Purchased the book, Borrowed the book from a friend or family member, Borrowed the book from a library, Got the book some other way, 8 (filtrar), 9 (filtrar)

diario =  libros %>% distinct(`Do you happen to read any daily news or newspapers?`,.keep_all = TRUE)
View(diario)
#diario = yes, no, don't know

revista =  libros %>% distinct(`Do you happen to read any magazines or journals?`,.keep_all = TRUE)
View(revista)
#revista = yes, no, don't know

#Filtrar resgistros
View(libros)
libros = filter(libros, Race != 'Refused')
libros = filter(libros,Race != "Don't know")
libros = filter(libros,`Marital status?`!= "Don't know")
libros = filter(libros,Education != "Don't know")
libros = filter(libros, Incomes != 'Refused')
sum(with(libros,Incomes == "9$100,000 to under $150,000")) #Error de tipeo? Como nose, lo filtro
libros = filter(libros, Incomes != "9$100,000 to under $150,000")
libros = filter(libros,`Read any e-books during last 12months?` != "Don't know" )
libros = filter(libros,`Last book you read, you.` != ' 8' )
libros = filter(libros,`Last book you read, you.`!= '9' )
libros = filter(libros,`Do you happen to read any daily news or newspapers?` != "Don't know" )
libros = filter(libros,`Do you happen to read any magazines or journals?` != "Don't know" )

#Variable a predir: Cuanto lee? (no lee, lee poco, lee bastante, lee mucho, lee demasiado)
libros = libros %>% mutate(Lee= case_when(
          `How many books did you read during last 12months?`==0 ~ "No Lee",
          `How many books did you read during last 12months?`>=1 & `How many books did you read during last 12months?`< 25~ "Lee muy poco",
          `How many books did you read during last 12months?`>=25 & `How many books did you read during last 12months?`< 50~ "Lee frecuentemente",
          `How many books did you read during last 12months?`>=50 & `How many books did you read during last 12months?`< 75 ~ "Lee mucho",
          T~"Lee en exceso"
          ))

install.packages("xlsx")
library(xlsx)
write.xlsx(libros, "base.xlsx")
base <- read_excel("base.xlsx")
View(base)

#OUTLIERS
boxplot(base$Age,main="Edad de los encuestados",ylab="Edad",col="pink")
boxplot(base$`How many books did you read during last 12months?`,main="Cantidad de libros leidos en 12 meses",ylab="Libros",col="lightblue")


#VISUALIZACION
library(ggplot2)
library(funModeling) 
library(tidyverse)
freq(base$Sex)
plot_num(base)

#Cuantos registros hay de cada categoria
install.packages("DataExplorer")
library(DataExplorer)
plot_histogram(base)
plot_boxplot(base, by = 'Lee')

#Cantidad
hist(base$Age,main="Edad de los encuestados",ylab="Cantidad", xlab="Edad",col="pink")
hist(base$`How many books did you read during last 12months?`,main="Cantidad de libros leidos en 12 meses",ylab="Cantidad", xlab="Libros",col="lightblue")

#Distribucion de las variables relacionadas a la persona en si en base a cuanto leen
names(base)
ggplot(base) +
  aes(x = Lee, y = Age, color = Lee ,xlab = "", ylab = ) +
  geom_jitter() +
  theme(legend.position = "none")

ggplot(base) +
  aes(x = Lee, y = Sex, color = Lee ,xlab = "", ylab = ) +
  geom_jitter() +
  theme(legend.position = "none")

ggplot(base) +
  aes(x = Lee, y = Race, color = Lee ,xlab = "", ylab = ) +
  geom_jitter() +
  theme(legend.position = "none")

ggplot(base) +
  aes(x = Lee, y = `Marital status?`, color = Lee ,xlab = "", ylab = ) +
  geom_jitter() +
  theme(legend.position = "none")

ggplot(base) +
  aes(x = Lee, y = Education, color = Lee ,xlab = "", ylab = ) +
  geom_jitter() +
  theme(legend.position = "none")

ggplot(base) +
  aes(x = Lee, y = `Employement`, color = Lee ,xlab = "", ylab = ) +
  geom_jitter() +
  theme(legend.position = "none")

ggplot(base) +
  aes(x = Lee, y = Incomes, color = Lee ,xlab = "", ylab = ) +
  geom_jitter() +
  theme(legend.position = "none")
