#Подключаем библиотеки (копировать в консоль)
library(rio)
library(skimr)
library(xlsx)
library(ggplot2)

#Читаем дату (уже подготовил заранее)
df <- read.csv("GBR.csv")

#Определяем пол
df$gen[d$ST004D01T == 1] <- "female"
df$gen[d$ST004D01T == 2] <- "male"
df$gen[d$ST004D01T != 1 & d$ST004D01T != 2] <- "other"

#Факторим образование матери
df$ST005Q01TA <- factor(df$ST005Q01TA, labels=c("Completed 3A",
"Completed 3B or 3C",
"Completed 2nd", "Completed 1st","Not completed 1st"))


#Считать математику придется отдельно
math_short <- select (df, gen, starts_with("PV") & ends_with ("MATH"),ST005Q01TA,MISCED)
math_short$rowMean <- rowMeans(math_short[,c(2:11)])

#Cтроим балл по математике в разрезе пола и образования матери
pl <- ggplot(subset(math_short,!is.na(ST005Q01TA)),aes(ST005Q01TA,rowMean, fill=gen))+
  geom_boxplot()+
  xlab("Mother's ISCED level")+
  ylab("Student's MATH")+
  scale_fill_discrete(name="Student's gender")+
  ggtitle("_")
pl

median(math_short$rowMean)
mean(math_short$rowMean)

#Ради интереса строим по MISCED
math_short$MISCED <- factor(math_short$MISCED,
labels=c("None","ISCED 1","ISCED 2",
"ISCED 3B, C","ISCED 3A, ISCED 4",
"ISCED 5B","ISCED 5A, 6"))

pl_misc <- ggplot(subset(math_short,!is.na(MISCED)),
aes(MISCED,rowMean, fill=gen))+
  geom_boxplot()+
  xlab("Mother's ISCED level")+
  ylab("Student's MATH")+
  scale_fill_discrete(name="Student's gender")+
  ggtitle("MATH level depending on Mother's ISCED level")
pl_misc

#Получаем распределение среднего PV по Математике
pl_MATH <-  ggplot(math_short,aes(rowMean))+
  geom_histogram(binwidth = 2)+
  xlab(" ")+
  ylab(" ")+
  ggtitle("PV MATH Average")
pl_MATH

#Получаем распределение среднего PV по Математике с учетом пола ученика
pl_MATH_gend <-  ggplot(math_short,aes(rowMean,fill=gen))+
  geom_histogram(binwidth = 2)+
  xlab(" ")+
  ylab(" ")+
  scale_fill_discrete(name="Student's gender")+
  ggtitle("PV MATH Average — adjusted to Stugent's gender")

pl_MATH_gend

#Дальше больно и сложно мы вытаскиваем обеспокоенность получить плохую оценку
df$ST118Q02NA <- factor(df$ST118Q02NA, labels=c("Completely Disagree",
"Disagree", "Agree","Completely Agree"))

tWorry_NNA <- subset(df,!is.na(ST118Q02NA))
tWorry <- select(tWorry_NNA,ST118Q02NA)

tw_labe <- table(tWorry)

tw_labels <- c(837,3460,6279,3084)

descr_WORRY <- df %>% 
  group_by(ST118Q02NA) %>% 
  summarise() 

#Дальше начинается очевидная ерунда. Где аналог countif?

descr_WORRY$NUMUP[1] <- tw_labels[1]
descr_WORRY$NUMUP[2] <- tw_labels[2]
descr_WORRY$NUMUP[3] <- tw_labels[3]
descr_WORRY$NUMUP[4] <- tw_labels[4]

descr_WORRY$NUM[1] <- ((tw_labels[1]*100)/13660) 
descr_WORRY$NUM[2] <- ((tw_labels[2]*100)/13660) 
descr_WORRY$NUM[3] <- ((tw_labels[3]*100)/13660) 
descr_WORRY$NUM[4] <- ((tw_labels[4]*100)/13660)

descr_WORRY$NUM[1] <- paste(as.character(round(as.numeric(descr_WORRY$NUM[1]),2)),"%")
descr_WORRY$NUM[2] <- paste(as.character(round(as.numeric(descr_WORRY$NUM[2]),2)),"%")
descr_WORRY$NUM[3] <- paste(as.character(round(as.numeric(descr_WORRY$NUM[3]),2)),"%")
descr_WORRY$NUM[4] <- paste(as.character(round(as.numeric(descr_WORRY$NUM[4]),2)),"%")

descr_WORRY <- descr_WORRY[1:4,]

pl_WORRY <- ggplot(descr_WORRY,aes(x=ST118Q02NA,y=NUMUP,
fill=ST118Q02NA))+
  geom_col() +
  coord_polar()+
  geom_text(aes(label = NUM),
  vjust = 0, nudge_y = 800)+
  xlab(" ")+
  ylab("Number of students")+
  scale_fill_discrete(name="Extent to what student agrees")+
  ggtitle("Worrying about bad grades")

pl_WORRY

#Срез в MISCED

WORRY_tbl <- subset(df,!is.na(ST118Q02NA)
  & !is.na(MISCED))%>% 
  count(ST118Q02NA,MISCED)

MISCED_tbl <- subset(df,!is.na(ST118Q02NA)
& !is.na(MISCED)) %>%
  count(MISCED)

#Дальше идет тупорукая ерунда

#WORRY_tbl[,WORRY_tbl$MISCED == "None"]$tot <- MISCED_tbl[,MISCED_tbl$MISCED == "None"]$n
#Она не сработала

WORRY_tbl$tot <- NA
j <- 1
k <- c("None","ISCED 1","ISCED 2",
       "ISCED 3B, C","ISCED 3A, 4",
       "ISCED 5B","ISCED 5A, 6")
i <- 1
l <- 1

for (i in i:nrow(WORRY_tbl)) {
  for (j in length(k)) {
    if (WORRY_tbl[i,2] == k[j]) {
      for (l in l:nrow(MISCED_tbl)) {
        if (MISCED_tbl[l,1] == k[j]) {
          WORRY_tbl[i,4] <- MISCED_tbl[l,2]
        }
      }
  }
  }
}

#Надо дебагнуть цикл выше

id <- c(1,8,15,22)
WORRY_tbl[c(1,8,15,22),4] <- 64
WORRY_tbl[c(2,9,16,23),4] <- 97
WORRY_tbl[c(3,10,17,24),4] <- 563
WORRY_tbl[c(4,11,18,25),4] <- 2730
WORRY_tbl[c(5,12,19,26),4] <- 2374
WORRY_tbl[c(6,13,20,27),4] <- 2609

WORRY_tbl$n <- as.numeric(WORRY_tbl$n)
WORRY_tbl$tot <- as.numeric(WORRY_tbl$tot)
WORRY_tbl$prct <- paste(
  as.character(
    round(
      as.numeric(
        WORRY_tbl$n * 100 / WORRY_tbl$tot)
      ,2))
    ,"%"
               )

pl_WORRY_adj <- ggplot(WORRY_tbl, 
aes(MISCED,n,fill=ST118Q02NA))+
  geom_col(position = 'fill')+
  coord_flip()+
  xlab(" ")+
  ylab(" ")+
  #geom_label()+
  #geom_text(aes(label=prct))+
  scale_fill_discrete(name="The extent to what student fears to receive a bad grade")+
  ggtitle("Proportion of those who fears to receive a bad grade in regard of Mother's education")
pl_WORRY_adj

#Считаем распределение ESCS
pl_ESCS <- ggplot(subset(df,!is.na(ESCS)),aes(ESCS))+
  geom_histogram(binwidth = 0.05)+
  xlab(" ")+
  ylab(" ")+
  ggtitle("ESCS Level Distribution")
pl_ESCS

#ESCS в зависимости от MISCED и GEN
df$MISCED <- factor(df$MISCED,
labels=c("None","ISCED 1","ISCED 2",
"ISCED 3B, C","ISCED 3A, 4",
"ISCED 5B","ISCED 5A, 6"))

pl_ESCS_ad <- ggplot(subset(df,!is.na(ESCS) 
& !is.na(MISCED) & !is.na(gen)),
aes(MISCED,ESCS))+
  geom_boxplot(aes(fill=gen))+
  xlab("Mother's ISCED level")+
  ylab("EsCS level")+
  scale_fill_discrete(name="Student's gender")+
  ggtitle("ESCS level — adjusted to Stugent's gender and Mother's education")

pl_ESCS_ad

#Количество компьютеров дома

df$ST012Q06NA <- factor(df$ST012Q06NA,labels=c("None",
  "One","Two","Three or more"))

tpl <- df %>% 
  group_by(ST012Q06NA) %>% 
  summarise()

pl_ICT <- ggplot(subset(df,!is.na(ST012Q06NA)),
aes(ST012Q06NA,fill=ST012Q06NA))+
  geom_bar()+
  ylab("Number of students")+
  xlab(" ")+
  geom_label()
  scale_fill_discrete(name="Amount of computers at student's home")+
  ggtitle("Computers at student's home")

pl_ICT

#Количество компьютеров дома от MISCED

pl_ICT_a <- ggplot(subset(df,!is.na(ST012Q06NA) & !is.na(MISCED)),
aes(MISCED,fill=ST012Q06NA))+
  geom_bar(position = 'fill')+
  ylab("Proportion of students")+
  xlab("Mother's ISCED level")+
  scale_fill_discrete(name="Amount of computers at student's home")+
  ggtitle("Computers at student's home in regard of Mother's education")
pl_ICT_a

#Считаем распределение MISCED

df$MISCED2 <- factor(df$MISCED,
labels=c("None","ISCED 1","ISCED 2",
"ISCED 3B, C","ISCED 3A, 4",
"ISCED 5B","ISCED 5A, 6"))

pl_MISCED <- ggplot(subset(df,!is.na(MISCED2)),aes(MISCED2))+
  geom_bar()+
  xlab("Mother's ISCED level")+
  ylab("Number of students")+
  ggtitle("MISCED Level Distribution")

pl_MISCED