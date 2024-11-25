

# read dataset 
dataset <- read.csv('C:/Users/STAR/OneDrive/Desktop/kidney_disease1.csv' , 
                    stringsAsFactors = FALSE)
dataset
str(dataset)

dataset$pcv <- as.numeric(dataset$pcv)
#change character to numeric because pcv column has number and we should define that as a numeric

dataset$wc <- as.numeric(dataset$wc)
#change character to numeric because wc column has number and we should define that as a numeric

dataset$rc <- as.numeric(dataset$rc)
#change character to numeric because rc column has number and we should define that as a numeric

dataset$id <- NULL  # remove the id column because i think it was useless


mean(dataset$age) # getting average of age column
#> NA
as.integer(mean(dataset$age, na.rm = T)) #this is a average age of age column
#> 51
mean_age <- as.integer(mean(dataset$age, na.rm = T))
dataset$age[is.na(dataset$age)]
#> NA NA NA NA NA NA NA NA NA
dataset$age[is.na(dataset$age)] <- mean_age #i define average age number instead of 'NA'


mean(dataset$bp)
#> NA
as.integer(mean(dataset$bp, na.rm = T))
#> 76
mean_bp <- as.integer(mean(dataset$bp, na.rm = T))
dataset$bp[is.na(dataset$bp)]
#> NA NA NA NA NA NA NA NA NA NA NA NA
dataset$bp[is.na(dataset$bp)] <- mean_bp


dataset$sg[is.na(dataset$sg)] <- 0 # it is nominal because of that i just change the NA to 0
dataset$al[is.na(dataset$al)] <- 0 # this is also nominal
dataset$su[is.na(dataset$su)] <- 0 # this is also nominal


dataset$rbc <- factor(dataset$rbc,levels = c('normal' , 'abnormal') , labels = c(2,3))
dataset$rbc[is.na(dataset$rbc)] <- 2
# rbc was a character and i change that to factor because 
# i wanted to change 'normal' , 'abnormal' to number 2 and 3
# also i change the NA to number 2

str(dataset) # i get the structure 

dataset$pc <- factor(dataset$pc,levels = c("normal" , "abnormal") , labels = c(2,3))
dataset$pc[is.na(dataset$pc)] <- 2
# pc was a character and i change that to factor because 
# i wanted to change 'normal' , 'abnormal' to number 2 and 3
# also i change the NA to number 2

dataset$pcc <- factor(dataset$pcc,levels = c("present" , "not present") , labels = c(2,3))
dataset$pcc[is.na(dataset$pcc)] <- 2
# pcc was a character and i change that to factor because 
# i wanted to change 'present' , 'not present' to number 2 and 3
# also i change the NA to number 2


dataset$ba <- factor(dataset$ba,levels = c("present" , "notpresent") , labels = c(1,2))
dataset$ba[is.na(dataset$ba)] <- 2
# pcc was a character and i change that to factor because 
# i wanted to change 'present' , 'not present' to number 1 and 2
# also i change the NA to number 2

mean(dataset$bgr)
#> NA 
as.integer(mean(dataset$bgr, na.rm = T)) # this is numeric and i change it as integer to get non decimal number
#> 148
mean_bgr <- as.integer(mean(dataset$bgr, na.rm = T)) 
dataset$bgr[is.na(dataset$bgr)] <- mean_bgr


mean(dataset$bu)
#> NA
as.integer(mean(dataset$bu, na.rm = T))
#> 57
mean_bu <- as.integer(mean(dataset$bu, na.rm = T))
dataset$bu[is.na(dataset$bu)] <- mean_bu

# those columns has not many NA because of that i decided to use this code just to change to the 0
dataset$sc[is.na(dataset$sc)] <- 0 
dataset$sod[is.na(dataset$sod)] <- 0
dataset$pot[is.na(dataset$pot)] <- 0
dataset$hemo[is.na(dataset$hemo)] <- 0


mean(dataset$pcv)
#> NA
as.integer(mean(dataset$pcv, na.rm = T))
#> 38
mean_pcv <- as.integer(mean(dataset$pcv, na.rm = T))
dataset$pcv[is.na(dataset$pcv)] <- mean_pcv


mean(dataset$wc)
#> NA
as.integer(mean(dataset$wc, na.rm = T))
#> 8406
mean_wc <- as.integer(mean(dataset$wc, na.rm = T))
dataset$wc[is.na(dataset$wc)] <- mean_wc

dataset$rc[is.na(dataset$rc)] <- 0

dataset$htn <- factor(dataset$htn,levels = c("yes" , "no") , labels = c(1,2))# changing character to factor
dataset$htn[is.na(dataset$htn)] <- 2

dataset$dm <- factor(dataset$dm,levels = c("yes" , "no") , labels = c(1,2))# changing character to factor
dataset$dm[is.na(dataset$dm)] <- 2

dataset$cad <- factor(dataset$cad,levels = c("yes" , "no") , labels = c(1,2))# changing character to factor
dataset$cad[is.na(dataset$cad)] <- 2

dataset$appet<- factor(dataset$appet,levels = c("good" , "poor") , labels = c(1,2))# changing character to factor
dataset$appet[is.na(dataset$appet)] <- 2

dataset$pe <- factor(dataset$pe,levels = c("yes" , "no") , labels = c(1,2))# changing character to factor
dataset$pe[is.na(dataset$pe)] <- 2

dataset$ane <- factor(dataset$ane,levels = c("yes" , "no") , labels = c(1,2))# changing character to factor
dataset$ane[is.na(dataset$ane)] <- 2

dataset$classification <- factor(dataset$classification,levels = c("ckd" , "notckd") , labels = c(1,2))
dataset$classification[is.na(dataset$classification)] <- 2 

# Graphical chart

library(ggplot2)# start to using ggplot 
ggplot(dataset, aes(x = age , y = al)) # defind X and Y 
ggplot(dataset, aes(x = age , y = al))+geom_point()

a <- ggplot(dataset, aes(x = age , y = al)) + 
  geom_point() +geom_smooth(method = "lm")
#  adding more layers

plot(a) 
a + xlim(c( 5, 80)) + ylim(c(0 , 4))
# deleting 4 rows with xlim to age part and one row from ylim for Albumin part

a1 <- a + coord_cartesian(xlim= c( 5, 80) , ylim = c(0 , 4))
a1 + labs(title="Age and  albumin", 
          subtitle="From Kidney disease dataset", y="Albumin", 
          x="Age", caption="Kidney disease Demographics")
# Defining title , subtitle and caption

ggplot(dataset, aes(x=age, y=al)) + 
  geom_point(col="green", size=3) +geom_smooth(method="lm", col="firebrick") +
  # changing color and size of points
  
  coord_cartesian(xlim=c(5, 80), ylim=c(0, 4)) + 
  labs(title="Age and  albumin", subtitle="From 
Kidney disease dataset", y="Albumin", x="Age", 
       caption="Kidney disease Demographics")


b <- ggplot(dataset, aes(x=age, y=al)) + 
  geom_point(aes(col=bgr), size=3) +
  
  geom_smooth(method="lm", col="firebrick", size=2) + 
  # changing color and size of the line
  
  coord_cartesian(xlim=c(5, 80), ylim=c(0, 4)) + 
  labs(title="Age and  albumin", subtitle="From 
Kidney disease dataset", y="al", x="Age", 
       caption="Kidney disease Demographics")
b + geom_point(aes(col=bgr , size=wc))


dataset$bp

# using barplot for making bar chart

barplot(table(dataset$bp,dataset$classification))
#defining barplot with table and bp and classification columns

classification=factor(dataset$classification) 
barplot(table(dataset$classification,dataset$bp),beside=TRUE,legend.text=T)
barplot(table(dataset$classification,dataset$bp),beside=TRUE ,
        legend.text=c("less than 10","10_30","more than 30"))
title(main = "Kidney disease", xlab = "Blood pressure", 
      ylab = "classification", col.lab= "orange", font.main= 4 , col.main="red")
# writing title , xlab name ,ylab name , color of the lab text ,
#size of the font and color for main title

write.csv(dataset,"C:\\Users\\STAR\\OneDrive\\Desktop\\city capture\\QUESTION2.csv"
          ,row.names = FALSE)
#exporting csv file to excel

QUSTION2 <- read.csv("C:\\Users\\STAR\\OneDrive\\Desktop\\city capture\\QUESTION2.csv" , sep = ',')
# reading a file again


