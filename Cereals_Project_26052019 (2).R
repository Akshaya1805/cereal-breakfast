setwd("D:/Neelam/DSP30/SM_24052020_Project_Cereals")
getwd()
#install.packages("tidyverse")
library(tidyverse)
library(psych)
#Welcome to the wonderful world of Tidyverse! It is the most powerful 
#collection of R packages for preparing, wrangling and visualizing data. 
# v ggplot2 3.3.0     v purrr   0.3.3
# v tibble  3.0.0     v dplyr   0.8.5
# v tidyr   1.0.2     v stringr 1.4.0
# v readr   1.3.1     v forcats 0.5.0
# -- Conflicts ------------------------------------------ tidyverse_conflicts() --
#   x dplyr::filter() masks stats::filter()
# x dplyr::lag()    masks stats::lag()
list.files()
cereals_data<-read.csv("D:/Neelam/DSP30/SM_24052020_Project_Cereals/cereals_data.csv")
View(cereals_data)
cereals_data1<-cereals_data
View(cereals_data1)
dim (cereals_data1)
str(cereals_data1)

#---------Data Clening & wrangling---------
#---------Replacing short column names with complete name--------
colnames(cereals_data1) <-c("Name", "Manufacturer", "Type", "Calories", "Protein", "Fat", 
                            "Sodium", "Fibre", "Carbohydrates", "Sugar", "Potassium", 
                            "Vitamins", "Shelf", "Weight", "Cups", "Rating")
variable.names(cereals_data1)
variable.names(cereals_data)
#-----------creating anothe variable Manufacturer_Name------
cereals_data1$Manufacturer_Name <- cereals_data1$Manufacturer
dim(cereals_data1)
variable.names(cereals_data1)
cereals_data1$Manufacturer_Name <- gsub(pattern = "P", replacement = "Post",x = cereals_data1$Manufacturer_Name)
cereals_data1$Manufacturer_Name <- gsub(pattern = "A", replacement = "American Home Food Products",x = cereals_data1$Manufacturer_Name)
cereals_data1$Manufacturer_Name <- gsub(pattern = "G", replacement = "General Mills",x = cereals_data1$Manufacturer_Name)
cereals_data1$Manufacturer_Name <- gsub(pattern = "K", replacement = "Kelloggs",x = cereals_data1$Manufacturer_Name)
cereals_data1$Manufacturer_Name <- gsub(pattern = "N", replacement = "Nabisco",x = cereals_data1$Manufacturer_Name)
cereals_data1$Manufacturer_Name <- gsub(pattern = "Q", replacement = "Quaker Oats",x = cereals_data1$Manufacturer_Name)
cereals_data1$Manufacturer_Name <- gsub(pattern = "R", replacement = "Ralston Purina",x = cereals_data1$Manufacturer_Name)

cereals_data1$Manufacturer_Name
dim(cereals_data1)
#--------Replace H and C in Type with Hot and Cold-------
cereals_data1$Type <- gsub("H", "Hot", x = cereals_data1$Type)
cereals_data1$Type <- gsub("C", "Cold", x = cereals_data1$Type)

#----Removing 1st variable "Names" and alloting to rows------
library(dplyr)
rownames(cereals_data1)<-cereals_data1[ ,1]
str(cereals_data1)
cereals_data2 <-cereals_data1[ ,-1]
View(cereals_data2)
# ------Change cereal type,shelf to factor----
str(cereals_data2)
cereals_data2$Type <- factor(cereals_data2$Type) 
cereals_data2$Shelf <- factor(cereals_data2$Shelf)
#cereals_data2$Manufacturer_Name <- factor(cereals_data2$Manufacturer_Name) 
str(cereals_data2)
sapply(cereals_data2, FUN = class)
#------alternate code FOR FACTOR CONVERSION----
# names <- c("Type","Shelf")
# cereals_data2[, names] <- lapply(cereals_data2[ ,names], factor)
# str(cereals_data2)

#------Evaluating each variable through concepts of sample statistics---
summary(cereals_data2)
describe(cereals_data2$Calories)
describe(cereals_data2$Protein)
describe(cereals_data2$Fat)
describe(cereals_data2$Sodium)
describe(cereals_data2$Fibre)
describe(cereals_data2$Carbohydrates)
describe(cereals_data2$Sugar)
describe(cereals_data2$Potassium)
describe(cereals_data2$Vitamins)

#-------Data Manipulation----------
table(cereals_data2$Calories)
cereals_data2 <- within(cereals_data2, {
  Calories_cat <- NA
  Calories_cat[cereals_data2$Calories <= 100] <-"Low Calories"
  Calories_cat[cereals_data2$Calories > 100 & cereals_data2$Calories < 130] <-"Medium Calories"
  Calories_cat[cereals_data2$Calories >= 130] <-"High Calories"
})
dim(cereals_data2)
table(cereals_data2$Calories_cat)
str(cereals_data2)
cereals_data2$Calories_cat<-factor(cereals_data2$Calories_cat)
str(cereals_data2$Calories_cat)
table(cereals_data2$Calories_cat)
#--------Rounding off Rating-------
cereals_data2$Rating<-round(cereals_data2$Rating,2)
View(cereals_data2)
#----Calculate NAs------
sum(is.na(cereals_data2))
summary(cereals_data2)
#---------raw histogram to check the distribution for imputation---------
par(mfrow=c(1,3))
hist(cereals_data2$Carbohydrates, col = "red")
hist(cereals_data2$Sugar, col= "blue")
hist(cereals_data2$Potassium, col = "green")
#----KNN Imputation-----
#----OR-- USE knnimputation in package DMwR, then we will not get additional columns
library(VIM)
remove.packages("DMwR")
library(DMwR)
cereals_data2<-kNN(cereals_data2, k=5)
rownames(cereals_data2)= cereals_data1$Name
rownames(cereals_data2)
is.na(cereals_data2$Potassium)
is.na(cereals_data2$Carbohydrates)
is.na(cereals_data2$Sugar)
sum(is.na(cereals_data2))
#---------potass=120 (3rd quart/??),95 (cluster mean may be???)-----MV REP
#---------Carbo=14 (cluster mean),sugar=3 (1st quart/??)------MV REP
dim(cereals_data2)
variable.names(cereals_data2)
cereals_data2<-select(cereals_data2,-18:-34) # (cereals_data2,-c(18:34))
dim(cereals_data2)
sum(is.na(cereals_data2))
#----calculate outliers----
#install.packages("outliers")
library(outliers)
#boxplot(cereals_data2$Calories)
boxplot(cereals_data2$Calories)
boxplot(cereals_data2$Protein)
boxplot(cereals_data2$Fat)
boxplot(cereals_data2$Sodium)
boxplot(cereals_data2$Fibre)
boxplot(cereals_data2$Carbohydrates)
boxplot(cereals_data2$Sugar)
boxplot(cereals_data2$Potassium)
boxplot(cereals_data2$Vitamins)

str(cereals_data2$Calories)
scr1 <- scores(cereals_data2$Calories, type = "t")
scr2 <- scores(cereals_data2$Protein, type = "t")
scr4 <- scores(cereals_data2$Sodium, type = "t")
scr5 <- scores(cereals_data2$Fibre, type = "t")
scr8 <- scores(cereals_data2$Potassium, type = "t")
scr9 <- scores(cereals_data2$Vitamins, type = "t")

#----Replace outliers with NAs----
library(ggplot2)
cereals_data2[scr1 > 3| scr1 < -3, "Calories"] <- NA
cereals_data2[scr2 > 3| scr2 < -3, "Protein"] <- NA
#cereals_data2[scr3 > 3| scr3 < -3, "Fat"] <- NA ---no outliers
cereals_data2[scr4 > 3| scr4 < -3, "Sodium"] <- NA
cereals_data2[scr5 > 3| scr5 < -3, "Fibre"] <- NA
#cereals_data2[scr6 > 3| scr6 < -3, "Carbohydrates"] <- NA ---no outliers
#cereals_data2[scr7 > 3| scr7 < -3, "Sugar"] <- NA ---no outliers
cereals_data2[scr8 > 3| scr8 < -3, "Potassium"] <- NA
cereals_data2[scr9 > 3| scr9 < -3, "Vitamins"] <- NA

sum(is.na(cereals_data2))
names(cereals_data2)
class(cereals_data2)

#----Replace NA's for outliers with kNN value----
install.packages("DMwR")
str(cereals_data2)
library(DMwR)
#cereals_dataoutlier<-knnImputation(cereals_data2, k=5)
library(VIM)
cereals_dataoutlier<-kNN(cereals_data2)
cereals_data2<-select(cereals_data2,-18:-34)
sum(is.na(cereals_data2))
sort(colSums(is.na(cereals_data2)))
str(cereals_data2)
#-----Scaling for box plots----
names(cereals_data2)
cereals_datascale1<- subset(cereals_data2, select = c(Calories:Vitamins))
str(cereals_datascale1)
cereals_datascale2<- scale(cereals_datascale1)
head(cereals_datascale2, 10)
#----------Gaurav Plots------
library(ggplot2)
library(gplots)
library(superheat)
library(corrplot)
dev.off()
#-----All nutrients in single box plot---
#use same code above to check 
boxplot(cereals_datascale2) +
  theme(axis.text.y= element_text(angle = 45, 
                                  hjust = 1)) 


ggplot(data = cereals_data2, mapping = aes(x = Rating, y = Calories)) + 
  geom_point(mapping = aes(color = Manufacturer_Name), size = 3) + 
  geom_smooth(method = 'loess')
#------Co-relation plot------
pairs.panels(cereals_datascale2)
dev.off()
par(mfrow = c(1,2))
pairs.panels(cereals_data2[,3:11],
             method = "pearson", #coorelation method
             hist.col = "red",
             main="Correlation of calories and Nutrients",
             density = TRUE, # show density plots
             ellipses = TRUE, # show correlation ellipses
             lm=TRUE #linear regression fits 
) 
#----Pie Charts----
library(readr)
library(ggplot2)
library(gplots)
library(corrplot)
dev.off()
str(cereals_data2)
cereals_data3 <- cereals_data2 %>%
  count(Manufacturer) %>%
  arrange(Manufacturer) %>%
  mutate(prop = round(n * 100 / sum(n), 0),
         lab.ypos = (cumsum(prop) - (0.8*prop)), 
         labl = paste0(prop, "", "%"))
cereals_data3
pie(cereals_data3$n, 
    labels = cereals_data3$labl, 
    main = "Pie Chart for Distribution of Manufacturer",
    col = rainbow(length(cereals_data3$n)))
legend("topright",as.vector(cereals_data3$Manufacturer), cex = 0.8,
       fill = rainbow(length(cereals_data3$n)))
dev.off()

library(plotrix)
pie3D(cereals_data3$n, labels = cereals_data3$labl,
      main="Pie Chart of Manufacturer",
      explode = 0.1)
#NO Manufacturer names and color code

#----BarPlot----
library(ggplot2)
dev.off()
ggplot(cereals_data2, 
       aes(x = Manufacturer_Name, 
           fill = Calories_cat)) + 
  geom_bar(position = "stack")  +
  labs(y = "Count", 
       fill = "Calories",
       x = "Manufacturers",
       title = "Calories by Manufacturer") + 
  theme(axis.text.y= element_text(angle = 15, 
                                  hjust = 1)) +
  coord_flip(expand = F)


ggplot(cereals_data2, 
       aes(x = Manufacturer_Name, 
           fill = Type)) + 
  geom_bar(position = "stack")  +
  labs(y = "Count", 
       fill = "Type",
       x = "Manufacturer",
       title = "Calories by Manufacturer") + 
  theme(axis.text.y= element_text(angle = 15, 
                                  hjust = 1)) +
  coord_flip(expand = F)

#---- Grouped kernel density plots----

ggplot(cereals_data2,
       aes(x = Calories,
           fill = Manufacturer_Name)) +
  geom_density(alpha = 0.6) +
  labs(title = "Calory Distribution Manufacturer wise")

#----Cat vs cat----
str(cereals_data2)
ggplot(cereals_data2, 
       aes(x = Calories_cat, 
           fill = Manufacturer_Name)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion", title = "Calories v/s Manufacturer")
#----Heatmap----Error
library(superheat)
library(gplots)
library(corrplot)

names(cereals_data2)
cereals_datahm<-cereals_data2[ ,1:11]
variable.names(cereals_datahm)
cereals_datahm<-cereals_datahm[, -c(2)]
variable.names(cereals_datahm)
str(cereals_datahm)
rownames(cereals_datahm)<-cereals_datahm[ ,1]
cereals_datahm2<-cereals_datahm[1:38, -1]
#install.packages("superheat")
superheat(cereals_datahm2,scale = TRUE, row.dendrogram = TRUE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,)

cereals_datahm3<-cereals_datahm[39:77,-1]
superheat(cereals_datahm3, scale = TRUE, row.dendrogram = TRUE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,)

dev.off()
ggplot(cereals_data2, mapping = aes(x = Calories_cat, y = Calories )) +
  geom_boxplot()
#------Data Visualisation Neelam-------
cp_impute1<-cereals_data2
ctype<-table(cp_impute1$Type)
ctype
str(ctype)
#install.packages("DMwR")
#describe(cereals_data)
#----------basic pie chart-------"Cereal's count in each type"---
ctype_cat<-c("Cold","Hot")
ctype_count<-c(74,3)
ctype_count_pct<-round(ctype_count/sum(ctype_count)*100)
ctype_count_pct
lbls<-paste(ctype_cat,"= ",ctype_count, ";", ctype_count_pct,"%",sep = " ")
pie(ctype_count,labels=lbls,col = rainbow(length(lbls)),radius=0.85,main = "Cereal's count in each type")
dev.off()
#library(ggplot2)
library(plotrix)
#----------3D--Pie Chart using Plotrix---------
a<-table(cp_impute1$Type)
grp<-c("Cold","Hot")
cnt<-round(a)
grpd<-paste(grp,"=",cnt)
lbl<-paste(grpd,";",ctype_count_pct,"%")

pie3D(a,
      labels = lbl,
      labelcex=0.9,
      main = "Cereals' Count in Each Type",
      col = rainbow(length(a)))
#----Refined Histogram-Distribution: Calories-----
par(mfcol = c(2,2))
dev.off()
x<-cp_impute1$Calories
h<-hist(x,
        breaks=10,
        col="red",
        xlab = "Calories",
        main = "Histogram of Calories")
#-----Normal Curve line in Calories histogram------
xfit<-seq(min(x),max(x),length(40))

yfit<-dnorm(xfit, mean = mean(x), sd=sd(x))
#h$mids are mid points of the intervals
yfit<-yfit * diff(h$mids[1:2]*length(x))
lines(xfit, yfit, col="blue",lwd=3)
dev.off()
#----- Kernel density of calories-------
d<-density(cp_impute1$Calories)
plot(d, main = "Kernel Density of Calories")
polygon(d, col="red", border = "black")

dev.off()
#----Refined Histogram-Distribution:potassium-----
x<-cp_impute1$Potassium
h<-hist(x,
        breaks = 10,
        col="green",
        xlab="Potassium",
        main="Histogram of Potassium with Normal curve")
#-----Normal Curve line in potass histogram------
xfit<-seq(min(x),max(x),length(40))

yfit<-dnorm(xfit, mean = mean(x), sd=sd(x))

yfit<-yfit * diff(h$mids[1:2]*length(x))
lines(xfit, yfit, col="blue",lwd=3)
dev.off()
#------Kernel density of potass-------
d<-density(cp_impute1$Potassium)
plot(d, main = "Kernel Density of Potassium")
polygon(d, col="green", border = "black")
#--------How calories related to carbohydrates,sugar and fat?--------
cp1<-subset(cp_impute1,select=c(Calories,Protein,Fat,Sodium,
              Fibre,Carbohydrates,Sugar,Potassium,Vitamins))
library(psych)
pairs.panels(cp1[,1:5],
             method = "pearson", #coorelation method
             hist.col = "blue",
             main="Scatter Plots,Histogram & Pearson Correlation calories Vs Nutrients",
             density = TRUE, # show density plots
             ellipses = TRUE, # show correlation ellipses
             #lm=TRUE #linear regression fits 
)
pairs.panels(cp1[,c(1,6:9)],
             method = "pearson", #coorelation method
             hist.col = "blue",
             main="Scatter Plots,Histogram & Pearson Correlation calories Vs Nutrients",
             density = TRUE, # show density plots
             ellipses = TRUE, # show correlation ellipses
             #lm=TRUE #linear regression fits 
)            
dev.off()
boxplot(Rating~Manufacturer_Name,
        data = cp_impute1,
        xlab = "Manufacturer",
        ylab = "Ratings",
        main = "Rating vs Manufacturer",
        col = topo.colors(7))

#Error---One name missing---------                               

#------Data Visualisation 2-------

table(cp_impute1$Manufacturer_Name)
table(cp_impute1$Calories_cat)
#rownames(cp_impute1) <- cp_impute1[ ,1] 
cp_impute2 <- cp_impute1[ ,-1]
View(cp_impute2)
str(cp_impute2)

ggplot(cp_impute2, 
       aes(x = Manufacturer_Name, 
           fill = Calories_cat)) + 
  geom_bar(position = "stack")  +
  labs(y = "Count", 
       fill = "Calories",
       x = "Manufacturers",
       title = "Calories by Manufacturers")+
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))


ggplot(cp_impute2, 
       aes(x = Manufacturer_Name, 
           fill = Type)) + 
  geom_bar(position = "stack")  +
  labs(y = "Count", 
       fill = "Type",
       x = "Manufacturers",
       title = "Calories by Manufacturer")+
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

dev.off()

#install.packages("GGally")
library(GGally)
#---------Browsing the data set-----------
library(DT)
datatable(data = cp_impute2, 
          rownames = FALSE, 
          filter = "top",
          options = list(autoWidth = TRUE))


#----------Add nutritionals per ounce----------
cp_impute6<-cp_impute2
cp_impute6$Calories_oz <- cp_impute6$Calories * cp_impute6$Weight
cp_impute6$Protein_oz <- cp_impute6$Protein * cp_impute6$Weight
cp_impute6$Carbohydrates_oz <- cp_impute6$Carbohydrates * cp_impute6$Weight
cp_impute6$Fat_oz <- cp_impute6$Fat * cp_impute6$Weight
View(cp_impute6)
# ----------Add nutritionals per 100g---------
# 1 oz. = 28.3495g
# 100g = 3.5274 oz.
cp_impute6$Calories_100g <- round(cp_impute6$Calories_oz * 3.5274, 0)
cp_impute6$Protein_100g <- round(cp_impute6$Protein_oz * 3.5274, 1)
cp_impute6$Carbohydrates_100g <- round(cp_impute6$Carbohydrates_oz * 3.5274, 1)
cp_impute6$Fat_100g <- round(cp_impute6$Fat_oz * 3.5274, 1)
View(cp_impute6)
dev.off()

#------Correlation Matrix
cp_impute6%>% 
  select(Calories =Calories_100g , Protein, Fat, Sodium, Fibre,Carbohydrates, Sugar, Potassium, Shelf, Rating) %>% 
  ggcorr(palette = "RdBu", label = TRUE, label_round =  2)


#--------Distribution of Calorie Content------
# Protein: 4 kcal/g
# Carbohydrates: 4 kcal/g
# Fat: 9 kcal/g
# Calories = 9 * Fat + 4 * Protein + 4 * Carbohydrates
cp_impute6$Calories_100g_calculated <- cp_impute6$Fat_100g * 9 + cp_impute6$Protein_100g * 4 + cp_impute6$Carbohydrates_100g * 4 
cp_impute6 %>% 
  ggplot(aes(x = Manufacturer_Name, y = Calories_100g_calculated, fill = Manufacturer_Name)) +
  geom_boxplot(show.legend = FALSE) +
  stat_summary(fun.y = mean, geom = "point", pch = 1, show.legend = FALSE) + # Add average to the boxplot
  scale_y_continuous(name = "Calories (g per 100g)", minor_breaks = NULL) +
  scale_fill_brewer(palette = "Set1") +
  coord_flip() + 
  theme_minimal() +
  labs(x = "Manufacturer") +
  ggtitle(label = "Distribution of Calorie Content by Manufacturer")


