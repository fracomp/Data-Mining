#The xlsx file has been traslated in csv for an easier reading
coles_raw <-read.csv("ColesData.csv",sep=";",header = TRUE)
df<-na.omit(coles_raw)
#ReceiptId
#check if ReceiptId contains duplication
n_duplication<-nrow(df[duplicated(df$IReceiptID),])
new_index<-seq(max(df$IReceiptID)+1,max(df$IReceiptID)+n_duplication)
#assign new_index to the duplicate value of ReceiptId
df[duplicated(df$IReceiptID),1]<-new_index

#Value
#Transforminf the value from factor to integer and removing commas
df$Value <-as.numeric(sub("," , "." , as.character(df$Value),fixed=TRUE))
summary(df$Value)#no missing values have been found
boxplot(df$Value,main="value in transaction")
df$Value[(df$Value > 500)]

#pmethod
summary(df$pmethod)
#lets check if there is any method value out of the range (1,4)
df$pmethod[(df$pmethod<1 | df$pmethod>4)]
table(df[df$pmethod <1 | df$pmethod>4,3])
#I substitute the 98 incorrect vaues with the mode
df$pmethod[df$pmethod <1 | df$pmethod>4]<-2
#sex
#translate sex into categorical value
df$sex <-sub("1","M",as.character(df$sex))
df$sex <-sub("2","F",as.character(df$sex))

#homeown
summary(df$homeown) #no missing value has been found
table(df$homeown)#99 values have been found with a value higher than 3
df$homeown[df$homeown <1 | df$homeown>3] <-1 #impute with mode

#income
#tranlate income into a numeric variable.
df$income <-as.numeric(sub("," , "." , as.character(df$income),fixed=TRUE))
summary(df$income)#1 missing value found and then replace with the median
df[which(is.na(df$income)),'income']<-70169
boxplot(df$income,main="Income of customer per year ($)")

#age
#tranlate age into a numeric variable.
df$age <-as.integer(sub("," , "." , as.character(df$age),fixed=TRUE))
summary(df$age)#1 missing value has been found and imputated with group median
df[which(is.na(df$age)),]#the observation is female
#calculate the female median 38
fmedian<-tapply(df$age,df$sex,median,na.rm=TRUE)
df[which(is.na(df$age)),'age'] <-fmedian[1]
#Postcode
df$PostCode<-as.numeric(as.character(df$PostCode))
summary(df$PostCode)
df$PostCode[df$PostCode<2000 |df$PostCode >2914] <-NA
#vadid postcode
Valid_Postcode<-df$PostCode[df$PostCode>=2000 & df$PostCode <=2914]
max(Valid_Postcode)
hist(df$PostCode)

#nchildren
table(df$nchildren)
#suspicious observations
df[df$nchildren>=12 & df$nchildren <=14,]
df$nchildren[df$nchildren>=14]<-median(df$nchildren)
#167 observations have number of kids >0 even if they are younger than 14
nrow((df[df$age >=0 & df$age <=14 & df$nchildren>0,]))
#those observations have been imputated with 0
df$nchildren[df$age >=0 & df$age <=14 & df$nchildren>0] <-0


#products data preprocessing(only those who I did important change)
#fruit
summary(df$fruit)
df$fruit[df$fruit=='o']<-'0'
df$fruit<-as.numeric(as.character(df$fruit))
df$fruit[df$fruit>=2]<-1


#fruitjuice
summary(df$fruitjuice)
df$fruitjuice[df$fruitjuice>=2]<-1



hist(df$PostCode)

#For clustering purposes I standardize the variable age,value and income
df$Z_age<-((df$age-mean(df$age))/sd(df$age))
df$Z_Value<-((df$Value-mean(df$Value))/sd(df$Value))
df$Z_income<-((df$income-mean(df$income))/sd(df$income))
df$Z_nchildren<-((df$nchildren-mean(df$nchildren))/sd(df$nchildren))
df$Z_homeown<-((df$homeown-mean(df$homeown))/sd(df$homeown))

#This will create the cleaned file to give as input in SPSS MODELER
write.csv(df, "filenameNewZZZZ.csv")
