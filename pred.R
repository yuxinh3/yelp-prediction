
#setwd("~/")
install.packages("stringr")
install.packages("glmnet")
install.packages("caret")
library(stringr)
library(glmnet)

ytable = read.csv("Yelp_train.csv")
yelp_test <- read.csv("Yelp_test.csv")
yelp_validate <- read.csv("Yelp_validate.csv")
yelp_out <- rbind(yelp_test,yelp_validate)

words = read.csv("333 words.csv", header = F)
poswords = tolower(words[,1])
poswords[1] = "thanks"###############
negwords = tolower(words[,2])
pn = unique(c(poswords,negwords))

#ytable = ytable[,-1]

tphrase = as.character(read.csv("8.csv", header = F)[,1])
tword = as.character(read.csv("115.csv", header = F)[,1])
uniw = unique(c(tword, pn))
uniph = unique(c(tphrase))
pnsvector = ytable$text
pnsvectorout = yelp_out$text

##functions
splitfunction = function(svector){# this fuction finds positive and negative words in review and store them in a list
  biglist = vector(mode = "list",length = length(svector))
  for(i in 1:length(svector)){#svector contains all the reviews from the table
    stri= svector[i] #loop through each review 
    stri = tolower(stri)
    x = gsub("[[:punct:]]", "", stri)#remove punctuations
    x = gsub("[\r\n]", " ", x)#remove new line character \n
    spliti = strsplit(x, "\\s+") #split string into words

    p = spliti[[1]] %in% pn #true false vector of positive words in review
    
    biglist[[i]] = c(unique(spliti[[1]][p]))#store them in a list and remove duplicates
  }
  return(biglist)
}
# pnbiglist = splitfunction(pnsvector) #list that contains list of positive words that appears in review and negative words that appears in review
# pnbiglistout = splitfunction(pnsvectorout)
#pn =  c(as.character(poswords),as.character(negwords))#combind positive and negative words => this will be columns for table

countpn = function(svector, pn){
  df = data.frame(pn)
  biglist = splitfunction(svector) #list that contains list of positive words that appears in review and negative words that appears in review
  for(i in 1:length(biglist)){
    
    str = biglist[[i]]#words in the ith review
    binary = as.numeric(pn %in% str)#1 if contains words in pn 0 otherwise
    df= cbind(df, binary)#
    
  }
  df = df[,-1] #get rid of pos & neg words
  df = t(df) #tranpose the dataframe
  colnames(df) = pn #name columns
  row.names(df) = seq(1:length(svector)) #name reviews
  
  return(df)
}
pnmocktable = countpn(pnsvector, pn)
pnmocktableout = countpn(pnsvectorout, pn)

combine = cbind(pnmocktable, ytable[-c(1:14)])
combineout = cbind(pnmocktableout,yelp_out[-c(1:13)])

##word predictors all
combine = combine[,unique(colnames(combine))]
combineout = combineout[,unique(colnames(combineout))]


stars = ytable$stars
ytable = ytable[,c(1:14)]
ytable = ytable[,-3]
yelp_out = yelp_out[,c(1:13)]

ytable = cbind(ytable, combine)
yelp_out = cbind(yelp_out, combineout)
#yelp_out = yelp_out[,-c(1,3)]


country = c("Italian","American (Traditional)", "American (New)", 
            "Greek", "Thai","French", "Belgian","Chinese","Japanese", "Korean",
            "Mexican", "Vietnamese", "Turkish","Irish", "Filipino", "Malaysian","Indonesian"
            ,"Indian", "Brazilian","Russian", "Cantonese", "African", "Latin", "German")
svector = str_split(as.character(ytable$categories), ", ")
svectorout = str_split(as.character(yelp_out$categories), ", ")
dvector = as.Date(ytable$date)
dvectorout = as.Date(yelp_out$date)

###functions
splitfunction = function(svector, country){
  biglist = data.frame()
  #biglist= rbind(country)
  for(i in 1:length(svector)){#svector contains all the reviews from the table
    spliti= svector[[i]] #loop through each review 
    
    
    tf = as.numeric(factor(country %in% svector[[i]], levels= c("TRUE", "FALSE"), labels = c(1, 0))) 
    tf[tf== 2] = 0
    
    biglist = rbind(biglist, tf)
  }
  return(biglist)
}


grabD = function(dvector){
  mv = c()
  yv = c()
  dv = c()
  wv = c()
  sv = c()
  for(i in 1:length(dvector)){
    myDate = as.POSIXct(dvector[i])
    M = format(myDate,"%B") #month string
    Y = format(myDate,"%Y") #year
    d = format(myDate,"%d") #date
    m = format(myDate,"%m") #month number
    weekday = strftime(myDate,'%A') #weekday
    
    mv = c(mv, M)
    yv = c(yv, Y)
    dv = c(dv, d)
    wv = c(wv, weekday)
    sv = c(sv, detseason(m))
  }
  l = data.frame(mv, yv, dv, wv,sv)
  return(l)
}
detseason = function(tf){
  spring = c(3,4,5)
  summer = c(6,7,8)
  fall = c(9,10,11)
  winter = c(12,1,2)
  
  if(tf %in% spring){
    season = "spring"
  }else if(tf %in% summer){
    season = "summer"
  }else if(tf %in% fall){
    season = "fall"
  }else{
    season = "winter"
  }
  return(season)
}
splitfunction_cap = function(svector){# this fuction finds positive and negative words in review and store them in a list
  biglist = vector(mode = "list",length = length(svector))
  df = data.frame(1:nrow(ytable))
  capempty = c()
  for(i in 1:length(svector)){#svector contains all the reviews from the table
    
    
    stri= svector[i] #loop through each review 
    x = gsub("[[:punct:]]", "", stri)#remove punctuations
    x = gsub("[\r\n]", " ", x)#remove new line character \n
    spliti = strsplit(x, "\\s+") #split string into words
    ###
    capcount = str_count(spliti[[1]], "\\b[A-Z]{2,}\\b")
    cap =  spliti[[1]][capcount >= 1]
    cap = cap[nchar(cap)>=4]
    if(length(cap)>0){
      capempty = c(capempty,length(cap))##
    } else{
      capempty = c(capempty,0)
    }
    ####
    spliti[[1]] = tolower(spliti[[1]])
  }
  return(capempty)
}
splitfunction_words = function(svector, uniw){# this fuction finds positive and negative words in review and store them in a list
  biglist = vector(mode = "list",length = length(svector))
  for(i in 1:length(svector)){#svector contains all the reviews from the table
    stri= svector[i] #loop through each review 
    stri = tolower(stri)
    x = gsub("[[:punct:]]", "", stri)#remove punctuations
    x = gsub("[\r\n]", " ", x)#remove new line character \n
    spliti = strsplit(x, "\\s+") #split string into words
    
    p = spliti[[1]] %in% uniw #true false vector of positive words in review
    
    biglist[[i]] = c(unique(spliti[[1]][p]))#store them in a list and remove duplicates
  }
  return(biglist)
}

phrasesplit = function(d,phrase){
  df = data.frame()
  for(i in 1:length(d)){
    sc = vector(mode = "numeric", length = length(phrase))
    s= d[i]
    for (j in 1:length(phrase)){
      v = str_count(s, regex(phrase[j], ignore_case=T)) # ignore the upper/lower case in the text
      sc[j] = v
    }
    df = rbind(df, sc)
  }
  names(df) = phrase
  return (df)
}
## two dataframes: country and Date
countrydf = splitfunction(svector, country)
countrydfout = splitfunction(svectorout, country)
names(countrydf) = country
names(countrydfout) = country

datedf = grabD(dvector)
datedfout = grabD(dvectorout)

names(datedf) = c("Month", "Year", "date", "Week day", "Season")
names(datedfout) = c("Month", "Year", "date", "Week day", "Season")

ytable = cbind(ytable, data.frame(countrydf, datedf)) # combine all together
yelp_out = cbind(yelp_out, data.frame(countrydfout, datedfout))

ytable2 = ytable[,-c(1,2,3,4,5,13)]
yelp_out2 = yelp_out[,-c(1,2,3,4,5,13)]
ytable2 = cbind(stars, ytable2)

yt = ytable2
yo = yelp_out2

cap = splitfunction_cap(pnsvector)
yt = cbind(yt, cap)
cap = splitfunction_cap(pnsvectorout)
yo = cbind(yo, cap)

yt = yt[,-296]
yo = yo[,-295]



write.csv(yt,"newyt.csv")
write.csv(yo,"newyo.csv")



yt = read.csv("newyt.csv")
yo = read.csv("newyo.csv")
yt = yt [,-1]
yo = yo [,-1]



worddf = countpn(svector,uniw)
worddfout = countpn(pnsvectorout,uniw)

write.csv(worddf, "ytableworddf.csv")
write.csv(worddfout, "yelp_outwordddf.csv")

phrasedf = phrasesplit(pnsvector, uniph)
write.csv(phrasedf, "phrasedf.csv")

phrasedfout = phrasesplit(pnsvectorout, uniph)
write.csv(worddfout, "phrasedfout.csv")

ytworddf = read.csv("ytableworddf.csv", header = F)
ytworddf = ytworddf[,-1]
names(ytworddf) = uniw
ytworddf =ytworddf[,-117]

ytphdf = read.csv("phrasedf.csv", header = T)
ytphdf = ytphdf[,-1]

####yelpout
yoworddf = read.csv("yelp_outwordddf.csv", header = F)
yoworddf = yoworddf[,-1]
names(yoworddf) = uniw
yoworddf =yoworddf[,-117]

yophdf = read.csv("phrasedfout.csv", header = T)
yophdf = yophdf[,-1]

yt = cbind(yt, cbind(ytworddf, ytphdf))
yt = yt[,-c(which(colnames(yt) == "Year"))]
s = unique(colnames(yt))
yt = yt[,s]
###Yelp_out
yo = cbind(yo, cbind(yoworddf, yophdf))
yo = yo[,-c(which(colnames(yo) == "Year"))]
s = unique(colnames(yo))
yo = yo[,s]
names(yo)[which(names(yo) == "stars")] = "stars.1"


library(caret)
set.seed(2)
train_control = trainControl(method = "cv", number = 10, verboseIter = TRUE)
modelcv = train(stars~., data = yt, trControl = train_control, method = "lm")
summary(modelcv)

pred = predict(modelcv, newdata=data.frame(yo))
star_out <- data.frame(Id=yelp_out$Id, Expected=pred)

for(i in 1:nrow(star_out)){
  if(star_out$Expected[i] > 5){
    star_out$Expected[i] = 5
  }else if(star_out$Expected[i] <1){
    star_out$Expected[i] = 1
  }
}
write.csv(star_out, file='832submission.csv', row.names=FALSE)
