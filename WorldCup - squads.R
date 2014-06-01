library(xlsx)
library(XML)
library(plyr)
library(stringr)
library(calibrate)

##==================================LINKS TO WEB PAGES==============================================

##history of top 4 places from all World Cups
wc_hist_f <- "http://en.wikipedia.org/wiki/FIFA_World_Cup"

#all World Cup years and their Wiki webpages
wcyears <- c(1930,1934,1938,1950,1954,1958,1962,1966,1970,1974,1978,1982,1986,1990,1994,1998,2002,2006,2010,2014)
for(i in 1:length(wcyears)) {
        assign(paste("squads", wcyears[i], sep = ""), 
			   gsub("2014",as.character(wcyears[i]),"http://en.wikipedia.org/wiki/2014_FIFA_World_Cup_squads"))
}
remove(i)

#ISO codes countries, country names, continents
country_codes <- "http://www.geonames.org/countries/"

##==================================READING DATA=====================================================
setwd("GitHub\\World_Cup")

##Make data frame of HTML table of World Cup squad
makesquads <- function(x,nrteams) {
        squads <- readHTMLTable(x, header=T, which=seq(2,nrteams*2,2),stringsAsFactors=F) 
        ##get squad country names
        html <- htmlTreeParse(x,useInternalNodes=T)
        htmlCtry <- xpathSApply(html,"//h3",xmlValue)
        names(squads) <- htmlCtry[1:nrteams]
        df <- ldply(squads, data.frame)
        df$worldcup <- as.numeric(substr(deparse(substitute(x)),7,10))
        remove(x)
        return(df)
}

squadsWC1930 <- makesquads(squads1930,13);squadsWC1934 <- makesquads(squads1934,17);squadsWC1938 <- makesquads(squads1938,15);
squadsWC1950 <- makesquads(squads1950,13);squadsWC1954 <- makesquads(squads1954,16);squadsWC1958 <- makesquads(squads1958,16);
squadsWC1962 <- makesquads(squads1962,16);squadsWC1966 <- makesquads(squads1966,16);
squadsWC1970 <- makesquads(squads1970,16);squadsWC1974 <- makesquads(squads1974,16);squadsWC1978 <- makesquads(squads1978,16);
squadsWC1982 <- makesquads(squads1982,24);squadsWC1986 <- makesquads(squads1986,24);
squadsWC1990 <- makesquads(squads1990,24);squadsWC1994 <- makesquads(squads1994,24);squadsWC1998 <- makesquads(squads1998,32);
squadsWC2002 <- makesquads(squads2002,32);squadsWC2006 <- makesquads(squads2006,32);
squadsWC2010 <- makesquads(squads2010,32);squadsWC2014 <- makesquads(squads2014,32);

##correct id for 1934 and 1938
squadsWC1934$.id <- c(rep("Italy",22),rep("Czechoslovakia",22),rep("Germany",22),rep("Austria",22),rep("Spain",22),rep("Hungary",22),
                      rep("Switzerland",22),rep("Sweden",22),rep("France",22),rep("Netherlands",22),rep("Argentina",18),rep("Romania",22),
                      rep("Egypt",20),rep("Brazil",20),rep("Belgium",22),rep("United States",19))

squadsWC1938$.id <- c(rep("Italy",22),rep("Hungary",22),rep("Brazil",22),rep("Sweden",22),rep("Czechoslovakia",22),rep("Switzerland",22),
                      rep("Cuba",15),rep("France",22),rep("Romania",22),rep("Germany",22),rep("Poland",22),rep("Norway",22),
                      rep("Belgium",22),rep("Netherlands",22),rep("Dutch East Indies",17))

##Combine all data frames into one					  
squads_total <- rbind(squadsWC1930,squadsWC1934,squadsWC1938,squadsWC1950,squadsWC1954,squadsWC1958,
                      squadsWC1962,squadsWC1966,squadsWC1970,squadsWC1974,squadsWC1978,squadsWC1982,
                      squadsWC1986,squadsWC1990,squadsWC1994,squadsWC1998,squadsWC2002,squadsWC2006,
                      squadsWC2010,squadsWC2014)
remove(squadsWC1930,squadsWC1934,squadsWC1938,squadsWC1950,squadsWC1954,squadsWC1958,
      squadsWC1962,squadsWC1966,squadsWC1970,squadsWC1974,squadsWC1978,squadsWC1982,
      squadsWC1986,squadsWC1990,squadsWC1994,squadsWC1998,squadsWC2002,squadsWC2006,
      squadsWC2010,squadsWC2014)

##Tidy up data frame
squads_total$.id <- str_trim(squads_total$.id)
squads_total$.id <- gsub("[edit]","",squads_total$.id,fixed=TRUE)
table(squads_total$.id,squads_total$worldcup)
names(squads_total) <- tolower(names(squads_total))
summary(squads_total)

##Extract age from dob.age column
reg_exp <- regexec("age(d?) (\\d+?)\\)",squads_total$dob.age)
match <- regmatches(squads_total$dob.age, reg_exp)
squads_total$ages <- sapply(match, function(x) as.numeric(x[3]))
remove(reg_exp,match)

#Fill out some missing ages where yob is given in dob.age
sel_agefill <- as.numeric(rownames(squads_total[is.na(squads_total$ages) & nchar(squads_total$dob.age)==4,]))
squads_total$ages[sel_agefill] <- squads_total$worldcup[sel_agefill] - as.numeric(squads_total$dob.age[sel_agefill])
remove(sel_agefill)
#Add some ages from 2014
squads_total$ages[squads_total$worldcup==2014 & squads_total$player == "Leroy Fer"] <-  24
squads_total$ages[squads_total$worldcup==2014 & squads_total$player == "Orestis Karnezis"] <-  28
squads_total$ages[squads_total$worldcup==2014 & squads_total$player == "Oswaldo Minda"] <-  30
squads_total$ages[squads_total$worldcup==2014 & grepl("Stéphane Ruffier",squads_total$player,fixed=TRUE) == TRUE] <-  27
squads_total$ages[squads_total$worldcup==2014 & squads_total$player == "Alireza Beiranvand"] <-  21
squads_total$ages[squads_total$worldcup==2014 & squads_total$player == "Reza Norouzi"] <-  31
squads_total$ages[squads_total$worldcup==2014 & squads_total$player == "Simon Mignolet"] <-  25
squads_total$ages[squads_total$worldcup==2014 & squads_total$player == "Koen Casteels"] <-  21
squads_total$ages[squads_total$worldcup==2014 & squads_total$player == "Javier Aquino"] <-  24

head(squads_total)
tail(squads_total)
summary(squads_total$ages)
mean(is.na(squads_total$ages))
squads_total[is.na(squads_total$ages),c(1,4,5,8,9)]

max(squads_total$worldcup[is.na(squads_total$ages)])
min(squads_total$ages,na.rm=TRUE)
mean(squads_total$ages,na.rm=TRUE)
max(squads_total$ages,na.rm=TRUE)

##Get 2014 squads
squads2014 <- squads_total[squads_total$worldcup==2014,]
groups2014 <- data.frame(cbind(country=unique(squads2014$.id),group=c(rep("Group A",4),rep("Group B",4),rep("Group C",4),rep("Group D",4),
                                              rep("Group E",4),rep("Group F",4),rep("Group G",4),rep("Group H",4))))
squads2014tot <- merge(squads2014,groups2014,by.x=".id",by.y="country")
##Number of players in squad
table(squads2014tot$.id)

##Calculate average age per country
avg_age2014 <- aggregate(ages ~ worldcup + .id + group, squads2014tot, function(x) mean = mean(x))
avg_age2014 <- arrange(avg_age2014,avg_age2014$group)
avg_age2014$id <- 1:32
avg_age2014$group <- as.factor(avg_age2014$group)

##Get World Cup History
wc_hist_full <- readHTMLTable(wc_hist_f, header=T, which=5,stringsAsFactors=F) 
names(wc_hist_full) <- c("year","host","winner","score_f","runnerup","third","score","fourth","nrteams")
wc_hist_full$year <- as.numeric(gsub("Details","",wc_hist_full$year))
##Get World Cup winners
wc_hist_winner <- wc_hist_full[,c("year","winner")]
squads_winners <- merge(squads_total,wc_hist_winner,by.x=c(".id","worldcup"),by.y=c("winner","year"))
squads_winners <- arrange(squads_winners,squads_winners$worldcup)

##Table to check
table(squads_winners$.id,squads_winners$worldcup)

##average age of World Cup winners
avgagewin <- aggregate(ages ~ worldcup + .id, squads_winners, function(x) mean = mean(x))
avgagewin <- arrange(avgagewin,desc(avgagewin$ages))
avgagewin$lbl <- paste(avgagewin$.id," (",avgagewin$worldcup,")",sep= "")

##Determine the mean age of the World Cup winners
mean(squads_winners$ages,na.rm=TRUE)

##================================MAKE PLOT=====================================================
        
ylow <- min(avg_age2014$ages,avgagewin$ages)
yhigh <- max(avg_age2014$ages,avgagewin$ages)
##par()

##png(file="WorldCupSquads.png", width=1280, height=480, units="px")

par(mfrow = c(1, 2),     # 1x2 layout
    oma = c(0, 2, 0, 0), # two rows of text at the outer left margin
    mar = c(1, 1, 2, 1), # more space for title
    mgp = c(2, 1, 0),    # axis label at 2 rows distance, tick labels at 1 row
    xpd = NA)            # allow content to protrude into outer margin (and beyond)

plot(avgagewin$worldcup,avgagewin$ages,
     main="World Cup winners",
     xaxt="n",
     xlab="",
     ylab="Average player age",
     pch=19,
     cex.axis=0.8,
     cex.main=1.1,
     xlim=c(1930,2025),
     ylim=c(ylow,yhigh))
textxy(avgagewin$worldcup,avgagewin$ages,avgagewin$lbl)
par(xpd=FALSE)
abline(h=mean(squads_winners$ages), col="gray", lwd=3, lty=3)
text(1940, mean(squads_winners$ages) + 0.1, "Overall winner average", col = "gray", cex = 0.5)

par(xpd=NA)
plot(avg_age2014$id,avg_age2014$ages,
     main="World Cup 2014 participant",
     xaxt="n",
     xlab="",
     yaxt="n",
     ylab="",
     pch=19,
     cex.axis=0.8,
     cex.main=1.1,     
     col=avg_age2014$group,
     xlim=c(0,38),
     ylim=c(ylow,yhigh))
textxy(avg_age2014$id,avg_age2014$ages,avg_age2014$.id)
par(xpd=FALSE)
abline(h=mean(squads_winners$ages), col="gray", lwd=3, lty=3)

##close PNG device
##dev.off()
