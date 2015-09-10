m <- read.csv("outpatient.csv")
hosp <- read.csv("Hospital_General_Information.csv")
hospid <- as.character(hosp$Provider.ID)

location <- apply(m,1,function(x) {
  y <- gsub(" ","",x[1])
  y <- paste0(rep("0",6-nchar(y)),y)
  ind <- which(hospid == y)
  as.character(hosp[ind,]$Location)[1]
})

rgx <- function(pattern,x,b=0,e=0) {
  g <- regexpr(pattern,x)
  start <- g[1]
  end <- start + attr(g,"match.length") - 1
  substr(x,start+b,end-e)
}

lat <- NULL
lon <- NULL
for (i in 1:length(location)) {
  lat[i] <- rgx("\\(-*\\d+\\.\\d+,",location[i],b=1,e=1)
  lon[i] <- rgx("-*\\d+\\.\\d+\\)",location[i],e=1)
}

out <- cbind(m,lat,lon)
head(out)

write.table(out,"outpatient_vol.csv",quote=F,row.names=F,sep="|")
system("cp outpatient.csv ~/luiarthur.github.io/assets/Hospital_Outpatient")


prop <- cbind(m[,4:10],
             as.numeric(as.character(lat)),
             as.numeric(as.character(lon)))

for (i in 1:nrow(prop)) {
  r <- prop[i,1:7]
  r <- unlist(ifelse(is.na(r),0,r))
  if (sum(r) > 0) {
    r <- r / sum(r)
  } 
  prop[i,1:7] <- r
}

prop <- prop[which(apply(prop[,1:7],1,sum)>0),]
colnames(prop)[8:9] <- c("lat","lon")
apply(prop[,1:7],2,quantile)

write.csv(prop,"prop.csv",quote=F,row.names=F)
system("cp prop.csv ~/luiarthur.github.io/assets/Hospital_Outpatient")

