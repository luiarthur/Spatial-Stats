m <- read.csv("outpatient.csv")
hosp <- read.csv("Hospital_General_Information.csv")
hospid <- as.character(hosp$Provider.ID)

location <- apply(m,1,function(x) {
  y <- gsub(" ","",x[1])
  y <- paste0(rep("0",6-nchar(y)),y)
  ind <- which(hospid == y)
  as.character(hosp[ind,]$Location)[1]
})

rgx <- function(pattern,x) {
  g <- regexpr(pattern,x)
  start <- g[1]
  end <- start + attr(g,"match.length") - 1
  substr(x,start+1,end-1)
}

# Missing Hospitals
miss <- m[which(is.na(location)),]

lat <- NULL
lon <- NULL
for (i in 1:length(location)) {
  lat[i] <- rgx("\\(-*\\d+\\.\\d+,",location[i])
  lon[i] <- rgx("-*\\d+\\.\\d+\\)",location[i])
}

out <- cbind(m,lat,lon)
head(out)

write.csv(out,"outpatient_vol.csv",quote=F,row.names=F)
