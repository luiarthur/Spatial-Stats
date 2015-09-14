x <- read.csv("chr2015.csv")

n <- nrow(x)
state <- x$STATECODE
county <- as.character(x$COUNTYCODE)
fips <- NULL
#ifelse(nchar(county < 3), paste0(rep("0", 3-nchar(county))), county)

for (i in 1:n) {
  y <- nchar(county[i])
  if (y < 3) {
    county[i] <- paste0(paste0(rep("0",3-y),collapse=""),county[i])
  }
  fips[i] <- paste0(state[i],county[i])
}

out <- cbind(x,fips)
write.csv(out,"chr2015fips.csv",quote=F,row.names=F)
