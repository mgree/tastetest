library("directlabels")
library("ggplot2")

# load top-level data
whiskies <- read.table("whiskies.txt",header=TRUE,sep=",",quote="\"")
tasters <- data.frame(read.table("tasters.txt",header=TRUE,sep=","))

fresh = TRUE

cleanName <- function(file) {
  file <- basename(file)
  return(substr(file,0,nchar(file)-nchar(".txt")))
}

normalize <- function(v,from=0,to=1) {
  high <- max(v, na.rm=TRUE)
  low <- min(v, na.rm=TRUE)
  ui <- (v - low)/(high - low)
  (ui * (to - from)) + from
}

loadRating <- function(file) {
  r <- data.frame(read.table(file,header=TRUE,sep=",",quote="\""))
  row.names(r) <- whiskies$Name
  
  # normalize and aggregate
  r <- lapply(r[,1:4],normalize,from=1,to=5)
  r$Whiskey <- whiskies$Name
  r$Proof <- whiskies$Proof

  name <- cleanName(file)
  r$Taster <- name
  r$Gender <- subset(tasters,Name==name)$Gender
  data.frame(r)
}

# load individual ratings
rawRatings <- list.files("ratings",pattern="*.txt",full.names=TRUE)
ratings <- data.frame(do.call("rbind",lapply(rawRatings,loadRating)))

drawPlot <- function (p,out,width=800,height=600) {
  if (!file.exists(out) | fresh) {
    png(out,width=width,height=height)
    plot(p)
    dev.off()
  }
}

# overall
drawPlot(ggplot(ratings) +
         geom_boxplot(aes(Whiskey,Overall),outlier.size=0) +
         geom_text(aes(x=Whiskey,y=Overall,colour=Taster,label=Taster),
                     size=4, position=position_jitter(width=.2,height=.05)) +
         guides(colour=FALSE) + ggtitle("Overall ratings"),
         "reports/overall.png",width=2000,height=500)
# overall by gender
drawPlot(ggplot(ratings) +
         geom_boxplot(aes(Whiskey,Overall,fill=Gender),outlier.size=0) +
         geom_text(aes(x=Whiskey,y=Overall,colour=Taster,label=Taster),
                     size=4, position=position_jitter(width=.2,height=.05)) +
         guides(colour=FALSE) + ggtitle("Overall ratings by gender"),
         "reports/overall_gender.png",width=2000,height=500)

# mellowness vs. proof
drawPlot(ggplot(ratings,aes(x=Proof,y=Mellowness,colour=Taster)) +
         geom_boxplot(aes(group=Proof),outlier.size=0) +
         geom_text(aes(label=Taster),
                   alpha=.3,position=position_jitter(width=.2,height=.05),size=4) +
         geom_smooth(aes(group=1,colour="red"),na.rm=TRUE,method="loess") +
         geom_smooth(aes(group=1,colour="blue"),na.rm=TRUE,method="lm") +
         guides(colour=FALSE) + ggtitle("Mellowness vs. proof"),
         "reports/mellowness_proof.png")
drawPlot(ggplot(ratings,aes(x=reorder(Whiskey,Proof),y=Mellowness)) +
         geom_violin(aes(fill=Proof)) +
         geom_text(aes(label=Taster,colour=Taster),
                   position=position_jitter(),size=4) +
         xlab("Whiskey") + guides(colour=FALSE) + ggtitle("Mellowness vs. proof, by whiskey"),
         "reports/mellowness_proof_violin.png",width=1500)
