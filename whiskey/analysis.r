library("directlabels")
library("ggplot2")
library("reshape2")
library("xtable")

# load top-level data
whiskies <- read.table("whiskies.txt",header=TRUE,sep=",",quote="\"")
tasters <- data.frame(read.table("tasters.txt",header=TRUE,sep=","))

# report config
reports = "reports"
fresh = FALSE

cleanName <- function(file) {
  file <- basename(file)
  return(substr(file,0,nchar(file)-nchar(".txt")))
}

normalize <- function(v,from=0,to=1) {
  high <- max(v, na.rm=TRUE)
  low <- min(v, na.rm=TRUE)

  ui <- (v - low)/max(high-low,1)
  (ui * (to - from)) + from
}

sink(file.path(reports,"prefs.html"))
cat("<html><head><title>Whiskey Preferences</title></head><body>")

loadRating <- function(file) {
  r <- data.frame(read.table(file,header=TRUE,sep=",",quote="\""))
  row.names(r) <- whiskies$Name

  name <- cleanName(file)
  
  # generate individual report in the prefs file
  df <- subset(r,!(is.na(Overall) &
                   is.na(Sweetness) &
                   is.na(Mellowness) &
                   is.na(Cost) &
                   nchar(as.character(Notes)) == 0),
               select=c(Overall,Sweetness,Mellowness,Cost,Notes))
  t <- xtable(df[order(-df$Overall),])
  cat(paste("<h1>",name,"</h1>"))
  print(t,type="html")
  
  # normalize and aggregate
  r <- lapply(r[,1:4],normalize,from=1,to=5)
  r$Whiskey <- whiskies$Name
  r$Proof <- whiskies$Proof
  r$RealCost <- whiskies$Cost
  
  # kludge fix up
  if (name == "justin_retaste") {
    name = "justin"
  }
  
  r$Taster <- name
  r$Gender <- subset(tasters,Name==name)$Gender
  data.frame(r)
}

# load individual ratings
rawRatings <- list.files("ratings",pattern="*.txt",full.names=TRUE)
ratings <- data.frame(do.call("rbind",lapply(rawRatings,loadRating)))

# finish up prefs file
cat("</body></html>")
sink()

drawPlot <- function (p,out,width=800,height=600) {
  if (!file.exists(out) | fresh) {
    png(out,width=width,height=height)
    plot(p)
    dev.off()
  }
}

if (!file.exists(reports)) {
  dir.create(reports)
}

# overall
drawPlot(ggplot(ratings) +
         geom_boxplot(aes(Whiskey,Overall),outlier.size=0) +
         geom_text(aes(x=Whiskey,y=Overall,colour=Taster,label=Taster),
                     size=4, position=position_jitter(width=.2,height=.05)) +
         guides(colour=FALSE) + ggtitle("Overall ratings"),
         file.path(reports,"overall.png"),width=2000,height=500)
# overall by gender
drawPlot(ggplot(ratings) +
         geom_boxplot(aes(Whiskey,Overall,fill=Gender),outlier.size=0) +
         geom_text(aes(x=Whiskey,y=Overall,colour=Taster,label=Taster),
                     size=4, position=position_jitter(width=.2,height=.05)) +
         guides(colour=FALSE) + ggtitle("Overall ratings by gender"),
         file.path(reports,"overall_gender.png"),width=2000,height=500)

# mellowness vs. proof
drawPlot(ggplot(ratings,aes(x=Proof,y=Mellowness,colour=Taster)) +
         geom_boxplot(aes(group=Proof),outlier.size=0) +
         geom_text(aes(label=Taster),
                   alpha=.3,position=position_jitter(width=.2,height=.05),size=4) +
         geom_smooth(aes(group=1,colour="red"),na.rm=TRUE,method="loess") +
         geom_smooth(aes(group=1,colour="blue"),na.rm=TRUE,method="lm") +
         guides(colour=FALSE) + ggtitle("Mellowness vs. proof"),
         file.path(reports,"mellowness_proof.png"))
drawPlot(ggplot(ratings,aes(x=reorder(Whiskey,Proof),y=Mellowness)) +
         geom_violin(aes(fill=Proof)) +
         geom_text(aes(label=Taster,colour=Taster),
                   position=position_jitter(),size=4) +
         xlab("Whiskey") + guides(colour=FALSE) + ggtitle("Mellowness vs. proof, by whiskey"),
         file.path(reports,"mellowness_proof_violin.png"),width=1500)

# comparing different rating axes
ratings.long <- melt(ratings,id=c("Whiskey","Overall","Taster"),measure=c("Sweetness","Mellowness","Cost"))
colnames(ratings.long) <- c("Whiskey","Overall","Taster","Variable","Value")
drawPlot(direct.label(ggplot(ratings.long,aes(x=Overall,y=Value,colour=Variable)) +
                      ylim(1,5) + ylab("") +
                      geom_smooth(na.rm=TRUE) +
                      ggtitle("Overall ratings vs. other variables"),
                      "top.bumptwice"),
         file.path(reports,"overall_variables.png"),width=500,height=500)
drawPlot(ggplot(ratings.long,aes(x=Overall,y=Value,colour=Variable)) +
         ylim(1,5) + ylab("") +
         geom_smooth(method="glm",na.rm=TRUE) +
         geom_jitter(alpha=.3,size=1) +
         ggtitle("Overall ratings vs. other variables") +
         facet_wrap(~Taster,ncol=4),
         file.path(reports,"overall_variables_taster.png"),width=1200,height=1200)

# cost vs real cost
drawPlot(ggplot(ratings,aes(x=RealCost,y=Cost,colour=Gender)) +
         ylim(1,5) + xlab("Real Cost ($/.75L)") + geom_smooth() +
         ggtitle("Cost vs. real cost"),
         file.path(reports,"cost.png"),width=500,height=500)

#ggplot(ratings,aes(x=RealCost,y=Proof,colour=Whiskey,size=Overall)) + geom_text(aes(label=Whiskey),position=position_jitter())

# Affine model
#
# Factor_{whiskey,taster} = discretize(
#  (actual_{whiskey,factor} + error*noise_{taster,factor})*gain_{taster,factor} +
#  bias_{taster,factor})
#
# actual, gain, and bias are fixed, scalar parameters of the model
# noise is a random variable parameter of the model
# error is N(0,1)
#
# general plan:
#   given this model, use recursive descent (or some other form of minimization)
#   to find values for the parameters that maximize Prob(data | model)
#     PROTIP: calculate log and add that together to minimize floating point error
#
# actual = 16 * 4
# noise = 23 * 4
# gain = 23 * 4
# bias = 23 * 4
#  |model| = 340
#
#
#factorProb <- function (w, tnoise, tgain, tbias) {
#  meanContrib <- w * tgain + tbias
#  stddevContrib <- tnoise * tgain
#  
#}
