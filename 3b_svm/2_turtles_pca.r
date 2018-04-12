if(!require(ade4,quiet=TRUE))install.packages('ade4');require(ade4)
data(tortues)

#?tortues
# need to install rgl
#rename the data set

pturtles = tortues

names(pturtles) = c("length", "width", "height", "sex")

sex = pturtles$sex
sexcol = ifelse(sex == "F", "pink", "blue")

measures = pturtles[, 1:3]
plot(measures, col = sexcol, pch = 19)

if(!require(rgl,quiet=TRUE))install.packages('ade4');require(rgl)
plot3d(measures, type = "s", col = sexcol)


measures.c.sc = scale(measures, center = TRUE, scale = TRUE)

pc.shell = prcomp(measures,scale=TRUE,center=TRUE)
pc.summary = summary(pc.shell)

plot(pc.shell$x[,1],rep(0,nrow(pc.shell$x)),bg=sexcol,pch=20+as.numeric(sex),xlab='PC 1 scores',ylab='',cex=1.5,yaxt='n')
plot(pc.shell$x[,1:2],bg=sexcol,pch=20+as.numeric(sex),xlab='PC 1 scores',ylab='PC 2 scores',cex=1.5)

