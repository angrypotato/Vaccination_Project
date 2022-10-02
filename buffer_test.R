
buffer <- data.frame(buffer = c(0.0001,0.0002,0.00025,0.0003,0.00035,0.0004,0.00045,0.0005,0.00055,0.0006,0.0007,0.0008,0.0009,0.0010,0.0012,
                                seq(0.0015,0.006,0.0005)),
                     proportion = c(0.957,0.912,0.903,0.894,0.889,0.884,0.881,0.878,0.876,0.874,0.871,0.869,0.866,0.864,0.86,
                                    0.855,0.845,0.834,0.822,0.81,0.798,0.786,0.773,0.761,0.748))

x<-buffer$buffer
y<-buffer$proportion
plot(x,y, col=ifelse(x %in% c(0.00045,0.0012), 'red', 'black'),
     xlab="Coordinates threshold", ylab="Outreach proportion",
     cex.axis = 1.3, cex.lab = 1.3)
lines(x,y)
text(x[7]+0.00035, y[7]+0.0002, labels=x[7])
text(x[15]+0.0003, y[15]+0.0004, labels=x[15])


line <- data.frame(buffer =  seq(0.001,0.006,0.0005),
                   proportion=c(0.864,0.855,0.845,0.834,0.822,0.81,0.798,0.786,0.773,0.761,0.748))
lmod <- lm(proportion~buffer,data = line)
predict(lmod,newx=0.0009)


