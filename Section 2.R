my.data <-read.table("Viscos.txt", header=TRUE, sep="")
class(data)
X <- matrix(  
  c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    0, 0, 0, 0, 0, 0, 10, 10, 10, 10, 10, 10, 20, 20, 20, 20, 20, 20, 30, 30, 30, 30, 30,
    0, 12, 24, 36, 48, 60, 0, 12, 24, 36, 48, 60, 0, 12, 24, 36, 48, 60, 12, 24, 36, 48, 60,
    0, 0, 0, 0, 0, 0, 100, 100, 100, 100, 100, 100, 400, 400, 400, 400, 400, 400, 900, 900, 900, 900, 900,
    0, 0, 0, 0, 0, 0, 0, 120, 240, 360, 480, 600, 0, 240, 480, 720, 960, 1200, 360, 720, 1080, 1440, 1800,
    0, 144, 576, 1296, 2304, 3600, 0, 144, 576, 1296, 2304, 3600, 0, 144, 576, 1296, 2304, 3600, 144, 576, 1296, 2304, 3600), 
  nrow=23,           
  ncol=6,              
  byrow = FALSE)
X
y = c(6.50, 9.50, 12.50, 19, 27, 39.25, 4.25, 6.50, 9.25, 13.25, 20.75, 31, 3.25, 5, 6.75, 9.25, 14.25, 21.75, 3.75, 5.50, 6.75, 10.25, 15.75)
y
XT = t(X)
XTX = XT %*% X
XTX
XTXI = solve(XTX)
XTXI
XTy = XT %*% y
XTy
beta.hat = XTXI %*% XTy
beta.hat
y.hat = X %*% beta.hat
y.hat
e = y.hat - y
e
SSE = sum(e^2)
SSE

y.bar = mean(y)
y.bar

ny.bar2 = 23 %*% y.bar^2 
ny.bar2

SSR = t(beta.hat) %*% XTy
SSR
SSRC = SSR - ny.bar2
SSRC

SST = t(y) %*% y
SST
SSTC = SST - ny.bar2
SSTC

R2 = SSRC/SSTC
R2

Fo = c(1, 10, 50, 100, 500, 2500)
FoT = t(Fo)
FoT
Mean = FoT %*% beta.hat
Mean
c = FoT %*% XTXI %*% Fo
c



#Analysis with R#
viscosity = c(6.50, 9.50, 12.50, 19.00, 27.00, 39.25, 4.25, 6.50, 9.25, 13.25, 20.75, 31.00, 3.25, 5.00, 6.75, 
              9.25, 14.25, 21.75, 3.75, 5.50, 6.75, 10.25, 15.75)
viscosity
oil = c(0, 0, 0, 0, 0, 0, 10, 10, 10, 10, 10, 10, 20, 20, 20, 20, 20, 20, 30, 30, 30, 30, 30)
oil
filler = c(0, 12, 24, 36, 48, 60, 0, 12, 24, 36, 48, 60, 0, 12, 24, 36, 48, 60, 12, 24, 36, 48, 60)
filler

model.fit <- lm(viscosity ~ oil + filler + I(oil^2) + oil*filler + I(filler^2))
summary(model.fit)
anova(model.fit)
confint(model.fit,level=0.90)


#Comparing Models A and Models B#

model.fit1 = lm(viscosity ~ oil + filler + oil*filler + I(oil^2) + I(filler^2))
model.fit2 = lm(viscosity ~ oil + filler + oil*filler)
anova(model.fit2, model.fit1)
qf(.95, df1=2, df2=19) 

new.data <- data.frame(oil=10, filler=50)
predict(model.fit1, newdata = new.dat, interval = 'confidence')
predict(model.fit1, newdata = new.dat, interval = 'prediction')

###############################################################################
###############################################################################
install.packages("ggpubr")
library("ggpubr")

ggscatter(my.data, x = "Oil", y = "Visc", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Oil (phr)", ylab = "Viscosity (M)")

ggscatter(my.data, x = "Filler", y = "Visc", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Filler (phr)", ylab = "Viscosity (M)")

new.data <- data.frame(oil=10, filler=c(30,40,50))
predict(model.fit1, newdata = new.data, interval = 'prediction')

new.data <- data.frame(oil=c(10,20,30), filler=50)
predict(model.fit1, newdata = new.data, interval = 'prediction')
