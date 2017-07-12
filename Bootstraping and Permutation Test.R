

#Bootstrap approach to obtain a 90% confidence interval (both t- and percentile) for the estimate of the
#CV of Engine Size
library(UsingR)
data("Cars93")
attach (Cars93)
par (mfrow = c(1,1))
cv = (sd(EngineSize))/(mean(EngineSize))
cv
n = length(EngineSize)
m = 1000
cv.enginesize = c()
set.seed(72548)
for (i in 1:m) {
  res=sample(EngineSize, n, replace = T)
  cv.enginesize[i] = (sd(res))/(mean(res))
}

hist(cv.enginesize, freq = F, xlab = "Sample CV of Engine Size")
lines(density(cv.enginesize))
abline (v=cv)
qqnorm(cv.enginesize, main = "Bootstrap Distribution of Sample CV of Engine Size")
qqline(cv.enginesize)



# Bootstrap t-CI
(cv + c(-1, 1)* qt(0.90, df = n-1)*sd(cv.enginesize))

# Bootstrap Percentle CI
quantile(cv.enginesize, c(0.1,0.90))

#3.B
# Permutation Test approach to test whether the CV of Engine Size differs by Origin of the cars

mean.es.usa = (mean(EngineSize[Origin=="USA"]))
mean.es.non= (mean(EngineSize[Origin=="non-USA"]))

sd.es.usa = sd(EngineSize[Origin=="USA"])
sd.es.non = sd(EngineSize[Origin=="non-USA"])
cv.usa = sd.es.usa/mean.es.usa
cv.non=sd.es.non/mean.es.non
diff.cv = cv.usa-cv.non
diff.cv
# Now the permute the data
es.grp = Origin
n=length(EngineSize)
B = 1000
mean.usa = c()
sd.usa =c()
mean.non = c()
sd.non=c()


set.seed(83877)
for (i in 1:B) {
  res=sample(es.grp, n, replace = F)
  mean.usa[i] = mean(EngineSize[res=="USA"])
  mean.non[i]=mean(EngineSize[res=="non-USA"])
  sd.usa[i] = sd(EngineSize[res=="USA"])
  sd.non[i] = sd(EngineSize[res=="non-USA"])
  
}

usa.cv = sd.usa/mean.usa
non.cv = sd.non/mean.non
cv.diff = usa.cv - non.cv
summary(cv.diff)
hist(cv.diff, freq = F, xlab = "Difference in Sample Means",
     main = "Permutation Distribution", ylim = c(0, .5), breaks = seq(-110, 70, 1), xlim = c(-10, 10))
lines(density(cv.diff))
abline(v = diff.cv)

#95% CI Bootstrap z-CI
diff.cv + c(-1, 1)* qnorm(0.975)*sd(cv.diff)

quantile(cv.diff, c(0.025,0.975))

detach(Cars93)
