
#Create some datea
a = c(1,2,3,2,4,1,2,3,4,5,6,7,3,1,2,3,4,5,6,1,2,3,2,1)
b = c(2,3,4,5,2,5,6,2,3,4,5,2,4,3,5,6,3,4,5,4,8,6,7,3)

c = c(1,2,5,3,6,7,6,3,2,5,7,9,6,4,3,2,6,8,9,0,5,3,2,4)
d = c(4,5,6,4,3,5,4,6,7,8,9,7,5,6,4,2,3,4,2,3,2,5,6,3)

#Create variables from t.test-results
tres1 <- t.test(a,b, paired = TRUE)
tres2 <- t.test(a,c, paired = TRUE)
tres3 <- t.test(a,d, paired = TRUE)
tres4 <- t.test(b,d, paired = TRUE)

#Collect pvalues from t-tests in data vector
pvals <- c(tres1$p.value, tres2$p.value, tres3$p.value, tres4$p.value)

#Adjust p-values (prints to console)
p.adjust(pvals, method = "BH")
