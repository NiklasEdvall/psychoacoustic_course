
a = c(1,2,3,2,4,1,2,3,4,5,6,7,3,1,2,3,4,5,6,1,2,3,2,1)

b = c(2,3,4,5,2,5,6,2,3,4,5,2,4,3,5,6,3,4,5,4,8,6,7,3)

tres1 <- t.test(a,b, paired = TRUE)
tres2 <- t.test(a,b, paired = TRUE)
tres3 <- t.test(a,b, paired = TRUE)

pvals <- c(tres1$p.value, tres2$p.value, tres3$p.value)

p.adjust(pvals2, method = "BH")

pvals2 <- c(0.003, 0.03, 0.45)
