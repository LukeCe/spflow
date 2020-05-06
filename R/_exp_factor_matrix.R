tt <- factor(factor(c("A","A","B","C")))
tt
matrix(tt,2)
dim(tt) <- 2
dim(tt) <- c(2,2)
tt
tt == 1
tt == 2
levels(tt)
dim(tt) %>% numeric()
tt == "A"
as.numeric(tt == "A")
as.integer(tt == "A")
