ata <- read.csv("CD.csv", header=T)

head(data, n=11)  ### get the first 11 rows to look at

y <- as.matrix(data$carbohydrate)

X <- as.matrix(cbind(rep(1, nrow(data)), data$age, data$weight, data$protein))

b <- solve(t(X) %*% X) %*% t(X) %*% y
b


### Variance-covariance matrix estimate for the betas:

sig <- 1/(nrow(data) - 4) * t(y-X%*%b) %*% (y-X%*%b) 
sig.mat <- matrix(rep(sig,16), 4)
Jinv <- sig.mat*solve(t(X) %*% X)

### The std errors for the betas are the square-roots of the diagonal elements 
### of Jinv above.  Alternatively, you could get them this way:

sqrt(diag(as.numeric(sig), 4) * solve(t(X) %*% X))

### Using the deviance to test whether the model without age variable
### is significant...


### The null model matrix is...
X.0 <- X.0 <- X[,-2]

### The alternative model matrix is the same as before...
X.1 <- X

### The beta estimates for the null and alternative models are 
b.0 <- solve(t(X.0) %*% X.0) %*% t(X.0) %*% y
b.1 <- b


### Delta Deviance or sum of squares alternative...

S.0 <- t(y) %*% y - t(b.0) %*% t(X.0) %*% y

S.1 <- t(y) %*% y - t(b.1) %*% t(X.1) %*% y

Fstat <- ((S.0 - S.1)/(4-3))    /    (S.1/(nrow(X) - 4 ))
Fstat
pval <- 1- pf(Fstat, 1, nrow(X) - 4)
pval

### fail to reject the null model in favor of the more detailed one: 
### we don't feel compelled to include age in our model.

### Of course we can do this quickly with R... using out1 for the 
### output corresponding to the null model, and out0 for the output
### corresponding to the alternative model...

out1 <- lm(carbohydrate ~ age + weight + protein, data = data)
summary(out1)
anova(out1)

out0 <- lm(carbohydrate ~ weight + protein, data = data)
summary(out0)
anova(out0)

### Using the glm() command...

glm.out.1 <- glm(carbohydrate ~ age + weight + protein, family = gaussian, data=data)
summary(glm.out.1)
glm.out.1

glm.out.0 <- glm(carbohydrate ~ weight + protein, family = gaussian, data=data)
summary(glm.out.0)
glm.out.0

##############################################################################











