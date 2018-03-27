# Set constants.
   sd <- 2
tails <- 1
alpha <- .05

# Population (p) and Sample (s)
p <- rnorm(20, mean = 50, sd = 2)
s <- rnorm(20, mean = 51, sd = 2)

# Execute a t-test to obtain the p-value.
t <- t.test(s, p, alternative = 'greater')
p_val <- t$p.value
  
hist(p, freq = F, border = 'white', 
     xlim = range(47, 58), 
     ylim = range(0.00, 0.50))

curve(dnorm(x, mean(p), sd(p)), add = T, col = 'blue')
curve(dnorm(x, mean(s), sd(s)), add = T, col = 'red', lty = 4)

abline(v = mean(p), col = 'blue')
abline(v = mean(s), col = 'red', lty = 5)

# Draw line to delineate critical region.  Set z to a constant, depending on
# the alpha value and whether this is a one or two tail comparison.
if (alpha == .05 & tails == 1) {z <- 1.65}
if (alpha == .01 & tails == 1) {z <- 2.33}

X <- mean(p) + (z * sd)                   # Gravetter, p. 148
abline (v = X, col = 'lightgrey')

