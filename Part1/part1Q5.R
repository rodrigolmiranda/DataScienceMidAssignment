# Probability = Quantity of outcomes that results in 4(3)/ Total of possibilities (6*6)
# p = 3/36


# p(x=k) = (n!/((n-k)!*K!))*(p^k(1-p)^(n-k))
n=8
x=5
k=x
p = 3/36
nk = factorial(n)/(factorial(n-k)*factorial(k))
Pr = nk*( (p^k)* ((1-p)^(n-k)))
Pr

binom_samp = rbinom(10000, 1, Pr)
plot(hist(binom_samp, breaks = seq(from = 0, to = 1, by = 0.1)),col="blue",
  main="full probability mass function for X for this die-rolling example", xlab="Probability")


  