# Plot normal pdf
```
norm_pdf <- function(x , d.mean = 0, d.sd = 1) {
    r <- (
           1/(
             d.sd*sqrt(2*pi)
             )
         )*exp(1)^(
           -0.5*(
             (
               (x-d.mean)/d.sd
             )^2
          )
        )
    #write(r, "")
    return(r)
}

men.mean <- 69.1
men.sd <- 2.9
men.sample_size <- 1000

women.mean <- 63.7
women.sd <- 2.7
women.sample_size <- 1000

men <- rnorm( men.sample_size, mean = men.mean, sd = men.sd )
women <- rnorm( women.sample_size, mean = women.mean, sd = women.sd )

write("Men", "")
mean(men)
quantile(men, c(0.978))
69.1 + 2*2.9


write("Women", "")
mean(women)
quantile(women, c(0.978))
63.7 + 2*2.7

domain_sz = 100
domain <- 0:(domain_sz-1)

from <- men.mean - 4*men.sd
to <- men.mean + 4*men.sd
delta <- (to - from)/domain_sz

domain <- delta * domain + from
fx <- lapply(domain, norm_pdf, d.mean = men.mean, d.sd = men.sd)

plot(domain, fx, type = "l")
```

fooplot.com see http://fooplot.com/#W3sidHlwZSI6MCwiZXEiOiIoMS8oMi45KnNxcnQoMipwaSkpKSplXigtMC41KigoKHgtNjkuMSkvMi45KV4yKSkiLCJjb2xvciI6IiM5NDQxNDEifSx7InR5cGUiOjAsImVxIjoiKDEvKDIuNypzcXJ0KDIqcGkpKSkqZV4oLTAuNSooKCh4LTYzLjcpLzIuNyleMikpIiwiY29sb3IiOiIjMjUyMjdBIn0seyJ0eXBlIjoxMDAwLCJ3aW5kb3ciOlsiNTQiLCI3OCIsIjAiLCIwLjE2Il19XQ--
normal distribution around mean:63.7, sd:2.7
`(1/(2.7*sqrt(2*pi)))*e^(-0.5*(((x-63.7)/2.7)^2))`

normal distribution around mean:69.1, sd:2.9
`(1/(2.9*sqrt(2*pi)))*e^(-0.5*(((x-69.1)/2.9)^2))`
