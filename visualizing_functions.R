prac <- expand.grid(
  a = seq(10,15,1),
  b = seq(1,5,1),
  chl = daph_fec$chl,
  out = 0
)

prac$out <- sat_fun(prac$a,prac$b,prac$chl)

ggplot(aes(chl, out), data = prac) + geom_point(aes(color = as.factor(a)))+ facet_wrap(~b)

prac$out2 <- 1/sat_fun(prac$a,prac$b,prac$chl)

ggplot(aes(chl, out2), data = prac) + geom_point(aes(color = as.factor(a)))+ facet_wrap(~b)

prac$out3 <- new_fun(prac$a, prac$chl)
  
ggplot(aes(chl, out3), data = prac) + geom_point(aes(color = as.factor(a))) #+ facet_wrap(~b)

  # saturating function for nutrient release where K is independent state variable
  sat_fun <- function(a,b,k) {
    
    a * k / (k + b)
    
  }

new_fun <- function(a,k) {
  
  k/a
  
}
