NUMBER_OF_BREAKPOINTS <- 20  
DEGREES_OF_FREEDOM <- 2     
GENERATED_VALUES <- 1000     
s <- 10000                   
FPS <- 0.5                    
COUNT <- 400    
main = function ()
{
  ### WYGENEROWANIE 1000 WARTOŚCI I NANIESIENIE NA HISTOGRAM
  z <- numeric()
  averages <- numeric()
  for (j in 1:s)
    {
  ## wygenerowane wartosci dla nowej probki
  new_sample <- rchisq(GENERATED_VALUES, DEGREES_OF_FREEDOM)
  ## srednia dla nowej probki
  averages <- append(averages, mean(new_sample))
  # obliczanie z_j
  z <- append(z,(averages[j] - DEGREES_OF_FREEDOM) / (DEGREES_OF_FREEDOM / sqrt(GENERATED_VALUES)))
  if ((j) %% COUNT == 0) {
    ## histogram wygenerowanych wartosci dla j-tej probki
    hist(
      new_sample, prob = TRUE, 
      ylab = 'gęstość rozkładu', xlab = paste('wygenerowane wartości dla j=',j), 
      main = paste('rozkład chi-kwadrat dla',DEGREES_OF_FREEDOM,' stopni swobody')
    )
    x <- seq(min(new_sample), max(new_sample), length.out = 50)
    lines(x, dchisq(x, df = DEGREES_OF_FREEDOM), col = "blue")
    Sys.sleep(1 / FPS)
    ## histogram dla wartosci z wektora z
    hist(
      z, breaks = 30, probability = TRUE, col = rgb(0, 0, 1, 0.2),
      main = "histogram Z_j i gęstość rozkładu normalnego N(0,1)",
      xlab = paste("z_j (iteracja =", j, ")"), ylab = "gęstość rozkładu"
    )
    curve(dnorm(x, mean = 0, sd = 1), add = TRUE, col = "red", lwd = 2)
    Sys.sleep(1 / FPS)
  }
  }
}
main()



