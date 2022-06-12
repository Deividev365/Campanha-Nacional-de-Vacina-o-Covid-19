#Suponha que a pressão arterial de uma pessoa mede tipicamente 160 ± 20 mm. Se
#alguém fizer n = 5 leituras de pressão arterial, qual é a probabilidade de a média ser <=
#150?


#Probabilidade
pnorm(q = 150, mean = 160, sd = 20 / sqrt(5), lower.tail = TRUE)


#Gráfico
data.frame(x = 100:200, 
           prob = pnorm(q = 100:200, 
                        mean = 160, 
                        sd = 20 / sqrt(5), 
                        lower.tail = TRUE)) %>%
  mutate(cdf = ifelse(x > 0 & x <= 150, prob, 0)) %>%
  ggplot() +
  geom_line(aes(x = x, y = prob)) +
  geom_area(aes(x = x, y = cdf), alpha = 0.3) +
  labs(title = bquote('X~N('~mu==.(160)~','~sigma^{2}==.(20 / sqrt(5))^{2}~')'),
       x = "x",
       y = "Probabilidade acumulada")