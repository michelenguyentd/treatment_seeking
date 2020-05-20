pregnant <- read.csv('Z:/GBD2017/Processing/Stages/01b_World_Bank_Covariates/Checkpoint_Outputs/pregnant_jul_2017.csv', sep = ";")
master.iso <- unique(pregnant$iso)

pdf('pregnant.pdf',width=8.7,height = 11.2)

par(mfrow=c(4,2))

for (i in 1:length(master.iso)){
  
  country_line <- pregnant[pregnant$iso==master.iso[i],]
  
  medianline <- country_line$gp_value
  upperline <- country_line$gp_value + qnorm(0.975, mean = 0, sd = 1)*sqrt(country_line$mse)
  lowerline <- country_line$gp_value - qnorm(0.975, mean = 0, sd = 1)*sqrt(country_line$mse)
  
  plot(country_line$year,upperline, ylim = c(0, 100), col="white",xlab="Year",ylab="Out-of-pocket health expenditure (%)",main=unique(country_line$country))
  polygon(c(country_line$year,rev(country_line$year)),c(lowerline,rev(upperline)),bg="yellow",border="yellow",col="yellow",density=-1)
  lines(country_line$year,medianline,col="blue")
  
}
dev.off()

