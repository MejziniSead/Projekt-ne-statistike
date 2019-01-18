#leximi i datasetit
data = read.csv("vetura.csv");
attach(data);
# karakteristikat
n = names(data);
print(n)
#numri i karakteristikave
l = length(data)
print(l)
#numri i rreshtave
n = length(data$MPG)
#te dhena bazike
summary(data);
#varianca,devijimi standard, frekuenca relative dhe komulative
counter = 1;
while (counter <= 6) {
  temp = as.numeric(data[counter][[1]]);
  print(names(data[counter]))

  if(counter==2){
    x = table(temp);
  }else{

    dist = max(temp)-min(temp);
    range = floor(sqrt(n));
    mod = dist%%range;
    a = dist + (range-mod);
    range = a/range;

    x = table(cut(temp,breaks = seq(min(temp),max(temp)+range,range),right = FALSE));
  }
  Frekuenca = x;
  Frekuenca_relative = x/n;
  Frekuenca_komulative = cumsum(x);
  Perqindja_komulative = cumsum(x/n);

  print(paste("Varianca",var(temp),sep = " "))
  print(paste("Devijimi standard",sd(temp),sep = " "))
  print("-----------------------------------------------------------------------------------");
  print(cbind(Frekuenca,Frekuenca_relative,Frekuenca_komulative,Perqindja_komulative))
  print("-----------------------------------------------------------------------------------")
  counter = counter+1;
}
#Regresion linear i njefisht ndermjet 'Horsepower' dhe 'Displacement' ('kuaj fuqi' dhe 'vëllimi i cilindrit të motorit')

m1 = lm(Horsepower~Displacement);
summary(m1);
ggplot(data, aes(x = Displacement, y = Horsepower,alpha = 0.8)) + geom_point()+geom_smooth(method = lm) +ggtitle("y = 40.37 + 0.3298x");
#2 Komandat e meposhtme te komentuara kryjen punen e njejte sikurse linja e mesiperme e kodit
#plot(Displacement,Horsepower)
#abline(m1)

#Intervali i konfidences.
confint(m1);





