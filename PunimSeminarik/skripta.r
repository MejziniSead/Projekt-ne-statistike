#librarit e perdorura
library(tidyverse);
library(ggplot2);
library(ggpubr);

#leximi i datasetit
data = read.csv("vetura.csv");
attach(data);
# karakteristikat
names(data);
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
  print(length(Frekuenca));
  print("-----------------------------------------------------------------------------------")
  counter = counter+1;
}
#Regresion linear i njefisht ndermjet 'Horsepower' dhe 'Displacement' ('kuaj fuqi' dhe 'vëllimi i cilindrit të motorit')

m1 = lm(Horsepower~Displacement);
summary(m1);

#Intervali i konfidences.
c = confint(m1);
print(c)

#Funksioni i vijes se regresionit
b = round((c[1]+c[3])/2,4);
k = round((c[2]+c[4])/2,4);
funksioni = paste("y=",b,"+",k,"*x");
print(funksioni)

#Scatter plot
ggplot(data, aes(x = Displacement, y = Horsepower,alpha = 0.8)) + geom_point()+geom_smooth(method = lm) +ggtitle(funksioni);
#2 Komandat e meposhtme te komentuara kryjen punen e njejte sikurse linja e mesiperme e kodit
#plot(Displacement,Horsepower)
#abline(m1)

#Q-Q plot
qqplot(Displacement, Horsepower);

#Regresion linear i njefisht ndermjet 'Displacement' dhe 'Cylinders' ('vëllimi i cilindrit të motorit'dhe 'Cilindri')

m2 = lm(Displacement~Cylinders);
summary(m2);

#Intervali i konfidences.
c = confint(m2);
print(c)

#Funksioni i vijes se regresionit
b = round((c[1]+c[3])/2,4);
k = round((c[2]+c[4])/2,4);
funksioni = paste("y=",b,"+",k,"*x");
print(funksioni)

#Scatter plot
ggplot(data, aes(x = Cylinders,y = Displacement,alpha = 0.8)) + geom_point()+geom_smooth(method = lm) +ggtitle(funksioni);

#Q-Q plot
qqplot(Cylinders,Displacement);

#Regresion linear i shumefishte ndermjet 'MPG' dhe 'Horsepower+Weight'  ('MPG' dhe 'kuaj-fuqi+pesha')

m3 = lm(MPG~Horsepower+Weight);
summary(m3);

#Intervali i konfidences.
c = confint(m3);
print(c)

#Funksioni i vijes se regresionit
b0 = round((c[1]+c[4])/2,4);
b1 = round((c[2]+c[5])/2,4);
b2 = round((c[3]+c[6])/2,4);
funksioni = paste("Funksioni: ","y=",b0,"+",b1,"*x1","+",b2,"*x2");
print(funksioni);

#Regresion linear i shumefishte ndermjet 'Horsepower' dhe 'Cylinders+Weight+Displacement'  ('MPG' dhe 'cilindri+pesha+vëllimi i cilindrit të motorit')

m4 = lm(Horsepower~Cylinders+Weight+Displacement);
summary(m4);

#Intervali i konfidences.
c = confint(m4);
print(c)

#Funksioni i vijes se regresionit
b0 = round((c[1]+c[5])/2,4);
b1 = round((c[2]+c[6])/2,4);
b2 = round((c[3]+c[7])/2,4);
b3 = round((c[4]+c[8])/2,4);
funksioni = paste("Funksioni: ","y=",b0,"+",b1,"*x1","+",b2,"*x2","+",b3,"*x3");
print(funksioni);

#Regresion linear i shumefishte ndermjet 'MPG' dhe 'Cylinders+Weight+Displacement+Horsepower+Acceleration'  ('MPG' dhe 'cilindri+pesha+vëllimi i cilindrit të motorit')

m5 = lm(MPG~Cylinders+Weight+Displacement+Horsepower+Acceleration);
summary(m5);

#Intervali i konfidences.
c = confint(m5);
print(c)

#Funksioni i vijes se regresionit
b0 = round((c[1]+c[7])/2,4);
b1 = round((c[2]+c[8])/2,4);
b2 = round((c[3]+c[9])/2,4);
b3 = round((c[4]+c[10])/2,4);
b4 = round((c[5]+c[11])/2,4);
b5 = round((c[6]+c[12])/2,4);
funksioni = paste("Funksioni: ","y=",b0,"+",b1,"*x1","+",b2,"*x2","+",b3,"*x3",b4,"*x4",b5,"*x5");
print(funksioni)

