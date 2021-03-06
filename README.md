+ [Метрические Алгоритмы классификации](https://github.com/vinovatvolondemord/ML0/blob/master/README.md#%D0%BC%D0%B5%D1%82%D1%80%D0%B8%D1%87%D0%B5%D1%81%D0%BA%D0%B8%D0%B5-%D0%B0%D0%BB%D0%B3%D0%BE%D1%80%D0%B8%D1%82%D0%BC%D1%8B-%D0%BA%D0%BB%D0%B0%D1%81%D1%81%D0%B8%D1%84%D0%B8%D0%BA%D0%B0%D1%86%D0%B8%D0%B8)
    + [Алгоритм	ближайших	соседей](https://github.com/vinovatvolondemord/ML0/blob/master/README.md#%D0%B0%D0%BB%D0%B3%D0%BE%D1%80%D0%B8%D1%82%D0%BC%D0%B1%D0%BB%D0%B8%D0%B6%D0%B0%D0%B9%D1%88%D0%B8%D1%85%D1%81%D0%BE%D1%81%D0%B5%D0%B4%D0%B5%D0%B9)
+ [Байесовский классификатор](https://github.com/vinovatvolondemord/ML0/blob/master/README.md#%D0%B1%D0%B0%D0%B9%D0%B5%D1%81%D0%BE%D0%B2%D1%81%D0%BA%D0%B8%D0%B9-%D0%BA%D0%BB%D0%B0%D1%81%D1%81%D0%B8%D1%84%D0%B8%D0%BA%D0%B0%D1%82%D0%BE%D1%80) 
    + [Наивный Нормальный Байесовский Классификатор](https://github.com/vinovatvolondemord/ML0/blob/master/README.md#%D0%BD%D0%B0%D0%B8%D0%B2%D0%BD%D1%8B%D0%B9-%D0%BD%D0%BE%D1%80%D0%BC%D0%B0%D0%BB%D1%8C%D0%BD%D1%8B%D0%B9-%D0%B1%D0%B0%D0%B9%D0%B5%D1%81%D0%BE%D0%B2%D1%81%D0%BA%D0%B8%D0%B9-%D0%BA%D0%BB%D0%B0%D1%81%D1%81%D0%B8%D1%84%D0%B8%D0%BA%D0%B0%D1%82%D0%BE%D1%80)
 + [Линейные Алгоритмы](https://github.com/vinovatvolondemord/ML0/blob/master/README.md#%D0%BB%D0%B8%D0%BD%D0%B5%D0%B9%D0%BD%D1%8B%D0%B5-%D0%B0%D0%BB%D0%B3%D0%BE%D1%80%D0%B8%D1%82%D0%BC%D1%8B)
    + [Метод Стохастического Градиента](https://github.com/vinovatvolondemord/ML0/blob/master/README.md#%D0%BC%D0%B5%D1%82%D0%BE%D0%B4-%D1%81%D1%82%D0%B0%D1%85%D0%BE%D1%81%D1%82%D0%B8%D1%87%D0%B5%D1%81%D0%BA%D0%BE%D0%B3%D0%BE-%D0%B3%D1%80%D0%B0%D0%B4%D0%B8%D0%B5%D0%BD%D1%82%D0%B0)
    + [ADALINE](https://github.com/vinovatvolondemord/ML0/blob/master/README.md#adaline)
## Метрические алгоритмы классификации
### Алгоритм	ближайших	соседей
Необходимо было реализовать	алгоритм __k__ ближайших соседей – __kNN__.	

Суть метода __kNN__ в том что для нашего обьекта __u__ мы рассматриваем его __k__ ближайших соседей и приписываем __u__ к тому классу в котором больше соседей данного класса. 

Если мы возьмеи маленькое __k__ то алгоритм будет неустойчив к шуму, т.е. неустойчив к погрешностям (выбросам -- объектам, которые окружены объектами чужого класса), если же возьмем k большим то алгоритм будет слишком устойчивым и к примеру если мы возьмем всю выборку как __k__ ,то любую точку алгоритм будет всегда приписывать к тому классу в котором больше всего элементов.

Нужно найти оптимальное __k__  и для этого воспользуемся критерием скользящего контроля __LOO__.
Суть критеия в том что мы берем нашу выборку и рассматриваем по одному её обьекту. Каждый обьект мы классифицируем пор методу __kNN (kwNN)__ и сравниваем совпал ли класс нашего обьекта с тем который у нас был в выборке. Если нет, то мы прибавляем к счетчику 1. После того как мы классифицировали так всю выборку поделим наш счетчик на длину ввыборки. Получим вероятность возникновения ошибки при __k__. Остаеться лишь найти такое __k__ при котором значение этой вероятности минимально.

![](https://raw.githubusercontent.com/vinovatvolondemord/ML0/master/img/img1.PNG)

Ну и остается лишь классифицировать все точки на разные классы

![](https://raw.githubusercontent.com/vinovatvolondemord/ML0/master/img/img2.PNG)

Далее необходимо было реализовать алгоритм __k__ взвешенных	ближайших соседей – __kwNN__.	
Он отличается от __kNN__ тем что учитывает порядок соседей классифицируемого объекта, т.е. будет учитываться последовательность  весов, задающая вклад соседа при классификации объекта.
 ![](https://raw.githubusercontent.com/vinovatvolondemord/ML0/master/img/img3.PNG) ![](https://raw.githubusercontent.com/vinovatvolondemord/ML0/master/img/img4.PNG)

Функцию весов я взял у других
```
weightsKWNN = function(i, k)  (k + 1 - i) / k
```
Просто сравнивая результаты работы алгоритмов __kNN__ и __kwNN__ можно увидеть насколько лучше работает __kwNN__.
![](https://raw.githubusercontent.com/vinovatvolondemord/ML0/master/img/img2.PNG)
![](https://raw.githubusercontent.com/vinovatvolondemord/ML0/master/img/img4.PNG)

В __kNN__ любые шумовые точки влияют на его работу.

__kNN__ очень часто плох в решении каких либо практических задач классификации.Он медленный и неточеный. 

__kwNN__ лучше __kNN__. тем что он более устойчив к выбросам, это улчшает качество классификации. 

## Байесовский классификатор
###  Наивный Нормальный Байесовский Классификатор
Формула Байеса:
    
![](https://wikimedia.org/api/rest_v1/media/math/render/svg/2634e395f47aaf16f5deb5b09a979afc646d83eb)
    где
        
![](https://wikimedia.org/api/rest_v1/media/math/render/svg/4f264d19e21604793c6dc54f8044df454db82744) — априорная вероятность гипотезы A (смысл такой терминологии см. ниже);
        
![](https://wikimedia.org/api/rest_v1/media/math/render/svg/8f8f30f4da85b53901e0871eb41ed8827f511bb7) — вероятность гипотезы A при наступлении события B (апостериорная вероятность);
        
![](https://wikimedia.org/api/rest_v1/media/math/render/svg/e2fe9ad0fdfd8920e56ca948400e111852af0665) — вероятность наступления события B при истинности гипотезы A;
        
![](https://wikimedia.org/api/rest_v1/media/math/render/svg/e593d180a26fd68657ea50368dbfe1a661e652aa)— полная вероятность наступления события B.

Байесовский алгоритм классификации является классическим и лежит в основе многих методов.
Он опирается на то. что если плотность распредиления классов известна, то алгоритм классификации, имеющий минимальную вероятность ошибок, можно выписать в явном виде.

"Наи́вный ба́йесовский классифика́тор — простой вероятностный классификатор, основанный на применении теоремы Байеса со строгими (наивными) предположениями о независимости.([Wikipedia](https://ru.wikipedia.org/wiki/%D0%9D%D0%B0%D0%B8%D0%B2%D0%BD%D1%8B%D0%B9_%D0%B1%D0%B0%D0%B9%D0%B5%D1%81%D0%BE%D0%B2%D1%81%D0%BA%D0%B8%D0%B9_%D0%BA%D0%BB%D0%B0%D1%81%D1%81%D0%B8%D1%84%D0%B8%D0%BA%D0%B0%D1%82%D0%BE%D1%80#%D0%9C%D0%BE%D0%B4%D0%B5%D0%BB%D1%8C_%D0%BD%D0%B0%D0%B8%D0%B2%D0%BD%D0%BE%D0%B3%D0%BE_%D0%B1%D0%B0%D0%B9%D0%B5%D1%81%D0%BE%D0%B2%D1%81%D0%BA%D0%BE%D0%B3%D0%BE_%D0%BA%D0%BB%D0%B0%D1%81%D1%81%D0%B8%D1%84%D0%B8%D0%BA%D0%B0%D1%82%D0%BE%D1%80%D0%B0))

Если мы предположим что наши признаки независимы то, нашу апостериорную вероятность можно вычислить как произведение вероятностей встретить признак среди объектов класса:

![](https://latex.codecogs.com/gif.latex?P%28C%7CF_1%2C%5Cdots%20%2CF_n%29%3D%5Cprod%5En_%7Bi%3D1%7D%20p%28F_i%7CC%29)

Так как мы умножаем слишком малые величины (условных вероятностей), происходит потеря значащих разрядов, чтобы избежать этого возьмем логарифмы этих вероятностей. Логарифм монотонно возрастает, поэтому класс с максимальным логарифмом вероятности останется наиболее вероятным.

 Тогда наше решающее правило имеет вид:

![](https://camo.githubusercontent.com/8bc9dd137568ab3e91170c9160834d144265d638/687474703a2f2f6c617465782e636f6465636f67732e636f6d2f6769662e6c617465783f61253238782532392533446172672532302535436d61785f25374279253543696e253230592537442532382535436c6e2532382535436c616d6264615f25374279253744505f792532392b25354373756d5f2537426a253344312537442535452537426e2537442535436c6e253238705f253742796a25374425323825354378695f6a253239253239253239)

где 
![](https://latex.codecogs.com/gif.latex?P%28yj%28%5Cxi%20_j%20%29%29%3D%5Cfrac%7B1%7D%7B%5Csqrt%5B%5D%7B2%5Cpi%5Csigma%5E2_y%7D%7Dexp%28-%5Cfrac%7B%28x_i-%5Cmu_y%29%5E2%7D%7B2%5Csigma%5E2_y%7D%29)
```
baiesNaiv <- function(x, mu, sigma, lamda, P){
  n <- 2
  myfunction <- log(lamda*P)
  pyj<-0
  for(i in 1 : n){
    pyj <- pyj + log( (1/(sigma[i]*sqrt(2*pi))) * exp(-1 * ((x[i] - mu[i])^2)/(2*sigma[i]^2)))
    
  }
  myfunction <- myfunction + pyj
  return(myfunction)
}
```
Я взял выборку из 100 элементов 
![](https://raw.githubusercontent.com/vinovatvolondemord/ML0/master/img/Rplot.png)

## Линейные Алгоритмы
Линейным классификатором называется алгоритм классификации вида: ![](https://raw.githubusercontent.com/TIR13/ML0/master/line/img/klass.gif)
Для  решения нашей задачи классификации мы используем метод стохастического градиента
В этом методе выбирается некоторое приближение для w, затем запускается итерационый процесс на каждом шаге которого вектор w изменяется в направлении наибольее быстрого убывания функционала Q.
Я инициализирую веса таким способом:
```
w = runif(cols, -1 / (2 * cols), 1 / (2 * cols))
```
После находим начальные значения функционала Q
```
for (i in 1:rows) {
    margin = sum(w * xl[i,]) * classes[i]
    Q[1] = Q[1] + L(margin)
  } 
```
Мы идем по нашей выборке и находим отступ и ошибку 
```
 margin = sum(w * xi) * yi
    error = L(margin)
    
```
где L наша функция потерь
далее делаем шаг градиентного спуска и находим функционал 
```
w = rule(w, eta, xi, yi) 
    Q[j] = (1 - lambda) * Q[j-1] + lambda * error
```
и наконец условия выхода из цикла
```
if(length(errorIndex)!=0)
      i = sample(errorIndex, 1)
    else
      break
```
```
if (abs(Q[j-1] - Q[j]) / abs(max(Q[j-1], Q[j])) < 1e-5)
      break;
```
### ADALINE
Адаптивны линейный элемент (ADALINE) - это алгоритм использующий такую функцию потерь ![](https://raw.githubusercontent.com/TIR13/ML0/master/line/img/ada_loss.png) Ну и обновление весов находим по формуле формуле ![](https://raw.githubusercontent.com/TIR13/ML0/master/line/img/ada_upd.png).

```
adaline = function(x)  (x - 1) ^ 2
adalineW = function(w, eta, xi, yi) w - eta * (sum(w * xi) - yi) * xi

```
Пример 

![](https://github.com/vinovatvolondemord/ML0/blob/master/img/ADALINE12.png)

![](https://github.com/vinovatvolondemord/ML0/blob/master/img/ADALINE21.png)

