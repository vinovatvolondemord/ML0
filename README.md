+ [Метрические Алгоритмы классификации](https://github.com/vinovatvolondemord/ML0/blob/master/README.md#%D0%BC%D0%B5%D1%82%D1%80%D0%B8%D1%87%D0%B5%D1%81%D0%BA%D0%B8%D0%B5-%D0%B0%D0%BB%D0%B3%D0%BE%D1%80%D0%B8%D1%82%D0%BC%D1%8B-%D0%BA%D0%BB%D0%B0%D1%81%D1%81%D0%B8%D1%84%D0%B8%D0%BA%D0%B0%D1%86%D0%B8%D0%B8)
    + [Алгоритм	ближайших	соседей](https://github.com/vinovatvolondemord/ML0/blob/master/README.md#%D0%B0%D0%BB%D0%B3%D0%BE%D1%80%D0%B8%D1%82%D0%BC%D0%B1%D0%BB%D0%B8%D0%B6%D0%B0%D0%B9%D1%88%D0%B8%D1%85%D1%81%D0%BE%D1%81%D0%B5%D0%B4%D0%B5%D0%B9)
+ [Байесовский классификатор](https://github.com/vinovatvolondemord/ML0/blob/master/README.md#%D0%B1%D0%B0%D0%B9%D0%B5%D1%81%D0%BE%D0%B2%D1%81%D0%BA%D0%B8%D0%B9-%D0%BA%D0%BB%D0%B0%D1%81%D1%81%D0%B8%D1%84%D0%B8%D0%BA%D0%B0%D1%82%D0%BE%D1%80) 
    - [Наивный Нормальный Байесовский Классификатор](https://github.com/vinovatvolondemord/ML0/blob/master/README.md#%D0%BD%D0%B0%D0%B8%D0%B2%D0%BD%D1%8B%D0%B9-%D0%BD%D0%BE%D1%80%D0%BC%D0%B0%D0%BB%D1%8C%D0%BD%D1%8B%D0%B9-%D0%B1%D0%B0%D0%B9%D0%B5%D1%81%D0%BE%D0%B2%D1%81%D0%BA%D0%B8%D0%B9-%D0%BA%D0%BB%D0%B0%D1%81%D1%81%D0%B8%D1%84%D0%B8%D0%BA%D0%B0%D1%82%D0%BE%D1%80)
## Метрические алгоритмы классификации
### Алгоритм	ближайших	соседей
Необходимо было реализовать	алгоритм __k__ ближайших соседей – __kNN__.	

Суть метода __kNN__ в том что для нашего обьекта __u__ мы рассматриваем его __k__ ближайших соседей и приписываем u к тому классу в котором больше соседей данного класса. 

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
Просто сравнивая результаты работы алгоритмов __kNN__ и __kwNN__ можно увидеть насколько лучше работает kwNN.
![](https://raw.githubusercontent.com/vinovatvolondemord/ML0/master/img/img2.PNG)
![](https://raw.githubusercontent.com/vinovatvolondemord/ML0/master/img/img4.PNG)

В __kNN__ любые шумовые точки влияют на его работу.

__kNN__ очень часто плох в решении каких либо практических задач классификации.Он медленный и неточеный. 

__kwNN__ лучше __kNN__. тем что он более устойчив к выбросам, это улчшает качество классификации. 

## Байесовский классификатор
###  Наивный Нормальный Байесовский Классификатор
![](https://upload.wikimedia.org/wikipedia/commons/thumb/1/18/Bayes%27_Theorem_MMB_01.jpg/220px-Bayes%27_Theorem_MMB_01.jpg)

   ![](https://raw.githubusercontent.com/TIR13/ML0/master/bayes/img/2b.gif) Апостериорная вероятность, т.е. вероятность того, что объект x принадлежит классу y.
   
   ![](https://raw.githubusercontent.com/TIR13/ML0/master/bayes/img/4b.gif) функция правдободобия.
   
   ![](https://raw.githubusercontent.com/TIR13/ML0/master/bayes/img/3b.gif) Априорная вероятность, т.е. вероятность появления класса.

Байесовский алгоритм классификации является классическим и лежит в основе многих методов.
Он опирается на то. что если плотность распредиления классов известна, то алгоритм классификации, имеющий минимальную вероятность ошибок, можно выписать в явном виде.

"Наивный" классификатор будет считать что классы __X__ имеют __n__ независимых признаков. Тогда решающее правило принимает вид:

![](https://camo.githubusercontent.com/8bc9dd137568ab3e91170c9160834d144265d638/687474703a2f2f6c617465782e636f6465636f67732e636f6d2f6769662e6c617465783f61253238782532392533446172672532302535436d61785f25374279253543696e253230592537442532382535436c6e2532382535436c616d6264615f25374279253744505f792532392b25354373756d5f2537426a253344312537442535452537426e2537442535436c6e253238705f253742796a25374425323825354378695f6a253239253239253239)
