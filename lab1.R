euclideanDistance <- function(u, v)
{
sqrt(sum((u - v)^2))
}
sortObjectsByDist <- function(xl, z, metricFunction =
euclideanDistance)
{
    l <- dim(xl)[1]
    n <- dim(xl)[2] - 1
    # Создаём матрицу расстояний
    distances <- matrix(NA, l, 2)
    for (i in 1:l)
    {
        distances[i, ] <- c(i, metricFunction(xl[i, 1:n], z))
       
    }
    ## Сортируем
    orderedXl <- xl[order(distances[, 2]), ]
    return (orderedXl);
}
# Применяем метод kNN
kNN <- function(xl, z, k)
{
    # Сортируем выборку согласно классифицируемого объекта
    orderedXl <- sortObjectsByDist(xl, z)
    n <- dim(orderedXl)[2] - 1
    # Получаем классы первых k соседей
    classes <- orderedXl[1:k, n + 1]
    counts <- table(classes)
    # Находим класс, который доминирует среди первых k соседей
    class <- names(which.max(counts))

return (class)
}
# Рисуем выборку
colors <- c("setosa" = "red", "versicolor" = "green3",
"virginica" = "blue")

# Классификация одного заданного объекта
z <- c(2,4)
xl <- iris[, 3:5]
xl<- xl[sample(1:nrow(xl),15),]
plot(xl[,1],xl[,2], pch = 21, bg = colors[xl$Species], col
= colors[xl$Species], asp = 1)
class <- kNN(xl, z, k=6)
l<-nrow(xl)
loos<-rep(NA,l)
key<-0
min<-1000
#===================================================================================================================
for (k in 1:l){
	loo<-0
	
	for(i in 1:l)
	{
	    ci <- kNN(xl[-i,],xl[i,1:2],k)
	    if(ci!=xl[i,3]){
		    loo<-loo+1
		}
	    }
	loo<-loo/l
	
loos[k]<-loo

if(  min>loo){

	key=k
	min=loo	
	
		}	

plot(1:l,loos, type="l")
}
print("оптимальное  значение к=")
print(key)
print("LOO=")
print(min)


#пробежатся по к и вывести график loo ;найти min loo
