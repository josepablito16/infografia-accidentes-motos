from sklearn.linear_model import LinearRegression
import numpy as np
import matplotlib.pyplot as plt
import csv

importacionesPorMes = {}

with open('../Data/ImportacionesPorDia.csv') as csvfile:
    reader = csv.reader(csvfile, delimiter=',')
    next(reader)
    for row in reader:
        if(int(row[2]) == 2020):
            break
        diaMes = row[1]+'-'+row[2]
        if (not diaMes in importacionesPorMes):
            importacionesPorMes[diaMes] = int(row[3])
        else:
            importacionesPorMes[diaMes] += int(row[3])

# print(importacionesPorMes)
# print()
# print(list(importacionesPorMes.values()))
# print(len(list(importacionesPorMes.values())))


x = np.array(list(range(len(list(importacionesPorMes.values()))))
             ).reshape(-1, 1)
y = np.array(list(importacionesPorMes.values()))

model = LinearRegression()
model.fit(x, y)

#print(model.score(x, y))

y_pred = model.predict(x)
# print(y_pred)

plt.scatter(x, y)
plt.plot(x, y_pred, color='red')
plt.xlabel('Meses del 2011 al 2019')
plt.ylabel('Cantidad de motos importadas')
plt.title('Cantidades de motos importadas por mes, del año 2011 al 2019')
plt.show()


x_new = np.array(list(range(107, 119+1))).reshape(-1, 1)
y_new = model.predict(x_new)
print(f'Importaciones 2020 {sum(y_new)}')

x_new = np.array(list(range(120, 132+1))).reshape(-1, 1)
y_new = model.predict(x_new)
print(f'Importaciones 2021 {sum(y_new)}')
