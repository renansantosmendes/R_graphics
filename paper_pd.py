import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import random, math
import re
from scipy import stats
from collections import Counter

def read_solutions(file):
	data = pd.read_csv(file, delimiter=',')
	return data.values.tolist()


def dissimilarity(s1,s2):
	# print(s1)
	# print(s2)
	if len(s1) != len(s2):
		print('dimensions error')
		return None
	else:
		total = 0
		for i in range(0,len(s1)):
			total = total + norm_Lp(s1[i],s2[i], 0.1)
		return total

def norm_Lp(x1,x2,p):
	return math.pow(math.fabs(x1 - x2),p)


def pure_diversity(data):
	diversity_matrix = []
	for i in range(0, len(data)):
		line = []
		for j in range(0, len(data)):
			line.append(dissimilarity(data[i],data[j]))
		diversity_matrix.append(line)

	sorted_data = []

	for j in range(0, len(diversity_matrix)):
		sorted_data.append(sorted(diversity_matrix[j]))
	
	PD = []

	for j in range(0, len(sorted_data)):
		PD.append(sorted_data[j][1])
	return sum(PD)


if __name__ == '__main__':
	path = '/home/renansantos/Área de Trabalho/PD/Resultados/Online/r050n12tw10k4/'
	number_of_paretos = 3
	cl_nsga = []
	nsga = []

	for i in range(0,number_of_paretos):
		data1 = read_solutions(path + 'NSGAII-Pareto-'+ str(i)+'.csv')
		print('PD(CL-NSGA-II) = ' + str(pure_diversity(data1)))
		cl_nsga.append(pure_diversity(data1))
	
	path = '/home/renansantos/Área de Trabalho/PD/Resultados/Offline/r050n12tw10k4/'
	number_of_paretos = 3

	for i in range(0,number_of_paretos):
		data2 = read_solutions(path + 'NSGAII-Pareto-'+ str(i)+'.csv')
		print('PD(NSGA-II) = ' + str(pure_diversity(data2)))
		nsga.append(pure_diversity(data2))


	# print(cl_nsga)
	# print(nsga)
	data_algs = [cl_nsga,nsga]


	plt.figure()
	plt.boxplot(data_algs)
	plt.xticks([1, 2], ['CL-NSGA-II','NSGA-II'])
	plt.title('PD')
	plt.savefig('PD' +'.png',bbox_inches='tight')
	plt.show()