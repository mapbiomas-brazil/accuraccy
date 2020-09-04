import glob
import pandas as pd 
import sys
import math

import warnings

from os import path, makedirs
import csv
import numpy as np
import matplotlib.pyplot as plt

from matplotlib.colors import LinearSegmentedColormap
from sklearn.metrics import confusion_matrix, precision_score, recall_score, accuracy_score, f1_score
from sklearn.utils.multiclass import unique_labels

input_dir = sys.argv[1]
output_dir = sys.argv[2]

POINTS_STRATA_FILE = 'points_strata.csv'
STRATA_FILE = 'strata.csv'

IGNORED_CLASSES = [0,31,32,30,25,23,5,29]
ALL_CLASSES = {
	0: {
		"l1_val": 0,
		"l1": "6. Não Observado",
		"l2_val": 0,
		"l2": "6. Não Observado",
		"l3_val": 0,
		"l3": "6. Não Observado"
	},
	3: {
		"l1_val": 1,
		"l1": "1. Floresta",
		"l2_val": 2,
		"l2": "1.1. Floresta Natural",
		"l3_val": 3,
		"l3": "1.1.1. Formação Florestal"
	},
	4: {
		"l1_val": 1,
		"l1": "1. Floresta",
		"l2_val": 2,
		"l2": "1.1. Floresta Natural",
		"l3_val": 4,
		"l3": "1.1.2. Formação Savânica"
	},
	5: {
		"l1_val": 1,
		"l1": "1. Floresta",
		"l2_val": 2,
		"l2": "1.1. Floresta Natural",
		"l3_val": 5,
		"l3": "1.1.3. Mangue"
	},
	9: {
		"l1_val": 1,
		"l1": "1. Floresta",
		"l2_val": 9,
		"l2": "1.2. Floresta Plantada",
		"l3_val": 9,
		"l3": "1.2. Floresta Plantada"
	},
	11: {
		"l1_val": 10,
		"l1": "2. Formação Natural não Florestal",
		"l2_val": 11,
		"l2": "2.1. Área Úmida Natural não Florestal",
		"l3_val": 11,
		"l3": "2.1. Área Úmida Natural não Florestal"
	},
	12: {
		"l1_val": 10,
		"l1": "2. Formação Natural não Florestal",
		"l2_val": 12,
		"l2": "2.2. Formação Campestre (Campo)",
		"l3_val": 12,
		"l3": "2.2. Formação Campestre (Campo)"
	},
	13: {
		"l1_val": 10,
		"l1": "2. Formação Natural não Florestal",
		"l2_val": 13,
		"l2": "2.5. Outra Formação não Florestal",
		"l3_val": 13,
		"l3": "2.5. Outra Formação não Florestal"
	},
	15: {
		"l1_val": 14,
		"l1": "3. Agropecuária",
		"l2_val": 15,
		"l2": "3.1. Pastagem",
		"l3_val": 15,
		"l3": "3.1. Pastagem"
	},
	19: {
		"l1_val": 14,
		"l1": "3. Agropecuária",
		"l2_val": 18,
		"l2": "3.2. Agricultura",
		"l3_val": 19,
		"l3": "3.2.1. Lavoura Temporária"
	},
	20: {
		"l1_val": 14,
		"l1": "3. Agropecuária",
		"l2_val": 18,
		"l2": "3.2. Agricultura",
		"l3_val": 20,
		"l3": "3.2.2. Cultura Semi-Perene"
	},
	21: {
		"l1_val": 14,
		"l1": "3. Agropecuária",
		"l2_val": 21,
		"l2": "3.3 Mosaico de Agricultura ou Pastagem",
		"l3_val": 21,
		"l3": "3.3 Mosaico de Agricultura ou Pastagem"
	},
	23: {
		"l1_val": 22,
		"l1": "4. Área não Vegetada",
		"l2_val": 23,
		"l2": "4.3. Praia e Duna",
		"l3_val": 23,
		"l3": "4.3. Praia e Duna"
	},
	24: {
		"l1_val": 22,
		"l1": "4. Área não Vegetada",
		"l2_val": 24,
		"l2": "4.1. Infraestrutura Urbana",
		"l3_val": 24,
		"l3": "4.1. Infraestrutura Urbana"
	},
	25: {
		"l1_val": 22,
		"l1": "4. Área não Vegetada",
		"l2_val": 25,
		"l2": "4.4. Outra Área não Vegetada",
		"l3_val": 25,
		"l3": "4.4. Outra Área não Vegetada"
	},
	29: {
		"l1_val": 10,
		"l1": "2. Formação Natural não Florestal",
		"l2_val": 29,
		"l2": "2.4. Afloramento Rochoso",
		"l3_val": 29,
		"l3": "2.4. Afloramento Rochoso"
	},
	30: {
		"l1_val": 22,
		"l1": "4. Área não Vegetada",
		"l2_val": 30,
		"l2": "4.2. Mineração",
		"l3_val": 30,
		"l3": "4.2. Mineração"
	},
	31: {
		"l1_val": 26,
		"l1": "5. Corpo D'água",
		"l2_val": 31,
		"l2": "5.2. Aquicultura",
		"l3_val": 31,
		"l3": "5.2. Aquicultura"
	},
	32: {
		"l1_val": 10,
		"l1": "2. Formação Natural não Florestal",
		"l2_val": 32,
		"l2": "2.3. Apicum",
		"l3_val": 32,
		"l3": "2.3. Apicum"
	},
	33: {
		"l1_val": 26,
		"l1": "5. Corpo D'água",
		"l2_val": 33,
		"l2": "5.1. Rio, Lago e Oceano",
		"l3_val": 33,
		"l3": "5.1. Rio, Lago e Oceano"
	},
	36: {
		"l1_val": 14,
		"l1": "3. Agropecuária",
		"l2_val": 18,
		"l2": "3.2. Agricultura",
		"l3_val": 36,
		"l3": "3.2.2. Lavoura Perene"
	},

}

def get_classes(df, level='l3'):

	class_values = {}
	class_names = {}

	clas_classes = pd.Index(df['classification'].unique())
	ref_classes = pd.Index(df['reference'].unique())

	acc_classes = clas_classes.intersection(ref_classes)

	val_remap = {}

	for value in ALL_CLASSES.keys():
		if (value not in IGNORED_CLASSES and (value in acc_classes)):
			
			val_key = "%s_val" % (level)
			new_val = ALL_CLASSES[value][val_key]
			class_name = ALL_CLASSES[value][level]

			val_remap[value] = new_val
			class_values[new_val] = True
			class_names[class_name] = True

	df = df[df['classification'].isin(val_remap.keys())]
	df = df[df['reference'].isin(val_remap.keys())]

	df['classification'] = df['classification'].map(val_remap)
	df['reference'] = df['reference'].map(val_remap)
	class_values = list(class_values.keys())
	class_names = list(class_names.keys())

	return df, class_values, class_names

def read_csvs():
	
	df_array = []
	points_strata = pd.read_csv(POINTS_STRATA_FILE)

	for csv in glob.glob(input_dir + "/*.csv"):
		df_csv = pd.read_csv(csv)
		print("Reading " + csv, df_csv.shape)
		df_array.append( df_csv )

	df = pd.concat(df_array)
	df = pd.merge(df, points_strata, how='inner', on='TARGET_FID')
	
	df = calculate_prob(df)

	return df

def classification_report_shinny(df, level, class_names, class_values, region, year):
	
	header = []
	rows = []
	footer = []

	y_true = df[['reference']].to_numpy().flatten()
	y_pred = df[['classification']].to_numpy().flatten()

	sample_weight = 1 / df[['PESO_VOT']].to_numpy().flatten()
	matrix = confusion_matrix(y_true, y_pred, sample_weight=sample_weight)

	glob_acc, glob_se = global_acc(df)

	user_acc, prod_acc, user_se, prod_se = user_prod_acc(df, class_values)
	refarea_prop, refarea_se = refarea_pop(df, class_values)
	map_bias, map_bias_se = calc_map_bias(df, class_values)

	matrix = matrix.transpose()
	estimated_pop = sample_weight.sum()

	matrix = (matrix / estimated_pop)

	header = [' ']
	header += class_names
	header += ["total", "population", "population bias"]
	header += ["user's accuracy", "user stderr"]
	header += ["error of comission", "area dis"]

	total_col = matrix.sum(axis=0)
	total_row = matrix.sum(axis=1)

	user_acc_tot = np.sum(user_acc * total_row)
	user_se_tot = np.sum(user_se * total_row)

	prod_acc_tot = np.sum(prod_acc * total_col)
	prod_se_tot = np.sum(prod_se * total_col)

	quantity_dis = np.absolute(total_row - total_col)
	allocation_dis = 2 * np.minimum((total_row - np.diagonal(matrix)), (total_col - np.diagonal(matrix)))

	quantity_dis_tot = np.sum(quantity_dis) / 2
	allocation_dis_tot = np.sum(allocation_dis) / 2

	#print(str(level) + ';' + region + ';' + str(glob_acc*100) + ';' + str(quantity_dis_tot*100) + ';' + str(allocation_dis_tot*100))

	fmt = '.3f'
	metric_fmt = '.3f'
	for i in range(matrix.shape[0]):
		row = [class_names[i]]
		for j in range(matrix.shape[1]):
			row.append(matrix[i, j])
		row.append(total_row[i])
		row.append(total_row[i])
		row.append(map_bias[i])
		row.append(user_acc[i])
		row.append(user_se[i])
		row.append((1 - user_acc[i]))
		row.append( quantity_dis[i] )

		rows.append(row)

	na_fill = ['NA', 'NA', 'NA', 'NA', 'NA', 'NA']
	
	total = ['total']
	total += ( col for col in total_col.tolist())
	total += [ np.sum(refarea_prop), np.sum(refarea_prop), 0, user_acc_tot, user_se_tot, (1-user_acc_tot), quantity_dis_tot ]

	r_adj_pop = ['adj population']
	r_adj_pop += ( col for col in refarea_prop)
	r_adj_pop += [np.sum(refarea_prop)]
	r_adj_pop += na_fill

	r_adj_pop_se = ['adj pop stdErr']
	r_adj_pop_se += ( col for col in refarea_se)
	r_adj_pop_se += [np.sum(refarea_se)]
	r_adj_pop_se += na_fill
	
	r_prod_acc = ["producer's accuracy"]
	r_prod_acc += ( col for col in prod_acc)
	r_prod_acc += [prod_acc_tot]
	r_prod_acc += na_fill

	r_prod_se = ["prod stdErr"]
	r_prod_se += ( col for col in prod_se)
	r_prod_se += [prod_se_tot]
	r_prod_se += na_fill

	r_omiss = ["error of omission"]
	r_omiss += ( (1 - col) for col in prod_se)
	r_omiss += [(1 - prod_acc_tot)]
	r_omiss += na_fill

	r_alloc_dis = ["alloc dis"]
	r_alloc_dis += ( col for col in allocation_dis)
	r_alloc_dis += [allocation_dis_tot]
	r_alloc_dis += na_fill

	result = [[" "]]
	result += [[" "]]
	result += [[" ANO: " + str(year) + " "]]
	result += [header]
	result += rows
	result += [total]
	result += [r_adj_pop]
	result += [r_adj_pop_se]
	result += [r_prod_acc]
	result += [r_prod_se]
	result += [r_omiss]
	result += [r_alloc_dis]

	return result

def save_csv(output_filename, data):
	with open(output_filename, encoding='latin-1', mode='w',newline='') as output_file:
		print("Generating " + output_filename)
		csv_writer = csv.writer(output_file, delimiter=';', quoting=csv.QUOTE_NONNUMERIC)
		csv_writer.writerows(data)

def calculate_prob(df):
	strata = pd.read_csv(STRATA_FILE)
	df = pd.merge(df, strata, how='inner', on="strata_id")

	samples = df['strata_id'].value_counts().rename_axis('strata_id').reset_index(name='n_samp')
	df = pd.merge(samples, df, on='strata_id')
	#df['PESO_VOT'] = df['n_samp'] / df['pop']

	biomes = df['BioNB'].unique()

	for biome in biomes:
		biome_filter = (df['BioNB'] == biome)
		n_samples, _ = df[biome_filter & (df['AMOSTRAS'] != 'treinamento')].shape
		t_samples, _ = df[biome_filter].shape
		 
		prob_biome_adjs = n_samples / t_samples
		df.loc[biome_filter, 'PESO_VOT'] = df[biome_filter]['PESO_VOT'] * prob_biome_adjs

	return df[df['AMOSTRAS'] != 'treinamento']

def mkdirp(path):
	try:
		makedirs(path)
	except:
		pass

def accuracy_assessment_all(df, biome='BRASIL'):
	
	for level in ['l2', 'l3', 'l1']:

		acc_output_dir = path.join(output_dir, level)
		mkdirp(acc_output_dir)

		#dic_biomes = {1:'Amazônia', 2:'Mata Atlântica', 3:'Pantanal', 4:'Cerrado', 5:'Caatinga', 6:'Pampa',7:'BRASIL'}

		#biome_nm = dic_biomes[biome]
		biome_nm = biome

		output_filename = path.join(acc_output_dir, ''.join(['acc_int_', str(biome_nm), '.csv']))

		years = df['year'].unique()
		years.sort()

		result = [["Thu Jan 11 18:27:46 2018   Matrizes de confusão anuais para " + biome_nm + "  -  Coleção 4.0 (CLASSIFICAÇÃO + FILTRO + INTEGRAÇÃO)  "]]
		result += [[" "]]
		result += [["Corpo da tabela contém distribuição de frequências cruzadas da amostra."]]
		result += [["Marginais:"]]
		result += [["Totais: A coluna 'total' contém a soma das amostras de cada classe mapeada ou a estatística de linha para todas as classes. A linha 'total' contém a soma das amostras em cada classe observada ou a respectiva estatística de coluna para todas as classes. "]]
		result += [["Número de píxeis na população - A coluna 'population' contém o número de píxeis totais de cada classe mapeada. A 'adj population' contém as estimativas no número de píxeis na população corrigida pela matriz de erros. A coluna 'bias' contém a estimativa do viés relativo da área mapeada. A linha 'adj pop stdErr' contém uma estimativa do erro padrão do número de píxeis corrigido. Usado para representar a incerteza d ou construir intervalos de confiança. A estimativa do erro padrão é confiável apenas quando n>30./nAcurácias - Acurácia do usuário (user's accuracy): estimativa da fração de píxeis (ou área) de cada classe do mapa que está corretamente mapeada. Acurácia do produtor (producer's accuracy): estimativa da fração de píxeis (ou área) de cada classe que foi corretamente mapeada. A coluna 'user stderr' contém a estimativa do erro padrão da acurácia do usuário"]]
		result += [["Erros - Erro de comissão e omissão: são os complementares da acurácia do usuário e produtor"]]
		result += [["Decomposição do erro - O erro é decomposto em 'discordância de área' (area dis) e 'discordância de alocação' (alloc dis). A soma deles é o erro total."]]

		for year in years:
			result += accuracy_assessment(df, level, year, biome)

		result += accuracy_assessment(df, level, 'Todos', biome)

		save_csv(output_filename, result)

def accuracy_assessment(df, level='l3', year='Todos', biome='BRASIL'):

	df = df.copy(deep=True)
	
	if year != 'Todos':
		df = df[df['year'] == year]

	if biome != 'BRASIL':
		df = df[df['BioNB'] == biome]
	
	df, class_values, class_names = get_classes(df, level)

	return classification_report_shinny(df, level, class_names, class_values, biome, year)

def population_estimation(df):
	sample_weight = 1 / df[['PESO_VOT']].to_numpy().flatten()
	return sample_weight.sum()

def covariance(x, y):
	if x.size < 1:
		x_mean = np.mean(x)
		y_mean = np.mean(y)

		return np.sum((x - x_mean) * (y - y_mean) / (x.size - 1))
	else:
		return 0.0

def user_prod_se(df, class_val, user_acc, prod_acc, map_total, ref_total):
	
	user_var = 0
	prod_var = 0

	user_se = 0
	prod_se = 0

	for name, df_strata in df.groupby('strata_id'):
		ref_val_s = df_strata['reference'].to_numpy()
		map_val_s = df_strata['classification'].to_numpy()

		map_total_s = np.where((map_val_s == class_val), 1, 0)
		map_correct_s = np.where(np.logical_and((map_val_s == class_val),(map_val_s == ref_val_s)), 1, 0)

		ref_total_s = np.where((ref_val_s == class_val), 1, 0)
		ref_correct_s = np.where(np.logical_and((ref_val_s == class_val),(map_val_s == ref_val_s)), 1, 0)
		
		nsamples_s, _ = df_strata.shape
		population_s = population_estimation(df_strata)

		user_var += math.pow(population_s,2) * (1 - nsamples_s/population_s) \
									* ( math.pow(	np.var(map_correct_s) , 2) \
											+ math.pow(user_acc,2) * math.pow( np.var(map_total_s) , 2) \
											- 2 * user_acc * covariance(map_total_s, map_correct_s) \
 										) / nsamples_s

		prod_var += math.pow(population_s,2) * (1 - nsamples_s/population_s) \
									* ( math.pow(	np.var(ref_correct_s) , 2) \
											+ math.pow(prod_acc,2) * math.pow( np.var(ref_total_s) , 2) \
											- 2 * prod_acc * covariance(ref_total_s, ref_correct_s) \
 										) / nsamples_s

	if (map_total !=0):
		user_var = 1 / math.pow(map_total,2) * user_var
		user_se = 1.96 * math.sqrt(user_var)

	if (ref_total !=0):
		prod_var = 1 / math.pow(ref_total,2) * prod_var
		prod_se = 1.96 * math.sqrt(prod_var)

	return user_se, prod_se

def global_se(df, mask, population):
	glob_var = 0

	for name, df_strata in df.groupby('strata_id'):
		ref_val_s = df['reference'].to_numpy()
		map_val_s = df['classification'].to_numpy()

		map_correct_s = np.where(mask, 1, 0)

		nsamples_s, _ = df_strata.shape
		population_s = population_estimation(df_strata)
		
		glob_var += math.pow(population_s,2) * (1 - nsamples_s/population_s) \
								* np.var(map_correct_s) / nsamples_s

	glob_var = (1 / math.pow(population,2)) * glob_var
	glob_se = 1.96 * math.sqrt(glob_var)

	return glob_se

def calc_map_bias(df, class_values):

	map_bias_arr = []
	map_bias_se_arr = []

	ref_val = df['reference'].to_numpy()
	map_val = df['classification'].to_numpy()
	samp_weight = 1 / df['PESO_VOT'].to_numpy()

	population = population_estimation(df)

	for class_val in class_values:
	
		map_mask = np.logical_and((map_val == class_val), (ref_val != class_val))
		map_comission_prop = np.sum(np.where(map_mask, 1, 0) * samp_weight) / population

		ref_mask = np.logical_and((ref_val == class_val), (map_val != class_val))
		map_omission_prop = np.sum(np.where(ref_mask, 1, 0) * samp_weight) / population

		map_bias = (map_omission_prop - map_comission_prop)
		
		se_mask = np.logical_xor(ref_mask,map_mask)
		map_bias_se = global_se(df, se_mask, population)

		map_bias_arr.append(map_bias)
		map_bias_se_arr.append(map_bias_se)

	return map_bias_arr, map_bias_se_arr

def refarea_pop(df, class_values):

	refarea_prop_arr = []
	refarea_se_arr = []

	ref_val = df['reference'].to_numpy()
	map_val = df['classification'].to_numpy()
	samp_weight = 1 / df['PESO_VOT'].to_numpy()

	population = population_estimation(df)

	for class_val in class_values:
	
		ref_mask = (ref_val == class_val)
		refarea = np.sum(np.where(ref_mask, 1, 0) * samp_weight)

		refarea_prop = (refarea / population)
		refarea_se = global_se(df, ref_mask, population)

		refarea_prop_arr.append(refarea_prop)
		refarea_se_arr.append(refarea_se)

	return refarea_prop_arr, refarea_se_arr

def global_acc(df):

	ref_val = df['reference'].to_numpy()
	map_val = df['classification'].to_numpy()
	samp_weight = 1 / df['PESO_VOT'].to_numpy()
	
	mask_correct = (map_val == ref_val)
	map_correct = np.sum(np.where(mask_correct, 1, 0) * samp_weight)
	population = population_estimation(df)

	glob_acc = (map_correct / population)

	glob_acc = glob_acc
	glob_se = global_se(df, mask_correct, population)

	return glob_acc, glob_se

def user_prod_acc(df, class_values):

	user_acc_arr = []
	prod_acc_arr = []
	user_se_arr = []
	prod_se_arr = []

	ref_val = df['reference'].to_numpy()
	map_val = df['classification'].to_numpy()
	samp_weight = 1.0 / df['PESO_VOT'].to_numpy()

	for class_val in class_values:

		map_total = np.sum(np.where((map_val == class_val), 1, 0) * samp_weight)
		map_correct = np.sum(np.where(np.logical_and((map_val == class_val),(map_val == ref_val)), 1, 0) * samp_weight)

		ref_total = np.sum(np.where((ref_val == class_val), 1, 0) * samp_weight)
		ref_correct = np.sum(np.where(np.logical_and((ref_val == class_val),(map_val == ref_val)), 1, 0) * samp_weight)

		user_acc = 0
		if map_total > 0:
			user_acc = map_correct / map_total

		prod_acc = 0
		if ref_total > 0:
			prod_acc = ref_correct / ref_total

		user_se, prod_se = user_prod_se(df, class_val, user_acc, prod_acc, map_total, ref_total)

		user_acc_arr.append(user_acc)
		prod_acc_arr.append(prod_acc)
		user_se_arr.append(user_se)
		prod_se_arr.append(prod_se)

	return user_acc_arr, prod_acc_arr, user_se_arr, prod_se_arr

def mod_BioNB(df):
	df.loc[ (df['BioNB']==1),'BioNB'] = 'Amazônia'
	df.loc[ (df['BioNB']==2),'BioNB'] = 'Mata Atlântica'
	df.loc[ (df['BioNB']==3),'BioNB'] = 'Pantanal'
	df.loc[ (df['BioNB']==4),'BioNB'] = 'Cerrado'
	df.loc[ (df['BioNB']==5),'BioNB'] = 'Caatinga'
	df.loc[ (df['BioNB']==6),'BioNB'] = 'Pampa'

	return df

def config_class(df):

	#Global

	df.loc[ (df['classification'] == 20) | (df['classification'] == 39) | (df['classification'] == 41), 'classification'] = 19 #Convert areas mapped as 20,39,41 to 21
	
	df.loc[ (df['classification'] == 21) & (df['reference'].isin([15,19,20,36])), 'reference'] = 21 #Convert reference from 15,19,20,36 to reference 21 for areas mapped as 21
	
	#Pampa 

	df.loc[ (df['BioNB']== 'Pampa') & (df['reference'] == 15),'reference'] = 19 #In Pampa, convert reference class 15 to 19 (Crop).
	df.loc[ (df['BioNB']=='Pampa') & (df['classification'] == 25) & (df['reference'] == 23), 'reference'] = 25 #In Pampa, convert reference 25 to 23 in areas mapped as 25
	df.loc[ (df['BioNB']== 'Pampa') & (df['classification'] == 12) & (df['reference'] == 13),'reference'] = 12 #In Pampa, convert reference 13 to 12 in areas mapped as 12

	#Mata Atântica

	df.loc[ (df['BioNB']== 'Mata Atlântica') & (df['classification'] == 11) & (df['reference'] == 13),'classification'] = 11

	#Pantanal

	df.loc[ (df['BioNB']== 'Pantanal') & (df['classification'] == 11) & (df['reference'] == 12),'reference'] = 11 #In Pantanal, convert reference 12 to 11 in areas mapped as 11

	#Amazônia

	df.loc[ (df['BioNB']=='Amazônia') & (df['classification'] == 12) & (df['reference'] == 13), 'reference'] = 12 #In Amazon, convert reference 13 to 12 in areas mapped as 12
	df.loc[ (df['BioNB']=='Amazônia') & (df['classification'] == 4) & (df['reference'] == 13), 'reference'] = 4 #In Amazon, convert reference 13 to 4 in areas mapped as 4

	#Cerrado

	df.loc[ (df['BioNB']=='Cerrado') & (df['classification'] == 12) & (df['reference'] == 13), 'reference'] = 12 #In Cerrado, convert reference 13 to 12 in areas mapped as 12
	df.loc[ (df['BioNB']=='Cerrado') & (df['classification'] == 25) & (df['reference'] == 23), 'reference'] = 25 #In Cerrado, convert reference 25 to 23 in areas mapped as 25

	return df

df = read_csvs()

total_points = population_estimation(df)

df = mod_BioNB(df)

df = config_class(df)

regions = df['BioNB'].unique().tolist()
regions.append('BRASIL')

for region in regions:
	accuracy_assessment_all(df, region)
