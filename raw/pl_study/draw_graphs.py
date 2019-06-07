import matplotlib
matplotlib.use('TKagg')
import seaborn as sns
import matplotlib.pyplot as plt
import csv
import numpy as np
import pandas as pd

data =  pd.DataFrame.from_csv('distances.csv')
sns.catplot(data=data[data.columns[0:4]], kind = 'box')
plt.show()