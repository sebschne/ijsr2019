import alg
import matplotlib
matplotlib.use('TKagg')
import seaborn as sns
import matplotlib.pyplot as plt
import csv
import numpy as np
import pandas as pd
learned = open('out.csv','rb')
groundtruth = open('groundtruth.csv','rb')
firstrow = True
header = ['id','hamming','position','discount']
out_array = []
#out_array.append(header)

for row in zip(learned,groundtruth):
    if firstrow:
        firstrow = False
    else:
        learned_array = row[0].strip().split(",")[1:6]
        id = row[0].strip().split(",")[0]
        user_rated = row[1].strip().split(",")[1:6]
        print ("comparing %s with %s") % (learned_array,user_rated)
        #print user_rated
        hamming = alg.hammingDistance(user_rated,
                                                learned_array,False)
        position = alg.position_error(user_rated,
                                                learned_array,False)
        discount = alg.discount_error(user_rated,
                                                learned_array,True)
        out_array.append([id,hamming,position,discount])
#outfile = open('distances.csv','w')
#writer = csv.writer(outfile)
#for i in out_array:
#    writer.writerow(i)
#outfile.close()
data = pd.DataFrame(np.asanyarray(out_array), columns = header)
#data['hamming'].plot.box()
sns.catplot(data=data[data.columns[1:4]], kind = 'box')

plt.show()

