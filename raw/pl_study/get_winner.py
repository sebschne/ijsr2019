import numpy as np
import pickle
import matplotlib
matplotlib.use('TKagg')
import matplotlib.pyplot as plt
import csv

with open("list_of_win_matrices.txt") as f:
    content = f.readlines()
# you may also want to remove whitespace characters like `\n` at the end of each line
content = [x.strip() for x in content] 
total = np.zeros([5,5])
exercises = [u'Dehnung', u'Kardio', u'Kraft', u'Taijii', u'Entspannung']
x = np.array([0,1,2,3,4])
my_xticks = ['John','Arnold','Mavis','Matt']
my_yticks = ['John','Arnold','Mavis','Matt']
plt.xticks(x, exercises)
dict = {}
finalcsv = []
plt.yticks(x, exercises)
for line in content:
    id = line.split("_")[1] + line.split("_")[2]
    a = np.load(line)
    winner_array = map(np.sum, np.load(line.strip()))

    winner_list = [x for (y, x) in sorted(zip(winner_array, exercises))]

    total += a
    winner_list = winner_list[::-1]
    winner_list.append(id)
    print winner_list
    # myString = ",".join(winner_list)
    finalcsv.append(winner_list)
print total



#with open('out.csv', 'w') as resultFile:
#    wr = csv.writer(resultFile, dialect='excel')
#    for row in finalcsv:
#        wr.writerow(row)

heatmap = plt.imshow(total, cmap=plt.cm.Blues,interpolation='nearest')
plt.colorbar(heatmap)
plt.show()
print "displaying the heatmap"
