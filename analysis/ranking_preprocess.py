import pandas as pd
import csv

learned = pd.read_csv("../raw/pl_study/out.csv")
ranked = pd.read_csv("../raw/pl_study/groundtruth.csv")

with open('melted_ranking.csv', mode = 'w') as file:
    file_writer = csv.writer(file, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
    file_writer.writerow(['id', 'learner','category','score'])
    for i in range(1,21):
        for j in range(1,6):
              col = "A" + str(j)
              print learned['id'][i - 1], 'learned', learned[col][i - 1],j
              print ranked['id'][i - 1], 'ranked', ranked[col][i - 1], j
              file_writer.writerow([learned['id'][i - 1], 'learned', learned[col][i - 1], j])
              file_writer.writerow([ranked['id'][i - 1], 'ranked', ranked[col][i - 1], j])


