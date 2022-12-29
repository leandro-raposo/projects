########################### step 01 #############################

from google.colab import auth
auth.authenticate_user()

########################### step 02 #############################
import pandas as pd

# https://cloud.google.com/resource-manager/docs/creating-managing-projects
project_id = '[your Cloud Platform project ID]'
sample_count = 2000

row_count = pd.io.gbq.read_gbq('''
  SELECT COUNT(*) as total
  FROM `bigquery-public-data.samples.gsod`''', project_id=project_id).total[0]

df = pd.io.gbq.read_gbq(f'''
  SELECT *
  FROM `bigquery-public-data.samples.gsod`
  WHERE RAND() < {sample_count}/{row_count}''', project_id=project_id)

print(f'Full dataset has {row_count} rows')
