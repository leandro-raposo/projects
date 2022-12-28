import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import scipy.cluster.vq as sp

# FIFA 18: exploring defenders

# Preprocess
fifa = pd.read_csv('./dataset/fifa_18_dataset.csv')
fifa.head()

fifa['scaled_sliding_tackle'] = sp.whiten(fifa['sliding_tackle'])
fifa['scaled_aggression'] = sp.whiten(fifa['aggression'])

# Fit the data into a hierarchical cluster
distance_matrix = sp.linkage(fifa[['scaled_sliding_tackle', 'scaled_aggression']], method='ward')

# Assign cluster labels to each row of data
fifa['cluster_labels'] = sp.fcluster(distance_matrix, 3, criterion='maxclust')

# Display cluster centers of each cluster
print(fifa[['scaled_sliding_tackle', 'scaled_aggression',
            'cluster_labels']].groupby('cluster_labels').mean())

# Create a scatter plot through seaborn
sns.scatterplot(x='scaled_sliding_tackle', y='scaled_aggression', hue='cluster_labels', data=fifa)
plt.savefig('../images/fifa_cluster.png')
