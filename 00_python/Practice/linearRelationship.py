import pandas as pd
import plotly.express as px
import matplotlib.pyplot as plt
import seaborn as sns

# Linear relationship is a statistical term to reffer the relationship between
# two variables. When the value of variable increases or decreases with the 
# increase or decrease in the value of another variable, then it is nothing but
# a linear relationship.

data = pd.read_csv("https://raw.githubusercontent.com/amankharwal/Website-data/master/Instagram.csv", encoding = 'latin1')
data = data.dropna()
print(data.head())

# view using plotly
figure = px.scatter(data_frame = data, 
                    x="Impressions",
                    y="Likes", 
                    size="Likes", 
                    trendline="ols", 
                    title = "Relationship Between Likes and Impressions")
figure.show()

# view using eaborn
plt.figure(figsize=(10, 8))
plt.style.use('fivethirtyeight')
plt.title("Relationship Bewtween Likes & Impressions")
sns.regplot(x="Impressions", y="Likes", data=data)
plt.show()
