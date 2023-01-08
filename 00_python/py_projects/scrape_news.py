import pandas as pd
from GoogleNews import GoogleNews

# scrape the latest trending news about Brazil
news = GoogleNews(period='1d')
news.search("Brazil")
result = news.result()

# Converting dictionary into dataframe with pandas
data = pd.DataFrame.from_dict(result)
data = data.drop(columns=["img"])
data.head()

# printing news
for i in result:
    print("Title : ", i["title"])
    print("News : ", i["desc"])
    print("Read Full News : ", i["link"])
