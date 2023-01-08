from sklearn.datasets import fetch_20newsgroups
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.naive_bayes import MultinomialNB
from sklearn.pipeline import make_pipeline
from sklearn.metrics import confusion_matrix
import seaborn as sns
import matplotlib.pyplot as plt

# getting data
data = fetch_20newsgroups()
print(data.target_names)

# definig text categories and train/test samples
categories = ['talk.religion.misc', 'soc.religion.christian', 'sci.space', 'comp.graphics']
train = fetch_20newsgroups(subset='train', categories=categories)
test = fetch_20newsgroups(subset='test', categories=categories)
print(train.data[5])

# creating the model
model = make_pipeline(TfidfVectorizer(), MultinomialNB())
model.fit(train.data, train.target)
labels = model.predict(test.data)
     
mat = confusion_matrix(test.target, labels)
sns.heatmap(mat.T,square=True,annot=True,fmt='d',cbar=False,
            xticklabels=train.target_names,yticklabels=train.target_names)
plt.xlabel('True Label')
plt.ylabel("Predicted Label")
plt.show()

#creation of 
def predict_category(s, train=train,model=model):
    pred = model.predict([s])
    print(train.target_names[pred[0]])


predict_category("sending a payload to the ISS")
# sci.space

predict_category("discussing islam vs atheism")
# soc.religion.christian

predict_category("determining the screen resolution")
# comp.graphics
