import awsLogin as al # login aws
import pandas as pd
import nltk
from nltk.corpus import stopwords
import string
from sklearn.metrics import classification_report, confusion_matrix, accuracy_score
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.model_selection import train_test_split

#LEITURA DOS DADOS
client = al.login()
client.download_file("leandro-raposo-projects", "py_projects/emails.csv", "emails.csv")
df = pd.read_csv("emails.csv")

df.head()
df.shape
df.columns
df.drop_duplicates(inplace=True)
print(df.shape)

# to show the number of missing data
print(df.isnull().sum())

# download the stopwords package
nltk.download("stopwords")
def process(text):
    nopunc = [char for char in text if char not in string.punctuation]
    nopunc = ''.join(nopunc)

    clean = [word for word in nopunc.split() if word.lower() not in stopwords.words('english')]
    return clean

# to show the tokenization
df['text'].head().apply(process)
message = CountVectorizer(analyzer=process).fit_transform(df['text'])

#split the data into 80% training and 20% testing
xtrain, xtest, ytrain, ytest = train_test_split(message, df['spam'], test_size=0.20, random_state=0)
print(message.shape)

# create and train the Naive Bayes Classifier
from sklearn.naive_bayes import MultinomialNB
classifier = MultinomialNB().fit(xtrain, ytrain)
print(classifier.predict(xtrain))
print(ytrain.values)

# Evaluating the model on the training data set
from sklearn.metrics import classification_report, confusion_matrix, accuracy_score
pred = classifier.predict(xtrain)
print(classification_report(ytrain, pred))
print()
print("Confusion Matrix: \n", confusion_matrix(ytrain, pred))
print("Accuracy: \n", accuracy_score(ytrain, pred))

#print the predictions
print(classifier.predict(xtest))

#print the actual values
print(ytest.values)

# Evaluating the model on the training data set
pred = classifier.predict(xtest)
print(classification_report(ytest, pred))
print()
print("Confusion Matrix: \n", confusion_matrix(ytest, pred))
print("Accuracy: \n", accuracy_score(ytest, pred))
