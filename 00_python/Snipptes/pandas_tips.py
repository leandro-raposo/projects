import pandas as pd

# creating dataset from list
last_names = ['Connor', 'Connor', 'Reese']
first_names = ['Sarah', 'John', 'Kyle']
df = pd.DataFrame({
  'first_name': first_names,
  'last_name': last_names,
})
df

#drop dupicate rows
df = pd.DataFrame({
  'first_name': ['Sarah', 'John', 'Kyle', 'Joe'],
  'last_name': ['Connor', 'Connor', 'Reese', 'Bonnot'],
})
df.set_index('last_name', inplace=True)

df.loc[~df.index.duplicated(), :]

# explode row containing a dictionary
df = pd.DataFrame({
  'date': ['2022-09-14', '2022-09-15', '2022-09-16'],
  'letter': ['A', 'B', 'C'],
  'dict' : [{ 'fruit': 'apple', 'weather': 'aces'},
            { 'fruit': 'banana', 'weather': 'bad'},
            { 'fruit': 'cantaloupe', 'weather': 'cloudy'}],
})

pd.concat([df.drop(['dict'], axis=1), df['dict'].apply(pd.Series)], axis=1)

# extract values using regex
df = pd.DataFrame({
  'request': ['GET /index.html?baz=3', 'GET /foo.html?bar=1'],
})

df['request'].str.extract('GET /([^?]+)\?', expand=True)

# filter by timestamp
df = pd.DataFrame({
  'time': ['2022-09-14 00:52:00-07:00', '2022-09-14 00:52:30-07:00', 
           '2022-09-14 01:52:30-07:00'],
  'letter': ['A', 'B', 'C'],
})
df['time'] = pd.to_datetime(df.time)
df.set_index('time', inplace=True)

df.loc['2022-09-14':'2022-09-14 00:53']

# ignore column
df = pd.DataFrame({
  'first_name': ['Sarah', 'John', 'Kyle', 'Joe'],
  'last_name': ['Connor', 'Connor', 'Reese', 'Bonnot'],
})

df.loc[:, df.columns!='last_name']

# query by regex
import pandas as pd
df = pd.DataFrame({
  'first_name': ['Sarah', 'John', 'Kyle', 'Joe'],
  'last_name': ['Connor', 'Connor', 'Reese', 'Bonnot'],
})

df[df.last_name.str.match('.*onno.*')]

# rename multiple columns
df = pd.DataFrame({
    'Year': [2016, 2015, 2014, 2013, 2012],
    'Top Animal': ['Giant panda', 'Chicken', 'Pig', 'Turkey', 'Dog']
})

df.rename(columns={
    'Year': 'Calendar Year', 
    'Top Animal': 'Favorite Animal', 
}, inplace=True)
df

# Reshape to have 1 row per value in a list column
df = pd.DataFrame({
  'date': ['9/1/22', '9/2/22', '9/3/22'],
  'action': ['Add', 'Update', 'Delete'],
  'msg_ids': [[1, 2, 3], [], [2, 3]],
})
df.set_index('date', inplace=True)
  
temp_series = df['msg_ids'].apply(pd.Series, 1).stack()
temp_series.index = temp_series.index.droplevel(-1)
temp_series.name = 'msg_id'
new_df = temp_series.to_frame()
new_df.set_index('msg_id', inplace=True)
new_df.loc[~new_df.index.duplicated(), :] # Drop duplicates.

# convert string in TS
pd.Timestamp('9/27/22 06:59').tz_localize('US/Pacific')

# convert TS in string
pd.Timestamp('9/27/22').tz_localize('US/Pacific')

# group timeseries by frequency
df = pd.DataFrame({
  'time': ['2022-09-01 00:00:01-07:00', '2022-09-01 00:00:02-07:00', 
           '2022-09-01 00:01:00-07:00', '2022-09-01 00:02:00-07:00',
           '2022-09-01 00:03:00-07:00', '2022-09-01 00:04:00-07:00',
           '2022-09-01 00:05:00-07:00', '2022-09-01 00:07:00-07:00'], 
  'requests': [1, 1, 1, 1, 1, 1, 1, 1],
})
df['time'] = pd.to_datetime(df.time)

df.groupby(pd.Grouper(key='time', freq='2min')).sum()
