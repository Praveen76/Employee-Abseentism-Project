# -*- coding: utf-8 -*-
"""
Created on Tue Aug 28 20:42:17 2018

@author: praveenanwla
"""

# -*- coding: utf-8 -*-
"""
Created on Tue Jul 31 21:53:33 2018

@author: praveenanwla
"""


import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
%matplotlib inline


from sklearn.preprocessing import StandardScaler
from sklearn.externals import joblib
import seaborn as sns
from sklearn import ensemble
from sklearn.cross_validation import train_test_split
from sklearn.metrics import mean_absolute_error
from sklearn.grid_search import GridSearchCV
from datetime import datetime

import os
os.getcwd()
os.chdir('C:/Project2') # Set wiorking Directory

os.getcwd()

data = pd.read_csv("Absenteeism_at_work_Project.csv")

data.shape



data.columns
##Check count of Missing Values
for col_name in data.columns: 
    print ("column:",col_name,".Missing:",sum(data[col_name].isnull()))


#==============================================================================
# Data Preprocessing
#==============================================================================
#Remove entries where 'Absenteeism time in hours' are less than 0 and more than 8

data =  data[(data['Absenteeism time in hours']> 0) & (data ['Absenteeism time in hours'] <=8)]

#Check out Null Values
pd.isnull(data).sum()
sns.heatmap(data.isnull(),yticklabels=False , cbar=False ,cmap='viridis')

data.describe()
data.columns
data.dtypes

 #Preprocess data['Reason for absence'] variable ; impute it with number 26; 
 #26 level is unjustified reason so I can merge these tow levels

np.unique(data['Reason for absence'])

data['Reason for absence'].fillna(26, inplace=True)

pd.isnull(data['Reason for absence']).sum()

data['Reason for absence']=data['Reason for absence'].astype(int)

data['Reason for absence'].dtypes



##Preprocess data$Month of absence variable , Impute it with Mode

np.unique(data['Month of absence'])

data['Month of absence'] = data['Month of absence'].fillna(data['Month of absence'].mode()[0])

pd.isnull(data['Month of absence']).sum()

data['Month of absence']=data['Month of absence'].astype(int)

data['Month of absence'].dtypes

  #Preprocess data['Transportation expense '] variable ; Impute it with mean
  
data['Transportation expense'].head()


pd.isnull(data['Transportation expense']).sum()

data['Transportation expense'].fillna(data['Transportation expense'].mean(), inplace=True)

#Preprocess data['Distance from Residence to Work'] variable ; Impute it with mean

data['Distance from Residence to Work'].head()
data['Distance from Residence to Work'].dtypes
pd.isnull(data['Distance from Residence to Work']).sum()

data['Distance from Residence to Work'].fillna(data['Distance from Residence to Work'].mean(), inplace=True)


#Preprocess data['Service time'] variable ; Impute it with mean


data.dtypes

data['Service time'].head()

np.unique(data['Service time'])
data['Service time'].fillna(data['Service time'].mean(), inplace=True)



#Preprocess data['Age'] variable ; Impute it with mean

data['Age'].head()
data['Age'].dtypes

np.unique(data['Age'])

pd.isnull(data['Age']).sum()
data['Age'].fillna(data['Age'].mean(), inplace=True)

data.dtypes

#Preprocess data['Work load Average/day'] variable ; Drop NAs
import string
data.dtypes

data.columns
pd.isnull(data['Work load Average day ']).sum()


#data['Work load Average day '] =  data['Work load Average day '].dropna()

#data['Work load Average day ']=data['Work load Average day '].astype(str)

#==============================================================================
# def xyz (data):
#     #data = str(data)
#     #data=data.replace(',','')
#     data =data.astype(int)
#     return data
# 
# data['Work load Average day ']= data['Work load Average day '].apply(xyz)
# 
# data['Work load Average day ']=data['Work load Average day '].astype(int)
# 
# data['Work load Average day '].head()
# 
# pd.isnull(data['Work load Average day ']).sum()
# 
# data['Work load Average day ']=data['Work load Average day '].fillna(0)
# 
# data['Work load Average day '].head()
# 
#==============================================================================
#==============================================================================
# def xyz (data):
#     data = data.astype(int)
#     print (data)
#     return data
# 
# data['Work load Average day ']=data['Work load Average day '].apply(xyz)
# 
#==============================================================================
# #==============================================================================
# data['Work load Average day ']=data['Work load Average day '].astype(float)
# 
# #del data['Work load Average day ']
# 
#==============================================================================

##Hit target,Impute with mean
data['Hit target'].head()

np.unique(data['Hit target'])
data['Hit target'].fillna(data['Hit target'].mean(), inplace=True)

data['Hit target']= data['Hit target'].astype(int)

##Disciplinary failure  ; Impute it with Mode

data['Disciplinary failure'].head()

np.unique(data['Disciplinary failure'])

data['Disciplinary failure'] = data['Disciplinary failure'].fillna(data['Disciplinary failure'].mode()[0])
np.unique(data['Disciplinary failure'])
data['Disciplinary failure'].head()

#Impute Education with Mode

data['Education'].head()

np.unique(data['Education'])

data['Education'] = data['Education'].fillna(data['Education'].mode()[0])
data['Education'].isnull().sum()

data['Education']= data['Education'].astype(int)

##Son; Impute with Mode

data['Son'].head()
np.unique(data['Son'])

data['Son'] = data['Son'].fillna(data['Son'].mode()[0])
data['Son'] = data['Son'].astype(int)


##Social drinker;Impute with Mode

data['Social drinker'].head()
np.unique(data['Social drinker'])

data['Social drinker'] = data['Social drinker'].fillna(data['Social drinker'].mode()[0])
data['Social drinker'] = data['Social drinker'].astype('category')

##Social smoker ;Impute with Mode

data['Social smoker'].head()
np.unique(data['Social smoker'])
data['Social smoker'] = data['Social smoker'].fillna(data['Social smoker'].mode()[0])
data['Social smoker'] = data['Social smoker'].astype('category')

#Pet;Impute with Mode

data['Pet'].head()
np.unique(data['Pet'])

data['Pet'] = data['Pet'].fillna(data['Pet'].mode()[0])
data['Pet'] = data['Pet'].astype(int)


#Weight;impute with mean
data.dtypes
data['Weight'].fillna(data['Weight'].mean(), inplace=True)

np.unique(data['Weight'])
data['Weight'] = data['Weight'].astype(int)

#Height ; Impute with mean

data.dtypes
data['Height'].fillna(data['Height'].mean(), inplace=True)

np.unique(data['Height'])
data['Height'] = data['Height'].astype(int)

#Body mass index  ;Impute with Mean

data.dtypes

np.unique(data['Body mass index'])
data['Body mass index'].fillna(data['Body mass index'].mean(), inplace=True)

data['Body mass index'] = data['Body mass index'].astype(int)


#Absenteeism time in hours ; Drop NAs

np.unique(data['Absenteeism time in hours'])
pd.isnull(data['Absenteeism time in hours']).sum()

data['Absenteeism time in hours']=data['Absenteeism time in hours'].fillna(0)

data['Absenteeism time in hours'] = data['Absenteeism time in hours'].astype(int)


#convert other variables also in int
data['Transportation expense'] = data['Transportation expense'].astype(int)

data['Distance from Residence to Work'] =data['Distance from Residence to Work'] .astype(int)
data['Service time'] =data['Service time'].astype(int)
data['Age'] = data['Age'].astype(int)

########$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

data.dtypes


data['Education'].head()
data['Education'].head()

data['Education'] = data['Education'].astype(int)



d = data.copy()

# Basic statistics and data preparation-
# Factors are in interger format , so for the sake of analysis I have changed them to factor format.
  
# converting variables to factors-
data.columns
  
Cat_Var = ['Reason for absence', 'Month of absence', 'Day of the week',
       'Seasons', 'Disciplinary failure','Education', 'Son', 'Social drinker',
       'Social smoker', 'Pet']
           

data[Cat_Var]=data[Cat_Var].apply(lambda x: x.astype('category'))

data.dtypes

data['Disciplinary failure'].head()



data.columns
    


data['Reason for absence']=data['Reason for absence'].astype(str)

data['Reason for absence'] = data['Reason for absence'].replace({'0':'infectious,parasitic diseases','1':'Certain infectious and parasitic diseases','2':'Neoplasms',
'3':'Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism',
'4':'Endocrine, nutritional and metabolic diseases','5':'Mental and behavioural disorders','6':'Diseases of the nervous system','7':'Diseases of the eye and adnexa',
'8':'Diseases of the ear and mastoid process','9':'Diseases of the circulatory system','10':'Diseases of the respiratory system', 
'11':'Diseases of the digestive system','12':'Diseases of the skin and subcutaneous tissue', 
'13':'Diseases of the musculoskeletal system and connective tissue','14':'Diseases of the genitourinary system','15':'Pregnancy, childbirth and the puerperium', 
'16':'Certain conditions originating in the perinatal period','17':'Congenital malformations, deformations and chromosomal abnormalities',
'18':'Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified','19':'Injury, poisoning and certain other consequences of external causes','20':'External causes of morbidity and mortality',
'21':'Factors influencing health status and contact with health services','22':'patient follow-up','23':'medical consultation','24':'blood donation',
'25':'laboratory examination','26':'unjustified absence','27':'physiotherapy','28':'dental consultation'})


data['Month of absence']=data['Month of absence'].astype(str)

data['Month of absence']= data['Month of absence'].replace({'0':None,'1':'Jan','2':'Feb','3':'Mar','4':'Apr','5':'May',
                                        '6':'Jun','7':'Jul','8':'Aug','9':'Sep','10':'Oct','11':'Nov','12':'Dec'})
  
data['Seasons']=data['Seasons'].astype(str)

data['Seasons']= data['Seasons'].replace({'1':'summer','2':'autumn','3':'winter','4':'spring'})
  
data['Education']=data['Education'].astype(str)

data['Education']= data['Education'].replace({'1':'highschool','2':'graduate','3':'postgraduate','4':'master& doctrate'})
  
data.columns


data['Day of the week']=data['Day of the week'].astype(str)

data['Day of the week']= data['Day of the week'].replace({'2':"Monday",'3':"Tuesday",'4':"Wednesday",'5':"Thursday",'6':"Friday"})
  

data['Social drinker']=data['Social drinker'].astype(str)

data['Social drinker']= data['Social drinker'].replace({'0.0':'No','1.0':'Yes'})


data['Social smoker']=data['Social smoker'].astype(str)

data['Social smoker']= data['Social smoker'].replace({'0.0':'No','1.0':'Yes'})


data['Disciplinary failure']=data['Disciplinary failure'].astype(str)

data['Disciplinary failure']= data['Disciplinary failure'].replace({'0.0':'No','1.0':'Yes'})


# Generate 2011 data
Absence2011Data = d.copy()
Absence2011Data.columns
d.columns

Absence2011Data.columns
Absence2011Data.Age.head()

Absence2011Data.Age=  Absence2011Data.Age + 1
Absence2011Data['Service time'] = Absence2011Data['Service time'] + 1


#Subset training data
training = Absence2011Data.loc[:, Absence2011Data.columns != 'Absenteeism time in hours']
target = Absence2011Data[['Absenteeism time in hours']]

training.columns
#del Absence2011Data['Absenteeism time in hours'] 


import sklearn
from sklearn.tree import export_graphviz
from sklearn import tree
Reg =   tree.DecisionTreeRegressor(max_depth = 4,
                min_samples_split = 5,
                min_samples_leaf = 5,
                max_leaf_nodes = 50,                
                random_state=42)

training['Reason for absence'].head()

target.columns
training.columns

#'Work load Average day ' should be added in below variables list
impt_Vars = ['Reason for absence', 'Month of absence', 'Distance from Residence to Work',
        'Age','Social drinker', 'Social smoker', 'Body mass index']

Reg = Reg.fit(training[impt_Vars], target)


#############Visualize the Decision tree online; It's working
with open("DT_Regressor.txt", "w") as f:
    f = tree.export_graphviz(Reg, out_file=f,feature_names= training[impt_Vars].columns,class_names=['Absenteeism time in hours'],
                              filled= True,rounded=True)
###############
###############
##Paste the output file in below link to visualize tree#
#http://webgraphviz.com/

#==============================================================================
# Right away, from the learned decision tree we can see that the feature
#  "Reason of absence" is the root node with the MSE value of 7.577, and this means that
#  DV absennteesimTimeinHours have this as their base separation. This also means that in
#  principle, if we used only one feature in a predictive model, the "Reason of absence" 
#  will allow us to predict correctly with MSE value of 7.577, assuming that the original
#  learned decision tree predicts perfectly.
#==============================================================================

#==============================================================================
# Then, from the root we see that the classes split off further with the
#  od280/od315_of_dilute_wines feature and the flavinoid feature. We can also see 
#  that a majority of the class_1 wine (81.7%) have an alcohol content ≤ 13.175 and a 
# flavinoid content ≤ 0.795. Also, recall that there are 13 features in the original 
# dataset, but the decision tree picked only a subset of 7 features for the classification

#==============================================================================



