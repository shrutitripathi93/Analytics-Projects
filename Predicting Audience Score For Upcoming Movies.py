#Scrapped and collected data from Rotten Tomatoes using HTML response request and BeautifulSoup to create a large database of 19000 movies 
#Developed derived features such to develop derived features such as actor score, director score and cast score using actor rating, 
#director rating and cast review available in database
#Worked with team to implement machine learning algorithms to predict the difference between Audience Score and Critics score and
# created a matrix of these prediction accuracies


from sklearn import linear_model
import csv
import re
from bs4 import BeautifulSoup
import pandas as pd
import re
import time
import requests
import urllib
import pandas
from sklearn.metrics import confusion_matrix
import nltk
from sklearn.preprocessing import LabelEncoder
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.naive_bayes import MultinomialNB
from sklearn.neighbors import KNeighborsClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import VotingClassifier
from sklearn.metrics import accuracy_score
from sklearn import preprocessing
from sklearn.metrics import mean_squared_error
from math import sqrt
from sklearn.decomposition import PCA
import matplotlib.pyplot as plt
import numpy as np

def encode(file):
    #   for critic score
    for i in range(len(movies_data)):
        movies_data.loc[i,'critic_score']=int(re.sub('[^0-9\d]',' ',movies_data.loc[i,'critic_score']))
         
    #   for audience score
    for i in range(len(movies_data)):
        movies_data.loc[i,'audience_score']=int(re.sub('[^0-9\d]',' ',movies_data.loc[i,'audience_score']))
    
    #   for actor_link
    for i in range(len(movies_data)):
        
        grand_cast_rating=0
        links=movies_data.loc[i,'actor_links']
        for actor_link in links.split(','):
            grand_rating=0
            extension=str(actor_link)
            common_link="https://www.rottentomatoes.com"
            URL=common_link+extension
            #print(URL)
            try:
                response=requests.get(str(URL),headers = { 'User-Agent': 'Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36', })
                html=str(response.content)
                #print(html)
                soup = BeautifulSoup(html)#.decode('ascii', 'ignore'),'lxml')
                review_table=soup.find('table', {'id':re.compile('filmographyTbl')}) # get all the review divs
                review_rows=review_table.findAll('td', {'data-rating':re.compile('')})
                if review_rows:
                    for review in review_rows:
                        print(review)
                    
                        rating=0 # initialize critic and text 
                        getfresh=review.find('span',{'title':re.compile('fresh')})
                        getrotten=review.find('span',{'title':re.compile('rotten')})
                        getcfresh=review.find('span',{'title':re.compile('certified_fresh')})
                    
                        if getfresh:
                            rating=1
                        if getrotten:
                            rating=-1
                        if getcfresh:
                            rating=2
                        grand_rating=grand_rating+rating
                        #print(grand_rating)
                else:
                    grand_rating=0;
                    grand_cast_rating=grand_cast_rating+grand_rating
            except Exception as e:
                print( "<p>Error: %s</p>" % str(e) )
              
        #print(grand_cast_rating)
        movies_data.loc[i,'cast_rating']=grand_cast_rating
      
            
    #   for genre
    for i in range(len(movies_data)):
        genre=movies_data.loc[i,'Genre']
        movies_data.loc[i,'Action']=0
        movies_data.loc[i,'Animation']=0
        movies_data.loc[i,'Art & Foreign']=0
        movies_data.loc[i,'Classics']=0
        movies_data.loc[i,'Comedy']=0
        movies_data.loc[i,'Documentary']=0
        movies_data.loc[i,'Drama']=0
        movies_data.loc[i,'Horror']=0
        movies_data.loc[i,'Kids & Family']=0
        movies_data.loc[i,'Mystery']=0
        movies_data.loc[i,'Romance']=0
        movies_data.loc[i,'Sci-fi & Fantasy']=0
        movies_data.loc[i,'other_genre']=0
        #calling a function to encode
        genre_encoding(genre,i)
    
    
    #   for date
    for i in range(len(movies_data)):
        date=re.sub('[^0-9a-zA-Z\d]',' ',movies_data.loc[i,'In Theaters']) #date
        date=re.sub(' +',' ',date).strip()
        movies_data.loc[i,'In Theaters']=date
    #for rating
    for i in range(len(movies_data)):
        movies_data.loc[i,'PG']=0
        movies_data.loc[i,'G']=0
        movies_data.loc[i,'NC17']=0
        movies_data.loc[i,'PG-13']=0
        movies_data.loc[i,'R']=0
        movies_data.loc[i,'NR']=0
        movies_data.loc[i,'other_rating']=0
        #calling a function to encode rating
        rating=movies_data.loc[i,'Rating']
        rating_encoding(rating,i)
    #for studio
    for i in range(len(movies_data)):
        movies_data.loc[i,'Warner']=0
        movies_data.loc[i,'Century']=0
        movies_data.loc[i,'MGM']=0
        movies_data.loc[i,'Disney']=0
        movies_data.loc[i,'Pixar']=0
        movies_data.loc[i,'Universal']=0
        movies_data.loc[i,'Sony']=0
        movies_data.loc[i,'Fox']=0
        movies_data.loc[i,'Paramount']=0
        movies_data.loc[i,'Marvel']=0
        movies_data.loc[i,'other_studio']=0
        #calling a function to encode
        
        studios=movies_data.loc[i,'Studio'].strip().split(',')
        for studio in studios:
            #calling a function to encode
            studio_encoding(studio,i)
    #print(movies_data.loc[1,:])
        for i in range(len(movies_data)):
            try:
                movies_data.loc[i,'Runtime']=int(re.sub('[^0-9\d]',' ',movies_data.loc[i,'Runtime']))
            except Exception as e:
                continue

    return movies_data    
        
        
def studio_encoding(studio,i):
    if ('Warner' in studio):
        movies_data.loc[i,'Warner']=1
    if ('Century' in studio):
        movies_data.loc[i,'Century']=1
    if ('MGM' in studio):
        movies_data.loc[i,'MGM']=1
    if ('Disney' in studio):
        movies_data.loc[i,'Disney']=1
    if ('Pixar' in studio):
        movies_data.loc[i,'Pixar']=1
    if ('Universal' in studio):
        movies_data.loc[i,'Universal']=1
    if ('Sony' in studio):
        movies_data.loc[i,'Sony']=1
    if ('Fox' in studio):
        movies_data.loc[i,'Fox']=1
    if ('Paramount' in studio):
        movies_data.loc[i,'Paramount']=1
    if ('Marvel' in studio):
        movies_data.loc[i,'Marvel']=1
    if ('Warner' not in studio and 'Century' not in studio and 'MGM' not in studio and 'Disney' not in studio and 'Pixar' not in studio and 'Universal' not in studio and 'Sony' not in studio and 'Fox' not in studio and 'Paramount' not in studio and 'Marvel' not in studio):
        movies_data.loc[i,'other_studio']=1
    
    
def rating_encoding(rating,i):
    if 'G' in rating:  #rating
        movies_data.loc[i,'G']=1
    if 'NC17' in rating:
        movies_data.loc[i,'NC17']=1
    if 'PG' in rating:
        if 'PG-13' in rating:
            movies_data.loc[i,'PG-13']=1
        else:
            movies_data.loc[i,'PG']=1
    
    if 'R' in rating:
        if 'NR' in rating:
            movies_data.loc[i,'NR']=1
        else:
            movies_data.loc[i,'R']=1
    
    if rating=='NA' or rating==None:
        movies_data.loc[i,'other_rating']=1

def genre_encoding(genre,i):
    
    if('Action' in genre):
        movies_data.loc[i,'Action']=1
    if('Animation' in genre):
        movies_data.loc[i,'Animation']=1
    if('Art & Foreign' in genre):
        movies_data.loc[i,'Art & Foreign']=1
    if('Classics' in genre):
        movies_data.loc[i,'Classics']=1
    if("Comedy" in genre):
        movies_data.loc[i,'Comedy']=1
    if('Documentary' in genre):
        movies_data.loc[i,'Documentary']=1
    if('Drama' in genre):
        movies_data.loc[i,'Drama']=1
    if('Horror' in genre):
        movies_data.loc[i,'Horror']=1
    if('Kids & Family' in genre):
        movies_data.loc[i,'Kids & Family']=1
    if('Mystery' in genre):
        movies_data.loc[i,'Mystery']=1
    if('Romance' in genre):
        movies_data.loc[i,'Romance']=1
    if('Sci-fi & Fantasy' in genre):
        movies_data.loc[i,'Sci-fi & Fantasy']=1
    if('NA' in genre):
        movies_data.loc[i,'other_genre']=1
    if genre==None:
        movies_data.loc[i,'other_genre']=1 

def predict(movies_data):
    movies_data['Rating_difference']=abs(movies_data['critic_score']-movies_data['audience_score'])
    movies_data = movies_data.drop('actor_names',1)
    movies_data = movies_data.drop('actor_links',1)
    movies_data = movies_data.drop('synopsis',1)
    movies_data = movies_data.drop('In Theaters',1)
    movies_data = movies_data.drop('Genre',1)
    movies_data = movies_data.drop('Studio',1)
    movies_data = movies_data.drop('Directed By',1)
    movies_data = movies_data.drop('Box Office',1)
    movies_data = movies_data.drop('Rating',1)
    movies_data = movies_data.drop('Written By',1)
    movies_data = movies_data.drop('movie_id',1)
    
    print(movies_data)
    
    result_stats={}
    
    X=movies_data.drop('audience_score',1)
    X = X.drop('critic_score',1)
    X=X.drop('Rating_difference',1)
    X = preprocessing.scale(X)
    
    #PCA Analysis
    
    pca = PCA().fit(X)
    plt.plot(np.cumsum(pca.explained_variance_ratio_))
    plt.xlabel('number of components')
    plt.ylabel('cumulative explained variance')
    
    pca2 = PCA(n_components=16)
    pca2.fit(X)
    X=pca2.transform(X)
    
    
    
    #dependant variable
    y = movies_data.iloc[:, 35].values
    #y = preprocessing.scale(y)
    #print(y)
    #print(y)
    #z = movies_data.iloc[:, 2].values
    #z = preprocessing.scale(z)
    #print(z)
    
    # Splitting the dataset into the Training set and Test set
    from sklearn.cross_validation import train_test_split
    X_train, X_test, y_train, y_test= train_test_split(X, y, test_size = 0.2, random_state = 2)
    
    #print(y_test)
    #print(y_train)
    
    
    lm = linear_model.LinearRegression()
    lm.fit(X_train,y_train)
    y_lm_prediction = lm.predict(X_test)
    print(y_lm_prediction)
    print(y_test)
    linear_table = pd.DataFrame({'Actual' : y_test, 'Predicted' : y_lm_prediction}) 
    print(linear_table)   
    meanSquaredError=mean_squared_error(y_test, y_lm_prediction)
    print("MSE:", meanSquaredError)
    rootMeanSquaredError = sqrt(meanSquaredError)
    print("RMSE:", rootMeanSquaredError)
    result_stats["Linear Regression"]=rootMeanSquaredError
    '''
    score = lm.score(X_test, y_test)
    print('Accuracy: {:.2%}'.format(score))
    '''
    ###################################################
    
    #mnb_classifier = MultinomialNB()
    knn_classifier = KNeighborsClassifier()
    lreg_classifier = LogisticRegression()
    dt_classifier = DecisionTreeClassifier()
    
    combo_predictors = [('knn', knn_classifier),('lreg', lreg_classifier),('dt', dt_classifier)]
    voting_classifier = VotingClassifier(combo_predictors)
    
    # Train the model
    voting_classifier.fit(X_train, y_train)
    
    # Predict the labels for test data
    y_combo_predicted = voting_classifier.predict(X_test)
    print(y_combo_predicted)
    linear_table1 = pd.DataFrame({'Actual' : y_test, 'Predicted' : y_combo_predicted}) 
    print(linear_table1)
    meanSquaredError1=mean_squared_error(y_test, y_combo_predicted)
    print("MSE:", meanSquaredError1)
    rootMeanSquaredError1 = sqrt(meanSquaredError1)
    print("RMSE:", rootMeanSquaredError1)
    result_stats["Ensemble Learning"]=rootMeanSquaredError1
    '''
    score1 = voting_classifier.score(X_test, y_test)
    print('Accuracy: {:.2%}'.format(score1))
    '''
    
###################################################

    from sklearn.ensemble import RandomForestRegressor
    rf_regressor = RandomForestRegressor(n_estimators = 10, random_state = 2)
    rf_regressor.fit(X_train, y_train)
    
    # Predicting a new result
    y_pred = rf_regressor.predict(X_test)
    print(y_pred)
    linear_table2 = pd.DataFrame({'Actual' : y_test, 'Predicted' : y_pred}) 
    print(linear_table2)
    meanSquaredError2=mean_squared_error(y_test, y_pred)
    print("MSE:", meanSquaredError2)
    rootMeanSquaredError2 = sqrt(meanSquaredError2)
    print("RMSE:", rootMeanSquaredError2)
    result_stats["Random Forest"]=rootMeanSquaredError2
    '''
    score2 = rf_regressor.score(X_test, y_test)
    print('Accuracy: {:.2%}'.format(score2))
    '''
    ####################################################
        
    from sklearn.svm import SVR
    regressor1 = SVR(kernel = 'rbf')
    regressor1.fit(X_train, y_train)
    
    # Predicting a new result
    y_svr_pred = regressor1.predict(X_test)
    
    print(y_svr_pred)
    linear_table3 = pd.DataFrame({'Actual' : y_test, 'Predicted' : y_svr_pred}) 
    print(linear_table3)
    meanSquaredError3=mean_squared_error(y_test, y_svr_pred)
    print("MSE:", meanSquaredError3)
    rootMeanSquaredError3 = sqrt(meanSquaredError3)
    print("RMSE:", rootMeanSquaredError3)
    result_stats["SVM"]=rootMeanSquaredError3
    '''
    score3 = regressor1.score(X_test, y_test)
    print('Accuracy: {:.2%}'.format(score3))
    '''
    ##################################################
    
    from sklearn.tree import DecisionTreeRegressor
    regressor2 = DecisionTreeRegressor(random_state = 2)
    regressor2.fit(X_train,y_train)
    
    # Predicting a new result
    y_pred1 = regressor2.predict(X_test)
    print(y_pred1)
    linear_table4 = pd.DataFrame({'Actual' : y_test, 'Predicted' : y_pred1}) 
    print(linear_table4)
    meanSquaredError4=mean_squared_error(y_test, y_pred1)
    print("MSE:", meanSquaredError4)
    rootMeanSquaredError4 = sqrt(meanSquaredError4)
    print("RMSE:", rootMeanSquaredError4)
    result_stats["Decision Tree"]=rootMeanSquaredError4
    '''
    score4 = regressor2.score(X_test, y_test)
    print('Accuracy: {:.2%}'.format(score4))
    '''
    print(result_stats)
    
if __name__=='__main__':
    
    movies_data = pandas.read_table('sample.txt',sep='\t')
    encoded_movie_data = encode(movies_data)
    predict(encoded_movie_data)

