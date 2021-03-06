# -*- coding: utf-8 -*-
"""WEAT.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/12D0zxzxpilR6Tq5TJoMXbpTa6yJ9uwdd
"""

# Commented out IPython magic to ensure Python compatibility.
import numpy as np
import random
import time
from itertools import filterfalse
from scipy import spatial
# %matplotlib notebook
# %matplotlib inline
import matplotlib.pyplot as plt
from sklearn.manifold import TSNE
from sklearn.decomposition import PCA
import gensim
from gensim.test.utils import datapath, get_tmpfile
from gensim.models import word2vec
from gensim.models import KeyedVectors
from sklearn.metrics.pairwise import cosine_similarity
from gensim.scripts.glove2word2vec import glove2word2vec

"""# Glove Common Crawl"""

!wget http://nlp.stanford.edu/data/glove.840B.300d.zip # Retrieves content from the given URL
!unzip -q glove.840B.300d.zip # Unzips the file and returns the .txt file which can be used for analysis

embeddings_glove = {} # Empty dictionary in which the words in the vocabulary and the corresponding vector will be added
with open("glove.840B.300d.txt") as f:
    for line in f: # Each line in the file contains the token followed by the vector representation of that token trained in glove in 300 dimensions separated by a space. Each dimension of the vector is separated by a space as well
        word, coefs = line.split(maxsplit=1) # Splits the line as described above at the first space, hence the token gets separated from the vector. The token is stored in 'word' and the vector is stored in 'coefs'
        coefs = np.fromstring(coefs, "f", sep=" ") # Returns the vector representation in the form of an array which can now be used for analysis (cosine similarity, etc)
        embeddings_glove[word] = coefs # Add the token and its corresponding vector into the dictionary as key and value pair. Now we can call the vector of any token in the vocabulary via the dictionary

"""# Glove Wikipedia"""

!wget http://nlp.stanford.edu/data/glove.6B.zip # Retrieves content from the given URL
!unzip -q glove.6B.zip # Unzips the file and returns the .txt file which can be used for analysis. In this case, returns 4 files, having 50, 100, 200, and 300 dimensional vectors. We are choosing to use the 300 dimensional vectors

embeddings_glove_wiki_gw = {} # Empty dictionary in which the words in the vocabulary and the corresponding vector will be added
with open("glove.6B.300d.txt") as f:
    for line in f: # Each line in the file contains the token followed by a the vector representation of that token trained in glove in 300 dimensions separated by a space. Each dimension of the vector is separated by a space as well
        word, coefs = line.split(maxsplit=1) # Splits the line as described above at the first space, hence the token gets separated from the vector. The token is stored in 'word' and the vector is stored in 'coefs'
        coefs = np.fromstring(coefs, "f", sep=" ") # Returns the vector representation in the form of an array which can now be used for analysis (cosine similarity, etc)
        embeddings_glove_wiki_gw[word] = coefs # Add the token and its corresponding vector into the dictionary as key and value pair. Now we can call the vector of any token in the vocabulary via the dictionary

"""# Fastext Common Crawl"""

!wget https://dl.fbaipublicfiles.com/fasttext/vectors-english/crawl-300d-2M.vec.zip # Retrieves content from the given URL
!unzip -q crawl-300d-2M.vec.zip # Unzips the file and returns the .vec file which can be used for analysis

embeddings_fastext_cc = {}
with open("crawl-300d-2M.vec") as f:
    for line in f: # Each line in the file contains the token followed by a the vector representation of that token trained in glove in 300 dimensions separated by a space. Each dimension of the vector is separated by a space as well
        word, coefs = line.split(maxsplit=1) # Splits the line as described above at the first space, hence the token gets separated from the vector. The token is stored in 'word' and the vector is stored in 'coefs'
        coefs = np.fromstring(coefs, "f", sep=" ") # Returns the vector representation in the form of an array which can now be used for analysis (cosine similarity, etc)
        embeddings_fastext_cc[word] = coefs # Add the token and its corresponding vector into the dictionary as key and value pair. Now we can call the vector of any token in the vocabulary via the dictionary

"""# Fastext Wikipedia"""

!wget https://dl.fbaipublicfiles.com/fasttext/vectors-english/wiki-news-300d-1M.vec.zip # Retrieves content from the given URL
!unzip -q wiki-news-300d-1M.vec.zip # Unzips the file and returns the .vec file which can be used for analysis

embeddings_fastext_wiki = {}
with open("wiki-news-300d-1M.vec") as f:
    for line in f: # Each line in the file contains the token followed by a the vector representation of that token trained in glove in 300 dimensions separated by a space. Each dimension of the vector is separated by a space as well
        word, coefs = line.split(maxsplit=1) # Splits the line as described above at the first space, hence the token gets separated from the vector. The token is stored in 'word' and the vector is stored in 'coefs'
        coefs = np.fromstring(coefs, "f", sep=" ") # Returns the vector representation in the form of an array which can now be used for analysis (cosine similarity, etc)
        embeddings_fastext_cc[word] = coefs # Add the token and its corresponding vector into the dictionary as key and value pair. Now we can call the vector of any token in the vocabulary via the dictionary

"""# word2vec"""

!wget -P /root/input/ -c "https://s3.amazonaws.com/dl4j-distribution/GoogleNews-vectors-negative300.bin.gz"

EMBEDDING_FILE = '/root/input/GoogleNews-vectors-negative300.bin.gz'
embeddings_word2vec = KeyedVectors.load_word2vec_format(EMBEDDING_FILE, binary=True)

"""# WEAT"""

def cos(v1, v2): 
  """Returns the cosine similarity of 2 vectors of same dimesionality.

    Arguments:
    v1 -- Vector 1
    v2 -- Vector 2
    """

  num = np.dot(v1, v2)
  denom = np.linalg.norm(v1) * np.linalg.norm(v2)
  return num/denom

def weat_swAB(w, A, B, embedding_type):  
  """Returns the association of a single word with the chosen attribute (eg. good/bad).

    Arguments:
    w -- The string(word) whose association with the chosen attribute you want to find
    A -- List of Attribute words representing 1 end of the spectrum (eg. for the attribute Good/Bad, the list of words representing 'Good')
    B -- List of Attribute words representing the other end of the spectrum (eg. for the attribute Good/Bad, the list of words representing 'Bad')
    embedding_type -- The dictionary for the corpus algorithm pair being used in the current analysis (eg. for glove with common crawl, use embeddings_glove)
    """
  A_list = []
  B_list = []

  for v in A:
    A_list.append(cos(embedding_type[w], embedding_type[v]))
  for v in B:
    B_list.append(cos(embedding_type[w], embedding_type[v]))

  A_array = np.array(A_list)
  B_array = np.array(B_list)

  return np.mean(A_array) - np.mean(B_array)

def weat_ES(X, Y, A, B, embedding_type):
  """Returns the effect size of a pair of categories (eg Simple vs Difficult) with the chosen attribute (eg. good/bad).

    Arguments:
    X -- List of Category words representing 1 of the categories (eg. for Simple/Difficult, the list of words representing 'Simple')
    Y -- List of Category words representing the other category (eg. for Simple/Difficult, the list of words representing 'Difficult')
    A -- List of Attribute words representing 1 end of the spectrum (eg. for the attribute Good/Bad, the list of words representing 'Good')
    B -- List of Attribute words representing the other end of the spectrum (eg. for the attribute Good/Bad, the list of words representing 'Bad')
    embedding_type -- The dictionary for the corpus algorithm pair being used in the current analysis (eg. for glove with common crawl, use embeddings_glove)
    """
  x_list = []
  y_list = []

  for x in X:
    x_list.append(weat_swAB(x, A, B, embedding_type))
  for y in Y:
    y_list.append(weat_swAB(y, A, B, embedding_type))

  x_array = np.array(x_list)
  y_array = np.array(y_list)

  return (np.mean(x_array) - np.mean(y_array))/(np.std(np.concatenate((x_array, y_array)), ddof = 1))

def test_statistic(X, Y, A, B, embedding_type):
  """Returns the test statistic given the 2 sets of category stimuli and attribute stimuli.

    Arguments:
    X -- List of Category words representing 1 of the categories (eg. for Simple/Difficult, the list of words representing 'Simple')
    Y -- List of Category words representing the other category (eg. for Simple/Difficult, the list of words representing 'Difficult')
    A -- List of Attribute words representing 1 end of the spectrum (eg. for the attribute Good/Bad, the list of words representing 'Good')
    B -- List of Attribute words representing the other end of the spectrum (eg. for the attribute Good/Bad, the list of words representing 'Bad')
    embedding_type -- The dictionary for the corpus algorithm pair being used in the current analysis (eg. for glove with common crawl, use embeddings_glove)
    """
  x_list = []
  y_list = []

  for x in X:
    x_list.append(weat_swAB(x, A, B, embedding_type))
  for y in Y:
    y_list.append(weat_swAB(y, A, B, embedding_type))

  x_array = np.array(x_list)
  y_array = np.array(y_list)

  return np.sum(x_array) - np.sum(y_array)

def random_permutation(targets, r):
  """Returns a random permutation of a fixed number of a chosed list.

    Arguments:
    targets -- The list whose permutation is to be generated
    r -- Size of the permutation
    """
  return random.sample(targets, r)

def p_value_weat(X, Y, A, B, embedding_type, iterations):
  """Returns the p-value given a pair of categories (eg Simple vs Difficult) and the chosen attribute (eg. good/bad).

    Arguments:
    X -- List of Category words representing 1 of the categories (eg. for Simple/Difficult, the list of words representing 'Simple')
    Y -- List of Category words representing the other category (eg. for Simple/Difficult, the list of words representing 'Difficult')
    A -- List of Attribute words representing 1 end of the spectrum (eg. for the attribute Good/Bad, the list of words representing 'Good')
    B -- List of Attribute words representing the other end of the spectrum (eg. for the attribute Good/Bad, the list of words representing 'Bad')
    embedding_type -- The dictionary for the corpus algorithm pair being used in the current analysis (eg. for glove with common crawl, use embeddings_glove)
    iterations -- Number of permutations you want to generate (The more you generate, the more accurate the result is)
    """
  X_Y = X + Y
  size_of_permutation = len(X)
  permutations = []
  test_statistics_permutations = []
  condition_satisfied = []
  count = 0

  while iterations != 0:
    permutations.append(random_permutation(X_Y, size_of_permutation))
    iterations = iterations - 1

  for Xi in permutations:
    count = count + 1
    Yi = filterfalse(lambda w:w in Xi, X_Y)
    test_statistics_permutations.append(test_statistic(Xi, Yi, A, B, embedding_type))

  overall_test_statistic = test_statistic(X, Y, A, B, embedding_type)

  condition_satisfied = [p > overall_test_statistic for p in test_statistics_permutations]

  condition_satisfied_array = np.array(condition_satisfied)

  return condition_satisfied_array.sum()/condition_satisfied_array.size

"""# Example"""

# Attribute Stimuli
Good = ["love", "cheer", "friend", "pleasure", "paradise", "splendid"]
Bad = ["abuse", "grief", "poison", "sadness", "pain", "bomb"]

# Category Stimuli
Simple = ["easy", "elementary", "straightforward", "effortless"]
Difficult = ["complicated", "challenging", "puzzling", "baffling"]

weat_ES(Simple, Difficult, Good, Bad, embeddings_glove_wiki_gw)

p_value_weat(Simple, Difficult, Good, Bad, embeddings_glove_wiki_gw, 1000)

