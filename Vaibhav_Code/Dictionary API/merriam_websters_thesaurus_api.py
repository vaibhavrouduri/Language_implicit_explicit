# -*- coding: utf-8 -*-
"""Merriam_Websters_Thesaurus_API.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1v-PA3pi_LFPiHaIH5mtnCpgho8PlJqCL
"""

pip install mwthesaurus # pip is the package installer for python. Here we are using it to download the Merriam-Webster Dictionary package

# In order to use the API, you are required to register on the website https://dictionaryapi.com/ after you will be provided with a key.
from mwthesaurus import MWClient 
client = MWClient(key = "c718d29d-2915-4216-963b-dc24718dd162")
client.get("idyll")