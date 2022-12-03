#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Oct 12 21:36:27 2021

@author: m31781
"""


from requests.auth import HTTPBasicAuth
import requests
import pandas as pd
import os
from datetime import date, timedelta

os.chdir(r'/Users/m31781/Documents/DataLyfe/NFL SB Prediction/NFL_Reg_Season_Predictions')

season_list = list(range(2019, 2022))

# data pull function
def data_pull(year):
    
    week_list = list(range(1, 18))
    data = []

    for week in week_list:
    
        url = 'https://api.sportsdata.io/v3/nfl/scores/json/TeamGameStats/' + str(year) + 'REG/' + str(week)
        headers = {'Ocp-Apim-Subscription-Key': '703dcf47836b4313891d49b915d249e1'}
        req = requests.get(url, headers=headers)
        
        df = pd.DataFrame(req.json())
        
        data.append(df)
    
    data = pd.concat(data)
        
    return data

# call
game_stats = pd.concat(map(data_pull, season_list))

today = date.today()
start = today - timedelta(days=today.weekday())
file_name = 'Weekly_Update/Weekly_Update_' + str(start) + '.csv'

game_stats.to_csv(file_name, index=False)

