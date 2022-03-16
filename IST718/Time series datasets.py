# -*- coding: utf-8 -*-
"""
Created on Sat Dec  4 16:13:03 2021

@author: jpugazhendhi
"""


import pandas as pd
import numpy as np
import statsmodels.api as sm
import seaborn as sns
import matplotlib.pyplot as plt  # 2D plotting
from bs4 import BeautifulSoup
import requests
import ssl
import difflib as dfl
import re
import statsmodels.formula.api as smf
from pandasql import sqldf  

shipping = pd.read_csv("C:\py\Logistics.csv")

shipping['dispatch_month'] = pd.to_datetime(shipping['dispatch_date']).dt.strftime('%b')
shipping['dispatch_year'] = pd.to_datetime(shipping['dispatch_date']).dt.strftime('%Y')
shipping['dispatch_date'] = pd.to_datetime(shipping['dispatch_date'])
shipping['estimated_ship_date'] = pd.to_datetime(shipping['estimated_ship_date'])
shipping['delivered_date'] = pd.to_datetime(shipping['delivered_date'])


shipping.dtypes
##### Adding Season to disptah date #####

#def season_of_date(date):
#    year = str(date.year)
#    seasons = {'spring': pd.date_range(start='21/03/'+year, end='20/06/'+year),
#               'summer': pd.date_range(start='21/06/'+year, end='22/09/'+year),
#               'autumn': pd.date_range(start='23/09/'+year, end='20/12/'+year)}
#    if date in seasons['spring']:
#        return 'spring'
#    if date in seasons['summer']:
#        return 'summer'
#    if date in seasons['autumn']:
#        return 'autumn'
#    else:
#        return 'winter'

# Assuming df has a date column of type `datetime`
##shipping['season'] = shipping.dispatch_date.map(season_of_date)


### Reading from website for crude prices ###
table1 = pd.read_html("https://www.eia.gov/dnav/pet/hist/LeafHandler.ashx?n=PET&s=RWTC&f=M")

crude_prices = table1[5]
crude_prices.dropna(axis=0, how='any', inplace=True)
crude_prices=sqldf('select * from crude_prices where Year>2014')
crude_prices_new=crude_prices.melt(id_vars=["Year"])
crude_prices_new.columns=['Year','Month','crude_price']


##### reading from website for inflation
table2=pd.read_html("https://www.usinflationcalculator.com/inflation/current-inflation-rates/")
inflation=table2[0]

inflation.columns = inflation.iloc[0]
inflation = inflation.iloc[1: , :]

inflation_new=inflation.melt(id_vars=["Year"])
inflation_new = inflation_new.rename(columns={'Year': 'Year', 'O': 'Month', 'value':'rate'})
inflation_new.columns=['Year','Month','inflation_rate']
inflation_new.dropna(axis=0, how='any', inplace=True)




##### joining all 3 datasets ###

shipping_final1=sqldf('select a.*,b.crude_price from shipping a left join crude_prices_new b on a.dispatch_year=b.Year and a.dispatch_month=b.Month  ')
shipping_final2=sqldf('select a.*,b.inflation_rate from shipping_final1 a left join inflation_new b on a.dispatch_year=b.Year and a.dispatch_month=b.Month  ')
shipping_final=sqldf('select * from shipping_final2')

shipping.dtypes
shipping_final.dtypes

shipping_final['dispatch_date'] = pd.to_datetime(shipping_final['dispatch_date'])
shipping_final['estimated_ship_date'] = pd.to_datetime(shipping_final['estimated_ship_date'])
shipping_final['delivered_date'] = pd.to_datetime(shipping_final['delivered_date'])



shipping_final['days_delayed'] = (shipping_final['dispatch_date'] - shipping_final['estimated_ship_date']).dt.days
shipping_final['shipping_days'] = (shipping_final['delivered_date'] - shipping_final['dispatch_date']).dt.days



shipping_final=sqldf('select *,case when days_delayed<0 then 0 else days_delayed end as days_delayed_final from shipping_final ')

shipping_zip_metrics_2021=sqldf('select to_zipcode,avg(cost_after_adjustment) as cost_after_adjustment,max(shipping_days) as max_shipping_days,avg(shipping_days) as shipping_days,avg(pallets) as pallets, avg(crude_price) as crude_price, avg(inflation_rate) as inflation_rate,avg(layover_duration) as layover_duration,avg(days_delayed_final) as days_delayed from shipping_final where dispatch_year=2021 group by to_zipcode ')
shipping_zip_metrics_2020=sqldf('select to_zipcode,avg(cost_after_adjustment) as cost_after_adjustment,max(shipping_days) as max_shipping_days,avg(shipping_days) as shipping_days,avg(pallets) as pallets, avg(crude_price) as crude_price, avg(inflation_rate) as inflation_rate,avg(layover_duration) as layover_duration,avg(days_delayed_final) as days_delayed from shipping_final where dispatch_year=2020 group by to_zipcode ')


shipping_final['dispatch_month_year'] = pd.to_datetime(shipping_final['dispatch_date']).dt.strftime('%b-%Y')
#shipping_final['dispatch_year'] = pd.to_datetime(shipping_final['dispatch_date']).dt.strftime('%Y')
shipping_final['dispatch_month_date'] = pd.to_datetime(shipping_final['dispatch_date']).dt.strftime('%Y-%m-'+'01')


#################### Time Series Models #####################

loads=sqldf('select dispatch_month_date as date,to_zipcode,count(1) as load_count from shipping_final group by dispatch_date,to_zipcode order by dispatch_date asc')
costs=sqldf('select dispatch_month_date as date,to_zipcode,avg(cost_after_adjustment) as avg_cost from shipping_final group by dispatch_date,to_zipcode order by dispatch_date asc')


loads['date'] = loads['date'].astype('datetime64[ns]')
loads["date"] = loads["date"].dt.strftime("%d-%m-%Y")
#sqldf('select * from loads where to_zipcode=95330 and load_count>5')
##loads_95330=sqldf('select date as time,load_count as value from loads where to_zipcode=95330 and load_count>5')
##loads_95330.set_index('time')


costs['date'] = costs['date'].astype('datetime64[ns]')
costs["date"] = costs["date"].dt.strftime("%d-%m-%Y")
costs['avg_cost']=costs['avg_cost'].astype('int')


#################### Model 1 ############



