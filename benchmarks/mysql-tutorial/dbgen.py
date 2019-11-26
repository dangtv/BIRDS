import pydbgen
from pydbgen import pydbgen
import numpy as np
import pandas as pd

myDB=pydbgen.pydb()
# dbsize=1000
# 
for dbsize in [1,10,100, 1000, 10000, 100000, 500000, 1000000, 1500000, 2000000, 2500000, 3000000]:
    items = myDB.gen_dataframe(dbsize,['id','name','price'])
    items['id'] = items.index
    items['id'] = items['id'] + 1
    items['price'] = np.random.uniform(low=0.0, high=10000, size=(dbsize, 1))
    # np.random.randint(0,500,size=(500, 1))
    items.to_csv("dbgen/items-"+str(dbsize)+".csv", header=False, index=False)

    offices = myDB.gen_dataframe(dbsize,['officecode','city','phone','address1','address2','state','country','zipcode','real_city'])
    offices['officecode'] = offices.index
    offices['officecode'] = offices['officecode'] + 1

    offices["address1"] = myDB.gen_data_series(dbsize, data_type="street_address")
    offices["address2"] = myDB.gen_data_series(dbsize, data_type="street_address")
    offices.to_csv("dbgen/offices-"+str(dbsize)+".csv", header=False, index=False)
