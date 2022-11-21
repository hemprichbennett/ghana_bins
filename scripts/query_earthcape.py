# query earthcape for desired data


import pymssql
import pandas as pd
import os
import params

conn = pymssql.connect(server=params.server, user=params.user, 
                       password=params.password, database=params.database)

desired_queries=['Unit', 'DNAextract', 'LocalityVisit']


for field in desired_queries:
    print(field)
    query_string= 'SELECT * FROM ' + field
    df = pd.read_sql(query_string, con=conn)
    # check to see if the df contains anything (has a size greater than zero)
    if df.size > 0:
        # write the file
        df.to_csv('./data/query_results/' + field + '.csv')
    else:
        print(field + ' has zero size')
 