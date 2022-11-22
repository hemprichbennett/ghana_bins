# query earthcape for desired data


import pymssql
import pandas as pd
import os
import params

conn = pymssql.connect(server=params.server, user=params.user, 
                       password=params.password, database=params.database)

desired_queries=['Unit', 'DNAextract', 'LocalityVisit']

# brief override to get all items
cursor = conn.cursor()  
desired_queries=[]
cursor.execute("SELECT name FROM SYSOBJECTS WHERE xtype = 'U'")
for row in cursor:
    desired_queries.append(row[0])

for field in desired_queries:
    print(field)
    query_string= 'SELECT * FROM ' + field
    df = pd.read_sql(query_string, con=conn)
    # check to see if the df contains anything (has a size greater than zero)
    if df.size > 0:
        # write the file
        df.to_csv('data/earthcape_query/' + field + '.csv')
    else:
        print(field + ' has zero size')
 