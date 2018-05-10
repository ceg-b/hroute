#!/usr/bin/env python3

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import sys


prefix = sys.argv[1]

df1=pd.read_csv(prefix+"f1.csv")
df2=pd.read_csv(prefix+"f2.csv")
df3=pd.read_csv(prefix+"f3.csv")

dff2=df2.copy()
dff2=dff2[3:]

dff3=df3.copy()
dff3=dff3[3:]


pf1=pd.read_csv("cnt_"+prefix+"f1.csv")
pf2=pd.read_csv("cnt_"+prefix+"f2.csv")
pf3=pd.read_csv("cnt_"+prefix+"f3.csv")


dff2.columns=['no','ename','fload']
dff3.columns=['no','ename','rload']

df4=dff2.set_index('ename').join(dff3.set_index('ename'),lsuffix='_forward', rsuffix='_backward')
df4['sum'] = df4.apply(lambda row: row.fload + row.rload, axis=1)
df4=df4.sort_values(by=['sum'],ascending=False)

df4['ename']=df4['sum']
df4.set_index('sum',inplace=True,drop=False)
print(df4)
df4[['rload','fload']].plot(kind='bar',stacked=True,xticks=df4['sum'])
plt.grid()
plt.savefig("uncon"+prefix+'load.eps', format='eps', dpi=1200)


pf2.columns=['no','len','fcount']
pf3.columns=['no','len','rcount']


df5=pf2.set_index('len').join(pf3.set_index('len'),lsuffix='_forward', rsuffix='_backward')
df5['sum'] = df5.apply(lambda row: row.fcount + row.rcount, axis=1)
#df5=df5.sort_values(by=['sum'],ascending=False)


#df5.set_index('sum',inplace=True,drop=False)
print(df5)
df5[['fcount','rcount']].plot(kind='bar',stacked=True,xticks=df5['sum'])

plt.savefig("uncon"+prefix+'length.eps', format='eps', dpi=1200)

# df2=df.copy()

# print(df2)

# #df2.plot("no","load",kind='bar')
# df2.plot("len","count",kind='bar')
plt.grid()
plt.show()
