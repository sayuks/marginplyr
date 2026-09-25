from concurrent.futures import ThreadPoolExecutor
from itertools import product
import subprocess,pathlib
root=pathlib.Path('/private/tmp/marginplyr-sqlite-design-20260925')
out=root/'review/independent/B-direct'
cases=list(product(('on','off'),('implicit','explicit'),('sorted','unsorted'),('collect','compute')))
def run(c):
 name='655-'+'-'.join(c)
 with (out/(name+'.log')).open('w') as log:
  p=subprocess.run(['Rscript','--vanilla',str(root/'review/independent/655-fresh.R'),str(root/'variants/B-direct'),*c,str(out/(name+'.rds'))],stdout=log,stderr=subprocess.STDOUT)
 return name,p.returncode
with ThreadPoolExecutor(max_workers=3) as ex:
 for name,code in ex.map(run,cases):print(name,code)
