import requests
import re

url = 'https://www.nihaowua.com/page/'

s = requests.session()
p1 = re.compile('<p>.*</p>')
p2 = re.compile('<.*?>')

with open('nhw.txt', 'w') as fw:
    for n in range(1,456):
        ul = '%s%d/' %(url, n)
        print(ul)
        r = s.get(ul)
        r1 = p1.findall(r.text)
        for r in r1:
            v = p2.sub('', r)
            fw.write(v.strip())
            fw.write('\n')
