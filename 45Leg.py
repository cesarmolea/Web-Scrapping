import requests
import re
from bs4 import BeautifulSoup
import csv

url = 'https://parlamento.gub.uy/documentosyleyes/documentos/diarios-de-sesion?Cpo_Codigo_2=D&Lgl_Nro=45&DS_Fecha%5Bmin%5D%5Bdate%5D=15-02-2000&DS_Fecha%5Bmax%5D%5Bdate%5D=14-02-2005&Ssn_Nro=&TS_Diario=&tipoBusqueda=T&Texto=Banco+Interamericano+de+Desarrollo'

s = requests.session()
r = s.post(url, verify=False)

#print(r.text)
csvfile = open('output.csv', 'w', newline='')
writer = csv.writer(csvfile)

soup = BeautifulSoup(r.content, 'html.parser')

tdhtml = soup.find_all(attrs={'class': 'views-field-DS-File-SSN'})
lista = [x.find('a')['href'] for x in tdhtml[1:]]

for pagina in lista:
    r = s.post('https://parlamento.gub.uy' + pagina, verify=False)
    soup = BeautifulSoup(r.content, 'html.parser')
    for x in soup.find_all('p'):
        ma = re.search("Banco\sInteramericano\sde\sDesarrollo", x.get_text())
        if ma and x.get_text():
            writer.writerow( (x.get_text(),) )


