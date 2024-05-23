import requests

# for years 2007 to 2022
# loop through and hit the base url
# just add the year to the end of the url
# get the json object
# keep it as a dictionary with {year, json} as key value pairs

base_url = "https://sinav30.conagua.gob.mx:8080/SINA43/Tarifas3/grafica1/" # 2013

all_dat = {}
for year in range(2007, 2023):
    url = base_url + str(year)
    response = requests.get(url)
    data = response.json()

    all_dat[year] = data

# the value is an array of objects with id, ciudad, usodomestica, usocomercial, usoindustrial
# i want to make a csv that has the year, id, ciudad, usodomestica, usocomercial, usoindustrial
# use the csv builder so it gets properly escapd 

with open(f"tarifas.csv", "w") as f:
    f.write("year,id,ciudad,usodomestica,usocomercial,usoindustrial\n")

    for year, data in all_dat.items():
        for obj in data:
            f.write(f"{year},{obj['id']},{obj['ciudad']},{obj['usodomestica']},{obj['usocomercial']},{obj['usoindustrial']}\n")
