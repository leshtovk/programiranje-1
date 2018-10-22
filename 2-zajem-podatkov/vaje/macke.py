import requests
import re
import os
import csv

###############################################################################
# Najprej definirajmo nekaj pomožnih orodij za pridobivanje podatkov s spleta.
###############################################################################

# definiratje URL glavne strani bolhe za oglase z mačkami
cats_frontpage_url = 'http://www.bolha.com/zivali/male-zivali/macke/'
# mapa, v katero bomo shranili podatke
cat_directory = 'ma_cats'
# ime datoteke v katero bomo shranili glavno stran
frontpage_filename = 'cat_frontpage.html'
# ime CSV datoteke v katero bomo shranili podatke
csv_filename = 'cat_ads.csv'


def download_url_to_string(url):
    '''This function takes a URL as argument and tries to download it
    using requests. Upon success, it returns the page contents as string.'''
    try:
        # del kode, ki morda sproži napako
        r = requests.get(url)
    except requests.exceptions.ConnectionError:
        print('failed to download')
        # koda, ki se izvede pri napaki
        # dovolj je če izpišemo opozorilo in prekinemo izvajanje funkcije
        return 
    # nadaljujemo s kodo če ni prišlo do napake
    return r.text 




def save_string_to_file(text, directory, filename):
    '''Write "text" to the file "filename" located in directory "directory",
    creating "directory" if necessary. If "directory" is the empty string, use
    the current directory.'''
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w', encoding='utf-8') as file_out:
        file_out.write(text)
    return None



# Definirajte funkcijo, ki prenese glavno stran in jo shrani v datoteko.


def save_frontpage():
    '''Save "cats_frontpage_url" to the file
    "cat_directory"/"frontpage_filename"'''
    cats_data_string = download_url_to_string(cats_frontpage_url) 
    if cats_data_string is None:
        print('the url download failed')
    else:
        save_string_to_file(cats_data_string, cat_directory, frontpage_filename)
        return None 
###############################################################################
# Po pridobitvi podatkov jih želimo obdelati.
###############################################################################


def read_file_to_string(directory, filename):
    '''Return the contents of the file "directory"/"filename" as a string.'''
    path = os.path.join(directory, filename)
    with open(path, 'r', encoding='utf-8') as file_in:
        return file_in.read()
    


# Definirajte funkcijo, ki sprejme niz, ki predstavlja vsebino spletne strani,
# in ga razdeli na dele, kjer vsak del predstavlja en oglas. To storite s
# pomočjo regularnih izrazov, ki označujejo začetek in konec posameznega
# oglasa. Funkcija naj vrne seznam nizov.


def page_to_ads(page):
    '''Split "page" to a list of advertisement blocks.'''
    regex =  re.compile(r'<div class="ad">(.*?)<div class="clear">', re.DOTALL)
    ads = re.findall(regex, page)
    return ads

# Definirajte funkcijo, ki sprejme niz, ki predstavlja oglas, in izlušči
# podatke o imenu, ceni in opisu v oglasu.


def get_dict_from_ad_block(block):
    '''Build a dictionary containing the name, description and price
    of an ad block.'''
    regex = re.compile(r'title="(?P<name>.*?)"'
                       r'.*?</h3>(?P<description>.*?)<div>', re.DOTALL)
                       #capture the prices also
    data = re.search(regex, block)
    ad_dict = data.groupdict()
    return 

# Definirajte funkcijo, ki sprejme ime in lokacijo datoteke, ki vsebuje
# besedilo spletne strani, in vrne seznam slovarjev, ki vsebujejo podatke o
# vseh oglasih strani.


def ads_from_file(directory, filename):
    '''Parse the ads in filename/directory into a dictionary list.'''
    #read the file 
    #split the page into blocks 
    #for each block get out the dict's 
    #return the list of dicts 
    list_of_dicts = []
    contents = read_file_to_string(directory, filename)
    blocks = page_to_ads(contents)
    for block in blocks: 
        list_of_dicts.append(get_dict_from_ad_block(block))
    return list_of_dicts

###############################################################################
# Obdelane podatke želimo sedaj shraniti.
###############################################################################


def write_csv(fieldnames, rows, directory, filename):
    '''Write a CSV file to directory/filename. The fieldnames must be a list of
    strings, the rows a list of dictionaries each mapping a fieldname to a
    cell-value.'''
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w', encoding='utf-8') as csv_file:
        writer = csv.DictWriter(csv_file, fieldnames=fieldnames)
        writer.writeheader()
        for row in rows:
            writer.writerow(row)
    return None

# Definirajte funkcijo, ki sprejme neprazen seznam slovarjev, ki predstavljajo
# podatke iz oglasa mačke, in zapiše vse podatke v csv datoteko. Imena za
# stolpce [fieldnames] pridobite iz slovarjev.


def write_cat_ads_to_csv(ads):
    fieldnames = ads[0].keys()
    write_csv(fieldnames, ads, cat_directory, csv_filename)
    return None 
