import xml.etree.ElementTree as etree 
import sqlite3
import csv
import hashlib

def xml2csv(xml, csv, encrypt = False):
    fout = open(csv, "w")
    tree = etree.parse(xml) 
    root = tree.getroot()
    fout.write("name,number,sent,time\n")
    for child in root:
        try:
            contactName = child.attrib["contact_name"]
        except:
            contactName = "NULL"
        contactNumber = child.attrib["address"]
        if encrypt:
            contactName = hashlib.sha224(contactName).hexdigest()
            contactNumber = hashlib.sha224(contactNumber).hexdigest()
        if child.attrib["type"] == "2":
            sent = 1
        else:
            sent = 0
        time = round(float(child.attrib["date"])/1000)
        # name, address, type, month, date, year, h, m, s, c
        fout.write(("%s,%s,%i,%i\n")%(contactName, 
                   contactNumber, sent, time))
    fout.close() 

def csv2sqlite(data, database, create = False, drop = False):
    con = sqlite3.connect(database)
    cur = con.cursor()
    data = open(data, "r")
    data = csv.DictReader(data)
    if drop:
        cur.execute("DROP TABLE IF EXISTS texts")
        con.commit()
    if create:
        cur.execute("CREATE TABLE IF NOT EXISTS texts \
        (name text, number int, type int, time int)")
        con.commit()
    baseCommand = "INSERT INTO texts (name, number, type, time) VALUES("
    for line in data:
        command = (baseCommand+("%s, %s, %i, %i)")%(
        "'" + line["name"] + "'", 
        "'" + line["number"] + "'", int(line["sent"]), 
        int(line["time"])))
        cur.execute(command)
    con.commit()
    cur.execute("ANALYZE texts")