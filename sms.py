import xml.etree.ElementTree as etree 
import sqlite3
import csv

def xml2csv(xml, csv):
    fout = open(csv, "w")
    tree = etree.parse(xml) 
    root = tree.getroot()
    fout.write("name,number,sent,month,day,year,hour,minute,second\n")
    for child in root:
        try:
            contactName = child.attrib["contact_name"]
        except:
            contactName = "NULL"
        contactNumber = child.attrib["address"]
        if child.attrib["type"] == "2":
            sent = 1
        else:
            sent = 0
        month = child.attrib["readable_date"].split()[0]
        if month == "Jan":
            month = 1
        elif month == "Feb":
            month = 2
        elif month == "Mar":
            month = 3
        elif month == "Apr":
            month = 4
        elif month == "May":
            month = 5
        elif month == "Jun":
            month = 6
        elif month == "Jul":
            month = 7
        elif month == "Aug":
            month = 8
        elif month == "Sep":
            month = 9
        elif month == "Oct":
            month = 10
        elif month == "Nov":
            month = 11
        elif month == "Dec":
            month = 12
        day = int(child.attrib["readable_date"].split()[1][0:-1])
        year = int(child.attrib["readable_date"].split()[2])
        if child.attrib["readable_date"].split()[4] == "PM":
            hour = 12 + int(child.attrib["readable_date"].split()[3].split(":")[0])
        else:
            hour = int(child.attrib["readable_date"].split()[3].split(":")[0])
        minute = int(child.attrib["readable_date"].split()[3].split(":")[1])
        second = int(child.attrib["readable_date"].split()[3].split(":")[2])
        # name, address, type, month, date, year, h, m, s, c
        fout.write(("%s,%s,%i,%i,%i,%i,%i,%i,%i\n")%(contactName, 
                   contactNumber, sent, month, day, year, hour, 
                   minute, second))
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
        (`key` int primary key, name text, number int, type int, month int,\
        day int, year int, hour int, minute int, second int)")
        con.commit()
    baseCommand = "INSERT INTO texts (name, number, type, month, day, year,\
    hour, minute, second) VALUES("
    for line in data:
        command = (baseCommand+("%s, %s, %i, %i, %i, %i, %i, %i, %i)")%(
        "'" + line["name"] + "'", 
        "'" + line["number"] + "'", int(line["sent"]), 
        int(line["month"]), int(line["day"]), int(line["year"]), 
        int(line["hour"]), int(line["minute"]), int(line["second"])))
        cur.execute(command)
    con.commit()
    