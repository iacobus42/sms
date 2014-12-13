import sms
import time
import sys

startTime = time.time()
basePath = sys.argv[1]
incomplete = True
i = 1
print basePath
while incomplete:
  try:
    f = basePath + "sms" + str(i) + ".xml"
    sms.xml2csv(f, f[0:-3] + "csv")
    if i == 1:
      sms.csv2sqlite(f[0:-3] + "csv", basePath + "sms.db", True, True)
    else:
      sms.csv2sqlite(f[0:-3] + "csv", basePath + "sms.db")
    i = i + 1
  except:
    if i == 1:
      print "Warning: 0 files imported, check provided path"
    incomplete = False
endTime = time.time()

print ("Imported %i files in %i seconds"%((i - 1), 
  round(endTime - startTime)))