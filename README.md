Parse XML text message output
=============================

Parses the XML output from [Android SMS Backup and Restore](https://play.google.com/store/apps/details?id=com.riteshsahu.SMSBackupRestore&hl=en)
and writes metadata to CSV and a SQLite database (with one table called texts). 

Usage
-----
Either use the functions in `sms.py` for your own purposes or run the `import.py` script at the 
command line. For `import.py`, run

`python import.py basePath encrypt`

where `basePath` is the path to the folder with the sms xml files to be imported. The files should be named 
`sms1.xml`, `sms2.xml` and so on. `encrypt` should be `True` or `False` and determines whether the contact
names and phone numbers should be hashed with sha224. 

Example usage would be 

`python import.py /home/iacobus42/sms/ True`

The script will produce output of `sms1.csv` and so forth for each `.xml` file. It will produce a 
SQLite database `sms.db` in the folder given by `basePath`. If you don't have SQLite installed, it 
will crash after writing out the `.csv` files. 
