Parse XML text message output
=============================

Example dashboard now live at [here](http://jacobsimmering.com:3838/sms/).

Parses the XML output from [Android SMS Backup and Restore](https://play.google.com/store/apps/details?id=com.riteshsahu.SMSBackupRestore&hl=en)
and writes metadata to CSV and a SQLite database (with one table called texts). This 
data can be used by a Shiny app to construct a dashboard of your text message based
friendships.

Usage
-----
### Database Construction
Either use the functions in `sms.py` for your own purposes or run the `import.py` script at the 
command line. For `import.py`, run

`python import.py basePath encrypt`

where `basePath` is the path to the folder with the sms xml files to be imported. The files should be named 
`sms1.xml`, `sms2.xml` and so on. `encrypt` should be `True` or `False` and determines whether the contact
names and phone numbers should be hashed with sha224. 

Example usage would be 

`python import.py /home/iacobus42/sms/ True`

The script will produce output of `sms1.csv` and so forth for each `.xml` file. If fewer than 1 files are 
found in the basepath, an warning will print saying 0 files were imported and to check the path. The 
trailing parens is required (`/home/iacobus42/sms` will not work while `/home/iacobus42/sms/` will). 

The script will produce a SQLite database `sms.db` in the folder given by `basePath`. If you don't have 
SQLite installed, it may crash after writing out the `.csv` files. 

To connect to the database after writing it out, from the command line run 

`sqlite3 path/to/data/base/sms.db`

and you'll enter a SQLite session with the `sms.db` database. 

### R and Shiny Dashboard
In the file `app.R`, change the path to the database in line 7 (under the comment) from mine to 
the path for your database. 

Load an R session and running the script with that change will generate the dashboard. RStudio 
will automatically start a brower session, otherwise you will need to connect to the 
port directed in the console. 

Note that the dashboard requires the packages `shiny`, `shinydashboard`, `dplyr` and `ggplot2`
in addition to `survival` (`survival` should already be installed, the rest can be installed 
using `install.packages()` except for `shinydashboard` which is installed using the `devtools`
package with the command `devtools::install_github("rstudio/shinydashboard")` or by using 
`R CMD build` with the source from the `shinydashboard` repo. 

