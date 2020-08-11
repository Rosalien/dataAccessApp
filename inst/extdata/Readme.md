# Data to test DataAccessApp

- dbconfLocal.yaml : yaml file configuration used to connect to database (snotest.dump)
- snotest.dump : pg_dump file used to create a database test :
	* to see database : 
	```
	pg_restore --list snotest.dump
	```
	* to create database : 
	```
	pg_restore --clean --dbname sno -h localhost -p 5432 -U snouser snotest.dump
	```



