# Data to test DataAccessApp

dbconfLocal.yaml : yaml file configuration used to connect to database (snotest.dump)

snotest.dump : pg_dump file used to create a database test :
	
- to see database : 
```
pg_restore --list snotest.dump
```
- to create database : 
```
pg_restore --clean --dbname sno -h localhost -p 5432 -U snouser snotest.dump
```

## Docker

### Build and run

```
docker build -t snodb .
docker run -d --name snodbrun -p 5433:5432 snodb:latest
```

### Restore database
```
docker exec -i snodbrun pg_restore --clean --dbname sno -h localhost -p 5432 -U snouser < snotest.dump
```
