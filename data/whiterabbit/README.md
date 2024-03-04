# `WhiteRabbit`  folder

Contains files created by the WhiteRabbit and Rabbit-in-a-Hat OHDSI tools.

## ScanReport.xlsx
Created by WhiteRabbit after scanning the AmsterdamUMCdb database running on the local PostgreSQL server. Formed the basis for the ETL document below.

## AmsterdamUMCdb-ETL.json.gz
Created by Rabbit-in-a-Hat after importing `ScanReport.xlsx`. Contains the table to table and field to field mapping including comments to design a ETL pipeline.