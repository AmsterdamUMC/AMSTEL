<img src="https://github.com/AmsterdamUMC/Amstel/blob/master/img/logo_c4i_square.png?raw=1" alt="Logo C4I" width=128px><img src="https://github.com/AmsterdamUMC/Amstel/blob/master/img/logo_amds.png?raw=1" alt="Logo AMDS" width=128px/><img src="https://github.com/AmsterdamUMC/Amstel/blob/master/img/lala_amds.png?raw=1" alt="LaLa" width=128px/><img src="https://github.com/AmsterdamUMC/Amstel/blob/master/img/gaia.png?raw=1" alt="Gaia Scientific" width=128px/><img src="https://github.com/AmsterdamUMC/Amstel/blob/master/img/ehden.png?raw=1" alt="EHDEN" width=128px/>

# AMSTEL: AmsterdamUMCdb - OMOP Common Data Model ETL

The `AMSTEL` GitHub respository and `amstel` R package were developed to convert [AmsterdamUMCdb](https://www.github.com/AmsterdamUMC/AmsterdamUMCdb), the first freely accessible European intensive care database, to the [Observational Medical Outcomes Partnership  Common Data Model version 5.4](https://ohdsi.github.io/CommonDataModel/cdm54.html) (OMOP CDM 5.4) maintained by the [Observational Health Data Sciences and Informatics](https://www.ohdsi.org/) (OHDSI) community. 

This repository contains a collection of resources, mainly documentation, R scripts, SQL queries and mapping files, that form the ETL (Extract, Transform, and Load) process for converting AmsterdamUMCdb to the OMOP Common Data Model.

The ETL for AmsterdamUMCdb is based on the excellent and well-documented ETL of the Sythea project available at: https://github.com/OHDSI/ETL-Synthea.

In this document we describe the steps that were taken to convert AmsterdamUMCdb to the OMOP Common Data Model. When steps are only needed to develop an ETL, and not for the actual conversion of AmsterdamUMC to the CDM, we have added the remark *Development Only*.

Currently, to create an environment that contains the complete OHDSI software stack, a number of installation steps have to be taken to be able to fully utilize the data in the CDM. Since many tutorials were written for Microsoft Windows, this document explicitly contains documentation to use a full open source environment running on Ubuntu, but should equally well run on any other modern operation system.

The `AMSTEL` project is a collaboration between the Center for Critical Care Computational Intelligence of [Amsterdam UMC](https://www.amsterdamumc.nl/), [Amsterdam Medical Data Science](https://amsterdammedicaldatascience.nl/), [Stichting LaLa](http://lalacongres.eu/), [Gaia Scientific](https://www.gaiascientific.com/) and the [European Health Data & Evidence Network](https://www.ehden.eu/).

# Funding
This project has received funding from the Innovative Medicines Initiative 2 Joint Undertaking (JU) under grant agreement No 806968. The JU receives support from the European Union’s Horizon 2020 research and innovation programme and EFPIA.

# Requirements
- AmsterdamUMCDb version 1.0.2 installed in a supported relational database management system (RDBMS), preferably PostgreSQL.
- Read access to the database containing AmsterdamUMCdb
- Read/write access to an (empty) database for creating the required CDM schemas 
- 600 GB or more disk space during conversion, 300 GB after successful conversion.

Development was performed on a Hyper-V based Ubuntu 22.04 LTS virtual machine (12 virtual cores, 64 GB RAM, 1 TB SSD based virtual disk)

**Note**: the SQL queries in this repository have been written in OHDSI SQL, a subset of the SQL Server SQL dialect. The functions as part of the `amstel` package that perform database transactions make use of the OHDSI [DatabaseConnector](https://ohdsi.github.io/DatabaseConnector/) package to convert queries in this dialect into SQL queries that should work on various database systems. However, testing has currently only been performed against PostgreSQL, since [OHDSI WebAPI](https://github.com/OHDSI/WebAPI) dropped support for other RDBMS and requires PostgreSQL to store its data.

# Install R with RStudio
## Install R
```bash
sudo apt-get install r-base
```

## Install RStudio server
Follow the instructions on https://posit.co/download/rstudio-server/ to determine the correct version.
```bash
sudo apt-get install gdebi-core
wget https://download2.rstudio.org/server/jammy/amd64/rstudio-server-2023.03.0-386-amd64.deb
sudo gdebi rstudio-server-2023.03.0-386-amd64.deb
```
Alteratively, you could install R-studio Desktop, if you only intend to use R from your local machine from [here](https://posit.co/download/rstudio-desktop/).

# Clone the AMSTEL Github repository
The Amstel Github respository contains mapping data and a folder structure to succcesfully combine the required data for running the ETL.

## Install git
```bash
sudo apt install git
```

## Clone the repository
```bash
git clone https://github.com/AmsterdamUMC/AMSTEL
```

# Install Java Development Kit
```bash
sudo apt install default-jdk
```

This will install the default Java Development Kit

# Setup a new PostgreSQL user
If working from a (virtual) machine that differs from the  database server, you may be required to set up an new user with Superuser privileges using pgAdmin (default postgres). In addition, a standard postgreSQL setup will require adding the following line to the `pg_hba.conf` file in the root of your PostgreSQL installation path to allow your machine to connect to the databaser server:

```
# add permission to query database from OHDSI (virtual) machine
host all <<your username>> <<your hostname>> scram-sha-256
```

# Install WhiteRabbit 
*Development only* 

OHDSI [WhiteRabbit](https://github.com/OHDSI/WhiteRabbit) is an application to analyse the structure and contents of a database as preparation for designing an ETL. It comes with RabbitInAHat, an application for interactive design of an ETL to the OMOP Common Data Model with the help of the scan report generated by White Rabbit.

**Required steps**
- Install the application by following the instructions on the [WhiteRabbit](https://github.com/OHDSI/WhiteRabbit) Github page. 
- Run WhiteRabbit and connect to the PostgreSQL database to generate the report. 

The report generated by WhiteRabbit for AmsterdamUMCdb can be found [here](./data/whiterabbit/ScanReport.xlsx). 

The ETL design developed for AmsterdamUMCdb using RabbitInAHat can be found [here](./data/whiterabbit/AmsterdamUMCdb-ETL.json.gz). You can open this file from within the RabbitInAHat application.


# Download Vocabularies from Athena
The (standard) vocabularies are an important part of the Common Data Model.
- Open https://athena.ohdsi.org/search-terms/start.
- Login or Create new account using Register
- Click Download, select the (default) vocabularies
- Unzip the files and place them in the [vocabulary](./data/vocabulary/) folder. 

# Install Usagi
*Development only* 

OHDSI [Usagi](https://github.com/OHDSI/Usagi) is an application to help create mappings between coding systems and the Vocabulary standard concepts.

The files containing the source (AmsterdamUMCdb) concepts can be found [here](./data/source_concepts/). The (final) mapping files created using Usagi and that will be used in the ETL can be found [here](./data/mappings/).

# Setup Google Translate Cloud API
*Development only*

The Usagi software uses the names of the concepts to find a matching standard concept in the Vocabulary. Since AmsterdamUMCdb was sourced from a Dutch patient data management system, most concepts can not be used directly to find a corresponding English concept. Follow the tutorial here to setup R to use Google Translate to translate these Dutch concepts to English to improving matching by the Usagi mapping tool: https://github.com/ropensci/googleLanguageR

After creating the JSON credentials file, save this file as `~/.config/amstel/credentials.json`

# Install Hades
OHDSI [HADES](https://ohdsi.github.io/Hades/) is a set of open source R packages for large scale analytics, including population characterization, population-level causal effect estimation, and patient-level prediction. In the context of th ETL, this will be used perform checks and aggregations by the [Achilles](https://ohdsi.github.io/Achilles/) and [DataQualityDashboard](https://ohdsi.github.io/DataQualityDashboard/) packages.

## Install cmake
```bash
sudo apt install cmake libsodium-dev
```

## Install HADES packages
Follow the tutorial here: https://ohdsi.github.io/Hades/installingHades.html. Please note the *requirement* for setting up a GitHub Personal Access Token (PAT).

# Install the `amstel` package
Open RStudio, choose `File` > `Open Project...` and open the `amstel.Rproj` project from the cloned AMSTEL Github repository.

From the R console, run:
```r
# install the `amstel` package from the github repository
install.packages("remotes")
remotes::install_github("AmsterdamUMC/AMSTEL") 

library(amstel)
```
This will install the `amstel` package from GitHub and imports the package into the R environment.

# Configure database connections
Before the ETL can start, we will need to define the settings to connect to the database servers, and optionally change the names of the schemas we'd like to use to store the CDM data into. In addition, we can change the locations of a number of required data sources, including the vocabularies, but it is recommended to leave them at their defaults.

From the R console, run:
```r
amstel::create_config()
```

This wil create a new configuration file (`config.yaml`) at the OS specified location for user settings and opens this file in RStudio for editing. While most settings kan be kept to their defaults, please make sure to set the correct database type, server, username and password for all database schemas (AmsterdamUMCdb, cdm, results, temp).


# Download required database drivers
Based on the configuration file above, the correct drivers to connect to the source and target databases will need to be downloaded once. The drivers by default, will be stored [here](./data/jdbc_drivers/), unless the location has been changed in the `config.yaml` file.
From the R console, run:
```r
amstel::download_drivers()
```

# Run the ETL
To continue using RStudio while running the ETL, it is recommended to run the ETL as a background job since the ETL, data quality checks and aggregation can take between hours and days depending on the performance of the database server. From the `Background Jobs` panel, choose `Start Background Job`, use `Browse` to select the [ETL.R](./ETL.R) script. Make sure the `Working Directory` is set to the directory of the local clone of the AMSTEL GitHub repository, unless you have specified an absolute path to the repository in the `configuration.yaml` file.

During the ETL, the process will be logged to both the console as wel as the [logs](./logs/) folder, unless the defaults have been changed. 

# Install ATLAS
OHDSI [ATLAS](https://github.com/OHDSI/Atlas) is an open source software tool for researchers to conduct scientific analyses on standardized observational data converted to the OMOP Common Data Model V5. It also enables you to graphically browse the reports generated by the ACHILLES aggregation tool.

To install ATLAS, we will need to install its dependencies first.
## Install WebAPI
OHDSI [WebAPI](https://github.com/OHDSI/WebAPI) contains all OHDSI RESTful services that can be called from OHDSI applications including ATLAS. Multiple steps are needed to install WebAPI.
### Install Java 8 JDK
```bash
sudo apt-get install openjdk-8-jdk
```

Before running Maven make sure to set the build environment to the version 8 JRE

```bash
export JAVA_HOME=/usr/lib/jvm/java-1.8.0-openjdk-amd64/jre/
export JRE_HOME=/usr/lib/jvm/java-1.8.0-openjdk-amd64/jre/
```
### Maven
The latest versions can be found here: https://maven.apache.org/download.cgi

```bash
sudo apt install maven
```

### Tomcat 8.0
WebAPI that was built using the Java 8 SDK currently requires Tomcat 8.0, due to changes in newer versions according to the [Apache Tomcat Documentation](https://tomcat.apache.org/download-10.cgi):

```
Users of Tomcat 10 onwards should be aware that, as a result of the move from Java EE to Jakarta EE as part of the transfer of Java EE to the Eclipse Foundation, the primary package for all implemented APIs has changed from javax.* to jakarta.*. This will almost certainly require code changes to enable applications to migrate from Tomcat 9 and earlier to Tomcat 10 and later. A migration tool has been developed to aid this process.
```

#### Create a separate Tomcat user
```bash
sudo groupadd tomcat

sudo useradd -s /bin/false -g tomcat -d /opt/tomcat tomcat
```

#### Download the latest Tomcat 8.0 binaries
```bash
cd /tmp

wget https://dlcdn.apache.org/tomcat/tomcat-8/v8.5.95/bin/apache-tomcat-8.5.95.tar.gz
```

#### Create installation folder
```bash
sudo mkdir /opt/tomcat

cd /opt/tomcat
```

#### Extract files
```bash
sudo tar xzvf /tmp/apache-tomcat-8.5.95.tar.gz -C /opt/tomcat --strip-components=1

sudo chgrp -R tomcat /opt/tomcat

sudo chmod -R g+r conf

sudo chmod g+x conf

sudo chown -R tomcat webapps/ work/ temp/ logs/
```

#### create service for tomcat
```bash
sudo nano /etc/systemd/system/tomcat.service

sudo systemctl daemon-reload
```

### Install WebAPI
#### Clone repository
```bash
git clone https://github.com/OHDSI/WebAPI.git
```
#### Checkout release
```bash
cd WebAPI/

git checkout refs/tags/v2.13.0
```

#### Create `settings.xml`
In the root of the WebAPI repository folder, there is a file named `sample_settings.xml`. Copy this file into a new folder WebAPIConfig and rename it to `settings.xml`.

```bash
mkdir WebAPIConfig

cp sample_settings.xml WebAPIConfig/settings.xml
```

#### Configure the `settings.xml` file
See the information as specified in the offical [documentation](https://github.com/OHDSI/WebAPI/wiki/WebAPI-Installation-Guide#configure-the-maven-profile) to configure the Maven profile in the `settings.xml` file
> nano WebAPIConfig/settings.xml

#### Building the WebAPI .war file using Maven
> mvn clean package -DskipUnitTests -DskipITtests -s WebAPIConfig/settings.xml -P webapi-postgresql

#### Deploy WebAPI to Tomcat
##### Edit the tomcat-users.xml file

> sudo nano /opt/tomcat/conf/tomcat-users.xml

Add to `<tomcat-users>` block

```xml
<role rolename="manager-gui"/>
  <user username="tomcat" password="<<my_tomcat_password>>" roles="manager-gui"/>
```

Edit settings to allow large uploads:

> sudo nano /opt/tomcat/webapps/manager/WEB-INF/web.xml

```xml
    <multipart-config>
      <!-- 50MB max -->
      <max-file-size>52428800</max-file-size>
      <max-request-size>52428800</max-request-size>
      <file-size-threshold>0</file-size-threshold>
    </multipart-config>

```
Update this to read:

```xml
    <multipart-config>
      <!-- 1024MB max -->
      <max-file-size>1073741824</max-file-size>
      <max-request-size>1073741824</max-request-size>
      <file-size-threshold>0</file-size-threshold>
    </multipart-config>
```
#### Run Tomcat
> sudo /opt/tomcat/bin/startup.sh run

#### Deploy WebAPI using Tomcat Application Manager

Open a web browser, navigate to http://localhost:8080/manager and you will be prompted for credentials. Enter the username of `tomcat` and password `<<my_password>>` configured in the step above.

You will then see the manager screen. At the bottom of the manager screen, you will see an area labled `WAR file to deploy`. Click on the `Choose File` button and navigate to the location of the WebAPI.war file created in one of the previous steps. Click the `Deploy` button to deploy and start up WebAPI.

#### Verify connection
Verify succesfull installation of WebAPI by navigating to:
http://localhost:8080/WebAPI/info. You should receive a json object containg the version info, e.g.
```json
{"version":"2.13.0","buildInfo":{"artifactVersion":"WebAPI 2.13.0","build":"NA","timestamp":"Fri Oct 27 23:25:17 UTC 2023","branch":"b4acb3a27962207aec132b5e79dafa181c41f1da","commitId":"b4acb3a","atlasRepositoryInfo":{"milestoneId":42,"releaseTag":"*"},"webapiRepositoryInfo":{"milestoneId":43,"releaseTag":"*"}},"configuration":{"security":{"samlActivated":false,"enabled":false,"samlEnabled":false},"vocabulary":{"solrEnabled":true},"person":{"viewDatesPermitted":false},"plugins":{"atlasgisEnabled":false},"heracles":{"smallCellCount":"5"}}}
```

### Create WebAPI tables
Creates the required WebAPI tables based on the CDM data and insert source records for the connection to the AmsterdamUMCdb CDM.

From the R console, run:
```r
amstel::create_webapi_tables()
```

Refresh WebAPI to show the added CDM instance:
http://localhost:8080/WebAPI/source/refresh

```json
[{"sourceId":3,"sourceName":"Amsterdam University Medical Centers Database v1.0.2","sourceDialect":"postgresql","sourceKey":"AmsterdamUMCdb v1.0.2","daimons":[{"sourceDaimonId":2,"daimonType":"CDM","tableQualifier":"cdm_54","priority":0},{"sourceDaimonId":3,"daimonType":"Vocabulary","tableQualifier":"cdm_54","priority":1},{"sourceDaimonId":4,"daimonType":"Results","tableQualifier":"cdm_results","priority":1},{"sourceDaimonId":5,"daimonType":"Temp","tableQualifier":"cdm_temp","priority":0}]}]
```

You can check the current sources by opening: http://localhost:8080/WebAPI/source/sources
```json
[{"sourceId":3,"sourceName":"Amsterdam University Medical Centers Database v1.0.2","sourceDialect":"postgresql","sourceKey":"AmsterdamUMCdb v1.0.2","daimons":[{"sourceDaimonId":2,"daimonType":"CDM","tableQualifier":"cdm_54","priority":0},{"sourceDaimonId":3,"daimonType":"Vocabulary","tableQualifier":"cdm_54","priority":1},{"sourceDaimonId":4,"daimonType":"Results","tableQualifier":"cdm_results","priority":1},{"sourceDaimonId":5,"daimonType":"Temp","tableQualifier":"cdm_temp","priority":0}]}]
```

## Install Apache Webserver
The OHDSI Setup guide does not suggest a default setup. For Apache 2 Webserver:

```bash
sudo apt install apache2
```

## Install NodeJS

1. Download and import the Nodesource GPG key
```bash
sudo apt-get update
sudo apt-get install -y ca-certificates curl gnupg
sudo mkdir -p /etc/apt/keyrings
curl -fsSL https://deb.nodesource.com/gpgkey/nodesource-repo.gpg.key | sudo gpg --dearmor -o /etc/apt/keyrings/nodesource.gpg
```

2. Create deb repository
```bash
NODE_MAJOR=21
echo "deb [signed-by=/etc/apt/keyrings/nodesource.gpg] https://deb.nodesource.com/node_$NODE_MAJOR.x nodistro main" | sudo tee /etc/apt/sources.list.d/nodesource.list
```

3. Run Update and Install
```bash
sudo apt-get update
sudo apt-get install nodejs -y
```

## Install OHDSI ATLAS
1. Download the lastest release

```bash
cd ~/git
git clone https://github.com/OHDSI/ATLAS.git
```

2. Checkout latest release

```bash
cd ATLAS/
git fetch --tags
latestTag=$(git describe --tags `git rev-list --tags --max-count=1`)
git checkout $latestTag
```

3. Build latest release

```bash
npm run build
```

4. Copy ATLAS build to Apache Webserver

```bash
sudo cp -r ./ /var/www/html/atlas/
```

## Run ATLAS
Navigate to http://localhost/atlas.
You should now have a running instance of ATLAS and should be able to browser that aggregated data and perform analyses.


# Trivia:
AMSTEL: an anagram of AMS (Amsterdam) ETL (Extract, Transform, and Load), named after the river flowing through the city of Amsterdam