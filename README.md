# NoFAQ - A tool that learns to repair your command line mistakes
# Copyright 2015-2016 Loris D'Antoni, Rishabh Singh, Michael Vaughn

NoFAQ is a web application that uses synthesis techniques to iteratively learn to repair incorrect command line usage based on training examples which consist of triples of <incorrect command> <error message> <corrected command>. Details of the synthesis algorithm will be available in our publication.

The application consists of two main components 1.) a dynamic web page served by a python back-end, and 2.) an F#-based web API that provides a set of calls that invoke the synthesis algorithm, as well as related functionality.

Persistent data is provided to both components via a MySQL database - the database model is specified pythonically in WebInterface/dataModel.py. We use Peewee as our database ORM. Additional code required for database initialization is located in DatabaseAdjustments. This code includes creation logic for several stored procedures, and some table alterations (since several MySQL data types are not provided by the default Peewee distribution).

Currently, setup of the application is still manual and complicated. We will clean up and automate it in the future.

Website Setup:
To set up the website interface, you need Python 2.7 and pip.
In the directory WebInterface, install the necessary libraries via "pip install -r requirements.txt"
Next, create a MySQL database. Configure dataModel.py to point to the database using your desired user and password.

Execute migration.py to create the database schema.

Go to DatabaseAdjustments and execute every SQL script located there.

Return to WebInterface, and configure webApp.py to use the desired port.

Run webApp.py, and navigate to localhost:<port>/submit.html, and verify that the website is available.

The website is not yet functional - it makes AJAX calls to the F# application located in Synthesis.

Synthesis Setup:
This project requires .net 4.6 to support the necessary libraries.

In database.fs, change the connection string to point to the database you set up previously.

In main.fs, find the line beginning with "let cfg =" and specify a different port than the one for your website.

Ensure that the project builds and runs.

----
The main web page is located in WebInterface/templates/submit.html
The version of this website currently in use takes advantage of URL rewriting, so to test your version locally, you will need to rewrite the URLS of the AJAX calls to point to <hostname>:<port> of the F# application. 
---


TO DO: Simplify website setup via config files.
Split F# code into several executable programs - one for web app, one to populate the database from the examples, one for various research activities.
Explain how to populate database with examples and rules.

Explain IIS setup and configuration.
