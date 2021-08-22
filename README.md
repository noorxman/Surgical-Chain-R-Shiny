# Surgical-Chain-R-Shiny
This is an eucational tool, developed in R Shiny and with the discrete event simulaiton package R simmer, with the goal of teaching the effects different appointment scheduling policys have on the performance of an operating room at a hospital. 

<pre>
src/  
 ├── server.R  
 ├── ui.R   
 ├── text/  
      ├── About.md  
      ├── Manual.md
      ├── Explain_Inputs.png 
      └── Schematic_Simulation.png 
 └── legacy/  
      ├── About.md  
      ├── Manual.md
      ├── Explain_Inputs.png 
      └── Schematic_Simulation.png
 README.md  
</pre>


**src/** Directory with the source code.

**server.R** The server side of the tool; one of the two parts of the shiny application. Handles the inputs from the user, creates/runs the simulation and ouputs the results to the UI.

**ui.R** The user interface of the tool; one of the two parts of the shiny application. The user interface is a web page with multiple tabs including the players page, operators page, manual page and about page. 

**text/** This folder contains all necessary files to display in the application.

**About.md** A markdown document that describes the project and is included in the tool as a tab. 

**Manual.md** A markdown document that describes the usage of the tool and is included in the tool as a tab. 

**Explain_Inputs.png** PNG file that showcases the possible player inputs.

**Schematic_Simulation.png** PNG file that showcases the underlying simulation. 

**legacy/** This folder contains previous iterations and prototypes of the tool and of different simualtions.
