# About page
### Information and FAQ

This page provides some context and information about this application.

## 1. Why?
### In which context was this application created and who created it?
This application was created as part of a Bachelor project in *Industrial Engineering & Management* at the University of Twente (Enschede, The Netherlands). It was mainly created by Noor Mansour, but with a lot of help and inspiration from some study colleagues and supervisors. For feedback or specific questions about this tool that are not answered here you can contact me by [Mail](mailto:noor1mansour@outlook.com) or on [LinkedIn](https://www.linkedin.com/in/noor-mansour/). 

### What was the motivation for this application?
The motivation for this application comes from the practical experience with consulting healthcare professionals on logistical performance improvements. The problem is that decision makers lack knowledge in the field of operations management and hence hesitate to implement proven solutions that would improve their processes. The idea was to support the process of teaching and explaining them certain concepts from operations management in an easy to understand and fun way.

### What were the objectives to the development of this application?
There were several objectives to the creation of this application. First of all, it focuses on teaching the effects different appointment scheduling policies for elective surgical patients have on the performance of the hospital especially related to access times and resource utilization of the operating room and other departments like the ward. Secondly, the application should interactively visualize and teach these effects in a fun and engaging way. Then, the tool was supposed to be accessible in a web environment to achieve high flexibility in the teaching process. And of course, it was necessary to take into account the limited time frame of the 10 week Bachelor project.

## 2. How? 
### How was this tool conceptualized?
The tool was conceptualized with the help of a modeling framework for simulation-based serious games. The specific framework was developed by van der Zee et al. (2012). It makes use of five iterable activities: 
1. Understanding the learning environment
2. Determine objectives
3. Identify the model outputs
4. Identify the model inputs
5. Determine model content

For more details see *van der Zee et al.* (2012).

### How and with the help of what was the tool implemented?
The tool is based on an implementation with *R Shiny*, which is a web-framework for the general purpose programming language R. The simulation model was implemented with the discrete event simulation package *simmer* for R, while the graphs were generated with *ggplot2*. For more details and used libraries please see the [code repository](https://github.com/NoorMansour1/Surgical-Chain-R-Shiny).

## 3. What?
### What is this applicaton about?
The goal of this application as a simple simulation based serious game is to convey the following predefined learning goals: 
- Highlighting the effects of Block, Open, Mixed Block scheduling policies for scheduling elective surgical patients in an operating room. 
- Highlighting that decisions made in the operating room have an impact on other departments in the surgical care chain (especially the ward). 
- Showcasing the challenging situation of operating in an uncertain and probabilistic environment and the negative impacts of purely static schedules in such variable environments. 

For a more detailed elaboration on the learning goals and other aspects, please refer to my [thesis](https://essay.utwente.nl/).

### What does the practical application of the tool look like?
The tool makes use of the involvement of a game operator as specified in the conceptual framework. The game operator can be the consultant or teacher, possibly also the player. The operator is supposed to set the tool into different scenarios to bring across different learning goals e.g changing the arrival rate of one patient type to impact the performance of the players current schedule. The player is then challenged to adapt the schedule or policy to level the demand for beds at the ward. More information about how the game can be played and what settings can be changed are presented in the *Manual* page. 