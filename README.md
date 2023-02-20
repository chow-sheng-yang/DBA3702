# Disease Tracker
**Organisation: National University of Singapore**  
**Module: Descriptive Analytics with R, DBA3702**  
**Academic Year: 2022/23 Semester 1**  
**Done By: Chow Sheng Yang, Yin Xin Min, Andrea Lim, Pong Zi Jie**  
**Taught By: Professor Liu Qi Zhang**  

This app is a one-stop platform inclusive of an interactive dashboard designed to automate data-wrangling processeses for epidemiologists (target audience). As large-scaled epidemics often involve hundreds of thousands of data being updated frequently, our app aims to eradicate the inefficiency of such a manually tedious process.

In an attempt to reflect realistic events, the base data is taken from COVID-19 cases in Singapore in 2020 along with other simulated attributes such as postal codes, addresses and coordinates. A package of functions is written in a separate Rmd to handle any dirty data using techniques such as text-mining, monte-carlo simulations, discretising and calculating attributes. The UI/Server in the app then translates the useable data into visual charts for insights. The main page includes a leaflet and ggmap displaying the distribution of cases in Singapore, with various interactions such as filters and map themes. The second page displays graphical distribution of the cases using plots, gganimations and wordcloud. 

_Maps used for this project are fetched from Google Cloud APIs._

_link to view app: https://chowshengyang.shinyapps.io/Project-SA2-Group1/_
