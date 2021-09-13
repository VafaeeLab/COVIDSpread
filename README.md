# COVID-19 Time-Series Forcasting Platform

## Frequently Asked Questions

### Who has developed the platform
Associate Professor Taha Rahsidi and Dr Fatemeh Vafaee with the help of PhD candidate Siroos Shariari and postdoctoral research associate AKM Azad have initiated developing the platform. 

### Are the codes available and can we use them?
Yes, but you need to cite it. The code can be found here in the GitHub Repository: https://github.com/VafaeeLab/COVID19-TS-Forcast 

### Which countries are included?
The left window provides a list of all countries having at least 30 days of days with minimum 50 infected cases. 

### What is meant by interventions?
The platform provides an option for measuring the average impact of previous inventions. These interventions can include lockdowns, step-by-step work from home announcements, shopping centre working hour restrictions, and police enforced social distancing, or complete lockdown. Once several instances of the past interventions are selected in the “add interventions” calendar, the model treats them being of the same type and measures an average impact of rall of them. The estimated average impact of these interventions can be then used for forecasting where a hypothetical intervention(s) can be considered in the next 10 days. 

### How are interventions interned into the model?
Clink on the calendar icon   and select the dates of the past and future interventions. The platform splits the records of intervention into past and future and uses the past intervention in model estimation and future interventions for forecasting.

### Where is the source of data
The data is sourced from European Centre for Disease Prevention and Control (ECDC) online platform (https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide)

### How a subset of data can be selected for model estimation?
The platform allows the user to develop models for a subset of all days for which data is available. This is for two main reasons: 1) the model can change over time and the user may want to develop step-wise models for different parts of the data, 2) predicting the progression of the spread of COVID19 at specific times in the past allows us to understand how effective the preventive strategies were. 
By clicking on the calendar icon on top of the “predict future spread” button  a window will open allowing for selection of the dates for which the model should be estimated. Note that that any interventions outside this time window will be excluded from the model. 

### How often is the dashboard information updated?
As the input data get updated the dashboard also get updated.

### What mathematical specifications are included in the backend? 
The code develops several transformations of the number of infected cases, including log-transformation, root-transformation, and ratio transformation, and then applies differencing on the data so obtain stationary data.  Following this, several ARIMA(p, i, q) models are developed and the best is selected based on an in-sample validation using the RMSE indicator.

### What does the map show?
The map shows the forecast of the number of infected cases in the world using the data provided by ECDC.

### What is the use of the Top 10 Countries?
This graph shows the spread of progression in different countries. 

### Can I see the estimated parameters?
Yes, click on the “Model Parameter” button and the parameters of the selected model are reported. 

### Is the platform copyrighted?
The website and its contents, including all data, mapping, and analysis, copyright 2020 UNSW, all rights reserved, is provided to the public strictly for educational and academic research purposes.

### Who can I talk to for more information about the dashboard?
For general information contact Dr Vafaee at f.vafaee@unsw.edu.au or A/Prof Taha Rashidi at rashidi@unsw.edu.au .
