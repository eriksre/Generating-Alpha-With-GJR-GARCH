## Authors
- Eriks Reinfelds 
	- Code writing
	- Model formation + specification
- Connor Maybury
	- Report summary and conclusion
- Pien Bruggeman 
	- Economic model assumptions and specification
- Larissa Venhorst 
	- Literature review
	- Report writing
- Alexander Yee 
	- Research and introduction of problem
	- Video editing
- Thomas Chassat
	- Data exploration
	- Graphing

---
## Code Replication instructions

#### Requirements
- [ ] Git (if cloning repository)
- [ ] R Studio

##### Download and Set Up
1. Clone the repo
	- Alternatively, download manually via github.com
```
git clone https://github.com/eriksre/Generating-Alpha-With-GJR-GARCH.git
```
2. Unzip the data folder 
3. Open R studio
4. Open the repository as a project

##### Running the Code
1. Run the install_libraries.R file to install any dependencies.
2. (OPTIONAL / NOT NECESSARY) To re-create final dataset used in the analysis, run the clean_and_process.r file. The file already exists, so you do not have to re-create it. This exists simply as an option. Note, run times are long. Approx 20 mins on my computer. 
3. To re-create the report results, run the create_and_test_models.qmd file.

---
## Project Synopsis

We wanted to see if adding an arbitrage coefficient between the same cryptocurrency across two different exchanges could improve a GARCH model's ability to predict price and volatility. To test this, we  high-frequency trading data for REEF USDT, streamed via Websockets, from ByBit, GateIO, and XT. Our main goal was to find out if including the arbitrage coefficient (price differential in % between mid price on 2 given exchanges) could make the GARCH model better at predicting cryptocurrency price movements.

We compared a few GARCH models (like GJR-GARCH and E-GARCH) and added the arbitrage coefficient. Adding it to the GJR-GARCH model helped predict the direction of ByBit's mid price, but not it's volatility. We also found that inter-exchange arbitrage coefficients (ie, the arbitrage coefficient between two external exchanges) were significant in predicting price movements on other exchanges (ie, (GateIO Mid / XT Mid) is a significant predictor of Bybit Mid), meaning price discrepancies tend to spread across markets. Finally, we found that the arbitrage coefficient itself turned out to be predictable using an ARMA framework, making it a feature market makers should include in their models. Ie, rather than simply skewing their bid/ask quotes to return price to mid, they may instead, skew based on predictions of a similar model, allowing them to capture gains as an arbitrage coefficient both widens and narrows.

In short, adding the arbitrage coefficient helps with predicting price direction but doesn't improve volatility forecasts. These findings could be useful for market makers and traders when setting bid-ask prices and understanding how price differences across exchanges affect the market (arbitrage coefficient was not found to influence volatility). Future research could dive deeper by optimizing the model parameters, and implementing similar ideas into live trading strategies. 

--- 
## Final References
#### Final Report
```
https://docs.google.com/document/d/1QM6MUhAKpF5RskvtKc9izbPMqDCGf_nK3WuCqOJrTOI/edit?usp=sharing
```
#### Video Presentation
```
https://www.youtube.com/watch?v=eepytRQKdCg
```
#### Final Presentation Slide Deck
```
https://docs.google.com/presentation/d/1CRcqqYySXf7tPWflIOpPxLLDlT4hVXU7S3BJBNQT6pM/edit?usp=sharing
```
