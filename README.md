# What do Twitter Comments Tell About News Article Bias? Assessing the Impact of News Article Bias on its Perception on Twitter
This repository accompanies the article "What do Twitter Comments Tell About News Article Bias? Assessing the Impact of News Article Bias on its Perception on Twitter". More information will be added in case of acceptance of the paper. 

In the following, we briefly describe the available scripts. All models can be found here: https://zenodo.org/record/7141335

### R Script:
1. adfontes:
This script collects all ad fontes media data

2. statistical_analysis: 
This script contains the correlation matrix calculatd for the final dataset and the multi-level regression models

### Python Script
1. scrape-all-tweets:
This script collects all Twitter data for all outlets during a particular time period (cf. Step 1)

2. search-original-tweets:
This script contains the sequential regex-based matching to find the original tweets, i.e., the tweets referring to a rated article (cf. Step 2)

3. scrape-all-comments & scrape-all-retweets:
These two scripts collect the comments and quoted retweets to all original tweets (cf. Step 3)

4. prepare-dataset:
This script combines all original tweets with its corresponding comments and quoted retweets.
This dataset is the basis for examining the comment characteristics.

5. hate-training & sentiment training: 
These two scripts fine-tune the xlnet-base-cased for hate speech detection and sentiment analysis respectively.

6. predict-hate & sentiment-training: 
These two scripts classify the comments on the articles according to their level of hate and their sentiment polarity. 

7. perspective-api:
This script requests Perspective API attributes for all all comments. 

8. final-dataset: 
This script merges all collected data.

### Note:
The Twitter data cannot be published in this repository because Twitter prohibits the publication of the data.
In case you want to see all data, please refer to the Google Drive link or contact me. <br>
The two fine-tuned XLNet-based models have not been added to this repository because the files are too big. 
Please also refer to the Google Drive link or contact me in case of interest.
