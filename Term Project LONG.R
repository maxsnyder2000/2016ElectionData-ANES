# Term Project
# Partners: Max Snyder, Gordon Kamer (2 team members) (POINT)

# Data: American National Election Survey
# Link: https://drive.google.com/file/d/1sXt531BuJm0ZoGNffywswxlI1r5397oC/view?usp=sharing
# Origin: https://electionstudies.org/project/2016-time-series-study/

# NOTE: Variable names from our dataset are very cryptic (V followed by several numbers).
# We submitted a PDF with variable name descriptions for reference.

DF <- read.csv("anes_timeseries_2016_short.csv")  # Dataframe (POINT)

# Two categorical columns (POINT)
head(DF$V162034a)  # 2016 vote
head(DF$V161006)   # 2012 vote

# Two numeric columns. (POINT)
head(DF$V161086)  # feeling thermometer for Clinton
head(DF$V161087)  # feeling thermometer for Trump

# At least 20 rows, preferably more, but real-world data may be limited. (POINT)
nrow(DF)

# A data set with lots of columns (POINT),
# allowing comparison of many different variables (which we do later!)
ncol(DF)
# In actuality, the original dataset has 1837 columns.
# We cut down the dataset to only the columns we needed for upload-size purposes.

# Professional-looking software engineering (POINT)
# (these functions are used later)
cleanFT <- function(ft) { is.integer(ft) & 0 <= ft & ft <= 100 }  # boolean to clean a feeling thermometer column

extractInt <- function(string) { strtoi(substr(string, 1, 1)) }   # extracts 1-digit integer from string

percent.right <- function(model, data_frame, actual)              # calculates percent of prediction model correct
{
  comparison_points <- cbind(predict(model,data_frame,type="resp"), actual, factor=.3)
  comparison_points <- na.omit(comparison_points)
  correct <- numeric(nrow(comparison_points))
  for (i in 1:length(correct))
  {
    if(((comparison_points[i,1] >= .5) && (comparison_points[i,2] == 1)) || ((comparison_points[i,1] <= .5) && (comparison_points[i,2] == 0)))
    {
      correct[i] <- 1
    }
    else
    {
      correct[i] <- 0
    }
  }
  sum(correct) / length(correct)
}

predict_user <- function(model)   # predict probability of Trump vote
{
  print("For the following prompts, please enter an integer response.")
  
  prompts <- c("On a scale of 1-7, 1 being extremely liberal and 7 being extremely conservative, how would you rate your political ideology?",
               "What is your party identification? (1) Strong Democrat, (2) Not very strong Democrat, (3) Indepdent-Democrat, (4) Independent, (5) Independent-Republican, (6) Not very strong Republican, (7) Strong Republican",
               "When selecting someone for the Supreme Court, how much should the way the nominee is likely to vote on controversial issues be considered? (1) A great deal, (2) A lot, (3) A moderate amount, (4) A little, (5) Not at all",
               "Do you favor, oppose, or neither favor nor oppose building a wall on the U.S. border with Mexico? (1) Favor, (2) Neither favor nor oppose, (3) Oppose",
               "How often can you trust the federal government in Washington to do what is right? (1) Always, (2) Most of the time, (3) About half the time, (4) Some of the time, (5) Never ",
               "You may have heard about the idea that the world's temperature may have been going up slowly over the past 100 years. What is your personal opinion on this? Do you think this has probably been happening, or do you think it probably hasn't been happening? (1) Has probably been happening, (2) Probably hasn't been happening",
               "Which is closest to your view on gay marriage? (1) Gay and lesbian couples should be allowed to legally marry. (2) Gay and lesbian couples should be allowed to form civil unions but not legally marry. (3) There should be no legal recognition of a gay or lesbian couple's relationship.",
               "What comes closest to your view: (1) The Bible is the actual word of God and is to be taken literally, word for word. (2) The Bible is the word of God but not everything in it should be taken literally, (3) The Bible is a book written by men and is not the word of God",
               "During a typical week, how many days do you watch, read, or listen to news on TV, radio, printed newspapers, or the Internet, not including sports? Answer 0-7.",
               "What should happen to immigrants who were brought to the U.S. illegally as children and have lived here for at least 10 years and graduated high school here? (1) Should be sent back where they came from, (2) Should be allowed to live and work in the U.S.",
               "Would you say that compared to 2008, the nation's economy is now better, worse or about the same? (1) Better, (2) About the same, (3) Worse")
  
  quiz_responses <- c()
  
  for (i in 1:length(prompts)){
    print(prompts[i])
    x <- as.integer(readline(prompt="Enter: "))
    switch(
      i,
      quiz_responses$V161126 <- x,
      quiz_responses$V161158x<- x,
      quiz_responses$V161176 <- x,
      quiz_responses$V161196 <- x,
      quiz_responses$V161215 <- x,
      quiz_responses$V161221 <- x,
      quiz_responses$V161231 <- x,
      quiz_responses$V161243 <- x,
      quiz_responses$V161008 <- x,
      quiz_responses$V161195 <- x,
      quiz_responses$V161235 <- x
    )
  }
  
  resp <- predict(fit,quiz_responses,type="resp")
  
  paste("Probability of a Trump vote:",round(resp, 2))
}


# Barplot of how many people in the dataset voted for Obama or Romney or other in 2012
# This is a cursory check to see how well the sample represents the national electorate
# We can compare these results to what actually happened

# clean it up
DF_BarplotClean <- DF$V161006[(DF$V161006 == '1. Barack Obama' | DF$V161006 == '2. Mitt Romney' | DF$V161006 == '5. Other SPECIFY')]

# A barplot (POINT)
barplot(table(droplevels(DF_BarplotClean)), main="Obama vs. Romney", ylab="Respondents", col=c("blue","red", "green"))
# While Obama may have a little more support in the sample than in reality, it's possible that people who voted for the loser said that they "couldn't remember" who they voted for

# We can compare that with the barplot for Clinton / Trump:

# clean it up
DF_BarplotClean2 <- DF$V162034a[(DF$V162034a == '1. Hillary Clinton' | DF$V162034a == '2. Donald Trump' | DF$V162034a == '3. Gary Johnson' | DF$V162034a == '4. Jill Steiin')]

barplot(table(droplevels(DF_BarplotClean2)), main="Clinton vs. Trump vs. Johnson vs. Stein", ylab="Respondents", col=c("blue","red", "darkblue", "green"))

# Lots more people voted third party in 2016, and, obviously, the race for president was closer.
# We can see that in this sample, Hillary Clinton wins the popular vote, which mirrors what actually happened.



# STUDY #1
# The relationship between the support for the two major candidates

# We first use linear regression to model the relationship between respondent's "feeling thermometer" score (1-100)
# for Clinton and for Trump.
# Here, we can predict your support for Trump based on your support for Clinton

# Cleaning
ftDem <- DF$V161086; ftRep <- DF$V161087
data <- cbind(ftDem, ftRep)
data <- data[cleanFT(ftDem) & cleanFT(ftRep),]
ftDem <- data[,1]; ftRep <- data[,2]

# Use of linear regression (POINT)
LR <- lm(ftRep ~ ftDem)
plot(ftDem, ftRep, main = "Feeling Thermometers: Trump vs. Clinton", xlab = "Clinton", ylab = "Trump", col = "gray")

# There is a clear negative relationship, which is expected
abline(LR, col = "red")

LR$coefficients[2]  # slope
# The more you support Clinton, the less like it is that you support Trump
# Your feeling thermometer goes down on trump by .67 by every point more that you like Clinton

# However, the relationship between the scores is perhaps not precisely linear, which is a concern
summary(LR)$r.squared  # moderate relationship

# If we try exponential decay (i.e. y = ae^(-bx) + c), here are the results:
x <- replace(ftDem, ftDem == 0, 0.0001); y <- replace(ftRep, ftRep == 0, 0.0001)
m <- nls(y ~ a*exp(-b*x)+c, start=list(a=50,b=1,c=0))

cor(y, predict(LR)) # correlation of the linear model
cor(y, predict(m))  # very slight increase in correlation with nonlinear model

# A graphical display that is different from those in the class scripts (POINT)
plot(y ~ x, main = "Feeling Thermometers: Trump vs. Clinton", xlab = "Clinton", ylab = "Trump", col = "grey")
lines(x,predict(m),col="red", type="p")  # here is the exponential decay model

# This model fits slightly better, likely because the high amount of "0" responses for both candidates
# pulls the line of best fit towards the axes.

# Moving on, we're going to do a permutation test
# We're interested in if Hillary Clinton voters liked Hillary Clinton more than Trump voters liked Trump
# We're going to compare the means of the survey's feeling thermometer (score from 1-100) on each candidate

dName <- "1. Hillary Clinton"
rName <- "2. Donald Trump"

votes <- DF$V162034a  # vote for president
ftDem <- DF$V161086  # thermometer for Clinton
ftRep <- DF$V161087  # thermometer for Trump

# Cleaning
data <- cbind(votes, ftDem, ftRep)
data <- data[!is.na(votes) & ((votes == dName & cleanFT(ftDem)) | (votes == rName & cleanFT(ftRep))),]

# More cleaning
votes <- data[,1]; ftDem <- data[,2]; ftRep <- data[,3]; ft <- numeric(length(votes))
dName <- 6; rName <- 7
for (i in 1:length(votes))
{ if (votes[i] == dName) {ft[i] <- data[i,2]} else {ft[i] <- data[i,3]} }

DemAvg <- sum(ft[votes == dName])/sum(votes == dName)  # Clinton mean
RepAvg <- sum(ft[votes == rName])/sum(votes == rName)  # Trump mean

observed <- DemAvg - RepAvg; observed  # observed difference in means is about 2.5

# Permutation test (POINT)
N <- 50000; diffs <- numeric(N)
for (i in 1:N)
{
  Candidate <- sample(votes)
  DemAvg <- sum(ft*(Candidate==dName))/sum(Candidate==dName); DemAvg
  RepAvg <- sum(ft*(Candidate==rName))/sum(Candidate==rName); RepAvg
  diffs[i] <- DemAvg - RepAvg
}

# Histogram (POINT)
hist(diffs, breaks = "FD", probability = T, main = "Permutation Test: Clinton minus Trump Feeling Therm.", xlab = "Clinton - Trump", ylab = "Probability Density")  # histogram of sample means, if candidates were random
abline(v = observed, col = "red")  # observed difference in means

# Probability Density Graph overlaid on a Histogram (POINT)
# Because of the CLT, we can overlay a normal distribution with the sample mean and standard deviation
curve(dnorm(x, mean = 0, sd = sd(diffs)), col = "blue", add = T)

# Calculating P value (POINT)
pvalue <- (sum(diffs >= observed)+1)/(N+1); pvalue  # the probability a difference as large arose by chance
# Because the p value is so low (p < .005), we can call this result statistically significant (there is a difference)

# Compare results of simulation to classical method (POINT)
# We can compare the results of our simulation to a t test
t <- t.test(ft ~ (votes == rName), alternative = "greater"); t
t$p.value; pvalue  # about the same

# Confidence interval of thermometer difference (POINT)
t <- t.test(ft ~ (votes == rName), alternative = "two.sided"); t
t$conf.int
# There is a 95% chance that the score of Hillar-Trump is between (.800, 4.197)

# We can conclude that Hillary voters liked Hillary MORE than Trump's liked Trump on average
# It suggests that Republicans might have held their nose and voted for Trump, while Hillary voters liked her more

# Interestingly, the quantiles reveal something odd about this conclusion:
# Cleaning
ftDemOnly <- numeric(sum(votes == dName)); ftRepOnly <- numeric(sum(votes == rName))

j <- 1; k <- 1
for (i in 1:length(votes))
{
  if (votes[i] == dName)
  {ftDemOnly[j] <- ftDem[i]; j <- j + 1}
  else
  {ftRepOnly[k] <- ftRep[i]; k <- k + 1}
}

# Appropriate use of quantiles to compare distributions (POINT)
quantile(ftDemOnly)
quantile(ftRepOnly)

# These quantiles are identical (because voters tend to give round number scores),
# even though the permutation test revealed a statistically significant difference.
# This reveals that, overall, the distribution of voters' feelings towards their own candidates are very similar,
# although, statistically, Hillary voters gave her a slightly higher edge by about 2.5 points.



# STUDY #2
# Exploring the liklihood that a voter believes that Obama is a Muslim

# Clean up table to only get yes/no to the muslim question and only democrats and republicans
# While it does eliminate people who are unsure, most people are not unsure,
# and the rest of the people eliminated are those who did not have a post election interview - a basically random subset

# While it elimiantes third parties, we're really just concerned with the democrat/republican split here
# So this seems fairly representative

DF_TableClean <- DF[(DF$V162255 == '1. Muslim' | DF$V162255 == '2. Not a Muslim') & (DF$V161019 == '1. Democratic party' | DF$V161019 == '2. Republican party') ,]

muslim <- droplevels(DF_TableClean$V162255)
party <- droplevels(DF_TableClean$V161019)

# Contingency table (POINT)
tbl <- table(muslim,party); tbl

# There are appears to be a relationship between being a republican and believing Obama is a Muslim
# Unsurpsing...

# Analaysis of contingency table: chi square (POINT)
chi <- chisq.test(muslim, party)
chi$p.value  # p value
# Yeah, as you can see, we have very strong statistical proof (p < 1.0e-15) that being a Republican and believing Obama is a Muslim are correlated

tbl[1,1] / (tbl[1,1] + tbl[2,1])  # percentage of democrats (who answered yes/no) who answered Muslim
# But it's a bit surprising that so many democrats responded "Muslim" (although, remember, eliminating the unsure responses, even though there are only few)
tbl[1,2] / (tbl[1,2] + tbl[2,2])  # percentage of republicans (who answered yes/no) who said Obama is a Muslim
# Maybe this idea is not so fringe... a little surprising

# Let's compare this to something else - how about belief in mandatory vaccination at schools

# Cleaning
DF_TableClean <- DF[(DF$V162146 == "1. Favor" | DF$V162146 == "2. Oppose") & (DF$V162255 == '1. Muslim' | DF$V162255 == '2. Not a Muslim'),]
muslim <- droplevels(DF_TableClean$V162255)
vaccines <- droplevels(DF_TableClean$V162146)

# contingency table
tbl <- table(muslim,vaccines); tbl

tbl[1,1] / (tbl[1,1] + tbl[2,1])  # proportion of those who favor vaccines who believe Obama is a Muslim
tbl[1,2] / (tbl[1,2] + tbl[2,2])  # proportion of those who oppose vaccines who believe Obama is a Muslim

chi <- chisq.test(muslim, vaccines); 
chi$p.value  # and it's significant once again! (p < 1.0e-5)

# OK - now what about views on the economy? Are the "forgotten men and women" - who believe the economy has gotten worse since 2008 - more like to believe Obama is a Muslim?

# Let's take only people who answered "better" or "worse" - leaving out people who say "about the same", for the simplicity of the table
DF_TableClean <- DF[(DF$V161235 == "1. Better" | DF$V161235 == "2. Worse") & (DF$V162255 == '1. Muslim' | DF$V162255 == '2. Not a Muslim'),]
muslim <- droplevels(DF_TableClean$V162255)
economy <- droplevels(DF_TableClean$V161235)

# contingency table
tbl <- table(muslim,economy); tbl

tbl[1,1] / (tbl[1,1] + tbl[2,1])  # proportion of those who believe the economy has gotten better who believe Obama is a Muslim
tbl[1,2] / (tbl[1,2] + tbl[2,2])  # proportion of those who believe the economy has gotten worse who believe Obama is a Muslim

# The difference here is even MORE striking than the party comparison! Wow!

chi <- chisq.test(muslim, economy); 
chi$p.value  # and it's significant once again, obviously! (p < 1.0e-113) - holy cow



# STUDY #3
# Modeling the likelihood that a respondent voted for Trump.

# Basically, let's see if we can predict if someone is going to vote for Trump before that person even steps foot in the ballot box
# This is incredibly important for political campaigns, and many campaigns have models that attempt to do this.
# It can affect campaign strategies that choose to emphasize certain issues depending on their salience

# First, let's do a simple logistic regression curve using only the self-reported liberal/conservative placement on a 7 point scale
# While in this case, it may make more sense just to look at the probabilites for each specific score, this approach will work better when we put many more variables into the model

# Let's extract the two columns we care about: the respondent's vote for president and the liberal/conservative score
# We also filter to make sure we get an answer on the liberal/conservative score (we accept the loss of leaving out people who haven't thought about it much)
libcon <- DF$V161126[(DF$V161126 == "1. Extremely liberal" | DF$V161126 == "2. Liberal" | DF$V161126 == "3. Slightly liberal" | DF$V161126 == "4. Moderate, middle of the road" | DF$V161126 == "5. Slightly conservative" | DF$V161126 == "6. Conservative" | DF$V161126 == "7. Extremely conservative")]
trumpvote <- as.numeric((DF$V162034a == "2. Donald Trump")[(DF$V161126 == "1. Extremely liberal" | DF$V161126 == "2. Liberal" | DF$V161126 == "3. Slightly liberal" | DF$V161126 == "4. Moderate, middle of the road" | DF$V161126 == "5. Slightly conservative" | DF$V161126 == "6. Conservative" | DF$V161126 == "7. Extremely conservative")])
DF_SimpleLog <- data.frame(libcon, trumpvote)

DF_SimpleLog$libcon <- strtoi(substr(DF_SimpleLog$libcon, 1, 1))  # convert libcon to an integer

head(DF_SimpleLog)

fit <- glm(trumpvote~libcon,data=DF_SimpleLog,family=binomial())  # making the model
summary(fit)

# Calculating and graphing logistic regression curve (POINT)
# Drawing a graph the old fashion way, using the coefficients
co1 <- as.numeric(fit$coefficients[1])
co2 <- as.numeric(fit$coefficients[2])
curve(exp(co1+co2*x) / (1+exp(co1+co2*x)), col = "blue", xlim=c(0,10)) # NOTE: Data is from 1 to 7, but we use 0 to 10 here just to show the full shape of the curve
# and now a way that R has, which is the same
curve(predict(fit,data.frame(libcon=x),type="resp"), xlim=c(0,10), main = "Logistic Regression: P(Trump Vote)", xlab = "Liberal --> Conservative Scale (1-7)", ylab = "P(Trump Vote)", col = "red")

# Now let's overlay the actual data
# to make it fit better on the screen, we're going to take samples of the data
ill_sample <- sample(1:nrow(DF_SimpleLog), 500);
ill_sample <- DF_SimpleLog[ill_sample,]; head(ill_sample)
# we use jitter to make it easier to see
plot(jitter(ill_sample$libcon, factor=.5),jitter(ill_sample$trumpvote, factor=.5), xlim=c(1,7), main = "Trump Vote for Lib/Con Scale", xlab="Liberal/Conservative", ylab="Voted for Trump")
curve(exp(co1+co2*x) / (1+exp(co1+co2*x)), col = "blue", xlim=c(1,7), add=TRUE)
# Does an OK job, but now let's move on to the more robust (but less graphically intuitive) version of the model

# Included variables:

# liberal/conservative 7pt scale - V161126
# Party ID - V161158x

# How to select justices for the Supreme Court - V161176
# Build a wall - V161196/V161196a
# Trust in washington - V161215
# Is global warming happening - V161221
# Gay marriage - V161231
# Is bible the word of God or men - V161243
# Days in week that you watch news - V161008
# Should illegal aliens be sent back - V161195
# Is economy better off since 2008 - V161235


# DATA CLEANING
# Most of this is converting the string responses into 1 number integers
DF_Log <- DF

# Vote for trump
DF_Log$V162034a <- extractInt(DF_Log$V162034a)
DF_Log$V162034a <- as.numeric((DF_Log$V162034a == 2))

# liberal/conservative spectrum
DF_Log$V161126 <- extractInt(DF_Log$V161126)
DF_Log <- DF_Log[DF_Log$V161126 != 9,]

# party ID
DF_Log$V161158x <- extractInt(DF_Log$V161158x)

# Supreme court
DF_Log$V161176 <- extractInt(DF_Log$V161176)

# Build a wall - switching 2s and 3s because 3 in the original dataset is the middle position
DF_Log$V161196 <- extractInt(DF_Log$V161196)
DF_Log$V161196 <- replace(DF_Log$V161196, DF_Log$V161196 == 2, 4)
DF_Log$V161196 <- replace(DF_Log$V161196, DF_Log$V161196 == 3, 2)
DF_Log$V161196 <- replace(DF_Log$V161196, DF_Log$V161196 == 4, 3)

# Trust in washington
DF_Log$V161215 <- extractInt(DF_Log$V161215)

# Global warming
DF_Log$V161221 <- extractInt(DF_Log$V161221)

# Gay marriage - V161231
DF_Log$V161231 <- extractInt(DF_Log$V161231)

# Bible - V161243
DF_Log$V161243 <- extractInt(DF_Log$V161243)
DF_Log <- DF_Log[DF_Log$V161243 != 5,]

# Days in week watch news - V161008
DF_Log$V161008 <- extractInt(DF_Log$V161008)

# Should illegal aliens be sent back - V161195
DF_Log$V161195 <- extractInt(DF_Log$V161195)

# Is economy better off since 2008 - V161235 (switch 2s and 3s to make "Worse" associated with larger number)
DF_Log$V161235 <- extractInt(DF_Log$V161235)
DF_Log$V161235 <- replace(DF_Log$V161235, DF_Log$V161235 == 2, 4)
DF_Log$V161235 <- replace(DF_Log$V161235, DF_Log$V161235 == 3, 2)
DF_Log$V161235 <- replace(DF_Log$V161235, DF_Log$V161235 == 4, 3)

# Now to the model
fit <- glm(V162034a~V161235+V161195+V161126+V161158x+V161176+V161196+V161215+V161221+V161231+V161243+V161008,data=DF_Log,family=binomial())  # making the model
summary(fit)

# Now we can see how good the fit was (again, using jitter to make graphically better)
# We graph the model's prediction on the x axis with whether or not the respondent voted for Trump on the y
comparison_points_j <- cbind(predict(fit,DF_Log,type="resp"), jitter(DF_Log$V162034a, factor=.3))
plot(comparison_points_j, main = "P(Trump Vote)", ylab="Voted for Trump", xlab="Predicted Probability")
# As you can see, the density of points in the top right and bottom left corners of the plot indicate graphically that the model is doing a decent job

# What percentage of the time does the model get it right, with a threshold of 50%?
percent.right(fit, DF_Log, DF_Log$V162034a)  # percent right
# The model is correct quite a bit (>90%)!

# Note: we could have just trained the model using half of the dataset and then compared with the other half in order to avoid overfitting
# Besides the fact that this seems outside the scope of this course, it seems unlikely to have overfitting here
# since there's a logical connection between the variables, and there aren't a tremendous amount of unique answers

# What if we add a few non-political variables - will they improve the model?

# Whether the respondent watches/reads:
# Hannity - V161370
# Rachel Maddow - V161393
# NYTimes - V161451
# Modern family - V161373
# Sunday night football - V161376
# house of cards - V161385
# game of thrones - V161389

# How often respondent attends church - V161245
# Is the respondent white, black, or latino (doesn't count other but doens't leave them out)

# Hannity - V161370
DF_Log$V161370 <- extractInt(DF_Log$V161370)

# Rachel Maddow - V161393
DF_Log$V161393 <- extractInt(DF_Log$V161393)

# NYTimes - V161451
DF_Log$V161451 <- extractInt(DF_Log$V161451)

# Modern family - V161373
DF_Log$V161373 <- extractInt(DF_Log$V161373)

# Sunday night football - V161376
DF_Log$V161376 <- extractInt(DF_Log$V161376)

# house of cards - V161385
DF_Log$V161385 <- extractInt(DF_Log$V161385)

# game of thrones - V161389
DF_Log$V161389 <- extractInt(DF_Log$V161389)

# How often respondent attends church - V161245
DF_Log$V161245 <- extractInt(DF_Log$V161245)

# Is respondent latino
DF_Log$V161309 <- extractInt(DF_Log$V161309)

# Is respondent white
DF_Log$V161310a <- extractInt(DF_Log$V161310a)

# Is respondent black
DF_Log$V161310b <- extractInt(DF_Log$V161310b)

new_fit <- glm(V162034a~V161310b+V161310a+V161309+V161245+V161389+V161385+V161376+V161373+V161451+V161393+V161370+V161235+V161195+V161126+V161158x+V161176+V161196+V161215+V161221+V161231+V161243+V161008,data=DF_Log,family=binomial())  # making the model
summary(new_fit)

percent.right(new_fit, DF_Log, DF_Log$V162034a)  # new
percent.right(fit, DF_Log, DF_Log$V162034a)      # old

# Very modest improvement (based on percent right) - algorithm is minimizing the sum of the squares of the residuals, though, so this need not be the case
# The significance is in the P(>|z|) column with stars on the right indicating the significance level - determining whether the factor impacts the model (is the coefficient != 0)
# It seems from the summary analysis that the only race that mattered was being white (being black or latino were not salient - only not being white mattered)

# (POINT) ^^ unexpectedly not significant relationship (p = about .42 for black, p = about .45 for latino),
# might be due to the fact that the things that make these groups vote the way they do are picked up in other variables (like not being white)

# It also looks like the only media that added to the modelsignificantly were Modern Family and the NYTimes, both with negative coefficients
# Which is interesting: If you watch Modern Family, I can predict that you're more likely to not vote for Trump

# (POINT) ^^unexpectedly significant! (p < .05, though perhaps the built-in test is bad because the underlying data are perhaps not normal)


# FOR FUN, check the probability that YOU voted for Trump here:
predict_user(fit)




# STUDY #4
# Potential correlation between views on particular policies with other policies

# Given that you like government spending on healthcare, what do you think about defense spending?
# The survey includes at least four variables which respondents give on a 7 point scale
# Three are (1) defense spending, (2) healthcare (3) the environment, and (4) government services spending

# Clean up to only get people who responded to all three - they go liberal (low) to conservative (high), except for the question on services spending
# V161181 - defense spending - 1 (decrease) to 7 (increase)
# V161184 - private public insurance - 1 (government) to 7 (private)
# V161201 - environment/jobs tradeoff - 1 (high regulation) to 7 (low of regulation)
# V161178 - government spending on services - 1 (less) to 7 (more)
DF_Cor <- DF[(DF$V161181 >= 1 & DF$V161181 <= 7) & (DF$V161184 >= 1 & DF$V161184 <= 7) & (DF$V161201 >= 1 & DF$V161201 <= 7) & (DF$V161178 >= 1 & DF$V161178 <= 7),]

issue_matrix <- cbind("Defense" = DF_Cor$V161181, "Healthcare" = DF_Cor$V161184, "Environment" = DF_Cor$V161201, "Spending" = DF_Cor$V161178); head(issue_matrix)

# Here's the ideological distribution on the three issues before we start
barplot(table(issue_matrix[,1]), main = "Barplot of Defense Spending", xlab = "Decrease -> Increase", col = "red")
barplot(table(issue_matrix[,2]), main = "Barplot of Healthcare", xlab = "Government -> Private", col = "orange")
barplot(table(issue_matrix[,3]), main = "Barplot of Environment", xlab = "High Regulation -> Low Regulation", col = "green")
barplot(table(issue_matrix[,4]), main = "Barplot of Social Services Spending", xlab = "Less -> More", col = "blue")

# Appropriate use of covariance or correlation (POINT)
cor(issue_matrix)  # correlation matrix using Pearson's method

# Seems like most of the issues are positively correlated with each other -
# meaning, if you are liberal on one issue, you're probably liberal on another, too
# The correlations mostly hover in the .35-.55 range, which is moderate
# This is important to get because politicians can take issues are certain stances strategically and pick up more voters
# It also tells us about how the parties align based on how different issues are correlated

cor.test(issue_matrix[,1], issue_matrix[,3])  # t test --> p < 2.2e-16, significant
# Also, note the 95% confidence interval produced in the test: (.338, .401) - I believe this is another bonus point, if we haven't already gotten this
# the conflidence intervals are included in the test summary for all
cor.test(issue_matrix[,1], issue_matrix[,2])  # t test --> p < 2.2e-16, significant
cor.test(issue_matrix[,1], issue_matrix[,4])  # t test --> p < 2.2e-16, significant

cor.test(issue_matrix[,2], issue_matrix[,3])  # t test --> p < 2.2e-16, significant
cor.test(issue_matrix[,2], issue_matrix[,4])  # t test --> p < 2.2e-16, significant
cor.test(issue_matrix[,2], issue_matrix[,1])  # t test --> p < 2.2e-16, significant

cor.test(issue_matrix[,3], issue_matrix[,4])  # t test --> p < 2.2e-16, significant
cor.test(issue_matrix[,3], issue_matrix[,2])  # t test --> p < 2.2e-16, significant
cor.test(issue_matrix[,3], issue_matrix[,1])  # t test --> p < 2.2e-16, significant

cor.test(issue_matrix[,4], issue_matrix[,3])  # t test --> p < 2.2e-16, significant
cor.test(issue_matrix[,4], issue_matrix[,2])  # t test --> p < 2.2e-16, significant
cor.test(issue_matrix[,4], issue_matrix[,1])  # t test --> p < 2.2e-16, significant

# All the relationships are statistically significant.


# (POINT) A visual representation that we haven't seen in class - using jitter to show density of discrete data
# Jitter is used to show where points overlap the most (where most responses are)
plot(jitter(issue_matrix[,1], factor=1), jitter(issue_matrix[,4], factor=1), type="p", main = "Social Services vs. Defense", xlab = "Defense (Decrease -> Increase)", ylab="Social Services (Less -> More)", col = "dark red")
plot(jitter(issue_matrix[,2], factor=1), jitter(issue_matrix[,4], factor=1), type="p", main = "Social Services vs. Healthcare", xlab = "Healthcare (Government -> Private)", ylab="Social Services (Less -> More)", col = "dark orange")
plot(jitter(issue_matrix[,3], factor=1), jitter(issue_matrix[,4], factor=1), type="p", main = "Social Services vs. Environment", xlab = "Environment (High Regulation -> Low Regulation)", ylab="Social Services (Less -> More)", col = "dark green")
# And yeah, no mistake: they're all correlated (and it looks fairly linear) (note that the coding for social services spending is the opposite of the others)

# They are all significant! This is very odd, but we've double-checked with the survey's description of questions and variables, and it appears correct
# One plausible explanation is that people read the question wrong - it says 1 means "many fewer services" instead of just "fewer"
# Another explanation is that many Republican voters are old, and they receive social security and most of medicare, which they might have interpreted as "services spending"

# Note: The tests are for cor != 0, but it's still significant if we just test for cor < 0, which is a weaker standard anyway

# Also Note: we've tested for signifiance for all correlations using R's built in Pearson test
# It offers Spearman and Kendall tests as well, and the correlations are all significant and similar in those as well
# We maybe should have used Kendall or Spearman because the data do not necessarily come from a bivariate normal distribution, but, as I said, all three methods provide similar results - Pearson's is a little more in line with what this class has done

# A brief aside: correlations like this are used most and are most illustrative with more continuous data, but the conclusion here is still sound!

# END

