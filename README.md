# ML_stackOverflow_Detect
A project on helping stackOverflow auto detect post to upvote, this is also a class project of UC Berkeley IEOR 142, Fall2019


### Background
Stack Overflow is a very popular question-and-answer website, featuring questions on many different
computer programming topics. (The Stack Exchange Network also features many different
spin-off sites on a wide variety of topics.) Users of Stack Overflow can post questions and also
provide answers to questions. Users who have earned certain privileges can also vote on the quality/
usefulness/helpfulness of both questions and answers. For each question (or answer), privileged
users who have read the question can elect to provide either an “upvote” or a “downvote” for
the question. An upvote constitutes an endorsement of the question’s usefulness for other users,
whereas a downvote signals that the question is likely to be especially not useful (or has been
sloppily written, or does not show enough research effort, etc.). The “score” of a question is equal
to the total number of upvotes for that question minus the total number of downvotes.

### Data
In this problem, we will work with question data from Stack Overflow, made available as part of
the Stack Exchange Network “data dump”. https://archive.org/details/stackexchange

The particular dataset contains questions about the ggplot2 package in the R programming language, which were posted between
January 2016 and September 2017.2 The dataset is in the file ggplot2questions2016 17.csv. 

There are 7,468 observations, with each observation recording text data associated with both the title
and the body of the associated question and also the score of the question. Table 1 describes the
dataset in further detail.

Table 1: Description of the dataset ggplot2questions2016 17.csv.<br/>


Variable     |      Description
---------------------------------------------------------
Title        |   Title text of the question
---------------------------------------------------------
Body         |   Main body text of the question (in html format)
---------------------------------------------------------
Score        |   Score of the question, equal to total number of upvotes minus total number of downvotes
----------------------------------------------------------

