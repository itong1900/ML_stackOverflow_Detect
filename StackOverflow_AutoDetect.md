    library(tm)
    library(MASS)
    library(rpart)
    library(boot)
    library(SnowballC)
    library(wordcloud)
    library(caTools)
    library(dplyr)
    library(rpart.plot)
    library(randomForest)
    library(caret)

    ## load the data, and a quick view of data, the data is consisted of Title, Body, score, we'll mainly dig in to Title and Body and infer score
    data = read.csv("ggplot2questions2016_17.csv", sep = ",", stringsAsFactors = FALSE)
    clean_data = data
    head(data,2)

    ##                                       Title
    ## 1                 Missing Ribbon in ggplot2
    ## 2 ggplot - label as calculated increase (%)
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             Body
    ## 1                                                                                                                                                 <p>I seem to be having trouble setting up a ribbon in ggplot2 to display. </p>\n\n<p>Here's a made up data set:</p>\n\n<pre><code>&gt; GlobalDFData\n  Estimate Upper  Lower  Date   Area\n1      100   125    75 Q1_16 Global\n2      125   150   100 Q2_16 Global\n3      150   175   125 Q3_16 Global\n4      175   200   150 Q4_16 Global\n</code></pre>\n\n<p>Here's the code that I'm trying with no success. I get the line chart but not the upper and lower bounds</p>\n\n<pre><code>ggplot(GlobalDFData, aes(x = Date)) + \n  geom_line(aes(y = Estimate, group = Area, color = Area))+\n  geom_point(aes(y = Estimate, x = Date))+\n  geom_ribbon(aes(ymin = Lower, ymax = Upper))\n</code></pre>\n
    ## 2 <p>I have developed a ggplot graph but now I am trying to add calculated label which shows increase in % year-on-year?</p>\n\n<p>My data frame is very simple (result of aggregate from the main dataset)</p>\n\n<pre><code>'data.frame':   4 obs. of  3 variables:\n $ Year    : int  2011 2012 2013 2014\n $ TotalPay: num  71744 74113 77611 75466\n</code></pre>\n\n<p>I have a code for my graph:</p>\n\n<pre><code>library(ggplot2)\nggplot(d1, aes(x=Year, y=TotalPay)) + geom_bar(stat="identity") + \nlabs(x="Year", y="Average Total Pay ($)")\n</code></pre>\n\n<p>and now trying to use stat_bin for lables? The calculation is Actual Year - Previous Year * 100%. I have this but not sure how to fill percent ()</p>\n\n<pre><code>stat_bin(aes (labels = paste("Total Pay" = ,scales::percent(())), vjust = 1, geom = "TexT")\n</code></pre>\n
    ##   Score
    ## 1     3
    ## 2     3

========= \#\# Data Cleaning ========== Let’s start with Data Cleaning

1.  remove
    <html>
    tag, as it’s not meaningful in prediction and add unecessary noise
    <br/>
2.  remove $\\n \\t$ <br/>
3.  all text convert to lower case <br/>
4.  remove puntunctuation <br/>
5.  remove stopwords(more details will be given at that step) <br/>
6.  stem the documents <br/>

<!-- -->

    ## doing i, ii
    clean_data$Body = gsub("<.*?>", "",clean_data$Body)
    clean_data$Body = gsub("[\n\t]", "", clean_data$Body)

    ## before going to next steps, convert all body to corpus for easier analysis
    corpusBody = Corpus(VectorSource(clean_data$Body))#create corpus

    ## step iii to lower
    corpusBody = tm_map(corpusBody, tolower)#to lower case
    ## step iv remove punctuation
    corpusBody = tm_map(corpusBody, removePunctuation)#remove punctuation

From Wikipedia, Stop words are defined as "“In computing, stop words are
words which are filtered out before or after processing of natural
language data.” In our case, we’ll remove top 10 stop words in English,
as well as “ggplot” and “ggplot2” since they are over common in this
topic

    ## step v 
    stopwords("english")[1:10] # find top stopwords

    ##  [1] "i"         "me"        "my"        "myself"    "we"        "our"      
    ##  [7] "ours"      "ourselves" "you"       "your"

    length(stopwords("english"))

    ## [1] 174

    corpusBody = tm_map(corpusBody, removeWords, c("ggplot2","ggplot", stopwords("english")))

Also from Wikipedia, word steming is defined as “In linguistics, a stem
is a part of a word used with slightly different meanings and would
depend on the morphology of the language in question. In Athabaskan
linguistics, for example, a verb stem is a root that cannot appear on
its own, and that carries the tone of the word”. We’ll apply the steming
to the corpusBody as our last data cleaning process.

    ## step vi stem the documents
    corpusBody = tm_map(corpusBody, stemDocument)

    ## Finally, as an example, let's look at the how the body of our first row are finally converted 
    strwrap(corpusBody[[1]])

    ## [1] "seem troubl set ribbon display here made data setgt globaldfdata estim" 
    ## [2] "upper lower date area1 100 125 75 q116 global2 125 150 100 q216 global3"
    ## [3] "150 175 125 q316 global4 175 200 150 q416 globalher code im tri success"
    ## [4] "get line chart upper lower boundsggplotglobaldfdata aesx date"          
    ## [5] "geomlineaesi estim group area color area geompointaesi estim x date"    
    ## [6] "geomribbonaesymin lower ymax upper"
