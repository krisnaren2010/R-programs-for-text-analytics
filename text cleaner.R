text.cleaner = function(x,u) #text data
{
require("tm")
x = gsub("<.*?>", " ", x) #regex for removing html
x= iconv(x,"latin1", "ASCII",sub ="") #keep only ASCII characters
x= gsub("[^[:alnum:]]", " ", x) # removing everything other than alphanumeric
x= tolower(x) # convert all to lower case
x=stripWhitespace(x) # removes white spaces
x= gsub("^\\s+|\\s+$","",x) # removes trailing and leading white spaces
stpw1 = u
stpw2 = tm::stopwords('english')      # tm package stop word list; tokenizer package has the same name function, hence 'tm::'
comn  = unique(c(stpw1, stpw2))         # Union of two list
        stopwords = unique(gsub("'"," ",comn))  # final stop word lsit after removing punctuation
        
        x  =  removeWords(x,stopwords)            # removing stopwords created above
        x  =  stripWhitespace(x)
        x = data.frame(id = 1:length(x),text = x, 
                       stringsAsFactors = F)
        # removing white space
    
        #  x  =  stemDocument(x)                   # can stem doc if needed.
        
        return(x)
}

x <-"Hitler was born in Austria-then part of Austria-Hungary-and was raised near Linz. He moved to Germany in 1913 and was decorated during his service in the German Army in World War I. In 1919, he joined the German Workers' Party (DAP), the precursor of the NSDAP, and was appointed leader of the NSDAP in 1921. In 1923, he attempted to seize power in a failed coup in Munich and was imprisoned. While in jail he dictated the first volume of his autobiography and political manifesto Mein Kampf (\"My Struggle\"). Released in 1924, Hitler gained popular support by attacking the Treaty of Versailles and promoting Pan-Germanism, anti-semitism and anti-communism with charismatic oratory and Nazi propaganda. He frequently denounced international capitalism and communism as being part of a Jewish conspiracy.

By 1933, the Nazi Party was the largest elected party in the German Reichstag, but did not have a majority, and no party was able to form a majority parliamentary coalition in support of a candidate for chancellor. This led to former chancellor Franz von Papen and other conservative leaders persuading President Paul von Hindenburg to appoint Hitler as Chancellor on 30 January 1933. Shortly after, the Reichstag passed the Enabling Act of 1933, which began the process of transforming the Weimar Republic into Nazi Germany, a one-party dictatorship based on the totalitarian and autocratic ideology of National Socialism. Hitler aimed to eliminate Jews from Germany and establish a New Order to counter what he saw as the injustice of the post-World War I international order dominated by Britain and France. His first six years in power resulted in rapid economic recovery from the Great Depression, the abrogation of restrictions imposed on Germany after World War I and the annexation of territories that were home to millions of ethnic Germans which gave him significant popular support.

Hitler sought Lebensraum (\"living space\") for the German people in Eastern Europe and his aggressive foreign policy is considered to be the primary cause of the outbreak of World War II in Europe. He directed large-scale rearmament and on 1 September 1939 invaded Poland, resulting in Britain and France declaring war on Germany. In June 1941, Hitler ordered an invasion of the Soviet Union. By the end of 1941, German forces and the European Axis powers occupied most of Europe and North Africa. In December 1941, he formally declared war on the United States, bringing them directly into the conflict. Failure to defeat the Soviets and the entry of the United States into the war forced Germany onto the defensive and it suffered a series of escalating defeats. In the final days of the war during the Battle of Berlin in 1945, he married his long-time lover Eva Braun. Less than two days later on 30 April 1945, the two killed themselves to avoid capture by the Soviet Red Army and their corpses were burned.

Under Hitler's leadership and racially motivated ideology, the Nazi regime was responsible for the genocide of at least 5.5 million Jews and millions of other victims whom he and his followers deemed Untermenschen (sub-humans) or socially undesirable. Hitler and the Nazi regime were also responsible for the killing of an estimated 19.3 million civilians and prisoners of war. In addition, 29 million soldiers and civilians died as a result of military action in the European theatre. The number of civilians killed during the Second World War was unprecedented in warfare and the casualties constituted the deadliest conflict in human history."
u <- c("adolf","hitler","germany")
x <- text.cleaner(x,u)

