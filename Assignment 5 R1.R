library(quanteda)
library(stm)
library(tm)
library(NLP)
library(openNLP)
setwd("C:/Users/sunil/Google Drive/Sem 3/Business analytics/Assignments/Assignment 5")

precorpus<- read.csv("Corpus1.csv", 
                       header=TRUE, stringsAsFactors=FALSE)
names(precorpus)   
head(precorpus)
require(quanteda)
help(corpus)

companycorpus<- corpus(precorpus$Mission.statement.and.core.values,
                    docnames=precorpus$Company.name,
                    docvar=NULL)
names(companycorpus)   
summary(companycorpus)
head(companycorpus)

companycorpus<- toLower(companycorpus, keepAcronyms = FALSE) 
cleancorpus <- tokenize(companycorpus, 
                        removeNumbers=TRUE,  
                        removePunct = TRUE,
                        removeSeparators=TRUE,
                        removeTwitter=FALSE,
                        verbose=TRUE)

str(cleancorpus)


#stop words from  https://github.com/arc12/Text-Mining-Weak-Signals/wiki/Standard-set-of-english-stopwords
swlist = c("a, about, above, across, after, again, against, all, almost, alone, along, already, also, although, always, am, among, an, and, another, any, anybody, anyone, anything, anywhere, are, area, areas, aren't, around, as, ask, asked, asking, asks, at, away, b, back, backed, backing, backs, be, became, because, become, becomes, been, before, began, behind, being, beings, below, best, better, between, big, both, but, by, c, came, can, cannot, can't, case, cases, certain, certainly, clear, clearly, come, could, couldn't, d, did, didn't, differ, different, differently, do, does, doesn't, doing, done, don't, down, downed, downing, downs, during, e, each, early, either, end, ended, ending, ends, enough, even, evenly, ever, every, everybody, everyone, everything, everywhere, f, face, faces, fact, facts, far, felt, few, find, finds, first, for, four, from, full, fully, further, furthered, furthering, furthers, g, gave, general, generally, get, gets, give, given, gives, go, going, good, goods, got, great, greater, greatest, group, grouped, grouping, groups, h, had, hadn't, has, hasn't, have, haven't, having, he, he'd, he'll, her, here, here's, hers, herself, he's, high, higher, highest, him, himself, his, how, however, how's, i, i'd, if, i'll, i'm, important, in, interest, interested, interesting, interests, into, is, isn't, it, its, it's, itself, i've, j, just, k, keep, keeps, kind, knew, know, known, knows, l, large, largely, last, later, latest, least, less, let, lets, let's, like, likely, long, longer, longest, m, made, make, making, man, many, may, me, member, members, men, might, more, most, mostly, mr, mrs, much, must, mustn't, my, myself, n, necessary, need, needed, needing, needs, never, new, newer, newest, next, no, nobody, non, noone, nor, not, nothing, now, nowhere, number, numbers, o, of, off, often, old, older, oldest, on, once, one, only, open, opened, opening, opens, or, order, ordered, ordering, orders, other, others, ought, our, ours, ourselves, out, over, own, p, part, parted, parting, parts, per, perhaps, place, places, point, pointed, pointing, points, possible, present, presented, presenting, presents, problem, problems, put, puts, q, quite, r, rather, really, right, room, rooms, s, said, same, saw, say, says, second, seconds, see, seem, seemed, seeming, seems, sees, several, shall, shan't, she, she'd, she'll, she's, should, shouldn't, show, showed, showing, shows, side, sides, since, small, smaller, smallest, so, some, somebody, someone, something, somewhere, state, states, still, such, sure, t, take, taken, than, that, that's, the, their, theirs, them, themselves, then, there, therefore, there's, these, they, they'd, they'll, they're, they've, thing, things, think, thinks, this, those, though, thought, thoughts, three, through, thus, to, today, together, too, took, toward, turn, turned, turning, turns, two, u, under, until, up, upon, us, use, used, uses, v, very, w, want, wanted, wanting, wants, was, wasn't, way, ways, we, we'd, well, we'll, wells, went, were, we're, weren't, we've, what, what's, when, when's, where, where's, whether, which, while, who, whole, whom, who's, whose, why, why's, will, with, within, without, won't, work, worked, working, works, would, wouldn't, x, y, year, years, yes, yet, you, you'd, you'll, young, younger, youngest, your, you're, yours, yourself, yourselves, you've, z") # stop words list
cleancorpus <- tokenize(companycorpus, 
                        removeNumbers=TRUE,  
                        removePunct = TRUE,
                        removeSeparators=TRUE,
                        removeTwitter=FALSE )
dfm.stem<- dfm(cleancorpus, toLower = TRUE, 
               ignoredFeatures = c(swlist, stopwords("english")),
               verbose=TRUE, 
               stem=TRUE) 
topfeatures.stem<-topfeatures(dfm.stem, n=50)
topfeatures.stem


#########################
### WORD CLOUD ########
#########################


library(wordcloud)
set.seed(142)   #keeps cloud' shape fixed
dark2 <- brewer.pal(7, "Set1")   
freq<-topfeatures(dfm.stem, n=500)

wordcloud(names(freq), 
          freq, max.words=200, 
          scale=c(3, .1), 
          colors=brewer.pal(8, "Set1"))

#########################
### Find associations ###
#########################
dfm.tm<-convert(dfm.stem, to="tm")
names(dfm.tm)
findAssocs(dfm.tm, 
           c("data", "busi", "compani","custom","oracl","world","enterprise","make","big","solut","analyt"), 
           corlimit=0.7) 


##########################
### Topic Modeling########
##########################

library(stm)

#Process the data for analysis.
help("textProcessor")
temp<-textProcessor(documents=precorpus$Mission.statement.and.core.values, metadata = precorpus)
names(temp)  
meta<-temp$meta
vocab<-temp$vocab
docs<-temp$documents
out <- prepDocuments(docs, vocab, meta)
docs<-out$documents
vocab<-out$vocab
meta <-out$meta


help("stm")
prevfit <-stm(docs , vocab , 
              K=5, 
              verbose=TRUE,
              data=meta, 
              max.em.its=5)

topics <-labelTopics(prevfit , topics=c(1:5))
topics   #shows topics with highest probability words

#explore the topics in context.  Provides an example of the text 
help("findThoughts")
findThoughts(prevfit, texts = precorpus$Mission.statement.and.core.values,  topics = 5,  n = 2)


help(topicCorr)
mod.out.corr <- topicCorr(prevfit)
plot.topicCorr(mod.out.corr)



