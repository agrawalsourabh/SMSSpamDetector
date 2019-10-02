# Bar Graph to count total number of span or not spam
ggplot(data = our.data, mapping = aes(x = as.factor(label), fill=as.factor(label))) +
  geom_bar() +
  ggtitle("Spam Vs Not Spam") +
  xlab("label")+ 
  ylab("Count") +
  geom_label(stat = 'count', aes(label = ..count..)) +
  scale_fill_discrete(name = "label", labels = c("Not Spam", "Spam"))
ggsave("plot/SpamVsNotSpam.png")

# Building word cloud
wordcloud(words = wf$words, freq = wf$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors = brewer.pal(8, "Dark2"))
