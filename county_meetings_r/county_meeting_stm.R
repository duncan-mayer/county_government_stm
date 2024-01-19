## initial stm 
library('stm')
library("tidyverse")

dfc <- readRDS("county_meetings_stm_dat/df_text_gov_meetings.rds")


processed <- textProcessor(dfc$text_clean, metadata=dfc, ucp = TRUE, removenumbers = TRUE)

# to remove documents dropped in processing 
dfc <-dfc[-processed$docs.removed,]

length(dfc$channeltype) # number of meetings 
sum(dfc1$vid_length_min) / 60 # number of hours 
length(unique(dfc1$fips)) # number of counties in which meetings occur

##
lthresh <- round(dim(dfc)[1]*.01,0)

### pre proccess and create corpus 
out <- prepDocuments(processed$documents, processed$vocab, processed$meta,
                     lower.thresh = lthresh)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

# density is highly correlated in the sample
cor(meta[,c("AR","HU","HE","OT","ED","fips_density")]) # unadjusted for population - density is highly correlated across sectors 

# takes 2.5 hours all models converged
time1 <- Sys.time()
stm_storage <- data_frame(K = c(25, 50, 75, 100, 125)) %>%
  mutate(topic_model = map(K, ~stm(documents = out$documents,
                                          vocab = out$vocab, 
                                          prevalence = ~year + 
                                            HU + AR + HE + OT + ED + 
                                            pctpeoplelivingbelow150pctfederalpovertylevel,
                                          K = .,
                                          verbose = TRUE, 
                                          data= out$meta,
                                          max.em.its = 100, 
                                          init.type =  "Spectral")))

time2 <- Sys.time()
(runtime <-   time2 - time1)

# evaluate
heldout <- make.heldout(out$documents, out$vocab)

k_result <- stm_storage |> 
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, out$documents),
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals,  out$documents),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

g1 <- k_result |> 
  transmute(K,
            `Lower bound` = lbound,
            Residuals = map_dbl(residual, "dispersion"),
            `Semantic coherence` = map_dbl(semantic_coherence, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) |> 
  gather(Metric, Value, -K) |> 
  ggplot(aes(K, Value)) +
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "K",
       y = NULL,
       title = "Model diagnostics") + theme_bw()



##
tm25 <- stm(documents = out$documents,
            vocab = out$vocab, 
            prevalence = ~year + 
              HU + AR + HE + OT + ED + 
              pctpeoplelivingbelow150pctfederalpovertylevel,
            K = 25,
            verbose = TRUE, 
            data= out$meta,
            max.em.its = 75, 
            init.type =  "Spectral")

# converges in ~15 mins
tm50 <- stm(documents = out$documents,
             vocab = out$vocab, 
             prevalence = ~year + log(population) +
               HU + AR + HE + OT + ED + 
               pctpeoplelivingbelow150pctfederalpovertylevel,
             K = 50,
             verbose = TRUE, 
             data= out$meta,
             max.em.its = 75, 
             init.type =  "Spectral")

#####
## search K - DON'T RUN 
#####
# Kcandidate <-c(5,25,50,75,100, 125, 150, 175)
# 
# kresult <- searchK(docs, vocab, Kcandidate, prevalence= ~ year + 
#                      HU + AR + HE + OT + ED + 
#                      pctpeoplelivingbelow150pctfederalpovertylevel, data=meta, cores = 5)
# 
# 
# plot(kresult)

###  estimate effects 

e50 <- estimateEffect(c(39,38,35,33,31,29,27,22,21,20,19,14,6,7,4,1) ~year + log(population) +
                  HU + AR + HE + OT + ED + 
                  pctpeoplelivingbelow150pctfederalpovertylevel, tm50, meta = meta, uncertainty = "Global")
e25 <- estimateEffect( ~year + log(population) +
                        HU + AR + HE + OT + ED + 
                        pctpeoplelivingbelow150pctfederalpovertylevel, tm25, meta = meta, uncertainty = "Global")
