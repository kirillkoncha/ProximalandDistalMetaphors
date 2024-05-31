# Processing of Different Types of Metaphors for Polysemous Words in Russian
Code, data, and stimuli for the article [Koncha, K., Orlov, A., Lopukhina, A., & Apresjan, V. (2022). Processing of different types of metaphors for polysemous words in Russian. The Russian Journal of Cognitive Science, 9(3–4), 41–61. https://doi.org/10.47010/22.3-4.3](https://www.cogjournal.ru/eng/9/4/KonchaetalRJCS2022.html)

## Abstract

Theories of storing the multiple senses of polysemous words in the mental lexicon suggest three key approaches. Some theories assume that only literal senses are stored, while non-literal senses are derived from them via rules; other theories suggest that all senses are stored separately. There is also a hybrid approach which assumes that some non-literal senses can be stored as separate units, while others are derived from literal ones. However, the evidence in favor of hybrid models has been obtained mainly from data comparing either metonymy and metaphor, or else proximal and distal metonymy. In this paper, the predictions of hybrid models are tested on proximal and distal metaphors. With the help of an experiment involving a group of adult native speakers of Russian, we demonstrate a difference in sensicality judgment and processing speed between proximal and distal metaphors, as compared to the literal senses of verbs and adjectives. Adjectival distal metaphors are treated almost in the same way as literal senses. Proximal metaphors are processed more slowly and less accurately. For verbs, both proximal and distal metaphors are processed slower than literal senses and are more likely to be evaluated as nonsensical. Such results suggest that verbal metaphors and adjectival proximal metaphors are derived via a reference to the word’s literal sense during processing. Adjectival distal metaphors, in turn, are processed independently. The difference between adjectives and verbs in the processing of metaphors can be associated with the different nature of the mechanisms involved in the development of polysemy.

# Files

* `participants.csv` — anonymized participant data
* `results.csv` — experiment results (stimuli phrase, reaction time of each participant, etc.)
* freq_for_data.csv - frequencies for phrases with adjectives (all frequencies above 500 are high-frequency phrases; below 500 are low-frequency), for verbs, information about high/low frequency is encrypted in the Type column (see `results.csv`)
* `adjectives_analysis.R` - analysis of adjective data (recommended to run in RStudio)
* `verbs_analysis.R` - analysis of verb data (recommended to run in RStudio)
* `tables` - tables with stimuli and analysis output

