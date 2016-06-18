;;; zone-words-emotions-dictionary.el --- zone-words dictionary.

;;; Commentary:
;; zone-words dictionary.


;;; Code:

;; From http://www.psychpage.com/learning/library/assess/feelings.html
(defvar zone-words-emotions-dictionary--unpleasant-feelings '("irritated" "lousy" "upset" "incapable" "enraged" "disappointed" "doubtful" "alone" "hostile" "discouraged" "uncertain" "paralyzed" "insulting" "ashamed" "indecisive" "fatigued" "sore" "powerless" "perplexed" "useless" "annoyed" "diminished" "embarrassed" "inferior" "upset" "guilty" "hesitant" "vulnerable" "hateful" "dissatisfied" "shy" "empty" "unpleasant" "miserable" "stupefied" "forced" "offensive" "detestable" "disillusioned" "hesitant" "bitter" "repugnant" "unbelieving" "despair" "aggressive" "despicable" "skeptical" "frustrated" "resentful" "disgusting" "distrustful" "distressed" "inflamed" "abominable" "misgiving" "woeful" "provoked" "terrible" "lost" "pathetic" "incensed" "in despair" "unsure" "tragic" "infuriated" "sulky" "uneasy" "cross" "bad" "pessimistic" "dominated" "worked up" "tense" "boiling" "fuming" "indignant" "insensitive" "fearful" "crushed" "tearful" "dull" "terrified" "tormented" "sorrowful" "nonchalant" "suspicious" "deprived" "pained" "neutral" "anxious" "pained" "grief" "reserved" "alarmed" "tortured" "anguish" "weary" "panic" "dejected" "desolate" "bored" "nervous" "rejected" "desperate" "preoccupied" "scared" "injured" "pessimistic" "cold" "worried" "offended" "unhappy" "disinterested" "frightened" "afflicted" "lonely" "lifeless" "timid" "aching" "grieved" "shaky" "victimized" "mournful" "restless" "heartbroken" "dismayed" "doubtful" "agonized" "threatened" "appalled" "cowardly" "humiliated" "quaking" "wronged" "menaced" "alienated" "wary"))

;; From http://www.psychpage.com/learning/library/assess/feelings.html
(defvar zone-words-emotions-dictionary--pleasant-feelings '("understanding" "great" "playful" "calm" "confident" "gay" "courageous" "peaceful" "reliable" "joyous" "energetic" "easy" "lucky" "liberated" "comfortable" "amazed" "fortunate" "optimistic" "pleased" "free" "delighted" "provocative" "encouraged" "sympathetic" "overjoyed" "impulsive" "clever" "interested" "gleeful" "free" "surprised" "satisfied" "thankful" "frisky" "content" "receptive" "important" "animated" "quiet" "accepting" "festive" "spirited" "certain" "kind" "ecstatic" "thrilled" "relaxed" "satisfied" "wonderful" "serene" "glad" "cheerful" "bright" "sunny" "blessed" "merry" "reassured" "elated" "jubilant" "loving" "concerned" "eager" "impulsive" "considerate" "affected" "keen" "free" "affectionate" "fascinated" "earnest" "sure" "sensitive" "intrigued" "intent" "certain" "tender" "absorbed" "anxious" "rebellious" "devoted" "inquisitive" "inspired" "unique" "attracted" "nosy" "determined" "dynamic" "passionate" "snoopy" "excited" "tenacious" "admiration" "engrossed" "enthusiastic" "hardy" "warm" "curious" "bold" "secure" "touched" "brave" "sympathy" "daring" "close" "challenged" "loved" "optimistic" "comforted" "re-enforced" "confident" "hopeful"))

(defun zone-words-emotions-dictionary-lookup-emotion ()
  (zone-words-emotions-dictionary--random-item (zone-words-emotions-dictionary--random-item
                                                (list
                                                 zone-words-emotions-dictionary--pleasant-feelings
                                                 zone-words-emotions-dictionary--unpleasant-feelings))))

(defun zone-words-emotions-dictionary--random-item (items)
  (nth (random (length items))
       items))

(provide 'zone-words-emotions-dictionary)

;;; zone-words-emotions-dictionary.el ends here
