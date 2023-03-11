(defvar my-lw-urls-analyzed nil)
(defvar my-lw-urls-to-visit-extra nil)
(defvar my-lw-urls-to-visit
  (-uniq
   '(;; All Rationality: AI to Zombies chapters, in order.
     "https://www.greaterwrong.com/posts/RcZCwxFiZzE6X7nsv/what-do-we-mean-by-rationality"
     "https://www.greaterwrong.com/posts/SqF8cHjJv43mvJJzx/feeling-rational"
     "https://www.greaterwrong.com/posts/YshRbqZHYFoEMqFAu/why-truth-and"
     "https://www.greaterwrong.com/posts/jnZbHi873v9vcpGpZ/what-s-a-bias-again"
     "https://www.greaterwrong.com/posts/R8cpqD3NA4rZxRdQ4/availability"
     "https://www.greaterwrong.com/posts/Yq6aA4M3JKWaQepPJ/burdensome-details"
     "https://www.greaterwrong.com/posts/CPm5LTwHrvBJCa9h5/planning-fallacy"
     "https://www.greaterwrong.com/posts/sSqoEw9eRP2kPKLCz/illusion-of-transparency-why-no-one-understands-you"
     "https://www.greaterwrong.com/posts/HLqWn5LASfhhArZ7w/expecting-short-inferential-distances"
     "https://www.greaterwrong.com/posts/46qnWRSR7L2eyNbMA/the-lens-that-sees-its-flaws"
     "https://www.greaterwrong.com/posts/a7n8GdKiAZRX86T5A/making-beliefs-pay-rent-in-anticipated-experiences"
     "https://www.greaterwrong.com/posts/6hfGNLf4Hg5DXqJCF/a-fable-of-science-and-politics"
     "https://www.greaterwrong.com/posts/CqyJzDZWvGhhFJ7dY/belief-in-belief"
     "https://www.greaterwrong.com/posts/NKaPFf98Y5otMbsPk/bayesian-judo"
     "https://www.greaterwrong.com/posts/jeyvzALDbjdjjv5RW/pretending-to-be-wise"
     "https://www.greaterwrong.com/posts/fAuWLS7RKWD2npBFR/religion-s-claim-to-be-non-disprovable"
     "https://www.greaterwrong.com/posts/RmCjazjupRGcHSm5N/professing-and-cheering"
     "https://www.greaterwrong.com/posts/nYkMLFpx77Rz3uo9c/belief-as-attire"
     "https://www.greaterwrong.com/posts/dLbkrPu5STNCBLRjr/applause-lights"
     "https://www.greaterwrong.com/posts/GJ4ZQm7crTzTM6xDW/focus-your-uncertainty"
     "https://www.greaterwrong.com/posts/6s3xABaXKPdFwA3FS/what-is-evidence"
     "https://www.greaterwrong.com/posts/fhojYBGGiYAFcryHZ/scientific-evidence-legal-evidence-rational-evidence"
     "https://www.greaterwrong.com/posts/nj8JKFoLSMEmD3RGp/how-much-evidence-does-it-take"
     "https://www.greaterwrong.com/posts/MwQRucYo6BZZwjKE7/einstein-s-arrogance"
     "https://www.greaterwrong.com/posts/f4txACqDWithRi7hs/occam-s-razor"
     "https://www.greaterwrong.com/posts/5JDkW4MYXit2CquLs/your-strength-as-a-rationalist"
     "https://www.greaterwrong.com/posts/mnS2WYLCGJP2kQkRn/absence-of-evidence-is-evidence-of-absence"
     "https://www.greaterwrong.com/posts/jiBFC7DcCrZjGmZnJ/conservation-of-expected-evidence"
     "https://www.greaterwrong.com/posts/WnheMGAka4fL99eae/hindsight-devalues-science"
     "https://www.greaterwrong.com/posts/fysgqk4CjAwhBgNYT/fake-explanations"
     "https://www.greaterwrong.com/posts/NMoLJuDJEms7Ku9XS/guessing-the-teacher-s-password"
     "https://www.greaterwrong.com/posts/4Bwr6s9dofvqPWakn/science-as-attire"
     "https://www.greaterwrong.com/posts/RgkqLqkg8vLhsYpfh/fake-causality"
     "https://www.greaterwrong.com/posts/FWMfQKG3RpZx6irjm/semantic-stopsigns"
     "https://www.greaterwrong.com/posts/6i3zToomS86oj9bS6/mysterious-answers-to-mysterious-questions"
     "https://www.greaterwrong.com/posts/8QzZKw9WHRxjR4948/the-futility-of-emergence"
     "https://www.greaterwrong.com/posts/kpRSCH7ALLcb6ucWM/say-not-complexity"
     "https://www.greaterwrong.com/posts/rmAbiEKQDpDnZzcRf/positive-bias-look-into-the-dark"
     "https://www.greaterwrong.com/posts/msJA6B9ZjiiZxT6EZ/lawful-uncertainty"
     "https://www.greaterwrong.com/posts/DwtYPRuCxpXTrzG9m/my-wild-and-reckless-youth"
     "https://www.greaterwrong.com/posts/97Y7Jwrzxyfzz3Ad2/failing-to-learn-from-history"
     "https://www.greaterwrong.com/posts/TLKPj4GDXetZuPDH5/making-history-available"
     "https://www.greaterwrong.com/posts/yxvi9RitzZDpqn6Yh/explain-worship-ignore"
     "https://www.greaterwrong.com/posts/L22jhyY9ocXQNLqyE/science-as-curiosity-stopper"
     "https://www.greaterwrong.com/posts/fg9fXrHpeaDD6pEPL/truly-part-of-you"
     "https://www.greaterwrong.com/posts/GrDqnMjhqoxiqpQPw/the-proper-use-of-humility"
     "https://www.greaterwrong.com/posts/erGipespbbzdG5zYb/the-third-alternative"
     "https://www.greaterwrong.com/posts/vYsuM8cpuRgZS5rYB/lotteries-a-waste-of-hope"
     "https://www.greaterwrong.com/posts/QawvGzYWhqdyPWgBL/new-improved-lottery"
     "https://www.greaterwrong.com/posts/q7Me34xvSG3Wm97As/but-there-s-still-a-chance-right"
     "https://www.greaterwrong.com/posts/dLJv2CoRCgeC2mPgj/the-fallacy-of-gray"
     "https://www.greaterwrong.com/posts/PmQkensvTGg7nGtJE/absolute-authority"
     "https://www.greaterwrong.com/posts/6FmqiAgS8h4EJm86s/how-to-convince-me-that-2-2-3"
     "https://www.greaterwrong.com/posts/ooypcn7qFzsMcy53R/infinite-certainty"
     "https://www.greaterwrong.com/posts/QGkYCwyC7wTDyt3yT/0-and-1-are-not-probabilities"
     "https://www.greaterwrong.com/posts/anCubLdggTWjnEvBS/your-rationality-is-my-business"
     "https://www.greaterwrong.com/posts/9weLK2AJ9JEt2Tt8f/politics-is-the-mind-killer"
     "https://www.greaterwrong.com/posts/PeSzc9JTBxhaYRp9b/policy-debates-should-not-appear-one-sided"
     "https://www.greaterwrong.com/posts/XYCEB9roxEBfgjfxs/the-scales-of-justice-the-notebook-of-rationality"
     "https://www.greaterwrong.com/posts/DB6wbyrMugYMK5o6a/correspondence-bias"
     "https://www.greaterwrong.com/posts/28bAMAxhoX3bwbAKC/are-your-enemies-innately-evil"
     "https://www.greaterwrong.com/posts/qNZM3EGoE5ZeMdCRt/reversed-stupidity-is-not-intelligence"
     "https://www.greaterwrong.com/posts/5yFRd3cjLpm3Nd6Di/argument-screens-off-authority"
     "https://www.greaterwrong.com/posts/2jp98zdLo898qExrr/hug-the-query"
     "https://www.greaterwrong.com/posts/Lz64L3yJEtYGkzMzu/rationality-and-the-english-language"
     "https://www.greaterwrong.com/posts/i8q4vXestDkGTFwsc/human-evil-and-muddled-thinking"
     "https://www.greaterwrong.com/posts/AdYdLP2sRqPMoe8fb/knowing-about-biases-can-hurt-people"
     "https://www.greaterwrong.com/posts/627DZcvme7nLDrbZu/update-yourself-incrementally"
     "https://www.greaterwrong.com/posts/WN73eiLQkuDtSC8Ag/one-argument-against-an-army"
     "https://www.greaterwrong.com/posts/34XxbRFe54FycoCDw/the-bottom-line"
     "https://www.greaterwrong.com/posts/kJiPnaQPiy4p9Eqki/what-evidence-filtered-evidence"
     "https://www.greaterwrong.com/posts/SFZoEBpLo9frSJGkc/rationalization"
     "https://www.greaterwrong.com/posts/9f5EXt8KNNxTAihtZ/a-rational-argument"
     "https://www.greaterwrong.com/posts/dHQkDNMhj692ayx78/avoiding-your-belief-s-real-weak-points"
     "https://www.greaterwrong.com/posts/L32LHWzy9FzSDazEg/motivated-stopping-and-motivated-continuation"
     "https://www.greaterwrong.com/posts/bfbiyTogEKWEGP96S/fake-justification"
     "https://www.greaterwrong.com/posts/TGux5Fhcd7GmTfNGC/is-that-your-true-rejection"
     "https://www.greaterwrong.com/posts/wyyfFfaRar2jEdeQK/entangled-truths-contagious-lies"
     "https://www.greaterwrong.com/posts/E7CKXxtGKPmdM9ZRc/of-lies-and-black-swan-blowups"
     "https://www.greaterwrong.com/posts/XTWkjCJScy2GFAgDt/dark-side-epistemology"
     "https://www.greaterwrong.com/posts/CahCppKy9HuXe3j2i/singlethink"
     "https://www.greaterwrong.com/posts/Hs3ymqypvhgFMkgLb/doublethink-choosing-to-be-biased"
     "https://www.greaterwrong.com/posts/Hs3ymqypvhgFMkgLb/doublethink-choosing-to-be-biased"
     "https://www.greaterwrong.com/posts/rZX4WuufAPbN6wQTv/no-really-i-ve-deceived-myself"
     "https://www.greaterwrong.com/posts/wP2ymm44kZZwaFPYh/belief-in-self-deception"
     "https://www.greaterwrong.com/posts/ERRk4thxxYNcScqR4/moore-s-paradox"
     "https://www.greaterwrong.com/posts/W7LcN9gmdnaAk9K52/don-t-believe-you-ll-self-deceive"
     "https://www.greaterwrong.com/posts/bMkCEZoBNhgRBtzoj/anchoring-and-adjustment"
     "https://www.greaterwrong.com/posts/BaCWFCxBQYjJXSsah/priming-and-contamination"
     "https://www.greaterwrong.com/posts/ZmQv4DFx6y4jFbhLy/never-leave-your-room"
     "https://www.greaterwrong.com/posts/BHYBdijDcAKQ6e45Z/cached-selves"
     "https://www.greaterwrong.com/posts/TiDGXt3WrQwtCdDj3/do-we-believe-everything-we-re-told"
     "https://www.greaterwrong.com/posts/2MD3NMLBPCqPfnfre/cached-thoughts"
     "https://www.greaterwrong.com/posts/qu95AwSrKqQSo4fCY/the-outside-the-box-box"
     "https://www.greaterwrong.com/posts/2MD3NMLBPCqPfnfre/cached-thoughts"
     "https://www.greaterwrong.com/posts/SA79JMXKWke32A3hG/original-seeing"
     "https://www.greaterwrong.com/posts/h3vdnR34ZvohDEFT5/stranger-than-history"
     "https://www.greaterwrong.com/posts/rHBdcHGLJ7KvLJQPk/the-logical-fallacy-of-generalization-from-fictional"
     "https://www.greaterwrong.com/posts/yDfxTj9TKYsYiWH5o/the-virtue-of-narrowness"
     "https://www.greaterwrong.com/posts/aSQy7yHj6nPD44RNo/how-to-seem-and-be-deep"
     "https://www.greaterwrong.com/posts/buixYfcXBah9hbSNZ/we-change-our-minds-less-often-than-we-think"
     "https://www.greaterwrong.com/posts/uHYYA32CKgKT3FagE/hold-off-on-proposing-solutions"
     "https://www.greaterwrong.com/posts/KZLa74SzyKhSJ3M55/the-genetic-fallacy"
     "https://www.greaterwrong.com/posts/Kow8xRzpfkoY7pa69/the-affect-heuristic"
     "https://www.greaterwrong.com/posts/3T6p93Mut7G8qdkAs/evaluability-and-cheap-holiday-shopping"
     "https://www.greaterwrong.com/posts/5u5THLyRkTpPHiaG5/unbounded-scales-huge-jury-awards-and-futurism"
     "https://www.greaterwrong.com/posts/ACGeaAk6KButv2xwQ/the-halo-effect"
     "https://www.greaterwrong.com/posts/krMzmSXgvEdf7iBT6/superhero-bias"
     "https://www.greaterwrong.com/posts/Tv7WWhgbKMWzEnMmx/mere-messiahs"
     "https://www.greaterwrong.com/posts/XrzQW69HpidzvBxGr/affective-death-spirals"
     "https://www.greaterwrong.com/posts/hwi8JQjspnMWyWs4g/resist-the-happy-death-spiral"
     "https://www.greaterwrong.com/posts/NCefvet6X3Sd4wrPc/uncritical-supercriticality"
     "https://www.greaterwrong.com/posts/ZQG9cwKbct2LtmL3p/evaporative-cooling-of-group-beliefs"
     "https://www.greaterwrong.com/posts/Tw9cLvzSKrkGjNHW3/when-none-dare-urge-restraint"
     "https://www.greaterwrong.com/posts/MBpj3QKfPg9xKNeXW/the-robbers-cave-experiment"
     "https://www.greaterwrong.com/posts/yEjaj7PWacno5EvWa/every-cause-wants-to-be-a-cult"
     "https://www.greaterwrong.com/posts/etBrzxdfNop3DqJvA/guardians-of-the-truth"
     "https://www.greaterwrong.com/posts/aFtWRL3QihoF5uQd5/guardians-of-the-gene-pool"
     "https://www.greaterwrong.com/posts/96TBXaHwLbFyeAxrg/guardians-of-ayn-rand"
     "https://www.greaterwrong.com/posts/Qr4MB9hFRzamuMRHJ/two-cult-koans"
     "https://www.greaterwrong.com/posts/WHK94zXkQm7qm7wXk/asch-s-conformity-experiment"
     "https://www.greaterwrong.com/posts/ovvwAhKKoNbfcMz8K/on-expressing-your-concerns"
     "https://www.greaterwrong.com/posts/CEGnJBHmkcwPTysb7/lonely-dissent"
     "https://www.greaterwrong.com/posts/gBma88LH3CLQsqyfS/cultish-countercultishness"
     "https://www.greaterwrong.com/posts/wCqfCLs8z5Qw4GbKS/the-importance-of-saying-oops"
     "https://www.greaterwrong.com/posts/qRWfvgJG75ESLRNu9/the-crackpot-offer"
     "https://www.greaterwrong.com/posts/waqC6FihC2ryAZuAq/just-lose-hope-already"
     "https://www.greaterwrong.com/posts/43PTNr4ZMaezyAJ5o/the-proper-use-of-doubt"
     "https://www.greaterwrong.com/posts/HYWhKXRsMAyvRKRYz/you-can-face-reality"
     "https://www.greaterwrong.com/posts/3nZMgRTfFEfHp34Gb/the-meditation-on-curiosity"
     "https://www.greaterwrong.com/posts/eY45uCCX7DdwJ4Jha/no-one-can-exempt-you-from-rationality-s-laws"
     "https://www.greaterwrong.com/posts/3XgYbghWruBMrPTAL/leave-a-line-of-retreat"
     "https://www.greaterwrong.com/posts/BcYBfG8KomcpcxkEg/crisis-of-faith"
     "https://www.greaterwrong.com/posts/kXAb5riiaJNrfR8v8/the-ritual"
     "https://www.greaterwrong.com/posts/pLRogvJLPPg6Mrvg4/an-alien-god"
     "https://www.greaterwrong.com/posts/ZyNak8F6WXjuEbWWc/the-wonder-of-evolution"
     "https://www.greaterwrong.com/posts/jAToJHtg39AMTAuJo/evolutions-are-stupid-but-work-anyway"
     "https://www.greaterwrong.com/posts/XC7Kry5q6CD9TyG4K/no-evolutions-for-corporations-or-nanodevices"
     "https://www.greaterwrong.com/posts/gDNrpuwahdRrDJ9iY/evolving-to-extinction"
     "https://www.greaterwrong.com/posts/QsMJQSFj7WfoTMNgW/the-tragedy-of-group-selectionism"
     "https://www.greaterwrong.com/posts/i6fKszWY6gLZSX2Ey/fake-optimization-criteria"
     "https://www.greaterwrong.com/posts/XPErvb8m9FapXCjhA/adaptation-executers-not-fitness-maximizers"
     "https://www.greaterwrong.com/posts/epZLSoNvjW53tqNj9/evolutionary-psychology"
     "https://www.greaterwrong.com/posts/J4vdsSKB7LzAvaAMB/an-especially-elegant-evpsych-experiment"
     "https://www.greaterwrong.com/posts/Jq73GozjsuhdwMLEG/superstimuli-and-the-collapse-of-western-civilization"
     "https://www.greaterwrong.com/posts/cSXZpvqpa9vbGGLtG/thou-art-godshatter"
     "https://www.greaterwrong.com/posts/HktFCy6dgsqJ9WPpX/belief-in-intelligence"
     "https://www.greaterwrong.com/posts/Zkzzjg3h7hW5Z36hK/humans-in-funny-suits"
     "https://www.greaterwrong.com/posts/HFTn3bAT6uXSNwv4m/optimization-and-the-singularity"
     "https://www.greaterwrong.com/posts/cnYHFNBF3kZEyx24v/ghosts-in-the-machine"
     "https://www.greaterwrong.com/posts/YhgjmCxcQXixStWMC/artificial-addition"
     "https://www.greaterwrong.com/posts/n5ucT5ZbPdhfGNLtP/terminal-values-and-instrumental-values"
     "https://www.greaterwrong.com/posts/Tc2H9KbKRjuDJ3WSS/leaky-generalizations"
     "https://www.greaterwrong.com/posts/4ARaTpNX62uaL86j6/the-hidden-complexity-of-wishes"
     "https://www.greaterwrong.com/posts/RcZeZt8cPk48xxiQ8/anthropomorphic-optimism"
     "https://www.greaterwrong.com/posts/sP2Hg6uPwpfp3jZJN/lost-purposes"
     "https://www.greaterwrong.com/posts/hQxYBfu2LPc9Ydo6w/the-parable-of-the-dagger"
     "https://www.greaterwrong.com/posts/bcM5ft8jvsffsZZ4Y/the-parable-of-hemlock"
     "https://www.greaterwrong.com/posts/3nxs2WYDGzJbzcLMp/words-as-hidden-inferences"
     "https://www.greaterwrong.com/posts/HsznWM9A7NiuGsp28/extensions-and-intensions"
     "https://www.greaterwrong.com/posts/jMTbQj9XB5ah2maup/similarity-clusters"
     "https://www.greaterwrong.com/posts/4mEsPHqcbRWxnaE5b/typicality-and-asymmetrical-similarity"
     "https://www.greaterwrong.com/posts/WBw8dDkAWohFjWQSk/the-cluster-structure-of-thingspace"
     "https://www.greaterwrong.com/posts/4FcxgdvdQP45D6Skg/disguised-queries"
     "https://www.greaterwrong.com/posts/yFDKvfN6D87Tf5J9f/neural-categories"
     "https://www.greaterwrong.com/posts/yA4gF5KrboK2m2Xu7/how-an-algorithm-feels-from-inside"
     "https://www.greaterwrong.com/posts/7X2j8HAkWdmMoS8PE/disputing-definitions"
     "https://www.greaterwrong.com/posts/dMCFk2n2ur8n62hqB/feel-the-meaning"
     "https://www.greaterwrong.com/posts/9ZooAqfh2TC9SBDvq/the-argument-from-common-usage"
     "https://www.greaterwrong.com/posts/i2dfY65JciebF3CAo/empty-labels"
     "https://www.greaterwrong.com/posts/WBdvyyHLdxZSAMmoz/taboo-your-words"
     "https://www.greaterwrong.com/posts/GKfPL6LQFgB49FEnv/replace-the-symbol-with-the-substance"
     "https://www.greaterwrong.com/posts/y5MxoeacRKKM3KQth/fallacies-of-compression"
     "https://www.greaterwrong.com/posts/veN86cBhoe7mBxXLk/categorizing-has-consequences"
     "https://www.greaterwrong.com/posts/yuKaWPRTxZoov4z8K/sneaking-in-connotations"
     "https://www.greaterwrong.com/posts/cFzC996D7Jjds3vS9/arguing-by-definition"
     "https://www.greaterwrong.com/posts/d5NyJ2Lf6N22AD9PB/where-to-draw-the-boundary"
     "https://www.greaterwrong.com/posts/soQX8yXLbKy7cFvy8/entropy-and-short-codes"
     "https://www.greaterwrong.com/posts/yLcuygFfMfrfK8KjF/mutual-information-and-density-in-thingspace"
     "https://www.greaterwrong.com/posts/82eMd5KLiJ5Z6rTrr/superexponential-conceptspace-and-simple-words"
     "https://www.greaterwrong.com/posts/gDWvLicHhcMfGmwaK/conditional-independence-and-naive-bayes"
     "https://www.greaterwrong.com/posts/YF9HB6cWCJrDK5pBM/words-as-mental-paintbrush-handles"
     "https://www.greaterwrong.com/posts/shoMpaoZypfkXv84Y/variable-question-fallacies"
     "https://www.greaterwrong.com/posts/FaJaCgqBKphrDzDSj/37-ways-that-words-can-be-wrong"
     "https://www.greaterwrong.com/posts/LaM5aTcXvXzwQSC2Q/universal-fire"
     "https://www.greaterwrong.com/posts/7iTwGquBFZKttpEdE/universal-law"
     "https://www.greaterwrong.com/posts/oKiy7YwGToaYXdvnj/is-reality-ugly"
     "https://www.greaterwrong.com/posts/bkSkRwo9SRYxJMiSY/beautiful-probability"
     "https://www.greaterwrong.com/posts/N2pENnTPB75sfc9kb/outside-the-laboratory"
     "https://www.greaterwrong.com/posts/QkX2bAkwG2EpGvNug/the-second-law-of-thermodynamics-and-engines-of-cognition"
     "https://www.greaterwrong.com/posts/zFuCxbY9E2E8HTbfZ/perpetual-motion-beliefs"
     "https://www.greaterwrong.com/posts/QrhAeKBkm2WsdRYao/searching-for-bayes-structure"
     "https://www.greaterwrong.com/posts/Mc6QcrsbH5NRXbCRX/dissolving-the-question"
     "https://www.greaterwrong.com/posts/XzrqkhfwtiSDgKoAF/wrong-questions"
     "https://www.greaterwrong.com/posts/rQEwySCcLtdKHkrHp/righting-a-wrong-question"
     "https://www.greaterwrong.com/posts/ZTRiSNmeGQK8AkdN2/mind-projection-fallacy"
     "https://www.greaterwrong.com/posts/f6ZLxEWaankRZ2Crv/probability-is-in-the-mind"
     "https://www.greaterwrong.com/posts/np3tP49caG4uFLRbS/the-quotation-is-not-the-referent"
     "https://www.greaterwrong.com/posts/BwtBhqvTPGG2n2GuJ/qualitatively-confused"
     "https://www.greaterwrong.com/posts/tWLFWAndSZSYN6rPB/think-like-reality"
     "https://www.greaterwrong.com/posts/NyFtHycJvkyNjXNsP/chaotic-inversion"
     "https://www.greaterwrong.com/posts/tPqQdLCuxanjhoaNs/reductionism"
     "https://www.greaterwrong.com/posts/cphoF8naigLhRf3tu/explaining-vs-explaining-away"
     "https://www.greaterwrong.com/posts/mTf8MkpAigm3HP6x2/fake-reductionism"
     "https://www.greaterwrong.com/posts/kQzs8MFbBxdYhe3hK/savanna-poets"
     "https://www.greaterwrong.com/posts/x4dG4GhpZH2hgz59x/joy-in-the-merely-real"
     "https://www.greaterwrong.com/posts/KfMNFB3G7XNviHBPN/joy-in-discovery"
     "https://www.greaterwrong.com/posts/WjpA4PCjt5EkTGbLF/bind-yourself-to-reality"
     "https://www.greaterwrong.com/posts/iiWiHgtQekWNnmE6Q/if-you-demand-magic-magic-won-t-help"
     "https://www.greaterwrong.com/posts/SXK87NgEPszhWkvQm/mundane-magic"
     "https://www.greaterwrong.com/posts/ndGYn7ZFiZyernp9f/the-beauty-of-settled-science"
     "https://www.greaterwrong.com/posts/hYqDp4qAucZM33qSh/amazing-breakthrough-day-april-1st"
     "https://www.greaterwrong.com/posts/PMr6f7ZocEWFtCYXj/is-humanism-a-religion-substitute"
     "https://www.greaterwrong.com/posts/MCYp8g9EMAiTCTawk/scarcity"
     "https://www.greaterwrong.com/posts/Fwt4sDDacko8Sh5iR/the-sacred-mundane"
     "https://www.greaterwrong.com/posts/3diLhMELXxM8rFHJj/to-spread-science-keep-it-secret"
     "https://www.greaterwrong.com/posts/fnEWQAYxcRnaYBqaZ/initiation-ceremony"
     "https://www.greaterwrong.com/posts/KmghfjH6RgXvoKruJ/hand-vs-fingers"
     "https://www.greaterwrong.com/posts/ddwk9veF8efn3Nzbu/angry-atoms"
     "https://www.greaterwrong.com/posts/ne6Ra62FB9ACHGSuh/heat-vs-motion"
     "https://www.greaterwrong.com/posts/nzzNFcrSk7akQ9bwD/brain-breakthrough-it-s-made-of-neurons"
     "https://www.greaterwrong.com/posts/f4RJtHBPvDRJcCTva/when-anthropomorphism-became-stupid"
     "https://www.greaterwrong.com/posts/qmqLxvtsPzZ2s6mpY/a-priori"
     "https://www.greaterwrong.com/posts/gRa5cWWBsZqdFvmqu/reductive-reference"
     "https://www.greaterwrong.com/posts/fdEWWr8St59bXLbQr/zombies-zombies"
     "https://www.greaterwrong.com/posts/4moMTeCy9EqYxAher/zombie-responses"
     "https://www.greaterwrong.com/posts/kYAuNJX2ecH2uFqZ9/the-generalized-anti-zombie-principle"
     "https://www.greaterwrong.com/posts/k6EPphHiBH4WWYFCj/gazp-vs-glut"
     "https://www.greaterwrong.com/posts/3XMwPNMSbaPm2suGz/belief-in-the-implied-invisible"
     "https://www.greaterwrong.com/posts/fsDz6HieZJBu54Yes/zombies-the-movie"
     "https://www.greaterwrong.com/posts/u6JzcFtPGiznFgDxP/excluding-the-supernatural"
     "https://www.greaterwrong.com/posts/7Au7kvRAPREm3ADcK/psychic-powers"
     "https://www.greaterwrong.com/posts/7FSwbFpDsca7uXpQ2/quantum-explanations"
     "https://www.greaterwrong.com/posts/5vZD32EynD9n94dhr/configurations-and-amplitude"
     "https://www.greaterwrong.com/posts/ybusFwDqiZgQa6NCq/joint-configurations"
     "https://www.greaterwrong.com/posts/KbeHkLNY5ETJ3TN3W/distinct-configurations"
     "https://www.greaterwrong.com/posts/xsZnufn3cQw7tJeQ3/collapse-postulates"
     "https://www.greaterwrong.com/posts/Atu4teGvob5vKvEAF/decoherence-is-simple"
     "https://www.greaterwrong.com/posts/DFxoaWGEh9ndwtZhk/decoherence-is-falsifiable-and-testable"
     "https://www.greaterwrong.com/posts/X2AD2LgtKgkRNPj2a/privileging-the-hypothesis"
     "https://www.greaterwrong.com/posts/qcYCAxYZT4Xp9iMZY/living-in-many-worlds"
     "https://www.greaterwrong.com/posts/k3823vuarnmL5Pqin/quantum-non-realism"
     "https://www.greaterwrong.com/posts/WqGCaRhib42dhKWRL/if-many-worlds-had-come-first"
     "https://www.greaterwrong.com/posts/Bh9cdfMjATrTdLrGH/where-philosophy-meets-science"
     "https://www.greaterwrong.com/posts/NEeW7eSXThPz7o4Ne/thou-art-physics"
     "https://www.greaterwrong.com/posts/S8ysHqeRGuySPttrS/many-worlds-one-best-guess"
     "https://www.greaterwrong.com/posts/ZxR8P8hBFQ9kC8wMy/the-failures-of-eld-science"
     "https://www.greaterwrong.com/posts/viPPjojmChxLGPE2v/the-dilemma-science-or-bayes"
     "https://www.greaterwrong.com/posts/5bJyRMZzwMov5u3hW/science-doesn-t-trust-your-rationality"
     "https://www.greaterwrong.com/posts/wzxneh7wxkdNYNbtB/when-science-can-t-help"
     "https://www.greaterwrong.com/posts/PGfJdgemDJSwWBZSX/science-isn-t-strict-enough"
     "https://www.greaterwrong.com/posts/WijMw9WkcafmCFgj4/do-scientists-already-know-this-stuff"
     "https://www.greaterwrong.com/posts/wustx45CPL5rZenuo/no-safe-defense-not-even-science"
     "https://www.greaterwrong.com/posts/SoDsr8GEZmRKMZNkj/changing-the-definition-of-science"
     "https://www.greaterwrong.com/posts/xTyuQ3cgsPjifr7oj/faster-than-science"
     "https://www.greaterwrong.com/posts/mpaqTWGiLT7GA3w76/einstein-s-speed"
     "https://www.greaterwrong.com/posts/5wMcKNAwB6X4mp9og/that-alien-message"
     "https://www.greaterwrong.com/posts/3Jpchgy53D2gB5qdk/my-childhood-role-model"
     "https://www.greaterwrong.com/posts/5o4EZJyqmHY4XgRCY/einstein-s-superpowers"
     "https://www.greaterwrong.com/posts/xAXrEpF5FYjwqKMfZ/class-project"
     "https://www.greaterwrong.com/posts/synsRtBKDeAFuo7e3/not-for-the-sake-of-happiness-alone"
     "https://www.greaterwrong.com/posts/n5ucT5ZbPdhfGNLtP/terminal-values-and-instrumental-values"
     "https://www.greaterwrong.com/posts/Masoq4NdmmGSiq2xw/fake-selfishness"
     "https://www.greaterwrong.com/posts/fATPBv4pnHC33EmJ2/fake-morality"
     "https://www.greaterwrong.com/posts/NnohDYHNnKDtbiMyp/fake-utility-functions"
     "https://www.greaterwrong.com/posts/D6rsNhHM4pBCpDzSb/fake-fake-utility-functions"
     "https://www.greaterwrong.com/posts/4ARaTpNX62uaL86j6/the-hidden-complexity-of-wishes"
     "https://www.greaterwrong.com/posts/bfbiyTogEKWEGP96S/fake-justification"
     "https://www.greaterwrong.com/posts/i6fKszWY6gLZSX2Ey/fake-optimization-criteria"
     "https://www.greaterwrong.com/posts/zY4pic7cwQpa9dnyk/detached-lever-fallacy"
     "https://www.greaterwrong.com/posts/p7ftQ6acRkgo6hqHb/dreams-of-ai-design"
     "https://www.greaterwrong.com/posts/tnWRXkcDi5Tw9rzXw/the-design-space-of-minds-in-general"
     "https://www.greaterwrong.com/posts/C8nEXTcjZb9oauTCW/where-recursive-justification-hits-bottom"
     "https://www.greaterwrong.com/posts/TynBiYt6zg42StRbb/my-kind-of-reflection"
     "https://www.greaterwrong.com/posts/PtoQdG7E8MxYJrigu/no-universally-compelling-arguments"
     "https://www.greaterwrong.com/posts/CuSTqHgeK4CMpWYTe/created-already-in-motion"
     "https://www.greaterwrong.com/posts/mMBTPTjRbsrqbSkZE/sorting-pebbles-into-correct-heaps"
     "https://www.greaterwrong.com/posts/eDpPnT7wdBwWPGvo5/2-place-and-1-place-words"
     "https://www.greaterwrong.com/posts/iGH7FSrdoCXa5AHGs/what-would-you-do-without-morality"
     "https://www.greaterwrong.com/posts/LhP2zGBWR5AdssrdJ/changing-your-metaethics"
     "https://www.greaterwrong.com/posts/vy9nnPdwTjSmt5qdb/could-anything-be-right"
     "https://www.greaterwrong.com/posts/FnJPa8E9ZG5xiLLp5/morality-as-fixed-computation"
     "https://www.greaterwrong.com/posts/PoDAyQMWEXBBBEJ5P/magical-categories"
     "https://www.greaterwrong.com/posts/HFyWNBnDNEDsDNLrZ/the-true-prisoner-s-dilemma"
     "https://www.greaterwrong.com/posts/NLMo5FZWFFq652MNe/sympathetic-minds"
     "https://www.greaterwrong.com/posts/29vqqmGNxNRGzffEj/high-challenge"
     "https://www.greaterwrong.com/posts/6qS9q5zHafFXsB6hf/serious-stories"
     "https://www.greaterwrong.com/posts/GNnHHmm8EzePmKzPk/value-is-fragile"
     "https://www.greaterwrong.com/posts/pGvyqAQw6yqTjpKf4/the-gift-we-give-to-tomorrow"
     "https://www.greaterwrong.com/posts/2ftJ38y9SRBCBsCzy/scope-insensitivity"
     "https://www.greaterwrong.com/posts/xiHy3kFni8nsxfdcP/one-life-against-the-world"
     "https://www.greaterwrong.com/posts/zJZvoiwydJ5zvzTHK/the-allais-paradox"
     "https://www.greaterwrong.com/posts/zNcLnqHF5rvrTsQJx/zut-allais"
     "https://www.greaterwrong.com/posts/knpAQ4F3gmguxy39z/allais-malaise"
     "https://www.greaterwrong.com/posts/zNcLnqHF5rvrTsQJx/zut-allais"
     "https://www.greaterwrong.com/posts/r5MSQ83gtbjWRBDWJ/the-intuitions-behind-utilitarianism"
     "https://www.greaterwrong.com/posts/K9ZaZXDnL3SEmYZqB/ends-don-t-justify-means-among-humans"
     "https://www.greaterwrong.com/posts/dWTEtgBfFaz6vjwQf/ethical-injunctions"
     "https://www.greaterwrong.com/posts/SGR4GxFK7KmW7ckCB/something-to-protect"
     "https://www.greaterwrong.com/posts/AJ9dX59QXokZb35fk/when-not-to-use-probabilities"
     "https://www.greaterwrong.com/posts/6ddcsdA2c2XpNpE5x/newcomb-s-problem-and-regret-of-rationality"
     "https://www.greaterwrong.com/posts/uD9TDHPwQ5hx4CgaX/my-childhood-death-spiral"
     "https://www.greaterwrong.com/posts/BA7dRRrzMLyvfJr9J/my-best-and-worst-mistake"
     "https://www.greaterwrong.com/posts/uNWRXtdwL33ELgWjD/raised-in-technophilia"
     "https://www.greaterwrong.com/posts/CcBe9aCKDgT5FSoty/a-prodigy-of-refutation"
     "https://www.greaterwrong.com/posts/Yicjw6wSSaPdb83w9/the-sheer-folly-of-callow-youth"
     "https://www.greaterwrong.com/posts/SwCwG9wZcAzQtckwx/that-tiny-note-of-discord"
     "https://www.greaterwrong.com/posts/PCfaLLtuxes6Jk4S2/fighting-a-rearguard-action-against-the-truth"
     "https://www.greaterwrong.com/posts/75LZMCCePG4Pwj3dB/my-naturalistic-awakening"
     "https://www.greaterwrong.com/posts/kXSETKZ3X9oidMozA/the-level-above-mine"
     "https://www.greaterwrong.com/posts/fLRPeXihRaiRo5dyX/the-magnitude-of-his-own-folly"
     "https://www.greaterwrong.com/posts/sYgv4eYH82JEsTD34/beyond-the-reach-of-god"
     "https://www.greaterwrong.com/posts/Ti3Z7eZtud32LhGZT/my-bayesian-enlightenment"
     "https://www.greaterwrong.com/posts/DoLQN5ryZ9XkZjq5h/tsuyoku-naritai-i-want-to-become-stronger"
     "https://www.greaterwrong.com/posts/gWGA8Da539EQmAR9F/tsuyoku-vs-the-egalitarian-instinct"
     "https://www.greaterwrong.com/posts/WLJwTJ7uGPA5Qphbp/trying-to-try"
     "https://www.greaterwrong.com/posts/fhEPnveFhb9tmd7Pe/use-the-try-harder-luke"
     "https://www.greaterwrong.com/posts/fpecAJLG9czABgCe9/on-doing-the-impossible"
     "https://www.greaterwrong.com/posts/GuEsfTpSDSbXFiseH/make-an-extraordinary-effort"
     "https://www.greaterwrong.com/posts/nCvvhFBaayaXyuBiD/shut-up-and-do-the-impossible"
     "https://www.greaterwrong.com/posts/yffPyiu7hRLyc7r23/final-words"
     "https://www.greaterwrong.com/posts/XqmjdBKa4ZaXJtNmf/raising-the-sanity-waterline"
     "https://www.greaterwrong.com/posts/Nu3wa6npK4Ry66vFp/a-sense-that-more-is-possible"
     "https://www.greaterwrong.com/posts/T8ddXNtmNSHexhQh8/epistemic-viciousness"
     "https://www.greaterwrong.com/posts/JnKCaGcgZL4Rsep8m/schools-proliferating-without-evidence"
     "https://www.greaterwrong.com/posts/5K7CMa6dEL7TN7sae/3-levels-of-rationality-verification"
     "https://www.greaterwrong.com/posts/7FzD7pNm9X68Gp5ZC/why-our-kind-can-t-cooperate"
     "https://www.greaterwrong.com/posts/JKxxFseBWz8SHkTgt/tolerate-tolerance"
     "https://www.greaterwrong.com/posts/Q8evewZW5SeidLdbA/your-price-for-joining"
     "https://www.greaterwrong.com/posts/3fNL2ssfvRzpApvdN/can-humanism-match-religion-s-output"
     "https://www.greaterwrong.com/posts/p5DmraxDmhvMoZx8J/church-vs-taskforce"
     "https://www.greaterwrong.com/posts/4PPE6D635iBcGPGRy/rationality-common-interest-of-many-causes"
     "https://www.greaterwrong.com/posts/f42BHX7rMw2dyFJfT/helpless-individuals"
     "https://www.greaterwrong.com/posts/ZpDnRCeef2CLEFeKM/money-the-unit-of-caring"
     "https://www.greaterwrong.com/posts/3p3CYauiX8oLjmwRF/purchase-fuzzies-and-utilons-separately"
     "https://www.greaterwrong.com/posts/K5nq3KcDXaGm7QQWR/bystander-apathy"
     "https://www.greaterwrong.com/posts/NnQbfLo868wgnHF4n/collective-apathy-and-the-internet"
     "https://www.greaterwrong.com/posts/oZNXmHcdhb4m7vwsv/incremental-progress-and-the-valley"
     "https://www.greaterwrong.com/posts/KsHmn6iJAEr9bACQW/bayesians-vs-barbarians"
     "https://www.greaterwrong.com/posts/6NvbSwuSAooQxxf7f/beware-of-other-optimizing"
     "https://www.greaterwrong.com/posts/LqjKP255fPRY7aMzw/practical-advice-backed-by-deep-theories"
     "https://www.greaterwrong.com/posts/pkFazhcTErMw7TFtT/the-sin-of-underconfidence"
     "https://www.greaterwrong.com/posts/aFEsqd6ofwnkNqaXo/go-forth-and-create-the-art"

     ;; Original sequences https://www.greaterwrong.com/tag/original-sequences

     "https://www.greaterwrong.com/posts/RcZCwxFiZzE6X7nsv/what-do-we-mean-by-rationality"
     "https://www.greaterwrong.com/posts/YshRbqZHYFoEMqFAu/why-truth-and"
     "https://www.greaterwrong.com/posts/6s3xABaXKPdFwA3FS/what-is-evidence"
     "https://www.greaterwrong.com/posts/nj8JKFoLSMEmD3RGp/how-much-evidence-does-it-take"
     "https://www.greaterwrong.com/posts/6FmqiAgS8h4EJm86s/how-to-convince-me-that-2-2-3"
     "https://www.greaterwrong.com/posts/f4txACqDWithRi7hs/occam-s-razor"
     "https://www.greaterwrong.com/posts/46qnWRSR7L2eyNbMA/the-lens-that-sees-its-flaws"
     "https://www.greaterwrong.com/posts/a7n8GdKiAZRX86T5A/making-beliefs-pay-rent-in-anticipated-experiences"
     "https://www.greaterwrong.com/posts/CqyJzDZWvGhhFJ7dY/belief-in-belief"
     "https://www.greaterwrong.com/posts/NKaPFf98Y5otMbsPk/bayesian-judo"
     "https://www.greaterwrong.com/posts/RmCjazjupRGcHSm5N/professing-and-cheering"
     "https://www.greaterwrong.com/posts/nYkMLFpx77Rz3uo9c/belief-as-attire"
     "https://www.greaterwrong.com/posts/GJ4ZQm7crTzTM6xDW/focus-your-uncertainty"
     "https://www.greaterwrong.com/posts/yDfxTj9TKYsYiWH5o/the-virtue-of-narrowness"
     "https://www.greaterwrong.com/posts/5JDkW4MYXit2CquLs/your-strength-as-a-rationalist"
     "https://www.greaterwrong.com/posts/mnS2WYLCGJP2kQkRn/absence-of-evidence-is-evidence-of-absence"
     "https://www.greaterwrong.com/posts/jiBFC7DcCrZjGmZnJ/conservation-of-expected-evidence"
     "https://www.greaterwrong.com/posts/fkM9XsNvXdYH6PPAx/hindsight-bias"
     "https://www.greaterwrong.com/posts/WnheMGAka4fL99eae/hindsight-devalues-science"
     "https://www.greaterwrong.com/posts/fysgqk4CjAwhBgNYT/fake-explanations"
     "https://www.greaterwrong.com/posts/NMoLJuDJEms7Ku9XS/guessing-the-teacher-s-password"
     "https://www.greaterwrong.com/posts/4Bwr6s9dofvqPWakn/science-as-attire"
     "https://www.greaterwrong.com/posts/RgkqLqkg8vLhsYpfh/fake-causality"
     "https://www.greaterwrong.com/posts/FWMfQKG3RpZx6irjm/semantic-stopsigns"
     "https://www.greaterwrong.com/posts/6i3zToomS86oj9bS6/mysterious-answers-to-mysterious-questions"
     "https://www.greaterwrong.com/posts/8QzZKw9WHRxjR4948/the-futility-of-emergence"
     "https://www.greaterwrong.com/posts/kpRSCH7ALLcb6ucWM/say-not-complexity"
     "https://www.greaterwrong.com/posts/rmAbiEKQDpDnZzcRf/positive-bias-look-into-the-dark"
     "https://www.greaterwrong.com/posts/DwtYPRuCxpXTrzG9m/my-wild-and-reckless-youth"
     "https://www.greaterwrong.com/posts/97Y7Jwrzxyfzz3Ad2/failing-to-learn-from-history"
     "https://www.greaterwrong.com/posts/TLKPj4GDXetZuPDH5/making-history-available"
     "https://www.greaterwrong.com/posts/yxvi9RitzZDpqn6Yh/explain-worship-ignore"
     "https://www.greaterwrong.com/posts/L22jhyY9ocXQNLqyE/science-as-curiosity-stopper"
     "https://www.greaterwrong.com/posts/dLbkrPu5STNCBLRjr/applause-lights"
     "https://www.greaterwrong.com/posts/fg9fXrHpeaDD6pEPL/truly-part-of-you"
     "https://www.greaterwrong.com/posts/NyFtHycJvkyNjXNsP/chaotic-inversion"
     "https://www.greaterwrong.com/posts/6hfGNLf4Hg5DXqJCF/a-fable-of-science-and-politics"
     "https://www.greaterwrong.com/posts/9weLK2AJ9JEt2Tt8f/politics-is-the-mind-killer"
     "https://www.greaterwrong.com/posts/PeSzc9JTBxhaYRp9b/policy-debates-should-not-appear-one-sided"
     "https://www.greaterwrong.com/posts/XYCEB9roxEBfgjfxs/the-scales-of-justice-the-notebook-of-rationality"
     "https://www.greaterwrong.com/posts/DB6wbyrMugYMK5o6a/correspondence-bias"
     "https://www.greaterwrong.com/posts/28bAMAxhoX3bwbAKC/are-your-enemies-innately-evil"
     "https://www.greaterwrong.com/posts/MBpj3QKfPg9xKNeXW/the-robbers-cave-experiment"
     "https://www.greaterwrong.com/posts/qNZM3EGoE5ZeMdCRt/reversed-stupidity-is-not-intelligence"
     "https://www.greaterwrong.com/posts/5yFRd3cjLpm3Nd6Di/argument-screens-off-authority"
     "https://www.greaterwrong.com/posts/2jp98zdLo898qExrr/hug-the-query"
     "https://www.greaterwrong.com/posts/Lz64L3yJEtYGkzMzu/rationality-and-the-english-language"
     "https://www.greaterwrong.com/posts/t6Fe2PsEwb3HhcBEr/the-litany-against-gurus"
     "https://www.greaterwrong.com/posts/n5xT2RJy2fWxCA3eH/politics-and-awful-art"
     "https://www.greaterwrong.com/posts/NbbK6YKTpQR7u7D6u/false-laughter"
     "https://www.greaterwrong.com/posts/i8q4vXestDkGTFwsc/human-evil-and-muddled-thinking"
     "https://www.greaterwrong.com/posts/Kow8xRzpfkoY7pa69/the-affect-heuristic"
     "https://www.greaterwrong.com/posts/3T6p93Mut7G8qdkAs/evaluability-and-cheap-holiday-shopping"
     "https://www.greaterwrong.com/posts/5u5THLyRkTpPHiaG5/unbounded-scales-huge-jury-awards-and-futurism"
     "https://www.greaterwrong.com/posts/ACGeaAk6KButv2xwQ/the-halo-effect"
     "https://www.greaterwrong.com/posts/krMzmSXgvEdf7iBT6/superhero-bias"
     "https://www.greaterwrong.com/posts/Tv7WWhgbKMWzEnMmx/mere-messiahs"
     "https://www.greaterwrong.com/posts/XrzQW69HpidzvBxGr/affective-death-spirals"
     "https://www.greaterwrong.com/posts/hwi8JQjspnMWyWs4g/resist-the-happy-death-spiral"
     "https://www.greaterwrong.com/posts/NCefvet6X3Sd4wrPc/uncritical-supercriticality"
     "https://www.greaterwrong.com/posts/ZQG9cwKbct2LtmL3p/evaporative-cooling-of-group-beliefs"
     "https://www.greaterwrong.com/posts/Tw9cLvzSKrkGjNHW3/when-none-dare-urge-restraint"
     "https://www.greaterwrong.com/posts/MBpj3QKfPg9xKNeXW/the-robbers-cave-experiment"
     "https://www.greaterwrong.com/posts/MBpj3QKfPg9xKNeXW/the-robbers-cave-experiment"
     "https://www.greaterwrong.com/posts/yEjaj7PWacno5EvWa/every-cause-wants-to-be-a-cult"
     "https://www.greaterwrong.com/posts/etBrzxdfNop3DqJvA/guardians-of-the-truth"
     "https://www.greaterwrong.com/posts/aFtWRL3QihoF5uQd5/guardians-of-the-gene-pool"
     "https://www.greaterwrong.com/posts/96TBXaHwLbFyeAxrg/guardians-of-ayn-rand"
     "https://www.greaterwrong.com/posts/t6Fe2PsEwb3HhcBEr/the-litany-against-gurus"
     "https://www.greaterwrong.com/posts/Qr4MB9hFRzamuMRHJ/two-cult-koans"
     "https://www.greaterwrong.com/posts/WHK94zXkQm7qm7wXk/asch-s-conformity-experiment"
     "https://www.greaterwrong.com/posts/CEGnJBHmkcwPTysb7/lonely-dissent"
     "https://www.greaterwrong.com/posts/gBma88LH3CLQsqyfS/cultish-countercultishness"
     "https://www.greaterwrong.com/posts/bMkCEZoBNhgRBtzoj/anchoring-and-adjustment"
     "https://www.greaterwrong.com/posts/BaCWFCxBQYjJXSsah/priming-and-contamination"
     "https://www.greaterwrong.com/posts/TiDGXt3WrQwtCdDj3/do-we-believe-everything-we-re-told"
     "https://www.greaterwrong.com/posts/2MD3NMLBPCqPfnfre/cached-thoughts"
     "https://www.greaterwrong.com/posts/qu95AwSrKqQSo4fCY/the-outside-the-box-box"
     "https://www.greaterwrong.com/posts/SA79JMXKWke32A3hG/original-seeing"
     "https://www.greaterwrong.com/posts/rHBdcHGLJ7KvLJQPk/the-logical-fallacy-of-generalization-from-fictional"
     "https://www.greaterwrong.com/posts/aSQy7yHj6nPD44RNo/how-to-seem-and-be-deep"
     "https://www.greaterwrong.com/posts/buixYfcXBah9hbSNZ/we-change-our-minds-less-often-than-we-think"
     "https://www.greaterwrong.com/posts/uHYYA32CKgKT3FagE/hold-off-on-proposing-solutions"
     "https://www.greaterwrong.com/posts/WHK94zXkQm7qm7wXk/asch-s-conformity-experiment"
     "https://www.greaterwrong.com/posts/ovvwAhKKoNbfcMz8K/on-expressing-your-concerns"
     "https://www.greaterwrong.com/posts/CEGnJBHmkcwPTysb7/lonely-dissent"
     "https://www.greaterwrong.com/posts/KZLa74SzyKhSJ3M55/the-genetic-fallacy"
     "https://www.greaterwrong.com/posts/5JDkW4MYXit2CquLs/your-strength-as-a-rationalist"
     "https://www.greaterwrong.com/posts/mnS2WYLCGJP2kQkRn/absence-of-evidence-is-evidence-of-absence"
     "https://www.greaterwrong.com/posts/fkM9XsNvXdYH6PPAx/hindsight-bias"
     "https://www.greaterwrong.com/posts/WnheMGAka4fL99eae/hindsight-devalues-science"
     "https://www.greaterwrong.com/posts/rmAbiEKQDpDnZzcRf/positive-bias-look-into-the-dark"
     "https://www.greaterwrong.com/posts/AdYdLP2sRqPMoe8fb/knowing-about-biases-can-hurt-people"
     "https://www.greaterwrong.com/posts/627DZcvme7nLDrbZu/update-yourself-incrementally"
     "https://www.greaterwrong.com/posts/WN73eiLQkuDtSC8Ag/one-argument-against-an-army"
     "https://www.greaterwrong.com/posts/34XxbRFe54FycoCDw/the-bottom-line"
     "https://www.greaterwrong.com/posts/kJiPnaQPiy4p9Eqki/what-evidence-filtered-evidence"
     "https://www.greaterwrong.com/posts/SFZoEBpLo9frSJGkc/rationalization"
     "https://www.greaterwrong.com/posts/9f5EXt8KNNxTAihtZ/a-rational-argument"
     "https://www.greaterwrong.com/posts/dHQkDNMhj692ayx78/avoiding-your-belief-s-real-weak-points"
     "https://www.greaterwrong.com/posts/L32LHWzy9FzSDazEg/motivated-stopping-and-motivated-continuation"
     "https://www.greaterwrong.com/posts/i2ruK7M3coWfv8mfD/a-case-study-of-motivated-continuation"
     "https://www.greaterwrong.com/posts/bfbiyTogEKWEGP96S/fake-justification"
     "https://www.greaterwrong.com/posts/i6fKszWY6gLZSX2Ey/fake-optimization-criteria"
     "https://www.greaterwrong.com/posts/TGux5Fhcd7GmTfNGC/is-that-your-true-rejection"
     "https://www.greaterwrong.com/posts/wyyfFfaRar2jEdeQK/entangled-truths-contagious-lies"
     "https://www.greaterwrong.com/posts/E7CKXxtGKPmdM9ZRc/of-lies-and-black-swan-blowups"
     "https://www.greaterwrong.com/posts/XTWkjCJScy2GFAgDt/dark-side-epistemology"
     "https://www.greaterwrong.com/posts/Fwt4sDDacko8Sh5iR/the-sacred-mundane"
     "https://www.greaterwrong.com/posts/CahCppKy9HuXe3j2i/singlethink"
     "https://www.greaterwrong.com/posts/Hs3ymqypvhgFMkgLb/doublethink-choosing-to-be-biased"
     "https://www.greaterwrong.com/posts/rZX4WuufAPbN6wQTv/no-really-i-ve-deceived-myself"
     "https://www.greaterwrong.com/posts/wP2ymm44kZZwaFPYh/belief-in-self-deception"
     "https://www.greaterwrong.com/posts/ERRk4thxxYNcScqR4/moore-s-paradox"
     "https://www.greaterwrong.com/posts/W7LcN9gmdnaAk9K52/don-t-believe-you-ll-self-deceive"
     "https://www.greaterwrong.com/posts/GrDqnMjhqoxiqpQPw/the-proper-use-of-humility"
     "https://www.greaterwrong.com/posts/erGipespbbzdG5zYb/the-third-alternative"
     "https://www.greaterwrong.com/posts/X2AD2LgtKgkRNPj2a/privileging-the-hypothesis"
     "https://www.greaterwrong.com/posts/q7Me34xvSG3Wm97As/but-there-s-still-a-chance-right"
     "https://www.greaterwrong.com/posts/dLJv2CoRCgeC2mPgj/the-fallacy-of-gray"
     "https://www.greaterwrong.com/posts/PmQkensvTGg7nGtJE/absolute-authority"
     "https://www.greaterwrong.com/posts/6FmqiAgS8h4EJm86s/how-to-convince-me-that-2-2-3"
     "https://www.greaterwrong.com/posts/ooypcn7qFzsMcy53R/infinite-certainty"
     "https://www.greaterwrong.com/posts/QGkYCwyC7wTDyt3yT/0-and-1-are-not-probabilities"
     "https://www.greaterwrong.com/posts/SqF8cHjJv43mvJJzx/feeling-rational"
     "https://www.greaterwrong.com/posts/wCqfCLs8z5Qw4GbKS/the-importance-of-saying-oops"
     "https://www.greaterwrong.com/posts/qRWfvgJG75ESLRNu9/the-crackpot-offer"
     "https://www.greaterwrong.com/posts/waqC6FihC2ryAZuAq/just-lose-hope-already"
     "https://www.greaterwrong.com/posts/43PTNr4ZMaezyAJ5o/the-proper-use-of-doubt"
     "https://www.greaterwrong.com/posts/HYWhKXRsMAyvRKRYz/you-can-face-reality"
     "https://www.greaterwrong.com/posts/3nZMgRTfFEfHp34Gb/the-meditation-on-curiosity"
     "https://www.greaterwrong.com/posts/SGR4GxFK7KmW7ckCB/something-to-protect"
     "https://www.greaterwrong.com/posts/eY45uCCX7DdwJ4Jha/no-one-can-exempt-you-from-rationality-s-laws"
     "https://www.greaterwrong.com/posts/3XgYbghWruBMrPTAL/leave-a-line-of-retreat"
     "https://www.greaterwrong.com/posts/BcYBfG8KomcpcxkEg/crisis-of-faith"
     "https://www.greaterwrong.com/posts/kXAb5riiaJNrfR8v8/the-ritual"
     "https://www.greaterwrong.com/posts/pLRogvJLPPg6Mrvg4/an-alien-god"
     "https://www.greaterwrong.com/posts/ZyNak8F6WXjuEbWWc/the-wonder-of-evolution"
     "https://www.greaterwrong.com/posts/jAToJHtg39AMTAuJo/evolutions-are-stupid-but-work-anyway"
     "https://www.greaterwrong.com/posts/XPErvb8m9FapXCjhA/adaptation-executers-not-fitness-maximizers"
     "https://www.greaterwrong.com/posts/XC7Kry5q6CD9TyG4K/no-evolutions-for-corporations-or-nanodevices"
     "https://www.greaterwrong.com/posts/gDNrpuwahdRrDJ9iY/evolving-to-extinction"
     "https://www.greaterwrong.com/posts/QsMJQSFj7WfoTMNgW/the-tragedy-of-group-selectionism"
     "https://www.greaterwrong.com/posts/i6fKszWY6gLZSX2Ey/fake-optimization-criteria"
     "https://www.greaterwrong.com/posts/hQxYBfu2LPc9Ydo6w/the-parable-of-the-dagger"
     "https://www.greaterwrong.com/posts/bcM5ft8jvsffsZZ4Y/the-parable-of-hemlock"
     "https://www.greaterwrong.com/posts/3nxs2WYDGzJbzcLMp/words-as-hidden-inferences"
     "https://www.greaterwrong.com/posts/HsznWM9A7NiuGsp28/extensions-and-intensions"
     "https://www.greaterwrong.com/posts/jMTbQj9XB5ah2maup/similarity-clusters"
     "https://www.greaterwrong.com/posts/4mEsPHqcbRWxnaE5b/typicality-and-asymmetrical-similarity"
     "https://www.greaterwrong.com/posts/WBw8dDkAWohFjWQSk/the-cluster-structure-of-thingspace"
     "https://www.greaterwrong.com/posts/4FcxgdvdQP45D6Skg/disguised-queries"
     "https://www.greaterwrong.com/posts/yFDKvfN6D87Tf5J9f/neural-categories"
     "https://www.greaterwrong.com/posts/yA4gF5KrboK2m2Xu7/how-an-algorithm-feels-from-inside"
     "https://www.greaterwrong.com/posts/7X2j8HAkWdmMoS8PE/disputing-definitions"
     "https://www.greaterwrong.com/posts/dMCFk2n2ur8n62hqB/feel-the-meaning"
     "https://www.greaterwrong.com/posts/9ZooAqfh2TC9SBDvq/the-argument-from-common-usage"
     "https://www.greaterwrong.com/posts/i2dfY65JciebF3CAo/empty-labels"
     "https://www.greaterwrong.com/posts/WBdvyyHLdxZSAMmoz/taboo-your-words"
     "https://www.greaterwrong.com/posts/GKfPL6LQFgB49FEnv/replace-the-symbol-with-the-substance"
     "https://www.greaterwrong.com/posts/y5MxoeacRKKM3KQth/fallacies-of-compression"
     "https://www.greaterwrong.com/posts/veN86cBhoe7mBxXLk/categorizing-has-consequences"
     "https://www.greaterwrong.com/posts/yuKaWPRTxZoov4z8K/sneaking-in-connotations"
     "https://www.greaterwrong.com/posts/cFzC996D7Jjds3vS9/arguing-by-definition"
     "https://www.greaterwrong.com/posts/d5NyJ2Lf6N22AD9PB/where-to-draw-the-boundary"
     "https://www.greaterwrong.com/posts/soQX8yXLbKy7cFvy8/entropy-and-short-codes"
     "https://www.greaterwrong.com/posts/yLcuygFfMfrfK8KjF/mutual-information-and-density-in-thingspace"
     "https://www.greaterwrong.com/posts/82eMd5KLiJ5Z6rTrr/superexponential-conceptspace-and-simple-words"
     "https://www.greaterwrong.com/posts/gDWvLicHhcMfGmwaK/conditional-independence-and-naive-bayes"
     "https://www.greaterwrong.com/posts/YF9HB6cWCJrDK5pBM/words-as-mental-paintbrush-handles"
     "https://www.greaterwrong.com/posts/shoMpaoZypfkXv84Y/variable-question-fallacies"
     "https://www.greaterwrong.com/posts/FaJaCgqBKphrDzDSj/37-ways-that-words-can-be-wrong"
     "https://www.greaterwrong.com/posts/LaM5aTcXvXzwQSC2Q/universal-fire"
     "https://www.greaterwrong.com/posts/7iTwGquBFZKttpEdE/universal-law"
     "https://www.greaterwrong.com/posts/Mc6QcrsbH5NRXbCRX/dissolving-the-question"
     "https://www.greaterwrong.com/posts/XzrqkhfwtiSDgKoAF/wrong-questions"
     "https://www.greaterwrong.com/posts/rQEwySCcLtdKHkrHp/righting-a-wrong-question"
     "https://www.greaterwrong.com/posts/ZTRiSNmeGQK8AkdN2/mind-projection-fallacy"
     "https://www.greaterwrong.com/posts/f6ZLxEWaankRZ2Crv/probability-is-in-the-mind"
     "https://www.greaterwrong.com/posts/np3tP49caG4uFLRbS/the-quotation-is-not-the-referent"
     "https://www.greaterwrong.com/posts/BwtBhqvTPGG2n2GuJ/qualitatively-confused"
     "https://www.greaterwrong.com/posts/tPqQdLCuxanjhoaNs/reductionism"
     "https://www.greaterwrong.com/posts/cphoF8naigLhRf3tu/explaining-vs-explaining-away"
     "https://www.greaterwrong.com/posts/mTf8MkpAigm3HP6x2/fake-reductionism"
     "https://www.greaterwrong.com/posts/kQzs8MFbBxdYhe3hK/savanna-poets"
     "https://www.greaterwrong.com/posts/x4dG4GhpZH2hgz59x/joy-in-the-merely-real"
     "https://www.greaterwrong.com/posts/KfMNFB3G7XNviHBPN/joy-in-discovery"
     "https://www.greaterwrong.com/posts/WjpA4PCjt5EkTGbLF/bind-yourself-to-reality"
     "https://www.greaterwrong.com/posts/iiWiHgtQekWNnmE6Q/if-you-demand-magic-magic-won-t-help"
     "https://www.greaterwrong.com/posts/SXK87NgEPszhWkvQm/mundane-magic"
     "https://www.greaterwrong.com/posts/ndGYn7ZFiZyernp9f/the-beauty-of-settled-science"
     "https://www.greaterwrong.com/posts/hYqDp4qAucZM33qSh/amazing-breakthrough-day-april-1st"
     "https://www.greaterwrong.com/posts/PMr6f7ZocEWFtCYXj/is-humanism-a-religion-substitute"
     "https://www.greaterwrong.com/posts/MCYp8g9EMAiTCTawk/scarcity"
     "https://www.greaterwrong.com/posts/3diLhMELXxM8rFHJj/to-spread-science-keep-it-secret"
     "https://www.greaterwrong.com/posts/fnEWQAYxcRnaYBqaZ/initiation-ceremony"
     "https://www.greaterwrong.com/posts/98cPZGwetyJsrXNdW/awww-a-zebra"
     "https://www.greaterwrong.com/posts/KmghfjH6RgXvoKruJ/hand-vs-fingers"
     "https://www.greaterwrong.com/posts/ddwk9veF8efn3Nzbu/angry-atoms"
     "https://www.greaterwrong.com/posts/ne6Ra62FB9ACHGSuh/heat-vs-motion"
     "https://www.greaterwrong.com/posts/nzzNFcrSk7akQ9bwD/brain-breakthrough-it-s-made-of-neurons"
     "https://www.greaterwrong.com/posts/gRa5cWWBsZqdFvmqu/reductive-reference"
     "https://www.greaterwrong.com/posts/fdEWWr8St59bXLbQr/zombies-zombies"
     "https://www.greaterwrong.com/posts/4moMTeCy9EqYxAher/zombie-responses"
     "https://www.greaterwrong.com/posts/kYAuNJX2ecH2uFqZ9/the-generalized-anti-zombie-principle"
     "https://www.greaterwrong.com/posts/k6EPphHiBH4WWYFCj/gazp-vs-glut"
     "https://www.greaterwrong.com/posts/3XMwPNMSbaPm2suGz/belief-in-the-implied-invisible"
     "https://www.greaterwrong.com/posts/fsDz6HieZJBu54Yes/zombies-the-movie"
     "https://www.greaterwrong.com/posts/PXoWk554FZ4Gpfvah/causal-reference"
     "https://www.greaterwrong.com/posts/u6JzcFtPGiznFgDxP/excluding-the-supernatural"
     "https://www.greaterwrong.com/posts/7Au7kvRAPREm3ADcK/psychic-powers"
     "https://www.greaterwrong.com/posts/7FSwbFpDsca7uXpQ2/quantum-explanations"
     "https://www.greaterwrong.com/posts/5vZD32EynD9n94dhr/configurations-and-amplitude"
     "https://www.greaterwrong.com/posts/ybusFwDqiZgQa6NCq/joint-configurations"
     "https://www.greaterwrong.com/posts/KbeHkLNY5ETJ3TN3W/distinct-configurations"
     "https://www.greaterwrong.com/posts/Bh9cdfMjATrTdLrGH/where-philosophy-meets-science"
     "https://www.greaterwrong.com/posts/Bp8vnEciPA5TXSy6f/can-you-prove-two-particles-are-identical"
     "https://www.greaterwrong.com/posts/KAHt3t7a6KH4kfX4L/classical-configuration-spaces"
     "https://www.greaterwrong.com/posts/eHeJxJZii6tqQupZL/the-quantum-arena"
     "https://www.greaterwrong.com/posts/oiu7YhzrDTvCxMhdS/feynman-paths"
     "https://www.greaterwrong.com/posts/Cpf2jsZsNFNH5TSpc/no-individual-particles"
     "https://www.greaterwrong.com/posts/RLScTpwc5W2gGGrL9/identity-isn-t-in-specific-atoms"
     "https://www.greaterwrong.com/posts/JrhoMTgMrMRJJiS48/decoherence"
     "https://www.greaterwrong.com/posts/eWuuznxeebcjWpdnH/the-so-called-heisenberg-uncertainty-principle"
     "https://www.greaterwrong.com/posts/XDkeuJTFjM9Y2x6v6/which-basis-is-more-fundamental"
     "https://www.greaterwrong.com/posts/WajiC3YWeJutyAXTn/where-physics-meets-experience"
     "https://www.greaterwrong.com/posts/vGbHKfgFNDeJohfeN/where-experience-confuses-physicists"
     "https://www.greaterwrong.com/posts/pRrksC5Y6TbyvKDJE/on-being-decoherent"
     "https://www.greaterwrong.com/posts/nso8WXdjHLLHkJKhr/the-conscious-sorites-paradox"
     "https://www.greaterwrong.com/posts/aWFwfk3MBEyR4Ne8C/decoherence-is-pointless"
     "https://www.greaterwrong.com/posts/HwMfEcmxyM3eqqfvi/decoherent-essences"
     "https://www.greaterwrong.com/posts/3ZKvf9u2XEWddGZmS/the-born-probabilities"
     "https://www.greaterwrong.com/posts/EneHGx8t8skPKxHhv/decoherence-as-projection"
     "https://www.greaterwrong.com/posts/GmFuZcE6udo7bykxP/entangled-photons"
     "https://www.greaterwrong.com/posts/AnHJX42C6r6deohTG/bell-s-theorem-no-epr-reality"
     "https://www.greaterwrong.com/posts/DY9h6zxq6EMHrkkxE/spooky-action-at-a-distance-the-no-communication-theorem"
     "https://www.greaterwrong.com/posts/Atu4teGvob5vKvEAF/decoherence-is-simple"
     "https://www.greaterwrong.com/posts/DFxoaWGEh9ndwtZhk/decoherence-is-falsifiable-and-testable"
     "https://www.greaterwrong.com/posts/k3823vuarnmL5Pqin/quantum-non-realism"
     "https://www.greaterwrong.com/posts/xsZnufn3cQw7tJeQ3/collapse-postulates"
     "https://www.greaterwrong.com/posts/WqGCaRhib42dhKWRL/if-many-worlds-had-come-first"
     "https://www.greaterwrong.com/posts/S8ysHqeRGuySPttrS/many-worlds-one-best-guess"
     "https://www.greaterwrong.com/posts/qcYCAxYZT4Xp9iMZY/living-in-many-worlds"
     "https://www.greaterwrong.com/posts/NsgcZx4BeTy5y84Ya/mach-s-principle-anti-epiphenomenal-physics"
     "https://www.greaterwrong.com/posts/vLZtf64wkyoAFNcu3/relative-configuration-space"
     "https://www.greaterwrong.com/posts/rrW7yf42vQYDf8AcH/timeless-physics"
     "https://www.greaterwrong.com/posts/GKTe9bCxFSE6EXEEu/timeless-beauty"
     "https://www.greaterwrong.com/posts/KipiHsTA3pw4joQkG/timeless-causality"
     "https://www.greaterwrong.com/posts/924arDrTu3QRHFA5r/timeless-identity"
     "https://www.greaterwrong.com/posts/NEeW7eSXThPz7o4Ne/thou-art-physics"
     "https://www.greaterwrong.com/posts/YYLmZFEGKsjCKQZut/timeless-control"
     "https://www.greaterwrong.com/posts/ZxR8P8hBFQ9kC8wMy/the-failures-of-eld-science"
     "https://www.greaterwrong.com/posts/viPPjojmChxLGPE2v/the-dilemma-science-or-bayes"
     "https://www.greaterwrong.com/posts/5bJyRMZzwMov5u3hW/science-doesn-t-trust-your-rationality"
     "https://www.greaterwrong.com/posts/wzxneh7wxkdNYNbtB/when-science-can-t-help"
     "https://www.greaterwrong.com/posts/PGfJdgemDJSwWBZSX/science-isn-t-strict-enough"
     "https://www.greaterwrong.com/posts/WijMw9WkcafmCFgj4/do-scientists-already-know-this-stuff"
     "https://www.greaterwrong.com/posts/wustx45CPL5rZenuo/no-safe-defense-not-even-science"
     "https://www.greaterwrong.com/posts/SoDsr8GEZmRKMZNkj/changing-the-definition-of-science"
     "https://www.greaterwrong.com/posts/xTyuQ3cgsPjifr7oj/faster-than-science"
     "https://www.greaterwrong.com/posts/mpaqTWGiLT7GA3w76/einstein-s-speed"
     "https://www.greaterwrong.com/posts/5wMcKNAwB6X4mp9og/that-alien-message"
     "https://www.greaterwrong.com/posts/3Jpchgy53D2gB5qdk/my-childhood-role-model"
     "https://www.greaterwrong.com/posts/5o4EZJyqmHY4XgRCY/einstein-s-superpowers"
     "https://www.greaterwrong.com/posts/xAXrEpF5FYjwqKMfZ/class-project"
     "https://www.greaterwrong.com/posts/gDL9NDEXPxYpDf4vz/why-quantum"
     "https://www.greaterwrong.com/posts/RFnkagDaJSBLDXEHs/heading-toward-morality"
     "https://www.greaterwrong.com/posts/PtoQdG7E8MxYJrigu/no-universally-compelling-arguments"
     "https://www.greaterwrong.com/posts/eDpPnT7wdBwWPGvo5/2-place-and-1-place-words"
     "https://www.greaterwrong.com/posts/iGH7FSrdoCXa5AHGs/what-would-you-do-without-morality"
     "https://www.greaterwrong.com/posts/K9JSM7d7bLJguMxEp/the-moral-void"
     "https://www.greaterwrong.com/posts/CuSTqHgeK4CMpWYTe/created-already-in-motion"
     "https://www.greaterwrong.com/posts/iAxkfiyG8WizPSPbq/the-bedrock-of-fairness"
     "https://www.greaterwrong.com/posts/SbdCX6A5AGyyfhdmh/moral-complexities"
     "https://www.greaterwrong.com/posts/F5WLc7hCxkB4X4yD4/is-morality-preference"
     "https://www.greaterwrong.com/posts/iQNKfYb7aRYopojTX/is-morality-given"
     "https://www.greaterwrong.com/posts/C8nEXTcjZb9oauTCW/where-recursive-justification-hits-bottom"
     "https://www.greaterwrong.com/posts/TynBiYt6zg42StRbb/my-kind-of-reflection"
     "https://www.greaterwrong.com/posts/KZLa74SzyKhSJ3M55/the-genetic-fallacy"
     "https://www.greaterwrong.com/posts/9EahWKqay6HZcaNTY/fundamental-doubts"
     "https://www.greaterwrong.com/posts/YhNGY6ypoNbLJvDBu/rebelling-within-nature"
     "https://www.greaterwrong.com/posts/XhaKvQyHzeXdNnFKy/probability-is-subjectively-objective"
     "https://www.greaterwrong.com/posts/szAkYJDtXkcSAiHYE/whither-moral-progress"
     "https://www.greaterwrong.com/posts/pGvyqAQw6yqTjpKf4/the-gift-we-give-to-tomorrow"
     "https://www.greaterwrong.com/posts/vy9nnPdwTjSmt5qdb/could-anything-be-right"
     "https://www.greaterwrong.com/posts/8rdoea3g6QGhWQtmx/existential-angst-factory"
     "https://www.greaterwrong.com/posts/dhGGnB2oxBP3m5cBc/can-counterfactuals-be-true"
     "https://www.greaterwrong.com/posts/WAQ3qMD4vdXheQmui/math-is-subjunctively-objective"
     "https://www.greaterwrong.com/posts/GAR8gT3d9uCtr4kv8/does-your-morality-care-what-you-think"
     "https://www.greaterwrong.com/posts/LhP2zGBWR5AdssrdJ/changing-your-metaethics"
     "https://www.greaterwrong.com/posts/T7tYmfD9j25uLwqYk/setting-up-metaethics"
     "https://www.greaterwrong.com/posts/fG3g3764tSubr6xvs/the-meaning-of-right"
     "https://www.greaterwrong.com/posts/7HDtecu4qW9PCsSR6/interpersonal-morality"
     "https://www.greaterwrong.com/posts/FnJPa8E9ZG5xiLLp5/morality-as-fixed-computation"
     "https://www.greaterwrong.com/posts/JynJ6xfnpq9oN3zpb/inseparably-right-or-joy-in-the-merely-good"
     "https://www.greaterwrong.com/posts/mMBTPTjRbsrqbSkZE/sorting-pebbles-into-correct-heaps"
     "https://www.greaterwrong.com/posts/BkkwXtaTf5LvbA6HB/moral-error-and-moral-disagreement"
     "https://www.greaterwrong.com/posts/9KacKm5yBv27rxWnJ/abstracted-idealized-dynamics"
     "https://www.greaterwrong.com/posts/HacgrDxJx3Xr7uwCR/arbitrary"
     "https://www.greaterwrong.com/posts/saw8WAML4NEaJ2Wmz/is-fairness-arbitrary"
     "https://www.greaterwrong.com/posts/RBszS2jwGM4oghXW4/the-bedrock-of-morality-arbitrary"
     "https://www.greaterwrong.com/posts/rm8tv9qZ9nwQxhshx/you-provably-can-t-trust-yourself"
     "https://www.greaterwrong.com/posts/YrhT7YxkRJoRnr7qD/no-license-to-be-human"
     "https://www.greaterwrong.com/posts/sCs48JtMnQwQsZwyN/invisible-frameworks"
     "https://www.greaterwrong.com/posts/pK4HTxuv6mftHXWC3/prolegomena-to-a-theory-of-fun"
     "https://www.greaterwrong.com/posts/29vqqmGNxNRGzffEj/high-challenge"
     "https://www.greaterwrong.com/posts/aEdqh3KPerBNYvoWe/complex-novelty"
     "https://www.greaterwrong.com/posts/QfpHRAMRM2HjteKFK/continuous-improvement"
     "https://www.greaterwrong.com/posts/eLHCWi8sotQT6CmTX/sensual-experience"
     "https://www.greaterwrong.com/posts/dKGfNvjGjq4rqffyF/living-by-your-own-strength"
     "https://www.greaterwrong.com/posts/EZ8GniEPSechjDYP9/free-to-optimize"
     "https://www.greaterwrong.com/posts/CtSS6SkHhLBvdodTY/harmful-options"
     "https://www.greaterwrong.com/posts/MTjej6HKvPByx3dEA/devil-s-offers"
     "https://www.greaterwrong.com/posts/wqDRRx9RqwKLzWt7R/nonperson-predicates"
     "https://www.greaterwrong.com/posts/vwnSPgwtmLjvTK2Wa/amputation-of-destiny"
     "https://www.greaterwrong.com/posts/W5PhyEQqEWTcpRpqn/dunbar-s-function"
     "https://www.greaterwrong.com/posts/WMDy4GxbyYkNrbmrs/in-praise-of-boredom"
     "https://www.greaterwrong.com/posts/NLMo5FZWFFq652MNe/sympathetic-minds"
     "https://www.greaterwrong.com/posts/Py3uGnncqXuEfPtQp/interpersonal-entanglement"
     "https://www.greaterwrong.com/posts/ctpkTaqTKbmm6uRgC/failed-utopia-4-2"
     "https://www.greaterwrong.com/posts/EQkELCGiGQwvrrp3L/growing-up-is-hard"
     "https://www.greaterwrong.com/posts/QZs4vkC7cbyjL9XA9/changing-emotions"
     "https://www.greaterwrong.com/posts/ZmDEbiEeXk3Wv2sLH/emotional-involvement"
     "https://www.greaterwrong.com/posts/6qS9q5zHafFXsB6hf/serious-stories"
     "https://www.greaterwrong.com/posts/hQSaMafoizBSa3gFR/eutopia-is-scary"
     "https://www.greaterwrong.com/posts/cWjK3SbRcLkb3gN69/building-weirdtopia"
     "https://www.greaterwrong.com/posts/DGXvLNpiSYBeQ6TLW/justified-expectation-of-pleasant-surprises"
     "https://www.greaterwrong.com/posts/88BpRQah9c2GWY3En/seduced-by-imagination"
     "https://www.greaterwrong.com/posts/4o3zwgofFPLutkqvd/the-uses-of-fun-theory"
     "https://www.greaterwrong.com/posts/5Pjq3mxuiXu2Ys3gM/higher-purpose"
     "https://www.greaterwrong.com/posts/v8rghtzWCziYuMdJ5/why-does-power-corrupt"
     "https://www.greaterwrong.com/posts/K9ZaZXDnL3SEmYZqB/ends-don-t-justify-means-among-humans"
     "https://www.greaterwrong.com/posts/wyyfFfaRar2jEdeQK/entangled-truths-contagious-lies"
     "https://www.greaterwrong.com/posts/yz2btSaCLHmLWgWD5/protected-from-myself"
     "https://www.greaterwrong.com/posts/cyRpNbPsW8HzsxhRK/ethical-inhibitions"
     "https://www.greaterwrong.com/posts/dWTEtgBfFaz6vjwQf/ethical-injunctions"
     "https://www.greaterwrong.com/posts/K2c3dkKErsqFd28Dh/prices-or-bindings"
     "https://www.greaterwrong.com/posts/hXuB8BCyyiYuzij3F/ethics-notes"
     "https://www.greaterwrong.com/posts/uD9TDHPwQ5hx4CgaX/my-childhood-death-spiral"
     "https://www.greaterwrong.com/posts/BA7dRRrzMLyvfJr9J/my-best-and-worst-mistake"
     "https://www.greaterwrong.com/posts/uNWRXtdwL33ELgWjD/raised-in-technophilia"
     "https://www.greaterwrong.com/posts/CcBe9aCKDgT5FSoty/a-prodigy-of-refutation"
     "https://www.greaterwrong.com/posts/Yicjw6wSSaPdb83w9/the-sheer-folly-of-callow-youth"
     "https://www.greaterwrong.com/posts/SwCwG9wZcAzQtckwx/that-tiny-note-of-discord"
     "https://www.greaterwrong.com/posts/PCfaLLtuxes6Jk4S2/fighting-a-rearguard-action-against-the-truth"
     "https://www.greaterwrong.com/posts/75LZMCCePG4Pwj3dB/my-naturalistic-awakening"
     "https://www.greaterwrong.com/posts/kXSETKZ3X9oidMozA/the-level-above-mine"
     "https://www.greaterwrong.com/posts/CKpByWmsZ8WmpHtYa/competent-elites"
     "https://www.greaterwrong.com/posts/9HGR5qatMGoz4GhKj/above-average-ai-scientists"
     "https://www.greaterwrong.com/posts/fLRPeXihRaiRo5dyX/the-magnitude-of-his-own-folly"
     "https://www.greaterwrong.com/posts/sYgv4eYH82JEsTD34/beyond-the-reach-of-god"
     "https://www.greaterwrong.com/posts/Ti3Z7eZtud32LhGZT/my-bayesian-enlightenment"
     "https://www.greaterwrong.com/posts/GrDqnMjhqoxiqpQPw/the-proper-use-of-humility"
     "https://www.greaterwrong.com/posts/DoLQN5ryZ9XkZjq5h/tsuyoku-naritai-i-want-to-become-stronger"
     "https://www.greaterwrong.com/posts/gWGA8Da539EQmAR9F/tsuyoku-vs-the-egalitarian-instinct"
     "https://www.greaterwrong.com/posts/etBrzxdfNop3DqJvA/guardians-of-the-truth"
     "https://www.greaterwrong.com/posts/96TBXaHwLbFyeAxrg/guardians-of-ayn-rand"
     "https://www.greaterwrong.com/posts/vYsuM8cpuRgZS5rYB/lotteries-a-waste-of-hope"
     "https://www.greaterwrong.com/posts/QawvGzYWhqdyPWgBL/new-improved-lottery"
     "https://www.greaterwrong.com/posts/ZxR8P8hBFQ9kC8wMy/the-failures-of-eld-science"
     "https://www.greaterwrong.com/posts/SGR4GxFK7KmW7ckCB/something-to-protect"
     "https://www.greaterwrong.com/posts/5o4EZJyqmHY4XgRCY/einstein-s-superpowers"
     "https://www.greaterwrong.com/posts/WLJwTJ7uGPA5Qphbp/trying-to-try"
     "https://www.greaterwrong.com/posts/fhEPnveFhb9tmd7Pe/use-the-try-harder-luke"
     "https://www.greaterwrong.com/posts/fpecAJLG9czABgCe9/on-doing-the-impossible"
     "https://www.greaterwrong.com/posts/GuEsfTpSDSbXFiseH/make-an-extraordinary-effort"
     "https://www.greaterwrong.com/posts/nCvvhFBaayaXyuBiD/shut-up-and-do-the-impossible"
     "https://www.greaterwrong.com/posts/XqmjdBKa4ZaXJtNmf/raising-the-sanity-waterline"
     "https://www.greaterwrong.com/posts/Nu3wa6npK4Ry66vFp/a-sense-that-more-is-possible"
     "https://www.greaterwrong.com/posts/T8ddXNtmNSHexhQh8/epistemic-viciousness"
     "https://www.greaterwrong.com/posts/JnKCaGcgZL4Rsep8m/schools-proliferating-without-evidence"
     "https://www.greaterwrong.com/posts/5K7CMa6dEL7TN7sae/3-levels-of-rationality-verification"
     "https://www.greaterwrong.com/posts/7FzD7pNm9X68Gp5ZC/why-our-kind-can-t-cooperate"
     "https://www.greaterwrong.com/posts/JKxxFseBWz8SHkTgt/tolerate-tolerance"
     "https://www.greaterwrong.com/posts/cyzXoCv7nagDWCMNS/you-re-calling-who-a-cult-leader"
     "https://www.greaterwrong.com/posts/YC3ArwKM8xhNjYqQK/on-things-that-are-awesome"
     "https://www.greaterwrong.com/posts/Q8evewZW5SeidLdbA/your-price-for-joining"
     "https://www.greaterwrong.com/posts/3fNL2ssfvRzpApvdN/can-humanism-match-religion-s-output"
     "https://www.greaterwrong.com/posts/p5DmraxDmhvMoZx8J/church-vs-taskforce"
     "https://www.greaterwrong.com/posts/4PPE6D635iBcGPGRy/rationality-common-interest-of-many-causes"
     "https://www.greaterwrong.com/posts/f42BHX7rMw2dyFJfT/helpless-individuals"
     "https://www.greaterwrong.com/posts/ZpDnRCeef2CLEFeKM/money-the-unit-of-caring"
     "https://www.greaterwrong.com/posts/3p3CYauiX8oLjmwRF/purchase-fuzzies-and-utilons-separately"
     "https://www.greaterwrong.com/posts/ZEj9ATpv3P22LSmnC/selecting-rationalist-groups"
     "https://www.greaterwrong.com/posts/oZNXmHcdhb4m7vwsv/incremental-progress-and-the-valley"
     "https://www.greaterwrong.com/posts/rg7vPTtyLMfT6Qqud/whining-based-communities"
     "https://www.greaterwrong.com/posts/gBewgmzcEiks2XdoQ/mandatory-secret-identities"
     "https://www.greaterwrong.com/posts/6NvbSwuSAooQxxf7f/beware-of-other-optimizing"
     "https://www.greaterwrong.com/posts/geqg9mk73NQh6uieE/akrasia-and-shangri-la"
     "https://www.greaterwrong.com/posts/NnQbfLo868wgnHF4n/collective-apathy-and-the-internet"
     "https://www.greaterwrong.com/posts/KsHmn6iJAEr9bACQW/bayesians-vs-barbarians"
     "https://www.greaterwrong.com/posts/xsyG7PkMekHud2DMK/of-gender-and-rationality"
     "https://www.greaterwrong.com/posts/FBgozHEv7J72NCEPB/my-way"
     "https://www.greaterwrong.com/posts/pkFazhcTErMw7TFtT/the-sin-of-underconfidence"
     "https://www.greaterwrong.com/posts/tscc3e5eujrsEeFN4/well-kept-gardens-die-by-pacifism"
     "https://www.greaterwrong.com/posts/LqjKP255fPRY7aMzw/practical-advice-backed-by-deep-theories"
     "https://www.greaterwrong.com/posts/W3LDwqHxiwKqWkWJi/less-meta"
     "https://www.greaterwrong.com/posts/aFEsqd6ofwnkNqaXo/go-forth-and-create-the-art"

     ;; Ethical Injunctions https://www.greaterwrong.com/s/AmFb5xWbPWWQyQ244

     "https://www.greaterwrong.com/posts/v8rghtzWCziYuMdJ5/why-does-power-corrupt"
     "https://www.greaterwrong.com/posts/K9ZaZXDnL3SEmYZqB/ends-don-t-justify-means-among-humans"
     "https://www.greaterwrong.com/posts/wyyfFfaRar2jEdeQK/entangled-truths-contagious-lies"
     "https://www.greaterwrong.com/posts/yz2btSaCLHmLWgWD5/protected-from-myself"
     "https://www.greaterwrong.com/posts/cyRpNbPsW8HzsxhRK/ethical-inhibitions"
     "https://www.greaterwrong.com/posts/dWTEtgBfFaz6vjwQf/ethical-injunctions"
     "https://www.greaterwrong.com/posts/K2c3dkKErsqFd28Dh/prices-or-bindings"
     "https://www.greaterwrong.com/posts/hXuB8BCyyiYuzij3F/ethics-notes"

     ;; Metaethics

     "https://www.greaterwrong.com/posts/RFnkagDaJSBLDXEHs/heading-toward-morality"
     "https://www.greaterwrong.com/posts/PtoQdG7E8MxYJrigu/no-universally-compelling-arguments"
     "https://www.greaterwrong.com/posts/eDpPnT7wdBwWPGvo5/2-place-and-1-place-words"
     "https://www.greaterwrong.com/posts/iGH7FSrdoCXa5AHGs/what-would-you-do-without-morality"
     "https://www.greaterwrong.com/posts/K9JSM7d7bLJguMxEp/the-moral-void"
     "https://www.greaterwrong.com/posts/CuSTqHgeK4CMpWYTe/created-already-in-motion"
     "https://www.greaterwrong.com/posts/iAxkfiyG8WizPSPbq/the-bedrock-of-fairness"
     "https://www.greaterwrong.com/posts/SbdCX6A5AGyyfhdmh/moral-complexities"
     "https://www.greaterwrong.com/posts/F5WLc7hCxkB4X4yD4/is-morality-preference"
     "https://www.greaterwrong.com/posts/iQNKfYb7aRYopojTX/is-morality-given"
     "https://www.greaterwrong.com/posts/C8nEXTcjZb9oauTCW/where-recursive-justification-hits-bottom"
     "https://www.greaterwrong.com/posts/TynBiYt6zg42StRbb/my-kind-of-reflection"
     "https://www.greaterwrong.com/posts/KZLa74SzyKhSJ3M55/the-genetic-fallacy"
     "https://www.greaterwrong.com/posts/9EahWKqay6HZcaNTY/fundamental-doubts"
     "https://www.greaterwrong.com/posts/YhNGY6ypoNbLJvDBu/rebelling-within-nature"
     "https://www.greaterwrong.com/posts/XhaKvQyHzeXdNnFKy/probability-is-subjectively-objective"
     "https://www.greaterwrong.com/posts/szAkYJDtXkcSAiHYE/whither-moral-progress"
     "https://www.greaterwrong.com/posts/pGvyqAQw6yqTjpKf4/the-gift-we-give-to-tomorrow"
     "https://www.greaterwrong.com/posts/vy9nnPdwTjSmt5qdb/could-anything-be-right"
     "https://www.greaterwrong.com/posts/8rdoea3g6QGhWQtmx/existential-angst-factory"
     "https://www.greaterwrong.com/posts/dhGGnB2oxBP3m5cBc/can-counterfactuals-be-true"
     "https://www.greaterwrong.com/posts/WAQ3qMD4vdXheQmui/math-is-subjunctively-objective"
     "https://www.greaterwrong.com/posts/GAR8gT3d9uCtr4kv8/does-your-morality-care-what-you-think"
     "https://www.greaterwrong.com/posts/LhP2zGBWR5AdssrdJ/changing-your-metaethics"
     "https://www.greaterwrong.com/posts/T7tYmfD9j25uLwqYk/setting-up-metaethics"
     "https://www.greaterwrong.com/posts/fG3g3764tSubr6xvs/the-meaning-of-right"
     "https://www.greaterwrong.com/posts/7HDtecu4qW9PCsSR6/interpersonal-morality"
     "https://www.greaterwrong.com/posts/FnJPa8E9ZG5xiLLp5/morality-as-fixed-computation"
     "https://www.greaterwrong.com/posts/JynJ6xfnpq9oN3zpb/inseparably-right-or-joy-in-the-merely-good"
     "https://www.greaterwrong.com/posts/mMBTPTjRbsrqbSkZE/sorting-pebbles-into-correct-heaps"
     "https://www.greaterwrong.com/posts/BkkwXtaTf5LvbA6HB/moral-error-and-moral-disagreement"
     "https://www.greaterwrong.com/posts/9KacKm5yBv27rxWnJ/abstracted-idealized-dynamics"
     "https://www.greaterwrong.com/posts/HacgrDxJx3Xr7uwCR/arbitrary"
     "https://www.greaterwrong.com/posts/saw8WAML4NEaJ2Wmz/is-fairness-arbitrary"
     "https://www.greaterwrong.com/posts/RBszS2jwGM4oghXW4/the-bedrock-of-morality-arbitrary"
     "https://www.greaterwrong.com/posts/rm8tv9qZ9nwQxhshx/you-provably-can-t-trust-yourself"
     "https://www.greaterwrong.com/posts/YrhT7YxkRJoRnr7qD/no-license-to-be-human"
     "https://www.greaterwrong.com/posts/sCs48JtMnQwQsZwyN/invisible-frameworks"

     ;; Quantum Physics

     "https://www.greaterwrong.com/posts/f6ZLxEWaankRZ2Crv/probability-is-in-the-mind"
     "https://www.greaterwrong.com/posts/tPqQdLCuxanjhoaNs/reductionism"
     "https://www.greaterwrong.com/posts/fdEWWr8St59bXLbQr/zombies-zombies"
     "https://www.greaterwrong.com/posts/3XMwPNMSbaPm2suGz/belief-in-the-implied-invisible"
     "https://www.greaterwrong.com/posts/7FSwbFpDsca7uXpQ2/quantum-explanations"
     "https://www.greaterwrong.com/posts/5vZD32EynD9n94dhr/configurations-and-amplitude"
     "https://www.greaterwrong.com/posts/ybusFwDqiZgQa6NCq/joint-configurations"
     "https://www.greaterwrong.com/posts/KbeHkLNY5ETJ3TN3W/distinct-configurations"
     "https://www.greaterwrong.com/posts/Bh9cdfMjATrTdLrGH/where-philosophy-meets-science"
     "https://www.greaterwrong.com/posts/Bp8vnEciPA5TXSy6f/can-you-prove-two-particles-are-identical"
     "https://www.greaterwrong.com/posts/KAHt3t7a6KH4kfX4L/classical-configuration-spaces"
     "https://www.greaterwrong.com/posts/eHeJxJZii6tqQupZL/the-quantum-arena"
     "https://www.greaterwrong.com/posts/oiu7YhzrDTvCxMhdS/feynman-paths"
     "https://www.greaterwrong.com/posts/Cpf2jsZsNFNH5TSpc/no-individual-particles"
     "https://www.greaterwrong.com/posts/RLScTpwc5W2gGGrL9/identity-isn-t-in-specific-atoms"
     "https://www.greaterwrong.com/posts/JrhoMTgMrMRJJiS48/decoherence"
     "https://www.greaterwrong.com/posts/eWuuznxeebcjWpdnH/the-so-called-heisenberg-uncertainty-principle"
     "https://www.greaterwrong.com/posts/XDkeuJTFjM9Y2x6v6/which-basis-is-more-fundamental"
     "https://www.greaterwrong.com/posts/WajiC3YWeJutyAXTn/where-physics-meets-experience"
     "https://www.greaterwrong.com/posts/vGbHKfgFNDeJohfeN/where-experience-confuses-physicists"
     "https://www.greaterwrong.com/posts/pRrksC5Y6TbyvKDJE/on-being-decoherent"
     "https://www.greaterwrong.com/posts/nso8WXdjHLLHkJKhr/the-conscious-sorites-paradox"
     "https://www.greaterwrong.com/posts/aWFwfk3MBEyR4Ne8C/decoherence-is-pointless"
     "https://www.greaterwrong.com/posts/HwMfEcmxyM3eqqfvi/decoherent-essences"
     "https://www.greaterwrong.com/posts/3ZKvf9u2XEWddGZmS/the-born-probabilities"
     "https://www.greaterwrong.com/posts/EneHGx8t8skPKxHhv/decoherence-as-projection"
     "https://www.greaterwrong.com/posts/GmFuZcE6udo7bykxP/entangled-photons"
     "https://www.greaterwrong.com/posts/AnHJX42C6r6deohTG/bell-s-theorem-no-epr-reality"
     "https://www.greaterwrong.com/posts/DY9h6zxq6EMHrkkxE/spooky-action-at-a-distance-the-no-communication-theorem"
     "https://www.greaterwrong.com/posts/Atu4teGvob5vKvEAF/decoherence-is-simple"
     "https://www.greaterwrong.com/posts/DFxoaWGEh9ndwtZhk/decoherence-is-falsifiable-and-testable"
     "https://www.greaterwrong.com/posts/k3823vuarnmL5Pqin/quantum-non-realism"
     "https://www.greaterwrong.com/posts/xsZnufn3cQw7tJeQ3/collapse-postulates"
     "https://www.greaterwrong.com/posts/WqGCaRhib42dhKWRL/if-many-worlds-had-come-first"
     "https://www.greaterwrong.com/posts/S8ysHqeRGuySPttrS/many-worlds-one-best-guess"
     "https://www.greaterwrong.com/posts/qcYCAxYZT4Xp9iMZY/living-in-many-worlds"
     "https://www.greaterwrong.com/posts/NsgcZx4BeTy5y84Ya/mach-s-principle-anti-epiphenomenal-physics"
     "https://www.greaterwrong.com/posts/vLZtf64wkyoAFNcu3/relative-configuration-space"
     "https://www.greaterwrong.com/posts/rrW7yf42vQYDf8AcH/timeless-physics"
     "https://www.greaterwrong.com/posts/GKTe9bCxFSE6EXEEu/timeless-beauty"
     "https://www.greaterwrong.com/posts/KipiHsTA3pw4joQkG/timeless-causality"
     "https://www.greaterwrong.com/posts/924arDrTu3QRHFA5r/timeless-identity"
     "https://www.greaterwrong.com/posts/NEeW7eSXThPz7o4Ne/thou-art-physics"
     "https://www.greaterwrong.com/posts/YYLmZFEGKsjCKQZut/timeless-control"
     "https://www.greaterwrong.com/posts/ZxR8P8hBFQ9kC8wMy/the-failures-of-eld-science"
     "https://www.greaterwrong.com/posts/viPPjojmChxLGPE2v/the-dilemma-science-or-bayes"
     "https://www.greaterwrong.com/posts/5bJyRMZzwMov5u3hW/science-doesn-t-trust-your-rationality"
     "https://www.greaterwrong.com/posts/wzxneh7wxkdNYNbtB/when-science-can-t-help"
     "https://www.greaterwrong.com/posts/PGfJdgemDJSwWBZSX/science-isn-t-strict-enough"
     "https://www.greaterwrong.com/posts/WijMw9WkcafmCFgj4/do-scientists-already-know-this-stuff"
     "https://www.greaterwrong.com/posts/wustx45CPL5rZenuo/no-safe-defense-not-even-science"
     "https://www.greaterwrong.com/posts/SoDsr8GEZmRKMZNkj/changing-the-definition-of-science"
     "https://www.greaterwrong.com/posts/xTyuQ3cgsPjifr7oj/faster-than-science"
     "https://www.greaterwrong.com/posts/mpaqTWGiLT7GA3w76/einstein-s-speed"
     "https://www.greaterwrong.com/posts/5wMcKNAwB6X4mp9og/that-alien-message"
     "https://www.greaterwrong.com/posts/3Jpchgy53D2gB5qdk/my-childhood-role-model"
     "https://www.greaterwrong.com/posts/5o4EZJyqmHY4XgRCY/einstein-s-superpowers"
     "https://www.greaterwrong.com/posts/xAXrEpF5FYjwqKMfZ/class-project"
     "https://www.greaterwrong.com/posts/gDL9NDEXPxYpDf4vz/why-quantum"

     ;; Positivism, Self Deception and Neuroscience

     "https://www.greaterwrong.com/posts/P3uavjFmZD5RopJKk/simultaneously-right-and-wrong"
     "https://www.greaterwrong.com/posts/mja6jZ6k9gAwki9Nu/the-mystery-of-the-haunted-rationalist"
     "https://www.greaterwrong.com/posts/ZiQqsgGX6a42Sfpii/the-apologist-and-the-revolutionary"
     "https://www.greaterwrong.com/posts/Cq45AuedYnzekp3LX/you-may-already-be-a-sinner"
     "https://www.greaterwrong.com/posts/atcJqdhCxTZiJSxo2/talking-snakes-a-cautionary-tale"
     "https://www.greaterwrong.com/posts/M7rwT264CSYY6EdR3/the-skeptic-s-trilemma"
     "https://www.greaterwrong.com/posts/T5McDuWDeCvDZKeSj/are-you-a-solar-deity"
     "https://www.greaterwrong.com/posts/KLjQedNYNEP4tW73W/the-spot-the-fakes-test"
     "https://www.greaterwrong.com/posts/6yTShbTdtATxKonY5/how-to-not-lose-an-argument"
     "https://www.greaterwrong.com/posts/azoP7WeKYYfgCozoh/the-power-of-positivist-thinking"
     "https://www.greaterwrong.com/posts/9hR2RmpJmxT8dyPo4/when-truth-isn-t-enough"

     ;; Argument and Analysis (Scott) https://www.greaterwrong.com/s/XsMTxdQ6fprAQMoKi

     "https://www.greaterwrong.com/posts/gFMH3Cqw4XxwL69iy/eight-short-studies-on-excuses"
     "https://www.greaterwrong.com/posts/Kbm6QnJv9dgWsPHQP/schelling-fences-on-slippery-slopes"
     "https://www.greaterwrong.com/posts/9kcTNWopvXFncXgPy/intellectual-hipsters-and-meta-contrarianism"
     "https://www.greaterwrong.com/posts/DSzpr8Y9299jdDLc9/cardiologists-and-chinese-robbers"
     "https://www.greaterwrong.com/posts/PQ3nutgxfTgvq69Xt/all-debates-are-bravery-debates"
     "https://www.greaterwrong.com/posts/2brqzQWfmNx5Agdrx/the-virtue-of-silence"
     "https://www.greaterwrong.com/posts/G5eMM3Wp3hbCuKKPE/proving-too-much"
     "https://www.greaterwrong.com/posts/fzeoYhKoYPR3tDYFT/beware-isolated-demands-for-rigor"
     "https://www.greaterwrong.com/posts/AYbhqi65SWzHzy7Xx/transhumanist-fables"
     "https://www.greaterwrong.com/posts/wJnm5cBiZGmKn595f/and-i-show-you-how-deep-the-rabbit-hole-goes"

     ;; LW Political Prereqs https://www.greaterwrong.com/s/ZnSMHcWjRx6yT4H92

     "https://www.greaterwrong.com/posts/9weLK2AJ9JEt2Tt8f/politics-is-the-mind-killer"
     "https://www.greaterwrong.com/posts/PeSzc9JTBxhaYRp9b/policy-debates-should-not-appear-one-sided"
     "https://www.greaterwrong.com/posts/XYCEB9roxEBfgjfxs/the-scales-of-justice-the-notebook-of-rationality"
     "https://www.greaterwrong.com/posts/DB6wbyrMugYMK5o6a/correspondence-bias"
     "https://www.greaterwrong.com/posts/28bAMAxhoX3bwbAKC/are-your-enemies-innately-evil"
     "https://www.greaterwrong.com/posts/qNZM3EGoE5ZeMdCRt/reversed-stupidity-is-not-intelligence"
     "https://www.greaterwrong.com/posts/5yFRd3cjLpm3Nd6Di/argument-screens-off-authority"
     "https://www.greaterwrong.com/posts/2jp98zdLo898qExrr/hug-the-query"
     "https://www.greaterwrong.com/posts/Lz64L3yJEtYGkzMzu/rationality-and-the-english-language"
     "https://www.greaterwrong.com/posts/i8q4vXestDkGTFwsc/human-evil-and-muddled-thinking"
     "https://www.greaterwrong.com/posts/jxfu7CTc3NidinuXD/politics-is-hard-mode"
     "https://www.greaterwrong.com/posts/GLMFmFvXGyAcG25ni/i-can-tolerate-anything-except-the-outgroup"

     ;; Intro to Naturalism (Brienne) https://www.greaterwrong.com/s/evLkoqsbi79AnM5sz

     "https://www.greaterwrong.com/posts/pPWiLGsWCtN92vLwu/intro-to-naturalism-orientation"
     "https://www.greaterwrong.com/posts/S8kAJamj66gekjBwC/knowing"
     "https://www.greaterwrong.com/posts/bRcsFM6jm272ELyx8/the-territory"
     "https://www.greaterwrong.com/posts/wY7vKT2PiNyG8w2Ep/interlude-on-realness"
     "https://www.greaterwrong.com/posts/6EyrTDGgfsk8TCbMf/observation"
     "https://www.greaterwrong.com/posts/y7wgtZ2ehfWuqzXfm/direct-observation"
     "https://www.greaterwrong.com/posts/sAiHxHkQrsYsRpKFP/patient-observation"
     "https://www.greaterwrong.com/posts/x9DEmdQa5brftFfpX/naturalism"

     ;; Replacing Guilt (Nate) https://www.greaterwrong.com/s/pFatcKW3JJhTSxqAF

     "https://www.greaterwrong.com/posts/ijYCZSQvgNeaQqcHN/half-assing-it-with-everything-you-ve-got"
     "https://www.greaterwrong.com/posts/HpHnMEiHYzSZiYk6g/failing-with-abandon"
     "https://www.greaterwrong.com/posts/C5Qucpm3QXM7fMqMJ/replacing-guilt"
     "https://www.greaterwrong.com/posts/AxTJuFSPdfhACJCea/the-stamp-collector"
     "https://www.greaterwrong.com/posts/8uk5bmmpJaSAgnZfg/you-re-allowed-to-fight-for-something"
     "https://www.greaterwrong.com/posts/Ag9yqE8WkLsCceaJS/caring-about-something-larger-than-yourself"
     "https://www.greaterwrong.com/posts/sx9pbNtBxqooiSg6A/you-don-t-get-to-know-what-you-re-fighting-for"
     "https://www.greaterwrong.com/posts/HqQ3CpMqQyaaLLKew/should-considered-harmful"
     "https://www.greaterwrong.com/posts/FPQuv8TwTLPLsrJCk/not-because-you-should"
     "https://www.greaterwrong.com/posts/jvByGYcBzfYB78sYL/your-shoulds-are-not-a-duty"
     "https://www.greaterwrong.com/posts/grfDjRpWr2xjtLBvi/working-yourself-ragged-is-not-a-virtue"
     "https://www.greaterwrong.com/posts/Kdgpm7t9gZvWncZqa/rest-in-motion"
     "https://www.greaterwrong.com/posts/CAWHpzaZGJZMhK6pb/shifting-guilt"
     "https://www.greaterwrong.com/posts/sG4paay6CeGbyYZZo/don-t-steer-with-guilt"
     "https://www.greaterwrong.com/posts/uGsALatTCgkkqijaw/update-from-the-suckerpunch"
     "https://www.greaterwrong.com/posts/KGoNQZAnmfd4oDtfY/be-a-new-homunculus"
     "https://www.greaterwrong.com/posts/ZegT37QwLLRACtZjy/not-yet-gods"
     "https://www.greaterwrong.com/posts/DwApBCz7LnB28mqrM/where-coulds-go"
     "https://www.greaterwrong.com/posts/euPT8ddxxpDpKZmmS/self-compassion"
     "https://www.greaterwrong.com/posts/2agT7asiBZJqfqKgH/there-are-no-bad-people"
     "https://www.greaterwrong.com/posts/ssP4LpD4wFj4H8yee/residing-in-the-mortal-realm"
     "https://www.greaterwrong.com/posts/TzpjnLf3oyEegmu9r/being-unable-to-despair"
     "https://www.greaterwrong.com/posts/wFjT6zEsnE9nCJC8L/see-the-dark-world"
     "https://www.greaterwrong.com/posts/7mTFJ65CuhjiWz8tv/choose-without-suffering"
     "https://www.greaterwrong.com/posts/jbizoDfWhqjdjhzct/detach-the-grim-o-meter"
     "https://www.greaterwrong.com/posts/7jp9iSywDj4D8epJi/simply-locate-yourself"
     "https://www.greaterwrong.com/posts/zhEmiCBoHNGxCtXsc/have-no-excuses"
     "https://www.greaterwrong.com/posts/L5ighGWYwJv6Jyr6G/come-to-your-terms"
     "https://www.greaterwrong.com/posts/zxTGg5AbGSNqkXHxH/transmute-guilt-into-resolve"
     "https://www.greaterwrong.com/posts/N5oXNbmtYG3kC2XJr/the-best-you-can"
     "https://www.greaterwrong.com/posts/YSGRdB5aD6xEMcWzQ/dark-not-colorless"
     "https://www.greaterwrong.com/posts/2rXiZFCxBcJzM9TMp/stop-trying-to-try-and-try"
     "https://www.greaterwrong.com/posts/C82z3KrtKWDTjhidv/there-is-no-try"
     "https://www.greaterwrong.com/posts/vYzLuRndnMACHM9HZ/obvious-advice"
     "https://www.greaterwrong.com/posts/ZJHDDM8eWyXvMB924/the-art-of-response"
     "https://www.greaterwrong.com/posts/jFvPWZB5WAGkDBpqN/confidence-all-the-way-up"
     "https://www.greaterwrong.com/posts/b2gkf98FZNh8EvHW9/desperation"
     "https://www.greaterwrong.com/posts/F73oLhauzi6fJaYHo/recklessness"
     "https://www.greaterwrong.com/posts/po2xSrF25uvKBkDay/defiance"
     "https://www.greaterwrong.com/posts/9GumD8cbnXdLsxFMM/how-we-will-be-measured"
     "https://www.greaterwrong.com/posts/ur9TCRnHJighHmLCW/on-caring"
     "https://www.greaterwrong.com/posts/dpMZHpA59xFFjCqBp/the-value-of-a-life"
     "https://www.greaterwrong.com/posts/jfXHYnreYJvrjDQsj/moving-towards-the-goal"
     "https://www.greaterwrong.com/posts/rWo2LmgQAvZDaa75z/self-signaling-the-ability-to-do-what-you-want"
     "https://www.greaterwrong.com/posts/TqepFWjxWdZvQnkJr/productivity-through-self-loyalty"
     "https://www.greaterwrong.com/posts/kidpjsxCwfsgH4hQK/conclusion-of-the-replacing-guilt-series"

     ;; CFAR Handbook https://www.greaterwrong.com/s/KAv8z6oJCTxjR8vdR

     "https://www.greaterwrong.com/posts/dbDHEQyKqnMDDqq2G/cfar-handbook-introduction"
     "https://www.greaterwrong.com/posts/gKeHcikcXA3bApyoM/opening-session-tips-and-advice"
     "https://www.greaterwrong.com/posts/LaYB3CPMnDNzh2via/building-a-bugs-list-prompts"
     "https://www.greaterwrong.com/posts/CsNtMunxsZnvumYLe/seeking-pck-pedagogical-content-knowledge"
     "https://www.greaterwrong.com/posts/cvDtmPNCyrkpg4d4F/units-of-exchange"
     "https://www.greaterwrong.com/posts/Htjbj8ystqc2zLtMX/murphyjitsu-an-inner-simulator-algorithm"
     "https://www.greaterwrong.com/posts/W5HcGywyPoDDdJtbz/trigger-action-planning"
     "https://www.greaterwrong.com/posts/ZHWiCM4QmX8WwYajH/goal-factoring-1"
     "https://www.greaterwrong.com/posts/dvzwqLbpSK2nD8Yvn/aversion-factoring"
     "https://www.greaterwrong.com/posts/3Jggtk5otfDoL7Lpq/turbocharging"
     "https://www.greaterwrong.com/posts/wanmjz6wkTCNtyKLX/taste-and-shaping"
     "https://www.greaterwrong.com/posts/hBKnkx8xvmhGZPdmg/goodhart-s-imperius-1"
     "https://www.greaterwrong.com/posts/QKpmPBpyCdNbBKAdR/systemization"
     "https://www.greaterwrong.com/posts/QnZRvud7fXfpdxBY3/againstness"
     "https://www.greaterwrong.com/posts/ifa45nkAZiF5Msubi/comfort-zone-exploration"
     "https://www.greaterwrong.com/posts/jiJquD34sa9Lyo5wc/resolve-cycles"
     "https://www.greaterwrong.com/posts/f3o9ydY7iPjFF2fyk/focusing-1"
     "https://www.greaterwrong.com/posts/x2KrcscqgKDk6pMeD/internal-double-crux-1"
     "https://www.greaterwrong.com/posts/WLQspe83ZkiwBc2SR/double-crux"
     "https://www.greaterwrong.com/posts/WFEt8QHGrnSQzPa9W/bucket-errors"
     "https://www.greaterwrong.com/posts/afEoXzGB4yt4Hk8zy/polaris-five-second-versions-and-thought-lengths"
     "https://www.greaterwrong.com/posts/CJGKkTjWLGCwhkjGY/socratic-ducking-ooda-loops-frame-by-frame-debugging"
     "https://www.greaterwrong.com/posts/T8piFGywHFd4ax9yx/gears-level-understanding-deliberate-performance-the"
     "https://www.greaterwrong.com/posts/ZCXjmeHNHYiM2uCw4/area-under-the-curve-eat-dirt-broccoli-errors-copernicus-and"
     "https://www.greaterwrong.com/posts/8FsLyk6oDrhoqWEjB/pendulums-policy-level-decisionmaking-saving-state"
     "https://www.greaterwrong.com/posts/RafYaSYQ2uLmYgwhk/appendix-hamming-questions"
     "https://www.greaterwrong.com/posts/fbv9FWss6ScDMJiAx/appendix-jargon-dictionary"
     "https://www.greaterwrong.com/posts/xAqRkrnjJTMZ6YmZA/appendix-how-to-run-a-successful-hamming-circle"

     ;; Gears Which Turn The World (johnswentworth) https://www.greaterwrong.com/s/xEFeCwk3pdYdeG2rL

     "https://www.greaterwrong.com/posts/gvK5QWRLk3H8iqcNy/gears-vs-behavior"
     "https://www.greaterwrong.com/posts/JJv8jmLYzYzdYkS3c/technology-changes-constraints"
     "https://www.greaterwrong.com/posts/DMopaHKS79aGMmLkA/constraints-and-slackness-as-a-worldview-generator"
     "https://www.greaterwrong.com/posts/eWxizGwgpNhckrzfc/material-goods-as-an-abundant-resource"
     "https://www.greaterwrong.com/posts/P6fSj3t4oApQQTB7E/coordination-as-a-scarce-resource"
     "https://www.greaterwrong.com/posts/hqSuo8ERcPw6nre2o/theory-and-data-as-constraints"
     "https://www.greaterwrong.com/posts/4s2gbwMHSdh2SByyZ/transportation-as-a-constraint"
     "https://www.greaterwrong.com/posts/hyShz2ABiKX56j5tJ/interfaces-as-a-scarce-resource"
     "https://www.greaterwrong.com/posts/wEebEiPpEwjYvnyqq/when-money-is-abundant-knowledge-is-the-real-wealth"
     "https://www.greaterwrong.com/posts/YABJKJ3v97k9sbxwg/what-money-cannot-buy"
     "https://www.greaterwrong.com/posts/Mha5GA5BfWcpf2jHC/potential-bottlenecks-to-taking-over-the-world"

     ;; Immoral Mazes

     "https://www.greaterwrong.com/posts/45mNHCMaZgsvfDXbw/quotes-from-moral-mazes"
     "https://www.greaterwrong.com/posts/2Zsuv5uPFPNTACwzg/moral-mazes-and-short-termism"
     "https://www.greaterwrong.com/posts/TxcRbCYHaeL59aY7E/meditations-on-moloch"
     "https://www.greaterwrong.com/posts/MYXmFLZcsQWA9X2r4/meditation-retreat-immoral-mazes-sequence-introduction"
     "https://www.greaterwrong.com/posts/ham9i5wf4JCexXnkN/moloch-hasn-t-won"
     "https://www.greaterwrong.com/posts/gECsXTQPBKda9rrGe/perfect-competition"
     "https://www.greaterwrong.com/posts/4NrmDPrCAnwM3uiyB/imperfect-competition"
     "https://www.greaterwrong.com/posts/229FkbLrhat9JLhwQ/does-big-business-hate-your-family"
     "https://www.greaterwrong.com/posts/sdFS5TaxGB8eAoGxY/what-is-life-in-an-immoral-maze"
     "https://www.greaterwrong.com/posts/R75HFkHYB9FxxvtrA/stripping-away-the-protections"
     "https://www.greaterwrong.com/posts/bQ7BKK5efogcR9YqN/what-is-success-in-an-immoral-maze"
     "https://www.greaterwrong.com/posts/zpA2Tnp2k38qSmr8J/how-to-identify-an-immoral-maze"
     "https://www.greaterwrong.com/posts/a8wjKNSGCPSzdWMMa/how-to-escape-from-immoral-mazes"
     "https://www.greaterwrong.com/posts/KLdqetnxgprifo9BP/the-road-to-mazedom"
     "https://www.greaterwrong.com/posts/ubHeLGc73iDvP6cTN/how-doomed-are-large-organizations"
     "https://www.greaterwrong.com/posts/S84Tcb7oXZJoTPHCe/ten-causes-of-mazedom"
     "https://www.greaterwrong.com/posts/S2Tgbve2d3vpsNtrQ/potential-ways-to-fight-mazes"
     "https://www.greaterwrong.com/posts/NerqNs3nfAkcJauWE/create-a-full-alternative-stack"
     "https://www.greaterwrong.com/posts/4inoCWnKrpHt4gCx9/protecting-large-projects-against-mazedom"
     "https://www.greaterwrong.com/posts/xc9DbPPtpqQjLx4Wt/mazes-sequence-roundup-final-thoughts-and-paths-forward"

     ;; Keep your beliefs cruxy and your frames explicit https://www.greaterwrong.com/s/hBFDRZCPLcrRDubgm

     "https://www.greaterwrong.com/posts/i2dNdHrGMT6QgqBFy/what-product-are-you-building"
     "https://www.greaterwrong.com/posts/eWGkk6X9owZ3XwdeG/doublecrux-is-for-building-products"
     "https://www.greaterwrong.com/posts/BmyoYkr7u2oas4ukn/keep-your-beliefs-cruxy"
     "https://www.greaterwrong.com/posts/f886riNJcArmpFahm/noticing-frame-differences"
     "https://www.greaterwrong.com/posts/bhSjufKtnPRWqW52h/picture-frames-window-frames-and-frameworks-1"
     "https://www.greaterwrong.com/posts/W8vSrHAM9qoWdzFoP/karate-kid-and-realistic-expectations-for-disagreement"
     "https://www.greaterwrong.com/posts/YN6daWakNnkXEeznB/propagating-facts-into-aesthetics"

     ;; Fun Theory

     "https://www.greaterwrong.com/posts/pK4HTxuv6mftHXWC3/prolegomena-to-a-theory-of-fun"
     "https://www.greaterwrong.com/posts/29vqqmGNxNRGzffEj/high-challenge"
     "https://www.greaterwrong.com/posts/aEdqh3KPerBNYvoWe/complex-novelty"
     "https://www.greaterwrong.com/posts/QfpHRAMRM2HjteKFK/continuous-improvement"
     "https://www.greaterwrong.com/posts/eLHCWi8sotQT6CmTX/sensual-experience"
     "https://www.greaterwrong.com/posts/dKGfNvjGjq4rqffyF/living-by-your-own-strength"
     "https://www.greaterwrong.com/posts/EZ8GniEPSechjDYP9/free-to-optimize"
     "https://www.greaterwrong.com/posts/CtSS6SkHhLBvdodTY/harmful-options"
     "https://www.greaterwrong.com/posts/MTjej6HKvPByx3dEA/devil-s-offers"
     "https://www.greaterwrong.com/posts/wqDRRx9RqwKLzWt7R/nonperson-predicates"
     "https://www.greaterwrong.com/posts/vwnSPgwtmLjvTK2Wa/amputation-of-destiny"
     "https://www.greaterwrong.com/posts/HsRFQTAySAx8xbXEc/nonsentient-optimizers"
     "https://www.greaterwrong.com/posts/gb6zWstjmkYHLrbrg/can-t-unbirth-a-child"
     "https://www.greaterwrong.com/posts/W5PhyEQqEWTcpRpqn/dunbar-s-function"
     "https://www.greaterwrong.com/posts/WMDy4GxbyYkNrbmrs/in-praise-of-boredom"
     "https://www.greaterwrong.com/posts/NLMo5FZWFFq652MNe/sympathetic-minds"
     "https://www.greaterwrong.com/posts/Py3uGnncqXuEfPtQp/interpersonal-entanglement"
     "https://www.greaterwrong.com/posts/ctpkTaqTKbmm6uRgC/failed-utopia-4-2"
     "https://www.greaterwrong.com/posts/EQkELCGiGQwvrrp3L/growing-up-is-hard"
     "https://www.greaterwrong.com/posts/QZs4vkC7cbyjL9XA9/changing-emotions"
     "https://www.greaterwrong.com/posts/ZmDEbiEeXk3Wv2sLH/emotional-involvement"
     "https://www.greaterwrong.com/posts/6qS9q5zHafFXsB6hf/serious-stories"
     "https://www.greaterwrong.com/posts/hQSaMafoizBSa3gFR/eutopia-is-scary"
     "https://www.greaterwrong.com/posts/cWjK3SbRcLkb3gN69/building-weirdtopia"
     "https://www.greaterwrong.com/posts/DGXvLNpiSYBeQ6TLW/justified-expectation-of-pleasant-surprises"
     "https://www.greaterwrong.com/posts/88BpRQah9c2GWY3En/seduced-by-imagination"
     "https://www.greaterwrong.com/posts/DGXvLNpiSYBeQ6TLW/justified-expectation-of-pleasant-surprises"
     "https://www.greaterwrong.com/posts/88BpRQah9c2GWY3En/seduced-by-imagination"
     "https://www.greaterwrong.com/posts/4o3zwgofFPLutkqvd/the-uses-of-fun-theory"
     "https://www.greaterwrong.com/posts/5Pjq3mxuiXu2Ys3gM/higher-purpose"

     ;; Slack and the Sabbath

     "https://www.greaterwrong.com/posts/ENBzEkoyvdakz4w5d/out-to-get-you"
     "https://www.greaterwrong.com/posts/nJPtHHq6L7MAMBvRK/play-in-easy-mode"
     "https://www.greaterwrong.com/posts/7hLWZf6kFkduecH2g/play-in-hard-mode"
     "https://www.greaterwrong.com/posts/yLLkWMDbC9ZNKbjDG/slack"
     "https://www.greaterwrong.com/posts/p7hW7E3fHF3PDzErk/sabbath-hard-and-go-home"
     "https://www.greaterwrong.com/posts/ZoCitBiBv97WEWpX5/bring-back-the-sabbath"
     "https://www.greaterwrong.com/posts/t3o8ds7LjtgW9t7FJ/sabbath-commentary"
     "https://www.greaterwrong.com/posts/bhfPQqkq5dAsSz3qG/confessions-of-a-slacker"
     "https://www.greaterwrong.com/posts/TqvCmAqLmtqXpDQWf/slack-for-your-belief-system"
     "https://www.greaterwrong.com/posts/S9PbycbiN7h8qWYD6/sacred-cash"
     "https://www.greaterwrong.com/posts/MeWtcyX8wHxjpDAeE/guarding-slack-vs-substance"
     "https://www.greaterwrong.com/posts/36Dhz325MZNq3Cs6B/the-amish-and-strategic-norms-around-technology"
     "https://www.greaterwrong.com/posts/fwSDKTZvraSdmwFsj/slack-gives-you-space-to-notice-reflect-on-subtle-things"
     "https://www.greaterwrong.com/posts/WuyNHrDXcGFgkZpBy/my-slack-budget-3-surprise-problems-per-week"
     "https://www.greaterwrong.com/posts/3qX2GipDuCq5jstMG/slack-has-positive-externalities-for-groups"

     ;; Introduction to Game Theory

     "https://www.greaterwrong.com/posts/EhEZoTFzys9EDmEXn/backward-reasoning-over-decision-trees"
     "https://www.greaterwrong.com/posts/yJfBzcDL9fBHJfZ6P/nash-equilibria-and-schelling-points"
     "https://www.greaterwrong.com/posts/QdXrkWoK2Pp6XhNuQ/introduction-to-prisoners-dilemma"
     "https://www.greaterwrong.com/posts/BroeiXGh9PrKZEkJ5/real-world-solutions-to-prisoners-dilemmas"
     "https://www.greaterwrong.com/posts/Yy7mgec8tsbTAuTqb/interlude-for-behavioral-economics"
     "https://www.greaterwrong.com/posts/KheBaeW8Pi7LwewoF/what-is-signaling-really"
     "https://www.greaterwrong.com/posts/um7w5RogAHhxGy8Ti/bargaining-and-auctions"
     "https://www.greaterwrong.com/posts/xNBRkPNHAGQ6EQaLS/imperfect-voting-systems"
     "https://www.greaterwrong.com/posts/A2Qam9Bd9xpbb2wLQ/game-theory-as-a-dark-art"

     ;; The Blue-Minimizing Robot

     "https://www.greaterwrong.com/posts/hQHuXuRGZxxWXaPgg/the-blue-minimizing-robot"
     "https://www.greaterwrong.com/posts/EMJ3egz48BtZS8Pws/basics-of-animal-reinforcement"
     "https://www.greaterwrong.com/posts/BfaAADSQ88cuxLQoD/basics-of-human-reinforcement"
     "https://www.greaterwrong.com/posts/7iDtkfyn322nPzTP4/time-and-effort-discounting"
     "https://www.greaterwrong.com/posts/eaczwARbFnrisFx8E/wanting-vs-liking-revisited"
     "https://www.greaterwrong.com/posts/LQp9cZPzJncFKh5c8/prospect-theory-a-framework-for-understanding-cognitive"
     "https://www.greaterwrong.com/posts/5dhWhjfxn4tPfFQdi/physical-and-mental-behavior"
     "https://www.greaterwrong.com/posts/cTQRGJTQ2eGKm5G9g/voluntary-behavior-conscious-thoughts"
     "https://www.greaterwrong.com/posts/DSnamjnW7Ad8vEEKd/trivers-on-self-deception"
     "https://www.greaterwrong.com/posts/ePA4NDzZkunz98tLx/to-what-degree-do-we-have-goals"
     "https://www.greaterwrong.com/posts/K2JBqDeETX2yEgyyZ/the-limits-of-introspection"
     "https://www.greaterwrong.com/posts/ibk7q8msSYxZXmfCf/ego-syntonic-thoughts-and-values"
     "https://www.greaterwrong.com/posts/yDRX2fdkm3HqfTpav/approving-reinforces-low-effort-behaviors"
     "https://www.greaterwrong.com/posts/dymK5c7BkpgXH4acw/connectionism-modeling-the-mind-with-neural-networks"
     "https://www.greaterwrong.com/posts/WQWhXzALcrzrJtqRh/secrets-of-the-eliminati"
     "https://www.greaterwrong.com/posts/3wBj8BPquskZAbXu9/tendencies-in-reflective-equilibrium"

     ;; Babble and Prune

     "https://www.greaterwrong.com/posts/i42Dfoh4HtsCAfXxL/babble"
     "https://www.greaterwrong.com/posts/wQACBmK5bioNCgDoG/more-babble"
     "https://www.greaterwrong.com/posts/rYJKvagRYeDM8E9Rf/prune"
     "https://www.greaterwrong.com/posts/h4K6bsWrYHDcvvPtw/circumambulation"
     "https://www.greaterwrong.com/posts/eJiE7uuKZaP5HqLvY/write"

     ;; Highly Advanced Epistemology 101 for Beginners

     "https://www.greaterwrong.com/posts/XqvnWFtRD2keJdwjX/the-useful-idea-of-truth"
     "https://www.greaterwrong.com/posts/KJ9MFBPwXGwNpadf2/skill-the-map-is-not-the-territory"
     "https://www.greaterwrong.com/posts/HcCpvYLoSFP4iAqSz/rationality-appreciating-cognitive-algorithms"
     "https://www.greaterwrong.com/posts/WbLAA8qZQNdbRgKte/firewalling-the-optimal-from-the-rational"
     "https://www.greaterwrong.com/posts/h6fzC6wFYFxxKDm8u/the-fabric-of-real-things"
     "https://www.greaterwrong.com/posts/hzuSDMx7pd2uxFc5w/causal-diagrams-and-causal-models"
     "https://www.greaterwrong.com/posts/NhQju3htS9W6p6wE6/stuff-that-makes-stuff-happen"
     "https://www.greaterwrong.com/posts/PXoWk554FZ4Gpfvah/causal-reference"
     "https://www.greaterwrong.com/posts/o5F2p3krzT4JgzqQc/causal-universes"
     "https://www.greaterwrong.com/posts/Z2CuyKtkCmWGQtAEh/proofs-implications-and-models"
     "https://www.greaterwrong.com/posts/3FoMuCLqZggTxoC3S/logical-pinpointing"
     "https://www.greaterwrong.com/posts/i7oNcHR3ZSnEAM29X/standard-and-nonstandard-numbers"
     "https://www.greaterwrong.com/posts/GZjGtd35vhCnzSQKy/godel-s-completeness-and-incompleteness-theorems"
     "https://www.greaterwrong.com/posts/SWn4rqdycu83ikfBa/second-order-logic-the-controversy"
     "https://www.greaterwrong.com/posts/bTsiPnFndZeqTnWpu/mixed-reference-the-great-reductionist-project"
     "https://www.greaterwrong.com/posts/zqwWicCLNBSA5Ssmn/by-which-it-may-be-judged"

     ;; Rationality and Philosophy

     "https://www.greaterwrong.com/posts/oTX2LXHqXqYg2u4g6/less-wrong-rationality-and-mainstream-philosophy"
     "https://www.greaterwrong.com/posts/FwiPfF8Woe5JrzqEu/philosophy-a-diseased-discipline"
     "https://www.greaterwrong.com/posts/du395YvCnQXBPSJax/how-you-make-judgments-the-elephant-and-its-rider"
     "https://www.greaterwrong.com/posts/WTS4ZbEwvKrcrnaaN/your-evolved-intuitions"
     "https://www.greaterwrong.com/posts/6Cc3TWZjAnrNWokWY/intuition-and-unconscious-learning"
     "https://www.greaterwrong.com/posts/myLSqHNgi6BumABvE/when-intuitions-are-useful"
     "https://www.greaterwrong.com/posts/wHjpCxeDeuFadG3jF/concepts-don-t-work-that-way"
     "https://www.greaterwrong.com/posts/nEA5vYzYS5zqrtQTm/living-metaphorically"
     "https://www.greaterwrong.com/posts/jaN4EKrRnZdynTJjH/intuitions-aren-t-shared-that-way"
     "https://www.greaterwrong.com/posts/2pdyL8bSGBfYsnkyS/philosophy-needs-to-trust-your-rationality-even-though-it"
     "https://www.greaterwrong.com/posts/LcEzxX2FNTKbB6KXS/train-philosophers-with-pearl-and-kahneman-not-plato-and"

     ;; Decision Theory: Newcomb’s Problem

     "https://www.greaterwrong.com/posts/sLxFqs8fdjsPdkpLC/decision-theory-an-outline-of-some-upcoming-posts"
     "https://www.greaterwrong.com/posts/B7bMmhvaufdtxBtLW/confusion-about-newcomb-is-confusion-about-counterfactuals"
     "https://www.greaterwrong.com/posts/gxxpK3eiSQ3XG3DW7/decision-theory-why-we-need-to-reduce-could-would-should"
     "https://www.greaterwrong.com/posts/miwf7qQTh2HXNnSuq/decision-theory-why-pearl-helps-reduce-could-and-would-but"

     ;; The Science of Winning at Life

     "https://www.greaterwrong.com/posts/33KewgYhNSxFpbpXg/scientific-self-help-the-state-of-our-knowledge"
     "https://www.greaterwrong.com/posts/RWo4LwFzpHNQCTcYt/how-to-beat-procrastination"
     "https://www.greaterwrong.com/posts/Ty2tjPwv8uyPK9vrz/my-algorithm-for-beating-procrastination"
     "https://www.greaterwrong.com/posts/ZbgCx2ntD5eu8Cno9/how-to-be-happy"
     "https://www.greaterwrong.com/posts/Q5CjE8pRiACqTvhRM/the-good-news-of-situationist-psychology"
     "https://www.greaterwrong.com/posts/GGn8MBiY8Xz6NdNdH/the-power-of-reinforcement"
     "https://www.greaterwrong.com/posts/JYckkCqhZPrdScjBx/rational-romantic-relationships-part-1-relationship-styles"

     ;; No-Nonsense Metaethics

     "https://www.greaterwrong.com/posts/SFnfhJkGsBQk8jakK/heading-toward-no-nonsense-metaethics"
     "https://www.greaterwrong.com/posts/s4Mcg9aLMeRwdW7fh/what-is-metaethics"
     "https://www.greaterwrong.com/posts/2YPbdHgcjt7g5ZaFN/conceptual-analysis-and-moral-theory"
     "https://www.greaterwrong.com/posts/3zDX3f3QTepNeZHGc/pluralistic-moral-reductionism"
     "https://www.greaterwrong.com/posts/FZ5aTJFXZMpPL7ycK/quick-thoughts-on-empathic-metaethics"

     ;; Inadequate Equilibria

     "https://www.greaterwrong.com/posts/zsG9yKcriht2doRhM/inadequacy-and-modesty"
     "https://www.greaterwrong.com/posts/yPLr2tnXbiFXkMWvk/an-equilibrium-of-no-free-energy"
     "https://www.greaterwrong.com/posts/x5ASTMPKPowLKpLpZ/moloch-s-toolbox-1-2"
     "https://www.greaterwrong.com/posts/PRAyQaiMWg2La7XQy/moloch-s-toolbox-2-2"
     "https://www.greaterwrong.com/posts/pRibkeqBa2AxrpgT6/living-in-an-inadequate-world"
     "https://www.greaterwrong.com/posts/6n9aKApfLre5WWvpG/blind-empiricism"
     "https://www.greaterwrong.com/posts/svoD5KLKHyAKEdwPo/against-modest-epistemology"
     "https://www.greaterwrong.com/posts/o28fkhcZsBhhgfGjx/status-regulation-and-anxious-underconfidence"
     "https://www.greaterwrong.com/posts/NtyBaxQShNFm92xnS/against-shooting-yourself-in-the-foot"

     ;; Cartesian Frames

     "https://www.greaterwrong.com/posts/BSpdshJWGAW6TuNzZ/introduction-to-cartesian-frames"
     "https://www.greaterwrong.com/posts/ewkYgtZapQRtDPT2F/additive-operations-on-cartesian-frames"
     "https://www.greaterwrong.com/posts/pWruFSY7494vnucCE/biextensional-equivalence"
     "https://www.greaterwrong.com/posts/z3S2xnoDYfohrQQoe/controllables-and-observables-revisited"
     "https://www.greaterwrong.com/posts/GYQwJsChoRosjdW2r/functors-and-coarse-worlds"
     "https://www.greaterwrong.com/posts/nwrkwTd6uKBesYYfx/subagents-of-cartesian-frames"
     "https://www.greaterwrong.com/posts/srTD8DgTCR27udzoe/multiplicative-operations-on-cartesian-frames"
     "https://www.greaterwrong.com/posts/LAHXvi4qwXogmdTHd/sub-sums-and-sub-tensors-1"
     "https://www.greaterwrong.com/posts/hxbjEgjNTSbdXqDFE/additive-and-multiplicative-subagents"
     "https://www.greaterwrong.com/posts/5HMqSGQ9ad9r9Hibw/committing-assuming-externalizing-and-internalizing"
     "https://www.greaterwrong.com/posts/5R9dRqTREZriN9iL7/eight-definitions-of-observability"
     "https://www.greaterwrong.com/posts/JTzLjARpevuNpGPZm/time-in-cartesian-frames"
     "https://www.greaterwrong.com/posts/MnWQ4o7Y4HryE5ffN/cartesian-frames-and-factored-sets-on-arxiv"

     ;; Living Luminously

     "https://www.greaterwrong.com/posts/L4GGomr86sEwxzPvS/sorting-out-sticky-brains"
     "https://www.greaterwrong.com/posts/SRaRHemkbsHWzzbPN/mental-crystallography"
     "https://www.greaterwrong.com/posts/baTWMegR42PAsH9qJ/generalizing-from-one-example"
     "https://www.greaterwrong.com/posts/9o3Cjjem7AbmmZfBs/living-luminously"
     "https://www.greaterwrong.com/posts/r6diXRLvkZBLpSoTf/you-are-likely-to-be-eaten-by-a-grue"
     "https://www.greaterwrong.com/posts/Y6TpEEKZq6HXfhWxd/let-there-be-light"
     "https://www.greaterwrong.com/posts/rLuZ6XrGpgjk9BNpX/the-abc-s-of-luminosity"
     "https://www.greaterwrong.com/posts/v4ngP587MDZ5rC48Y/lights-camera-action"
     "https://www.greaterwrong.com/posts/Zstm38omrpeu7iWeS/the-spotlight"
     "https://www.greaterwrong.com/posts/tCTmAmAapB37dAz9Y/highlights-and-shadows"
     "https://www.greaterwrong.com/posts/vfHRahpgbp9YFPuGQ/city-of-lights"
     "https://www.greaterwrong.com/posts/goCfoiQkniQwPryki/lampshading"
     "https://www.greaterwrong.com/posts/xnPFYBuaGhpq869mY/ureshiku-naritai"
     "https://www.greaterwrong.com/posts/w8g7AkSbyApokD3dH/a-suite-of-pragmatic-considerations-in-favor-of-niceness"
     "https://www.greaterwrong.com/posts/nK5jraMp7E4xPvuNv/on-enjoying-disagreeable-company"
     "https://www.greaterwrong.com/posts/9sguwESkteCgqFMbj/seven-shiny-stories"

     ;; Multiagent Models of Mind

     "https://www.greaterwrong.com/posts/M4w2rdYgCKctbADMn/sequence-introduction-non-agent-and-multiagent-models-of"
     "https://www.greaterwrong.com/posts/x4n4jcoDP7xh5LWLq/book-summary-consciousness-and-the-brain"
     "https://www.greaterwrong.com/posts/5gfqG3Xcopscta3st/building-up-to-an-internal-family-systems-model"
     "https://www.greaterwrong.com/posts/AhcEaqWYpa2NieNsK/subagents-introspective-awareness-and-blending"
     "https://www.greaterwrong.com/posts/oJwJzeZ6ar2Hr7KAX/subagents-akrasia-and-coherence-in-humans"
     "https://www.greaterwrong.com/posts/hnLutdvjC8kPScPAj/integrating-disagreeing-subagents"
     "https://www.greaterwrong.com/posts/7zQPYQB5EeaqLrhBh/subagents-neural-turing-machines-thought-selection-and"
     "https://www.greaterwrong.com/posts/u5RLu5F3zKTB3Qjnu/subagents-trauma-and-rationality"
     "https://www.greaterwrong.com/posts/HbXXd2givHBBLxr3d/system-2-as-working-memory-augmented-system-1-reasoning"
     "https://www.greaterwrong.com/posts/i9xyZBS3qzA8nFXNQ/book-summary-unlocking-the-emotional-brain"
     "https://www.greaterwrong.com/posts/WYmmC3W6ZNhEgAmWG/a-mechanistic-model-of-meditation"
     "https://www.greaterwrong.com/posts/Mf2MCkYgSZSJRz5nM/a-non-mystical-explanation-of-insight-meditation-and-the"
     "https://www.greaterwrong.com/posts/W59Nb72sYJhMJKGB8/a-non-mystical-explanation-of-no-self-three-characteristics"
     "https://www.greaterwrong.com/posts/gvXFBaThWrMsSjicD/craving-suffering-and-predictive-processing-three"
     "https://www.greaterwrong.com/posts/r6kzvdia4S8TKE6WF/from-self-to-craving-three-characteristics-series"
     "https://www.greaterwrong.com/posts/h2xgbYBNP4dLharg4/on-the-construction-of-the-self"
     "https://www.greaterwrong.com/posts/T8gD9mRDHnb2gyn9N/three-characteristics-impermanence"
     "https://www.greaterwrong.com/posts/rgpAWJwuiFKq6Gbwy/beliefs-as-emotional-strategies"
     "https://www.greaterwrong.com/posts/YXBpBCNC66daaofoY/my-current-take-on-internal-family-systems-parts"

     ;; Why Everyone Else is a Hypocrite

     "https://www.greaterwrong.com/posts/WnjGhcRb2c6CabK5d/consistently-inconsistent"
     "https://www.greaterwrong.com/posts/jgkWqbNph57rAfPsi/modularity-and-buzzy"
     "https://www.greaterwrong.com/posts/fxgkYCbG5Hgy58TyC/strategic-ignorance-and-plausible-deniability"
     "https://www.greaterwrong.com/posts/o6CuZk2oPtDXqeY5A/modularity-signaling-and-belief-in-belief"

     ;; What Intelligence Tests Miss

     "https://www.greaterwrong.com/posts/ujTE9FLWveYz9WTxZ/what-cost-for-irrationality"
     "https://www.greaterwrong.com/posts/qMTzv8ATgDtfLq9ME/a-taxonomy-of-bias-the-cognitive-miser"
     "https://www.greaterwrong.com/posts/a5DQxG9NgzSLRZMnQ/a-taxonomy-of-bias-mindware-problems"
     "https://www.greaterwrong.com/posts/zRbh2mYgXtDJk8T42/what-intelligence-tests-miss-the-psychology-of-rational"

     )))

(defun my-lw-gather-crosslinks-from-file (url fname)
  (message "Gathering from %s" fname)
  (let* (
         (title (string-replace
                 " - LessWrong 2.0 viewer" "" (buffer-substring-no-properties
                                               (goto-char (point-min))
                                               (line-end-position))))
         (skip nil)
         (end-of-post (progn
                        (cond ((search-forward "What links here?" nil t)
                               (forward-line -1))
                              ((or (re-search-forward "^\* .*?UTC $" nil t)
                                   (re-search-forward "^No comments" nil t))
                               (forward-line -11))
                              (t
                               (message "Did not find end of post, skipping %s" fname)
                               (setq skip t)))
                        (point)))
         (date (progn
                 (goto-char (point-min))
                 (if (re-search-forward
                      (rx digit (?? digit) " " (= 3 word) " " (= 4 digit)) nil t)
                     (ts-format "%F" (ts-parse (match-string 0)))
                   (message "Did not find date, skipping %s" fname)
                   (setq skip t))))
         (crosslinks nil))
    (goto-char (point-min))
    (unless (search-forward "Post permalink" nil t)
      (message "Did not find start of post, skipping %s" fname))
    (forward-line 2)
    (unless skip
      ;; Get links in main body
      (while (< (point) end-of-post)
        (let ((match (text-property-search-forward 'shr-url nil nil t)))
          (when match
            (goto-char (prop-match-beginning match))))
        (unless (> (point) end-of-post)
          (let ((link (get-text-property (point) 'shr-url)))
            (when (string-match-p "^/" link)
              (setq link (concat "https://www.greaterwrong.com" link)))
            (cl-pushnew (replace-regexp-in-string "[?#].*$" "" link) crosslinks))))
      ;; Get previous-in-sequence
      (while (search-forward "Part of the sequence" nil t)
        (when (search-forward "Previous: " (line-end-position) t)
          (let ((link (get-text-property (point) 'shr-url)))
            (when (string-match-p "^/" link)
              (setq link (concat "https://www.greaterwrong.com" link)))
            (cl-pushnew (replace-regexp-in-string "[#?].*$" "" link) crosslinks))))
      (setq crosslinks (-uniq crosslinks))
      (dolist (link crosslinks)
        (when (string-search "greaterwrong.com/posts/" link)
          (unless (or (assoc link my-lw-urls-analyzed)
                      (member link my-lw-urls-to-visit)
                      (member link my-lw-urls-to-visit-extra))
            (cl-pushnew link my-lw-urls-to-visit-extra))))
      (push (list url title (org-id-uuid) date crosslinks) my-lw-urls-analyzed))))

;; -----------------------------------------------------------------------------


;; SLOW  -- hopefully only need to run once
(let ((default-directory "/home/lesswrong/"))
  (mkdir default-directory t)
  (f-write (string-join my-lw-urls-to-visit "\n") 'utf-8 "/tmp/download-urls")
  (async-shell-command "aria2c -x 16 -i /tmp/download-urls"))

(setq max-specpdl-size 180000)
;; original  (setq max-specpdl-size 1800)

(setq urls-filenames
      (cl-loop for url in my-lw-urls-to-visit
               collect
               (cons url (replace-regexp-in-string
                          "https://www.greaterwrong.com/posts/.*?/" "" url))))

;; SLOW
(let ((default-directory "/home/lesswrong/"))
  (cl-loop for pair in urls-filenames
           do
           (find-file-literally (cdr pair))
           (shr-render-buffer (current-buffer))
           (my-lw-gather-crosslinks-from-file (car pair) (cdr pair))
           (kill-buffer (find-buffer-visiting (cdr pair)))
           (pop urls-filenames)))

;; NOTE: lw-gather-crosslinks-from-file has now populated
;; lw-urls-to-visit-extra.  eval above again based on that instead of lw-urls-to-visit!



;; NOTE: i suggest putting the output dir somewhere that is ignored by
;; org-roam-db-node-include-function.  Org-id-locations will still know the
;; location, so clicking links will still work.  Then when you're ready to take
;; your own notes about one post, you pull the skeleton note out of that directory.
(defun my-write-org-dir-from-crosslinks ()
  (interactive)
  (shell-command "rm -rf /home/lesswrong-org/")
  (mkdir "/home/lesswrong-org")
  (dolist (item my-lw-urls-analyzed)
    (seq-let (url title id date crosslinks) item
      (setq crosslinks
            (-uniq (cl-loop
                    for link in crosslinks
                    collect (if (string-search "greaterwrong" link)
                                (replace-regexp-in-string "#.*?$" "" link)
                              link))))
      (with-temp-file (concat "/home/lesswrong-org/" date "-" (my-slugify title) ".org")
        (insert ":PROPERTIES:")
        (insert "\n:ID: " (caddr (assoc url my-lw-urls-analyzed)))
        (insert "\n:ROAM_REFS: " url)
        (insert "\n:END:")
        (insert "\n#+title: " title)
        (insert "\n#+filetags: :lw:")
        (insert "\n#+date: [" date "]")
        ;; (insert "\nBased on " url)
        (when crosslinks
          (insert "\n\n* Dependencies")
          (let ((pos (point)))
            (insert "\n\n* See also")
            (dolist (link crosslinks)
              (let ((data (assoc link my-lw-urls-analyzed)))
                (if data
                    (let ((link-title (cadr data))
                          (link-id (caddr data)))
                      (goto-char pos)
                      (insert "\n- [[id:" link-id "][" link-title "]]" ))
                  (goto-char (point-max))
                  (insert "\n- " link))))))))))


;; (defun my-lw-gather-crosslinks-from-post ()
;;   ;; (remove-hook 'eww-after-render-hook #'my-lw-gather-crosslinks-from-post)
;;   (let* ((url (plist-get eww-data :url))
;;          (title (string-replace " - LessWrong 2.0 viewer" ""
;;                                 (plist-get eww-data :title)))
;;          (skip nil)
;;          (end-of-post (progn
;;                         (cond ((search-forward "What links here?" nil t)
;;                                (forward-line -1))
;;                               ((or (re-search-forward "^\* .*?UTC $" nil t)
;;                                    (re-search-forward "^No comments" nil t))
;;                                (forward-line -11))
;;                               (t
;;                                (message "Did not find end of post, skipping %s" url)
;;                                (setq skip t)))
;;                         (point)))
;;          (date (progn
;;                  (goto-char (point-min))
;;                  (re-search-forward
;;                   (rx digit (?? digit) " " (= 3 word) " " (= 4 digit)))
;;                  (ts-format "%F" (ts-parse (match-string 0)))))
;;          (crosslinks nil))
;;     (unless skip
;;       ;; Start of post
;;       (goto-char (point-min))
;;       (unless (search-forward "Post permalink" nil t)
;;         (error "Did not find start of post"))
;;       (forward-line 2)
;;       ;; Get links in main body
;;       (while (< (point) end-of-post)
;;         (let ((match (text-property-search-forward 'shr-url nil nil t)))
;;           (when match
;;             (goto-char (prop-match-beginning match))))
;;         (unless (> (point) end-of-post)
;;           (let ((link (get-text-property (point) 'shr-url)))
;;             (when (stringp link)
;;               (cl-pushnew (replace-regexp-in-string "[?#].*$" "" link) crosslinks)))))
;;       ;; Get previous-in-sequence
;;       (while (search-forward "Part of the sequence" nil t)
;;         (when (search-forward "Previous: " (line-end-position) t)
;;           (let ((link (get-text-property (point) 'shr-url)))
;;             (if (stringp link)
;;                 (cl-pushnew (replace-regexp-in-string "[#?].*$" "" link) crosslinks)
;;               (error "Unexpected that it's not a string: %s" link)))))
;;       (setq crosslinks (-uniq crosslinks))
;;       (dolist (link crosslinks)
;;         (when (string-search "greaterwrong.com/posts/" link)
;;           (unless (or (assoc link my-lw-urls-analyzed)
;;                       (member link my-lw-urls-to-visit)
;;                       (member link my-lw-urls-to-visit-extra))
;;             (cl-pushnew link my-lw-urls-to-visit-extra))))
;;       (push (list url title (org-id-uuid) date crosslinks) my-lw-urls-analyzed)))
;;   (my-lw-gather-next))

;; (defun my-lw-gather-next ()
;;   (interactive)
;;   (when my-lw-urls-to-visit
;;     (unwind-protect
;;         (eww-browse-url (pop my-lw-urls-to-visit))
;;       (remove-hook 'eww-after-render-hook #'my-lw-gather-crosslinks-from-post)))

;;   (add-hook 'eww-after-render-hook #'my-lw-gather-crosslinks-from-post))

;; (defun my-write-single-org-file-from-crosslinks ()
;;   (interactive)
;;   (with-temp-file "/tmp/lesswrong.org"
;;     (dolist (item my-lw-urls-analyzed)
;;       (seq-let (url title crosslinks) item
;;         (setq crosslinks
;;               (-uniq (cl-loop
;;                       for link in crosslinks
;;                       collect (if (string-search "greaterwrong" link)
;;                                   (replace-regexp-in-string "#.*?$" "" link)
;;                                 link))))
;;         (insert "\n* " title " :lw:")
;;         (insert "\n:PROPERTIES:")
;;         (insert "\n:ID: " (caddr (assoc url my-uniques)))
;;         (insert "\n:END:")
;;         (insert "\nBased on " url)
;;         (when crosslinks
;;           (insert "\n\nDependencies:")
;;           (let ((pos (point)))
;;             (insert "\n\nNon-integrated dependencies:")
;;             (dolist (link crosslinks)
;;               (if (assoc link my-uniques)
;;                   (let ((link-title (cadr (assoc link my-uniques)))
;;                         (link-id (caddr (assoc link my-uniques))))
;;                     (goto-char pos)
;;                     (insert "\n- [[id:" link-id "][" link-title "]]" ))
;;                 (goto-char (point-max))
;;                 (insert "\n- " link)))))
;;         (goto-char (point-max))))))


