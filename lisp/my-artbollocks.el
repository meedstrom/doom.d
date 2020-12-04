(setq artbollocks-default-weasel-words
      '("many" "various" "very" "fairly" "several" "extremely" "exceedingly"
        "quite" "remarkably" "few" "surprisingly" "mostly" "largely" "huge" "tiny"
        "are a number" "is a number" "excellent" "interestingly" "significantly"
        "substantially" "clearly" "vast" "relatively" "completely"))
(setq artbollocks-default-jargon
      '("a priori" "ad hoc" "affirmation" "affirm"
        "affirms" "alterity" "altermodern" "aporia" "aporetic" "appropriates"
        "appropriation" "archetypal" "archetypical" "archetype" "archetypes"
        "autonomous" "autonomy" "baudrillardian" "baudrillarian" "commodification"
        "committed" "commitment" "commonalities" "contemporaneity" "context" "contexts"
        "contextual" "contextualise" "contextualises" "contextualisation"
        "contextialize" "contextializes" "contextualization" "contextuality"
        "convention" "conventional" "conventions" "coterminous" "critique" "cunning"
        "cunningly" "death of the author" "debunk" "debunked" "debunking" "debunks"
        "deconstruct" "deconstruction" "deconstructs" "deleuzian" "desire" "desires"
        "dialectic" "dialectical" "dialectically" "discourse" "discursive" "disrupt"
        "disrupts" "engage" "engagement" "engages" "episteme" "epistemic" "ergo"
        "fetish" "fetishes" "fetishise" "fetishised" "fetishize" "fetishized" "gaze"
        "gender" "gendered" "historicise" "historicisation" "historicize"
        "historicization" "hegemonic" "hegemony" "identity" "identity politics"
        "intensifies" "intensify" "intensifying" "interrogate" "interrogates"
        "interrogation" "intertextual" "intertextuality" "irony" "ironic" "ironical"
        "ironically" "ironisation" "ironization" "ironises" "ironizes" "jouissance"
        "juxtapose" "juxtaposes" "juxtaposition" "lacanian" "lack" "loci" "locus"
        "locuses" "matrix" "mise en abyme" "mocking" "mockingly" "modalities" "modality"
        "myth" "mythologies" "mythology" "myths" "narrative" "narrativisation"
        "narrativization" "narrativity" "nexus" "nodal" "node" "normative" "normativity"
        "notion" "notions" "objective" "objectivity" "objectivities" "objet petit a"
        "ontology" "ontological" "operate" "operates" "otherness" "othering" "paradigm"
        "paradigmatic" "paradigms" "parody" "parodic" "parodies" "physicality"
        "plenitude" "poetics" "popular notions" "position" "post hoc" "post internet"
        "post-internet" "postmodernism" "postmodernist" "postmodernity" "postmodern"
        "practice" "practise" "praxis" "problematic" "problematics" "problematise"
        "problematize" "proposition" "qua" "reading" "readings" "reification" "relation"
        "relational" "relationality" "relations" "representation" "representations"
        "rhizomatic" "rhizome" "simulacra" "simulacral" "simulation" "simulationism"
        "simulationism" "situate" "situated" "situates" "stereotype" "stereotypes"
        "strategy" "strategies" "subjective" "subjectivity" "subjectivities" "subvert"
        "subversion" "subverts" "text" "textual" "textuality" "thinker" "thinkers"
        "trajectory" "transgress" "transgresses" "transgression" "transgressive"
        "unfolding" "undermine" "undermining" "undermines" "work" "works" "wry" "wryly"))
(setq artbollocks-weasel-words-regex
      (concat "\\b" (regexp-opt (append artbollocks-default-weasel-words
                                        '("think"
                                          "one of the"
                                          "should"
                                          "just"
                                          "sort of"
                                          "a lot"
                                          "probably"
                                          "maybe"
                                          "perhaps"
                                          "I think"
                                          "really"
                                          "pretty"
                                          "maybe"
                                          "nice"
                                          "action"
                                          "utilize"
                                          "leverage")))
              "\\b"))
(setq artbollocks-jargon-regex
      (concat "\\b" (regexp-opt
                     (append
                      '()
                      (seq-difference artbollocks-default-jargon
                                      '("a priori"
                                        "position"
                                        "matrix"
                                        ))))
              "\\b"))

(after! artbollocks-mode
  (set-face-attribute 'artbollocks-face () :background 'unspecified)
  (set-face-attribute 'artbollocks-passive-voice-face () :background 'unspecified :foreground "gray")
  (set-face-attribute 'artbollocks-lexical-illusions-face () :background 'unspecified)
  (set-face-attribute 'artbollocks-weasel-words-face () :background 'unspecified))

(setq subs-for-think '("conclude" "define" "expect" "generalize" "hear"
                       "observe" "opine" "postulate" "state" "experience"
                       "remember" "particularize" "deduce" "infer" "induce"))

(setq subs-for-copula '("exist" "remain" "taste" "feel" "smell" "grow" "stay"
                        "turn" "look"))

;; download from the grand List and filter out the ridiculous substitutions
;; https://en.wikipedia.org/wiki/List_of_Germanic_and_Latinate_equivalents_in_English
(setq latinate-layman-alist
      '((("adolescence") . ("youth"))
        (("annual") . ("yearly"))
        (("labor") . ("work" "drudge"))
        (("forest") . ("wood" "brush" "grove" "thicket"))
        (("feminine") . ("womanly"))
        (("desire") . ("wish" "will" "yearning" "longing"))
        (("prudent" "intelligent") . ("wish" "will" "yearning" "longing"))
        (("savage" "feral") . ("wild"))
        (("latitude") . ("breadth" "span" "width"))
        (("entire") . ("whole"))
        (("important") . ("weighty" "key"))
        (("lament") . ("mourn" "regret"))
        (("mundane") . ("worldly" "earthly"))
        (("strange") . ("weird" "odd" "queer" "eerie"))
        (("marry") . ("wed"))
        (("marriage") . ("wedlock"))
        (("observe" "supervise") . ("watch" "keep" "behold"))
        (("launder") . ("wash" "cleanse" "rinse" "scrub"))
        (("notice" "alert") . ("warn" "warning" "tip off"))
        (("expect") . ("await"))
        ))

(defun my-suggest-sub ()
  (interactive)
  (cond ((member (word-at-point) '("think" "thought"))
         (when-let ((sub (completing-read "Substitute: " subs-for-think))
                    (bounds (bounds-of-thing-at-point 'word)))
           (delete-region (car bounds) (cdr bounds))
           (insert sub)))
        ((member (word-at-point) '("am" "are" "be" "is"))
         (when-let ((sub (completing-read "Substitute: " subs-for-copula))
                    (bounds (bounds-of-thing-at-point 'word)))
           (delete-region (car bounds) (cdr bounds))
           (insert sub)))
        ((when-let* ((cell (assoc-if (lambda (x) (member (word-at-point) x))
                                     latinate-layman-alist))
                     (sub (completing-read "Substitute: " (cdr cell)))
                     (bounds (bounds-of-thing-at-point 'word)))
           (delete-region (car bounds) (cdr bounds))
           (insert sub)))))
