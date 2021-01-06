(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#1c1408" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#bbc2cf"])
 '(custom-safe-themes
   '("3e810e100d84c8300a6595a956a542c4f07a4927a1031aca83941280edf84a19" "d7f15aa657f5d4905c8111bdf6ee81e9cbf61e984559c4ab65d35caf9d647e4b" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "b4466b46b7dcb9624305c4724519f6876bf185d9f1ae6d8d81b3b6c91c2599d8" "f0c6c8c6e03e3890f3483e2ea28ea31399513beb259a4a4c707b23669d6773b6" "317db7e1216965a53ffb5c3bac5650581e2095004ea5a38a1e9f6e70aaceea17" "d3b5ae5bc8132cdb5ecbc0d22120ec0f31ccb9da3345f6bd9efb7c25df797c4d" "79278310dd6cacf2d2f491063c4ab8b129fee2a498e4c25912ddaa6c3c5b621e" "74ba9ed7161a26bfe04580279b8cad163c00b802f54c574bfa5d924b99daa4b9" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "d6603a129c32b716b3d3541fc0b6bfe83d0e07f1954ee64517aa62c9405a3441" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "d74c5485d42ca4b7f3092e50db687600d0e16006d8fa335c69cf4f379dbd0eee" "e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" "01cf34eca93938925143f402c2e6141f03abb341f27d1c2dba3d50af9357ce70" "6084dce7da6b7447dcb9f93a981284dc823bab54f801ebf8a8e362a5332d2753" "65576ccf5f3c60051d6d25d9a0f534972c8c475099534a550ed5f2741e97039d" "098d1af9585c07e4192ee9e1278216a57fce2313b4d0f49692dd56a10520c242" "5ca6d04a5ce856da69a3c30ea06afbd1c15768719ac58bde72cb3b8fd5be15d4" "4efecc4596fcf41a04f2a5017102f65c406c856560de127713537822c1878cee" "272c5e0c54507c7fffdbbe56a2377f87183a71501c7615adcf8942cd7b1b38e3" "cae81b048b8bccb7308cdcb4a91e085b3c959401e74a0f125e7c5b173b916bf9" "a3b6a3708c6692674196266aad1cb19188a6da7b4f961e1369a68f06577afa16" "d5a878172795c45441efcd84b20a14f553e7e96366a163f742b95d65a3f55d71" "4bca89c1004e24981c840d3a32755bf859a6910c65b829d9441814000cf6c3d0" "990e24b406787568c592db2b853aa65ecc2dcd08146c0d22293259d400174e37" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" default))
 '(deianira-global-mode nil)
 '(escape-modality-global-mode nil)
 '(fci-rule-color "#5B6268")
 '(icomplete-mode t)
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(ledger-reports
   ''(("bal monthly" "%(binary) -f %(ledger-file) bal -M")
      ("bal" "%(binary) -f %(ledger-file) bal")
      ("reg" "%(binary) -f %(ledger-file) reg")
      ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
      ("account" "%(binary) -f %(ledger-file) reg %(account)")))
 '(objed-cursor-color "#ff6c6b")
 '(org-agenda-files
   '("/home/kept/Archive/memacs/git/escape-modality4.org_archive" "/home/kept/Journal/measurable.org" "/home/kept/Diary/200907.org"))
 '(pdf-view-midnight-colors (cons "#bbc2cf" "#282c34") t)
 '(rustic-ansi-faces
   ["#282c34" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#bbc2cf"])
 '(safe-local-variable-values
   '((nameless-current-name "scr")
     (nameless-current-name . scr)
     (eval font-lock-add-keywords nil
           `((,(concat "("
                       (regexp-opt
                        '("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl")
                        t)
                       "\\_>")
              1 'font-lock-variable-name-face)))
     (indent-tabs-mode nil)
     (auto-revert-mode . t)
     (org-confirm-babel-evaluate)))
 '(selectrum-mode t)
 '(vc-annotate-background "#282c34")
 '(vc-annotate-color-map
   (list
    (cons 20 "#98be65")
    (cons 40 "#b4be6c")
    (cons 60 "#d0be73")
    (cons 80 "#ECBE7B")
    (cons 100 "#e6ab6a")
    (cons 120 "#e09859")
    (cons 140 "#da8548")
    (cons 160 "#d38079")
    (cons 180 "#cc7cab")
    (cons 200 "#c678dd")
    (cons 220 "#d974b7")
    (cons 240 "#ec7091")
    (cons 260 "#ff6c6b")
    (cons 280 "#cf6162")
    (cons 300 "#9f585a")
    (cons 320 "#6f4e52")
    (cons 340 "#5B6268")
    (cons 360 "#5B6268")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka" :foundry "CYEL" :slant normal :weight normal :height 90 :width normal)))))
(put 'customize-themes 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'customize-variable 'disabled nil)
(put 'projectile-grep 'disabled nil)
