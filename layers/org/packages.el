;;; packages.el --- Org Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq org-packages
  '(
    company
    company-emoji
    emoji-cheat-sheet-plus
    evil-org
    gnuplot
    http-post-simple
    htmlize
    ;; org is installed by `org-plus-contrib'
    (org :location built-in)
    (org-plus-contrib :step pre)
    org-bullets
    ;; org-mime is installed by `org-plus-contrib'
    (org-mime :location built-in)
    org-pomodoro
    org-present
    org-repo-todo
    toc-org
    ))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun org/post-init-company ()
    (spacemacs|add-company-hook org-mode)
    (push 'company-capf company-backends-org-mode))
  (defun org/post-init-company-emoji ()
    (push 'company-emoji company-backends-org-mode)))

(defun org/post-init-emoji-cheat-sheet-plus ()
  (add-hook 'org-mode-hook 'spacemacs/delay-emoji-cheat-sheet-hook))

(defun org/init-evil-org ()
  (use-package evil-org
    :commands evil-org-mode
    :init
    (add-hook 'org-mode-hook 'evil-org-mode)
    :config
    (progn
      (evil-leader/set-key-for-mode 'org-mode
        "mC" 'evil-org-recompute-clocks

        ;; evil-org binds these keys, so we bind them back to their original
        ;; value
        "t" (lookup-key evil-leader--default-map "t")
        "a" (lookup-key evil-leader--default-map "a")
        "b" (lookup-key evil-leader--default-map "b")
        "c" (lookup-key evil-leader--default-map "c")
        "l" (lookup-key evil-leader--default-map "l")
        "o" (lookup-key evil-leader--default-map "o"))
      (evil-define-key 'normal evil-org-mode-map
        "O" 'evil-open-above)
      (spacemacs|diminish evil-org-mode " ⓔ" " e"))))

(defun org/init-gnuplot ()
  (use-package gnuplot
    :defer t
    :init (evil-leader/set-key-for-mode 'org-mode
            "mtp" 'org-plot/gnuplot)))

;; dummy init function to force installation of `org-plus-contrib'
(defun org/init-org-plus-contrib ())

(defun org/init-org ()
  (use-package org
    :mode ("\\.org$" . org-mode)
    :defer t
    :bind (("C-c l" . org-store-link)
           ("C-c c" . org-capture)
           ("C-c a" . org-agenda)
           ("C-c b" . org-iswitchb)
           ("C-c h" . rfnash-visit-org-agenda-buffer)
           ("C-c F" . oog))
    :init
    (progn
      (evil-leader/set-key
        "oh"   'rfnash-visit-org-agenda-buffer)
      (setq org-clock-persist-file
            (concat spacemacs-cache-directory "org-clock-save.el")
            org-log-done t
            org-startup-with-inline-images t
            org-src-fontify-natively t)

      (eval-after-load 'org-indent
        '(spacemacs|hide-lighter org-indent-mode))
      (setq org-startup-indented t)
      (let ((dir (configuration-layer/get-layer-property 'org :dir)))
        (setq org-export-async-init-file (concat dir "org-async-init.el")))
      (defmacro spacemacs|org-emphasize (fname char)
        "Make function for setting the emphasis in org mode"
        `(defun ,fname () (interactive)
                (org-emphasize ,char)))

      ;; Insert key for org-mode and markdown a la C-h k
      ;; from SE endless http://emacs.stackexchange.com/questions/2206/i-want-to-have-the-kbd-tags-for-my-blog-written-in-org-mode/2208#2208
      (defun spacemacs/insert-keybinding-org (key)
        "Ask for a key then insert its description.
Will work on both org-mode and any mode that accepts plain html."
        (interactive "kType key sequence: ")
        (let* ((tag "@@html:<kbd>@@ %s @@html:</kbd>@@"))
          (if (null (equal key "\r"))
              (insert
               (format tag (help-key-description key nil)))
            (insert (format tag ""))
            (forward-char -8))))
      (evil-leader/set-key-for-mode 'org-mode
        "m'" 'org-edit-special
        "mc" 'org-capture
        "md" 'org-deadline
        "me" 'org-export-dispatch
        "mf" 'org-set-effort
        "m:" 'org-set-tags

        "ma" 'org-agenda
        "mb" 'org-tree-to-indirect-buffer
        "mA" 'org-archive-subtree
        "ml" 'org-open-at-point
        "mT" 'org-show-todo-tree

        "m." 'org-time-stamp

        ;; headings
        "mhi" 'org-insert-heading-after-current
        "mhI" 'org-insert-heading

        ;; More cycling options (timestamps, headlines, items, properties)
        "mL" 'org-shiftright
        "mH" 'org-shiftleft
        "mJ" 'org-shiftdown
        "mK" 'org-shiftup

        ;; Change between TODO sets
        "m C-S-l" 'org-shiftcontrolright
        "m C-S-h" 'org-shiftcontrolleft
        "m C-S-j" 'org-shiftcontroldown
        "m C-S-k" 'org-shiftcontrolup

        ;; Subtree editing
        "mSl" 'org-demote-subtree
        "mSh" 'org-promote-subtree
        "mSj" 'org-move-subtree-down
        "mSk" 'org-move-subtree-up

        ;; tables
        "mta" 'org-table-align
        "mtb" 'org-table-blank-field
        "mtc" 'org-table-convert
        "mtdc" 'org-table-delete-column
        "mtdr" 'org-table-kill-row
        "mte" 'org-table-eval-formula
        "mtE" 'org-table-export
        "mth" 'org-table-previous-field
        "mtH" 'org-table-move-column-left
        "mtic" 'org-table-insert-column
        "mtih" 'org-table-insert-hline
        "mtiH" 'org-table-hline-and-move
        "mtir" 'org-table-insert-row
        "mtI" 'org-table-import
        "mtj" 'org-table-next-row
        "mtJ" 'org-table-move-row-down
        "mtK" 'org-table-move-row-up
        "mtl" 'org-table-next-field
        "mtL" 'org-table-move-column-right
        "mtn" 'org-table-create
        "mtN" 'org-table-create-with-table.el
        "mtr" 'org-table-recalculate
        "mts" 'org-table-sort-lines
        "mttf" 'org-table-toggle-formula-debugger
        "mtto" 'org-table-toggle-coordinate-overlays
        "mtw" 'org-table-wrap-region

        "mI" 'org-clock-in
        (if dotspacemacs-major-mode-leader-key
            (concat "m" dotspacemacs-major-mode-leader-key)
          "m,") 'org-ctrl-c-ctrl-c
          "mn" 'org-narrow-to-subtree
          "mN" 'widen
          "mO" 'org-clock-out
          "mq" 'org-clock-cancel
          "mR" 'org-refile
          "ms" 'org-schedule

          ;; insertion of common elements
          "mil" 'org-insert-link
          "mif" 'org-footnote-new
          "mik" 'spacemacs/insert-keybinding-org

          ;; images and other link types have no commands in org mode-line
          ;; could be inserted using yasnippet?
          ;; region manipulation
          "mxb" (spacemacs|org-emphasize spacemacs/org-bold ?*)
          "mxc" (spacemacs|org-emphasize spacemacs/org-code ?~)
          "mxi" (spacemacs|org-emphasize spacemacs/org-italic ?/)
          "mxr" (spacemacs|org-emphasize spacemacs/org-clear ? )
          "mxs" (spacemacs|org-emphasize spacemacs/org-strike-through ?+)
          "mxu" (spacemacs|org-emphasize spacemacs/org-underline ?_)
          "mxv" (spacemacs|org-emphasize spacemacs/org-verbose ?=))

      (eval-after-load "org-agenda"
        '(progn
           (define-key org-agenda-mode-map "j" 'org-agenda-next-line)
           (define-key org-agenda-mode-map "k" 'org-agenda-previous-line)
           ;; Since we override SPC, let's make RET do that functionality
           (define-key org-agenda-mode-map
             (kbd "RET") 'org-agenda-show-and-scroll-up)
           (define-key org-agenda-mode-map
             (kbd "SPC") evil-leader--default-map)))
      (defun rfnash-visit-org-agenda-buffer ()
        "Create or visit 'org-mode' agenda buffer."
        (interactive)
        (if (get-buffer "*Org Agenda*")
            (rfnash-switch-buffer "*Org Agenda(n)*")
          (org-agenda nil "n"))))
    :config
    (progn
      ;; setup org directory
      (unless (file-exists-p org-directory)
        (make-directory org-directory))
      (font-lock-add-keywords
       'org-mode '(("\\(@@html:<kbd>@@\\) \\(.*\\) \\(@@html:</kbd>@@\\)"
                    (1 font-lock-comment-face prepend)
                    (2 font-lock-function-name-face)
                    (3 font-lock-comment-face prepend))))

      (require 'org-indent)
      (define-key global-map "\C-cl" 'org-store-link)
      (define-key global-map "\C-ca" 'org-agenda)

      ;; We add this key mapping because an Emacs user can change
      ;; `dotspacemacs-major-mode-emacs-leader-key' to `C-c' and the key binding
      ;; C-c ' is shadowed by `spacemacs/default-pop-shell', effectively making
      ;; the Emacs user unable to exit src block editing.
      (define-key org-src-mode-map (kbd (concat dotspacemacs-major-mode-emacs-leader-key " '")) 'org-edit-src-exit)

      (evil-leader/set-key
        "Cc" 'org-capture))
    (setq org-startup-folded t
          ;; org-blank-before-new-entry
          ;; org-global-properties '(("Effort_ALL" . "0 0:02 0:05 0:10 0:15 0:20 0:30 0:45 1:00 2:00"))
          org-startup-indented t        ; Appears to slow down org-mode
          org-hide-leading-stars t      ; Looks a bit funny without indent
          org-log-done t
          org-log-into-drawer t
          org-return-follows-link t
          org-journal-dir "~/Documents/OrgMode/journal/"
          org-directory "~/Documents/OrgMode"
          org-default-notes-file (concat org-directory "/notes.org")
          org-return-follows-link t
          org-enforce-todo-dependencies t
          org-tags-exclude-from-inheritance '("project")
          org-columns-default-format "%40ITEM(Task) %TODO %3PRIORITY %TAGS %17Effort(Estimated Effort){:} %5CLOCKSUM")
    (defun rfnash-show-all ()
      "Unfold all headings and show everything except drawers and archived subtrees"
      (interactive)
      (show-all)
      (if org-hide-block-startup (org-hide-block-all))
      (org-set-visibility-according-to-property 'no-cleanup)
      (org-cycle-hide-archived-subtrees 'all)
      (org-cycle-hide-drawers 'all)
      (org-cycle-show-empty-lines t))
    (use-package org-agenda
      :config
      (setq org-agenda-columns-add-appointments-to-effort-sum t
            org-agenda-dim-blocked-tasks t
            org-agenda-skip-deadline-if-done t
            org-agenda-skip-scheduled-if-deadline-is-shown nil
            org-agenda-skip-scheduled-if-done t
            org-agenda-span 'day
            org-agenda-sticky t
            org-agenda-window-setup 'current-window))
    (setq org-sort-agenda-notime-is-late t
          org-agenda-sorting-strategy
          '((agenda time-up habit-down timestamp-up priority-down category-keep)
            ;; (agenda time-up habit-down priority-down)
            (todo priority-down category-keep)
            (tags priority-down category-keep)
            (search category-keep)))
    (setq org-agenda-files
          (list org-directory
                (concat org-directory "/backlog")
                "~/Dropbox/notes"
                "~/Dropbox/Projects/learning.org"
                "~/.emacs.d/prelude/personal/rfnash/personal.org"))
    (setq org-stuck-projects '("+project/-DONE" ("TODO" "NEXT") nil ""))
    (setq org-agenda-custom-commands
          '(("n" "Agenda and all NEXT actions"
             ((agenda "")
              (todo "NEXT" nil))
             (;; (org-agenda-regexp-filter-preset '("-SOMEDAY" "-TOODLDO"))
              ))
            ("d" "Upcoming deadlines"
             agenda ""
             ((org-agenda-time-grid nil)
              (org-deadline-warning-days 36500)
              (org-agenda-entry-types '(:deadline))))
            ("v" "Videos to watch"
             tags "CATEGORY=\"ToWatch\"" nil)
            ;; TODO: consider only including those not under the appropriate level 1 heading
            ("f" "Bookmarks to file"
             ((todo "FILE")))
            ("u" "Upcoming next tasks"
             tags-todo "NEXT" nil)
            ("p" "Next actions of my projects"
             tags-todo "project" nil)
            ("i" "Ideas"
             ((todo "IDEA")))
            ("r" . "Review Toodledo Tasks")
            ("rt" "Review Toodledo Active tasks"
             ((todo "TOODLDO"))
             ((org-agenda-files '("~/Documents/OrgMode/Toodledo.org"))))
            ("rs" "Review Toodledo Someday tasks"
             ((todo "SOMEDAY"))
             ((org-agenda-files '("~/Documents/OrgMode/Toodledo.org"))))
            ))
    (setq org-agenda-time-grid
          '((daily today require-timed)
            #("----------------" 0 16
              (org-heading t))
            (800 1000 1200 1400 1600 1800 2000)))
    ;; (use-package org-agenda-property
    ;;   :ensure t
    ;;   :config
    ;;   ;; (setq org-agenda-property-list '("Effort"))
    ;;   (setq org-agenda-property-list nil)
    ;;   )
    (use-package org-capture
      :config
      (setq org-capture-templates
            '(("a" "Article"
               entry (id "1c5d07ad-8ba3-4db9-b3ae-3f4441cf51ca")
               "* TODO %?\n:PROPERTIES:\n:CREATED:  %U\n:END:")
              ("b" "Bookmarks to file"
               entry (file "~/Documents/OrgMode/Tasks.org")
               "* FILE %?\n:PROPERTIES:\n:CREATED:  %U\n:END:")
              ("B" "Bookmarks to file (with link annotation)"
               entry (id "3693c528-9bfb-43c4-91e3-d68550fc4799")
               "* FILE %?%a\n:PROPERTIES:\n:CREATED:  %U\n:END:"
               :immediate-finish t)
              ("H" "Scheduled Task"
               entry (file "~/Documents/OrgMode/Tasks.org")
               "* SCHED %?\nSCHEDULED: %t")
              ("d" "Design Examples"
               entry (id  "756f3a10-f323-409b-b418-00074ba93de9")
               "* %? :GoodDesign:\n:PROPERTIES:\n:CREATED:  %U\n:END:")
              ("D" "Task with a deadline"
               entry ( file+datetree "~/Documents/OrgMode/agenda.org")
               "* TODO %?\nDEADLINE: %t\n:PROPERTIES:\n:CREATED:  %U\n:END:")
              ("h" "Habbit"
               entry ( id "b8e8b020-2772-4028-b209-1a52d3cb2fe6")
               "* HABIT %?\n:PROPERTIES:\n:STYLE: habit\n:END:")
              ("j" "Datetree entry"
               entry ( file+datetree "~/Documents/OrgMode/agenda.org")
               "* %?\n:PROPERTIES:\n:CREATED:  %U\n:END:")
              ("J" "Clocked entry"
               entry ( file+datetree "~/Documents/OrgMode/agenda.org")
               "* %?\n%U"
               :clock-in t
               :clock-keep t)
              ("k" "Book to read"
               entry ( id "31b8afb4-8d11-4334-a12a-5f4490a77f3a")
               "* SOMEDAY %?\n:PROPERTIES:\n:CREATED:  %U\n:END:")
              ("m" "Good Song"
               table-line (id "b3a14888-312d-432e-8062-4ed352866729")
               " | %^{Title} | %^{Artist} |"
               :immediate-finish t)
              ;; ("n" "Task - Next"
              ;;  entry ( file+datetree "~/Documents/OrgMode/agenda.org")
              ;;  "* NEXT %?\n:PROPERTIES:\n:CREATED:  %U\n:END:")
              ("n" "Task - Next"
               entry (id "cc694bc0-77e0-4a17-bb64-0fb33d166aec")
               "* NEXT %?\n:PROPERTIES:\n:CREATED:  %U\n:Effort: 5\n:END:")
              ("N" "Non-profit"
               entry ( id "9e1cc358-cbc6-4b72-af68-c16017986720")
               "* %?\n:PROPERTIES:\n:CREATED:  %U\n:END:")
              ("o" "Toodledo Task"
               entry (id "cc694bc0-77e0-4a17-bb64-0fb33d166aec")
               "* TOODLDO %?\nSCHEDULED: %t\n:PROPERTIES:\n:CREATED:  %U\n:Effort:  5\n:END:")
              ("p" "Person"
               entry ( id "fbefa010-a0a4-4915-bc7e-ce1844a5e3a5")
               "* %?\n:PROPERTIES:\n:CREATED:  %U\n:END:")
              ("P" "Personal"
               entry ( id "5baf8be5-b956-421a-950c-6e39c8e6d52e")
               "* %?%a\n:PROPERTIES:\n:CREATED:  %U\n:END:")
              ("s" "Scratch"
               entry ( file+datetree "~/Documents/OrgMode/agenda.org")
               "* %? :SCRATCH:\n:PROPERTIES:\n:CREATED:  %U\n:END:")
              ("S" "Sent to Kindle"
               entry ( id "baef29b1-40b5-4704-bf28-24b1898c41c9")
               "* TODO %?\n:PROPERTIES:\n:CREATED:  %U\n:END:")
              ("t" "Task"
               entry (file "~/Documents/OrgMode/Tasks.org")
               "* TODO %?\n:PROPERTIES:\n:CREATED:  %U\n:END:")
              ("T" "Task (with link annotation)"
               entry (file "~/Documents/OrgMode/Tasks.org")
               "* TODO %?%a\n:PROPERTIES:\n:CREATED:  %U\n:END:")
              ("u" "Quote"
               entry (id "67e35685-d8e5-488d-8ab2-ac36a7ee3c9a")
               "* %?\n:PROPERTIES:\n:CREATED:  %U\n:END:")
              ("w" "To Watch"
               entry ( id "e8e91a3d-d21b-403d-b46e-0be14a3d7c2d")
               "* TODO %?\n:PROPERTIES:\n:CREATED:  %U\n:END:")
              ;; From http://sachachua.com/blog/2014/11/using-org-mode-keep-process-journal/
              ;; ("z" "Journal entry" plain
              ;;  (file+datetree+prompt "~/Documents/OrgMode/journal.org")
              ;;  "%K - %a\n%i\n%?\n")
              )))
    ;; An example
    ;;   (setq org-capture-templates
    ;;         (("c" "Jac" entry (file+datetree "~/cjr/jac/jac.org")
    ;;           "* %^{Title}  :blog:
    ;;    :PROPERTIES:
    ;;    :on: %T
    ;;    :END:
    ;;    %?
    ;;    %x")))
    ;; (use-package org-mobile-sync :ensure t)
    ;; (use-package org-pomodoro
    ;;   :ensure t
    ;;   :config
    ;;   (setq org-pomodoro-audio-player "mplayer"
    ;;         org-pomodoro-killed-sound-args "-volume 10"
    ;;         org-pomodoro-long-break-sound-args "-volume 10"
    ;;         org-pomodoro-short-break-sound-args "-volume 10"
    ;;         org-pomodoro-sound-args "-volume 10"
    ;;         org-pomodoro-start-sound-args "-volume 10"
    ;;         org-pomodoro-ticking-sound-args "-volume 10"))
    ;; (use-package org-trello :ensure t)
    ;; http://orgmode.org/worg/org-contrib/
    (require 'org-annotate-file)
    (require 'org-bbdb)
    (require 'org-bibtex)                   ; export bibtex fragments(require '
    (require 'org-bookmark)
    (require 'org-checklist)
    (require 'org-choose)                   ; http://orgmode.org/worg/org-contrib/org-choose.html
    (require 'org-collector)                ; http://orgmode.org/worg/org-contrib/org-collector.html
    (require 'org-ctags)
    ;; (require org-depend)                 ; http://orgmode.org/worg/org-contrib/org-depend.html
    (require 'org-elisp-symbol)
    (require 'org-eshell)
    (require 'org-eval)
    (require 'org-eval-light)
    ;; (require 'org-git-link)
    (require 'org-gnus)
    ;; (require org-index)                  ; http://orgmode.org/worg/org-contrib/org-index.html
    (require 'org-info)
    (require 'org-inlinetask)
    ;;(require 'org-json)
    (require 'org-learn)
    (require 'org-man)
    (require 'org-mouse)
    ;;(require 'org-mtags)
    ;; (require org-occur-goto)             ; http://www.emacswiki.org/emacs/org-search-goto.el
    ;;(require 'org-panal)
    ;; (require org-search-goto)            ; http://www.emacswiki.org/emacs/org-search-goto.el
    ;;(require org-search-goto-ml)          ;  http://www.emacswiki.org/emacs/org-search-goto-ml.el
    (require 'org-secretary)                ; http://juanreyero.com/article/emacs/org-teams.html
    (require 'org-toc)
    (require 'org-track)                    ; http://orgmode.org/worg/org-contrib/org-track.html
    (require 'org-w3m)
    (require 'remember)
    (setq flycheck-global-modes '(not org-mode))
    ;; (use-package org-drill
    ;;   :config
    ;;   ;; (setq org-drill-optimal-factor-matrix nil)
    ;;   (setq org-drill-scope 'agenda))
    (use-package org-mobile
      :config
      (setq org-mobile-directory "~/Dropbox/MobileOrg"
            ;; org-mobile-use-encryption t
            org-mobile-inbox-for-pull (concat org-directory "/from-mobile.org")))
    (use-package org-toodledo
      :load-path "~/.emacs.d/prelude/vendor/org-toodledo"
      :demand t
      :bind ("C-c x t" . org-toodledo-sync)
      :config
      (setq org-toodledo-preserve-drawers nil        ; Since I no longer sync habits, I don't need drawers synced
            org-toodledo-sync-new-completed-tasks t
            org-toodledo-userid "td52fa891b12039"
            org-toodledo-sync-on-save 'no
            org-toodledo-archive-completed-tasks nil ; Conflicts with sync-new-completed-tasks
            org-toodledo-archive-deleted-tasks t
            org-toodledo-status-to-org-map
            '(("Active" . "TOODLDO")
              ("None" . "HABIT")
              ("Next Action" . "NEXT")
              ("Planning" . "TODO")
              ("Delegated" . "DELEGATED")
              ("Waiting" . "WAITING")
              ("Someday" . "SOMEDAY")
              ("Hold" . "SOMEDAY")
              ("Postponed" . "SOMEDAY")
              ("Canceled" . "CANCELED")
              ("Reference" . "REFERENCE"))))
    ;; (use-package org-page
    ;;   :ensure t
    ;;   :config
    ;;   (setq op/personal-disqus-shortname "rfnash"
    ;;         op/personal-github-link "https://github.com/rfnash"
    ;;         op/repository-directory "~/git/blog.git/"
    ;;         op/repository-html-branch "gh-pages"
    ;;         op/repository-org-branch "source"
    ;;         op/site-domain "http://robertnash.net/"
    ;;         op/site-main-title "Robert Nash's Blog"
    ;;         op/site-sub-title "A sedomly updated blog"))
    (setq org-todo-keywords
          '((sequence "NEXT(n)" "TODO(t)" "TOODLDO(o)" "DELEGATED(g@)" "SOMEDAY(s)" "WAITING(w@)"
                      "|" "DONE(d!)" "CANCELLED(C@)" "REFERENCE(r)")
            (sequence "SCHED(c)" "|" "DONE(d!)")
            (sequence "HABIT(h)" "|" "DONE(d!)")
            (sequence "FILE(f)" "|")
            (sequence "IDEA(i)" "|")))
    (setq org-tag-alist
          '(("@work"      . ?w)
            ("@home"      . ?h)
            ("@phone"     . ?p)
            ("@computer"  . ?c)
            ("@tofile"    . ?t)
            ("READING"    . ?r)
            ("GoodDesign" . ?d)
            ("7plus"      . ?s)
            ("SCRATCH"    . ?e)
            ("NEXT"       . ?n)
            ("anki"       . ?a)))
    (use-package org-id
      :config
      (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))
    (use-package org-habit
      :config
      (setq org-habit-graph-column 65
            org-habit-following-days 1
            org-habit-graph-column 64
            org-habit-preceding-days 14
            org-habit-show-all-today nil
            org-habit-show-done-always-green t))
    (setq org-publish-project-alist
          '(("org-contents"
             :base-directory "~/org/"
             :base-extension "org"
             :publishing-directory "~/public_html/org-site/"
             :recursive t
             :publishing-function org-publish-org-to-html
             :table-of-contents nil
             :section-numbers nil
             ;;:author nil
             ;;:creator-info nil
             :html-postamble nil
             :auto-sitemap t)
            ("org" :components ("org-contents"))))
    ;; See http://kangtu.me/~kangtu/pavilion.html and http://doc.norang.ca/org-mode.html#RefileSetup.
    ;; Targets include this file and any file contributing to the agenda - up to 9 levels deep.
    (setq org-refile-targets '((nil :maxlevel . 2)
                               (org-agenda-files :maxlevel . 2))
          org-refile-use-outline-path t                    ; Use full outline paths for refile targets (for use with IDO/Helm)
          org-outline-path-complete-in-steps nil           ; Targets complete directly with IDO / Helm
          org-refile-allow-creating-parent-nodes 'confirm  ; Allow refile to create parent tasks with confirmation
          org-refile-use-cache t)
    (use-package org-expiry
      :config
      (setq org-expiry-inactive-timestamps t)
      (org-expiry-insinuate))
    (use-package org-contacts
      :config
      (setq org-contacts-birthday-format "Birthday: %h (%Y)"))
    ;; ;; Reminders
    ;; ;; From http://doc.norang.ca/org-mode.html#Reminders
    ;; ;; Set up reminders for all upcoming appointments
    ;;   (defun bh/org-agenda-to-appt ()
    ;;     "Erase all reminders and rebuilt reminders for today from the agenda."
    ;;     (interactive)
    ;;     (setq appt-time-msg-list nil)
    ;;     (org-agenda-to-appt))

    ;;   ;; Rebuild the reminders everytime the agenda is displayed
    ;;   (add-hook 'org-finalize-agenda-hook 'bh/org-agenda-to-appt 'append)

    ;;   ;; This is at the end of my .emacs - so appointments are set up when Emacs starts
    ;;   (bh/org-agenda-to-appt)

    ;;   ;; Activate appointments so we get notifications
    ;;   (appt-activate t)

    ;;   ;; If we leave Emacs running overnight - reset the appointments one minute after midnight
    ;;   (run-at-time "24:01" nil 'bh/org-agenda-to-appt)
    ;; From http://donarmstrong.com/posts/org_mode_mutt_capture:2F
    ;; I modified it so that org-capture-mode only closes other windows if its called via org-protocol.
    (use-package org-protocol
      :config
      (setq my-org-protocol-flag nil)

      (defun my-org-protocol-delete-other-windows ()
        (if my-org-protocol-flag (delete-other-windows)))

      (add-hook 'org-capture-mode-hook 'my-org-protocol-delete-other-windows)

      (defadvice org-capture-finalize (after delete-frame-at-end activate)
        "Delete frame at remember finalization"
        (progn (if my-org-protocol-flag (delete-frame))
               (setq my-org-protocol-flag nil)))

      (defadvice org-capture-refile (around delete-frame-after-refile activate)
        "Delete frame at remember refile"
        (if my-org-protocol-flag
            (progn
              (setq my-org-protocol-flag nil)
              ad-do-it
              (delete-frame))
          ad-do-it)
        )

      (defadvice org-capture-kill (after delete-frame-at-end activate)
        "Delete frame at remember abort"
        (progn (if my-org-protocol-flag (delete-frame))
               (setq my-org-protocol-flag nil)))

      (defadvice org-protocol-capture (before set-org-protocol-flag activate)
        (setq my-org-protocol-flag t)))
    (require 'org-registry)
    (org-registry-initialize)
    ;; (org-registry-insinuate)                ; Calls org-registry-update on save, but slows down org a lot

     ;;; Slightly modified org-registry-show from org-registry.el
     ;;; TODO: I could probably rewrite most of this function to use helm
    (defun rfnash-org-registry-show (visit link)
      "Show Org files where there are links pointing to the current
     buffer."
      ;; Not sure if I should use M on s in interactive
      (interactive "P\nMlink: ")
      (org-registry-initialize)
      (let* ((files (org-registry-assoc-all link))
             file point selection tmphist)
        (cond ((and files visit)
               ;; result(s) to visit
               (cond ((< 1 (length files))
                      ;; more than one result
                      (setq tmphist (mapcar (lambda(entry)
                                              (format "%s (%d) [%s]"
                                                      (nth 3 entry) ; file
                                                      (nth 2 entry) ; point
                                                      (nth 1 entry))) files))
                      (setq selection (completing-read "File: " tmphist
                                                       nil t nil 'tmphist))
                      (string-match "\\(.+\\) (\\([0-9]+\\))" selection)
                      (setq file (match-string 1 selection))
                      (setq point (string-to-number (match-string 2 selection))))
                     ((eq 1 (length files))
                      ;; just one result
                      (setq file (nth 3 (car files)))
                      (setq point (nth 2 (car files)))))
               ;; visit the (selected) file
               (funcall org-registry-find-file file)
               (goto-char point)
               (unless (org-before-first-heading-p)
                 (org-show-context)))
              ((and files (not visit))
               ;; result(s) to display
               (cond  ((eq 1 (length files))
                       ;; show one file
                       (message "Link in file %s (%d) [%s]"
                                (nth 3 (car files))
                                (nth 2 (car files))
                                (nth 1 (car files))))
                      (t (org-registry-display-files files link))))
              (t (message "No link to this in org-agenda-files")))))

    (defun rfnash-org-registry-show-clipboard (visit)
      (interactive "P")
      (rfnash-org-registry-show visit (x-get-clipboard)))
    ;; http://orgmode.org/worg/org-contrib/org-velocity.html
    (use-package org-velocity
      :load-path "~/.emacs.d/prelude/vendor/org-velocity/org-velocity.el"
      :commands org-velocity-read
      :bind "C-x c v"
      :config (setq org-velocity-bucket "~/Documents/OrgMode/reference.org"))
    ;; (use-package orgbox :ensure t)
    ;; (require 'orgbox)
    ;; https://github.com/jplindstrom/emacs-org-transform-tree-table
    ;; (use-package org-transform-tree-table :ensure t)
    ;; (use-package org-cliplink :ensure t)
    (defvar-local rfnash-hide-blocked-tasks nil "If non-nil, hide blocked tasks, else dim them.")
    (defun org-agenda-toggle-blocked-tasks ()
      "Toggle dimming/hiding blocked tasks."
      (interactive)
      (if rfnash-hide-blocked-tasks
          (progn (setq-local rfnash-hide-blocked-tasks nil)
                 (org-agenda-dim-blocked-tasks))
        (progn (setq-local rfnash-hide-blocked-tasks t)
               (org-agenda-dim-blocked-tasks t))))

      ;;; org-agenda-redo resets the value of rfnash-hide-blocked-tasks,
      ;;; thus its value has to be saved before its called, and restored afterwards
    (defun rfnash-org-agenda-redo (&optional all)
      "Rebuild possibly ALL agenda view(s) in the current buffer, hiding blocked tasks"
      (interactive "P")
      (let ((old-rfnash-hide-blocked-tasks rfnash-hide-blocked-tasks))
        (org-agenda-redo all)
        (setq-local rfnash-hide-blocked-tasks old-rfnash-hide-blocked-tasks)
        (if rfnash-hide-blocked-tasks
            (org-agenda-dim-blocked-tasks t))))

    (bind-key "#" #'org-agenda-toggle-blocked-tasks org-agenda-mode-map)
    (bind-key "r" #'rfnash-org-agenda-redo org-agenda-mode-map)))

(defun org/init-org-bullets ()
  (use-package org-bullets
    :defer t
    :init (add-hook 'org-mode-hook 'org-bullets-mode)))

(defun org/init-org-mime ()
  (use-package org-mime
    :defer t
    :commands (org-mime-htmlize org-mime-org-buffer-htmlize)
    :init
    (progn
      (evil-leader/set-key-for-mode 'message-mode
        "mM" 'org-mime-htmlize)
      (evil-leader/set-key-for-mode 'org-mode
        "mm" 'org-mime-org-buffer-htmlize))))

(defun org/init-org-pomodoro ()
  (use-package org-pomodoro
    :defer t
    :init
    (progn
      (when (spacemacs/system-is-mac)
        (setq org-pomodoro-audio-player "/usr/bin/afplay"))
      (evil-leader/set-key-for-mode 'org-mode
        "mp" 'org-pomodoro))))

(defun org/init-org-present ()
  (use-package org-present
    :defer t
    :init
    (progn
      (evilify nil org-present-mode-keymap
               "h" 'org-present-prev
               "l" 'org-present-next
               "q" 'org-present-quit)
      (defun spacemacs//org-present-start ()
        "Initiate `org-present' mode"
        (org-present-big)
        (org-display-inline-images)
        (org-present-hide-cursor)
        (org-present-read-only)
        (evil-evilified-state))
      (defun spacemacs//org-present-end ()
        "Terminate `org-present' mode"
        (org-present-small)
        (org-remove-inline-images)
        (org-present-show-cursor)
        (org-present-read-write)
        (evil-normal-state))
      (add-hook 'org-present-mode-hook 'spacemacs//org-present-start)
      (add-hook 'org-present-mode-quit-hook 'spacemacs//org-present-end))))

(defun org/init-org-repo-todo ()
  (use-package org-repo-todo
    :defer t
    :init
    (progn
      (evil-leader/set-key
        "Ct"  'ort/capture-todo
        "CT"  'ort/capture-checkitem)
      (evil-leader/set-key-for-mode 'org-mode
        "mgt" 'ort/goto-todos))))

(defun org/init-toc-org ()
  (use-package toc-org
    :defer t
    :init
    (progn
      (setq toc-org-max-depth 10)
      (add-hook 'org-mode-hook 'toc-org-enable))))

(defun org/init-htmlize ()
 (use-package htmlize
    :defer t))

(defun org/init-http-post-simple ()
  (use-package http-post-simple))
