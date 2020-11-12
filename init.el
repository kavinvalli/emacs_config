(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; (use-package dashboard
;;   :ensure t
;;   :config
;;   (dashboard-setup-startup-hook))

(setq rune/is-termux
      (string-suffix-p "Android" (string-trim (shell-command-to-string "uname -a"))))

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                           ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

(use-package autothemer
  :ensure t)

(setq inhibit-startup-message t)

(unless rune/is-termux
  (scroll-bar-mode -1)        ; Disable visible scrollbar
  (tool-bar-mode -1)          ; Disable the toolbar
  (tooltip-mode -1)           ; Disable tooltips
  (set-fringe-mode 10))        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

(setq initial-scratch-message "; Hi Kavin. C-x C-f eh" ) ; Message on Scratch Buffer

(unless rune/is-termux
  (set-frame-parameter (selected-frame) 'alpha '(90 . 90))
  (add-to-list 'default-frame-alist '(alpha . (90 . 90)))
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

;; Set up the visible bell
(when (equal system-name "Kavins-Air.Dlink")
  (setq visible-bell nil
        ring-bell-function 'double-flash-mode-line)
  (defun double-flash-mode-line ()
    (let ((flash-sec (/ 1.0 20)))
      (invert-face 'mode-line)
      (run-with-timer flash-sec nil #'invert-face 'mode-line)
      (run-with-timer (* 2 flash-sec) nil #'invert-face 'mode-line)
      (run-with-timer (* 3 flash-sec) nil #'invert-face 'mode-line))))

(when (equal system-name "kavin-pc")
  (setq visible-bell t))

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                vterm-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(defvar runemacs/default-font-size 135)

(when (equal system-name "Kavins-Air.Dlink")
    (set-face-attribute 'default nil :font "MesloLGS NF" :height runemacs/default-font-size)
    (set-face-attribute 'fixed-pitch nil :font "MesloLGS NF" :height 140)
)
(when (equal system-name "kavin-pc")
    (set-face-attribute 'default nil :font "Fira Code Retina" :height runemacs/default-font-size)
    (set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 140)
)
(set-face-attribute 'variable-pitch nil :family "Cantarell" :height 160 :weight 'regular)

(use-package all-the-icons)

(use-package doom-modeline
  :hook (after-init . doom-modeline-init)
  :custom
  (doom-modeline-height 15)
  (doom-modeline-bar-width 6)
  (doom-modeline-lsp t)
  (doom-modeline-github nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-irc nil)
  (doom-modeline-minor-modes t)
  (doom-modeline-persp-name nil)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-major-mode-icon nil))

(use-package doom-themes :defer t)
;; (use-package spacemacs-theme :defer t)
;; (load-theme 'doom-dracula t)
;; (load-theme 'spacemacs-dark t)
;; (load-theme 'doom-palenight t)
;; (load-theme 'doom-horizon t)
(load-theme 'doom-acario-dark t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package general
  :config
  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

(defun dw/dont-arrow-me-bro ()
  (interactive)
  (message "Arrow keys are bad, you know?"))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (unless rune/is-termux                                       
    (define-key evil-normal-state-map (kbd "<left>") 'dw/dont-arrow-me-bro)
    (define-key evil-normal-state-map (kbd "<right>") 'dw/dont-arrow-me-bro)
    (define-key evil-normal-state-map (kbd "<down>") 'dw/dont-arrow-me-bro)
    (define-key evil-normal-state-map (kbd "<up>") 'dw/dont-arrow-me-bro)
    (evil-global-set-key 'motion (kbd "<left>") 'dw/dont-arrow-me-bro)
    (evil-global-set-key 'motion (kbd "<right>") 'dw/dont-arrow-me-bro)
    (evil-global-set-key 'motion (kbd "<down>") 'dw/dont-arrow-me-bro)
    (evil-global-set-key 'motion (kbd "<up>") 'dw/dont-arrow-me-bro))

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package command-log-mode)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         ("C-M-j" . counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package autopair)
(autopair-global-mode)

(use-package emmet-mode
  :mode "\\.edge\\'"
  :diminish (emmet-mode . "ε")
  :bind* (("C-)" . emmet-next-edit-point)
          ("C-(" . emmet-prev-edit-point)
          ("M-<tab>" . emmet-expand-line))
  :commands (emmet-mode
             emmet-next-edit-point
             emmet-prev-edit-point)
  :init
  (setq emmet-indentation 2)
  (setq emmet-move-cursor-between-quotes t)
  :config
  ;; Auto-start on any markup modes
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode)
  (setq emmet-expand-jsx-className? nil)  ;; Set to nil because className was being used in non-jsx files too
  (setq emmet-self-closing-tag-style " /"))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(rune/leader-keys
  "h" '(:ignore t :which-key "Hydra")
  "hs" '(hydra-text-scale/body :which-key "Scale Text"))

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org-bullets
  ;; :if (not rune/is-termux) 
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :ensure org-plus-contrib
  :config
  (setq org-ellipsis " ▾")

  (setq org-directory
        (if rune/is-termux
            "~/storage/shared/Notes"
          "~/Notes"))

  ;; (setq org-src-fontify-natively t)

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files
        '("~/Notes/Tasks.org"
          "~/Notes/Birthdays.org"
          "~/Notes/Calendar.org"))

  (setq org-refile-targets
    '(("Archive.org" :maxlevel . 1)
      ("Tasks.org" :maxlevel . 1)))

  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (efs/org-font-setup))

(setq org-tag-alist
  '((:startgroup)
     ; Put mutually exclusive tags here
     (:endgroup)

     ("@school" . ?S)
     ("@home" . ?H)
     ("@tricycle" . ?T)
     ("@fiitjee" . ?F)
     ("planning" . ?p)
     ("study" . ?s)
     ("note" . ?n)
     ("idea" . ?i)))

(setq org-agenda-custom-commands
 '(("d" "Dashboard"
   ((agenda "" ((org-deadline-warning-days 7)))
    (todo "TODO"
      ((org-agenda-overriding-header "TODO Tasks")))
    (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

  ("n" "TODO Tasks"
   ((todo "TODO"
      ((org-agenda-overriding-header "Todo Tasks")))))

  ("T" "Tricycle Tasks" tags-todo "+@tricycle")

  ("S" "School Tasks" tags-todo "+@school")

  ("s" "Study Planning" tags-todo "+study-planning")

  ;; Low-effort next actions
  ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
   ((org-agenda-overriding-header "Low Effort Tasks")
    (org-agenda-max-todos 20)
    (org-agenda-files org-agenda-files)))

  ("w" "Workflow Status"
   ((todo "WAIT"
          ((org-agenda-overriding-header "Waiting on External")
           (org-agenda-files org-agenda-files)))
    (todo "REVIEW"
          ((org-agenda-overriding-header "In Review")
           (org-agenda-files org-agenda-files)))
    (todo "PLAN"
          ((org-agenda-overriding-header "In Planning")
           (org-agenda-todo-list-sublevels nil)
           (org-agenda-files org-agenda-files)))
    (todo "BACKLOG"
          ((org-agenda-overriding-header "Project Backlog")
           (org-agenda-todo-list-sublevels nil)
           (org-agenda-files org-agenda-files)))
    (todo "READY"
          ((org-agenda-overriding-header "Ready for Work")
           (org-agenda-files org-agenda-files)))
    (todo "ACTIVE"
          ((org-agenda-overriding-header "Active Projects")
           (org-agenda-files org-agenda-files)))
    (todo "COMPLETED"
          ((org-agenda-overriding-header "Completed Projects")
           (org-agenda-files org-agenda-files)))
    (todo "CANC"
          ((org-agenda-overriding-header "Cancelled Projects")
           (org-agenda-files org-agenda-files)))))))

(setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/Notes/Tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal" entry
           (file+olp+datetree "~/Notes/Journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
      ("jm" "Meeting" entry
           (file+olp+datetree "~/Notes/Journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

      ("w" "Workflows")
      ("we" "Checking Email" entry (file+olp+datetree "~/Notes/Journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)))

(define-key global-map (kbd "C-c j")
  (lambda () (interactive) (org-capture nil "jj")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (js . t)
   (sql . t)
   (lua . t)))

 (push '("conf-unix" . conf-unix) org-src-lang-modes)

(use-package org-special-block-extras
  :ensure t
  :hook (org-mode . org-special-block-extras-mode))

(use-package org-make-toc
  :hook (org-mode . org-make-toc-mode))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("temp" . "src"))
(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("js" . "src js"))
(add-to-list 'org-structure-template-alist '("html" . "src html"))
(add-to-list 'org-structure-template-alist '("sql" . "src sql"))
(add-to-list 'org-structure-template-alist '("rust" . "src rust"))
(add-to-list 'org-structure-template-alist '("lua" . "src lua"))

(defun efs/org-babel-tangle-config ()
  (let ((org-confirm-babel-evaluate nil))
    (org-babel-tangle)))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(use-package org-alert
  :ensure t
   :custom (alert-default-style 'osx-notifier)
   :config
   (setq org-alert-interval 300
         org-alert-notification-title "Reminder!")
   (org-alert-enable))

(use-package counsel-osx-app
  :bind* ("S-M-SPC" . counsel-osx-app)
  :commands counsel-osx-app
  :config
  (setq counsel-osx-app-location
        (list "/Applications"
              "/Applications/Misc"
              "/Applications/Utilities"
              (expand-file-name "~/Applications")
              (expand-file-name "~/.nix-profile/Applications")
              "/Applications/Xcode.app/Contents/Applications")))

(use-package elcord
  :ensure t
  :disabled rune/is-termuxk
  :custom
  (elcord-display-buffer-details nil)
  :config
  (elcord-mode))

(use-package speed-type
  :ensure t)

(defun rune/bluetooth-connect-philips ()
  (interactive)
  (start-process-shell-command "blueutil" nil "blueutil --connect a4-77-58-7a-2d-7d"))


(rune/leader-keys
  "b" '(:ignore t :which-key "Bluetooth")
  "bc" '(:ignore t :which-key "connect")
  "bcp" '(rune/bluetooth-connect-philips :which-key "Philips SHB3075"))

(defun rune/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode)
  (lsp))

(use-package lsp-mode
  :ensure t
  :bind (:map lsp-mode-map
              ("TAB" . completion-at-point))
  :commands (lsp lsp-deffered)
  :hook (lsp-mode . rune/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c s-p")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-ivy)

(use-package lsp-treemacs
    :init (treemacs-display-current-project-exclusively)
    :after lsp)

(rune/leader-keys
  "t" '(:ignore t :which-key "Treemacs")
  "to" 'treemacs
  "tt" 'treemacs-display-current-project-exclusively)

(use-package dap-mode)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(defun rune/set-js-indentation ()
  (setq js-indent-level 2)
  (setq evil-shift-width js-indent-level)
  (setq-default tab-width 2))

(use-package js2-mode
  :mode "\\.js\\'"
  :hook (js2-mode . lsp-deferred)
  :config
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))
  (setq js2-mode-show-strict-warnings nil)
  (add-hook 'js2-mode-hook #'rune/set-js-indentation)
  (add-hook 'json-mode-hook #'rune/set-js-indentation)
  (require 'dap-node)
  (dap-node-setup))

(use-package prettier-js
  :hook ((js2-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode))
  :config
  (setq prettier-js-show-errors nil))

(use-package web-mode
  :mode "(\\.\\(html?\\|ejs\\|tsx\\|js[x]?\\|edge\\)\\'"
  :hook (web-mode . lsp-deferred)
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))

;; 1. Start the server with `httpd-start'
;; 2. Use `impatient-mode' on any buffer
(use-package impatient-mode
  :ensure t)

(use-package skewer-mode
  :ensure t)

(use-package css-mode
  :mode "\\.css\\'"
  :hook (css-mode . lsp-deferred)
  :config
  (setq css-indent-offset 2))

(use-package scss-mode
  :mode "\\.scss\\'"
  :hook (scss-mode . lsp-deferred)
  :config
  (setq scss-output-directory "../css")
  (setq scss-compile-at-save t))

(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                          (lsp-deferred))))

(use-package cargo
      :ensure t
      :mode "\\.rs"
      :init
      (bind-keys :prefix-map cargo-mode-map
                 :prefix "C-c c"
                 ("C" . cargo-process-repeat)
                 ("." . cargo-process-repeat)
                 ("X" . cargo-run-example)
                 ("c" . cargo-process-build)
                 ("d" . cargo-process-doc)
                 ("e" . cargo-process-bench)
                 ("R" . cargo-process-current-test)
                 ("f" . cargo-process-fmt)
                 ("i" . cargo-process-init)
                 ("n" . cargo-process-new)
                 ("o" . cargo-process-current-file-tests)
                 ("s" . cargo-process-search)
                 ("u" . cargo-process-update)
                 ("x" . cargo-process-run)
                 ("t" . cargo-process-test)
                 ("R" . cargo-process-test-regexp)))
(use-package rustic
  :ensure t
  :mode ("\\.rs" . rustic-mode)
  :hook (web-mode . lsp-deferred)
  :commands (cargo-minor-mode)
  :config
  (bind-keys :map rustic-mode-map
             ("C-c TAB" . rustic-format-buffer)
             ("TAB" . company-indent-or-complete-common))
  :init
  (setq rustic-lsp-server 'rust-analyzer)
  (setq company-tooltip-align-annotations t
        rustic-format-on-save nil)
  (add-hook 'rustic-mode-hook #'cargo-minor-mode))

(use-package lua-mode
  :ensure t
  :mode (("\\.lua\\'" . lua-mode))
  :config
  (add-hook 'lua-mode-hook #'company-mode))

(use-package company
  :after lsp-mode
  :hook ((lsp-mode . company-mode)
         (eldoc-mode . company-mode))
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :diminish
  :functions (all-the-icons-faicon
              all-the-icons-material
              all-the-icons-octicon
              all-the-icons-alltheicon)
  :hook (company-mode . company-box-mode)
  :init (setq company-box-enable-icon (display-graphic-p))
  :config
  (setq company-box-backends-colors nil))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Documents/projects")
    (setq projectile-project-search-path '("~/Documents/projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
     :custom
     (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package evil-magit
     :after magit)

(setq auth-sources '("~/.authinfo"))

(use-package forge)

(use-package lorem-ipsum
  :ensure t
  :config
  (lorem-ipsum-use-default-bindings))

(use-package evil-nerd-commenter
  :bind ("s-/" . evilnc-comment-or-uncomment-lines))

(use-package comment-tags
  :init
	(autoload 'comment-tags-mode "comment-tags-mode")
  (setq comment-tags-keyword-faces
        `(("TODO" . ,(list :weight 'bold :foreground "#28ABE3"))
          ("BUG" . ,(list :weight 'bold :foreground "#DB3340"))
          ("INFO" . ,(list :weight 'bold :foreground "#F7EAC8"))
          ("DONE" . ,(list :weight 'bold :foreground "#1FDA9A"))))
  (setq comment-tags-keymap-prefix (kbd "C-c t"))
  (setq comment-tags-comment-start-only t
        comment-tags-require-colon t
        comment-tags-case-sensitive t
        comment-tags-show-faces t
        comment-tags-lighter nil)
  :config
  (add-hook 'prog-mode-hook 'comment-tags-mode))

(rune/leader-keys
  "r" '(:ignore t :which-key "Rename")
  "rf" 'rename-file)

(use-package ivy-pass
  :commands ivy-pass
  :config
  (setq password-store-password-length 12)
  (setq epa-file-cache-passphrase-for-symmetric-encryption nil))

(use-package auth-source-pass
  :config
  (auth-source-pass-enable))

(rune/leader-keys
  "p" '(:ignore t :which-key "pass")
  "pp" 'ivy-pass
  "pi" 'password-store-insert
  "pg" 'password-store-generate)

;; (use-package org-gcal
;;      :after org
;;      :config

;;      (setq org-gcal-client-id (password-store-get "API/Google/kavinvalli-emacs-id")
;;            org-gcal-client-secret (password-store-get "API/Google/kavinvalli-emacs-secret")
;;            org-gcal-file-alist '(("kavinvalli@gmail.com" . "~/Notes/Calendar.org"))))

;; (rune/leader-keys
;;   "c" '(:ignore t :which-key "calendar")
;;   "cs" '(org-gcal-fetch :which-key "sync")
;;   "cp" '(org-gcal-post-at-point :which-key "post"))

(use-package counsel-spotify
  :after ivy
  :config
  (setq counsel-spotify-client-id (password-store-get "API/Spotify/kavinvalli-emacs-id"))
  (setq counsel-spotify-client-secret (password-store-get "API/Spotify/kavinvalli-emacs-secret")))

(rune/leader-keys
  "s" '(:ignore t :which-key "Counsel Spotify")
  "ss" '(:ignore t :which-key "Search")
  "ssp" '(counsel-spotify-search-playlist :which-key "Search Playlist")
  "sst" '(counsel-spotify-search-track :which-key "Search Track")
      "sp" '(counsel-spotify-toggle-play-pause :which-key "Toggle Play Pause")
      "sa" '(counsel-spotify-search-album :which-key "Search Album")
      "s>" '(counsel-spotify-next :which-key "Next")
      "s<" '(counsel-spotify-previous :which-key "Previous"))

(use-package ivy-youtube
  :config
  (setq ivy-youtube-key (password-store-get "API/Youtube/kavinvalli-emacs-api-key")))
(rune/leader-keys
  "y" '(ivy-youtube :which-key "Ivy Youtube"))

(use-package term
  :config
  (setq explicit-shell-file-name "zsh")
  (setq term-prompt-regexp "^[^#$%>\\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  ;;(setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first")
           (insert-directory-program "/usr/local/bin/gls")
           (delete-by-moving-to-trash t))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(use-package dired-single)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :config
  (setq dired-open-extensions '(("png" . "open")
                                ("jpg" . "open")
                                ("jpeg" . "open")
                                ("pdf" . "open")
                                ("mov" . "open"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package dired-rainbow
  :config
  (progn
    (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
    (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
    (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
    (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
    (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
    (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
    (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
    (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
    (dired-rainbow-define log "#c17d11" ("log"))
    (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
    (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
    (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
    (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
    (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
    (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
    (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
    (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
    (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")
    ))

(+ 50 100)
