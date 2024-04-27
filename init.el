;; the code in this code block and ALL code block with ":tangle yes" will be exported

(defun open-my-startup-file ()
  "Open a specific file and maximize the Emacs window on startup."
  (find-file (concat user-emacs-directory "PKM/notes/" "tutorial.org"))  ; Change the path to your specific file
  (delete-other-windows))

;; Add the custom startup function to the Emacs startup hook
(add-hook 'emacs-startup-hook 'open-my-startup-file)

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(when (version< emacs-version "29")
  (straight-use-package 'use-package))

;; always download package automatically (without :ensure t)
(setq use-package-always-ensure t)

;; Configure use-package to use straight.el by default
(setq straight-use-package-by-default t)

(use-package no-littering
	     :init
	     (require 'no-littering)
	     )

(defgroup eepkm nil
  "Customization group for EasyEmacsPKM"
  :group 'main-group  ; Inherits from main-group
  :prefix "eepkm-"
  )

;;; Encodings
;; Contrary to what many Emacs users have in their configs, you don't need more
;; than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")
;; ...but `set-language-environment' also sets `default-input-method', which is
;; a step too opinionated.
(setq default-input-method nil)
;; ...And the clipboard on Windows could be in a wider encoding (UTF-16), so
;; leave Emacs to its own devices.
(when (memq system-type '(cygwin windows-nt ms-dos))
  (setq selection-coding-system 'utf-8))

;; make esc key do cancel. works only in gui emacs
(define-key key-translation-map (kbd "<escape>") (kbd "C-g"))
;; the first don't work with all the time
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; visuellement
(global-visual-line-mode 1)

(use-package smartparens
    :hook (org-mode . smartparens-mode)
    :config
    (sp-pair "\«" "\»")  
    ;; the second argument is the closing delimiter, so you need to skip it with nil
    (sp-pair "'" nil :actions :rem)  
    (sp-local-pair 'org-mode "*" "*") ;; adds * as a local pair in org mode
    (sp-local-pair 'org-mode "=" "=") ;; adds = as a local pair in org mode
    (sp-local-pair 'org-mode "\/" "\/")
    )

(auto-save-visited-mode 1)
(setq auto-save-visited-interval 10) ; every X seconds

(cua-mode 1)

(use-package doom-modeline
	     :init
	     (doom-modeline-mode)
	     :custom    
	     (doom-modeline-height 25)
	     (doom-modeline-bar-width 1)
	     (doom-modeline-icon t)
	     (doom-modeline-major-mode-icon t)
	     (doom-modeline-major-mode-color-icon t)
	     (doom-modeline-buffer-file-name-style 'truncate-upto-project)
	     (doom-modeline-buffer-state-icon t)
	     (doom-modeline-buffer-modification-icon t)
	     (doom-modeline-minor-modes nil)
	     (doom-modeline-enable-word-count t)
	     (doom-modeline-buffer-encoding nil)
	     (doom-modeline-indent-info nil)
	     (doom-modeline-checker-simple-format t)
	     (doom-modeline-vcs-max-length 20)
	     (doom-modeline-env-version t)
	     (doom-modeline-irc-stylize 'identity)
	     (doom-modeline-github-timer nil)
	     (doom-modeline-gnus-timer nil)
	     )

(use-package all-the-icons-dired)

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(use-package nyan-mode
    :init (nyan-mode)
    )

(use-package doom-themes
	     :config
	     ;; (load-theme 'doom-moonlight t)
	     (load-theme 'leuven t)
	     )

(use-package hydra)

(use-package pretty-hydra
	     :init
	     
	     ;; Customizable key bindings for PKM section
	     (defcustom eepkm-bindings-find-node-key "f"
	       "Key for `org-roam-node-find` in the eepkm-bindings PKM section."
	       :type 'string
	       :group 'eepkm-bindings)
	     
	     (defcustom eepkm-bindings-insert-node-key "i"
	       "Key for `org-roam-node-insert` in the eepkm-bindings PKM section."
	       :type 'string
	       :group 'eepkm-bindings)
	     
	     (defcustom eepkm-bindings-attach-key "a"
	       "Key for `org-attach` in the eepkm-bindings PKM section."
	       :type 'string
	       :group 'eepkm-bindings)
	     
	     ;; Customizable key bindings for Note section
	     (defcustom eepkm-bindings-note-new-heading-key "h"
	       "Key for `org-meta-return` in the eepkm-bindings Note section."
	       :type 'string
	       :group 'eepkm-bindings)
	     
	     (defcustom eepkm-bindings-note-todo-key "t"
	       "Key for `org-todo` in the eepkm-bindings Note section."
	       :type 'string
	       :group 'eepkm-bindings)
	     
	     (defcustom eepkm-bindings-note-export-key "e"
	       "Key for `org-export-dispatch` in the eepkm-bindings Note section."
	       :type 'string
	       :group 'eepkm-bindings)
	     
	     (defcustom eepkm-bindings-note-store-link-key "l"
	       "Key for `org-store-link` in the eepkm-bindings Note section."
	       :type 'string
	       :group 'eepkm-bindings)
	     
	     (defcustom eepkm-bindings-note-insert-link-key "m"
	       "Key for `org-insert-link` in the eepkm-bindings Note section."
	       :type 'string
	       :group 'eepkm-bindings)
	     
	     ;; Customizable key bindings for Window section
	     (defcustom eepkm-bindings-window-split-horizontally-key "h"
	       "Key for `split-window-below` in the eepkm-bindings Window section."
	       :type 'string
	       :group 'eepkm-bindings)
	     
	     (defcustom eepkm-bindings-window-split-vertically-key "v"
	       "Key for `split-window-right` in the eepkm-bindings Window section."
	       :type 'string
	       :group 'eepkm-bindings)
	     
	     (defcustom eepkm-bindings-window-next-window-key "n"
	       "Key for `next-window` in the eepkm-bindings Window section."
	       :type 'string
	       :group 'eepkm-bindings)
	     
	     (defcustom eepkm-bindings-window-previous-window-key "p"
	       "Key for `previous-window` in the eepkm-bindings Window section."
	       :type 'string
	       :group 'eepkm-bindings)
	     
	     (defcustom eepkm-bindings-window-winner-undo-key "w"
	       "Key for `winner-undo` in the eepkm-bindings Window section."
	       :type 'string
	       :group 'eepkm-bindings)
	     
	     (defcustom eepkm-bindings-window-winner-redo-key "x"
	       "Key for `winner-redo` in the eepkm-bindings Window section."
	       :type 'string
	       :group 'eepkm-bindings)
	     
	     (defcustom eepkm-bindings-window-delete-other-windows-key "k"
	       "Key for `delete-other-windows` in the eepkm-bindings Window section."
	       :type 'string
	       :group 'eepkm-bindings)
	     
	     (defcustom eepkm-bindings-window-delete-window-key "d"
	       "Key for `delete-window` in the eepkm-bindings Window section."
	       :type 'string
	       :group 'eepkm-bindings)
	     
	     
	     )

(defgroup eepkm-bindings nil
  "Customization subgroup for key bindings"
  :group 'eepkm  
  )

(defcustom eepkm-bindings-menu "<f11>"
  ;; (kbd "<escape>")
  ;; (kbd "C-c h")
  "Key for `org-roam-node-find` in the eepkm-bindings PKM section."
  :type 'string
  :group 'eepkm-bindings)

(global-set-key (kbd eepkm-bindings-menu) 'eepkm-bindings/body)

;; hydra-keyboard-quit
(eval
 `(pretty-hydra-define eepkm-bindings
    (:title "Main Commands of the PKM" :color amaranth :quit-key "ESC" :exit t)
    ("PKM"
     ((,eepkm-bindings-find-node-key org-roam-node-find "Find and go to a node")
      (,eepkm-bindings-insert-node-key org-roam-node-insert "Find and insert a link to a node")
      (,eepkm-bindings-attach-key org-attach "Attach a document to the heading"))
     "Note"
     ((,eepkm-bindings-note-new-heading-key org-meta-return "Insert new heading or list")
      (,eepkm-bindings-note-todo-key org-todo "Mark a heading as TODO")
      (,eepkm-bindings-note-export-key org-export-dispatch "Export to another format")
      (,eepkm-bindings-note-store-link-key org-store-link "Store the link under the cursor")
      (,eepkm-bindings-note-insert-link-key org-insert-link "Insert a link"))
     "Window"
     ((,eepkm-bindings-window-split-horizontally-key split-window-below "Split your window horizontally")
      (,eepkm-bindings-window-split-vertically-key split-window-right "Split your window vertically")
      (,eepkm-bindings-window-next-window-key next-window "Next window")
      (,eepkm-bindings-window-previous-window-key previous-window "Previous window")
      (,eepkm-bindings-window-winner-undo-key winner-undo "Undo previous configuration of window(s)")
      (,eepkm-bindings-window-winner-redo-key winner-redo "Redo previous configuration of window(s)")
      (,eepkm-bindings-window-delete-other-windows-key delete-other-windows "Keep only the current window")
      (,eepkm-bindings-window-delete-window-key delete-window "Delete current window")))))

(use-package which-key
	     :init
	     (which-key-mode)
	     :config
	     (setq which-key-idle-delay 0.5)  ; Adjust to the desired delay in seconds before which-key pops up
	     (setq which-key-popup-type 'side-window)  ; Display in side window by default
	     (setq which-key-side-window-location 'bottom)  ; Display at the bottom of the screen
	     (setq which-key-side-window-max-width 0.33)  ; Use a third of the screen width for which-key window
	     (setq which-key-side-window-max-height 0.25)  ; Use a quarter of the screen height for which-key window
	     (which-key-setup-side-window-bottom)  ; Setup to display at the bottom
	     )

;; vertical completion
(use-package vertico
	     :init
	     (vertico-mode 1)
	     :custom
	     (vertico-cycle t)
	     ;; :custom-face
	     ;; (vertico-current ((t (:background "#3a3f5a"))))
	     )

;; annotation in the minibuffer
(use-package marginalia
	     :init
	     (marginalia-mode 1)
	     :custom
	     (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
	     )

;; Use the `orderless' completion style.
;; Use space-separated search terms in any order when completing with Icomplete or the default interface.
;; Example : M-x consult-line, write "use ordeless", and you will find the configuration of the package orderless !
(use-package orderless
	     :init
	     ;; Enable `partial-completion' for files to allow path expansion.
	     ;; You may prefer to use `initials' instead of `partial-completion'.
	     (setq completion-styles '(orderless)
		   completion-category-defaults nil
		   completion-category-overrides '((file (styles partial-completion)))))

;; better searching 
(use-package consult
	     :config
	     ;; Replace bindings with Consult commands
	     (global-set-key (kbd "C-s") 'consult-line)
	     (global-set-key (kbd "C-x b") 'consult-buffer)
	     (global-set-key (kbd "M-y") 'consult-yank-pop)
	     (global-set-key [remap switch-to-buffer] 'consult-buffer)
	     (global-set-key [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
	     (global-set-key [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame))

(winner-mode 1)

(use-package org :straight (org :type git :repo "https://code.orgmode.org/bzg/org-mode.git")
	     :config
	     (setq org-directory (concat user-emacs-directory "PKM/notes/"))
	     )

(defun my/org-export-output-dir (orig-fun &rest args)
  "Modification of the export-output directory for Org-mode."
  (let ((old-default-directory default-directory))
    ;; Change working directory temporarily to 'export' directory.
    (setq default-directory (expand-file-name "PKM/data/export/" user-emacs-directory))
    (apply orig-fun args)
    ;; Restores original working directory after export.
    (setq default-directory old-default-directory)))

;; Applies directory modification function to all Org export functions.
(advice-add 'org-export-to-file :around #'my/org-export-output-dir)

;;Pour obtenir des polices proportionnelles
(variable-pitch-mode 1)

;; Make sure org-indent face is available
(require 'org-indent)
;; (set-face-attribute 'org-document-title nil :font "Fira Mono" :weight 'bold :height 1.5)
;; (dolist (face '((org-level-1 . 1.3)
;;                 (org-level-2 . 1.25)
;;                 (org-level-3 . 1.20)
;;                 (org-level-4 . 1.15)
;;                 (org-level-5 . 1.10)
;;                 (org-level-6 . 1.05)
;;                 (org-level-7 . 1.0)
;;                 (org-level-8 . 1.0)))
;;   (set-face-attribute (car face) nil :font "Fira Mono" :weight 'medium :height (cdr face)))


;; ;; Ensure that 
;; anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

;;couleur des checkbox
(defface org-checkbox-todo-text
  '((t (:inherit org-todo)))
  "Face for the text part of an unchecked org-mode checkbox.")

(font-lock-add-keywords
 'org-mode
 `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?: \\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-checkbox-todo-text prepend))
 'append)

(defface org-checkbox-done-text
  '((t (:inherit org-done)))
  "Face for the text part of a checked org-mode checkbox.")

(font-lock-add-keywords
 'org-mode
 `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-checkbox-done-text prepend))
 'append)

(setq org-ellipsis "⬎")

(add-hook 'org-mode-hook 'org-indent-mode)

(setq org-attach-dir (concat user-emacs-directory "PKM/data/org-attach"))

;; each attached document go to the ID of the nodes

;;The first function in this list defines the preferred function which will be used when creating new attachment folders.
(setq org-attach-id-to-path-function-list
      '(cp/org-attach-id-uuid-folder-format
	;; org-attach-id-uuid-folder-format
	))

(defun cp/org-attach-id-uuid-folder-format (id)
  "Return the path to attach a file with an id"
  (format "%s" id))

(use-package org-roam
	     :init
	     (setq org-roam-directory org-directory)
	     ;;avoid nottif from version 1 to 2
	     (setq org-roam-v2-ack t)
	     :custom
	     (org-roam-completion-everywhere t) ;; to have completion everywhere
	     ;;set my log capture, not used
	     (org-roam-dailies-directory "journals/")
	     ;; what's in the backlinks buffer
	     (org-roam-mode-sections
	      (list #'org-roam-backlinks-section
		    #'org-roam-reflinks-section
		    #'org-roam-unlinked-references-section
		    ))
	     :config
	     (setq org-roam-directory org-directory)
	     ;; automatic sync with files 
	     (org-roam-db-autosync-mode +1)
	     )

(use-package org-roam-ui
    :after org-roam
    :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    ;; :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start nil)
    )

(use-package consult-org-roam
   :after org-roam
   :init
   (require 'consult-org-roam)
   ;; Activate the minor mode
   (consult-org-roam-mode 1)
   :custom
   ;; Use `ripgrep' for searching with `consult-org-roam-search'
   (consult-org-roam-grep-func #'consult-ripgrep)
   ;; Display org-roam buffers right after non-org-roam buffers
   ;; in consult-buffer (and not down at the bottom)
   (consult-org-roam-buffer-after-buffers t)
   :bind
   ;; Define some convenient keybindings as an addition
   ("C-c n e" . consult-org-roam-file-find)
   ("C-c n b" . consult-org-roam-backlinks)
   ("C-c n B" . consult-org-roam-backlinks-recursive)
   ("C-c n l" . consult-org-roam-forward-links)
   ("C-c n r" . consult-org-roam-search))

;;to directly delete the buffer if a file (or directory) is deleted
(defun my--dired-kill-before-delete (file &rest rest)
  (if-let ((buf (get-file-buffer file)))
      (kill-buffer buf)
    (dolist (dired-buf (dired-buffers-for-dir file))
      (kill-buffer dired-buf))))
(advice-add 'dired-delete-file :before 'my--dired-kill-before-delete)



					; automatic refresh of dired when file is modified
(add-hook 'dired-mode-hook 'auto-revert-mode)

(setq dired-auto-revert-buffer t) ; ; Update dired buffer on revisit
(setq dired-dwim-target t) ; ; If two dired buffers are open, save in the other on copy attempt
(setq dired-hide-details-hide-symlink-targets nil) ; ; Do not hide symlink targets
(setq dired-listing-switches "-alh") ; ; Allow dired to display all folders, in lengty format, with quantities of data in human-readable format
(setq dired-ls-F-marks-symlinks nil) ; ; Informs dired how 'ls -lF' marks symbolic links, see help page for details
(setq dired-recursive-copies 'always) ; ; Always recursively copies without prompting
(setq dired-recursive-deletes 'always) ; asks for more to delete recursively
(setq dired-dwim-target t) ; qd t-on copies, if another dired is open, copies into it "directly".

(load (concat user-emacs-directory "personal.el"))

(customize-set-variable 'custom-file (no-littering-expand-etc-file-name "custom.el"))

  ;; after-init-hook ?
  (when (file-exists-p custom-file)
    (load custom-file nil 'nomessage))

;; (add-hook 'after-init-hook (lambda ()
;; 				    (if (file-exists-p custom-file)
;; 					(load custom-file nil 'nomessage)
;; 				      (message "Le fichier de configuration custom-file [%s] n'existe pas" custom-file))))
