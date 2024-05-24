;; the code in this code block and ALL code block with ":tangle yes" will be exported

;; Install elpaca
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
			      :ref nil :depth 1
			      :files (:defaults "elpaca-test.el" (:exclude "extensions"))
			      :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
	(if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
		 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
						 ,@(when-let ((depth (plist-get order :depth)))
						     (list (format "--depth=%d" depth) "--no-single-branch"))
						 ,(plist-get order :repo) ,repo))))
		 ((zerop (call-process "git" nil buffer t "checkout"
				       (or (plist-get order :ref) "--"))))
		 (emacs (concat invocation-directory invocation-name))
		 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
				       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
		 ((require 'elpaca))
		 ((elpaca-generate-autoloads "elpaca" repo)))
	    (progn (message "%s" (buffer-string)) (kill-buffer buffer))
	  (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
;; change here, because after-init-hook don't exist ?
(add-hook 'emacs-startup-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Uncomment for systems which cannot create symlinks:
(when (memq system-type '(
			  ;; cygwin 
			  windows-nt
			  ;; ms-dos
			  ))
  (elpaca-no-symlink-mode))

;; (add-hook 'elpaca-after-init-hook (lambda () (message "🪝 elpaca-after-init-hook")))

(elpaca no-littering
  ;; loading of no-littering
  (require 'no-littering)
  )

;; Install use-package
(when (version< emacs-version "29")
  (elpaca elpaca-use-package
    ;; Enable use-package :ensure support for Elpaca.
    (elpaca-use-package-mode)

    ;; Download automatically packages missing (without :ensure t)
    (require 'use-package-ensure)
    (setq use-package-always-ensure t)
    ;; always defer package to speed up time
    (setq use-package-always-defer t)
    ))

;; (elpaca-wait)

(use-package restart-emacs)
(add-hook 'elpaca-after-init-hook
	  (lambda ()
	    (if (eq 0 (elpaca-alist-get 'failed elpaca--status-counts 0))
		(message "All the packages are installed")
	      (when (yes-or-no-p "Emacs has not finish to download all packages, do you want to restart ?") (restart-emacs))
	      ))
	  )

(with-eval-after-load 'no-littering
  (customize-set-variable 'custom-file (no-littering-expand-etc-file-name "custom.el"))
  )

;; load before everything else
(if (file-exists-p custom-file)
					(load custom-file nil 'nomessage)
				      (message "The customisation of the user [%s] is not present." custom-file))

;;loading of saved customizations with elpaca
;; (add-hook 'elpaca-after-init-hook (lambda ()
;; 				    (if (file-exists-p custom-file)
;; 					(load custom-file nil 'nomessage)
;; 				      (message "The customisation of the user [%s] is not present." custom-file))))

(defun open-main-tutorial ()
    "Open a specific file and maximize the Emacs window on startup."
    (interactive)
    (find-file (concat user-emacs-directory "PKM/notes/tutorial/" "tutorial.org")))

(add-hook 'elpaca-after-init-hook
	    #'open-main-tutorial
	    )

(defun stop-using-minibuffer (&optional arg)
  "Kill the minibuffer when Emacs loses focus or the mouse leaves the buffer."
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

;; Add to mouse-leave-buffer-hook to handle mouse leaving Emacs window
(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;; make esc key do cancel. works only in gui emacs
(define-key key-translation-map (kbd "<escape>") (kbd "C-g"))
;; the first don't work with all the time
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(cua-mode 1)

(defcustom eepkm-auto-save t
  "If t, save after `auto-save-visited-interval'"
  :type 'boolean
  :group 'eepkm)

(when (>= emacs-major-version 26)
  ;; real auto save
  (auto-save-visited-mode eepkm-auto-save))

(defun eepkm-display-message (msg)
  "Display the message MSG in the echo area with yellow foreground."
  (propertize msg 'face '(:foreground "gold" :weight bold :height 1.7)))
(setq set-message-function #'eepkm-display-message)

(custom-set-faces
 '(minibuffer-prompt ((t (:foreground "gold" :weight bold :height 1.7)))))

(defgroup eepkm nil
  "Customization group for EasyEmacsPKM"
  :group 'main-group  ; Inherits from main-group
  :prefix "eepkm-"
  )

;; visuellement
(global-visual-line-mode 1)

(defcustom eepkm-text-scale 170
    "Size of text in Emacs."
    :type 'integer
    :group 'eepkm)

(set-face-attribute 'default (selected-frame) :height eepkm-text-scale)

(let ((font-name-1 "DejaVu Sans Mono")
      (font-name-2 "DejaVu Serif")
      (fallback-font "Courier New"))
  (if (and (find-font (font-spec :name font-name-1)) (find-font (font-spec :name font-name-2)))
      (progn
	(set-face-attribute 'default nil :family font-name-1)
	(set-face-attribute 'fixed-pitch nil :family font-name-1)
	(set-face-attribute 'variable-pitch nil :family font-name-2))  ; Keeping 'DejaVu Serif' for variable-pitch as before
    (progn
      (set-face-attribute 'default nil :family fallback-font)
      (set-face-attribute 'fixed-pitch nil :family fallback-font)
      (set-face-attribute 'variable-pitch nil :family fallback-font))))

(use-package smartparens
    :hook (org-mode . smartparens-mode)
    :config
    (sp-pair "\«" "\»")  
    ;; the second argument is the closing delimiter, so you need to skip it with nil
    (sp-pair "'" nil :actions :rem)  
    ;; (sp-local-pair 'org-mode "*" "*") ;; adds * as a local pair in org mode
    (sp-local-pair 'org-mode "=" "=") ;; adds = as a local pair in org mode
    (sp-local-pair 'org-mode "\/" "\/")
    )

(auto-save-visited-mode 1)
(setq auto-save-visited-interval 10) ; every X seconds

(defcustom eepkm-margin 170
  "Increment for text scaling in Emacs."
  :type 'integer
  :group 'eepkm)

(use-package perfect-margin
	     :init (perfect-margin-mode)
	     :config (setq perfect-margin-visible-width eepkm-margin)
	     )

;; nice color mode line
(custom-set-faces
 '(mode-line ((t (:box (:line-width 1 :color "#1A2F54") :foreground "#85CEEB" :background "#335EA8")))))

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

(use-package nerd-icons
	     :init
	     ;; (unless (member "Symbols Nerd Font Mono" (font-family-list))
	       ;; (nerd-icons-install-fonts t))
	     )

(use-package nerd-icons-dired
	     :hook
	     (dired-mode . nerd-icons-dired-mode))

(use-package good-scroll
	     :hook (org-mode . good-scroll-mode)
	     )

(use-package nyan-mode
    :init (nyan-mode)
    )

(use-package doom-themes
	     :init
	     ;; (load-theme 'doom-moonlight t)
	     )

(use-package leuven-theme
	     :init
	     ;; (load-theme 'leuven t)
	     ;; (load-theme 'leuven-dark t)
	     )

(use-package ef-themes
	     :init

	     (defcustom eepkm-dark-theme t
	       "If non-nil, launch emacs with the dark-theme."
	       :type 'boolean
	       :group 'eepkm)

	     (defun eepkm-ef-themes-select (theme &optional variant)
	       "Function to select and apply an EF theme."

	       ;; Set variables before the package is loaded
	       (setq ef-themes-to-toggle '(ef-duo-dark ef-duo-light)
		     ef-themes-region '(intense)
		     ef-themes-mixed-fonts t
		     ef-themes-variable-pitch-ui t
		     ef-themes-headings '((0 . (variable-pitch light 1.9))
					  (1 . (variable-pitch light 1.8))
					  (2 . (variable-pitch regular 1.7))
					  (3 . (variable-pitch regular 1.6))
					  (4 . (variable-pitch regular 1.5))
					  (5 . (variable-pitch 1.4))  ; absence of weight means `bold'
					  (6 . (variable-pitch 1.3))
					  (7 . (variable-pitch 1.2))
					  (t . (variable-pitch 1.1))))

	       (load-theme theme t))

	     (eepkm-ef-themes-select 'ef-duo-dark)

	     (when (not eepkm-dark-theme)
	       (ef-themes-toggle)
	       )

	     )

(use-package hydra)

(use-package pretty-hydra
	     :init
	     
	     (pretty-hydra-define eepkm-master-hydra
	     		     (:title "Master Commands Menu" :color red :exit t :quit-key "ESC")
	     		     ("Menus"
	     		      (("o" eepkm-org-mode-hydra/body "Org Mode Menu (org-mode-hydra)")
	     		       ("w" eepkm-window-management-hydra/body "Window Management (window-management-hydra)")
	     		       ("e" eepkm-movement-and-editing-hydra/body "Basic Movement and Editing Commands (eepkm-movement-and-editing-hydra)")
	     		       ("b" eepkm-buffer-file-hydra/body "Buffer and File Management (buffer-file-hydra)")
	     		       ("h" eepkm-help-and-customisation-hydra/body "Help and Documentation (help-documentation-hydra)")
	     		       ("c" execute-extended-command "Execute a command with name (execute-extended-command)")
	     		       )
	     		      "Nodes"
	     		      (("f" org-roam-node-find "Find node (org-roam-node-find)")
	     		       ("i" org-roam-node-insert "Insert node link (org-roam-node-insert)")
	     		       ("s" switch-eepkm-include-tutorial "Activate or desactivate search in tutorial (switch-eepkm-include-tutorial)")
	     		       ("t" open-main-tutorial "Go to tutorial (open-main-tutorial)")
	     		       ("g" org-roam-ui-open "Open the graphe of nodes in browser (org-roam-ui-open)")
	     		       )
	     		      "Attached file"
	     		      (("a" org-attach "Attach document to node at point (org-attach)")
	     		       ("r" org-attach-reveal "See attached document for the node (org-attach-reveal)")
	     		       )))
	     
	     
	     (pretty-hydra-define eepkm-org-mode-hydra
	     		     (:title "Org Mode Operations" :color blue :quit-key "ESC")
	     		     ("Editing"
	     		      (("h" org-meta-return "New heading/item (org-meta-return)")
	     		       ("l" org-insert-link "Insert link (org-insert-link)")
	     		       ("s" org-store-link "Store link (org-store-link)")
	     		       ("t" org-todo "Toggle TODO (org-todo)"))
	     		      "Navigation"
	     		      (("u" outline-up-heading "Up heading (outline-up-heading)")
	     		       ("n" org-next-visible-heading "Next heading (org-next-visible-heading)")
	     		       ("p" org-previous-visible-heading "Previous heading (org-previous-visible-heading)"))
	     		      "Misc"
	     		      (("a" org-agenda "Open Agenda (org-agenda)")
	     		       ("c" org-capture "Capture item (org-capture)")
	     		       ("b" org-switchb "Switch org buffer (org-switchb)")
	     		       ("e" org-export-dispatch "Export (org-export-dispatch)")
	     		       )))
	     
	     
	     (pretty-hydra-define eepkm-window-management-hydra
	       (:title "Window Management" :color teal :quit-key "ESC")
	       ("Windows"
	        (("s" split-window-below "Split horizontally (split-window-below)")
	         ("v" split-window-right "Split vertically (split-window-right)")
	         ("d" delete-window "Delete window (delete-window)")
	         ("o" delete-other-windows "Delete other windows (delete-other-windows)"))
	        "Frames"
	        (("f" make-frame "New frame (make-frame)")
	         ("x" delete-frame "Delete frame (delete-frame)"))
	        "Screen"
	        (("u" winner-undo "Undo layout (winner-undo)")
	         ("r" winner-redo "Redo layout (winner-redo)"))))
	     
	     
	     (defun org-mark-ring-push (&optional pos buffer)
	       "Put the current position into the mark ring and rotate it.
	     Also push position into the Emacs mark ring.  If optional
	     argument POS and BUFFER are not nil, mark this location instead."
	       (interactive)
	       (let ((pos (or pos (point)))
	     	(buffer (or buffer (current-buffer))))
	         (with-current-buffer buffer
	           (org-with-point-at pos (push-mark nil t)))
	         (setq org-mark-ring (nthcdr (1- org-mark-ring-length) org-mark-ring))
	         (move-marker (car org-mark-ring) pos buffer))
	       (message
	        (substitute-command-keys
	         "Position saved to mark ring, go back with the menu eepkm-movement-and-editing-hydra.")))
	     
	     (pretty-hydra-define eepkm-movement-and-editing-hydra
	       (:title "Basic Editing Commands" :color teal :quit-key "ESC")
	       (
	     "Movement"
	        (("m" (lambda () (interactive) (set-mark-command t)) "Go to the previous mark (set-mark-command t)"))
	     "Edit"
	        (("c" copy-region-as-kill "Copy (copy-region-as-kill)")
	         ("x" kill-region "Cut (kill-region)")
	         ("v" yank "Paste (yank)")
	         ("z" undo "Undo (undo)"))
	        "Search"
	        (("s" consult-line "Search (consult-line)")
	         ("q" query-replace "Replace occurence (query-replace)"))))
	     
	     
	     (defun xah-open-in-external-app (&optional Fname)
	       "Open the current file or dired marked files in external app.
	     When called in emacs lisp, if Fname is given, open that.
	     
	     URL `http://xahlee.info/emacs/emacs/emacs_dired_open_file_in_ext_apps.html'
	     Version: 2019-11-04 2023-03-10 2023-04-05"
	       (interactive)
	       (let (xfileList xdoIt)
	         (setq xfileList
	     	  (if Fname
	     	      (list Fname)
	     	    (if (string-equal major-mode "dired-mode")
	     		(dired-get-marked-files)
	     	      (list buffer-file-name))))
	         (setq xdoIt (if (<= (length xfileList) 10) t (y-or-n-p "Open more than 10 files? ")))
	         (when xdoIt
	           (cond
	            ((string-equal system-type "windows-nt")
	     	(let ((xoutBuf (get-buffer-create "*xah open in external app*"))
	     	      (xcmdlist (list "PowerShell" "-Command" "Invoke-Item" "-LiteralPath")))
	     	  (mapc
	     	   (lambda (x)
	     	     (message "%s" x)
	     	     (apply 'start-process (append (list "xah open in external app" xoutBuf) xcmdlist (list (format "'%s'" (if (string-match "'" x) (replace-match "`'" t t x) x))) nil)))
	     	   xfileList)
	     	  ;; (switch-to-buffer-other-window xoutBuf)
	     	  )
	     	;; old code. calling shell. also have a bug if filename contain apostrophe
	     	;; (mapc (lambda (xfpath) (shell-command (concat "PowerShell -Command \"Invoke-Item -LiteralPath\" " "'" (shell-quote-argument (expand-file-name xfpath)) "'"))) xfileList)
	     	)
	            ((string-equal system-type "darwin")
	     	(mapc (lambda (xfpath) (shell-command (concat "open " (shell-quote-argument xfpath)))) xfileList))
	            ((string-equal system-type "gnu/linux")
	     	(mapc (lambda (xfpath)
	     		(call-process shell-file-name nil nil nil
	     			      shell-command-switch
	     			      (format "%s %s"
	     				      "xdg-open"
	     				      (shell-quote-argument xfpath))))
	     	      xfileList))
	            ((string-equal system-type "berkeley-unix")
	     	(mapc (lambda (xfpath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" xfpath))) xfileList))))))
	     
	     (pretty-hydra-define eepkm-buffer-file-hydra
	     		     (:title "Buffer and File Management" :color pink :quit-key "ESC")
	     		     ("File"
	     		      (("f" find-file "Open file (find-file)")
	     		       ("s" save-buffer "Save file (save-buffer)")
	     		       ("o" xah-open-in-external-app "Open thing under cursor outside emacs (xah-open-in-external-app)")
	     		       )
	     		      "Buffer"
	     		      (("b" switch-to-buffer "Switch buffer (switch-to-buffer)")
	     		       ("k" kill-buffer "Kill buffer (kill-buffer)")
	     		       ("r" revert-buffer "Revert buffer (revert-buffer)"))))
	     
	     
	     (pretty-hydra-define eepkm-help-and-customisation-hydra
	     		     (:title "Help and Customisation" :color amaranth :quit-key "ESC")
	     		     ("Help"
	     		      (("h" help-command "Help Prefix (help-command)")
	     		       ("f" describe-function "Describe Function (describe-function)")
	     		       ("v" describe-variable "Describe Variable (describe-variable)")
	     		       ("k" describe-key "Describe Key (describe-key)")
	     		       ("m" describe-mode "Describe Mode (describe-mode)"))
	     		      "Customize"
	     		      (("V" customize-variable "Customize Variable")
	     		       ("G" customize-group "Customize Group")
	     		       ("F" customize-face "Customize Face")
	     		       ("O" customize-option "Customize Option")
	     		       ("T" customize-themes "Customize Themes"))
	     		      "Documentation"
	     		      (("i" info "Info (info)")
	     		       ("e" view-echo-area-messages "View Messages (view-echo-area-messages)")
	     		       ("l" view-lossage "Key Lossage (view-lossage)"))
	     		      ))
	     
	     )

(defgroup eepkm-bindings nil
  "Customization subgroup for key bindings"
  :group 'eepkm  
  )

(defcustom eepkm-master-hydra "<f11>"
  "Key for `org-roam-node-find` in the eepkm-bindings PKM section."
  :type 'string
  :group 'eepkm)

(global-set-key (kbd eepkm-master-hydra) 'eepkm-master-hydra/body)

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
	     :hook (window-setup . vertico-mode)
	     :custom
	     (vertico-cycle t)
	     ;; :custom-face
	     ;; (vertico-current ((t (:background "#3a3f5a"))))
	     )

(use-package vertico-prescient
	     :custom
	     (vertico-prescient-enable-sorting t "Enable sorting in Vertico via Prescient")
	     (vertico-prescient-enable-filtering nil "Disable filtering in Vertico via Prescient")
	     (prescient-history-length 1000 "Set the history length for Prescient")
	     :hook ((vertico-mode . vertico-prescient-mode)
		    (vertico-prescient-mode . prescient-persist-mode)
		    ))  ; Automatically enable vertico-prescient-mode in vertico-mode

;; (use-package vertico-posframe
;; 	     :after vertico
;; 	     ;; :hook(vertico-mode . vertico-posframe-mode)
;; 	     :init 
;; 	     (vertico-posframe-mode)
;; 	     :config
;; 	     (setq
;; 	      vertico-posframe-poshandler #'posframe-poshandler-frame-top-center
;; 	      vertico-posframe-border-width 2
;; 	      vertico-posframe-width nil ;;pile la bonne largeur
;; 	      vertico-posframe-height nil ;;pile la bonne hauteur
;; )
;; )

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

(use-package org 
	     :ensure 
	     ;; (org :type git :repo "https://code.orgmode.org/bzg/org-mode.git")
	     (org :type git :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git" :branch "bugfix")
	     :init
	     (setq org-directory (concat user-emacs-directory "PKM/notes/"))
	     :config
	     
	     ;;Pour obtenir des polices proportionnelles
	     (add-hook 'org-mode-hook 'variable-pitch-mode)
	     
	     
	     (setq org-startup-with-inline-images t
	           ;; size of images
	           org-image-actual-width 1000
	           )
	     
	     
	     (defcustom eepkm-create-node-every-heading t
	       "If non-nil, after create a heading, create a node."
	       :type 'boolean
	       :group 'eepkm)
	     
	     (defun eepkm-org-insert-id ()
	       (let ((buffer-path (buffer-file-name))
	     	(roam-dir (expand-file-name org-roam-directory)))
	         (when (and buffer-path (string-prefix-p roam-dir buffer-path))
	           (save-excursion
	     	(org-back-to-heading)
	     	(org-id-get-create)))))
	     
	     (when eepkm-create-node-every-heading
	       (add-hook 'org-insert-heading-hook 'eepkm-org-insert-id))
	     
	     )

(defun eepkm-org-export-output-dir (orig-fun &rest args)
  "Modification of the export-output directory for Org-mode."
  (let ((old-default-directory default-directory))
    ;; Change working directory temporarily to 'export' directory.
    (setq default-directory (expand-file-name "PKM/data/export/" user-emacs-directory))
    (apply orig-fun args)
    ;; Restores original working directory after export.
    (setq default-directory old-default-directory)))

;; Applies directory modification function to all Org export functions.
(advice-add 'org-export-to-file :around #'eepkm-org-export-output-dir)

(setq org-ellipsis "⬎")

(add-hook 'org-mode-hook 'org-indent-mode)

(use-package org-modern
	       :init

	       (defcustom eepkm-org-modern-mode nil
		 "Toggle modern enhancements in Org mode."
		 :type 'boolean
		 :group 'eepkm)

	       (when eepkm-org-modern-mode
		 (add-hook 'org-mode-hook 'org-modern-mode)
		 (add-hook 'org-agenda-finalize-hook 'org-modern-agenda))

	       :config
	       (setq 
		;; don't hide the stars of heading
		org-modern-hide-stars nil
		org-hide-leading-stars t

org-modern-star '("◉" "○" "◈" "◇" "✳" "★" "☆" "▲" "△" "▼" "▽" "□" "■" "☐" "♦")

		;; desactivate code block
		org-modern-block-fringe nil
		org-modern-block-name nil

		)
	       )

(use-package org-appear
  :hook (org-mode . org-appear-mode)  ; Automatically enable org-appear-mode in org-mode
  :custom
  (org-appear-autolinks t "Automatically reveal the details of links")
  (org-appear-autoentities t "Automatically reveal the details of entities, see https://orgmode.org/manual/Special-Symbols.html")
  (org-appear-autosubmarkers t "Automatically reveal sub- and superscripts")
  :config
  ;; You can add any additional configuration that needs to be executed after the package is loaded here
  ;; For example, if you want to enable pretty entities globally, you could uncomment the following line:
  ;; (setq org-pretty-entities-include-sub-superscripts t)
  )

(setq org-attach-dir (concat user-emacs-directory "PKM/data/org-attach"))

;; each attached document go to the ID of the nodes

;;The first function in this list defines the preferred function which will be used when creating new attachment folders.
(setq org-attach-id-to-path-function-list
      '(eepkm-org-attach-id-uuid-folder-format
	;; org-attach-id-uuid-folder-format
	))

(defun eepkm-org-attach-id-uuid-folder-format (id)
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
	     (custom-set-variables '(warning-suppress-types '((magit))))
	     (setq org-roam-directory org-directory)
	     ;; automatic sync with files 
	     (org-roam-db-autosync-mode +1)
	     
	     (defcustom eepkm-include-tutorial t
	       "If non-nil, include the tutorial in the personal DB."
	       :type 'boolean
	       :group 'eepkm)
	     
	     (defvar eepkm-note-tutorial-directory (concat org-directory "Tutorial/"))
	     
	     (defun toggle-eepkm-include-tutorial ()
	       "Toggle the inclusion of tutorial notes in the Org Roam database."
	       (interactive)
	       (setq org-roam-db-node-include-function
	     	(if eepkm-include-tutorial
	     	    (lambda () t)  ; Always include the node.
	     	  (lambda ()
	     	    (let ((current-dir (file-name-directory (or buffer-file-name ""))))
	     	      (equal eepkm-note-tutorial-directory current-dir)))))  ; Check directory.
	       (message "Tutorial inclusion is now %s."
	     	   (if eepkm-include-tutorial "enabled" "disabled")))
	     
	     (defun switch-eepkm-include-tutorial ()
	       "Switch the value of `eepkm-include-tutorial` and update inclusion function."
	       (interactive)
	       (setq eepkm-include-tutorial (not eepkm-include-tutorial))
	       (toggle-eepkm-include-tutorial))  ; Call the toggle function to update the function based on the new value.
	     
	     
	     (setq org-roam-node-display-template " ${directory} ${hierarchy-light} ")
	     
	     
	     (cl-defmethod org-roam-node-directory ((node org-roam-node))
	       "Return the directory of the org-roam node, but only for tutorial directory."
	       (let ((file-path (org-roam-node-file node)))
	         (if (string-equal (file-name-nondirectory (directory-file-name (file-name-directory file-path))) "tutorial")
	     	"Tutorial"
	           (make-string (length "Tutorial") ?\s))))  ; Return an empty string if not in tutorial
	     
	     
	     (cl-defmethod org-roam-node-hierarchy-light ((node org-roam-node))
	       "Return a simple hierarchy for the given org-roam node."
	       (let ((olp (org-roam-node-olp node))
	             (title (org-roam-node-title node)))
	         (if olp
	             (concat (string-join olp " > ") " > " title)
	           title)))
	     
	           (defun eepkm-org-roam-get-parent-node ()
	           "Return the node of the current node at point, if any
	       Recursively traverses up the headline tree to find the parent node.
	       Take in accout if this is a file node."
	           (save-restriction
	     	(widen)
	     	(save-excursion
	     	  (let ((current-org-roam-node-id (org-roam-id-at-point)))
	     	    ;; move to the good place
	     	    (while (and 
	     		    (if (equal (org-roam-id-at-point) current-org-roam-node-id)
	     			t ; if this is the same node, say "continue"
	     		      (not (org-roam-db-node-p)) ; check if this is a node. If not, continue. If yes, stop
	     		      )
	     		    (not (bobp))		; but stop if this is the end of the file
	     		    )
	     	      ;; command to go up 
	     	      (org-roam-up-heading-or-point-min))
	     	    ;; now this is the good place
	     	    (let ((node-at-point (org-roam-node-at-point)))
	     	      (when (and (org-roam-db-node-p) ; check if we are at a node (that can be not the case with "ROAM_EXCLUDE" at the beginning of a file)
	     			 (not (equal (org-roam-node-id node-at-point) current-org-roam-node-id))) ; check if this if the node at point is not the same of the default
	     		node-at-point
	     		))))))
	     
	         (defun eepkm-org-roam-get-outline-path-with-aliases (&optional WITH-SELF USE-CACHE) ;argument to match the function org-get-outline-path
	           "Get the full outline path with aliases for the current headline. Take in account a file node."
	           ;; using the olp of the parent, because org-roam save node by files, from top to end
	           (let ((parent-node (eepkm-org-roam-get-parent-node)))
	     	(when parent-node
	     	  (let* ((aliases (org-roam-node-aliases parent-node))
	     		 (alias-str (if (> (length aliases) 0)
	     				;; first separator after title
	     				(concat ", " 
	     					(mapconcat 'identity aliases
	     						   ;; separator between aliases
	     						   ", "))
	     			      nil)))
	     	    ;; let's append the title at the end
	     	    (append (org-roam-node-olp parent-node) (list (concat (org-roam-node-title parent-node) alias-str))))))
	           )
	     
	         (defun eepkm-replace-org-get-outline-path-advice (orig-func &rest args)
	           "Temporarily override `org-get-outline-path` during `org-roam-db-insert-node-data` execution."
	           (cl-letf (((symbol-function 'org-get-outline-path)
	     		 (lambda (&optional with-self use-cache)
	     		   (eepkm-org-roam-get-outline-path-with-aliases with-self use-cache))))
	     	(apply orig-func args)))
	     
	         (advice-add 'org-roam-db-insert-node-data :around #'eepkm-replace-org-get-outline-path-advice)
	     
	     )

(use-package org-roam-ui
    :after org-roam
    :ensure
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

(if (file-exists-p (concat user-emacs-directory "personal.el"))
    (load (concat user-emacs-directory "personal.el") nil 'nomessage)
  (message "The personal configuration of the user [%s] is not present." (concat user-emacs-directory "personal.el")))
