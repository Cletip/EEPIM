(defun open-my-startup-file ()
  "Open a specific file and maximize the Emacs window on startup."
  (find-file (concat user-emacs-directory "init.org"))  ; Change the path to your specific file
  (delete-other-windows))

;; Add the custom startup function to the Emacs startup hook
(add-hook 'emacs-startup-hook 'open-my-startup-file)


;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
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


;; Configure use-package to use straight.el by default
(use-package straight
             :custom
             (straight-use-package-by-default t))

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)


  (use-package hydra
    :straight t)

(use-package pretty-hydra
  :after hydra
  :straight t)

(pretty-hydra-define my-hydra
  (:title "My Hydra" :color amaranth :quit-key "q")
  ("Buffer"
   (("b" switch-to-buffer "switch")
    ("B" ibuffer "ibuffer")
    ("k" kill-buffer "kill")
    ("s" save-buffer "save"))
   "Window"
   (("n" next-window "next window")
    ("p" previous-window "prev window")
    ("d" delete-window "delete window")
    ("m" maximize-window "maximize")
    ("S" shrink-window "shrink vertically")
    ("V" enlarge-window "enlarge vertically")
    ("<" shrink-window-horizontally "shrink horizontally")
    (">" enlarge-window-horizontally "enlarge horizontally"))))


(use-package which-key
	     :straight t
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
	     (vertico-mode 1))

;; annotation in the minibuffer
(use-package marginalia
	     :init
	     (marginalia-mode 1))

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

(load (concat user-emacs-directory "personal.el"))
