;; Disable package.el in favor of elpaca
(setq package-enable-at-startup nil)

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
