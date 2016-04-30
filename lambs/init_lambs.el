;;; -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'cl)

(global-set-key (kbd "C-d") (lambda () (interactive) (insert "\u225C")))
(global-set-key (kbd "C-l") (lambda () (interactive) (insert "\u03bb")))
(global-set-key (kbd "C-S-l") (lambda () (interactive) (insert "\u039b")))
(global-set-key (kbd "C-p") (lambda () (interactive) (insert "\u03C0")))
(global-set-key (kbd "C-S-p") (lambda () (interactive) (insert "\u03A0")))
(global-set-key (kbd "C-o") (lambda () (interactive) (insert "\u03C9")))
(global-set-key (kbd "C-S-o") (lambda () (interactive) (insert "\u03A9")))
(global-set-key (kbd "C--") (lambda () (interactive) (insert "\u2192")))

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(setq slidel 60)

(setq reststring "")
(setq lamb-sml nil)

(defun slide (x &optional p)
  (let ((line (make-string slidel ?-))
        (fname (format "%slambs/slides/%s.txt" lambda-homedir x)))
    (if (file-exists-p fname)
      (progn
        (insert "\n")
        (insert line)
        (insert "\n")
        (insert (get-string-from-file fname))
        (insert "\n")
        (if p (progn
                (insert-image (create-image (format "%slambs/slides/%s.png" lambda-homedir p)))
                (insert "\n")))
        (insert line)
        (insert "\n"))
      (insert "\n:("))))

(defun pic (x)
  (insert "\n")
  (insert-image (create-image (format "%slambs/slides/%s.png" lambda-homedir x)))
  (insert "\n"))

(let ((lambd-rpls '(("\u225C" ":=")
                    ("\u03bb" "#l")
                    ("\u039b" "#L")
                    ("\u03C0" "#p")
                    ("\u03A0" "#P")
                    ("\u03C9" "_o")
                    ("\u03A9" "_O")
                    ("\u2192" "->")))
      (lambdabuffer "lambda"))
  (labels ((elam->smlam (s)
           (cl-reduce (lambda (s c)
                        (replace-regexp-in-string (car c) (cadr c) s)) lambd-rpls :initial-value s))

         (smlam->elam (s)
           (let ((case-fold-search nil))
             (cl-reduce (lambda (s c)
                          (replace-regexp-in-string (cadr c) (car c) s)) lambd-rpls :initial-value s))))

        (defun lambd-print-string-sml (str)
          (insert str))

        (defun lambd-print-string-lamb (str)
          (let ((strs (split-string (concat reststring str) "#e")))
            (while (cdr strs)
              (if (equal (car strs) "#w")
                (insert "\nWrong!") ;(pic "wrong")
                (insert (smlam->elam (car strs))))
              (setq strs (cdr strs)))
            (setq reststring (car strs))))

        (defun lambd-print-string (str)
          (if lamb-sml
              (lambd-print-string-lamb str)
              (lambd-print-string-sml str)))

        (defun exec-lamb (command)
          (let ((s (concat command (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
            (goto-char (line-end-position))
            (if lamb-sml nil (insert "\n"))
            (process-send-string "lambdproc" (format "%s\n" (elam->smlam s)))))

        (defun exec-lamb-rename ()
          (interactive)
          (exec-lamb "#rename "))

        (defun exec-lamb-default ()
          (interactive)
          (exec-lamb ""))

        (defun exec-lamb-1000 ()
          (interactive)
          (exec-lamb "#eval1000 "))
        
        (defun stop-lamb ()
          (interactive)
          (kill-process "lambdproc"))

        (defun start-stuff ()

          (rename-buffer "lambda")
          (setq lambdabuffer "lambda")
          (setq reststring "")

          (start-process "lambdproc" (current-buffer) "sml")
          (set-process-filter (get-process "lambdproc")
                              (lambda (p s)
                                (let ((v (get-buffer-window lambdabuffer)))
                                  (if v
                                      (with-selected-window v
                                        (lambd-print-string s))
                                      (with-current-buffer (get-buffer lambdabuffer)
                                        (lambd-print-string s))))))
          (global-set-key (kbd "C-r") 'exec-lamb-rename)
          (global-set-key (kbd "C-e") 'exec-lamb-default)
          (global-set-key (kbd "C-S-e") 'exec-lamb-1000)
          (insert "\n")
          (process-send-string "lambdproc" "use \"unt.sml\";\n"))

        (defun start-lamb ()
          (setq lamb-sml 't)
          (insert "\n")
          (process-send-string "lambdproc" "repl.repl ();\n"))
        (defun stop-lamb ()
          (setq lamb-sml nil)
          (insert "\n")
          (process-send-string "lambdproc" "#end\n"))))
