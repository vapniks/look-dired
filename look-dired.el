;;; look-dired.el --- Extensions to look-mode for dired buffers

;; Filename: look-dired.el
;; Description: Extensions to look-mode for dired buffers
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2013, Joe Bloggs, all rites reversed.
;; Created: 2013-05-11 20:39:19
;; Version: 20151110.1832
;; Last-Updated: 2013-05-11 20:39:19
;;           By: Joe Bloggs
;; URL: https://github.com/vapniks/look-dired
;; Keywords: convenience
;; Compatibility: GNU Emacs 24.3.1
;; Package-Requires: ((look-mode "1.0"))
;;
;; Features that might be required by this library:
;;
;; look-mode cl find-dired
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: 
;; 
;; * Commentary
;; Bitcoin donations gratefully accepted: 1ArFina3Mi8UDghjarGqATeBgXRDWrsmzo
;; 
;; This library provides extra commands for [[http://www.emacswiki.org/emacs/LookMode][look-mode]] (see below).
;; In addition if you mark some files in a dired buffer and then run look-at-file (or press M-l), 
;; all of the marked files will be visited in the *look* buffer.
;; ** New commands
;; - look-dired-do-rename                  : Rename current looked file, to location given by TARGET.
;; - look-dired-unmark-looked-files        : Unmark all the files in `look-buffer' in the corresponding dired-mode buffer.
;; - look-dired-mark-looked-files          : Mark all the files in `look-buffer' in the corresponding dired-mode buffer.
;; - look-dired-mark-current-looked-file   : Mark `look-current-file' in the corresponding dired-mode buffer.
;; - look-dired-unmark-current-looked-file : Unmark `look-current-file' in the corresponding dired-mode buffer.
;; - look-dired-run-associated-program     : Run program associated with currently looked at file.
;; 
;;;;

;; (org-readme-sync)
;; (auto-document)
;;; Commands:
;;
;; Below is a complete list of commands:
;;
;;  `look-dired-do-rename'
;;    Rename current looked file, to location given by TARGET.
;;    Keybinding: M-r
;;  `look-dired-unmark-looked-files'
;;    Unmark all currently looked at files in the corresponding `dired-mode' buffer.
;;    Keybinding: M-U
;;  `look-dired-mark-looked-files'
;;    Mark all currently looked at files in the corresponding dired-mode buffer.
;;    Keybinding: M-M
;;  `look-dired-mark-current-looked-file'
;;    Mark `look-current-file' in the corresponding dired-mode buffer.
;;    Keybinding: M-m
;;  `look-dired-unmark-current-looked-file'
;;    Unmark `look-current-file' in the corresponding dired-mode buffer.
;;    Keybinding: M-u
;;  `look-dired-run-associated-program'
;;    Run program associated with currently looked at file.
;;    Keybinding: M-RET
;;  `look-dired-dired'
;;    Jump to a dired buffer containing the looked at files.
;;    Keybinding: M-D
;;  `look-dired-find'
;;    Run `find' and view found files in `look-mode' buffer.
;;    Keybinding: M-x look-dired-find
;;
;;; Customizable Options:
;;
;; Below is a list of customizable options:
;;


;;; Installation:
;;
;; Put look-dired.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'look-dired)

;;; Change log:
;; 10-Nov-2015      
;;    Last-Updated: 2013-05-11 20:39:19 (Joe Bloggs)
;;    ;;    
;; 10-Nov-2015      
;;    Last-Updated: 2013-05-11 20:39:19 (Joe Bloggs)
;;    ;;    
;;	
;; 2013/05/11
;;      * First released.
;; 

;;; Acknowledgements:
;;
;; Peter H. Mao <peter.mao@gmail.com> <peterm@srl.caltech.edu> (creator of look-mode)
;;

;;; TODO
;;
;;

;;; Require
(require 'cl-lib)
(require 'look-mode)
(require 'find-dired)

;;; Code:

(defvar-local look-dired-rename-target nil
  "The target of `look-current-file' in `look-dired-do-rename'.")
;; TODO make-local-variable
(defvar-local look-dired-buffer nil
  "The associated `dired-mode' buffer, from which `look-at-files' is called.")

;; Keybindings for look-dired commands
(define-key look-minor-mode-map (kbd "M-r") 'look-dired-do-rename)
(define-key look-minor-mode-map (kbd "M-m") 'look-dired-mark-current-looked-file)
(define-key look-minor-mode-map (kbd "M-M") 'look-dired-mark-looked-files)
(define-key look-minor-mode-map (kbd "M-u") 'look-dired-unmark-current-looked-file)
(define-key look-minor-mode-map (kbd "M-U") 'look-dired-unmark-looked-files)
(define-key look-minor-mode-map (kbd "M-RET") 'look-dired-run-associated-program)
(define-key look-minor-mode-map (kbd "M-D") 'look-dired-dired)

(defadvice look-reset-variables (after look-dired-reset-variables activate)
  "Reset `look-dired-rename-target' and `look-dired-buffer'."
  (setq look-dired-rename-target nil)
  (setq look-dired-buffer nil))

;;; Useful helper functions
(defsubst look-file-list nil
  "Return the list of looked at files in current buffer in order."
  (append (reverse look-reverse-file-list)
	  (if look-current-file (list look-current-file))
	  look-forward-file-list))

;;;; Navigation Commands
;; Redefine look-modes `look-at-files' command
;; TODO: fix to work with buffer-local vars
;;;###autoload
(cl-defun look-at-files (look-wildcard &optional add name dired-buffer file-list index)
  "Look at files in a directory.  Insert them into a temporary buffer one at a time. 
This function gets the file list and passes it to `look-at-next-file'.
When called interactively, if the current directory is a dired buffer containing 
marked files then those files will be used, otherwise a filename with wildcards
will be prompted for to match the files to be used. The name of an existing look 
buffer or new buffer is prompted for. If an existing look buffer is chosen then 
the files will be added to those in that buffer.

When called programmatically, you can either supply a filename with wildcards to
the LOOK-WILDCARD argument, or a dired buffer containing marked files as the 
DIRED-BUFFER argument, or a list of files as the FILE-LIST argument.
If ADD is non-nil then files are added to the end of the currently looked at files
in buffer NAME (default \"*look*<N>\"), otherwise they replace them.
If INDEX is non-nil then goto the INDEX'th file in the list initially."
  (interactive (let* ((diredp (and (eq major-mode 'dired-mode)
				   (look-dired-has-marked-file)))
		      (dired-buffer (if diredp (current-buffer) nil))
		      (wildcard (if diredp nil
				  (read-from-minibuffer "Enter filename (w/ wildcards): ")))
		      (name (ido-completing-read
			     "Look buffer: "
			     (append '("new buffer") (look-buffer-list))))
		      (add (not (equal name "new buffer")))
		      (name2 (if add name
			       (generate-new-buffer-name
				(read-string "Look buffer name: " "*look*:")))))
		 (list wildcard add name2 dired-buffer nil)))
  ;; first check the cache directory isn't too full
  (let ((totalsize (look-get-cache-directory-size)))
    (if (and (> totalsize look-cache-directory-size)
	     (y-or-n-p (format "Cache directory is using more than %d bytes! Remove old files? "
			       look-cache-directory-size)))
	(let* ((sortedfiles (sort (directory-files-and-attributes look-cache-directory t nil t)
				  (lambda (a b) (time-less-p (nth 4 (cdr a)) (nth 4 (cdr b)))))))
	  (cl-loop for (file . attrib) in sortedfiles
		   with sum = (- totalsize look-cache-directory-size)
		   if (> sum 0) do (delete-file file)
		   (setq sum (- sum (nth 7 attrib)))))))
  ;; load eimp if necessary
  (if (and look-wildcard (not (featurep 'eimp))
       (string-match (regexp-opt (mapcar 'symbol-name image-types))
			 look-wildcard))
      (require 'eimp nil t))
  ;; if no wildcard is supplied match everything with *  
  (if (and look-wildcard (string= look-wildcard ""))
      (setq look-wildcard "*"))
  ;; get look buffer
  (switch-to-buffer (or name (generate-new-buffer-name "*look*")))
  ;; reset buffer local variables
  (unless add
      (setq look-forward-file-list nil
	    look-reverse-file-list nil
	    look-current-file nil))
  (setq look-dired-buffer dired-buffer
	look-subdir-list (list "./")
	look-pwd (replace-regexp-in-string 
		  "^~" (getenv "HOME")
		  (replace-regexp-in-string "^Directory " "" (pwd)))
	look-dired-rename-target nil)
  (let ((look-file-list (or file-list
			    (and dired-buffer
				 (with-current-buffer dired-buffer
				   (look-dired-has-marked-file))
				 (mapcar (lambda (file)
					   (replace-regexp-in-string (concat "^" look-pwd) "" file))
					 (with-current-buffer dired-buffer
					   (dired-get-marked-files))))
			    (and look-wildcard
				 (file-expand-wildcards look-wildcard))
			    (error "No files supplied")))
        (fullpath-dir-list nil))
    ;; use relative file names to prevent weird side effects with skip lists
    ;; cat look-pwd with filename, separate dirs from files,
    ;; remove files/dirs that match elements of the skip lists ;;
    (dolist (lfl-item look-file-list)
      (if (and (file-regular-p lfl-item)
               ;; check if any regexps in skip list match filename
               (catch 'skip-this-one 
                 (dolist (regexp look-skip-file-list t)
                   (if (string-match regexp lfl-item)
                       (throw 'skip-this-one nil)))))
          (setq look-forward-file-list
                (nconc look-forward-file-list
                       (list (if (file-name-absolute-p lfl-item) lfl-item
			       (concat look-pwd lfl-item)))))
        (if (and (file-directory-p lfl-item)
                 ;; check if any regexps in skip list match directory
                 (catch 'skip-this-one 
                   (dolist (regexp look-skip-directory-list t)
                     (if (string-match regexp lfl-item)
                         (throw 'skip-this-one nil)))))
            (if look-recurse-dirlist
                (setq fullpath-dir-list
                      (nconc fullpath-dir-list
                             (list lfl-item)
                             (list-subdirectories-recursively
                              (if (file-name-absolute-p lfl-item)
				  lfl-item
				(concat look-pwd lfl-item))
			      look-skip-directory-list)))
              (setq fullpath-dir-list
                    (nconc fullpath-dir-list
                           (list lfl-item)))))))
    ;; now strip look-pwd off the subdirs in subdirlist    
    ;; or maybe I should leave everything as full-path....
    (dolist (fullpath fullpath-dir-list)
      (setq look-subdir-list
            (nconc look-subdir-list
                   (list (file-name-as-directory
                          (replace-regexp-in-string look-pwd "" fullpath)))))))
  (look-mode)
  (look-at-nth-file (or index 1)))

;;;###autoload
(defun look-dired-has-marked-file nil
  "Return `t' if there are marked files in current dired buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((next-position (and (re-search-forward (dired-marker-regexp) nil t)
			      (point-marker))))
      (not (null next-position)))))

;;;###autoload
(defun look-dired-do-rename (&optional target prompt prefix suffix)
  "Rename current looked file, to location given by TARGET.
`look-current-file' will be removed from the file list if the rename succeeds.
When TARGET is `nil' or prompt is non-nil, prompt for the location.
PREFIX and SUFFIX specify strings to be placed before and after the cursor in the prompt,
 (but after the target dir). PREFIX or SUFFIX may also be functions that take a single string 
argument and return a string. In this case they will be called with the argument set to the 
filename of the current looked file (without the directory part).
If PROMPT is nil the file will be moved to this directory while retaining the same filename, unless 
PREFIX and/or SUFFIX are non-nil in which case the filename will be changed to the concatenation of 
PREFIX and SUFFIX.
This command also renames any buffers that are visiting the files.
The default suggested for the target directory depends on the value of `dired-dwim-target' (usually 
the directory in which the current file is located)."
  (interactive)
  (let ((prompt (if (null target) t prompt))
	(pre (if (functionp prefix) 
		 (funcall prefix (file-name-nondirectory look-current-file))
	       prefix))
	(post (if (functionp suffix) 
		  (funcall suffix (file-name-nondirectory look-current-file))
		suffix)))
    (look-dired-do-create-file 'move (function dired-rename-file)
			       "Move" nil dired-keep-marker-rename "Rename" prompt target nil pre post)
    ;; if `look-current-file' exists, it means there is no need to delete `look-current-file'
    (unless (file-exists-p look-current-file)
      (setq look-current-file nil)
      (look-at-next-file))))

;;; This is a modified version of `dired-do-create-files'
;;; TODO: Should `look-current-file' be updated after the moving? Now
;;; it'll result in an error if there are two continous rename while the
;;; first op rename the file to a different directory.
;;;###autoload
(defun look-dired-do-create-file (op-symbol file-creator operation arg
					    &optional marker-char op1 prompt target-file
					    how-to prefix suffix)
  "Create a new file for `look-current-file'.
Prompts user for target, which is a directory in which to create the new files. 
Target may also be a plain file if only one marked file exists.  
The way the default for the target directory is computed depends on the value 
of `dired-dwim-target-directory'.
OP-SYMBOL is the symbol for the operation.  Function `dired-mark-pop-up'
will determine whether pop-ups are appropriate for this OP-SYMBOL.
FILE-CREATOR and OPERATION as in `dired-create-files'.
ARG as in `dired-get-marked-files'.
Optional arg MARKER-CHAR as in `dired-create-files'.
Optional arg OP1 is an alternate form for OPERATION if there is only one file.
Optional arg PROMPT determines whether prompts for the target location.
`nil' means not prompt and TARGET-FILE is the target location, non-nil
means prompt for the target location.
Optional arg HOW-TO determiness how to treat the target.
If HOW-TO is nil, use `file-directory-p' to determine if the
target is a directory.  If so, the marked file(s) are created
inside that directory.  Otherwise, the target is a plain file;
an error is raised unless there is exactly one marked file.
If HOW-TO is t, target is always treated as a plain file.
Otherwise, HOW-TO should be a function of one argument, TARGET.
If its return value is nil, TARGET is regarded as a plain file.
If it return value is a list, TARGET is a generalized
directory (e.g. some sort of archive).  The first element of
this list must be a function with at least four arguments:
 operation - as OPERATION above.
 rfn-list  - list of the relative names for the marked files.
 fn-list   - list of the absolute names for the marked files.
 target    - the name of the target itself.
The rest of into-dir are optional arguments.
For any other return value, TARGET is treated as a directory."
  (or op1 (setq op1 operation))
  (let* ((fn-list (list look-current-file))
	 (rfn-list (mapcar (function dired-make-relative) fn-list))
	 (dired-one-file	; fluid variable inside dired-create-files
	  (and (consp fn-list) (null (cdr fn-list)) (car fn-list)))
	 (target-dir (if (and target-file (file-name-directory target-file)) (file-name-directory target-file)
		       (file-name-directory look-current-file)))
	 (default (if target-file (concat target-dir prefix suffix)
		    (and dired-one-file
			 (expand-file-name (if (or prefix suffix) (concat prefix suffix)
					     (file-name-nondirectory (car fn-list)))
					   target-dir))))
	 (target (if (null prompt)
		     (concat target-file prefix suffix)
		   (expand-file-name   ; fluid variable inside dired-create-files
		    (look-dired-mark-read-file-name
		     (concat (if dired-one-file op1 operation) " %s to: ")
		     (concat target-dir prefix) op-symbol arg rfn-list default suffix))))
	 (into-dir (cond ((null how-to)
			  ;; Allow DOS/Windows users to change the letter
			  ;; case of a directory.  If we don't test these
			  ;; conditions up front, file-directory-p below
			  ;; will return t because the filesystem is
			  ;; case-insensitive, and Emacs will try to move
			  ;; foo -> foo/foo, which fails.
			  (if (and (memq system-type '(ms-dos windows-nt cygwin))
				   (eq op-symbol 'move)
				   dired-one-file
				   (string= (downcase
					     (expand-file-name (car fn-list)))
					    (downcase
					     (expand-file-name target)))
				   (not (string=
					 (file-name-nondirectory (car fn-list))
					 (file-name-nondirectory target))))
			      nil
			    (file-directory-p target)))
			 ((eq how-to t) nil)
			 (t (funcall how-to target)))))
    (if (and (consp into-dir) (functionp (car into-dir)))
	(apply (car into-dir) operation rfn-list fn-list target (cdr into-dir))
      (unless (or dired-one-file into-dir)
	(error "Marked %s: target must be a directory: %s" operation target))
      ;; rename-file bombs when moving directories unless we do this:
      (or into-dir (setq target (directory-file-name target)))
      (if into-dir
	  (setq look-dired-rename-target
		(expand-file-name (file-name-nondirectory look-current-file) target))
	(setq look-dired-rename-target target))
      (dired-create-files
       file-creator operation fn-list
       (if into-dir			; target is a directory
	   ;; This function uses fluid variable target when called
	   ;; inside dired-create-files:
	   (function
	    (lambda (from)
	      (expand-file-name (file-name-nondirectory from) target)))
	 (function (lambda (from) target)))
       marker-char))))

;;; This is a modified version of `dired-mark-read-file-name'
;;; I have added and extra arg `initial' which specifies an initial part 
;;; of the filename (after the cursor position) in the prompt.
;;;###autoload
(defun look-dired-mark-read-file-name (prompt dir op-symbol arg files
					 &optional default initial)
  (dired-mark-pop-up
   nil op-symbol files
   (function read-file-name)
   (format prompt (dired-mark-prompt arg files)) dir default nil initial))

;;;;;;;;;; Look dired mark/unmark commands ;;;;;;;;;;;
;;;###autoload
(defun look-dired-unmark-looked-files ()
  "Unmark all currently looked at files in the corresponding `dired-mode' buffer.
This is only meaningful when the *look* buffer has an associated `dired-mode' buffer,
i.e. `look-at-files' is called from a `dired-mode' buffer."
  (interactive)
  (when look-dired-buffer
    (let ((file-list (look-file-list)))
      (mapc #'look-dired-unmark-file file-list))
    (message "Unmarked all looked at files in dired buffer")))

;;;###autoload
(defun look-dired-mark-looked-files ()
  "Mark all currently looked at files in the corresponding dired-mode buffer.
This is only meaningful when the *look* has an associated `dired-mode' buffer,
i.e. `look-at-files' is called from a dired-mode buffer."
  (interactive)
  (when look-dired-buffer
    (let ((file-list (look-file-list)))
      (mapc #'look-dired-mark-file file-list))
    (message "Marked all looked at files in dired buffer")))

;;;###autoload
(defun look-dired-mark-current-looked-file (&optional show-next-file)
  "Mark `look-current-file' in the corresponding dired-mode buffer.
When SHOW-NEXT-FILE is non-nil, the next file will be looked at.
Similar to `look-dired-unmark-looked-files', this function only works when
the *look* has an associated dired-mode buffer."
  (interactive)
  (when look-dired-buffer
    (look-dired-mark-file look-current-file)
    (message (concat "Marked " (file-name-nondirectory look-current-file) " in dired buffer"))
    (when show-next-file (look-at-next-file))))

;;;###autoload
(defun look-dired-unmark-current-looked-file (&optional show-next-file)
  "Unmark `look-current-file' in the corresponding dired-mode buffer.
When SHOW-NEXT-FILE is non-nil, the next file will be looked at.
Similar to `look-dired-unmark-looked-files', this function only works when
 the *look* has an associated dired-mode buffer."
  (interactive)
  (when look-dired-buffer
    (look-dired-unmark-file look-current-file)
    (message (concat "Unmarked " (file-name-nondirectory look-current-file) " in dired buffer"))
    (when show-next-file (look-at-next-file))))

;;;###autoload
(defun look-dired-mark-file (file)
  "`dired-mark' FILE in `look-dired-buffer'."
  (assert look-dired-buffer)
  (if (buffer-live-p (if (stringp look-dired-buffer)
			 (get-buffer look-dired-buffer)
		       look-dired-buffer))
      (save-excursion
	(set-buffer look-dired-buffer)
	(goto-char (point-min))
	(block nil
	  (while (not (eobp))
	    (when (and (not (looking-at dired-re-dot))
		       (not (eolp))
		       (let ((fn (dired-get-filename nil t)))
			 (and fn (string= fn file))))
	      (dired-mark 1)
	      (return-from nil))
	    (forward-line 1))))
    (error "No %s buffer available"
	   (if (stringp look-dired-buffer) look-dired-buffer
	     (buffer-name look-dired-buffer)))))

;;;###autoload
(defun look-dired-run-associated-program nil
  "Run program associated with currently looked at file.
Requires run-assoc library."
  (interactive)
  (require 'run-assoc)
  (run-associated-program look-current-file))

;;;###autoload
(defun look-dired-unmark-file (file)
  "`dired-unmark' FILE in `look-dired-buffer'."
  (assert look-dired-buffer)
  (if (buffer-live-p (if (stringp look-dired-buffer)
			 (get-buffer look-dired-buffer)
		       look-dired-buffer))
    (save-excursion
      (set-buffer look-dired-buffer)
      (goto-char (point-min))
      (block nil
	(while (not (eobp))
	  (when (and (not (looking-at dired-re-dot))
		     (not (eolp))
		     (let ((fn (dired-get-filename nil t)))
		       (and fn (string= fn file))))
	    (dired-unmark 1)
	    (return-from nil))
	  (forward-line 1))))
    (error "No %s buffer available"
	   (if (stringp look-dired-buffer) look-dired-buffer
	     (buffer-name look-dired-buffer)))))

;;;###autoload
(defun look-dired-dired nil
  "Jump to a dired buffer containing the looked at files.
If such a buffer does not already exist, create one."
  (interactive)
  (if (buffer-live-p (if (stringp look-dired-buffer)
			 (get-buffer look-dired-buffer)
		       look-dired-buffer))
      (switch-to-buffer look-dired-buffer)
    (setq look-dired-buffer
	  (generate-new-buffer-name
	   (replace-regexp-in-string "^*look" "*look-dired" (buffer-name))))
    (dired (cons look-dired-buffer (look-file-list)))))

;;;###autoload
(defun look-dired-find (dir args)
  "Run `find' and view found files in `look-mode' buffer.
The `find' command run (after changing into DIR) is:

    find . \\( ARGS \\) -ls
Warning: this will lock up Emacs if the find command is slow to return.
Instead you can use `find-dired', then mark all the files, and look at
them with `look-dired'."
  (interactive
   (let ((default (and find-dired-default-fn
                       (funcall find-dired-default-fn))))
     (list (ido-read-file-name "Run `find' in directory: " nil "" t)
           (read-from-minibuffer "Run `find' (with args): " default
                                 nil nil 'find-args-history default t))))
  (find-dired dir args)
  (while (eq 'run (process-status "find")) (sleep-for 0.2))
  (dired-mark-unmarked-files ".*" nil)
  (let ((buf (current-buffer)))
    (look-at-files nil nil (remove nil
				   (mapcar (lambda (x)
					     (car (remove x (file-expand-wildcards
							     (replace-regexp-in-string "\\.txt$" ".*" x)))))
					   (dired-get-marked-files))))
    (kill-buffer buf)))


(provide 'look-dired)

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "look-dired.el" (buffer-name) (buffer-string) "update")

;;; look-dired.el ends here

