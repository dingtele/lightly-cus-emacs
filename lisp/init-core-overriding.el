(defun desktop-save (dirname &optional release only-if-changed version)
  "Save the state of Emacs in a desktop file in directory DIRNAME.
Optional argument RELEASE non-nil says we're done with this
desktop, in which case this function releases the lock of the
desktop file in DIRNAME.
If ONLY-IF-CHANGED is non-nil, compare the current desktop
information to that in the desktop file, and if the desktop
information has not changed since it was last saved, then do
not rewrite the file.

To restore the desktop, use `desktop-read'.

This function can save the desktop in either format version
208 (which only Emacs 25.1 and later can read) or version
206 (which is readable by any Emacs from version 22.1 onwards).
By default, it will use the same format the desktop file had when
it was last saved, or version 208 when writing a fresh desktop
file.

To upgrade a version 206 file to version 208, call this command
explicitly with a prefix argument: \\[universal-argument] \\[desktop-save].
If you are upgrading from Emacs 24 or older, we recommend to do
this once you decide you no longer need compatibility with versions
of Emacs before 25.1.

To downgrade a version 208 file to version 206, use a double prefix
argument: \\[universal-argument] \\[universal-argument] \\[desktop-save].

Emacs will ask for confirmation when you upgrade or downgrade your
desktop file.

In a non-interactive call, VERSION can be given as an integer, either
206 or 208, to specify the format version in which to save the file,
no questions asked."
  (interactive (list
                ;; Or should we just use (car desktop-path)?
                (let ((default (car desktop-path)))
                  (read-directory-name "Directory to save desktop file in: "
                                       default default t))
                nil
                nil
                current-prefix-arg))
  (setq desktop-dirname (file-name-as-directory (expand-file-name dirname)))
  (save-excursion
    (let ((eager desktop-restore-eager)
	  (new-modtime (file-attribute-modification-time
			(file-attributes (desktop-full-file-name)))))
      (when
	  (or (not new-modtime)		; nothing to overwrite
	      (time-equal-p desktop-file-modtime new-modtime)
	      (yes-or-no-p (if desktop-file-modtime
			       (if (time-less-p desktop-file-modtime
						new-modtime)
				   "Desktop file is more recent than the one loaded.  Save anyway? "
				 "Desktop file isn't the one loaded.  Overwrite it? ")
			     "Current desktop was not loaded from a file.  Overwrite this desktop file? "))
	      (unless release (error "Desktop file conflict")))

	;; If we're done with it, release the lock.
	;; Otherwise, claim it if it's unclaimed or if we created it.
	(if release
	    (desktop-release-lock)
	  (unless (and new-modtime (desktop-owner)) (desktop-claim-lock)))

        ;; What format are we going to write the file in?
        (setq desktop-io-file-version
              (cond
               ((equal version '(4))
                (if (or (eq desktop-io-file-version 208)
                        (yes-or-no-p "Save desktop file in format 208 \
\(Readable by Emacs 25.1 and later only)? "))
                    208
                  (or desktop-io-file-version desktop-native-file-version)))
               ((equal version '(16))
                (if (or (eq desktop-io-file-version 206)
                        (yes-or-no-p "Save desktop file in format 206 \
\(Readable by all Emacs versions since 22.1)? "))
                    206
                  (or desktop-io-file-version desktop-native-file-version)))
               ((memq version '(206 208))
                version)
               ((null desktop-io-file-version) ; As yet, no desktop file exists.
                desktop-native-file-version)
               (t
                desktop-io-file-version)))

	(with-temp-buffer
	  (insert
	   ";; -*- mode: emacs-lisp; lexical-binding:t; coding: utf-8-emacs; -*-\n"
	   desktop-header
	   ";; Created " (current-time-string) "\n"
	   ";; Desktop file format version " (format "%d" desktop-io-file-version) "\n"
	   ";; Emacs version " emacs-version "\n")
	  (save-excursion (run-hooks 'desktop-save-hook))
	  (goto-char (point-max))
	  (insert "\n;; Global section:\n")
	  ;; Called here because we save the window/frame state as a global
	  ;; variable for compatibility with previous Emacsen.
	  (desktop-save-frameset)
	  (unless (memq 'desktop-saved-frameset desktop-globals-to-save)
	    (desktop-outvar 'desktop-saved-frameset))
	  (mapc #'desktop-outvar desktop-globals-to-save)
	  (setq desktop-saved-frameset nil) ; after saving desktop-globals-to-save
	  (when (memq 'kill-ring desktop-globals-to-save)
	    (insert
	     "(setq kill-ring-yank-pointer (nthcdr "
	     (int-to-string (- (length kill-ring) (length kill-ring-yank-pointer)))
	     " kill-ring))\n"))

	  (insert "\n;; Buffer section -- buffers listed in same order as in buffer list:\n")
	  (dolist (l (mapcar #'desktop-buffer-info (buffer-list)))
	    (let ((base (pop l)))
	      (when (apply #'desktop-save-buffer-p l)
		(insert "("
			(if (or (not (integerp eager))
				(if (zerop eager)
				    nil
				  (setq eager (1- eager))))
			    "desktop-create-buffer"
			  "desktop-append-buffer-args")
			" "
			(format "%d" desktop-io-file-version))
		;; If there's a non-empty base name, we save it instead of the buffer name
		(when (and base (not (string= base "")))
		  (setcar (nthcdr 1 l) base))
		(dolist (e l)
		  (insert "\n  " (desktop-value-to-string e)))
		(insert ")\n\n"))))

	  (setq default-directory desktop-dirname)
	  ;; When auto-saving, avoid writing if nothing has changed since the last write.
	  (let* ((beg (and only-if-changed
			   (save-excursion
			     (goto-char (point-min))
			     ;; Don't check the header with changing timestamp
			     (and (search-forward "Global section" nil t)
				  ;; Also skip the timestamp in desktop-saved-frameset
				  ;; if it's saved in the first non-header line
				  (search-forward "desktop-saved-frameset"
						  (line-beginning-position 3) t)
				  ;; This is saved after the timestamp
				  (search-forward (format "%S" desktop--app-id) nil t))
			     (point))))
		 (checksum (and beg (md5 (current-buffer) beg (point-max) 'utf-8-emacs))))
	    (unless (and checksum (equal checksum desktop-file-checksum))
	      (let ((coding-system-for-write 'utf-8-emacs))
		(write-region (point-min) (point-max) (desktop-full-file-name) nil 'nomessage))
	      (setq desktop-file-checksum checksum)
	      ;; We remember when it was modified (which is presumably just now).
	      (desktop--get-file-modtime))))))))

(provide 'init-core-overriding)
