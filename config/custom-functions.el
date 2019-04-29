;; ~/.emacs.d/custom-functions.el
;; Some customized functions to make Emacs even better.
;;
;; Author: Nathan Campos <nathanpc@dreamintech.net>

(defun indent-buffer ()
  "Indent a whole buffer"
  (interactive)
  (indent-region (point-min) (point-max)))

(defun c-doc-block-comment-return ()
  "Auto-completion of documentation comment blocks after RET"
  (interactive)
  (message "Newline")
  (setq last (point))
  (setq is-inside                         ; Check if inside a valid comment.
        (if (search-backward "*/" nil t)  ; Check if there are endings before.
            (search-forward "/*" last t)  ; Check if it's a new comment block.
          (goto-char last)                ; Go back to the original point.
          (search-backward "/*" nil t)))  ; Check if we are really inside.
  (goto-char last)                        ; Reset cursor position again.
  (if is-inside
      (progn
		(insert "\n* ")                   ; Insert "* " with the newline.
		(indent-for-tab-command))         ; Indent comment properly.
    (insert "\n")))                       ; Not inside, insert the newline only.

(defun c-doc-block-comment-end-slash ()
  "End documentation comment blocks easily after hitting '/'"
  (interactive)
  (setq last (point))
  (message "Hello!")
  (setq is-inside                         ; Check if inside a valid comment.
        (if (search-backward "*/" nil t)  ; Check if there are endings before.
            (search-forward "/*" last t)  ; Check if it's a new comment block.
          (goto-char last)                ; Go back to the original point.
          (search-backward "/*" nil t)))  ; Check if we are really inside.
  (goto-char last)                        ; Reset cursor position again.
  (if is-inside
	  (if (search-backward "* " nil t)            ; At an empty comment block line.
		  (progn
			(goto-char last)                      ; Reset cursor after search.
			(backward-delete-char-untabify 1))))  ; Delete the trailling whitespace.
  (insert "/"))                                   ; Insert the typed character.

(provide 'custom-functions)
