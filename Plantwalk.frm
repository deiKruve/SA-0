; -*-mode: forms;-*-

(require 'sort)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The hard coded variables.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This forms format file visits `Plantwalk.csv'.
(setq forms-file "Plantwalk.csv")
;; date field number, change according to 'forms-format-list'
(setq pw-date-field 20)
;; reference number field, change according to 'forms-format-list'
(setq pw-number-field 4)
;; equipment code field, change according to 'forms-format-list'
(setq pw-code-field 2)
;; photo hyperlink field, change according to 'forms-format-list'
(setq pw-photo-field 18)
;; datasheet hyperlink field, change according to 'forms-format-list'
(setq pw-datasheet-field 19)
;; 21 fields hard-coded, change according to 'forms-format-list'
(setq forms-number-of-fields 21)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other setup stuff.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq forms-read-only t)                 ; to make sure
(setq forms-field-sep "|")
;; Insert after present form
(setq forms-insert-after t)
;; Do allow multi-line fields.
;; (setq forms-multi-line nil)
(setq forms-ro-face '(:foreground "DarkRed"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Runs when a record has been modified.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-modified-record-filter (record)
  ;; Modify date/time
  (aset record pw-date-field (current-time-string))

  ;; try to find  the number field:
  ;; ans copy the equipment code to the code field
  (let ((number-field '(""))
        (code-start nil))
    (setq number-field (aref record pw-number-field))
    ;;(message "number-field %s" number-field)
    (if (eval number-field)
        (progn
          (setq code-start (string-match "[A-Z][A-Z][A-Z]" number-field))
          (if (eval 'code-start)
              (aset record pw-code-field
                    (substring number-field code-start (+ 3 code-start)))
            ))))
  ;; Return the field vector.
  record)

(setq forms-modified-record-filter 'my-modified-record-filter)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initializes a new record.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-new-record-filter (fields)
  ;; To print something like this: '=HYPERLINK("http://www.google.com")'
  ;; in the csv file
  (aset fields pw-photo-field "=HYPERLINK(\"file://\")")
  (aset fields pw-datasheet-field "=HYPERLINK(\"http://\")")
  ;;log the time
  (aset fields pw-date-field (current-time-string))
  ;;return the field vector
  fields)

(setq forms-new-record-filter 'my-new-record-filter)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Before writing this is executed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-write-file-filter ()
  (forms-sort-fields pw-number-field (point-min) (point-max))
  (forms-find-doubles pw-number-field (point-min) (point-max))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; After reading and writing this is
;; executed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-read-file-filter ()
  ;; do nothing try to trick forms mode
  )

(setq forms-write-file-filter 'my-write-file-filter)
(setq forms-read-file-filter 'my-read-file-filter)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun forms-find-doubles (field beg end)
  "Find any doubles in the ARGth field of the lines.
Fields are separated by '|' and numbered from 1 up.
the database must be sorted on 'field' for this function to work.
See defun 'forms-sort-fields' below.
Called from a program, there are three arguments:
FIELD, BEG and END.  BEG and END specify region to sort.
The variable `sort-fold-case' determines whether alphabetic case affects
the sort order."
  (interactive "p\nr")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (forms-sort-skip-fields field)

      (let* ((my-mark (point))
             (my-record 1)
             (this-field '(""))
             (last-field
              (buffer-substring-no-properties
               my-mark (+ my-mark (skip-chars-forward "^|\n")))))

        (forward-line)
        (while (progn
          (setq my-record (1+ my-record))
          ;;(message "forms-find-doubles: last-field: '%s'" last-field)
          (forms-sort-skip-fields field)
          (setq my-mark (point))
          (setq this-field
                (buffer-substring-no-properties
                 my-mark (+ my-mark (skip-chars-forward "^|\n"))))
          (if (eval 'this-field)
              (progn
                (if (string= this-field last-field)
                    (progn
                      (error "forms : double field : %s" this-field))
                  (setq last-field this-field)))
            ;;else
            (progn
              (error "forms : no data in field : %s" this-field)))
          
          (forward-line)
          ;; until end of buffer
          (not (eobp))))
        )))
  
      (message "forms: no double fields"))


;;;###autoload
(defun forms-sort-fields (field beg end)
  "Sort lines in region lexicographically by the ARGth field of each line.
Fields are separated by '|' and numbered from 1 up.
With a negative arg, sorts by the ARGth field counted from the right.
Called from a program, there are three arguments:
FIELD, BEG and END.  BEG and END specify region to sort.
The variable `sort-fold-case' determines whether alphabetic case affects
the sort order."
  (interactive "p\nr")
  (let ;; To make `end-of-line' and etc. to ignore fields.
      ((inhibit-field-text-motion t))
    (sort-fields-1 field beg end
		   (function (lambda ()
			       (forms-sort-skip-fields field)
			       nil))
		   (function (lambda () (skip-chars-forward "^|\n"))))))


;; Position at the beginning of field N on the current line,
;; assuming point is initially at the beginning of the line.
(defun forms-sort-skip-fields (n)
  (if (> n 0)
      ;; Skip across N - 1 fields.
      (let ((i (1- n)))
        (if (= ?| (following-char))(setq i (1- i)))
	(while (> i 0)
          (if (= ?| (char-after (+ 1 (point))))
              (forward-char)
            (skip-chars-forward "|"))
	  (skip-chars-forward "^|\n")
	  (setq i (1- i)))
	(skip-chars-forward "|")
	(if (eolp)
	    (error "Line has too few fields (1): %s"
		   (buffer-substring
		    (line-beginning-position)
		    (line-end-position)))))
    
    (end-of-line)
    ;; Skip back across - N - 1 fields.
    (let ((i (1- (- n))))
      (while (> i 0)
	(skip-chars-backward "|")
	(skip-chars-backward "^|\n")
	(setq i (1- i)))
      (skip-chars-backward "|"))
    (if (bolp)
	(error "Line has too few fields (2): %s"
	       (buffer-substring
		(line-beginning-position)
		(line-end-position))))
    ;; Position at the front of the field
    ;; even if moving backwards.
    (skip-chars-backward "^|\n")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is the spec for the database record
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq forms-format-list
      (list
       "Status          : "    1
       "\n"
       "Obj             : "    2
       "\n"
       "Vers            : "    3
       "\n"
       "\n====== item ======\n"
       "Number          : "    4
       "\n\n"
       "--Identification--\n"
       "Machine         : "    5
       "\n"
       "Alarm Message   : "    6
       "\n\n"
       "--Specifications--\n"
       "Designation     : "    7
       "\n"
       "Make            : "    8
       "\n"
       "Type            : "    9
       "\n"
       "Power           : "   10
       "\n"
       "Current         : "   11
       "\n"
       "Voltage         : "   12
       "\n"
       "acdc            : "   13
       "\n"
       "Buhler standard : "   14
       "\n\n"
       "---  Location  ---\n"
       "Panel No        : "   15
       "\n"
       "Bldg            : "   16
       "\n"
       "Floor           : "   17
       "\n\n"
       "------------------\n"
       "Photograph      : "   18
       "\n"
       "datasheet       : "   19
       "\n"
       "Date            : "   20
       "\n"
       "Remarks         : "   21
       "\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
