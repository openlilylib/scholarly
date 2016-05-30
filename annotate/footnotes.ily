; trigger lilypond footnotes, and counting function
; NOTE: this is different than annotation footnotes, which are compiled by LaTeX
; This file (all or parts of it) may be parsed out to other files as needed.

; PART ONE: THE TRIGGER
; triggers lilypond footnote
; ... is syntax a problem? \annotation follows the grob, while normal \footnote's
; in lilypond precede it...

; PART TWO: THE AUTOMATIC COUNTER

#(define footnote-counter
  (lambda ()
    (let ((count 0))              ; equals 0 until procedure used at least once
      (lambda message             ; when called ...
        (unless (equal? message '(retrieve-count))  ;; if not (num-footnotes 'retrieve-count'),
          (set! count (+ count 1)))                 ;; then add 1 to previous count
        count))))                 ; ... express current count

#(define update-footnotes (footnote-counter)) ; or replace 'update-footnotes' with
;  appropriate funciton name from lilypond.
;         OR... duplicate the command (the duplicate inlcuding
;         the lilypond footnote item in place of 'update-footnotes')
;
; '(udpate-footnotes)' each time footnote is used (only lilypond footnotes in this case)
; at the end of the compile...
;     return (update-footnotes 'retrieve-count) to be written into export / LaTeX export
;
;
; CONDITIONAL...
; - Does reset-footnotes-on-new-page = #t? If so, finally return 0 or nothing... to export / LaTeX export
;
; - If not (= #f), finally send the value from the procedure... to export / LaTeX export
;
; For now.. exporting to log as well, for testing.
