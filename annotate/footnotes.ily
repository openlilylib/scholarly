%
% footnotes functionality for scholarLY; to be continued

% Set defaults and handle values for annotation footnotes
#(define (footnote-proplist props)
   (let
    ;; Consider a footnote if 'offset is specified
    ((footnote-case (if (assq-ref props 'offset) #t #f)))
    (set! props
          (assq-set! props 'footnote-case footnote-case))

    ;; if we have a footnote but no footnote text is specified
    ;; assign the message property to the footnote
    (if (and footnote-case
             (not (assq-ref props 'footnote)))
        (set! props
              (assq-set! props 'footnote
                (assq-ref props 'message)))))
   props)


% conditionally automated footnote hook
lyfootnote =
#(define-music-function (mark item props)
; QUESTION:
; I don't see yet whether \lyfootnote is intended to be
; used by users as well.
; If so then then props argument should probably be extended
; to accept either an alist or a ly:context-mod (new predicate)
; and process it accordingly.
   ((markup?) symbol-list-or-music? list?)
     (let* ((xoff (car (assq-ref props 'offset)))
            (yoff (cdr (assq-ref props 'offset)))
            (ftex (assq-ref props 'footnote))
            (mus (make-music
                       'FootnoteEvent
                       'X-offset xoff
                       'Y-offset yoff
                       'automatically-numbered (not mark)
                       'text (or mark (make-null-markup))
                       'footnote-text ftex)))
           (once (propertyTweak 'footnote-music mus item))))
