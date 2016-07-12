% preamble

% Conditionally generate a footnote for annotation
#(define ann-footnote
   (define-music-function (mark item props)
     ((markup?) symbol-list-or-music? list?)
     (let* ((xoff (car (assq-ref props 'footnote-offset)))
            (yoff (cdr (assq-ref props 'footnote-offset)))
            (ftex (assq-ref props 'footnote-text))
            (mus (make-music
                  'FootnoteEvent
                  'X-offset xoff
                  'Y-offset yoff
                  'automatically-numbered (not mark)
                  'text (or mark (make-null-markup))
                  'footnote-text ftex)))
       (once (propertyTweak 'footnote-music mus item)))))
