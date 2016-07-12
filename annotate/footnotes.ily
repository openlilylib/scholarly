% add preamble

% duplicate props list and check for footnote
#(define (with-footnote-props props)
    (let ((footnote-case (if (assq-ref props 'offset) #t #f)))
         (set! props (assq-set! props 'footnote-case footnote-case))
         (if (and footnote-case
             (not (assq-ref props 'footnote)))
             (set! props (assq-set! props 'footnote (assq-ref props 'message)))))
    props)

% Implicitly generate a footnote for an annotation
#(define ann-footnote
   (define-music-function (mark item props)
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
       (once (propertyTweak 'footnote-music mus item)))))
