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
#(define-music-function (mark item)
   ((markup?) symbol-list-or-music?)
     (let* ((xoff (car (assq-ref temp-props 'offset)))
            (yoff (cdr (assq-ref temp-props 'offset)))
            (ftex (assq-ref temp-props 'footnote))
            (mus (make-music
                       'FootnoteEvent
                       'X-offset xoff
                       'Y-offset yoff
                       'automatically-numbered (not mark)
                       'text (or mark (make-null-markup))
                       'footnote-text ftex)))
           (once (propertyTweak 'footnote-music mus item))))
