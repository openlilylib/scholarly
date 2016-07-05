%
% footnotes functionality for scholarLY; to be continued

% footnote-props holds temporary footnote information
#(define footnote-props '())

% TODO amend to make 'message' the 'fntext' arg if fntext not present
#(define (send-footnote-props pair)
    (set! footnote-props
      (assoc-set! footnote-props
        (cadr pair) (caddr pair))))

#(define (set-footnote-proplist proplist)
    (begin
      (set! footnote-props (assoc-set! footnote-props 'footnote ""))
      (map send-footnote-props (ly:get-context-mods proplist))
      (if (string-null? (assq-ref footnote-props 'footnote))
          (set! footnote-props
            (assoc-set! footnote-props 'footnote
              (assq-ref footnote-props 'message))))))

% footnote hook (TODO: make automated)
lyfootnote =
#(define-music-function (mark item)
   ((markup?) symbol-list-or-music?)
     (let ((xoff (car (assq-ref footnote-props 'offset)))
            (yoff (cdr (assq-ref footnote-props 'offset)))
            (ftex (assq-ref footnote-props 'footnote)))
           (let ((mus (make-music
                       'FootnoteEvent
                       'X-offset xoff
                       'Y-offset yoff
                       'automatically-numbered (not mark)
                       'text (or mark (make-null-markup))
                       'footnote-text ftex)))
                 (once (propertyTweak 'footnote-music mus item)))))
