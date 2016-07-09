%
% footnotes functionality for scholarLY; to be continued

% temp-props holds temporary footnote information
#(define temp-props '())

#(define (send-temp-props pair)
    (set! temp-props (assoc-set! temp-props (cadr pair) (caddr pair))))

% test props list from annotation procedures
#(define (set-footnote-proplist proplist)
    (begin
      (set! temp-props (assoc-set! temp-props 'footnote ""))
      (set! temp-props (assoc-set! temp-props 'offset '()))
      (map send-temp-props (ly:get-context-mods proplist))
      (if (string-null? (assq-ref temp-props 'footnote))
          (set! temp-props (assoc-set! temp-props 'footnote (assq-ref temp-props 'message))))
      (if (null? (assq-ref temp-props 'offset))
          (set! temp-props (assoc-set! temp-props 'footnote-case #f))
          (set! temp-props (assoc-set! temp-props 'footnote-case #t)))))

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
