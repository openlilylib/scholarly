%
% footnotes functionality for scholarLY; to be continued

% footnote stub; footnote-props holds temporary footnote information
#(define footnote-props '((fnoffset 0 . 0)(fntext "")))

% in ctiricalRemark, etc. sets optional arguments pair and string to footnote-props
#(define (set-footnote-props offs texts)
   (set! footnote-props (assoc-set! footnote-props 'fnoffset offs))
   (set! footnote-props (assoc-set! footnote-props 'fntext texts)))

% an only slightly altered footnote procedure
lyfootnote =
#(define-music-function (mark item)
   ((markup?) symbol-list-or-music?)
     (let ((xoff (car (assq-ref footnote-props 'fnoffset)))
            (yoff (cdr (assq-ref footnote-props 'fnoffset)))
            (tex (assq-ref footnote-props 'fntext)))
           (let ((mus (make-music
                       'FootnoteEvent
                       'X-offset xoff
                       'Y-offset yoff
                       'automatically-numbered (not mark)
                       'text (or mark (make-null-markup))
                       'footnote-text tex)))
                 (once (propertyTweak 'footnote-music mus item)))))
