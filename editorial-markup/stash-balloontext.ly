%{
#(define (make-balloon mus annot)
   (let ((offset (assq-ref annot 'balloon-offset)))
     (if offset
         (let*
          ((text (or (assq-ref annot 'balloon-text)
                     (assq-ref annot 'message)))
           (item (string->symbol (or (assq-ref annot 'item) "NoteHead"))))
          (cond
           ((assq-ref annot 'is-sequential?)
            (let ((elts (ly:music-property mus 'elements)))
            (ly:message "Sequential")
            (set! mus
                  #{
                    \balloonGrobText #item #offset #text
                    #(first elts)
                    #(cdr elts)
                  #})))
           ((memq 'rhythmic-event (ly:music-property mus 'types))
;            (ly:music-set-property! mus 'articulations
 ;             (list (make-music
 ;                    'AnnotateOutputEvent
 ;                    'X-offset (car offset)
 ;                    'Y-offset (cdr offset)
 ;                    'text text))))

            (ly:music-set-property! mus 'articulations
              (list (make-music
                     'AnnotateOutputEvent
                     'X-offset (car offset)
                     'Y-offset (cdr offset)
                     'text text)))

            (set! mus
                  #{ < #mus > #}))
;                  (make-music
;                   'EventChord
;                   'elements
;                    (list mus)))
;            )
;                   (list
;                    (make-music
;                     'AnnotateOutputEvent
;                     'X-offset (car offset)
;                     'Y-offset (cdr offset)
;                     'text text)
;                     mus)))))

           ;            (set! mus
           ;            #{
           ;              \balloonGrobText #item #offset #text
           ;              #mus
           ;            #}))
           (else
            (ly:music-set-property! mus 'articulations
              (list (make-music
                     'AnnotateOutputEvent
                     'X-offset (car offset)
                     'Y-offset (cdr offset)
                     'text text)))))
          (void (displayMusic mus))

          ))))

%   (if (assq-ref annot 'balloon-offset)
%       (display "")
%       ))


(if (assq-ref props 'balloon-offset)
        ;; we want balloon text:
        (let* ((grob (list-ref item 0))
               (description (assoc-get grob all-grob-descriptions)))
          (if (member 'spanner-interface
                (assoc-get 'interfaces (assoc-get 'meta description)))
              ;; the grob is a spanner, so cancel the balloon
              (oll:warn "We can't give engrave balloon text to spanners yet. Balloon ignored for ~a" grob)
              (begin
               (if (not (assq-ref props 'balloon-text))
                   (set! props (assoc-set! props 'balloon-text
                                 (assq-ref props 'message))))
               (let ((offset (assq-ref props 'balloon-offset))
                     (text (assq-ref props 'balloon-text)))
                 #{ \balloonGrobText #grob #offset \markup { #text } #}))))))
%}
