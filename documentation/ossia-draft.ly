\version "2.19.80"

ossia =
#(define-music-function
  (ossia-music music) (ly:music? ly:music?)
  (let*
   ((name #f))
   (make-sequential-music
    (list
     (make-apply-context
      (lambda (context)
        (set! name (ly:context-id (ly:context-find context 'Staff)))))
     #{
       << \new Staff \with {
         #(ly:make-context-mod
           `((apply
              ,(lambda (c)
                 (set! (ly:context-property c 'alignAboveContext) name)))))
         #@(map
         (lambda (grob)
         (omit (list grob)))
         '())
          } { #ossia-music }
          #music
       >>
     #}))))
ossia =
#(define-music-function
  (ossia-music music) (ly:music? ly:music?)
  (let*
   ((name #f))

   (make-sequential-music
    (list
     (make-apply-context
      (lambda (context)
        (set! name (ly:context-id (ly:context-find context 'Staff)))))
     #{
       << \new Staff \with {
         #(if #t
              (ly:make-context-mod
               `((apply
                  ,(lambda (c)
                     (set! (ly:context-property c 'alignAboveContext) name))))))
         #@(map
           (lambda (grob)
             (omit (list grob)))
           '())
          } { #ossia-music }
          #music
       >>
     #}))))

\new Staff = "My Staff"
\relative {
  g'8 a b c
  \ossia { d c b a } { c b a g }
  g b d b g2
}
