% editorial commands for scholarLY
%

#(define scholarly-edition-bools (make-hash-table))
#(define scholarly.editions (make-hash-table))

% slurDashed, and other slur commands work normally since they don't depend on music arguments out of context
#(hash-set! scholarly.editions (list 'addition 'Slur) slurDashed)
#(hash-set! scholarly.editions (list 'addition 'NoteHead) parenthesize)
#(hash-set! scholarly.editions (list 'deletion 'Slur) slurDotted)


#(define scholarly-edit-cmd (make-hash-table))
% define arbitrary initial value so scholarlytempfunction will pass until used
#(hash-set! scholarly-edit-cmd 'temp-applic 'addition)
#(hash-set! scholarly-edit-cmd 'temp-grp 'NoteHead)

% temporary function used to apply editorial commands to music
#(define editorialCommand
   (let ((cmdtype (hash-ref scholarly-edit-cmd 'temp-applic))
         (musitem (hash-ref scholarly-edit-cmd 'temp-grp)))
        (hash-ref scholarly.editions (list cmdtype musitem))))
