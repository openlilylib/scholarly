%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
% This file is part of ScholarLY,                                             %
%                      =========                                              %
% a toolkit library for scholarly work with GNU LilyPond and LaTeX,           %
% belonging to openLilyLib (https://github.com/openlilylib/openlilylib        %
%              -----------                                                    %
%                                                                             %
% ScholarLY is free software: you can redistribute it and/or modify           %
% it under the terms of the GNU General Public License as published by        %
% the Free Software Foundation, either version 3 of the License, or           %
% (at your option) any later version.                                         %
%                                                                             %
% ScholarLY is distributed in the hope that it will be useful,                %
% but WITHOUT ANY WARRANTY; without even the implied warranty of              %
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               %
% GNU Lesser General Public License for more details.                         %
%                                                                             %
% You should have received a copy of the GNU General Public License           %
% along with ScholarLY.  If not, see <http://www.gnu.org/licenses/>.          %
%                                                                             %
% ScholarLY is maintained by Urs Liska, ul@openlilylib.org                    %
% Copyright Urs Liska, 2015                                                   %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%{
  Output annotations as LaTeX code
%}

% Lookup list for lilyglyphs representations of rhythmic values
#(define lilyglyphs-rhythmic-values
   '((1 . "\\wholeNote")
     (1/2 . "\\halfNote")
     (1/4 . "\\crotchet")
     (1/8 . "\\quaver")
     (1/16 . "\\semiquaver")
     (1/32 . "\\semidemiquaver")))

% Lookup a lilyglyphs representation for a rhythmic fraction
% or return a 'NA' string and issue a warning
% This is probably mostly an issue with the display of beat fractions
#(define (lilyglyphs-lookup frac)
   (or (assoc-ref lilyglyphs-rhythmic-values frac)
       ; Currently we don't provide any intelligent handling of
       ; values that are not in the above list.
       (begin
        (ly:warning (format "Did not find a lilyglyphs representation for ~a" frac))
        "NA")))

% If requested format the measure position using lilyglyphs commands
#(define (lilyglyphs-beat-string loc-props)
   (let*
    ((meter (assq-ref loc-props 'meter))
     (beat-length (/ 1 (cdr meter)))
     (beat-lily (lilyglyphs-lookup beat-length))
     (our-beat (assq-ref loc-props 'our-beat))
     (beat-string (format "~a.\\,~a" our-beat beat-lily))
     (beat-fraction (assq-ref loc-props 'beat-fraction))
     (beat-part (assq-ref loc-props 'beat-part)))

    (if (= 0 beat-fraction)
        beat-string
        (format "~a,\\,~a" beat-string
          (let*
           ((part-numerator (+ 1 (ly:moment-main-numerator beat-part)))
            (sub-beat-length (/ 1 (ly:moment-main-denominator beat-part))))
           (format "~a. ~a" part-numerator
             (lilyglyphs-lookup sub-beat-length)))))))


% Format a single property assignment into a LaTeX-optional-argument.
#(define (format-property prop)
   (let
    ((k (car prop))
     (val (cdr prop)))
    (cons k
      ;; Format the right hand side of the assignment
      ;; handling special known keys and all others
      ;; depending on the *type* of the value.
      (case k
        ;; handle known keys
        ((input-file-name) (car val))
        ((rhythmic-location)
         (format "~a,~a" (car val) (ly:moment-main (cdr val))))
        ((beat-string-lilyglyphs)
         (lilyglyphs-beat-string val))
        (else
         (cond
          ((ly:moment? val) (ly:moment-main val))
          ((ly:input-location? val)
           (let*
            ((vals (ly:input-file-line-char-column val))
             (nums (map number->string (cdr vals))))
            (string-join (append (list (first vals)) nums) ":")))
          ((ly:music? val)
           ;; use \displayLilyMusic for music arguments,
           ;; stripping the (redundant) curly braces.
           (let ((mus-string
                  (with-output-to-string
                   (lambda () (display-lily-music val)))))
             (substring mus-string 1 (- (string-length mus-string) 2))))
          ;; everything else will be printed as-is
          (else val)))))))

% Format a single annotation for export as a LaTeX command
#(define (format-annotation ann)
   (let*
    ((regular-props
      ;; all properties except 'grob-location (which is a list of sub-props)
      (filter
       (lambda (p)
         (not (member (car p) '(grob-location grob))))
       ann))
     ;; The sub-list of 'grob-location
     (location-props
      (let ((lp (assq-ref ann 'grob-location)))
        (if (getOption '(scholarly annotate export latex use-lilyglyphs))
            lp
            (append lp (list (cons 'beat-string-lilyglyphs lp))))))
     ;; Generate a string list with key=value assignments
     (assignments
      (map
       (lambda (p)
         (let ((prop (format-property p)))
           (format "    ~a={~a}" (car prop)(cdr prop))))
       (append regular-props location-props)))
     )

    ;; compose the resulting string list
    (append-to-output-stringlist
     (string-append
      (or (assq-ref annotation-type-latex-commands (assq-ref ann 'type))
          "\\annotation")
      "[\n"
      (string-join assignments ",\n")
      "]\n"))))

% Generate and write annotations to a LaTeX input file
\register-export-routine latex
#(lambda ()
   ;; process annotations, adding lines to 'annotate-export-stringlist'
   (for-each
    (lambda (ann)
      (format-annotation ann))
    annotations)

   ;; write to output file
   (write-output-file "inp"))

