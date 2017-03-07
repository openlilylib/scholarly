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
% Copyright Urs Liska, 2015-17                                                %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Export annotations to html file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define (nest-indent inpt num)
  (let* ((indentation ""))
    (do ((i 0 (1+ i)))
        ((= i num))
        (set! indentation (string-append indentation "  ")))
    (string-append indentation inpt)))

% Probably a more sophisticated approach would be to use a list of div-props
% which contain the class (still automatically set here) as well as id's which
% may be set by the user or also automatically by scholarly. Those will presumably
% be necessary for linking the annotations to grobs later, which is an important
% feature to keep in mind.

#(define (div-class-open name nest-level)
  (let* ((class-name (string-append "<div class=\""
                      (string-append name "\">")))
         (div-begin (nest-indent class-name nest-level)))
    (append-to-output-stringlist div-begin)))

#(define (div-class-close nest-level)
   (append-to-output-stringlist (nest-indent "</div>" nest-level)))


#(define (html-process-props ann)
  (let ((props (getOption `(scholarly annotate export html props))))
    (if (> (length props) 0)
        (do ((i 0 (1+ i)))
            ((= i (length props)))
            (let* ((prop (list-ref props i))
                   (val (if (equal? prop 'grob-location)
                            (format-location ann)
                            (assq-ref ann prop))))
              (begin
              (if (symbol? val)
                  (set! val (symbol->string val)))
              (div-class-open (symbol->string prop) 3)
              (append-to-output-stringlist
                (nest-indent val 4))
              (div-class-close 3)))))))


\register-export-routine html
#(lambda ()

  (let ((println append-to-output-stringlist))

  (println "<head>")
  (println (string-append
    "  <link rel=\"stylesheet\" type=\"text/css\" href=\""
    (string-append (getOption `(scholarly annotate export html css))
                   "\">")))
  (println "</head>")
  (println " ")

  (println "<body>")
  (println " ")

  ;; wrap everything in the annotations div. this is sort of redundant, but
  ;; could be useful if projects have multiple bookparts with annotation lists.
  (println "<annotations>")
  (println " ")
  (for-each
    (lambda (ann)
    ;; wrap each annotation in the common annotation class
    (div-class-open "annotation" 1)
      ;; type as a class - maybe we want different types to have some different styles
      (div-class-open
        (getChildOption '(scholarly annotate export html labels) (assq-ref ann 'type))
          2)
        ;; add the rest of the props to output
        (html-process-props ann) ;; nest-indents x 3
      (div-class-close 2)
    (div-class-close 1)
    (println " "))
    annotations)
    ;; close ann list div
    (println "</annotations>")

    (println " ")
    (println "</body>")

    ;; write to output file
    (write-output-file "html")))
