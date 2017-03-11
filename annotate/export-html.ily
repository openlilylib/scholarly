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

% convenience functions
#(define (stringify-html-tag tag)
  (format "\"~a\"" tag))
#(define (delimit-html-tags tags)
  (format "<div ~a>" tags))


% the following two should be refactored into a single function that
% solely accepts the class name as a string, then automatically checks
% for the html-id tag prop (adding the ann argument to do that)
% and adds it if necessary.
#(define (div-class-open div-class nest-level)
  (let* ((class (string-append "class=" (stringify-html-tag div-class)))
         (div-tag (delimit-html-tags class))
         (div-begin (nest-indent div-tag nest-level)))
    (append-to-output-stringlist div-begin)))

#(define (div-class-id-open div-class div-id nest-level)
    (let* ((class (string-append "class=" (stringify-html-tag div-class)))
           (id (string-append " id=" (stringify-html-tag div-id)))
           (div-tags (delimit-html-tags (string-append class id)))
           (div-begin (nest-indent div-tags nest-level)))
      (append-to-output-stringlist div-begin)))


#(define (div-close nest-level)
   (append-to-output-stringlist (nest-indent "</div>" nest-level)))

% TODO simplify the following to a lambda procedure?
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
              (div-close 3)))))))


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
  (div-class-open "annotations" 0)
  (println " ")
  (for-each
    (lambda (ann)
    ;; wrap each annotation in the common annotation class
    ;; add div ID tag if available



    ;(if (assq-ref ann 'html-id)
    ;    (let ((div-id (assoc-ref ann 'html-id)))
    ;      (div-class-open (string-append "annotation\""
    ;                              (string-append " id=\"" div-id)) 1))
    ;    ;; no div ID, so just annotation class
    ;    (div-class-open "annotation" 1))


    (if (assq-ref ann 'html-id)
        (let ((div-id (assoc-ref ann 'html-id)))
          (div-class-id-open "annotation" div-id 1))
        ;; no div ID, so just annotation class
        (div-class-open "annotation" 1))


      ;; type as a class - maybe we want different types to have some different styles
      (div-class-open
        (getChildOption '(scholarly annotate export html labels) (assq-ref ann 'type))
          2)
        ;; add the rest of the props to output
        (html-process-props ann) ;; nest-indents x 3
      (div-close 2)
    (div-close 1)
    (println " "))
    annotations)
    ;; close ann list div
    (div-close 0)

    (println " ")
    (println "</body>")

    ;; write to output file
    (write-output-file 'html)))
