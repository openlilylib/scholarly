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


% convenience functions
#(define (stringify-html-tag tag)
  (format "\"~a\"" tag))
#(define (delimit-html-tags tags)
  (format "<div ~a>" tags))

% open div with unique tags
#(define (div-open ann-or-string nest-level)
  ;; if class = string, don't check for an id. otherwise it
  ;; is an ann props list, so check for an id and apply if necessary
  (if (string? ann-or-string)
      (let* ((class (string-append "class=" (stringify-html-tag ann-or-string)))
             (div-tag (delimit-html-tags class))
             (div-begin (nest-indent div-tag nest-level)))
          (append-to-output-stringlist div-begin))
      (let* ((ann ann-or-string)
             (class (string-append "class=" (stringify-html-tag "annotation")))
             (id (if (assq-ref ann 'html-id)
                     (string-append " id=" (stringify-html-tag (assoc-ref ann 'html-id)))
                     ""))
             (div-tags (delimit-html-tags (string-append class id)))
             (div-begin (nest-indent div-tags nest-level)))
          (append-to-output-stringlist div-begin))))

% close any div
#(define (div-close nest-level)
   (append-to-output-stringlist (nest-indent "</div>" nest-level)))

% get all the props we want exported from the option
#(define (html-process-props ann)
  (let ((props (getOption `(scholarly annotate export html props))))
        (for-each
          (lambda (prop)
            (let* ((val (cond ((equal? prop 'grob-location)
                                 (format-location ann))
                              ((equal? prop 'type)
                                 (getChildOption
                                   `(scholarly annotate export html labels)
                                   (assq-ref ann 'type)))
                              (else (assq-ref ann prop)))))
              (begin
              (if (symbol? val)
                  (set! val (symbol->string val)))
              (div-open (symbol->string prop) 3)
              (append-to-output-stringlist
                (nest-indent val 4))
              (div-close 3))))
          props)))



\register-export-routine html
#(lambda ()

  (let ((println append-to-output-stringlist)
        (full-doc (getOption `(scholarly annotate export html full-document))))

  ;; If option is True, add the header and body
  (if full-doc
    (begin
      (println "<head>")
        (println (string-append
          "  <link rel=\"stylesheet\" type=\"text/css\" href=\""
          (string-append (getOption `(scholarly annotate export html css))
                         "\">")))
      (println "</head>")
      (println " ")
      (println "<body>")
      (println " ")))

  ;; wrap everything in the annotations div. this is sort of redundant, but
  ;; could be useful if projects have multiple bookparts with annotation lists.
  (div-open "annotations" 0)
  (println " ")

  (for-each
    (lambda (ann)

      ;; wrap each annotation in the common annotation class
      ;; add div ID tag if available
      (div-open ann 1)

        ;; type as a class - maybe we want different types to have some different styles
        (div-open (symbol->string (assq-ref ann 'type)) 2)

          ;; add the rest of the props to output
          (html-process-props ann) ;; nest-indents x 3

        (div-close 2)

      (div-close 1)
      (println " "))

    annotations)

    ;; close "annotations" div
    (div-close 0)

    (if full-doc
      (begin
        (println " ")
        (println "</body>")))

    ;; write to output file
    (write-output-file 'html)))
