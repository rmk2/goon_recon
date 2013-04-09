#!/usr/bin/env stklos-script

;; #! /usr/bin/guile  \
;; -q -e main -s
;; !#

;; (use-modules (ice-9 format))
;; (use-modules (ice-9 ftw))
;; (use-modules (ice-9 rdelim))

(Define-syntax tag
  (syntax-rules (/ class: id:)
    ((_ type) (format port "<~a> ~&" type))
    ((_ type /) (format port "</~a> ~&" type))
    ((_ type class: class) (format port "<~a class=\"~a\"> ~&" type class))
    ((_ type id: id) (format port "<~a id=\"~a\"> ~&" type id))
    ((_ type field)
     (format port "<~a>~a</~a> ~&" type field type))
    ((_ type field class: class)
     (format port "<~a class=\"~a\">~a</~a> ~&" type class field type))
    ((_ type field id: id)
     (format port "<~a id=\"~a\">~a</~a> ~&" type id field type))
    ((_ type field class: class id: id)
     (format port "<~a class=\"~a\" id=\"~a\">~a</~a> ~&" type class id field type))))

(define-syntax link
  (syntax-rules (name: href: class: id:)
    ((_ href) (format #t "<a href=\"~a\"></a>" href))
    ((_ href: href name: name) (format #t "<a href=\"~a\">~a</a>" href name))
    ((_ href: href name: name class: class) (format #t "<a class=\"~a\" href=\"~a\">~a</a>" class href name))
    ((_ href: href name: name id: id) (format #t "<a id=\"~a\" href=\"~a\">~a</a>" id href name))
    ((_ href: href name: name class: class id: id) (format #t "<a class=\"~a\" id=\"~a\" href=\"~a\">~a</a>" class id href name))))

(define-syntax format->string
  (syntax-rules (\n)
    ((_ command) (with-output-to-string (lambda () command)))
    ((_ command \n) (format #t "~a ~&" (with-output-to-string (lambda () command))))))

(define (index port)
  (format port "<!DOCTYPE html> ~&")
  (format port "<meta http-equiv=\"Content-type\" content=\"text/html;charset=UTF-8\" /> ~&")
  (format port "<link rel=\"stylesheet\" type=\"text/css\" href=\"css/main.css\" /> ~&")
  (tag 'html)
  (tag 'head)
  (tag 'title "End Cycle")
  (tag 'head /)
  (tag 'body)
  (tag 'div id: 'main)
  (tag 'div id: 'header)
  (tag 'h1 "End Cycle" id: 'title)
  (tag 'div "A blog" id: 'description)
  (tag 'div /)
  (tag 'div class: 'content)
  (let loop ((postdir "./schemeblog/posts") (i 0))
    (if (not (= i (length (directory-files postdir))))
	(begin
	  (tag 'div class: 'post)
	  (tag 'div (date) class: 'timestamp)
	  (format port "~a" (port->string (open-input-file (string-append postdir "/" (list-ref (reverse (directory-files postdir)) i)))))
	  (tag 'div /)
	  (loop postdir (+ i 1)))))
  (tag 'div /)    
  (tag 'div "CC BY-NC-SA -- rmk2" id: 'footer)
  (tag 'div /)
  (tag 'body /)
  (tag 'html /))

(define (head port)
  (format port "<!DOCTYPE html> ~&")
  (format port "<meta http-equiv=\"Content-type\" content=\"text/html;charset=UTF-8\" /> ~&")
  (format port "<link rel=\"stylesheet\" type=\"text/css\" href=\"css/main.css\" /> ~&")
  (tag 'html)
  (tag 'head)
  (tag 'title "End Cycle")
  (tag 'head /)
  (tag 'body)
  (tag 'div id: 'main)
  (tag 'div id: 'header)
  (tag 'h1 "End Cycle" id: 'title)
  (tag 'div "A blog" id: 'description)
  (tag 'div /)
  (tag 'div class: 'content))

(define (foot port)
  (tag 'div /)    
  (tag 'div "CC BY-NC-SA -- rmk2" id: 'footer)
  (tag 'div /)
  (tag 'body /)
  (tag 'html /))

(define (make type)
  (let loop ((postdir "./schemeblog/posts") (content ()) (i 0))
    (if (not (= i (length (directory-files postdir))))
	(loop postdir (cons (port->string (open-input-file (string-append postdir "/" (list-ref (directory-files postdir) i)))) content) (+ i 1))
	(begin
	  (if (= type 0)
	      (begin
		(head)
		(let jloop ((j 0) (limit 2))
		  (if (not (= j limit))
		      (begin
			(tag 'div class: 'post)
			(tag 'div (date) class: 'timestamp)
			(display (list-ref content j))
			(tag 'div /)
			(jloop (+ j 1) limit))))
		(foot))
	      (begin
		(let jloop ((j 0))
		  (if (not (= j (length content)))
		      (begin
			(head)
			(tag 'div class: 'post)
			(tag 'div (date) class: 'timestamp)
			(display (list-ref content j))
			(tag 'div /)
			(foot)
			(jloop (+ j 1)))))))))))

(define (index-list)
  (let ((content ()))
    (let loop ((postdir "./schemeblog/posts") (i 0))
      (if (not (= i (length (directory-files postdir))))
	  (begin
	    (set! content (cons (port->sexp-list (open-input-file (string-append postdir "/" (list-ref (reverse (directory-files postdir)) i)))) content))
	    (loop postdir (+ i 1)))))
    (format #t "~a ~&" content)))

(define (index-list-loop)
  (let loop ((postdir "./schemeblog/posts") (content ()) (i 0))
    (if (not (= i (length (directory-files postdir))))
	(loop postdir (cons (port->string (open-input-file (string-append postdir "/" (list-ref (directory-files postdir) i)))) content) (+ i 1))
	(let ((j 0))
	  (for-each (lambda (n) (format #t "~a" (list-ref content j)) (set! j (+ j 1))) content)))))

(define (index-foreach)
  (let* ((postdir "/home/ryko/Development/scheme/stklos/schemeblog/posts") (ls (reverse (directory-files postdir))) (i 0))
    (for-each
     (lambda (n)
       (tag 'div class: 'post)
       (tag 'div (date) class: 'timestamp)
       (format #t "~a" (port->string (open-input-file (string-append postdir "/" (list-ref ls i))))) (set! i (+ i 1))
       (tag 'div /))
     ls)))

(define (archive-test port)
  (let loop ((postdir "./schemeblog/posts") (i 0))
    (if (not (= i (length (directory-files postdir))))
	(begin
	  #;	  (format #t "~a ~&" (list-ref (reverse (directory-files postdir)) i))
	  (tag 'p (format->string (link :href (string-append postdir "/" (list-ref (reverse (directory-files postdir)) i)) name: (list-ref (reverse (directory-files postdir)) i))))
	  (loop postdir (+ i 1))))))

(define (main arg)
  (display
   (call-with-output-string
    (lambda (port)
      (index port)))))

(define (index-test)
  (let* ((postdir "./schemeblog/posts") (ls (length (directory-files postdir))))
    (let loop ((i 0))
      (if (not (= i ls))
	  (begin
	    (call-with-output-file (string-append postdir "/proc_" (number->string i) ".html")
	      (lambda(port)
		(let ((postdir postdir) (i i))
		  (format port "<!DOCTYPE html> ~&")
		  (format port "<meta http-equiv=\"Content-type\" content=\"text/html;charset=UTF-8\" /> ~&")
		  (format port "<link rel=\"stylesheet\" type=\"text/css\" href=\"../css/main.css\" /> ~&")
		  (format port "~a" (port->string (open-input-file (string-append postdir "/" (list-ref (reverse (directory-files postdir)) i))))))))
	    (loop (+ i 1)))))))
