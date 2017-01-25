#lang racket

(require eve)

(require (for-syntax racket/syntax)
	 (for-syntax syntax/parse))

(provide (all-defined-out))

;; Define maximum admissable distance (in km) for moons/towers

(define max_distance (make-parameter 10000))

(define (sql-get-scanned-regions table)
  (map vector->values (query-rows sqlc (string-append "SELECT DISTINCT regionName FROM " table " ORDER BY regionName"))))

(define (get-filter req filter)
  (match
    (bindings-assq
     filter
     (request-bindings/raw req))
    [(? binding:form? b)
     (list
      (bytes->string/utf-8
       (binding:form-value b)))]
    [_ null]))

(define (extract-post-data req query)
  (match
    (bindings-assq
     query
     (request-bindings/raw req))
    [(? binding:form? b)
     (bytes->string/utf-8
      (binding:form-value b))]
    [_ null]))

;; Define filter-* from URL parameters via (get-filter), check their validity

(define-syntax (sql-bind-user-input stx)
  (syntax-case stx ()
    [(_ type #:request req)
     (with-syntax ([make-id (format-id #'type "filter-~a" (syntax->datum #'type))]
		   [make-test (format-id #'type "~a?" (syntax->datum #'type))]
		   [make-bind (string->bytes/utf-8 (format "~a" (syntax->datum #'type)))])
       #'(define make-id
	   (filter-map make-test (split-user-input (get-filter req make-bind)))))]))

;; Print a list of applied filters

(define-syntax print-filters
  (syntax-rules (:list)
    ((_ :list descriptions filters)
     (format "Results filtered for: ~a"
	     (string-join
	      (map (lambda (desc filter) (format "~a (~a)" desc (string-join filter "|")))
		   descriptions
		   filters)
	      ", ")))
    ((_ a b c)
     (print-filters :list (list "Region" "Constellation" "System")
		    (list a
			  b
			  c)))
    ((_ a b c d)
     (print-filters :list (list "Region" "Constellation" "System" "Alliance")
		    (list a
			  b
			  c
			  (map (lambda (x) (if (string? x) (parse-alliance :ticker (string-upcase x)) x)) d))))
    ((_ a b c d e)
     (print-filters :list (list "Region" "Constellation" "System" "Alliance" "Corporation")
		    (list a
			  b
			  c
			  (map (lambda (x) (if (string? x) (parse-alliance :ticker (string-upcase x)) x)) d)
			  (map (lambda (x) (if (string? x) (parse-corporation :ticker (string-upcase x)) x)) e))))))

;; Set HTTP Headers for authed pages to disallow caching. This passes
;; arguments transparently to response/output, internally adding the required
;; headers

(define-syntax (response/auth/output stx)
  (syntax-parse stx
		[(_ (~optional (~seq #:code code:expr) #:defaults ([code #'200]))
		    (~optional (~seq #:message message:expr) #:defaults ([message #'#"Okay"]))
		    (~optional (~seq #:seconds seconds:expr) #:defaults ([seconds #'(current-seconds)]))
		    (~optional (~seq #:mime-type mime-type:expr) #:defaults ([mime-type #'TEXT/HTML-MIME-TYPE]))
		    (~optional (~seq #:headers headers:expr) #:defaults ([headers #'null]))
		    body)
		 #'(response/output
		    #:code code
		    #:message message
		    #:seconds seconds
		    #:mime-type mime-type
		    #:headers (append headers
				      (list
				       (make-header #"Cache-Control" #"no-cache, no-store, must-revalidate")
				       (make-header #"Pragma" #"no-cache")
				       (make-header #"Expires" #"0")))
		    body)]))

;; Try whether request has a header valid JWT token as a cookie

(define (try-auth-cookie req #:type [type "registration_token"])
  (let* ([auth-cookie (findf (lambda (c) (string=? type (client-cookie-name c))) (request-cookies req))]
	 [auth-token (if (client-cookie? auth-cookie) (client-cookie-value auth-cookie) #f)]
	 [auth-struct (if (string? auth-token) (auth:extract-data (auth:verify-token auth-token)) #f)])
    auth-struct))
