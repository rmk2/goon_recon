#lang racket

(require grommet/crypto/hash/md5)
(require grommet/crypto/hash/sha1)
(require grommet/crypto/hash/sha256)
(require (only-in openssl/sha1 bytes->hex-string))

(require file/gzip
	 file/gunzip
	 racket/serialize)

(provide (all-defined-out))

;; Get a hashed hex-string from dscan

(define-syntax dscan-data->id
  (syntax-rules (:md5 :sha1 :sha256)
    ((_ :md5 data) (bytes->hex-string (md5-bytes data)))
    ((_ :sha1 data) (bytes->hex-string (sha1-bytes (string->bytes/utf-8 data))))
    ((_ :sha256 data) (bytes->hex-string (sha256-bytes (string->bytes/utf-8 data))))
    ((_ :sha256-truncate data) (bytes->hex-string (subbytes (sha256-bytes (string->bytes/utf-8 data)) 0 16)))
    ((_ data) (dscan-data->id :sha256-truncate data))))

;; Create a filename from dscan
;; filename: hex-string + ".gz"

(define (dscan-data->filename data)
  (string-append (dscan-data->id data) ".gz"))

;; Create a filename from a hex-string

(define (dscan-id->filename id)
  (string-append id ".gz"))

;; Write dscan to file

(define (dscan-gzip-write data [prefix "."])
  (call-with-output-file (build-path prefix (dscan-data->filename data))
    (lambda (out)
      (gzip-through-ports (open-input-string data)
			  out
			  #f
			  (current-seconds)))
    #:exists 'can-update))

;; Read dscan from file 

(define (dscan-gunzip-read id [prefix "."])
  (call-with-output-bytes
   (lambda (out)
     (gunzip-through-ports (open-input-file (build-path prefix (dscan-id->filename id)))
			   out))))

;; Wrapper to read/write local scans as parsed structs to disk

(define-syntax (dscan-local->string stx)
  (syntax-case stx (:id :write :read :read-id)
    [(_ param) #'(with-output-to-string (lambda () (write (serialize param))))]
    [(_ :id param) #'(dscan-data->id (dscan-local->string param))]
    [(_ :write param) #'(dscan-gzip-write (dscan-local->string param) "./")]
    [(_ :write param prefix) #'(dscan-gzip-write (dscan-local->string param) prefix)]
    [(_ :read param)
     #'(deserialize (read (open-input-bytes (dscan-gunzip-read (dscan-data->id (dscan-local->string param)) "./"))))]
    [(_ :read param prefix)
     #'(deserialize (read (open-input-bytes (dscan-gunzip-read (dscan-data->id (dscan-local->string param)) prefix))))]
    [(_ :read-id id) #'(deserialize (read (open-input-bytes (dscan-gunzip-read id))))]
    [(_ :read-id id prefix) #'(deserialize (read (open-input-bytes (dscan-gunzip-read id prefix))))]))
