;;;; lyrips.lisp

(in-package #:lyrips) 
;; (ql:quickload '(:dexador :plump :lquery :lparallel :cl-ppcre))

(defun space->- (x)
  (values (cl-ppcre:regex-replace-all "\\s" x "-")))

(defun create-metrolyrics-url (artist song)
  (let ((artist-metro (space->- artist))
        (song-metro (space->- song)))
    (format nil "http://www.metrolyrics.com/~a-lyrics-~a.html" song-metro artist-metro)))

(defun get-metrolyric (artist song)
  (let* ((request (dex:get (create-metrolyrics-url artist song)))
         (parsed-content (plump:parse request))
         (lyric-vec (lquery:$  parsed-content "p.verse" (text))))
    (reduce (lambda (x acc)  (concatenate 'string x acc))  lyric-vec)))

(defun create-az-lyrics-url (artist song)
  (let* ((->trim-all (lambda (x) (cl-ppcre:regex-replace-all " +" x "")))
         (artist-az (funcall ->trim-all artist))
         (song-az (funcall ->trim-all song)))
    (format nil "https://www.azlyrics.com/lyrics/~a/~a.html" artist-az song-az)))

(defun get-azlyric (artist song)
  (let* ((request (dex:get (create-az-lyrics-url artist song)))
         (parsed-content (plump:parse request))
         (lyric-vec (lquery:$  parsed-content "div.text-center" (text))))
    (values
     (cl-ppcre:regex-replace-all
      ".*ba.*"
      (cl-ppcre:regex-replace
       "(?s)if  \\(.*"
       (cl-ppcre:regex-replace
        "(?s).+Usage of azlyrics\.com .+ Sorry about that\."
        (elt lyric-vec 3)"")"")""))))

;; (with-open-file (str file-path
;;                      :direction :output
;;                      :if-exists :supersede
;;                      :if-does-not-exist :create)
;;   (format str large-string))

(defun create-genius-url (artist song)
  (let* ((artist-genius (space->- artist))
         (song-genius (space->- song)))
    (format nil "https://genius.com/~a-~a-lyrics" artist-genius song-genius)))

(defun get-genius (artist song)
  (let* ((request (dex:get (create-genius-url artist song)))
         (parsed-content (plump:parse request))
         (lyric-vec (lquery:$  parsed-content "div.lyrics" (text)))
         (lyric (reduce (lambda (x acc)  (concatenate 'string x acc))  lyric-vec))) 
    (values
     (cl-ppcre:regex-replace "(?s).*Verse 1"
                             (cl-ppcre:regex-replace
                              "(?s)/sse.*/sse"
                              lyric
                              "")
                             "[Verse 1"))))
