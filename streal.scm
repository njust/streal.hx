(require "helix/components.scm")
(require "helix/misc.scm")
(require "helix/editor.scm")
(require (prefix-in helix. "helix/commands.scm"))
(require (prefix-in helix.static. "helix/static.scm"))

(struct Mark (num path line))
(struct StrealState (marks mode branch) #:mutable)

(define MAX-MARKS 9)

(define keymap-help
  '("s"      "Add / remove current position"
    "1..9"   "Jump to mark (or delete in d mode)"
    "Esc, q" "Close popup"
    "C"      "Clear all marks"
    "h"      "Open in horizontal split"
    "v"      "Open in vertical split"
    "d"      "Delete mode"
    "e"      "Edit current Streal file"
    "?"      "Show keymap"))

(define chars-to-encode '(#\% #\space #\\ #\/ #\: #\* #\? #\" #\< #\> #\|))

(define (editor-focus-path)
  (~> (editor-focus) (editor->doc-id) (editor-document->path)))

(define (trim-current-directory path)
  (~> (or path "")
      (trim-start-matches (string-append (current-directory) (path-separator)))
      (trim-start-matches (string-append "." (path-separator)))))

(define (toggle-mode state mode)
  (set-StrealState-mode! state (if (eqv? (StrealState-mode state) mode) 'normal mode)))

(define (percent-encode str)
  (~>> str
       (string->list)
       (map (lambda (x)
              (if (list-contains x chars-to-encode)
                  (~>> x
                       (char->integer)
                       ((flip number->string) 16)
                       (string-upcase)
                       (string-append "%")
                       (string->list))
                  x)))
       (flatten)
       (apply string)))

(define (handle-error err)
  (set-error! (string-append "'streal':" (error-object-message err))))

(define (split-first str delim-char)
  (let loop ([chars (string->list str)] [before '()])
    (cond
      [(null? chars) (list str "")]
      [(eqv? (car chars) delim-char)
       (list (list->string (reverse before))
             (list->string (cdr chars)))]
      [else (loop (cdr chars) (cons (car chars) before))])))

(define (get-git-branch)
  (begin
    (define result
      (~> (command "git" '("branch" "--show-current"))
          with-stdout-piped
          with-stderr-piped
          spawn-process))
    (cond
      [(Ok? result)
       (let ([handle (Ok->value result)])
         (define stdout (read-port-to-string (child-stdout handle)))
         (define stderr (read-port-to-string (child-stderr handle)))
         (if (and (string=? stderr "") (not (string=? stdout "")))
             (trim stdout)
             #false))]
      [(Err? result) (error (Err->value result))])))

(define (get-streal-file-path branch)
  (let* ([slash (path-separator)]
         [branch-path (if branch
                          (string-append "branch" slash (percent-encode branch) slash)
                          "")])
    (string-append (canonicalize-path "~")
                   (path-separator)
                   ".streal"
                   (path-separator)
                   branch-path
                   (percent-encode (current-directory))
                   ".txt")))

(define (read-file-as-string name)
  (call-with-input-file name
                        (lambda (in)
                          (do ((x (read-char in) (read-char in)) (chars '() (cons x chars)))
                              ((eof-object? x) (list->string (reverse chars)))))))

(define (parse-mark-line str num)
  (let* ([trimmed (trim str)]
         [parts (split-first trimmed #\space)]
         [line-str (car parts)]
         [path (cadr parts)]
         [line-num (string->number line-str)])
    (if (and line-num (> (string-length path) 0))
        (Mark num (trim-current-directory path) line-num)
        #false)))

(define (format-mark-line mark)
  (string-append (number->string (Mark-line mark)) " " (Mark-path mark)))

(define (mark-for-num marks n)
  (cond
    [(null? marks) #false]
    [(= (Mark-num (car marks)) n) (car marks)]
    [else (mark-for-num (cdr marks) n)]))

(define (insert-sorted marks new-mark)
  (cond
    [(null? marks) (list new-mark)]
    [(< (Mark-num new-mark) (Mark-num (car marks)))
     (cons new-mark marks)]
    [else (cons (car marks) (insert-sorted (cdr marks) new-mark))]))

(define (set-mark marks num path line)
  (insert-sorted (filter (lambda (m) (not (= (Mark-num m) num))) marks)
                 (Mark num path line)))

(define (remove-mark marks num)
  (filter (lambda (m) (not (= (Mark-num m) num))) marks))

(define (next-available-num marks)
  (let ([used (map Mark-num marks)])
    (let loop ([n 1])
      (cond
        [(> n MAX-MARKS) #false]
        [(member n used) (loop (+ n 1))]
        [else n]))))

(define (find-mark-at marks path line)
  (cond
    [(null? marks) #false]
    [(and (string=? (Mark-path (car marks)) path)
          (= (Mark-line (car marks)) line))
     (car marks)]
    [else (find-mark-at (cdr marks) path line)]))

(define (get-marks branch)
  (let ([path (get-streal-file-path branch)])
    (if (is-file? path)
        (let ([lines (split-many (read-file-as-string path) "\n")])
          (let loop ([ls lines] [i 1] [marks '()])
            (if (or (null? ls) (> i MAX-MARKS))
                (reverse marks)
                (let* ([line (trim (car ls))]
                       [mark (if (> (string-length line) 0)
                                 (parse-mark-line line i)
                                 #false)])
                  (loop (cdr ls) (+ i 1)
                        (if mark (cons mark marks) marks))))))
        '())))

(define (write-marks marks branch)
  (let* ([path (get-streal-file-path branch)]
         [max-num (if (empty? marks) 0 (apply max (map Mark-num marks)))]
         [lines (map (lambda (i)
                       (let ([m (mark-for-num marks (+ i 1))])
                         (if m (format-mark-line m) "")))
                     (range max-num))]
         [contents (if (> (length lines) 0)
                       (string-append (string-join lines "\n") "\n")
                       "")]
         [directory (parent-name path)])
    (when (is-file? path) (delete-file! path))
    (unless (path-exists? directory) (create-directory! directory))
    (unless (empty? marks)
      (call-with-output-file path (lambda (out) (write-string contents out))))
    (delete-empty-directories)))

(define (directory-empty? path) (= (length (read-dir path)) 0))

(define (delete-empty-directories)
  (let* ([slash (path-separator)]
         [streal-path (string-append (canonicalize-path "~") slash ".streal")]
         [branch-path (string-append streal-path slash "branch")])
    (when (path-exists? branch-path)
      (begin
        (for-each delete-directory! (filter directory-empty? (read-dir branch-path)))
        (when (directory-empty? branch-path)
          (delete-directory! branch-path))))))

(define (shorten-paths paths)
  (let ([split-paths (map (lambda (x) (reverse (split-many x (path-separator)))) paths)])
    (map (lambda (split-path)
           (let ([result (mutable-vector)]
                 [i 0]
                 [len (length split-path)]
                 [split-paths-to-check split-paths])
             (while [and (< i len) (> (length split-paths-to-check) 0)]
                    (vector-push! result (list-ref split-path i))
                    (set! split-paths-to-check
                          (filter (lambda (x)
                                    (and (not (eq? x split-path))
                                         (< i (length x))
                                         (string=? (list-ref split-path i) (list-ref x i))))
                                  split-paths-to-check))
                    (set! i (+ i 1)))
             (string-join (reverse (vector->list result)) (path-separator))))
         split-paths)))

(define (mark-display-texts marks shortened-paths)
  (map (lambda (m sp)
         (string-append sp ":" (number->string (Mark-line m))))
       marks shortened-paths))

(define (calculate-popup-area rect marks display-texts mode)
  (let* ([rect-width (area-width rect)]
         [rect-height (area-height rect)]
         [width (min (if (eqv? mode 'help)
                         (+ (apply max (map string-length keymap-help)) 11)
                         (+ (if (> (length marks) 0)
                                (max (apply max (map string-length display-texts)) 8)
                                9)
                            6))
                     (- rect-width 4))]
         [height (min (if (eqv? mode 'help)
                          (+ (/ (length keymap-help) 2) 2)
                          (+ (max (length marks) 1) 2))
                      (- rect-height 4))]
         [x (ceiling (max 0 (- (ceiling (/ rect-width 2)) (floor (/ width 2)))))]
         [y (ceiling (max 0 (- (ceiling (/ rect-height 2)) (floor (/ height 2)))))])
    (area (- x 1) (- y 1) width height)))

(define (calculate-text-area popup-area)
  (let ([padding-x 2]
        [padding-y 1])
    (area (+ (area-x popup-area) padding-x)
          (+ (area-y popup-area) padding-y)
          (- (area-width popup-area) (* padding-x 2))
          (- (area-height popup-area) (* padding-y 2)))))

(define (switch-or-open path line mode)
  (let* ([doc-ids (editor-all-documents)]
         [path-hash (apply hash
                           (flatten (map (lambda (x)
                                           (list (trim-current-directory (editor-document->path x))
                                                 x))
                                         doc-ids)))]
         [path-doc-id (hash-try-get path-hash path)])
    (when (eq? mode 'horizontal)
      (helix.hsplit))
    (when (eq? mode 'vertical)
      (helix.vsplit))
    (if path-doc-id
        (begin
          (editor-switch-action! path-doc-id (Action/Replace))
          (helix.goto-line line)
          (helix.static.align_view_center))
        (helix.open (string-append path ":" (number->string line))))))

(define (render-streal state area buf)
  (let* ([mode (StrealState-mode state)]
         [marks (StrealState-marks state)]
         [paths (map Mark-path marks)]
         [shortened-paths (shorten-paths paths)]
         [display-texts (mark-display-texts marks shortened-paths)]
         [streal-area (calculate-popup-area area marks display-texts mode)]
         [text-area (calculate-text-area streal-area)]
         [popup-style (theme-scope "ui.popup")]
         [mode-style (theme-scope "ui.text.focus")]
         [number-style (theme-scope "markup.list")]
         [delete-style (theme-scope "error")])
    (buffer/clear buf streal-area)
    (block/render buf streal-area (make-block popup-style (style) "all" "plain"))
    (when (not (eqv? mode 'normal))
      (frame-set-string! buf
                         (+ (area-x streal-area) 2)
                         (area-y streal-area)
                         (symbol->string mode)
                         mode-style))
    (if (eqv? mode 'help)
        (for-each
         (lambda (i)
           (let ([key (list-ref keymap-help (* i 2))]
                 [description (list-ref keymap-help (+ (* i 2) 1))])
             (frame-set-string! buf (area-x text-area) (+ (area-y text-area) i) key number-style)
             (frame-set-string! buf
                                (+ (area-x text-area) 7)
                                (+ (area-y text-area) i)
                                description
                                popup-style)))
         (range (/ (length keymap-help) 2)))
        (begin
          (when (= (length marks) 0)
            (frame-set-string! buf (area-x text-area) (area-y text-area) "  (empty)" popup-style))
          (for-each (lambda (i)
                      (let* ([mark (list-ref marks i)]
                             [display-text (list-ref display-texts i)]
                             [current-style (if (eqv? mode 'delete)
                                                  delete-style
                                                  popup-style)])
                        (frame-set-string! buf
                                           (area-x text-area)
                                           (+ (area-y text-area) i)
                                           (number->string (Mark-num mark))
                                           number-style)
                        (frame-set-string! buf
                                           (+ (area-x text-area) 2)
                                           (+ (area-y text-area) i)
                                           display-text
                                           current-style)))
                    (range (length marks)))))))

(define (handle-event state event)
  (let* ([mode (StrealState-mode state)]
         [marks (StrealState-marks state)]
         [branch (StrealState-branch state)]
         [char (key-event-char event)]
         [num (char->number (or char #\null))]
         [current-path (trim-current-directory (editor-focus-path))])
    (with-handler
     (lambda (err)
       (handle-error err)
       event-result/consume)
     (cond
       [(key-event-escape? event) event-result/close]
       [(eqv? char #\q) event-result/close]
       [(not (eqv? num #false))
        (let ([mark (mark-for-num marks num)])
          (if mark
              (if (eqv? mode 'delete)
                  (begin
                    (write-marks (remove-mark marks (Mark-num mark)) branch)
                    (set-StrealState-marks! state (get-marks branch))
                    (set-status! (string-append "Mark " (number->string num) " removed."))
                    event-result/consume)
                  (begin
                    (switch-or-open (Mark-path mark) (Mark-line mark) mode)
                    event-result/close))
              (error (string-append "No mark at " (number->string num) "."))))]
       [(eqv? char #\s)
        (if (string=? current-path "")
            (error "Can't add mark with no file open.")
            (let* ([current-line (helix.static.get-current-line-number)]
                   [existing (find-mark-at marks current-path current-line)])
              (if existing
                  (begin
                    (write-marks (remove-mark marks (Mark-num existing)) branch)
                    (set-status! (string-append "Mark " (number->string (Mark-num existing)) " removed.")))
                  (let ([num (next-available-num marks)])
                    (if num
                        (begin
                          (write-marks (set-mark marks num current-path current-line) branch)
                          (set-status! (string-append "Mark " (number->string num) " set: "
                                                      current-path ":" (number->string current-line))))
                        (error "All mark slots are full."))))
              event-result/close))]
       [(eqv? char #\e)
        (switch-or-open (get-streal-file-path branch) 1 mode)
        event-result/close]
       [(eqv? char #\C)
        (write-marks '() branch)
        (set-status! "All marks cleared.")
        event-result/close]
       [(eqv? char #\d)
        (toggle-mode state 'delete)
        event-result/consume]
       [(eqv? char #\h)
        (toggle-mode state 'horizontal)
        event-result/consume]
       [(eqv? char #\v)
        (toggle-mode state 'vertical)
        event-result/consume]
       [(eqv? char #\?)
        (toggle-mode state 'help)
        event-result/consume]
       [(eqv? char #\:) event-result/ignore]
       [else event-result/consume]))))

(define (validate-flag flag)
  (when (and (not (string=? flag "")) (not (string=? flag "--per-branch")))
    (error (string-append "unknown flag '" flag "'"))))

(define (flag->branch flag)
  (validate-flag flag)
  (if (string=? flag "--per-branch") (get-git-branch) #false))

;;@doc
;; Open the Streal popup showing all numbered marks
;; Flags:
;;   --per-branch  Keep a separate Streal file per Git branch
(define (streal-open [flag ""])
  (with-handler handle-error
                (let ([branch (flag->branch flag)])
                  (push-component! (new-component! "streal"
                                                   (StrealState (get-marks branch) 'normal branch)
                                                   render-streal
                                                   (hash "handle_event" handle-event))))))

;;@doc
;; Set a numbered mark (1-9) at the current cursor position
;; Usage: :streal-mark <number> [--per-branch]
(define (streal-mark num [flag ""])
  (with-handler handle-error
                (let* ([branch (flag->branch flag)]
                       [marks (get-marks branch)]
                       [current-path (trim-current-directory (editor-focus-path))]
                       [current-line (helix.static.get-current-line-number)]
                       [n (string->number num)])
                  (if (and n (>= n 1) (<= n MAX-MARKS))
                      (begin
                        (write-marks (set-mark marks n current-path current-line) branch)
                        (set-status! (string-append "Mark " (number->string n) " set: "
                                                    current-path ":" (number->string current-line))))
                      (error (string-append "Mark number must be between 1 and " (number->string MAX-MARKS) "."))))))

;;@doc
;; Jump to a numbered mark (1-9)
;; Usage: :streal-goto <number> [--per-branch]
(define (streal-goto num [flag ""])
  (with-handler handle-error
                (let* ([branch (flag->branch flag)]
                       [marks (get-marks branch)]
                       [n (string->number num)]
                       [mark (if n (mark-for-num marks n) #false)])
                  (cond
                    [(not n) (error "Invalid mark number.")]
                    [(not mark) (error (string-append "No mark at " num "."))]
                    [else (switch-or-open (Mark-path mark) (Mark-line mark) 'normal)]))))

(provide streal-open streal-mark streal-goto)
