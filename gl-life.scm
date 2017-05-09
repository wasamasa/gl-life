(use random-bsd srfi-18
     (prefix opengl-glew gl:)
     (prefix glfw3 glfw:)
     (prefix gl-utils glu:))

(define width 500)
(define height 500)

(define rows 20)
(define columns 20)

(define-record cell mesh live? live?-next)

(define (cell-vertices x y)
  ;; convert coordinates from 0..1 to -1..1
  (define (->normalize n)
    (- (* n 2) 1))
  (let ((x1 (->normalize (/ x columns)))
        (x2 (->normalize (/ (+ x 1) columns)))
        (y1 (->normalize (/ y rows)))
        (y2 (->normalize (/ (+ y 1) rows))))
    (list x1 y1 0
          x1 y2 0
          x2 y2 0

          x1 y1 0
          x2 y1 0
          x2 y2 0)))

(define (create-cell prog x y live?)
  (let ((mesh
         (glu:make-mesh
          vertices:
          `(attributes: ((position float: 3))
            initial-elements: ((position . ,(cell-vertices x y)))))))
    (glu:mesh-make-vao! mesh `((position . ,(gl:get-attrib-location
                                             prog "position"))))
    (make-cell mesh live? live?)))

(define treshold 0.15)

(define (create-board prog)
  (let ((board (make-vector (* rows columns))))
    (do ((row 0 (add1 row)))
        ((= row rows))
      (do ((column 0 (add1 column)))
          ((= column columns))
        (let ((cell (create-cell prog column row (< (random-real) treshold))))
          (vector-set! board (+ (* row columns) column) cell))))
    board))

(define square-vertices 6) ; ugh

(define vertex-shader-source
  "#version 120
   attribute vec3 position;
   void main() {
       gl_Position = vec4(position, 1.0);
   }")

(define fragment-shader-source
  "#version 120
   void main() {
       gl_FragColor = vec4(1, 1, 1, 1);
   }")

(define (init-gl!)
  (gl:init)
  (let ((vertex-shader (glu:make-shader gl:+vertex-shader+
                                        vertex-shader-source))
        (fragment-shader (glu:make-shader gl:+fragment-shader+
                                          fragment-shader-source)))
    (glu:make-program (list vertex-shader fragment-shader))))

(define (update-cell! cell board x y)
  (cell-live?-set! cell (cell-live?-next cell))
  (let ((neighbors (live-neighbors cell board x y)))
    (if (cell-live? cell)
        (cond
         ((< neighbors 2)
          (cell-live?-next-set! cell #f))
         ((or (= neighbors 2) (= neighbors 3))
          (cell-live?-next-set! cell #t))
         ((> neighbors 3)
          (cell-live?-next-set! cell #f)))
        (if (= neighbors 3)
            (cell-live?-next-set! cell #t)))))

(define (live-neighbors cell board x y)
  (define (live? x y)
    (cond
     ((= x columns) (live? 0 y))
     ((< x 0) (live? (- columns 1) y))
     ((= y rows) (live? x 0))
     ((< y 0) (live? x (- rows 1)))
     (else
      (cell-live? (vector-ref board (+ (* y columns) x))))))
  (+ (if (live? (- x 1) y) 1 0) ; left
     (if (live? (+ x 1) y) 1 0) ; right
     (if (live? x (+ y 1)) 1 0) ; up
     (if (live? x (- y 1)) 1 0) ; down
     (if (live? (- x 1) (+ y 1)) 1 0) ; top-left
     (if (live? (+ x 1) (+ y 1)) 1 0) ; top-right
     (if (live? (- x 1) (- y 1)) 1 0) ; bottom-left
     (if (live? (+ x 1) (- y 1)) 1 0) ; bottom-right
     ))

(define (draw! board window prog)
  (gl:clear (bitwise-ior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
  (gl:use-program prog)

  (do ((row 0 (add1 row)))
      ((= row rows))
    (do ((column 0 (add1 column)))
        ((= column columns))
      (let ((cell (vector-ref board (+ (* row columns) column))))
        (when (cell-live? cell)
          (gl:bind-vertex-array (glu:mesh-vao (cell-mesh cell)))
          (gl:draw-arrays gl:+triangles+ 0 square-vertices)))))

  (glfw:poll-events)
  (glfw:swap-buffers window))

(define fps 10)

(define (main)
  (glfw:with-window (width height "Game of Life"
                     resizable: #f
                     context-version-major: 2
                     context-version-minor: 1)
    (let ((window (glfw:window)))
      (glfw:make-context-current window)
      (let* ((prog (init-gl!))
             (board (create-board prog)))
        (let loop ()
          (when (not (glfw:window-should-close window))
            (let ((now (current-milliseconds)))
              (do ((row 0 (add1 row)))
                  ((= row rows))
                (do ((column 0 (add1 column)))
                    ((= column columns))
                  (let ((cell (vector-ref board (+ (* row columns) column))))
                    (update-cell! cell board column row))))
              (draw! board window prog)
              (gc)
              (thread-sleep! (/ (- (/ 1000 fps) (- (current-milliseconds) now))
                                1000)))
            (loop)))))))

(main)
