(use srfi-4 lolevel
     (prefix opengl-glew gl:)
     (prefix glfw3 glfw:)
     (prefix gl-utils glu:))

(define width 500)
(define height 500)

(define rows 10)
(define columns 10)

(define-record cell x y vao)

(define (cell-vertices x y)
  (define (->normalize n) (- (* n 2) 1))
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

(define (create-cell prog x y)
  (let ((mesh
         (glu:make-mesh
          vertices:
          `(attributes: ((position float: 3))
            initial-elements: ((position . ,(cell-vertices x y)))))))
    (glu:mesh-make-vao! mesh `((position . ,(gl:get-attrib-location
                                             prog "position"))))
    (make-cell x y (glu:mesh-vao mesh))))

(define (create-board prog)
  (let ((board (make-vector (* rows columns))))
    (do ((row 0 (add1 row)))
        ((= row rows))
      (do ((column 0 (add1 column)))
          ((= column columns))
        (vector-set! board (+ (* row columns) column)
                     (create-cell prog column row))))
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

(define (draw! board window prog)
  (gl:clear (bitwise-ior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
  (gl:use-program prog)

  (do ((row 0 (add1 row)))
      ((= row rows))
    (do ((column 0 (add1 column)))
        ((= column columns))
      (let ((vao (cell-vao (vector-ref board (+ (* row columns) column)))))
        (gl:bind-vertex-array vao)
        (gl:draw-arrays gl:+triangles+ 0 square-vertices))))

  (glfw:poll-events)
  (glfw:swap-buffers window))

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
            (draw! board window prog)
            (loop)))))))

(main)
