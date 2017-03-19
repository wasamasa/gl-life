(use srfi-4 lolevel
     (prefix opengl-glew gl:)
     (prefix glfw3 glfw:)
     (prefix gl-utils glu:))

(define width 500)
(define height 500)

(define triangle
  (glu:make-mesh
   vertices: '(attributes: ((position float: 3))
               initial-elements: ((position
                                    0    0.5 0
                                   -0.5 -0.5 0
                                    0.5 -0.5 0)))))

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

(define (draw! vao window prog)
  (gl:clear (bitwise-ior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
  (gl:use-program prog)

  (gl:bind-vertex-array vao)
  (gl:draw-arrays gl:+triangles+ 0 (glu:mesh-n-vertices triangle))

  (glfw:poll-events)
  (glfw:swap-buffers window))

(define (main)
  (glfw:with-window (width height "Game of Life"
                     resizable: #f
                     context-version-major: 2
                     context-version-minor: 1)
    (let ((window (glfw:window)))
      (glfw:make-context-current window)
      (let ((prog (init-gl!)))
        (glu:mesh-make-vao! triangle `((position . ,(gl:get-attrib-location
                                                     prog "position"))))
        (let ((vao (glu:mesh-vao triangle)))
          (let loop ()
            (when (not (glfw:window-should-close window))
              (draw! vao window prog)
              (loop))))))))

(main)
