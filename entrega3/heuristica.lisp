; Esto nos va a ayudar a definir todos los sectores
(defmacro defsectores (&body body)
  (let
    ((solucion (gensym)))
    (labels
      ((sector (body) `(lambda (,solucion) ,(walk body)))
       (walk (body)
         (loop for clause in body collect
               (if (listp clause)
                 (walk clause)
                 (if (member clause '(and not or))
                   clause
                   `(getf ,solucion ',clause))))))
      `(list
         ,@(loop for (id evaluador) on body by #'cddr append
             `(',id ,(sector evaluador)))))))

(defun sector-satisfecho (sectores sector solucion)
  (funcall (getf sectores sector) solucion))

(defun partido-satisfecho (sectores solucion)
  (loop for (sector evaluador) on sectores by #'cddr
        if (sector-satisfecho sectores sector solucion)
        collect sector into satisfechos
        else
        collect sector into insatisfechos
        finally
        (return (values (null insatisfechos) satisfechos insatisfechos))))

(defun fitness (sectores solucion)
  (- (/ (length sectores) 2)
     (length (nth 2 (multiple-value-list
                      (partido-satisfecho sectores solucion))))))

(defun build-seed (asuntos &optional (generator (constantly nil)))
  (loop for asunto in asuntos appending
        `(,asunto ,(funcall generator))))

(defun neighbours (solution)
  (loop for (asunto valor) on solution by #'cddr collecting
        (let ((neighbour (copy-list solution)))
          (setf (getf neighbour asunto) (not valor))
          neighbour)))

(defun hillclimb (problem seed)
  (let
    ((visited (make-hash-table :test #'equal))
     (to-visit (list seed))
     (max-fitness 0)
     (candidates ()))
    (loop
      while to-visit do
      (let ((current (first to-visit)))
        (setf to-visit (rest to-visit))
        (unless (gethash current visited)
          (setf (gethash current visited) t)
          (let ((fitness (fitness problem current)))
            (format t "Max:~a~%Candidate: ~a~%Fitness:~a~%"
                    max-fitness current fitness)
            (unless (< fitness max-fitness)
              (when (= fitness max-fitness)
                (format t "Candidate is fit! Added to pool~%")
                (setf candidates (append (list current) candidates)
                      to-visit (append (neighbours current) to-visit)))
              (when (> fitness max-fitness)
                (format t "Candidate is better! Replacing pool~%")
                (setf candidates (list current)
                      to-visit (neighbours current)
                      max-fitness fitness)))))))
    (values candidates max-fitness)))

; Definimos lista de asuntos
(defparameter asuntos
  '(huelga-empresarios diputados-juventud importaciones-abrir))

; Definimos lista de sectores
(defparameter sectores
  (defsectores
    corriente-historica
    (not (and huelga-empresarios
              diputados-juventud
              importaciones-abrir))
    juventud
    (and huelga-empresarios
         diputados-juventud)
    sindicatos
    (not (or huelga-empresarios
             importaciones-abrir))
    empresarios
    (or huelga-empresarios
        importaciones-abrir)))

; Corremos resolvedor e imprimimos todos los mejores candidatos
(multiple-value-bind
         (soluciones aptitud) (hillclimb sectores (build-seed asuntos))
         (dolist (solucion soluciones)
           (multiple-value-bind
             (satisfecho satisfechos insatisfechos)
             (partido-satisfecho sectores solucion)
             (format t "Solución: ~a~%Satisfechos: ~a~%Insatisfechos: ~a~%"
                   solucion satisfechos insatisfechos))))
