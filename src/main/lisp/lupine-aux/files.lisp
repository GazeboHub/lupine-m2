
(in-package #:lupine/aux)

(defvar %file-buffer-size% 1024)

(defvar %file-buffer-element-type% '(unsigned-byte 8))

(defun file= (f1 f2 &key (start 0) end (test #'=) (element-type %file-buffer-element-type%) (buffer-size %file-buffer-size%))
    ;; Initial motivation: This function design was begun, originally, with the intention of providing a capability for testing a specific section of a source file for equivalence onto a given section of a template file - as for ensuring the presence of specific licensing information within a source file, from procedures that may be executed before a source code commit. Secondly, it was intended that the function be reusable for other file byte comparisons. 
    (let ((n (when end start))
          (test-fn (coerce test 'function))
          (b1 (make-array file-buffer-size :element-type element-type))
          (b2 (make-array file-buffer-size :element-type element-type)))
         (declare (type function test-fn))
        (with-open-file (f1s f1 :direction :input :element-type element-type)
            (setf (file-position f1s) start)
          (with-open-file (f2s f2 :direction :input :element-type element-type)
            (setf (file-position f2s) start)
            (block read-loop
                (flet ((return-false () (return-from read-loop nil))
                       (return-true () (return-from read-loop t)))
                    (loop 
                        (cond
                          ((and n (= n end))
                           (return-from read-loop t))
                          (t 
                            (let ((r1n (read-sequence b1 f1s))
                                  (r2n (read-sequence b2 f2s)
                                  long-read-p ;;; signifies an "effective EOF"
                                  ))
                                  ;; FIXME: Revise this to account for a read past END
                                (when end
                                    ;; FIXME: Test this logic
                                    (let ((foo-1 (+ n r1n))
                                          (foo-2 (+ n r2n)))
                                        (when (> foo-1 end)
                                            (setq long-read-p t
                                                  r1n (- foo-1 (- foo-1 end))))
                                        (when (> foo-2 end)
                                            (setq long-read-p t
                                                  r2n (- foo-2 (- foo-2 end))))
                                ))
                                (cond
                                    ((not (= (the fixnum r1n) (the fixnum r2n))))
                                     (return-false)
                                    ((zerop (the fixnum r1n))
                                     (return-true))
                                    (t
                                        (dotimes (nth r1n)
                                            (let ((it-1 (aref b1 nth))
                                                  (it-2 (aref b2 nth)))
                                              (unless (funcall test-fn it-1 it-2)
                                                (return-false))))))
                                (cond
                                    (long-read-p (return-true))
                                    (n (incf n (the fixnum r1n))))
                                    ))))))))))) ;; file=