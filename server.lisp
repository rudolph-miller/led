(ql:quickload :swank)
(defun main ()
  (swank:create-server :port 4005)
  (loop do (sleep 10)))
