
(defun c:WBChC (/ ss)
  (princ (strcat "\n*** 标注外包尺寸自动生成软件V110620 ***"))
  (princ (strcat "\n       [它山之石图形工作室]"))
  (princ)

  (GL-Sys-Begin)
  (if (setq ss (ssget '((0 . "dimension"))))
    (GL-Dim-WBCC ss nil)
  ) ;_结束if

  (GL-Sys-End)
  (princ (strcat "\n*** 标注外包尺寸自动生成软件V110620 ***"))
  (princ (strcat "\n       [它山之石图形工作室]"))
  (princ)
)

