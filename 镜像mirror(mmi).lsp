(defun c:mmi()
  (setq en (ssget))
  (setq pt1 (getpoint "\\n指定鏡射線的第一點:"))
  (setq pt2 (getpoint pt1 "\\n指定鏡射線的第二點:"))
  (initget "Y N")
  (setq qq (getkword "\\n是否刪除來源物件？[是(Y)/否(N)] <Y>"))
  (if (= qq nil) (setq qq "Y"))
  (command "mirror" en "" pt1 pt2 qq)
  (princ)
)
(prompt "\\n<<<<<  程式指令為: mmi >>>>>")

