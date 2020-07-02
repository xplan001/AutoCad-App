;作者：炫翔   QQ：2363673534  网址:http://xuanxiang.ys168.com 
(defun xx-layhb(O_Lay D_Lay)
     (setvar "cmdecho" 0) ;指令执行过程不响应
(command "_.change" (ssget "x" (list (cons 8 O_Lay))) "" "p" "la" D_Lay "")
(command "_.purge" "la" O_Lay "n")
(princ))

(defun xx-get-dxf (code ename) 
(cdr (assoc code (entget ename)))) 


(defun c:TCHB ()
     (setvar "cmdecho" 0) ;指令执行过程不响应
     (PRINC "\n【炫翔CAD插件】---合并图层功能")(PRINC) 
(progn 
(setq ent (entsel "\n请选择目标层上的对象:"))
(setq la (cdr(assoc 8 (entget (car ent)))))
)
     (PRINC "\n框选要合并的层")
  (setq	ss (SSGET)
	i  -1
  )
  (while (setq s1 (ssname ss (setq i (1+ i))))
    (setq tx (xx-get-DXF 8 s1))
 (xx-layhb tx la))

(princ)
)

