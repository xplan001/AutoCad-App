defun fillread(/ ffn ff str );读文件到列表 
(setq ffn (getfiled "选择文件" "" "txt" 2)
ff (open ffn "r")
return '()
) 
(while (setq str (read-line ff))
(setq return (cons str return))
)
(setq return(reverse return))
(close ff)
(setq ffn nil ff nil str nil)
return
) 