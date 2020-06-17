; Next available MSG number is   83 
; MODULE_ID CHTEXT_LSP_
;;;
;;;    CHTEXT.lsp - change text
;;;
;;;    Copyright 1997 by Autodesk, Inc.
;;;
;;;    Permission to use, copy, modify, and distribute this software
;;;    for any purpose and without fee is hereby granted, provided
;;;    that the above copyright notice appears in all copies and
;;;    that both that copyright notice and the limited warranty and
;;;    restricted rights notice below appear in all supporting
;;;    documentation.
;;;
;;;    AUTODESK PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.
;;;    AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF
;;;    MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK, INC.
;;;    DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE
;;;    UNINTERRUPTED OR ERROR FREE.
;;;
;;;    Use, duplication, or disclosure by the U.S. Government is subject to
;;;    restrictions set forth in FAR 52.227-19 (Commercial Computer
;;;    Software - Restricted Rights) and DFAR 252.227-7013(c)(1)(ii) 
;;;    (Rights in Technical Data and Computer Software), as applicable.
;;;  
;;;--------------------------------------------------------------------------;
;;; DESCRIPTION
;;;   This is a "text processor" which operates in a global manner 
;;;   on all of the text entities that the user selects; i.e., the
;;;   Height, Justification, Location, Rotation, Style, Text, and
;;;   Width can all be changed globally or individually, and the
;;;   range of values for a given parameter can be listed.
;;;   
;;;   The command is called with CHT from the command line at which
;;;   time the user is asked to select the objects to change.
;;;   
;;;     Select text to change. 
;;;     Select objects:
;;;  
;;;   If nothing is selected the message "ERROR: Nothing selected." 
;;;   is displayed and the command is terminated.  If more than 25
;;;   entities are selected the following message is displayed while
;;;   the text entities are sorted out from the non-text entities.
;;;   A count of the text entities found is then displayed.
;;;   
;;;     Verifying the selected entities...
;;;     nnn  text entities found.
;;;     CHText:  Height/Justification/Location/Rotation/Style/Text/Undo/Width:
;;;   
;;;   A typical example of the prompts you may encounter follows:
;;;   
;;;   If you select a single text entity to change and ask to change
;;;   the height, the prompt looks like this:
;;;   
;;;     CHText:  Height/Justification/Location/Rotation/Style/Text/Undo/Width:
;;;     New text height for text entity. <0.08750000>:
;;;   
;;;   If you select more than one text entity to change and ask to change
;;;   the height, the prompt looks like this:
;;;   
;;;     CHText:  Height/Justification/Location/Rotation/Style/Text/Undo/Width:
;;;     Individual/List/<New height for all entities>:
;;;   
;;;   Typing "L" at this prompt returns a prompt showing you the range of
;;;   values that you are using for your text.
;;;   
;;;     Height -- Min: 0.05000000  Max: 0.10000000  Ave: 0.08392857
;;;   
;;;   Typing "I" at this prompt puts you in a loop, processing the text
;;;   entities you have selected one at a time, and giving the same prompt
;;;   you get for a single text entity shown above.
;;;   
;;;   Pressing ENTER at this point puts you back at the Command: prompt.
;;;   Selecting any of the other options allows you to change the text
;;;   entities selected. 
;;;   
;;;---------------------------------------------------------------------------;

(defun cht_Main ( / sset opt ssl nsset temp unctr ct_ver sslen style hgt rot
                    txt ent loc loc1 just-idx justp justq orthom
                    cht_ErrorHandler cht_OrgError cht_OrgCmdecho
                    cht_OrgTexteval cht_OrgHighlight)

  ;; Reset if changed
  (setq ct_ver "2.00")

  ;; Internal error handler defined locally
  (defun cht_ErrorHandler (s)
    (if (/= s "Function cancelled")
      (if (= s "quit / exit abort")
        (princ)
        (princ (strcat "\nError: " s))
      )
    )
    (eval (read U:E))
    ;;  Reset original error handler if there
    (if cht_OrgError (setq *error* cht_OrgError))
    (if temp (redraw temp 1))
    (ai_undo_off) ;; restore undo state
    (if cht_OrgCmdecho (setvar "cmdecho" cht_OrgCmdecho))
    (if cht_OrgTexteval (setvar "texteval" cht_OrgTexteval)) 
    (if cht_OrgHighlight (setvar "highlight" cht_OrgHighlight)) 
    (princ)
  )

  ;; Set error handler
  (if *error*
    (setq cht_OrgError *error*
          *error* cht_ErrorHandler)
    (setq *error* cht_ErrorHandler) 
  )

  ;; Set undo groups and ends with (eval(read U:G)) or (eval(read U:E))
  (setq U:G "(command \"_.undo\" \"_group\")"
        U:E "(command \"_.undo\" \"_en\")"
  )
  
  (ai_undo_on)       ;; enable undo
  
  (setq cht_OrgCmdecho (getvar "cmdecho"))
  (setq cht_OrgHighlight (getvar "highlight")) 
  (setvar "cmdecho" 0)
  
  (princ (strcat "\nChange text, Version "
                 ct_ver 
                 ", Copyright © 1997 by Autodesk, Inc."))
  (prompt "\nSelect annotation objects to change.")
  (setq sset (ai_aselect))
  (if (null sset) 
    (progn
      (princ "\nNo objects selected.")
      (exit)
    )
  )
  ;; Validate selection set
  (setq ssl   (sslength sset)
        nsset (ssadd))
  (if (> ssl 25)
    (princ "\nVerifying selected objects...")
  )
  (while (> ssl 0)
    (setq temp (ssname sset (setq ssl (1- ssl))))
    (if (or
          (= (cdr (assoc 0 (entget temp))) "TEXT")
          (= (cdr (assoc 0 (entget temp))) "ATTDEF")
          (= (cdr (assoc 0 (entget temp))) "MTEXT")
        )
      (ssadd temp nsset)
    )
  )
  (setq ssl (sslength nsset)
        sset nsset
        unctr 0
  )
  (print ssl)
  (princ "annotation objects found.")

  ;; Main loop
  (setq opt T)
  (while (and opt (> ssl 0))
    (setq unctr (1+ unctr))
    (command "_.UNDO" "_GROUP")
    (initget "Location Justification Style Height Rotation Width Text Undo")
    (setq opt (getkword 
      "\nHeight/Justification/Location/Rotation/Style/Text/Undo/Width: "))
    (if opt
      (cond
        ((= opt "Undo")
          (cht_Undo)
        )
        ((= opt "Location")
          (cht_Location)
        )
        ((= opt "Justification")
          (cht_Justification)
        )
        ((= opt "Style")
          (cht_Property "Style"    "New style name"      7) )
        ((= opt "Height")
          (cht_Property "Height"   "New height"         40) )
        ((= opt "Rotation")
          (cht_Property "Rotation" "New rotation angle" 50) )
        ((= opt "Width")
          (cht_Property "Width"    "New width factor"   41) )
        ((= opt "Text")
          (cht_Text)
        )
      )
      (setq opt nil)
    )
    (command "_.UNDO" "_END")
  )

  ;; Restore
  (if cht_OrgError (setq *error* cht_OrgError))
  (eval (read U:E))
  (ai_undo_off) ;; restore undo state
  (if cht_OrgTexteval (setvar "texteval" cht_OrgTexteval)) 
  (if cht_OrgHighlight (setvar "highlight" cht_OrgHighlight)) 
  (if cht_OrgCmdecho (setvar "cmdecho" cht_OrgCmdecho))
  (princ)
)

;;; Undo an entry
(defun cht_Undo ()
  (if (> unctr 1)
    (progn
      (command "_.UNDO" "_END")
      (command "_.UNDO" "2")
      (setq unctr (- unctr 2))
    )
    (progn
      (princ "\nNothing to undo. ")
      (setq unctr (- unctr 1))
    )
  )
)

;;; Change the location of an entry
(defun cht_Location ()
  (setq sslen (sslength sset)
        style ""
        hgt   ""
        rot   ""
        txt   ""
  )
  (command "_.CHANGE" sset "" "")
  (while (> sslen 0)
    (setq ent (entget(ssname sset (setq sslen (1- sslen))))
          opt (list (cadr (assoc 11 ent))
                    (caddr (assoc 11 ent))
                    (cadddr (assoc 11 ent)))
    )
    (prompt "\nNew text location: ")
    (command pause)
    (if (null loc)
      (setq loc opt)
    )
    (command style hgt rot txt)
  )
  (command)
)

;;; Change the justification of an entry
(defun cht_Justification ()
  (initget "TL TC TR ML MC MR BL BC BR Align Center Fit Left Middle Right ?")
  (setq sslen (sslength sset))
  (setq justp (getkword "\nAlign/Fit/Center/Left/Middle/Right/TL/TC/TR/ML/MC/MR/BL/BC/BR/<?>: "))
  (cond
    ((= justp "Left")    (setq justp 0 justq 0 just-idx 4) )
    ((= justp "Center")  (setq justp 1 justq 0 just-idx 5) )
    ((= justp "Right")   (setq justp 2 justq 0 just-idx 6) )
    ((= justp "Align")   (setq justp 3 justq 0 just-idx 1) )
    ((= justp "Fit")     (setq justp 5 justq 0 just-idx 1) )
    ((= justp "TL")      (setq justp 0 justq 3 just-idx 1) )
    ((= justp "TC")      (setq justp 1 justq 3 just-idx 2) )
    ((= justp "TR")      (setq justp 2 justq 3 just-idx 3) )
    ((= justp "ML")      (setq justp 0 justq 2 just-idx 4) )
    ((= justp "Middle")  (setq justp 4 justq 0 just-idx 5) )
    ((= justp "MC")      (setq justp 1 justq 2 just-idx 5) )
    ((= justp "MR")      (setq justp 2 justq 2 just-idx 6) )
    ((= justp "BL")      (setq justp 0 justq 1 just-idx 7) )
    ((= justp "BC")      (setq justp 1 justq 1 just-idx 8) )
    ((= justp "BR")      (setq justp 2 justq 1 just-idx 9) )
    ((= justp "?")       (setq justp nil)       )
    (T                   (setq justp nil)       )
  )   
  (if justp
    (progn ;; Process them...
      (while (> sslen 0)
        (setq ent (entget (ssname sset (setq sslen (1- sslen)))))
        (cond
          ((= (cdr (assoc 0 ent)) "MTEXT")
            (setq ent (subst (cons 71 just-idx) (assoc 71 ent) ent))
          )
          ((= (cdr (assoc 0 ent)) "TEXT")
            (setq ent (subst (cons 72 justp) (assoc 72 ent) ent)
                  opt (trans (list (cadr (assoc 11 ent))
                                   (caddr (assoc 11 ent))
                                   (cadddr (assoc 11 ent))) 
                             (cdr (assoc -1 ent)) ;; from ECS
                             1)               ;; to current UCS
            )
            (setq ent (subst (cons 73 justq) (assoc 73 ent) ent))
            (cond
              ((or (= justp 3) (= justp 5))
                (prompt "\nNew text alignment points: ")
                (if (= (setq orthom (getvar "orthomode")) 1)
                  (setvar "orthomode" 0)
                )
                (redraw (cdr (assoc -1 ent)) 3)
                (initget 1)
                (setq loc (getpoint))
                (initget 1)
                (setq loc1 (getpoint loc))
                (redraw (cdr (assoc -1 ent)) 1)
                (setvar "orthomode" orthom)
                (setq ent (subst (cons 10 loc) (assoc 10 ent) ent))
                (setq ent (subst (cons 11 loc1) (assoc 11 ent) ent))
              )
              ((or (/= justp 0) (/= justq 0))
                (redraw (cdr (assoc -1 ent)) 3)
                (prompt "\nNew text location: ")
                (if (= (setq orthom (getvar "orthomode")) 1)
                  (setvar "orthomode" 0)
                )
                (setq loc (getpoint opt))
                (setvar "orthomode" orthom)
                (redraw (cdr (assoc -1 ent)) 1)
                (if (null loc)
                  (setq loc opt)
                  (setq loc (trans loc 1 (cdr (assoc -1 ent))))
                )
                (setq ent (subst (cons 11 loc) (assoc 11 ent) ent))
              )
            )
          )
        )
        (entmod ent)
      )
    )
    (progn	;; otherwise list options
      (textpage)
      (princ "\nAlignment options:\n")
      (princ "\t  TL     TC      TR\n")
      (princ "\t  ML     MC      MR\n")
      (princ "\t  BL     BC      BR\n")
      (princ "\t Left   Center  Right\n")
      (princ "\tAlign   Middle  Fit\n")
      (princ "\nPress ENTER to continue: ")
      (grread)
      (princ "\r                                                            ")
      (graphscr)
    )
  )
  (command)
)

;;; Change the text of an object
(defun cht_Text ( / ans)
  (setq sslen (sslength sset))
  (initget "Globally Individually Retype")
  (setq ans (getkword 
    "\nFind and replace text.  Individually/Retype/<Globally>:"))
  (setq cht_OrgTexteval (getvar "texteval"))
  (setvar "texteval" 1)
  (cond 
    ((= ans "Individually")
      (progn
        (initget "Yes No")
        (setq ans (getkword "\nEdit text in dialog? <Yes>:"))
      )
  
      (while (> sslen 0)
        (redraw (setq sn (ssname sset (setq sslen (1- sslen)))) 3)
        (setq ss (ssadd))
        (ssadd (ssname sset sslen) ss)
        (if (= ans "No") 
          (cht_Edit ss)
          (command "_.DDEDIT" sn "")
        )
        (redraw sn 1)
      )
    )
    ((= ans "Retype")
      (while (> sslen 0)
        (setq ent (entget (ssname sset (setq sslen (1- sslen)))))
        (redraw (cdr (assoc -1 ent)) 3)
        (prompt (strcat "\nOld text: " (cdr (assoc 1 ent))))
        (setq nt (getstring  T "\nNew text: "))
        (redraw (cdr (assoc -1 ent)) 1)
        (if (> (strlen nt) 0)
          (entmod (subst (cons 1 nt) (assoc 1 ent) ent))
        )
      )
    )
    (T
      (cht_Edit sset)   ;; Change all
    )
  )
  (setvar "texteval" cht_OrgTexteval)
)

;;; The old CHGTEXT command - rudimentary text editor
(defun C:CHGTEXT () (cht_Edit nil))

(defun cht_Edit (objs / last_o tot_o ent o_str n_str st s_temp 
                       n_slen o_slen si chf chm cont ans class)
  ;; Select objects if running standalone
  (if (null objs)
    (setq objs (ssget))
  )
  (setq chm 0)
  (if objs 
    (progn                   ;; If any objects selected
      (if (= (type objs) 'ENAME) 
        (progn
          (setq ent (entget objs))
          (princ (strcat "\nExisting string: " (cdr (assoc 1 ent))))
        )
        (if (= (sslength objs) 1)
          (progn
            (setq ent (entget (ssname objs 0)))
            (princ (strcat "\nExisting string: " (cdr (assoc 1 ent))))
          )
        )
      )
      (setq o_str (getstring "\nMatch string   : " t))
      (setq o_slen (strlen o_str))
      (if (/= o_slen 0)
        (progn
          (setq n_str (getstring "\nNew string     : " t))
          (setq n_slen (strlen n_str))
          (setq last_o 0 
                tot_o  (if (= (type objs) 'ENAME)
                         1
                         (sslength objs)
                       )
          )
          ;; For each selected object...
          (while (< last_o tot_o)
            (setq class (cdr (assoc 0 (setq ent (entget (ssname objs last_o))))))
            (if (or (= "TEXT" class)
                    (= "MTEXT" class) )
              (progn
                (setq chf nil si 1)
                (setq s_temp (cdr (assoc 1 ent)))
                (while (= o_slen (strlen (setq st (substr s_temp si o_slen))))
                  (if (= st o_str)
                    (progn
                      (setq s_temp (strcat 
                                     (if (> si 1)
                                       (substr s_temp 1 (1- si)) 
                                       ""
                                     )
                                     n_str
                                     (substr s_temp (+ si o_slen))
                                   )
                      )
                      (setq chf t)    ;; Found old string
                      (setq si (+ si n_slen))
                    )
                    (setq si (1+ si))
                  )
                )
                (if chf 
                  (progn              ;; Substitute new string for old
                    ;; Modify the TEXT entity
                    (entmod (subst (cons 1 s_temp) (assoc 1 ent) ent))
                    (setq chm (1+ chm))
                  )
                )
              )
            )
            (setq last_o (1+ last_o))
          )
        )
        ;; else go on to the next line...
      )
    )
  )
  (if (/= (type objs) 'ENAME)
    ;; Print total lines changed
    (if (/= (sslength objs) 1)
      (princ (strcat (rtos chm 2 0) " text lines changed."))
    )
  )
  (terpri)
)

;;; Main procedure for manipulating text entities
(defun cht_Property (typ prmpt fld / temp ow nw ent tw sty w hw lw 
                              sslen n sn ssl)
  (if (= (sslength sset) 1)           ;; Special case if there is only
                                      ;; one entity selected
    ;; Process one entity.
    (cht_ProcessOne)
    ;; Else
    (progn
      ;; Set prompt string.
      (cht_SetPrompt)
      (if (= nw "List")
        ;; Process List request.
        (cht_ProcessList)
        (if (= nw "Individual")
          ;; Process Individual request.
          (cht_ProcessIndividual)
          (if (= nw "Select")
            ;; Process Select request.
            (cht_ProcessSelect)
            ;; Else
            (progn
              (if (= typ "Rotation")
                (setq nw (* (/ nw 180.0) pi))
              )
              (if (= (type nw) 'STR)
                (if (not (tblsearch "style" nw))
                  (progn
                    (princ (strcat nw ": Style not found. "))
                  )
                  (cht_ProcessAll)
                )
                (cht_ProcessAll)
              )
            )
          )
        )
      )
    )
  )
)

;;; Change all of the entities in the selection set
(defun cht_ProcessAll (/ hl temp)
  (setq sslen (sslength sset))
  (setq hl (getvar "highlight"))
  (setvar "highlight" 0)
  (while (> sslen 0)
    (setq temp (ssname sset (setq sslen (1- sslen))))
    (entmod (subst (cons fld nw)
                   (assoc fld (setq ent (entget temp)))
                   ent ) )
  )
  (setvar "highlight" hl)
)

;;; Change one text entity
(defun cht_ProcessOne ()
  (setq temp (ssname sset 0))
  (setq ow (cdr (assoc fld (entget temp))))
  (if (= opt "Rotation")
    (setq ow (/ (* ow 180.0) pi))
  )
  (redraw (cdr (assoc -1 (entget temp))) 3)
  (initget 0)
  (if (= opt "Style")
    (setq nw (getstring (strcat prmpt " <" ow ">: ")))
    (setq nw (getreal (strcat prmpt " <" (rtos ow 2) ">: ")))
  )
  (if (or (= nw "") (= nw nil))
    (setq nw ow)
  )
  (redraw (cdr (assoc -1 (entget temp))) 1)
  (if (= opt "Rotation")
    (setq nw (* (/ nw 180.0) pi))
  )
  (if (= opt "Style")
    (if (null (tblsearch "style" nw))
      (princ (strcat nw ": Style not found. "))
      (entmod (subst (cons fld nw)
                     (assoc fld (setq ent (entget temp)))
                     ent
              )
      )
    )
    (entmod (subst (cons fld nw)
                   (assoc fld (setq ent (entget temp)))
                   ent
            )
    )
  )
)

;;; Set the prompt string
(defun cht_SetPrompt ()
  (if (= typ "Style")
    (progn
      (initget "Individual List New Select ")
      (setq nw (getkword (strcat "\nIndividual/List/Select style/<"
                                 prmpt
                                 " for all text objects" ">: ")))
      (if (or (= nw "") (= nw nil) (= nw "Enter"))
        (setq nw (getstring (strcat prmpt
                                    " for all text objects" ": ")))
      )
    )
    (progn
      (initget "List Individual" 1)
      (setq nw (getreal (strcat "\nIndividual/List/<"
                                 prmpt
                                 " for all text objects" ">: ")))
    )
  )
)

;;; Process List request
(defun cht_ProcessList ()
  (setq unctr (1- unctr))
  (setq sslen (sslength sset))
  (setq tw 0)
  (while (> sslen 0)
    (setq temp (ssname sset (setq sslen (1- sslen))))
    (if (= typ "Style")
      (progn
        (if (= tw 0)
          (setq tw (list (cdr (assoc fld (entget temp)))))
          (progn
            (setq sty (cdr (assoc fld (entget temp))))
            (if (not (member sty tw))
              (setq tw (append tw (list sty)))
            )
          )
        )
      )
      (progn
        (setq tw (+ tw (setq w (cdr (assoc fld (entget temp))))))
        (if (= (sslength sset) (1+ sslen)) (setq lw w hw w))
        (if (< hw w) (setq hw w))
        (if (> lw w) (setq lw w))
      )
    )
  )
  (if (= typ "Rotation")
    (setq tw (* (/ tw pi) 180.0)
          lw (* (/ lw pi) 180.0)
          hw (* (/ hw pi) 180.0))
  )
  (if (= typ "Style")
    (progn
      (princ (strcat "\n" typ "(s) -- "))
      (princ tw)
    )
    (princ (strcat "\n" typ
             " -- Min: " (rtos lw 2)
             "\t Max: " (rtos hw 2)
             "\t Avg: " (rtos (/ tw (sslength sset)) 2) ) )
  )
)

;;; Process Individual request
(defun cht_ProcessIndividual ()
  (setq sslen (sslength sset))
  (while (> sslen 0)
    (setq temp (ssname sset (setq sslen (1- sslen))))
    (setq ow (cdr (assoc fld (entget temp))))
    (if (= typ "Rotation")
      (setq ow (/ (* ow 180.0) pi))
    )
    (initget 0)
    (redraw (cdr (assoc -1 (entget temp))) 3)
    (if (= typ "Style")
      (progn
        (setq nw (getstring (strcat "\n" prmpt " <" ow ">: ")))
      )
      (progn
        (setq nw (getreal (strcat "\n" prmpt " <" (rtos ow 2) ">: ")))
      )
    )
    (if (or (= nw "") (= nw nil))
      (setq nw ow)
    )
    (if (= typ "Rotation")
      (setq nw (* (/ nw 180.0) pi))
    )
    (entmod (subst (cons fld nw)
                   (assoc fld (setq ent (entget temp)))
                   ent
            )
    )
    (redraw (cdr (assoc -1 (entget temp))) 1)
  )
)

;;; Process the Select option
(defun cht_ProcessSelect ()
  (princ "\nSearch for which Style name?  <*>: ")
  (setq sn  (xstrcase (getstring))
        n   -1
        nsset (ssadd)
        ssl (1- (sslength sset))
        )
  (if (or (= sn "*") (null sn) (= sn ""))
    (setq nsset sset sn "*")
    (while (and sn (< n ssl))
      (setq temp (ssname sset (setq n (1+ n))))
      (if (= (cdr (assoc 7 (entget temp))) sn)
        (ssadd temp nsset)
      )
    )
  )

  (princ (strcat "\nStyle: " sn))
  (print (setq ssl (sslength nsset)))
  (princ "objects found.")
)

;;; Check to see if AI_UTILS is loaded, If not, try to find it,
;;; and then try to load it.  If it can't be found or can't be
;;; loaded, then abort the loading of this file immediately.
(cond
  ((and ai_dcl (listp ai_dcl)))		; it's already loaded.
  ((not (findfile "ai_utils.lsp"))		; find it
    (ai_abort "CHT" nil)
  )
  ((eq "failed" (load "ai_utils" "failed"))	; load it
    (ai_abort "CHT" nil)
  )
)

;;; If we get this far, then AI_UTILS.LSP is loaded and it can
;;; be assumed that all functions defined therein are available.

;;; Next, check to see if ACADAPP.EXP has been xloaded, and abort
;;; if the file can't be found or xloaded.  Note that AI_ACADAPP
;;; does not abort the running application itself (so that it can
;;; also be called from within the command without also stopping
;;; an AutoCAD command currently in progress).
(if (not (ai_acadapp)) (ai_abort "CHT" nil))

;;; The C: function definition
(defun c:cht () (cht_Main))
(princ "\n\tCHT command loaded.")
(princ)
