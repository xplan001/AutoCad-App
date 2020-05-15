
;; RepathXrefs by Joe Burke.

;; MISCELLANEOUS NOTES:

  ;; Bug reports may be sent to me directly at lowercase@hawaii.rr.com.
  ;; Program updates will be posted at www.theswamp.org under "Show your stuff"
  ;; in a topic named "Repath xrefs full or relative".

  ;; The standard disclaimer applies. Use at your own risk...
  ;; Please be aware this program is potentially dangerous.

  ;; Requires DOSLib15.arx or later from Robert McNeel & Associates 
  ;; available for free at www.mcneel.com. Thanks to Dale Fugier.

  ;; Thanks to Steve Doman for suggestions and beta testing. Also
  ;; to Jason Piercey for his input.

  ;; Tested with 2002, 2004, 2005, 2006, 2007 and 2008.
  
  ;; The shortcut is RPX.

;; PROGRAM NOTES:

  ;; The general idea is to allow repathing of xrefs by searching for
  ;; them within a selected folder. The term "Project" folder refers to 
  ;; to that folder. It specifies where to search for xrefs. All files 
  ;; within all sub-folders are included. Typically the Project folder
  ;; is one which contains all files and folders related to a particular 
  ;; project.
  
  ;; The program is similar to the standalone application Reference Manager 
  ;; in terms of what it's designed to do. Hopefully users will find this 
  ;; interface easier to understand.
  
  ;; Following is a brief example of what search allows. If an xref
  ;; is moved from one folder to another within a Project folder, any
  ;; file which references it will not be able to find it regardless of
  ;; whether the path is full, relative or none. The program can repath
  ;; such files to the new location with an option to make the new path 
  ;; full or relative.
  
  ;; Another application is a file or files which contain a mix of 
  ;; relative and full paths. Change all to one or the other.

  ;; Nested xrefs are not repathed because those paths are determined by 
  ;; the parent xref. In that case run the program on the parent file and
  ;; save it. Then reload the parent file in the active file.

  ;; The relative path option requires the Project folder and all files
  ;; processed to be on the same drive. This is a standard AutoCAD
  ;; limitation which is explained in help. If the relative path drive
  ;; condition is not met the program ends with an alert message.  
  ;; This limitation does not apply to the full path option.

  ;; If an xref is in the same folder as the file referencing it and the
  ;; relative path option is chosen then any path is removed. The path will
  ;; be the xref file name like the no path option when attaching an xref.
  ;; Given a Project folder which contains all files at the Project folder
  ;; level, the program will remove all xref paths.

;; PROGRAM OPTIONS:

  ;; First option presented at the command line:
    ;; Path type [Full/Relative] <R>:
    ;; New paths are either Full or Relative.

  ;; Second set of options at the command line when using 2005 or later.
    ;; These options are not presented when using 2004 or earlier. 
    ;; Only xrefs in the active file may be repathed using those versions.
    ;; Repath current drawing, all opened drawings, or browse? [Browse/Current/Opened]
    ;; If Current is chosen the xrefs in the active file are repathed.
    ;; If Opened is chosen all open files are processed.
    ;; If Browse is chosen a select files window is presented and the
    ;; selected files are processed.

  ;; Third prompt at the command line:
    ;; Select Project folder or reuse last Project folder selected if
    ;; the program was run at least once before with the active file.
    ;; If not the select Project folder window is presented.

  ;; Fourth prompt on screen:
    ;; A confirm message box is presented before any files are repathed.
    ;; Choose OK to proceed or Cancel to abort.
    
  ;; Other messages/options which may appear at the command line:

    ;; If multiple copies of an xref are found within the Project folder, 
    ;; the program will list in the text window where they were found
    ;; and the file date. The list is sorted by latest file date first.
    ;; Then a question is presented: 
    ;; Repath using the latest date file? [Yes/No] <Y>:
    ;; If Yes is chosen the xref file with the latest date is used.
    ;; If No is chosen the xref is not repathed in any file which 
    ;; references it.
    
    ;; The Browse option allows selection of files which may be saved in
    ;; an earlier file format than the version in use. In this case the 
    ;; files would be saved in the current file format, assuming the file
    ;; is not open. Since this may produce undesireable results, such files
    ;; are listed in the text window and a question is presented at the 
    ;; command line: Allow files to change format? [Yes/No] <Y>:
    ;; if No is chosen the files will not be modified/repathed.
    ;; If Yes is chosen the files will be saved in the current version
    ;; file format.
    
    ;; The main reason for the Opened files option is to provide a method
    ;; of operating on multiple files while avoiding the change file
    ;; format issue.
   
;; REPORTS/EVENTS AT PROGRAM END:

  ;; Xref files which were not found within Project folder are reported, 
  ;; assuming they are not nested. The total number of repathed xrefs is 
  ;; displayed. If the Current option is chosen the program reports the  
  ;; number of nested xrefs found. Otherwise it does not.

  ;; Repathed xrefs in the active file will dynamically update. Xrefs in
  ;; open but not active files will not. Either save, close and reopen the 
  ;; file or reload the xrefs. Note, open files are not saved by the program.
  
  ;; With the Browse option any file which is either in a newer file format
  ;; than the version in use or is marked as read-only by the OS is listed.
  ;; Such files are not repathed/modified.

;; VERSION HISTORY:

  ;; RepathXrefs 1.0.lsp - 3/7/2008
  ;; First release version.

  ;; RepathXrefs 1.0a.lsp - 3/10/2008
  ;; Minor bug fix for read-only files under 2007 and 2008 at sub-function
  ;; RPX:DocAtPath.

(vl-load-com)

(defun c:RepathXrefs ( / *error* fullrel actdoc *acad* files path fullpath  
                         fn docslst filelst temppath reuse temp pathlst 
                         cnt nestcnt datalst masterlst oklst notfoundlst
                         multiplelst blkname blkobj expath time multans 
                         returnlst msg version formatlst targdoc newerdoclst
                         documents processpathlst prmpt init rpwhat formatans
                         lenlst curformat paths 
                         RPX:DocAtPath RPX:Repath RPX:GetDate 
                         RPX:CurrentFileFormat RPX:CheckDOSLib RPX:XrefsData)
                         ;; globals: *projpath* *OKDOSLib*

  (defun *error* (msg)
    (cond
      ((not msg))
      ((wcmatch (strcase msg) "*QUIT*,*CANCEL*"))
      (T (princ (strcat "\nError: " msg)))
    )
    (if 
      (and 
        targdoc
        (not (vlax-object-released-p targdoc))
      )
      (vlax-release-object targdoc)
    )
    (setvar "cmdecho" 1)    
    (vla-EndUndoMark actdoc)
    (if (eq (type dos_waitcursor) 'EXRXSUBR)
      (dos_waitcursor)
    )

    (princ)
  ) ;end error

  ;;;; START SUB-FUNCTIONS ;;;;

  ;; Argument: document vla-object.
  ;; Returns: list of lists: (xreffilename blockname existingpath blockobj)
  (defun RPX:XrefsData (doc / blocks blkname fn xreflst expath 
                              NestedXref GetPath)

    ;; Argument: xref block definition vla-object.
    ;; Returns: the path property via DXF codes.
    ;; Used when the path property is not available in 2004 and earlier.
    (defun GetPath (blkdef / elst)
      (setq elst (entget (vlax-vla-object->ename blkdef)))
      (cdr (assoc 1 (entget (cdr (assoc 360 elst)))))
    )

    ;; Argument: block definition vla-object.
    ;; Returns a count number if the xref is nested, otherwise nil.
    ;; Based on code by Stephan Koster in a program named XrefTree.
    ;; Function renamed from nested_p.
    ;; The nestcnt variable is local to the primary routine.
    ;; There is a known flaw in the function which Jason pointed out.
    ;; If an xref is both nested and referenced as a parent, the
    ;; function does not flag it as nested. The fallout from that situation,
    ;; if it occurs, should not be a serious problem.
    (defun NestedXref (blkdef / elst)
      (setq elst (entget (vlax-vla-object->ename blkdef)))
      (if
        (or
          (not (vl-position '(102 . "{BLKREFS") elst))
          (and
            (vl-position '(102 . "{BLKREFS") elst)
            (not (cdr (assoc 331 elst)))
          )
        )
        (if nestcnt (setq nestcnt (1+ nestcnt)))
        ;; Else return nil to the parent function.
      )
    ) ;end

    (setq blocks (vla-get-blocks doc))

    (vlax-for x blocks
      (if 
        (and
          (= -1 (vlax-get x 'IsXref))
          ;; Filter out nested xrefs.
          (not (NestedXref x))
          (setq blkname (vlax-get x 'Name))
          ;; Existing path
          (if (vlax-property-available-p x 'Path)
            (setq expath (vlax-get x 'Path))
            (setq expath (GetPath x))
          )
          (setq fn (strcat (vl-filename-base expath) ".dwg"))
        )
        (setq xreflst (cons (list fn blkname expath x) xreflst))
      )
    )
    xreflst
  ) ;end RPX:XrefsData

  ;; Argument: full path.
  ;; Returns: date string - 2008/02/08 23:34:18
  ;; Use this because vl-file-systime returns nil if a file is open.
  (defun RPX:GetDate (path / date year mo day hr mn sec)
    (setq date (cdr (car (dos_filedate path)))
          date (rtos date 2 6)
          year (substr date 1 4)
          mo (substr date 5 2)
          day (substr date 7 2)
          hr (substr date 10 2)
          mn (substr date 12 2)
          sec (substr date 14 2)
    )
    (strcat year "/" mo "/" day " " hr ":" mn ":" sec)
  )

  ;; Used with dos_dwgver to determine if an ODBX doc
  ;; will change versions when saved. Update with newer
  ;; versions will be needed.
  (defun RPX:CurrentFileFormat ( / v)
    (setq v (atoi (getvar "acadver")))
    (cond
      ((= 15 v) 2000)
      ((= 16 v) 2004)
      ((= 17 v) 2007)
      ((= 18 v) 2010)
      (T 2013)
    )
  )

  ;; Added 2/20/2008. 
  ;; *OKDOSLib* is global so this check need not happen each
  ;; time one of the routines is called. Might use (arx) here.
  ;; I think "doslib15.arx" contains all these functions.
  (defun RPX:CheckDOSLib ()
    (if
      (and
        (eq (type dos_waitcursor) 'EXRXSUBR)
        (eq (type dos_relativepath) 'EXRXSUBR)
        (eq (type dos_getdir) 'EXRXSUBR)
        (eq (type dos_ispathsameroot) 'EXRXSUBR)
        (eq (type dos_find) 'EXRXSUBR)
        (eq (type dos_getfilem) 'EXRXSUBR)
        (eq (type dos_msgbox) 'EXRXSUBR)
        (eq (type dos_dwgver) 'EXRXSUBR)
        (eq (type dos_filedate) 'EXRXSUBR)
        ;; Used to check if file is read only
        ;; in the RPX:DocAtPath function.
        (eq (type dos_file) 'EXRXSUBR)
      )
      (setq *OKDOSLib* T)
      (progn
        (princ "\nDOSLib from Robert McNeel & Associates is required. ")
        (princ "\nIt's available for free at www.mcneel.com. ")
        (princ "\nExiting... ")
        (exit)
      )
    )
  ) ;end

  ;; Argument: full path.
  ;; Returns a document object. An ODBX doc if the file isn't open.
  ;; Otherwise a doc contained in the active documents collection.
  ;; Note, an ODBX doc does not have a ReadOnly property. So use
  ;; dos_file instead to check for read-only.
  (defun RPX:DocAtPath (path / version file srcdoc)
    ;check the documents collection
    (vlax-for x (vla-get-documents *acad*)
      (if 
        (and
          (eq (strcase path) (strcase (vlax-get x 'FullName)))
          ;; Check for file marked read-only by the OS.
          ;; Though files in the documents collection are not saved,
          ;; there's no point repathing a document which can't be saved 
          ;; by the user. List as read-only at the end of report.
          (or 
            (and 
              (>= (atoi (getvar "acadver")) 17)
              (/= 1 (logand 1 (last (dos_file path))))
            )
            ;; For DOSLib prior to version 7.0.
            (and
              (< (atoi (getvar "acadver")) 17)
              (not (eq "R" (nth 4 (dos_file path))))
            )
          )
        )
        (setq srcdoc x)
      )
    )
    ;Otherwise use ObjectDBX.
    (if 
      (and 
        (not srcdoc)
        ;; Check for file marked read-only by the OS.
        ;; An attempt to SaveAs such a file causes an error.
        (or 
          (and 
            (>= (atoi (getvar "acadver")) 17)
            (/= 1 (logand 1 (last (dos_file path))))
          )
          (and
            (< (atoi (getvar "acadver")) 17)
            (not (eq "R" (nth 4 (dos_file path))))
          )
        )
      )
      (cond
        ; 2004 or later. Allow for future versions like 
        ;"ObjectDBX.AxDbDocument.17" by Tony Tanzillo
        ((> (setq version (atoi (getvar "AcadVer"))) 15)
          (setq srcdoc 
            (vla-GetInterfaceObject *acad* 
              (strcat "ObjectDBX.AxDbDocument." (itoa version))))
          ;; Catch the error if file format is later than the version 
          ;; in use. Return nil.  
          (if (vl-catch-all-error-p
            (vl-catch-all-apply
              '(lambda ()
                (vla-open srcdoc path)))
            )
            (setq srcdoc nil)
          )
        )
        ;prior to 2004
        (T
          (if
            (and
              (vl-catch-all-error-p
                (vl-catch-all-apply 
                  'vla-GetInterfaceObject
                    (list *acad* "ObjectDBX.AxDbDocument")))
              (setq file (findfile "AxDb15.dll"))
            )
            (startapp "regsvr32.exe" (strcat "/s \"" file "\""))
          )
          (setq srcdoc (vla-GetInterfaceObject *acad* "ObjectDBX.AxDbDocument"))
          (if (vl-catch-all-error-p
            (vl-catch-all-apply
              '(lambda ()
                (vla-open srcdoc path)))
            )
            (setq srcdoc nil)
          )
        )
      )
    )
    srcdoc
  ) ;end

  ;; Arguments: document and a list of lists.
  ;; Like this if there's multiple copies of an xref within project folder
  ;; ((xreffilename blockname existingpath blkobj) path [path]). 
  ;; Or one path if there's not.
  (defun RPX:Repath (doc lst / xname expath blk fullpath docname docpath 
                               name newpath reportlst)

    (foreach x lst
      ;; Xref block name
      (setq xname (cadar x))
      ;; Existing path
      (setq expath (caddar x))
      ;; xref block vla-object
      (setq blk (last (car x)))
      ;; If duplicate xrefs found in project folder then this is the 
      ;; first path among multiple paths which are sorted by date, latest first.
      ;; If not it's the only path found.
      (setq fullpath (cadr x))
      (if (vl-position doc docslst)
        (setq docname (vlax-get doc 'Name) 
              docpath (vlax-get doc 'Path)
        )
        (setq name (vlax-get doc 'Name)
              docname (strcat (vl-filename-base name) ".dwg")
              docpath (vl-filename-directory name)
        )
      )

      (if (eq "Relative" fullrel)
        (progn
          (setq newpath
            (dos_relativepath
              ;; Path to the document passed.
              docpath             
              ;; Full path to xref.
              fullpath
            )
          )          
          ;; Because dos_relativepath returns a path like this 
          ;; ".\\Plan Bldg B 2 lv KLSC.dwg" when an xref is in the
          ;; same folder as the file referencing it. 
          ;; Remove the leading relative path.
          (if (not (vl-string-search "\\" (substr newpath 3)))
            (setq newpath (substr newpath 3))
          )
        )  
        ;; Else fullrel is "Full" path.
        (setq newpath fullpath)
      )

      ;; Check the path found is not the same as the original path.
      ;; Repath if not.
      (if (not (eq (strcase expath) (strcase newpath)))
        (progn
          ;; If doc is the active doc use command xref to update xrefs.
          ;; Otherwise use ActiveX to repath.
          (if (equal doc actdoc)
            (command "._xref" "path" xname newpath)
            (vlax-put blk 'Path newpath)
          )
          
          (setq cnt (1+ cnt))
          (setq reportlst (cons (list xname newpath) reportlst))
        )
      )
      
      ;; Double check each xref which should have been repathed actually was.
      ;; If not try again. In some cases where an xref is both referenced as a
      ;; parent and also nested, this will allow repathing the xref.
      ;; Without it an xref may be reported as repathed when it was not.
      (if (equal doc actdoc)
        (foreach x reportlst
          (if (not (eq (cadr x) (cdr (assoc 1 (tblsearch "block" xname)))))
            (command "._xref" "path" (car x) (cadr x))
          )
        )
      )
    
    ) ;foreach

    (if reportlst (list docname reportlst))

  ) ;end RPX:Repath
  ;; ----------------------

  ;;;; END SUB-FUNCTIONS ;;;;

  ;;;; START MAIN FUNCTION ;;;;

  (if (not *OKDOSLib*) 
    (RPX:CheckDOSLib)
  )
  
  (setq *acad* (vlax-get-acad-object)
        actdoc (vla-get-ActiveDocument *acad*)
        documents (vla-get-Documents *acad*)
  )
  
  (vla-StartUndoMark actdoc)

  (setq cnt 0)
  (setvar "cmdecho" 0)

  (initget "Full Relative")
  (setq fullrel (getkword "\nPath type [Full/Relative] <R>: "))
  (if (not fullrel) (setq fullrel "Relative"))

  ;; If 2004 or earlier only the Current option is allowed because
  ;; an xref block does not have a path property in those versions.
  ;; ACAD 2005 "16.1s (LMS Tech)"
  (if (< (atof (getvar "acadver")) 16.1)
    (setq rpwhat "Current")
    (progn
      (if (< 1 (vlax-get documents 'Count))
        (setq prmpt (strcat "\nRepath current drawing, all opened drawings, "
                            "or browse? [Browse/Current/Opened] <Browse>: ")
              init "Browse Current Opened"
        )
        (setq prmpt "\nRepath current drawing or browse? [Browse/Current] <Browse>: "
              init "Browse Current"
        )
      )
      (initget init)
      (setq rpwhat (getkword prmpt))
      (if (not rpwhat) (setq rpwhat "Browse"))
    )
  )

  ;; Option to reuse of the last project folder selected.
  (if *projpath*
    (progn
      (setq temppath *projpath*)
      (princ (strcat "\nCurrent folder: " *projpath*))
      (initget "Yes No")
      (setq reuse (getkword "\n  Use current Project folder? [Yes/No] <Y>: "))
      (if (eq reuse "No")
        (setq *projpath* (dos_getdir "Select Project Folder" (getvar "dwgprefix")))
      )
      (if 
        (and
          (eq reuse "No")
          (not *projpath*)
        )
        (progn
          (setq *projpath* temppath)
          (exit)
        )
      )
    )
    ;else
    (if
      (not
        (setq *projpath* (dos_getdir "Select Project Folder" (getvar "dwgprefix")))
      )
      (exit)
    )
  )

  ;; List of open files.
  (vlax-for x documents
    (setq docslst (cons x docslst))
  )

  ;; Browse/Current/Opened
  ;; processpathlst is a list of fully qualified paths.
  ;; Each is the path to one or more files to be processed.
  (cond
    ;; Process selected files.
    ((eq "Browse" rpwhat)
      (if 
        (not (setq files 
               (dos_getfilem "Select files" *projpath*
               "Drawing files (*.dwg)|*.dwg||")))
        (exit)
        (progn
          ;; dos_getfilem returns a qualified path first and then
          ;; the names of the drawings selected. A single list of strings.
          (setq path (car files))
          (foreach fn (cdr files)
            (setq processpathlst (cons (strcat path fn) processpathlst))
          )
        )
      )
    )
    ;; Process the documents collection.
    ((eq "Opened" rpwhat)
      (vlax-for x documents
        (setq processpathlst (cons (vlax-get x 'FullName) processpathlst))
      )
      (setq processpathlst (reverse processpathlst))
    )
    ;; Process the active file.
    ((eq "Current" rpwhat)
      (setq processpathlst (list (vlax-get actdoc 'FullName)))
    )
  ) ;cond
  
  (if 
    (and 
      (eq "Relative" fullrel)
      (or
        (eq "Opened" rpwhat)
        (eq "Current" rpwhat)
      )
      (not 
        (vl-every 
          '(lambda (x) (dos_ispathsameroot *projpath* x)) processpathlst
        )
      )
    )
    (progn
      (alert 
        (strcat "The project folder and all open files must be\n"
                "on the same drive when using relative paths.\n"
                "Please try again. Exiting..."
        )
      )
      (exit)
    )
  )


  (if 
    (and 
      (eq "Relative" fullrel)
      (eq "Browse" rpwhat)
      (not (dos_ispathsameroot *projpath* (car files)))
    )
    (progn
      (alert 
        (strcat "The project folder and the selected files must\n"
                "be on the same drive when using relative paths.\n"
                "Please try again. Exiting..."
        )
      )
      (exit)
    )
  )

  ;; List of full paths.
  ;; All .dwg files found within project folder sorted by date with newest first.
  (setq filelst (dos_find (strcat *projpath* "*.dwg") 4))

  ;; ------------------------
  ;; ------------------------
  ;; The following code scans the file path list to see if any file
  ;; references a file which is duplicated within the Project folder.
  ;; Also check the file format of each file to see if the format
  ;; will change if saved using ODBX. Also check if any file is in
  ;; a newer file format than the version in use.

  ;; Get the current file format.
  (setq curformat (RPX:CurrentFileFormat))

  (foreach x processpathlst
    (if (setq targdoc (RPX:DocAtPath x))
      ;; List of lists: (xreffilename blockname existingpath blockobj)
      (setq datalst (RPX:XrefsData targdoc))
      ;; if RPX:DocAtPath returns nil the file format is later than
      ;; the version in use. This alos catches files which are
      ;; marked read-only by the OS. Report at end.
      (setq newerdoclst (cons x newerdoclst))
    )

    ;; Make a list of full paths which would change file format if saved.
    ;; Note, dos_dwgver returns "Unknown" if the file format is later
    ;; than the version in use.
    (if (not (vl-position targdoc docslst))
      (progn
        (setq version (dos_dwgver x))
        (if (not (eq "Unknown" version))
          (progn
            (setq version (atoi (substr version 9)))
            (if 
              (and 
                (< version curformat)
                (not (vl-position x formatlst))
              )
              (setq formatlst (cons x formatlst))
            )
          )
        )
      )
    )

    (if 
      (and 
        targdoc
        (not (vlax-object-released-p targdoc))
      )
      (vlax-release-object targdoc)
    )

    ;; Datalst is a list of lists.
    ;; (xreffilename blockname existingpath blockobj)
    (foreach x datalst
      (setq temp nil
            fn (car x)
            temp (cons fn temp)
      )
      (foreach str filelst
        (if (eq (strcase fn) (strcase (strcat (vl-filename-base str) ".dwg")))
          (setq temp (cons str temp))
        )
      )
      (setq temp (reverse temp))
      (if (not (vl-position temp masterlst))
        (setq masterlst (cons temp masterlst))
      )
    )
  ) ;foreach path in processpathlst
  
  ;; Each item in masterlst is (xreffilename path [path]). It's a list of 
  ;; lists containing all non-nested xrefs in all selected files.

  ;; List xrefs which have multiple paths.
  (foreach x masterlst
    (if (< 1 (length (cdr x)))
      (setq multiplelst (cons x multiplelst))
    )
  )

  ;; Each list in multiplelst is like this (xreffilename path [path]).
  ;; This part prints to the command line any xrefs involved which are
  ;; duplicated in the project folder.
  (if multiplelst
    (progn      
      ;; Temp turn off the wait cursor.
      (dos_waitcursor)
      (textscr)

      (princ "\nDuplicate xrefs found sorted by latest date first. ")

      (foreach x multiplelst
        (princ "\n\n  -------------- ")
        (princ (strcat "\n  " (car x)))
        (foreach p (cdr x)
          (princ (strcat "\n\n" p "\n"))
          ; 2008/02/08 23:34:18
          (princ (strcat "  " (RPX:GetDate p)))
        )
        (princ "\n  -------------- ")
      )

      ;; Ask the question.
      (initget "Yes No")
      (setq multans (getkword "\n\nRepath using the latest date file? [Yes/No] <Y>: "))
      (if (not multans) (setq multans "Yes"))

      ;; Turn the wait cursor back on.
      (dos_waitcursor T)
    )
  )

  (if formatlst
    (progn      
      ;; Temp turn off the wait cursor.
      (dos_waitcursor)
      (textscr)

      (princ "\nSome selected files will change file format. \n")
      
      (foreach x formatlst
        (princ (strcat "\n  " x))
      )
      
      (princ "\n")

      ;; Ask the question.
      (initget "Yes No")
      (setq formatans (getkword "\nAllow files to change format? [Yes/No] <Y>: "))
      (if (not formatans) (setq formatans "Yes"))

      ;; Turn the wait cursor back on.
      (dos_waitcursor T)
    )
  )

  (if (eq formatans "No")
    (foreach x formatlst
      (setq processpathlst (vl-remove x processpathlst))
    )
  )
  
  ;; End pre-check code.  
  ;; ------------------------
  ;; ------------------------
  
  (setq nestcnt 0)

  ;; Confirm proceed.
  ;; Temp turn off the wait cursor.
  (dos_waitcursor)

  (cond
    ((eq "Current" rpwhat)
      (setq msg
        (dos_msgbox "Proceed with repathing the current file?"
           "Confirm" 2 4
        )
      )
    )
    ((eq "Opened" rpwhat)
      (setq msg
        (dos_msgbox "Proceed with repathing opened files?"
           "Confirm" 2 4
        )
      )
    )
    ((eq "Browse" rpwhat)
      (setq lenlst (length processpathlst))
      (cond
        ((= 1 lenlst)
          (setq msg
            (dos_msgbox 
               (strcat "Proceed with repathing " (itoa lenlst) " file?")
               "Confirm" 2 4
            )
          )
        )
        ((< 1 lenlst)
          (setq msg
            (dos_msgbox 
               (strcat "Proceed with repathing " (itoa lenlst) " files?")
               "Confirm" 2 4
            )
          )
        )
        ((= 0 lenlst)
          (setq msg
            (dos_msgbox 
               "No files found which need repathing."
               "Confirm" 2 4
            )
          )
        )
      ) ; end cond
    )
  ) ; end cond

  ;; With options in-hand, step through the files again and repath.  

  (if (= 4 msg)
    (progn
      (dos_waitcursor T)
      (setq temp nil)

      (foreach x processpathlst
        (setq targdoc nil)
        (if (setq targdoc (RPX:DocAtPath x))
          ;; List of lists: (xreffilename blockname existingpath blockobj)
          (setq datalst (RPX:XrefsData targdoc)
                pathlst nil
          )
        )

        ;; datalst is a list of lists
        ;; (xreffilename blockname existingpath blockobj)
        (foreach x datalst
          (setq temp nil
                fn (car x)
                blkname (cadr x)
                expath (caddr x)
                blkobj (last x)
                temp (cons (list fn blkname expath blkobj) temp)
          )

          ;;;; AVOID scanning the project folder twice. 
          ;; Do speed test compared to beta 1.15.
          (foreach i masterlst
            (if (eq fn (car i))
              (setq paths (cdr i))
            )
          )
 
          ;; Finally seems right.
          (setq pathlst (cons (append temp paths) pathlst))

        )

        (setq oklst nil multiplelst nil)

        ;; Each list in pathlst is like this
        ;; ((xreffilename blockname existingpath blkobj) path [path]).
        ;; Following sorts out which xref files have either a single 
        ;; path (oklst), no path (notfoundlst) or multiple paths (multiplelst).
        (foreach x pathlst
          (cond
            ((= 1 (length (cdr x)))
              (setq oklst (cons x oklst))
            )
            ((= 0 (length (cdr x)))
              (setq temp (caar x))
              (if (not (vl-position temp notfoundlst))
                (setq notfoundlst (cons temp notfoundlst))
              )
            )   
            ((< 1 (length (cdr x)))
              (setq multiplelst (cons x multiplelst))
            )
          )
        )

        (if (eq multans "Yes") (setq oklst (append multiplelst oklst)))

        (if (and targdoc oklst)
          (if (setq temp (RPX:Repath targdoc oklst))
            (setq returnlst (cons temp returnlst))
          )
        )

        ;; Save ODBX documents.
        (if 
          (and
            targdoc
            (not (vl-position targdoc docslst))
          )
          (vl-catch-all-apply 'vla-SaveAs (list targdoc x))
        )

        (if 
          (and 
            targdoc
            (not (vlax-object-released-p targdoc))
          )
          (vlax-release-object targdoc)
        )

      ) ;end foreach file selected

      ;; ----------------------------
      ;;;; Report ;;;;;

      (textscr)

      ;; typical return list
      ; (("A2-102 Bldg C 1 FP KLSC.dwg" (("Plan Bldg B 1 lv KLSC" "..\\Xrefs B\\Plan 
      ; Bldg B 1 lv KLSC.dwg") ("Border 3624 KLSC" "..\\..\\Common AB\\Xrefs AB\\Border 
      ; 3624 KLSC.dwg"))) ("A2-101 Bldg B 1 FP KLSC.dwg" (("Plan Bldg B 1 lv KLSC" 
      ; "..\\Xrefs B\\Plan Bldg B 1 lv KLSC.dwg") ("Border 3624 KLSC" "..\\..\\Common 
      ; AB\;\Xrefs AB\\Border 3624 KLSC.dwg"))))

      (foreach x (reverse returnlst)
        ;; print file name
        (princ (strcat "\n\n" (car x)))
        ;; xref name and new path
        (foreach xr (last x)
          (princ (strcat "\n Xref: " (car xr) " repathed: \n"))
          (princ (strcat "  " (cadr xr)))
        )
      )

      (if notfoundlst
        (progn
          (princ "\n\nXrefs not found in project folder: ")
          (foreach x notfoundlst
            (princ (strcat "\n  " x))
          )
        )
      )

      ;; Number of nested xrefs if any.
      ;; Only reported with the Current option.
      (if (eq "Current" rpwhat)
        (if (not (zerop nestcnt))
          (princ (strcat "\n\nNumber of nested xrefs found: " (itoa nestcnt)))
        )
      )

      (princ (strcat "\n\nTotal number of xrefs repathed: " (itoa cnt) "\n"))
      
      (if newerdoclst
        (progn
          (princ 
            (strcat "\nThe following files were not repathed. "
                    "\n The file format is later than the version in use "
                    "or the file is read-only. \n"
            )
          )
          (foreach x newerdoclst
            (princ (strcat "\n  " x))
          )
        )
      )

    ) ;progn
  ) ;if msg = 4

  (dos_waitcursor)
  
  (*error* nil)
  
) ;end RepathXrefs

;-------------------------------
;shortcut
(defun c:RPX () (c:RepathXrefs))
;-------------------------------

;|
(defun c:LoadedXrefs2 ( / blocks name)
  (setq blocks 
    (vla-get-blocks
      (vla-get-activedocument
        (vlax-get-acad-object))))
  (vlax-for x blocks
    (setq name (vlax-get x 'Name))  
    (if (= -1 (vlax-get x 'IsXref))
      (cond
        ((not 
           (vl-catch-all-error-p
             (vl-catch-all-apply
               'vlax-get 
                 (list x 'XrefDatabase)
             )
           )
         )
         (princ (strcat "\n" name " is loaded. "))
        )
        (T (princ (strcat "\n" name " is not loaded. ")))
      )
    )
  )
  (princ)
)