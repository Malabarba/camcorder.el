;;; screencast.el --- Record screencasts in gif or other formats.  -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/Bruce-Connor/screencast.el
;; Keywords: multimedia
;; Version: 0.1
;; Package-Requires: ((names "20141119"))
;; Keywords: lisp tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'names)

;;;###autoload
(define-namespace screencast-
:package screencast
:version "0.1"
:group emacs


;;; Variables
(defcustom frame-parameters
  '((name . "screencast.el Recording - F12 to Stop - F11 to Pause/Resume")
    (height . 30)
    (width . 80)
    (top .  80))
  "Parameters used on the recording frame.
See `make-frame'."
  :type '(alist :key-type symbol :value-type sexp))

(defcustom recording-command
  '("recordmydesktop"
    " --fps 10" " --no-sound"
    " --windowid " window-id
    " -o " temp-file
    " && mplayer -ao null " temp-file
    " -vo png:z=1:outdir=" temp-dir
    " && convert " temp-dir "* " file
    "; rm -r " temp-file " " temp-dir)
  "Command used to start the recording.
This is a list where all elements are concated together (with no
separators) and passed to `shell-command'. Each element must be a
string or a symbol. The first string should be just the name of a
command (no args), so that we can SIGTERM it.

To increase compression at the cost of slower conversion, change
\"z=1\" to \"z=9\" (or something in between). Other options you
may want to configure are \"--fps 10\" and \"--no-sound\".

Meaning of symbols:
   'file is the output file.
   'window-id is the window-id parameter of the recording frame.
   'temp-file and 'temp-dir are self-explanatory."
  :type '(repeat
          (choice
           string
           (const :tag "window-id parameter of the recording frame" window-id)
           (const :tag "Output file" file)
           (const :tag "Temporary intermediate file" temp-file)
           (const :tag "Temporary intermediate dir" temp-file))))

(defcustom window-id-offset -4
  "Difference between Emacs' and X's window-id."
  :type 'integer)

(defcustom output-directory (expand-file-name "~/Videos")
  "Directory where screencasts are saved."
  :type 'directory)

(defvar temp-dir
  (if (fboundp 'temp-directory)
      (temp-directory)
    (if (boundp 'temporary-file-directory)
        temporary-file-directory
      "/tmp/"))
  "Directory to store intermediate conversion files.")

(defvar recording-frame nil
  "Frame created for recording.
Used by `screencast-start-recording-recording' to decide on the dimensions.")

(defvar -process nil "Recording process PID.")

(defvar -output-file-name nil
  "Temporarily bound to the filename chosen by the user.")


;;; User Functions
(defun stop () "Stop recording." (interactive) (mode -1))

:autoload
(defun record ()
  "Open a new Emacs frame and start recording.
You can customize the size and properties of this frame with
`screencast-frame-parameters'."
  (interactive)
  (select-frame
   (if (frame-live-p recording-frame)
       recording-frame
     (setq recording-frame
           (make-frame frame-parameters))))
  (mode))

:autoload
(defalias 'screencast-start #'record)

:autoload
(define-minor-mode mode nil nil "sc"
  '(([f12] . screencast-stop)
    ([f11] . screencast-pause))
  :global t
  (if mode
      (progn
        (unless recording-frame
          (setq recording-frame (selected-frame)))
        (let ((-output-file-name
               (expand-file-name
                (read-file-name
                 "Output file (out.gif): "
                 (file-name-as-directory output-directory)
                 "out.gif")
                output-directory)))
          (-start-recording))
        (add-hook 'delete-frame-functions
          #'-stop-recording-if-frame-deleted))
    (remove-hook 'delete-frame-functions
      #'-stop-recording-if-frame-deleted)
    (when (-is-running-p)
      (signal-process -process 'SIGTERM))
    (setq -process nil)
    (when (frame-live-p recording-frame)
      (delete-frame recording-frame))
    (setq recording-frame nil)
    (pop-to-buffer "*screencast output*")))

(defun -is-running-p ()
  "Non-nil if the recording process is running."
  (and (integerp -process)
       (memq -process (list-system-processes))))

(defun pause ()
  "Pause or resume recording."
  (interactive)
  (when (-is-running-p)
    (signal-process -process 'SIGUSR1)))


;;; Internal
(defun -stop-recording-if-frame-deleted (frame)
  "Stop recording if FRAME matches `screencast-recording-frame'.
Meant for use in `delete-frame-functions'."
  (when (equal frame screencast-recording-frame)
    (stop)))

(defun -start-recording ()
  "Start recording process.
Used internally. You should call `screencast-record' or
`screencast-mode' instead."
  (message "Recording started.")
  (if (-is-running-p)
      (error "Recording process already running %s" -process)
    (setq -process nil)
    (let ((display-buffer-overriding-action
           (list (lambda (x y) t))))
      (funcall #'shell-command
        (format "(%s) &"
                (mapconcat #'identity
                           (mapcar
                            #'-convert-args
                            recording-command)
                           ""))
        "*screencast output*"))
    (while (null -process)
      (sleep-for 0.1)
      (let* ((name (car recording-command))
             (process
              (car
               (cl-member-if
                (lambda (x) (string= name (cdr (assoc 'comm (process-attributes x)))))
                (list-system-processes)))))
        (setq -process process)))))

(defun -convert-args (arg)
  "Convert recorder argument ARG into values.
Used on `screencast-recording-command'."
  (cond
   ((stringp arg) arg)
   ((eq arg 'file) -output-file-name)
   ((eq arg 'window-id)
    (-frame-window-id recording-frame))
   ((eq arg 'temp-dir)
    (expand-file-name "screencast/" temp-dir))
   ((eq arg 'temp-file)
    (expand-file-name "screencast.ogv" temp-dir))
   (t (error "Don't know this argument: %s" arg))))

(defun -frame-window-id (frame)
  "Return FRAME's window-id in hex.
Increments the actual value by `window-id-offset'."
  (format
   "0x%x"
   (+ (string-to-number
       (frame-parameter frame 'window-id))
      window-id-offset)))

)

(provide 'screencast)
;;; screencast.el ends here
