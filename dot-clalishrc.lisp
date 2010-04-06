;-*-Lisp-*-

(alias-directory-bins "/sbin")
(alias-directory-bins "/bin")
(alias-directory-bins "/usr/bin")
(alias-directory-bins "/usr/kde/3.5/bin")
(alias-directory-bins "/usr/local/bin")

;;; Directories

(defsh-partial lsc sh-term-std :ls :--color)
(defsh-partial lsa lsc "-A")

(defsh lcd (x &rest args)
  (cd x)
  (apply #'lsc args))

(defsh-partial cdb cd "..")
(defsh-partial lcdb lcd "..")

(defsh-sequence mkcd mkdir cd)

;;; Screens

(defsh same-screens ()
  (xrandr
    :--output "LVDS" :--auto
    :--output "VGA"  :--auto :--same-as "LVDS"))

(defsh different-screens ()
  (xrandr
    :--output "LVDS" :--mode :1024x768 :--pos :0x0
    :--output "VGA"  :--mode :1024x768 :--pos :1024x0))

(defsh lvds-only ()
  (xrandr
    :--output "LVDS" :--mode :1024x768 :--pos :0x0
    :--output "VGA"  :--off))

(defsh vga-only ()
  (xrandr
    :--output "LVDS" :--off
    :--output "VGA"  :--mode :1024x768 :--pos :0x0))

;;; Portage

;; Shouldn't ever need to call this directly.
(defsh update-portage ()
  (emerge :--update :--deep :--newuse :world)
  (emerge :--depclean)
  (revdep-rebuild))

(defsh edit-use-flags ()
  (nano "/etc/make.conf")
  (update-portage))

;; Need to work out how to retrieve portage messages for later viewing.
(defsh sync-portage ()
  (emerge :--sync)
  (update-portage))

(defsh unmerge (soft)
  (emerge :--unmerge soft))

;;; Shutdown

(defsh-partial shutdown-now shutdown :-h :now)
(defsh-compose just-shutdown-now just shutdown-now)

