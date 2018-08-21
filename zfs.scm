(define-module (common zfs)
  #:export (install-deps-zfs)
  #:use-module ((common utils) #:prefix utils:)
  #:use-module ((ice-9 rdelim) #:prefix rdelim:)
  #:use-module ((ice-9 regex) #:prefix regex:))

(define (read-debian-version)
  (let* ((rdr (open-input-file "/etc/debian_version"))
	 (res (rdelim:read-string rdr))
	 (res (regex:string-match "^[0-9]+" res))
	 (res (regex:match:substring res))
	 (res (string->number res)))
    (close rdr)
    res))

(define (install-deps-zfs)
  (when (not (file-exists? ".deps_zfs"))
    (cond
     ((file-exists? "/etc/debian_version")
      (let ((release (read-debian-version)))
	(case release
	  ((8)
	   (with-output-to-file "/etc/apt/sources.list.d/backports.list"
	     (lambda ()
	       (call-with-input-file "/etc/apt/sources.list"
		 (lambda (port)
		   (let* ((result (rdelim:read-string port))
			  (pattern (make-regexp "^deb (.*) jessie main$" regexp/newline))
			  (result (regex:match:substring (regexp-exec pattern result) 1)))
		     (utils:println "deb" result "stretch" "main" "contrib"))))))
	   (system* "apt" "update")
	   (system* "apt" "install" "-y" "-t" "jessie-backports" "zfs-dkms"))
	  ((9)
	   (system* "sed" "-i" "-re" "s;^deb (.+) stretch main$;deb \\1 stretch main contrib;"
		    "/etc/apt/sources.list.d/base.list")
	   (system* "apt" "update")
	   (system* "apt" "install" "-y" "zfs-dkms"))
	  (else
	   (error "Debian version is not supported" release)))))
     (else
      (error "Necessary binaries are missing, and unable to install them! Please make sure ZFS kernel modules are loaded and CLI commands are available (i.e. zpool and zfs)!")))
    (when (not (zero? (system* "modprobe" "zfs")))
      (error "ZFS kernel modules are missing!"))
    (utils:println "ZFS kernel modules are loaded!")
    (with-output-to-file ".deps_zfs"
      (lambda () (display "")))))
