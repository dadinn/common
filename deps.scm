(define-module (common deps)
  #:export (install-deps-base install-deps-lvm install-deps-zfs read-debian-version)
  #:use-module ((common utils) #:prefix utils:)
  #:use-module ((ice-9 rdelim) #:prefix rdelim:)
  #:use-module ((ice-9 regex) #:prefix regex:))

(define (read-debian-version)
  (if (file-exists? "/etc/debian_version")
    (let* ((rdr (open-input-file "/etc/debian_version"))
	   (res (rdelim:read-string rdr))
	   (res (regex:string-match "^[0-9]+" res))
	   (res (regex:match:substring res))
	   (res (string->number res)))
      (close rdr)
      res)))

(define* (install-deps-base #:optional lockfile-path)
  (when (not (and lockfile-path (file-exists? lockfile-path)))
    (let ((missing (utils:which* "sgdisk" "partprobe" "cryptsetup")))
      (if (not (null? missing))
	  (if (read-debian-version)
	      (begin
		(display "Installing necessary packages...")
		(system "apt update")
		(when (not (zero? (system "apt install -y gdisk parted cryptsetup")))
		 (error "Failed to install packages: gdisk, parted, cryptsetup")))
	      (error "Necessary binaries are missing" missing))))
    (when lockfile-path
      (with-output-to-file lockfile-path
	(lambda () (display ""))))))

(define (install-deps-lvm)
  (let ((missing (utils:which* "pvcreate" "vgcreate" "lvcreate")))
    (if (not (null? missing))
	(cond
	 ((file-exists? "/etc/debian_version")
	  (display "Installing necessary packages...")
	  (system "apt update")
	  (when (not (zero? (system "apt install -y lvm2")))
	   (error "Failed to install package lvm2")))
	 (else
	  (error "Necessary binaries are missing" missing))))))

(define* (install-deps-zfs #:optional lockfile-path)
  (when (not (and lockfile-path (file-exists? lockfile-path)))
    (let ((release (or (read-debian-version) 0)))
      (cond
       ((member release (list 8 10))
	(call-with-input-file "/etc/apt/sources.list"
	  (lambda (port)
	    (let* ((pattern (make-regexp "^deb ([^ ]+) ([^ ]+) main$" regexp/newline))
		   (match (rdelim:read-string port))
		   (match (regexp-exec pattern match))
		   (uri (regex:match:substring match 1))
		   (suite (regex:match:substring match 2))
		   (suite (string-append suite "-backports")))
	      (with-output-to-file "/etc/apt/sources.list.d/backports.list"
		(lambda ()
		  (utils:println "deb" uri suite  "main" "contrib")
		  (utils:println "deb-src" uri suite  "main" "contrib")))
	      (system* "apt" "update")
	      (when (not (zero? (system* "apt" "install" "-y" "-t" suite "zfs-dkms")))
		(error "Failed to install package zfs-dkms"))))))
       ((<= 9 release)
	(system* "sed" "-i" "-re" "s;^deb ([^ ]+) ([^ ]+) main$;deb \\1 \\2 main contrib;"
		 "/etc/apt/sources.list.d/base.list")
	(system* "apt" "update")
	(when (not (zero? (system* "apt" "install" "-y" "zfs-dkms")))
	  (error "Failed to install package zfs-dkms")))
       (else
	(error "Necessary binaries are missing, and unable to install them! Please make sure ZFS kernel modules are loaded and CLI commands are available (i.e. zpool and zfs)!")))
      (when (not (zero? (system* "modprobe" "zfs")))
	(error "ZFS kernel modules are missing!"))
      (utils:println "ZFS kernel modules are loaded!")
      (when lockfile-path
	(with-output-to-file lockfile-path
	  (lambda () (display "")))))))
