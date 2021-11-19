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

(define* (install-deps-base)
  (let ((missing (utils:which* "sgdisk" "partprobe" "cryptsetup" "mkfs.fat")))
    (if (not (null? missing))
	(if (read-debian-version)
	    (begin
	      (display "Installing necessary packages...")
	      (system "apt update")
	      (when (not (zero? (system "apt install -y gdisk parted cryptsetup dosfstools")))
		(error "Failed to install packages: gdisk, parted, cryptsetup")))
	    (error "Necessary binaries are missing" missing)))))

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

(define* (install-deps-zfs)
  (let ((release (or (read-debian-version) 0)))
    (cond
     ((member release (list 8 10))
      (call-with-input-file "/etc/apt/sources.list"
	(lambda (input-port)
	  (let* ((pattern (make-regexp "^deb ([^ ]+) ([^ ]+) main$" regexp/newline))
		 (match (rdelim:read-string input-port))
		 (match (regexp-exec pattern match))
		 (uri (regex:match:substring match 1))
		 (suite (regex:match:substring match 2))
		 (suite (string-append suite "-backports")))
	    (call-with-output-file "/etc/apt/sources.list.d/backports.list"
	      (lambda (output-port)
		(format output-port "deb ~A ~A main contrib\n" uri suite)
		(format output-port "deb-src ~A ~A main contrib\n" uri suite)))
	    (system* "apt" "update"
		     "-o" "Acquire::Check-Valid-Until=false"
		     "-o" "Acquire::Check-Date=false")
	    (when (not (zero? (system* "apt" "install" "-y" "-t" suite "zfs-dkms")))
	      (error "Failed to install package zfs-dkms"))))))
     ((<= 9 release)
      (when (not (file-exists? "/etc/apt/sources.list.d/base.list"))
	(utils:move-file "/etc/apt/sources.list" "/etc/apt/sources.list.d/base.list"))
      (call-with-input-file "/etc/apt/sources.list.d/base.list"
	(lambda (input-port)
	  (let* ((pattern (make-regexp "^deb ([^ ]+) ([^ ]+) main$" regexp/newline))
		 (content (rdelim:read-string input-port))
		 (matches (regexp-exec pattern content))
		 (uri (and matches (regex:match:substring matches 1)))
		 (suite (and matches (regex:match:substring matches 2))))
	    (when matches
	      (call-with-output-file "/etc/apt/sources.list"
		(lambda (output-port)
		  (format output-port "deb ~A ~A main contrib\n" uri suite)
		  (format output-port "deb-src ~A ~A main contrib\n" uri suite)))))))
      (when (file-exists? "/etc/apt/sources.list")
	(delete-file "/etc/apt/sources.list.d/base.list"))
      (system* "apt" "update")
      (when (not (zero? (system* "apt" "install" "-y" "zfs-dkms")))
	(error "Failed to install package zfs-dkms")))
     (else
      (error "Necessary binaries are missing, and unable to install them! Please make sure ZFS kernel modules are loaded and CLI commands are available (i.e. zpool and zfs)!")))))
