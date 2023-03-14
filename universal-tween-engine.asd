(defsystem universal-tween-engine
  :version "1.0.0"
  :author "Bohong Huang <1281299809@qq.com>"
  :maintainer "Bohong Huang <1281299809@qq.com>"
  :license "Apache-2.0"
  :description "Common Lisp port of Universal Tween Engine"
  :homepage "https://github.com/bohonghuang/cl-universal-tween-engine"
  :bug-tracker "https://github.com/bohonghuang/cl-universal-tween-engine/issues"
  :source-control (:git "https://github.com/bohonghuang/cl-universal-tween-engine.git")
  :depends-on (#:alexandria)
  :components ((:file "api" :depends-on ("package" "tween-manager" "pool" "tween-equations" "tween-paths"))
               (:file "base-tween" :depends-on ("package" "tween-callback"))
               (:file "package")
               (:file "pool" :depends-on ("package"))
               (:file "timeline" :depends-on ("package" "base-tween" "pool"))
               (:file "tween" :depends-on ("package" "base-tween" "pool"))
               (:file "tween-callback" :depends-on ("package"))
               (:file "tween-equations" :depends-on ("package"))
               (:file "tween-manager" :depends-on ("package" "base-tween" "tween" "timeline"))
               (:file "tween-paths" :depends-on ("package"))))

(defsystem universal-tween-engine/demo
  :pathname "./demo/"
  :components ((:file "package"))
  :depends-on (#:asdf #:alexandria #:universal-tween-engine #:cl-raylib)
  :build-operation program-op
  :build-pathname "universal-tween-engine-demo"
  :entry-point "ute.demo:main")
