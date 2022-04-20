;; Time-stamp: <2022-03-24 12:43:31 gorbag>

(cl:in-package :asdf)

(defsystem :fpga-support
  :serial t ; inefficient but simple. 
  :depends-on (named-readtables)
  :components
  (
   (:file "packages")
   (:file "common")

   ;; stuff about this library, and specifying important things about
   ;; our project in the project files (e.g. bits in a nominal byte,
   ;; what a byte means (is used to represent) for the project, word
   ;; length, register size, etc.). It's early days for this library
   ;; so I expect more things to move to this kind of declaratory
   ;; system as we harden our first couple projects. (Note that most
   ;; of these things may specify parameters about the code we
   ;; generate or the PLAs we construct, and thus have a runtime
   ;; impact but are mostly interpreted offline during construction of
   ;; the generated project).
   
   (:file "fpga-support-module-defs")
   (:file "project-defs") ; should be bound/set in a particular project
   
   ;; stuff that is for simulation support ONLY should go in this directory
   
   (:module "simulation" :serial t
    :components
    #+capi
    ((:module "gui" :serial t
      :components
      ((:file "grid-impl"))) ;patched version of LW CAPI demo

     (:file "fpga-diagnostics"))) ;diagnostics 'package'

   ;; stuff that is for VHDL generation ONLY should go into this directory
   (:module "VHDL" :serial t
    :components
    ((:file "vhdl-defs")
     (:file "vhdl-comments")
     (:file "vhdl-components")))

   ;; stuff that is for XDC gneeration should go into this directory
   ;; (XDC = Xilinx Design Constraints: this is what we need to
   ;; actually synthesize a design for an FPGA and includes
   ;; information not needed for simulation, like clock frequency)
   ;;
   ;; generally speaking we have to pass XDC information to Vivado via
   ;; TCL so there may be some TCL support in this directory as well,
   ;; including that needed to run Vivado from the command line.
   (:module "XDC" :serial t
    :components
    ())

   ;; other things may be both for simulation and generating FPGA HDL
   ;; we separate those in general to stuff that is combinatoric, and
   ;; stuff that is clocked (mainly because HDL distinguishes
   ;; them). Clocked things are registered/buffered/latched using a
   ;; clock signal (so the relevant code here describes those latches,
   ;; buffers, etc. as classes allowing instances to be described in
   ;; the project(s)), while combinatoric things will generate
   ;; networks of NAND gates (or whatever) at the lowest level - so
   ;; may have a propegation time through the network, and may be
   ;; astable until some period has passed after which it is typically
   ;; latched for the next network to take as an imput. So typical
   ;; HDL/FPGA code will include both.
   
   (:module "combinatorics" :serial t
    :components
    ((:file "fpga-combinatorics")))
   (:module "clocked-state" :serial t
    :components
    ((:file "fpga-clocked-state")
     (:file "fpga-clocks")
     (:file "fpga-clock-sim")
     (:file "plas")
     (:file "registers")
     (:file "pads")))

   ;; here we describe the FPGA platform we are using, as well as any
   ;; additional relevant hardware (for which we may want to have supporting
   ;; microcode!). Note the intent is, like everything else, this will drive
   ;; HDL generation as well as building simulated versions (at this time that
   ;; would be CAPI).
   
   (:module "hardware" :serial t
    :components
    ())
   
   ;; and then we have PLA (programmable logic arrays) that for us are
   ;; essentially EPROMS: we have tools for specifying what to put in
   ;; them and use them as our microcode store, for instance (but also
   ;; might describe, say, mantissas for a floating-point unit). Stuff
   ;; in this subdirectory is expected to run offline only, but will
   ;; create structures (generally arrays of bit-vectors) that get
   ;; loaded into the FPGA or simulator. Also note that we have more
   ;; 'flexibility' in simulation, so some capabilities (a microcode
   ;; source code console display, for instance) might only be
   ;; 'available' in simulation and thus get put into representations
   ;; that are defined in the simulation module, above.

   (:module "pla-support" :serial t
    :components
    ((:file "common-defs")
     (:file "assembler-core-defs")
     (:file "ulisp-defs")
     (:file "compiler-core-defs")
     (:file "ulisp-support")
     (:file "ulisp-macros")
     (:file "ulisp-validation")
     (:file "upla-assembler")
     (:file "ulisp-compiler")))
   ))
