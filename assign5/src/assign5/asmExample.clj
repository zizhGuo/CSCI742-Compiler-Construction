(ns test4.asmExample
    (:use [clojure.java.io :only (output-stream)])
    (:import (clojure.asm Opcodes Type ClassWriter)
             (clojure.asm.commons Method GeneratorAdapter)))

(defn generate-class-prelude [class-name] ; takes a string; returns a ClassWriter instance
   (let [cw (ClassWriter. ClassWriter/COMPUTE_MAXS)]
     (.visit cw Opcodes/V1_1 Opcodes/ACC_PUBLIC class-name nil "java/lang/Object" nil)
     (let [mv (.visitMethod cw Opcodes/ACC_PUBLIC "<init>" "()V" nil nil)]
       (.visitVarInsn mv Opcodes/ALOAD 0)
       (.visitMethodInsn mv Opcodes/INVOKESPECIAL "java/lang/Object" "<init>" "()V")
       (.visitInsn mv Opcodes/RETURN)
       (.visitMaxs mv 1 1)
       (.visitEnd mv)
       cw)))

(defn generate-class-postlude [cw] ;takes a ClassWriter instance; returns a byte array
   (.toByteArray cw))

(defn generate-method-prelude [cw method-name arg-type ret-type] ; takes a ClassWriter instance, 3 strings
   (.visitMethod cw Opcodes/ACC_PUBLIC method-name (str "(" arg-type ")" ret-type) nil nil)); returns a MethodVisitor

(defn generate-static-method-prelude [cw method-name arg-type ret-type] ; takes a ClassWriter instance, 3 strings
   (.visitMethod cw (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) 
                    method-name (str "(" arg-type ")" ret-type) nil nil)); returns a MethodVisitor

(defn generate-method-postlude [mv] ; takes a MethodVisitor
   (.visitMaxs mv 1 1)
   (.visitEnd mv))

(defn create-static [cname]
  (let [cw (generate-class-prelude cname)
        mv (generate-static-method-prelude cw "main" "[Ljava/lang/String;" "V")]
     ; instructions go here
     (.visitFieldInsn mv Opcodes/GETSTATIC "java/lang/System" "out" "Ljava/io/PrintStream;")
     (.visitLdcInsn mv (Integer. 2)) 
     (.visitLdcInsn mv (Integer. 3)) 
     (.visitInsn mv Opcodes/IADD)
     (.visitMethodInsn mv Opcodes/INVOKEVIRTUAL "java/io/PrintStream" "println" "(I)V") 
     (.visitInsn mv Opcodes/RETURN)
     (let [_ (generate-method-postlude mv)
           b (generate-class-postlude cw)]
        b)))

(defn write-class-bytes [class-name b]
  (with-open [o (output-stream (str class-name ".class"))]
    (.write o b))) 