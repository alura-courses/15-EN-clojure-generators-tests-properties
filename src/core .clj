(ns hospital5.core
  (:use clojure.pprint)
  (:require [clojure.test.check.generators :as gen]
            [hospital5.model :as h.model]
            [schema-generators.generators :as g]))

;(println (gen/sample gen/boolean 3))
;(println (gen/sample gen/int 100))
;(println (gen/sample gen/string-alphanumeric 100))
;(println (gen/sample (gen/vector gen/int) 100))
(println (g/sample 5 h.model/PatientID))
(println "Departments")
(pprint (g/sample 10 h.model/Department))
(println "Hospitals")
;(pprint (g/sample 10 h.model/Hospital))
(pprint (g/generate h.model/Hospital))