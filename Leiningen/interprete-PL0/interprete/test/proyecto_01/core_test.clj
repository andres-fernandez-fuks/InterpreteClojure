(ns proyecto-01.core-test
  (:require [clojure.test :refer :all]
            [proyecto-01.core :refer :all]))

(deftest test-identificador
  (testing "Identificador con diferentes llamadas"
    (is (= false 
           (identificador? 2)
        )
    )
    (is (= true 
           (identificador? 'V2)
        )
    )
    (is (= true 
           (identificador? "V2")
        )
    )
    (is (= false 
           (identificador? 'CALL)
        )
    )
  )
)
