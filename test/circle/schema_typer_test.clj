(ns circle.schema-typer-test
  (:require [clojure.test :refer :all]
            [clojure.core.typed :as t]
            [schema.core :as s]
            [circle.schema-typer :as st])
  (:import (clojure.lang Keyword)))

(defmacro is-equiv [val schema type]
  `(do
     (is (s/validate ~schema ~val))
     (is (t/check-form* ~val ~type))
     (is (t/check-form* ~val (st/schema->type ~schema)))))

(deftest schema-type-equivalence

  (testing "any"
    (is-equiv "foo" s/Any 'Any))

  (testing "numbers"
    (is-equiv 3 s/Number 'java.lang.Number))
  
  (testing "schema-pred"
    (is-equiv :foo s/Keyword 'clojure.lang.Keyword))
  
  (testing "hmaps-work"
    (is-equiv {:foo 5}
              {:foo Number}
              '(HMap :mandatory {:foo java.lang.Number})))
  
  (testing "maps-work"
    (is-equiv {:bar "foo"}
              {Keyword String}
              (list 'clojure.core.typed/Map 'clojure.lang.Keyword 'java.lang.String)))
  
  (testing "nested-hmaps-work"
    (is-equiv {:foo {:bar "baz"}}
              {:foo {:bar String}}
              '(HMap :mandatory {:foo (HMap :mandatory {:bar java.lang.String})})))
  
  (testing "mixed-map-hmap"
    (is-equiv {:foo {:bar "baz"}
               :not-listed "String"}
              {:foo {:bar String}
               s/Keyword s/Any}
              '(HMap :mandatory {:foo (HMap :mandatory {:bar java.lang.String})}))))













(deftest optional-keys
  (let [schema {:foo Number
                (s/optional-key :bar) String}
        type '(HMap :mandatory {:foo java.lang.Number} :optional {:bar java.lang.String})]
    (is-equiv {:foo 3}
              schema
              type)
    (is-equiv {:foo 3
               :bar "hello"}
              schema
              type)))

(deftest vectors
  (is-equiv [{:foo 3}]
            [{:foo Long}]
            '(clojure.core.typed/Vec (HMap :mandatory {:foo Integer}))))

(deftest real-use-works
  (is (t/check-ns 'circle.schema-typer-def)))
