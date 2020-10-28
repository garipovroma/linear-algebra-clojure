**Linear algebra on Clojure**

Realised some linear algebra operations using simple clojure functions.

```clojure
(defn abstract-operation [check-function]
  (fn [operation]
    (letfn [(recursive-operation [& args]
              {:pre [(or (every? number? args) (vectors-sizes-equals? args))]}
              (if (every? number? args) (apply operation args) (apply mapv recursive-operation args)))]
      (fn [& args]
        {:pre [(every? check-function args)]}
        (apply recursive-operation args)))))
```

`abstract-operation` - factory function for creating operation functions. All functions created using it.