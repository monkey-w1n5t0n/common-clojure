;; Test nested destructuring
(let [{[{:keys [data]}] :via
       data-top-level :data}
       (Throwable->map (ex-info "ex-info" {:some "data"}))]
  data-top-level)
