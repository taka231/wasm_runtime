(module
  (func (export "_start") (result i64) (local i64 i64)
    i64.const 0
    (local.set 1)
    i64.const 0
    (local.set 0)
    (loop
      i64.const 1
      (local.get 0)
      i64.add
      local.set 0
      (local.get 1)
      (local.get 0)
      i64.add
      (local.set 1)
      (br_if 0 (i64.lt_u (local.get 0) (i64.const 10)))
    )
    (local.get 1)
  )
)
