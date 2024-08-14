(module
  (type $complex (struct (field $re (mut f64)) (field $im (mut f64))))
  (type $vec (array (mut i32)))

  (func $calc_abs
    (param $c (ref null $complex))
    (result f64)
    ;; local variables
    (local $re f64)
    (local $im f64)

    ;; access to ref $complex
    local.get $c
    struct.get $complex 0
    local.set $re
    local.get $c
    struct.get $complex 1
    local.set $im

    ;; Calculate abs
    local.get $re
    local.get $re
    f64.mul
    local.get $im
    local.get $im
    f64.mul
    f64.add
  )

  (func $update_complex
    (param $tmp (ref null $complex))
    (param $init (ref null $complex))
    ;; local variables
    (local $re f64)
    (local $im f64)

    ;; access to ref $complex
    local.get $tmp
    struct.get $complex 0
    local.set $re
    local.get $tmp
    struct.get $complex 1
    local.set $im

    ;; tmp = tmp * tmp + init
    ;; re
    local.get $re
    local.get $re
    f64.mul
    local.get $im
    local.get $im
    f64.mul
    f64.sub
    local.get $init
    struct.get $complex 0
    f64.add
    local.set $re
    ;; update tmp
    (struct.set $complex 0 (local.get $tmp) (local.get $re))

    ;; im
    local.get $re
    local.get $im
    f64.mul
    f64.const 2.0
    f64.mul
    local.get $init
    struct.get $complex 1
    f64.add
    local.set $im
    ;; update tmp
    (struct.set $complex 1 (local.get $tmp) (local.get $im))
  )

  (func $MandelbrotOneInternal 
    (param $init (ref null $complex)) (result i32) 
    ;; local variables
    (local $i i32)
    (local $tmp (ref null $complex))
    (local.set $i (i32.const 0))
    (local.set $tmp (call $make (f64.const 0.0) (f64.const 0.0)))

    ;; loop
    (loop $loop_divergence
      ;; add one to $i
      local.get $i
      i32.const 1
      i32.add
      local.set $i

      ;; Calculate abs(tmp)
      local.get $tmp
      call $calc_abs
      
      ;; if abs(tmp) > 2.0, break the loop and return 0
      f64.const 4.0
      f64.gt
      (if
        (then
          i32.const 0
          return
        )
      )

      ;; update tmp
      local.get $tmp
      local.get $init
      call $update_complex

      ;; if $i < 20, branch to $loop_divergence
      local.get $i
      i32.const 20
      i32.lt_s
      br_if $loop_divergence
    )
    ;; return 1
    i32.const 1
  )  

  (func $make
    (param $re f64) (param $im f64)
    (result (ref null $complex))
    (struct.new $complex (local.get $re) (local.get $im))
  )

  (func $MandelbrotOne (export "MandelbrotOne") (param $re f64) (param $im f64) (result i32)
    (local $c (ref null $complex))
    (local.set $c (call $make (local.get $re) (local.get $im)))
    (call $MandelbrotOneInternal (local.get $c))
  )
  
  (func (export "Mandelbrot") (param $min f64) (param $max f64) (param $delta f64) (param $step i32) (result (ref null $vec))
    ;; delta = (max - min) / step
    ;; for(int iy = 0; iy < step; iy++) {
    ;;  float y = ymin + dy*(double)iy;
    ;;  for(int ix = 0; ix < step; ix++) {
    ;;    float x = xmin + dx*(double)ix;
    ;;    int res = MandelbrotOne(x, y);
    ;;    int idx = iy * step + ix;
    ;;    data[idx] = res;
    ;;  }
    ;; }
    (local $ix i32)
    (local $iy i32)
    (local $x f64)
    (local $y f64)
    (local $res i32)
    (local $idx i32)
    (local $veclen i32)
    (local $resultvec (ref null $vec))

    (local.set $ix (i32.const 0))
    (local.set $iy (i32.const 0))
    (local.set $x (f64.const 0.0))
    (local.set $y (f64.const 0.0))

    ;; vec size = step * step
    (local.get $step)
    (local.get $step)
    i32.mul
    local.set $veclen
    (local.set $resultvec (array.new_default $vec (local.get $veclen)))

    (loop $loop_y
      ;; y = min + delta * iy
      (local.get $min)
      (local.get $delta)
      (local.get $iy)
      f64.convert_i32_s
      f64.mul
      f64.add
      local.set $y

      (local.set $ix (i32.const 0))
      (loop $loop_x
        ;; x = min + delta * ix
        (local.get $min)
        (local.get $delta)
        (local.get $ix)
        f64.convert_i32_s
        f64.mul
        f64.add
        local.set $x

        ;; res = MandelbrotOne(x, y)
        (call $MandelbrotOne (local.get $x) (local.get $y))
        local.set $res

        ;; idx = iy * step + ix
        (local.get $iy)
        (local.get $step)
        i32.mul
        (local.get $ix)
        i32.add
        local.set $idx

        ;; data[idx] = res
        (array.set $vec (local.get $resultvec) (local.get $idx) (local.get $res))

        ;; ix++
        (local.get $ix)
        i32.const 1
        i32.add
        local.set $ix

        ;; if ix >= step, break
        (local.get $ix)
        (local.get $step)
        i32.lt_s
        br_if $loop_x
      )

      ;; iy++
      (local.get $iy)
      i32.const 1
      i32.add
      local.set $iy

      ;; if iy >= step, break
      (local.get $iy)
      (local.get $step)
      i32.lt_s 
      br_if $loop_y
    )
  )
)
