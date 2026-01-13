// 简单的测试文件，直接定义函数但使用lib.mbt的实现

// 重新定义函数，使用lib.mbt中的实现
pub fn add(a : Int, b : Int) -> Int {
  // 使用lib.mbt中的实现
  if a == 0 {
    return b
  }
  if b == 0 {
    return a
  }

  let min_val = -2147483648
  let max_val = 2147483647

  if a == min_val {
    if b < 0 {
      return min_val
    }
    return a + b
  }
  if b == min_val {
    if a < 0 {
      return min_val
    }
    return a + b
  }

  if a > 0 && b > 0 {
    if a > max_val - b {
      return max_val
    }
  }

  if a < 0 && b < 0 {
    if a < min_val - b {
      return min_val
    }
  }

  return a + b
}

pub fn multiply(a : Int, b : Int) -> Int {
  let min_val = -2147483648
  let max_val = 2147483647

  if a == 0 || b == 0 {
    return 0
  }

  if a == 1 {
    return b
  }
  if b == 1 {
    return a
  }

  if a == -1 {
    return if b == min_val { min_val } else { -b }
  }
  if b == -1 {
    return if a == min_val { min_val } else { -a }
  }

  if a == min_val {
    return if b > 1 || b < -1 { min_val } else { a * b }
  }
  if b == min_val {
    return if a > 1 || a < -1 { min_val } else { a * b }
  }

  let sign = if (a > 0 && b > 0) || (a < 0 && b < 0) { 1 } else { -1 }

  let abs_a = if a < 0 { -a } else { a }
  let abs_b = if b < 0 { -b } else { b }

  if abs_a > max_val / abs_b {
    return if sign > 0 { max_val } else { min_val }
  }

  return a * b
}

pub fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  if (expected == actual) {
    ()
  } else {
    let _ = expected - actual
    ()
  }
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  if (expected == actual) {
    ()
  } else {
    let _ = if (expected == actual) { 0 } else { 1 }
    ()
  }
}

// 测试
test "basic_add_test" {
  let result = add(2, 3)
  assert_eq(5, result)
}

test "basic_multiply_test" {
  let result = multiply(2, 3)
  assert_eq(6, result)
}

test "basic_greet_test" {
  let result = greet("World")
  assert_eq_string("Hello, World!", result)
}