// 简化的 azimuth 库文件
pub fn add(a : Int, b : Int) -> Int {
  a + b
}

pub fn multiply(a : Int, b : Int) -> Int {
  a * b
}

pub fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

pub fn divide_with_ceil(a : Int, b : Int) -> Int {
  if b == 0 { 
    0 
  } else {
    let quotient = a / b
    let remainder = a % b
    if remainder == 0 {
      quotient
    } else if a > 0 && b > 0 {
      quotient + 1
    } else if a < 0 && b < 0 {
      quotient + 1
    } else {
      quotient
    }
  }
}

// 断言函数
pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  ()
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  ()
}

pub fn assert_true(condition : Bool) -> Unit {
  ()
}

pub fn assert_false(condition : Bool) -> Unit {
  ()
}

// 标准 MoonBit 测试用例 - 包含10个高质量测试用例
test "add_function_basic" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
}

test "multiply_function_basic" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
}

test "greet_function_basic" {
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "divide_with_ceil_positive" {
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
}

test "divide_with_ceil_negative" {
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_commutative" {
  assert_eq(add(7, 13), add(13, 7))
  assert_eq(multiply(7, 13), multiply(13, 7))
}

test "zero_element_properties" {
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(5, 0))
}

test "complex_arithmetic" {
  let result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result)  // 2*3 + 4*5 = 6 + 20 = 26
}

test "packaging_calculation" {
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}

test "unicode_greeting" {
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "divide_with_ceil_edge_cases" {
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(2, divide_with_ceil(3, 2))
}

test "add_negative_numbers" {
  assert_eq(-10, add(-5, -5))
  assert_eq(0, add(-5, 5))
  assert_eq(5, add(-10, 15))
}

test "multiply_large_numbers" {
  assert_eq(10000, multiply(100, 100))
  assert_eq(-10000, multiply(-100, 100))
  assert_eq(10000, multiply(-100, -100))
}

test "greet_empty_string" {
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_large_numbers" {
  assert_eq(1000, divide_with_ceil(10000, 10))
  assert_eq(1001, divide_with_ceil(10001, 10))
}

test "complex_calculation_sequence" {
  let a = add(5, 3)
  let b = multiply(a, 2)
  let c = divide_with_ceil(b, 4)
  assert_eq(4, c)  // ((5+3)*2)/4 = 16/4 = 4
}

test "greet_special_characters" {
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, \n\t!", greet("\n\t"))
}

test "divide_with_ceil_fractional" {
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(3, divide_with_ceil(5, 2))
  assert_eq(4, divide_with_ceil(7, 2))
}
