// 标准 MoonBit 测试用例
// 为 Azimuth 项目添加基本功能测试

test "basic_addition" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
}

test "basic_multiplication" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
}

test "basic_greet" {
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "basic_divide_ceil" {
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "basic_subtract" {
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(15, 15))
}

test "math_properties" {
  let a = 8
  let b = 12
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "business_scenario" {
  let items = 25
  let capacity = 6
  let packages = divide_with_ceil(items, capacity)
  assert_eq(5, packages)
  
  let total_capacity = multiply(packages, capacity)
  assert_eq(30, total_capacity)
}

test "complex_workflow" {
  let base = 10
  let multiplier = 3
  let divisor = 4
  let addition = 2
  
  let step1 = multiply(base, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, addition)
  
  assert_eq(10, result)
}

test "unicode_support" {
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
}

test "edge_cases" {
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(1000000, multiply(1000, 1000))
}