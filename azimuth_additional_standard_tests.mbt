// Azimuth 额外标准测试用例
// 使用标准的 MoonBit 测试语法，包含10个测试用例

test "add_positive_numbers" {
  assert_eq(10, add(7, 3))
  assert_eq(100, add(50, 50))
  assert_eq(1, add(0, 1))
}

test "add_negative_numbers" {
  assert_eq(-10, add(-7, -3))
  assert_eq(-5, add(-8, 3))
  assert_eq(0, add(-5, 5))
}

test "multiply_various_numbers" {
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(0, multiply(0, 100))
  assert_eq(25, multiply(-5, -5))
}

test "greet_different_names" {
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob!", greet("Bob"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_positive_results" {
  assert_eq(2, divide_with_ceil(5, 3))
  assert_eq(1, divide_with_ceil(3, 3))
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(0, divide_with_ceil(2, 0))
}

test "divide_with_ceil_negative_results" {
  assert_eq(-1, divide_with_ceil(-5, 3))
  assert_eq(2, divide_with_ceil(-5, -3))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "complex_arithmetic_operations" {
  let step1 = add(10, 20)
  let step2 = multiply(step1, 2)
  let step3 = divide_with_ceil(step2, 5)
  assert_eq(12, step3)  // (10+20)*2 = 60, ceil(60/5) = 12
}

test "real_world_package_calculation" {
  let total_products = 47
  let box_capacity = 8
  let needed_boxes = divide_with_ceil(total_products, box_capacity)
  assert_eq(6, needed_boxes)  // ceil(47/8) = 6
}

test "string_concatenation_with_greet" {
  let greeting = greet("MoonBit")
  let full_message = greeting + " Welcome!"
  assert_eq_string("Hello, MoonBit! Welcome!", full_message)
}

test "mathematical_properties" {
  // 测试加法交换律
  assert_eq(add(15, 25), add(25, 15))
  
  // 测试乘法交换律
  assert_eq(multiply(6, 7), multiply(7, 6))
  
  // 测试加法单位元
  assert_eq(42, add(42, 0))
  
  // 测试乘法零元
  assert_eq(0, multiply(42, 0))
}