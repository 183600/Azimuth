// 简单的 Azimuth 测试用例
// 包含 10 个基本测试用例，使用标准 MoonBit 测试语法

test "addition_basic" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-2, 1))
}

test "multiplication_basic" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
}

test "subtraction_basic" {
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
}

test "division_ceil_basic" {
  assert_eq(2, divide_with_ceil(5, 3))
  assert_eq(1, divide_with_ceil(3, 3))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_basic" {
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "math_properties" {
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "package_calculation" {
  let items = 25
  let capacity = 6
  let packages = divide_with_ceil(items, capacity)
  assert_eq(5, packages)
}

test "temperature_conversion" {
  let celsius = 25
  let fahrenheit = add(multiply(divide_with_ceil(multiply(celsius, 9), 5), 32))
  assert_eq(77, fahrenheit)
}

test "shopping_cart" {
  let price = 25
  let quantity = 4
  let subtotal = multiply(price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, 10), 100)
  let total = add(subtotal, tax)
  assert_eq(110, total)
}

test "resource_allocation" {
  let total = 100
  let tasks = 8
  let per_task = divide_with_ceil(total, tasks)
  let remainder = subtract(total, multiply(per_task, tasks))
  assert_eq(13, per_task)
  assert_eq(4, remainder)
}