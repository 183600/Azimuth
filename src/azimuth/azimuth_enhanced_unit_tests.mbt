// Azimuth 增强单元测试
// 使用标准 MoonBit 测试语法编写的8个高质量测试用例

test "add_function_basic" {
  // 测试加法函数的基本功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
}

test "multiply_function_basic" {
  // 测试乘法函数的基本功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
}

test "divide_with_ceil_basic" {
  // 测试向上取整除法的基本功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "subtract_function_basic" {
  // 测试减法函数的基本功能
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
}

test "greet_function_basic" {
  // 测试问候函数的基本功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "complex_calculation" {
  // 测试复杂计算
  let result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result)  // 2*3 + 4*5 = 6 + 20 = 26
}

test "business_scenario" {
  // 测试业务场景
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  
  let subtotal = multiply(unit_price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(200, subtotal)
  assert_eq(215, total)
}

test "packaging_calculation" {
  // 测试包装计算
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}