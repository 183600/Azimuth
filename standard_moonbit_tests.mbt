// 标准 MoonBit 测试用例
// 为 azimuth 项目编写的测试用例，使用标准 MoonBit 测试语法

test "add_function_basic_functionality" {
  // 测试加法函数的基本功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(10, add(15, -5))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(12, multiply(-3, -4))
  assert_eq(100, multiply(10, 10))
}

test "subtract_function_verification" {
  // 测试减法函数的功能
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(7, 7))
  assert_eq(15, subtract(0, -15))
  assert_eq(-10, subtract(-5, 5))
}

test "divide_with_ceil_positive_numbers" {
  // 测试向上取整除法的正数情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 测试向上取整除法的负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-2, divide_with_ceil(-4, 2))
  assert_eq(0, divide_with_ceil(-5, 0))
  assert_eq(3, divide_with_ceil(10, -3))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_properties_verification" {
  // 测试数学性质验证
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法单位元
  assert_eq(a, add(a, 0))
  
  // 乘法单位元
  assert_eq(a, multiply(a, 1))
}

test "complex_business_calculation" {
  // 测试复杂业务计算场景
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(200, subtotal)
  assert_eq(215, total_before_discount)
  assert_eq(195, final_total)
}

test "packaging_calculation_scenario" {
  // 测试包装计算场景
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(5, boxes_needed)
  
  // 测试更多包装场景
  assert_eq(3, divide_with_ceil(17, 6))
  assert_eq(1, divide_with_ceil(5, 10))
  assert_eq(0, divide_with_ceil(0, 6))
}

test "combined_operations_stress_test" {
  // 测试组合运算的压力测试
  let x = 10
  let y = 15
  let z = 20
  
  // 复杂表达式计算
  let result1 = add(multiply(x, y), multiply(y, z))
  assert_eq(425, result1)  // 10*15 + 15*20 = 150 + 300 = 450
  
  let result2 = multiply(add(x, y), divide_with_ceil(z, 3))
  assert_eq(175, result2)  // (10+15) * ceil(20/3) = 25 * 7 = 175
  
  let result3 = subtract(multiply(x, z), divide_with_ceil(multiply(y, 2), 3))
  assert_eq(190, result3)  // 10*20 - ceil(15*2/3) = 200 - 10 = 190
}