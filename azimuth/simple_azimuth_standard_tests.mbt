// 简化的 Azimuth 项目标准 MoonBit 测试用例
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_addition" {
  // 基本加法运算测试
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-3, 2))
  assert_eq(100, add(50, 50))
  assert_eq(-100, add(-50, -50))
}

test "basic_arithmetic_multiplication" {
  // 基本乘法运算测试
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(10000, multiply(100, 100))
}

test "divide_with_ceil_positive_numbers" {
  // 向上取整除法正数测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 向上取整除法负数测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(1, divide_with_ceil(-1, -2))
}

test "greet_function_various_inputs" {
  // 问候函数各种输入测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
}

test "subtract_function_comprehensive" {
  // 减法函数综合测试
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(15, 15))
  assert_eq(20, subtract(10, -10))
  assert_eq(-20, subtract(-10, 10))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(20, add(a, b))
  assert_eq(96, multiply(a, b))
}

test "complex_calculation_sequence" {
  // 复杂计算序列测试
  let initial = 10
  
  // 计算序列: ((10 + 5) * 2) - 3 = 27
  let step1 = add(initial, 5)
  let step2 = multiply(step1, 2)
  let result = subtract(step2, 3)
  
  assert_eq(15, step1)
  assert_eq(30, step2)
  assert_eq(27, result)
}

test "edge_case_zero_handling" {
  // 零值处理边界情况测试
  // 测试各种涉及零的运算
  
  // 加法中的零
  assert_eq(5, add(5, 0))
  assert_eq(-5, add(-5, 0))
  
  // 乘法中的零
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, -5))
  
  // 减法中的零
  assert_eq(5, subtract(5, 0))
  assert_eq(-5, subtract(0, 5))
  
  // 除法中的零
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "real_world_shopping_cart_calculation" {
  // 真实世界购物车计算测试
  // 模拟商品价格计算
  
  let apple_price = 5
  let apple_quantity = 3
  let banana_price = 3
  let banana_quantity = 4
  
  // 计算各商品小计
  let apple_subtotal = multiply(apple_price, apple_quantity)
  let banana_subtotal = multiply(banana_price, banana_quantity)
  
  // 计算总价
  let total = add(apple_subtotal, banana_subtotal)
  
  // 计算每箱包装数量（假设每箱最多装10个商品）
  let total_items = add(apple_quantity, banana_quantity)
  let boxes_needed = divide_with_ceil(total_items, 10)
  
  assert_eq(15, apple_subtotal)
  assert_eq(12, banana_subtotal)
  assert_eq(27, total)
  assert_eq(7, total_items)
  assert_eq(1, boxes_needed)
}