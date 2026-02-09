// 标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_function_basic" {
  // 测试加法函数基本功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-2, @azimuth.add(-5, 3))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "multiply_function_basic" {
  // 测试乘法函数基本功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
}

test "subtract_function_basic" {
  // 测试减法函数基本功能
  @azimuth.assert_eq(3, @azimuth.subtract(8, 5))
  @azimuth.assert_eq(-3, @azimuth.subtract(5, 8))
  @azimuth.assert_eq(0, @azimuth.subtract(10, 10))
  @azimuth.assert_eq(15, @azimuth.subtract(5, -10))
}

test "divide_with_ceil_function" {
  // 测试向上取整除法函数
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(5, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
}

test "greet_function_basic" {
  // 测试问候函数基本功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 3
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 加法单位元
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  
  // 乘法单位元
  @azimuth.assert_eq(a, @azimuth.multiply(a, 1))
}

test "combined_operations" {
  // 测试组合运算
  // (a + b) * c
  let result1 = @azimuth.multiply(@azimuth.add(3, 2), 4)
  @azimuth.assert_eq(20, result1)
  
  // a * b + c
  let result2 = @azimuth.add(@azimuth.multiply(3, 2), 4)
  @azimuth.assert_eq(10, result2)
  
  // (a - b) / c (向上取整)
  let result3 = @azimuth.divide_with_ceil(@azimuth.subtract(10, 3), 2)
  @azimuth.assert_eq(4, result3)
}

test "negative_number_operations" {
  // 测试负数运算
  @azimuth.assert_eq(-2, @azimuth.add(-5, 3))
  @azimuth.assert_eq(2, @azimuth.add(5, -3))
  @azimuth.assert_eq(-8, @azimuth.add(-5, -3))
  
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  
  @azimuth.assert_eq(-8, @azimuth.subtract(-3, 5))
  @azimuth.assert_eq(8, @azimuth.subtract(5, -3))
}

test "boundary_value_tests" {
  // 测试边界值
  // 零值测试
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.subtract(5, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  
  // 单位值测试
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(5, @azimuth.subtract(5, 0))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(5, 1))
}

test "practical_application_scenario" {
  // 实际应用场景测试：购物车计算
  let item_price = 25
  let quantity = 4
  let shipping_cost = 10
  let tax_rate = 10
  
  // 计算商品小计
  let subtotal = @azimuth.multiply(item_price, quantity)
  
  // 计算税费（向上取整）
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total = @azimuth.add(@azimuth.add(subtotal, tax), shipping_cost)
  
  // 计算包装箱数（假设每箱装6个）
  let boxes_needed = @azimuth.divide_with_ceil(quantity, 6)
  
  @azimuth.assert_eq(100, subtotal)
  @azimuth.assert_eq(10, tax)
  @azimuth.assert_eq(120, total)
  @azimuth.assert_eq(1, boxes_needed)
}