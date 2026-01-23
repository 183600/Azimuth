// 新的 MoonBit 测试用例
// 为 azimuth 包添加标准测试用例

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(-3, @azimuth.add(5, -8))
  @azimuth.assert_eq(0, @azimuth.add(100, -100))
  
  @azimuth.assert_eq(24, @azimuth.multiply(6, 4))
  @azimuth.assert_eq(-18, @azimuth.multiply(3, -6))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 999))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  
  // 测试最小值边界
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "string_processing_comprehensive" {
  // 测试字符串处理的综合情况
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
  @azimuth.assert_eq_string("Hello,   spaced   !", @azimuth.greet("  spaced  "))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 场景1：计算矩形周长
  let length = 8
  let width = 5
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(26, perimeter)
  
  // 场景2：计算平均数
  let num1 = 85
  let num2 = 90
  let num3 = 95
  let sum = @azimuth.add(@azimuth.add(num1, num2), num3)
  let average = sum / 3
  @azimuth.assert_eq(90, average)
  
  // 场景3：计算折扣价格
  let original_price = 200
  let discount_rate = 20  // 20%
  let discount_amount = @azimuth.multiply(original_price, discount_rate) / 100
  let final_price = @azimuth.add(original_price, -discount_amount)
  @azimuth.assert_eq(160, final_price)
}

test "mathematical_properties_verification" {
  // 测试数学性质验证
  let a = 12
  let b = 8
  let c = 5
  
  // 验证加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 验证乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 验证分配律
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
  
  // 验证结合律
  let add_assoc1 = @azimuth.add(@azimuth.add(a, b), c)
  let add_assoc2 = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(add_assoc1, add_assoc2)
}

test "real_world_business_calculations" {
  // 测试实际业务计算
  // 场景1：订单总价计算
  let item_price = 89
  let quantity = 4
  let shipping_cost = 15
  let tax_rate = 8  // 8%
  
  let subtotal = @azimuth.multiply(item_price, quantity)
  let tax_amount = @azimuth.multiply(subtotal, tax_rate) / 100
  let total_cost = @azimuth.add(@azimuth.add(subtotal, tax_amount), shipping_cost)
  @azimuth.assert_eq(383, total_cost)
  
  // 场景2：利润计算
  let revenue = 5000
  let cost1 = 2000
  let cost2 = 1500
  let total_cost = @azimuth.add(cost1, cost2)
  let profit = @azimuth.add(revenue, -total_cost)
  @azimuth.assert_eq(1500, profit)
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 测试特殊值处理
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
  @azimuth.assert_eq(12345, @azimuth.multiply(12345, 1))
  @azimuth.assert_eq(-12345, @azimuth.multiply(12345, -1))
  @azimuth.assert_eq(12345, @azimuth.add(12345, 0))
}

test "performance_and_stress_testing" {
  // 测试性能和压力情况
  // 大数运算测试
  let large_num1 = 1000000
  let large_num2 = 2000
  
  @azimuth.assert_eq(1002000, @azimuth.add(large_num1, large_num2))
  @azimuth.assert_eq(2000000000, @azimuth.multiply(large_num1, large_num2))
  
  // 连续运算测试
  let result = @azimuth.add(@azimuth.multiply(10, 20), @azimuth.multiply(30, 40))
  @azimuth.assert_eq(1400, result)
  
  // 复杂数学表达式：((a + b) * c) - d
  let a = 50
  let b = 30
  let c = 4
  let d = 100
  let complex_result = @azimuth.add(@azimuth.multiply(@azimuth.add(a, b), c), -d)
  @azimuth.assert_eq(220, complex_result)
}

test "data_validation_and_type_handling" {
  // 测试数据验证和类型处理
  // 字符串验证测试
  @azimuth.assert_eq_string("Hello, test!", @azimuth.greet("test"))
  @azimuth.assert_eq_string("Hello, world!", @azimuth.greet("world"))
  
  // 数值范围验证
  let positive_num = 100
  let negative_num = -100
  let zero = 0
  
  @azimuth.assert_eq(zero, @azimuth.add(positive_num, negative_num))
  @azimuth.assert_eq(zero, @azimuth.multiply(positive_num, zero))
  @azimuth.assert_eq(zero, @azimuth.multiply(negative_num, zero))
  
  // 边界值验证
  let max_val = 2147483647
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
}

test "integration_and_composite_functions" {
  // 测试集成和复合函数
  // 场景1：温度转换（摄氏度转华氏度，简化版：F = C * 2 + 30）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 场景2：计算二次方程 ax² + bx + c
  let x = 3
  let a = 2
  let b = 4
  let c = 1
  let quadratic = @azimuth.add(@azimuth.add(@azimuth.multiply(a, @azimuth.multiply(x, x)), @azimuth.multiply(b, x)), c)
  @azimuth.assert_eq(31, quadratic)
  
  // 场景3：计算数组索引（二维转一维）
  let row = 2
  let col = 3
  let width = 5
  let index = @azimuth.add(@azimuth.multiply(row, width), col)
  @azimuth.assert_eq(13, index)
}