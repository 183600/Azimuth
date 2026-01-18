// Azimuth 项目标准 MoonBit 测试用例
// 测试核心功能的典型使用场景

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(10, @azimuth.add(6, 4))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 用户123!", @azimuth.greet("用户123"))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  
  // 边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "practical_business_calculation" {
  // 测试实际业务计算：订单总价
  let unit_price = 199
  let quantity = 3
  let discount = 50
  let shipping = 15
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(unit_price, quantity)
  // 应用折扣和运费
  let total = @azimuth.add(@azimuth.add(subtotal, -discount), shipping)
  
  @azimuth.assert_eq(602, total)
}

test "temperature_conversion_scenario" {
  // 测试温度转换场景
  // 摄氏度转华氏度（简化公式：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing = @azimuth.add(@azimuth.multiply(0, 2), 30)
  @azimuth.assert_eq(30, freezing)
}

test "complex_expression_evaluation" {
  // 测试复杂表达式求值：(a + b) × c - d
  let a = 5
  let b = 3
  let c = 4
  let d = 10
  
  let sum_ab = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum_ab, c)
  let result = @azimuth.add(product, -d)
  
  @azimuth.assert_eq(22, result)
}

test "statistical_mean_calculation" {
  // 测试统计平均值计算
  let numbers = [65, 80, 75, 90, 85]
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(65, 80), 75), 90), 85)
  // 计算平均值
  let mean = sum / 5
  
  @azimuth.assert_eq(79, mean)
}

test "assertion_functions_verification" {
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}

test "edge_case_combinations" {
  // 测试边界情况组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 零值运算
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(max_val, @azimuth.add(0, max_val))
  
  // 负一运算
  @azimuth.assert_eq(-max_val, @azimuth.multiply(max_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(min_val, -1))
}