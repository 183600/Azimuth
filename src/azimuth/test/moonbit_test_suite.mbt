// MoonBit 标准测试套件
// 为 Azimuth 项目编写的标准测试用例

test "add_function_basic_cases" {
  // 测试加法函数的基本情况
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-3, @azimuth.add(-1, -2))
  @azimuth.assert_eq(10, @azimuth.add(15, -5))
  @azimuth.assert_eq(100, @azimuth.add(100, 0))
  @azimuth.assert_eq(-100, @azimuth.add(-100, 0))
}

test "add_function_overflow_cases" {
  // 测试加法函数的溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出情况
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  
  // 负数溢出情况
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  
  // 边界值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "multiply_function_basic_cases" {
  // 测试乘法函数的基本情况
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
}

test "multiply_function_overflow_cases" {
  // 测试乘法函数的溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 大数相乘溢出
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 特殊值处理
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(-1, @azimuth.multiply(1, -1))
}

test "greet_function_cases" {
  // 测试问候函数的各种情况
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  let c = 0
  
  // 加法单位元
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  @azimuth.assert_eq(b, @azimuth.add(0, b))
  
  // 乘法单位元
  @azimuth.assert_eq(a, @azimuth.multiply(a, 1))
  @azimuth.assert_eq(b, @azimuth.multiply(1, b))
  
  // 乘法零元
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, b))
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "practical_business_calculations" {
  // 测试实际业务计算场景
  let unit_price = 25
  let quantity = 8
  let tax_rate = 5
  let discount = 15
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(unit_price, quantity)
  @azimuth.assert_eq(200, subtotal)
  
  // 计算税额
  let tax_amount = @azimuth.multiply(subtotal, tax_rate)
  @azimuth.assert_eq(1000, tax_amount)
  
  // 计算折扣
  let discount_amount = @azimuth.multiply(subtotal, discount)
  @azimuth.assert_eq(3000, discount_amount)
  
  // 计算最终总价（简化计算）
  let final_total = @azimuth.add(subtotal, @azimuth.add(tax_amount, -discount_amount))
  @azimuth.assert_eq(-2800, final_total)
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  let base_value = 100
  let multiplier = 3
  let increment = 50
  let decrement = 25
  
  // 复杂表达式: (base × multiplier) + increment - decrement
  let step1 = @azimuth.multiply(base_value, multiplier)
  @azimuth.assert_eq(300, step1)
  
  let step2 = @azimuth.add(step1, increment)
  @azimuth.assert_eq(350, step2)
  
  let final_result = @azimuth.add(step2, -decrement)
  @azimuth.assert_eq(325, final_result)
  
  // 验证另一种计算方式
  let alternative = @azimuth.add(@azimuth.add(@azimuth.multiply(base_value, multiplier), increment), -decrement)
  @azimuth.assert_eq(final_result, alternative)
}

test "edge_cases_and_special_values" {
  // 测试边界情况和特殊值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值和最小值的加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  
  // 最大值和最小值的乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 自反性测试
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(0, @azimuth.add(-10, 10))
  @azimuth.assert_eq(0, @azimuth.add(100, -100))
}