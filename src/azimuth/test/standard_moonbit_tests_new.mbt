// MoonBit 标准测试用例
// 测试核心功能和边界情况

test "addition_basic_functionality" {
  // 测试基本加法功能
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "addition_overflow_handling" {
  // 测试加法溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "multiplication_basic_functionality" {
  // 测试基本乘法功能
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
}

test "multiplication_overflow_handling" {
  // 测试乘法溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "complex_mathematical_operations" {
  // 测试复杂数学运算
  // 计算 (a + b) * c - d
  let a = 5
  let b = 3
  let c = 4
  let d = 10
  
  let sum_ab = @azimuth.add(a, b)  // 8
  let product = @azimuth.multiply(sum_ab, c)  // 32
  let result = @azimuth.add(product, -d)  // 22
  
  @azimuth.assert_eq(22, result)
}

test "real_world_calculation_scenario" {
  // 测试实际应用场景：计算折扣价格
  let original_price = 200
  let discount_percentage = 15  // 15% 折扣
  let tax_rate = 8  // 8% 税率
  
  // 计算折扣金额
  let discount_amount = @azimuth.multiply(original_price, discount_percentage) / 100  // 30
  let discounted_price = @azimuth.add(original_price, -discount_amount)  // 170
  
  // 计算税费
  let tax_amount = @azimuth.multiply(discounted_price, tax_rate) / 100  // 13
  let final_price = @azimuth.add(discounted_price, tax_amount)  // 183
  
  @azimuth.assert_eq(183, final_price)
}

test "boundary_value_comprehensive" {
  // 测试边界值的综合情况
  let max_val = 2147483647
  let min_val = -2147483648
  let zero = 0
  
  // 边界值与零的运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, zero))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, zero))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, zero))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, zero))
  
  // 边界值与1的运算
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

test "mathematical_properties_verification" {
  // 测试数学性质的验证
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  let c = 5
  @azimuth.assert_eq(
    @azimuth.multiply(a, @azimuth.add(b, c)),
    @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  )
}