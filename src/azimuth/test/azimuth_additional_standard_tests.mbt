// Azimuth 项目额外的标准 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

// 测试用例 1: 测试加法交换律
test "addition_commutativity" {
  let a = 42
  let b = 17
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
}

// 测试用例 2: 测试乘法结合律
test "multiplication_associativity" {
  let a = 3
  let b = 4
  let c = 5
  let left = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let right = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(left, right)
}

// 测试用例 3: 测试分配律
test "distributive_property" {
  let a = 6
  let b = 7
  let c = 8
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)
}

// 测试用例 4: 测试零元特性
test "zero_properties" {
  let x = 123
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(x, @azimuth.add(0, x))
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, x))
}

// 测试用例 5: 测试单位元特性
test "identity_properties" {
  let x = 456
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(x, @azimuth.multiply(1, x))
}

// 测试用例 6: 测试负数运算
test "negative_operations" {
  let a = 10
  let b = -3
  let c = -7
  
  // 测试正数与负数相加
  @azimuth.assert_eq(7, @azimuth.add(a, b))
  
  // 测试两个负数相加
  @azimuth.assert_eq(-10, @azimuth.add(b, c))
  
  // 测试正数与负数相乘
  @azimuth.assert_eq(-30, @azimuth.multiply(a, b))
  
  // 测试两个负数相乘
  @azimuth.assert_eq(21, @azimuth.multiply(b, c))
}

// 测试用例 7: 测试字符串处理
test "string_processing_variations" {
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  
  // 测试包含数字的字符串
  @azimuth.assert_eq_string("Hello, User123!", @azimuth.greet("User123"))
  
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
}

// 测试用例 8: 测试边界值组合
test "boundary_combinations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 测试边界值附近的运算
  @azimuth.assert_eq(2147483646, @azimuth.add(max_val, -1))
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
}

// 测试用例 9: 测试复合计算场景
test "complex_calculation_scenario" {
  // 模拟计算折扣价格：原价 × (1 - 折扣率) + 运费
  let original_price = 1000
  let discount_rate = 20  // 20% 折扣
  let shipping_fee = 50
  
  let discount_amount = @azimuth.multiply(original_price, discount_rate) / 100
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  let final_price = @azimuth.add(discounted_price, shipping_fee)
  
  @azimuth.assert_eq(850, final_price)  // 1000 - 200 + 50 = 850
}

// 测试用例 10: 测试数学公式验证
test "mathematical_formula_validation" {
  // 验证平方差公式：(a+b)(a-b) = a² - b²
  let a = 13
  let b = 5
  
  let left = @azimuth.multiply(@azimuth.add(a, b), @azimuth.add(a, -b))
  let right = @azimuth.add(@azimuth.multiply(a, a), @azimuth.multiply(-b, b))
  
  @azimuth.assert_eq(left, right)
}