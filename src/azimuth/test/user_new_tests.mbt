// 用户新增的 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，覆盖不同功能场景

test "addition_with_positive_numbers" {
  // 测试正数相加
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "addition_with_mixed_numbers" {
  // 测试正负数混合相加
  @azimuth.assert_eq(3, @azimuth.add(10, -7))
  @azimuth.assert_eq(-5, @azimuth.add(-10, 5))
  @azimuth.assert_eq(0, @azimuth.add(-25, 25))
}

test "multiplication_by_zero_and_one" {
  // 测试乘以0和1的情况
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(-9876, 0))
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(-17, @azimuth.multiply(-17, 1))
}

test "multiplication_with_negative_numbers" {
  // 测试负数相乘
  @azimuth.assert_eq(36, @azimuth.multiply(-6, -6))
  @azimuth.assert_eq(-45, @azimuth.multiply(9, -5))
  @azimuth.assert_eq(-72, @azimuth.multiply(-8, 9))
}

test "greet_function_with_different_inputs" {
  // 测试greet函数处理不同输入
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形的面积和周长
  let length = 8
  let width = 5
  
  // 计算面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(40, area)
  
  // 计算周长
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(26, perimeter)
}

test "boundary_value_operations" {
  // 测试边界值运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试边界值相加
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 测试边界值相乘
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "assertion_functions_test" {
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}

test "real_world_shopping_cart" {
  // 测试实际应用场景：购物车计算
  let item_price = 199
  let quantity = 3
  let discount = 50
  let shipping = 15
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  // 应用折扣
  let discounted_total = @azimuth.add(subtotal, -discount)
  // 加上运费
  let final_total = @azimuth.add(discounted_total, shipping)
  
  @azimuth.assert_eq(602, final_total)  // 199*3 - 50 + 15 = 597 - 50 + 15 = 562
}

test "temperature_conversion" {
  // 测试温度转换场景（简化版）
  // 摄氏度转华氏度：F = C * 2 + 30（简化公式）
  let celsius = 20
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(70, fahrenheit)
  
  // 测试冰点
  let freezing = @azimuth.add(@azimuth.multiply(0, 2), 30)
  @azimuth.assert_eq(30, freezing)
  
  // 测试沸点
  let boiling = @azimuth.add(@azimuth.multiply(100, 2), 30)
  @azimuth.assert_eq(230, boiling)
}