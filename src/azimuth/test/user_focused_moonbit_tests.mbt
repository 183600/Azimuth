// 用户聚焦的 MoonBit 测试用例
// 测试项目的核心功能和边界情况

test "divide_with_ceil_basic" {
  // 测试基本的向上取整除法
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 4))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
}

test "divide_with_ceil_negative" {
  // 测试负数的向上取整除法
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-7, 4))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-4, 5))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 4))
}

test "add_boundary_values" {
  // 测试边界值的加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 测试最小值加法
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 测试最大值和最小值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "multiply_boundary_values" {
  // 测试边界值的乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值乘法
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 测试最小值乘法
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 测试零乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

test "greet_special_characters" {
  // 测试包含特殊字符的字符串处理
  @azimuth.assert_eq_string("Hello, @#$%^&*()!", @azimuth.greet("@#$%^&*()"))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, \n\t!", @azimuth.greet("\n\t"))
}

test "mathematical_associativity" {
  // 测试数学运算的结合律
  let a = 10
  let b = 20
  let c = 30
  
  // 加法结合律：(a + b) + c = a + (b + c)
  let result1 = @azimuth.add(@azimuth.add(a, b), c)
  let result2 = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(result1, result2)
  
  // 乘法结合律：(a * b) * c = a * (b * c)
  let result3 = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let result4 = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(result3, result4)
}

test "shopping_cart_calculation" {
  // 测试实际应用场景：购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let discount = 50
  let shipping = 15
  
  // 计算商品总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(total1, total2)
  
  // 应用折扣和运费
  let final_total = @azimuth.add(@azimuth.add(subtotal, -discount), shipping)
  
  // 验证结果：(199*2) + (299*1) - 50 + 15 = 398 + 299 - 50 + 15 = 662
  @azimuth.assert_eq(662, final_total)
}

test "error_handling_scenarios" {
  // 测试错误处理场景
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试溢出保护
  @azimuth.assert_true(@azimuth.add(max_val, 100) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -100) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, -2) == min_val)
}

test "complex_mathematical_calculation" {
  // 测试复杂数学计算：计算多项式值
  // P(x) = 3x³ + 2x² - x + 5，当 x = 4
  let x = 4
  
  // 计算 x² 和 x³
  let x_squared = @azimuth.multiply(x, x)
  let x_cubed = @azimuth.multiply(x_squared, x)
  
  // 计算多项式各项
  let term1 = @azimuth.multiply(3, x_cubed)
  let term2 = @azimuth.multiply(2, x_squared)
  let term3 = @azimuth.multiply(-1, x)
  let term4 = 5
  
  // 计算多项式值
  let polynomial_value = @azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4)
  
  // 验证结果：3*64 + 2*16 - 4 + 5 = 192 + 32 - 4 + 5 = 225
  @azimuth.assert_eq(225, polynomial_value)
}

test "performance_related_scenarios" {
  // 测试性能相关场景：批量计算
  let base = 100
  let count = 50
  
  // 计算 1 + 2 + 3 + ... + count 的和
  // 使用公式：count * (count + 1) / 2
  let sum = @azimuth.multiply(count, @azimuth.add(count, 1)) / 2
  
  // 计算总和：base * sum
  let total = @azimuth.multiply(base, sum)
  
  // 验证结果：100 * (50 * 51 / 2) = 100 * 1275 = 127500
  @azimuth.assert_eq(127500, total)
  
  // 测试连续乘法：计算 2^10
  let power_of_2 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(2, 2)), @azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(2, 2)))
  @azimuth.assert_eq(256, power_of_2)
}