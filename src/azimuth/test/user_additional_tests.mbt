// 用户添加的 MoonBit 测试用例
// 测试 Azimuth 项目的核心功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  
  @azimuth.assert_eq(42, @azimuth.multiply(6, 7))
  @azimuth.assert_eq(-24, @azimuth.multiply(-4, 6))
  @azimuth.assert_eq(30, @azimuth.multiply(-5, -6))
}

test "string_functionality_tests" {
  // 测试字符串功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 12
  let b = 18
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "boundary_value_tests" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 最小值测试
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 零值测试
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_val))
}

test "real_world_calculations" {
  // 测试实际应用计算
  // 计算矩形面积和周长
  let length = 10
  let width = 5
  
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(50, area)
  
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(30, perimeter)
}

test "complex_expression_evaluation" {
  // 测试复杂表达式计算
  // 计算 (a + b) * c - d
  let a = 8
  let b = 7
  let c = 3
  let d = 10
  
  let sum_ab = @azimuth.add(a, b)  // 15
  let product = @azimuth.multiply(sum_ab, c)  // 45
  let result = @azimuth.add(product, -d)  // 35
  
  @azimuth.assert_eq(35, result)
}

test "sequential_operations" {
  // 测试连续运算
  // 计算 1 + 2 + 3 + 4 + 5
  let sum1 = @azimuth.add(1, 2)  // 3
  let sum2 = @azimuth.add(sum1, 3)  // 6
  let sum3 = @azimuth.add(sum2, 4)  // 10
  let sum4 = @azimuth.add(sum3, 5)  // 15
  
  @azimuth.assert_eq(15, sum4)
  
  // 计算 2 * 3 * 4
  let product1 = @azimuth.multiply(2, 3)  // 6
  let product2 = @azimuth.multiply(product1, 4)  // 24
  
  @azimuth.assert_eq(24, product2)
}

test "special_case_handling" {
  // 测试特殊情况处理
  // 测试与0和1的运算
  let test_value = 42
  
  @azimuth.assert_eq(test_value, @azimuth.add(test_value, 0))
  @azimuth.assert_eq(test_value, @azimuth.add(0, test_value))
  @azimuth.assert_eq(test_value, @azimuth.multiply(test_value, 1))
  @azimuth.assert_eq(test_value, @azimuth.multiply(1, test_value))
  @azimuth.assert_eq(0, @azimuth.multiply(test_value, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, test_value))
  
  // 测试负数运算
  @azimuth.assert_eq(-test_value, @azimuth.multiply(test_value, -1))
  @azimuth.assert_eq(-test_value, @azimuth.multiply(-1, test_value))
}

test "error_prevention_tests" {
  // 测试错误预防机制
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_true(@azimuth.add(max_val, 1000) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -1000) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 1000) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, 1000) == min_val)
  
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(100, 100)
  @azimuth.assert_eq_string("test", "test")
}

test "practical_application_scenarios" {
  // 测试实际应用场景
  // 场景1：计算购物车总价
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 99
  let item2_quantity = 3
  
  let total1 = @azimuth.multiply(item1_price, item1_quantity)  // 398
  let total2 = @azimuth.multiply(item2_price, item2_quantity)  // 297
  let cart_total = @azimuth.add(total1, total2)  // 695
  
  @azimuth.assert_eq(695, cart_total)
  
  // 场景2：温度转换（简化版）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)  // 25 * 2 + 30 = 80
  
  @azimuth.assert_eq(80, fahrenheit)
}