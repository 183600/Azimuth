// Azimuth 核心功能测试用例
// 使用标准 MoonBit 测试语法

use azimuth

// 测试基本加法功能
test "core_add_functionality" {
  // 测试正数相加
  @assertion.assert_eq(10, azimuth.add(3, 7))
  
  // 测试负数相加
  @assertion.assert_eq(-5, azimuth.add(-2, -3))
  
  // 测试正负数相加
  @assertion.assert_eq(0, azimuth.add(5, -5))
  
  // 测试与零相加
  @assertion.assert_eq(42, azimuth.add(0, 42))
  @assertion.assert_eq(42, azimuth.add(42, 0))
}

// 测试基本乘法功能
test "core_multiply_functionality" {
  // 测试正数相乘
  @assertion.assert_eq(12, azimuth.multiply(3, 4))
  
  // 测试负数相乘
  @assertion.assert_eq(15, azimuth.multiply(-3, -5))
  
  // 测试正负数相乘
  @assertion.assert_eq(-20, azimuth.multiply(4, -5))
  
  // 测试与零相乘
  @assertion.assert_eq(0, azimuth.multiply(0, 123))
  @assertion.assert_eq(0, azimuth.multiply(123, 0))
  
  // 测试与1相乘
  @assertion.assert_eq(42, azimuth.multiply(42, 1))
  @assertion.assert_eq(42, azimuth.multiply(1, 42))
}

// 测试字符串拼接功能
test "core_greet_functionality" {
  // 测试基本问候
  @assertion.assert_eq_string("Hello, World!", azimuth.greet("World"))
  
  // 测试空字符串
  @assertion.assert_eq_string("Hello, !", azimuth.greet(""))
  
  // 测试包含空格的字符串
  @assertion.assert_eq_string("Hello, John Doe!", azimuth.greet("John Doe"))
  
  // 测试包含特殊字符的字符串
  @assertion.assert_eq_string("Hello, test@123!", azimuth.greet("test@123"))
}

// 测试边界值处理
test "core_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  @assertion.assert_eq(max_val, azimuth.add(max_val, 1))
  @assertion.assert_eq(max_val, azimuth.add(max_val, 100))
  @assertion.assert_eq(max_val, azimuth.multiply(max_val, 2))
  
  // 测试最小值边界
  @assertion.assert_eq(min_val, azimuth.add(min_val, -1))
  @assertion.assert_eq(min_val, azimuth.add(min_val, -100))
  @assertion.assert_eq(min_val, azimuth.multiply(min_val, -1))
  
  // 测试最大值与最小值相加
  @assertion.assert_eq(-1, azimuth.add(max_val, min_val))
}

// 测试数学运算性质
test "core_mathematical_properties" {
  let a = 7
  let b = 11
  let c = 5
  
  // 测试加法交换律
  @assertion.assert_eq(azimuth.add(a, b), azimuth.add(b, a))
  
  // 测试乘法交换律
  @assertion.assert_eq(azimuth.multiply(a, b), azimuth.multiply(b, a))
  
  // 测试分配律
  @assertion.assert_eq(azimuth.multiply(a, azimuth.add(b, c)), azimuth.add(azimuth.multiply(a, b), azimuth.multiply(a, c)))
  
  // 测试结合律
  @assertion.assert_eq(azimuth.add(azimuth.add(a, b), c), azimuth.add(a, azimuth.add(b, c)))
  @assertion.assert_eq(azimuth.multiply(azimuth.multiply(a, b), c), azimuth.multiply(a, azimuth.multiply(b, c)))
}

// 测试实际应用场景
test "core_real_world_scenarios" {
  // 场景1：计算购物车总价
  let item_price = 199
  let quantity = 3
  let shipping = 20
  let cart_total = azimuth.add(azimuth.multiply(item_price, quantity), shipping)
  @assertion.assert_eq(617, cart_total)
  
  // 场景2：计算矩形周长
  let length = 10
  let width = 5
  let perimeter = azimuth.add(azimuth.multiply(2, length), azimuth.multiply(2, width))
  @assertion.assert_eq(30, perimeter)
  
  // 场景3：温度转换（简化版：摄氏度转华氏度）
  let celsius = 25
  let fahrenheit = azimuth.add(azimuth.multiply(celsius, 2), 30)
  @assertion.assert_eq(80, fahrenheit)
}

// 测试错误处理和特殊情况
test "core_error_handling" {
  // 测试断言函数
  @assertion.assert_true(true)
  @assertion.assert_false(false)
  @assertion.assert_eq(100, 100)
  @assertion.assert_eq_string("test", "test")
  
  // 测试最小值的特殊乘法情况
  let min_val = -2147483648
  @assertion.assert_eq(min_val, azimuth.multiply(min_val, -1))
  @assertion.assert_eq(min_val, azimuth.multiply(-1, min_val))
  
  // 测试溢出保护
  let max_val = 2147483647
  @assertion.assert_true(azimuth.add(max_val, 1) == max_val)
  @assertion.assert_true(azimuth.multiply(max_val, 2) == max_val)
}

// 测试复合运算
test "core_complex_operations" {
  // 计算 (a + b) * (c - d)
  let a = 10
  let b = 5
  let c = 20
  let d = 8
  
  let sum = azimuth.add(a, b)
  let diff = azimuth.add(c, -d)
  let result = azimuth.multiply(sum, diff)
  
  @assertion.assert_eq(180, result)
  
  // 计算 a * b + c * d
  let product1 = azimuth.multiply(a, b)
  let product2 = azimuth.multiply(c, d)
  let total = azimuth.add(product1, product2)
  
  @assertion.assert_eq(210, total)
}

// 测试性能相关的计算
test "core_performance_calculations" {
  // 计算等差数列的和：1 + 2 + ... + 100
  let n = 100
  let sum = azimuth.multiply(n, azimuth.add(n, 1)) / 2
  @assertion.assert_eq(5050, sum)
  
  // 计算阶乘：5! = 5 * 4 * 3 * 2 * 1
  let factorial = azimuth.multiply(5, azimuth.multiply(4, azimuth.multiply(3, azimuth.multiply(2, 1))))
  @assertion.assert_eq(120, factorial)
  
  // 计算2的幂：2^6 = 64
  let power_of_two = azimuth.multiply(azimuth.multiply(2, 2), azimuth.multiply(2, 2))
  @assertion.assert_eq(16, power_of_two)
}

// 测试字符串处理的高级场景
test "core_advanced_string_processing" {
  // 测试长字符串
  let long_name = "ThisIsAVeryLongNameForTestingPurposes"
  let result1 = azimuth.greet(long_name)
  @assertion.assert_eq_string("Hello, " + long_name + "!", result1)
  
  // 测试包含Unicode字符的字符串
  let unicode_name = "张三李四"
  let result2 = azimuth.greet(unicode_name)
  @assertion.assert_eq_string("Hello, 张三李四!", result2)
  
  // 测试包含数字和字母的混合字符串
  let mixed_name = "User2023"
  let result3 = azimuth.greet(mixed_name)
  @assertion.assert_eq_string("Hello, User2023!", result3)
}

// 测试除法向上取整功能
test "core_divide_with_ceil" {
  // 测试正常除法
  @assertion.assert_eq(3, azimuth.divide_with_ceil(10, 3))
  @assertion.assert_eq(4, azimuth.divide_with_ceil(11, 3))
  
  // 测试整除情况
  @assertion.assert_eq(5, azimuth.divide_with_ceil(10, 2))
  @assertion.assert_eq(7, azimuth.divide_with_ceil(14, 2))
  
  // 测试负数情况
  @assertion.assert_eq(-3, azimuth.divide_with_ceil(-10, 3))
  @assertion.assert_eq(-3, azimuth.divide_with_ceil(-11, 3))
  
  // 测试边界情况
  @assertion.assert_eq(1, azimuth.divide_with_ceil(1, 1))
  @assertion.assert_eq(0, azimuth.divide_with_ceil(0, 5))
}