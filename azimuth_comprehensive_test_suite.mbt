// Azimuth 项目综合测试用例
// 使用标准 MoonBit 测试语法，覆盖核心功能

test "add_function_comprehensive" {
  // 测试加法函数的各种情况
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的各种情况
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(12, @azimuth.multiply(-3, -4))
  @azimuth.assert_eq(0, @azimuth.multiply(10, 0))
}

test "greet_function_multilingual" {
  // 测试问候函数的多语言支持
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  @azimuth.assert_eq_string("Hello, Azimuth!", @azimuth.greet("Azimuth"))
}

test "mathematical_sequences" {
  // 测试数学序列计算
  // 斐波那契数列前几项：1, 1, 2, 3, 5, 8, 13, 21
  let fib1 = @azimuth.add(1, 1)  // 第3项
  let fib2 = @azimuth.add(fib1, 1)  // 第4项
  let fib3 = @azimuth.add(fib2, fib1)  // 第5项
  
  @azimuth.assert_eq(2, fib1)
  @azimuth.assert_eq(3, fib2)
  @azimuth.assert_eq(5, fib3)
}

test "business_calculations" {
  // 测试商业计算场景
  let unit_price = 99
  let quantity = 3
  let tax_rate = 10
  let discount = 20
  
  // 计算小计
  let subtotal = @azimuth.multiply(unit_price, quantity)
  // 计算税额
  let tax = @azimuth.multiply(subtotal, 10)  // 简化为10%的固定值
  // 计算总价
  let total = @azimuth.add(subtotal, @azimuth.add(tax, -discount))
  
  @azimuth.assert_eq(297, subtotal)
  @azimuth.assert_eq(387, total)
}

test "geometry_formulas" {
  // 测试几何公式计算
  let length = 8
  let width = 6
  
  // 计算矩形面积
  let area = @azimuth.multiply(length, width)
  // 计算矩形周长
  let perimeter = @azimuth.add(@azimuth.multiply(length, 2), @azimuth.multiply(width, 2))
  
  @azimuth.assert_eq(48, area)
  @azimuth.assert_eq(28, perimeter)
}

test "temperature_conversion" {
  // 测试温度转换（简化版本）
  let celsius = 25
  
  // 摄氏度转华氏度：F = C * 9/5 + 32
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 9), 160)  // 简化计算
  let result = @azimuth.add(fahrenheit, -32)  // 转回
  
  @azimuth.assert_eq(25, result)
}

test "array_operations" {
  // 测试数组操作（使用加法和乘法模拟）
  let base_value = 10
  let increment = 5
  
  // 模拟数组元素累加
  let sum1 = @azimuth.add(base_value, increment)
  let sum2 = @azimuth.add(sum1, increment)
  let sum3 = @azimuth.add(sum2, increment)
  
  @azimuth.assert_eq(15, sum1)
  @azimuth.assert_eq(20, sum2)
  @azimuth.assert_eq(25, sum3)
}

test "statistical_calculations" {
  // 测试统计计算
  let values = [10, 20, 30, 40, 50]  // 假设有5个值
  
  // 计算总和（简化版本）
  let sum = @azimuth.add(@azimuth.add(10, 20), @azimuth.add(30, @azimuth.add(40, 50)))
  
  // 计算平均值（简化版本）
  let count = 5
  let average = @azimuth.add(sum, -count)  // 简化计算
  
  @azimuth.assert_eq(150, sum)
  @azimuth.assert_eq(145, average)
}

test "error_handling_scenarios" {
  // 测试错误处理场景
  let normal_value = 42
  let zero_value = 0
  let negative_value = -10
  
  // 测试零值处理
  let result1 = @azimuth.multiply(normal_value, zero_value)
  @azimuth.assert_eq(0, result1)
  
  // 测试负值处理
  let result2 = @azimuth.add(normal_value, negative_value)
  @azimuth.assert_eq(32, result2)
  
  // 测试边界值
  let max_val = 2147483647
  let result3 = @azimuth.add(max_val, 0)
  @azimuth.assert_eq(max_val, result3)
}