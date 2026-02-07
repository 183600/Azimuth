// 标准 MoonBit 测试用例
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  @azimuth.assert_eq(8, @azimuth.add(5, 3))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(7, @azimuth.subtract(10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
}

test "negative_number_operations" {
  // 负数运算测试
  @azimuth.assert_eq(-2, @azimuth.add(-5, 3))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(-8, @azimuth.subtract(-5, 3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-5, 3))
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "boundary_value_testing" {
  // 边界值测试
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(5, @azimuth.subtract(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
}

test "mathematical_properties" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 加法单位元
  @azimuth.assert_eq(@azimuth.add(a, 0), a)
  
  // 乘法单位元
  @azimuth.assert_eq(@azimuth.multiply(a, 1), a)
}

test "complex_calculation_scenario" {
  // 复杂计算场景测试
  let base = 100
  let multiplier = 3
  let offset = 25
  let divisor = 5
  
  let step1 = @azimuth.multiply(base, multiplier)
  let step2 = @azimuth.add(step1, offset)
  let step3 = @azimuth.divide_with_ceil(step2, divisor)
  
  @azimuth.assert_eq(300, step1)
  @azimuth.assert_eq(325, step2)
  @azimuth.assert_eq(65, step3)
}

test "business_packaging_calculation" {
  // 业务包装计算测试
  let total_items = 35
  let items_per_package = 8
  let packages_needed = @azimuth.divide_with_ceil(total_items, items_per_package)
  
  @azimuth.assert_eq(5, packages_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(packages_needed, items_per_package)
  @azimuth.assert_eq(40, total_capacity)
  
  // 验证剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, total_items)
  @azimuth.assert_eq(5, remaining_space)
}

test "financial_compound_interest" {
  // 金融复利计算测试
  let principal = 1000
  let rate = 10
  
  // 第一年
  let year1_interest = @azimuth.divide_with_ceil(@azimuth.multiply(principal, rate), 100)
  let year1_total = @azimuth.add(principal, year1_interest)
  
  // 第二年
  let year2_interest = @azimuth.divide_with_ceil(@azimuth.multiply(year1_total, rate), 100)
  let year2_total = @azimuth.add(year1_total, year2_interest)
  
  @azimuth.assert_eq(1100, year1_total)
  @azimuth.assert_eq(1210, year2_total)
}

test "fibonacci_sequence" {
  // 斐波那契数列计算测试
  let f0 = 0
  let f1 = 1
  let f2 = @azimuth.add(f1, f0)
  let f3 = @azimuth.add(f2, f1)
  let f4 = @azimuth.add(f3, f2)
  let f5 = @azimuth.add(f4, f3)
  let f6 = @azimuth.add(f5, f4)
  
  @azimuth.assert_eq(1, f2)
  @azimuth.assert_eq(2, f3)
  @azimuth.assert_eq(3, f4)
  @azimuth.assert_eq(5, f5)
  @azimuth.assert_eq(8, f6)
}

test "temperature_conversion" {
  // 温度转换测试
  let celsius = 25
  
  // 摄氏度转华氏度：F = C × 9/5 + 32
  let fahrenheit_step1 = @azimuth.multiply(celsius, 9)
  let fahrenheit_step2 = @azimuth.divide_with_ceil(fahrenheit_step1, 5)
  let fahrenheit = @azimuth.add(fahrenheit_step2, 32)
  
  // 验证转换结果
  @azimuth.assert_eq(77, fahrenheit)
  
  // 华氏度转摄氏度（简化）：C = (F - 32) × 5/9
  let celsius_back_step1 = @azimuth.subtract(fahrenheit, 32)
  let celsius_back_step2 = @azimuth.multiply(celsius_back_step1, 5)
  let celsius_back = @azimuth.divide_with_ceil(celsius_back_step2, 9)
  
  @azimuth.assert_eq(25, celsius_back)
}