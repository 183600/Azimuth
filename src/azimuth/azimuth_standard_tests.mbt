// Azimuth 标准 MoonBit 测试用例
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(8, add(5, 3))
  assert_eq(15, multiply(3, 5))
  assert_eq(7, subtract(10, 3))
  assert_eq(4, divide_with_ceil(10, 3))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-2, add(-5, 3))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(-8, subtract(-5, 3))
  assert_eq(-1, divide_with_ceil(-5, 3))
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "boundary_value_testing" {
  // 边界值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(5, subtract(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "mathematical_properties" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法单位元
  assert_eq(add(a, 0), a)
  
  // 乘法单位元
  assert_eq(multiply(a, 1), a)
}

test "complex_calculation_scenario" {
  // 复杂计算场景测试
  let base = 100
  let multiplier = 3
  let offset = 25
  let divisor = 5
  
  let step1 = multiply(base, multiplier)
  let step2 = add(step1, offset)
  let step3 = divide_with_ceil(step2, divisor)
  
  assert_eq(300, step1)
  assert_eq(325, step2)
  assert_eq(65, step3)
}

test "business_packaging_calculation" {
  // 业务包装计算测试
  let total_items = 35
  let items_per_package = 8
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  assert_eq(5, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  assert_eq(40, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(5, remaining_space)
}

test "financial_compound_interest" {
  // 金融复利计算测试
  let principal = 1000
  let rate = 10
  
  // 第一年
  let year1_interest = divide_with_ceil(multiply(principal, rate), 100)
  let year1_total = add(principal, year1_interest)
  
  // 第二年
  let year2_interest = divide_with_ceil(multiply(year1_total, rate), 100)
  let year2_total = add(year1_total, year2_interest)
  
  assert_eq(1100, year1_total)
  assert_eq(1210, year2_total)
}

test "fibonacci_sequence" {
  // 斐波那契数列计算测试
  let f0 = 0
  let f1 = 1
  let f2 = add(f1, f0)
  let f3 = add(f2, f1)
  let f4 = add(f3, f2)
  let f5 = add(f4, f3)
  let f6 = add(f5, f4)
  
  assert_eq(1, f2)
  assert_eq(2, f3)
  assert_eq(3, f4)
  assert_eq(5, f5)
  assert_eq(8, f6)
}

test "temperature_conversion" {
  // 温度转换测试
  let celsius = 25
  
  // 摄氏度转华氏度：F = C × 9/5 + 32
  let fahrenheit_step1 = multiply(celsius, 9)
  let fahrenheit_step2 = divide_with_ceil(fahrenheit_step1, 5)
  let fahrenheit = add(fahrenheit_step2, 32)
  
  // 验证转换结果
  assert_eq(77, fahrenheit)
  
  // 华氏度转摄氏度（简化）：C = (F - 32) × 5/9
  let celsius_back_step1 = subtract(fahrenheit, 32)
  let celsius_back_step2 = multiply(celsius_back_step1, 5)
  let celsius_back = divide_with_ceil(celsius_back_step2, 9)
  
  assert_eq(25, celsius_back)
}