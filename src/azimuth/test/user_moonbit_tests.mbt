// 用户自定义 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，测试项目核心功能
// 包含9个测试用例，覆盖不同场景

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正常加法
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  
  // 测试包含0的加法
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(-17, @azimuth.add(-17, 0))
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
}

test "multiply_function_special_cases" {
  // 测试乘法函数的特殊情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正常乘法
  @azimuth.assert_eq(36, @azimuth.multiply(6, 6))
  
  // 测试包含0的乘法
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
  @azimuth.assert_eq(0, @azimuth.multiply(-456, 0))
  
  // 测试包含1的乘法
  @azimuth.assert_eq(789, @azimuth.multiply(1, 789))
  @azimuth.assert_eq(-321, @azimuth.multiply(-321, 1))
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "greet_function_variations" {
  // 测试greet函数的各种变体
  // 测试普通字符串
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  
  // 测试空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试包含数字的字符串
  @azimuth.assert_eq_string("Hello, User123!", @azimuth.greet("User123"))
  
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, test@domain.com!", @azimuth.greet("test@domain.com"))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Smith!", @azimuth.greet("John Smith"))
}

test "mathematical_associativity" {
  // 测试数学结合律
  let a = 5
  let b = 10
  let c = 15
  
  // 加法结合律: (a + b) + c = a + (b + c)
  let left_assoc = @azimuth.add(@azimuth.add(a, b), c)
  let right_assoc = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(left_assoc, right_assoc)
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  let left_mul_assoc = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let right_mul_assoc = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(left_mul_assoc, right_mul_assoc)
}

test "distributive_property" {
  // 测试分配律: a * (b + c) = a * b + a * c
  let a = 7
  let b = 8
  let c = 9
  
  // a * (b + c)
  let sum = @azimuth.add(b, c)
  let left_side = @azimuth.multiply(a, sum)
  
  // a * b + a * c
  let product1 = @azimuth.multiply(a, b)
  let product2 = @azimuth.multiply(a, c)
  let right_side = @azimuth.add(product1, product2)
  
  @azimuth.assert_eq(left_side, right_side)
}

test "financial_calculation_scenario" {
  // 测试金融计算场景：简单利息计算
  let principal = 10000  // 本金
  let rate = 5           // 年利率（百分比）
  let time = 3           // 时间（年）
  
  // 简单利息：本金 × 利率 × 时间
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, time))
  // 总金额：本金 + 利息
  let total_amount = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(160000, total_amount)  // 10000 + (10000 × 5 × 3) = 10000 + 150000 = 160000
}

test "temperature_conversion" {
  // 测试温度转换场景：摄氏度到华氏度
  // 公式：F = C × 9/5 + 32
  // 由于整数除法，我们使用近似值：F = C × 2 + 32 - C/5
  let celsius = 25
  
  let step1 = @azimuth.multiply(celsius, 9)
  let step2 = @azimuth.multiply(step1, 1)  // 保持整数运算
  let fahrenheit = @azimuth.add(@azimuth.multiply(step2, 1), @azimuth.multiply(32, 5))
  
  // 简化版本：F ≈ C × 2 + 30
  let approx_fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  
  @azimuth.assert_eq(80, approx_fahrenheit)  // 25 × 2 + 30 = 80
}

test "geometric_calculations" {
  // 测试几何计算：正方形和立方体
  let side = 4
  
  // 正方形面积 = side × side
  let square_area = @azimuth.multiply(side, side)
  @azimuth.assert_eq(16, square_area)
  
  // 正方形周长 = 4 × side
  let square_perimeter = @azimuth.multiply(4, side)
  @azimuth.assert_eq(16, square_perimeter)
  
  // 立方体体积 = side × side × side
  let cube_volume = @azimuth.multiply(@azimuth.multiply(side, side), side)
  @azimuth.assert_eq(64, cube_volume)
  
  // 立方体表面积 = 6 × side × side
  let cube_surface_area = @azimuth.multiply(6, @azimuth.multiply(side, side))
  @azimuth.assert_eq(96, cube_surface_area)
}

test "boolean_assertions_with_logic" {
  // 测试布尔断言和逻辑运算
  let x = 10
  let y = 20
  let z = 30
  
  // 测试比较结果
  @azimuth.assert_true(x < y)
  @azimuth.assert_true(y <= z)
  @azimuth.assert_false(x > y)
  @azimuth.assert_false(z < x)
  
  // 测试数学运算结果
  let sum = @azimuth.add(x, y)
  @azimuth.assert_true(sum == z)
  
  let product = @azimuth.multiply(x, 2)
  @azimuth.assert_true(product == x)
  
  // 测试边界条件
  let max_val = 2147483647
  let overflow_result = @azimuth.add(max_val, 100)
  @azimuth.assert_true(overflow_result == max_val)
}