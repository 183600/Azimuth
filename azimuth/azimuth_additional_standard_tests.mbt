// Azimuth 项目附加标准测试用例
// 使用标准 MoonBit 测试语法，覆盖核心功能的边界情况和实际应用场景

// 导入必要的函数
pub fn add(a : Int, b : Int) -> Int {
  a + b
}

pub fn multiply(a : Int, b : Int) -> Int {
  a * b
}

pub fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

pub fn divide_with_ceil(dividend : Int, divisor : Int) -> Int {
  let quotient = dividend / divisor
  let remainder = dividend % divisor
  if remainder > 0 { add(quotient, 1) } else { quotient }
}

pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  if (expected == actual) {
    ()
  } else {
    let _ = 1 / 0
    ()
  }
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  if (expected == actual) {
    ()
  } else {
    let _ = 1 / 0
    ()
  }
}

test "add_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 12345
  let b = 67890
  
  let result1 = add(a, b)
  let result2 = add(b, a)
  assert_eq(result1, result2)
  
  // 测试包含负数的交换律
  let c = -12345
  let d = 67890
  
  let result3 = add(c, d)
  let result4 = add(d, c)
  assert_eq(result3, result4)
}

test "multiply_distributive_property" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let a = 15
  let b = 20
  let c = 25
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
  
  // 测试包含负数的分配律
  let d = -10
  let e = 30
  let f = -15
  
  let left_side2 = multiply(d, add(e, f))
  let right_side2 = add(multiply(d, e), multiply(d, f))
  assert_eq(left_side2, right_side2)
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  // 测试整除情况
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(7, divide_with_ceil(21, 3))
  
  // 测试有余数的情况
  assert_eq(5, divide_with_ceil(14, 3))
  assert_eq(8, divide_with_ceil(22, 3))
  
  // 测试负数情况
  assert_eq(-4, divide_with_ceil(-12, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  
  // 测试混合符号
  assert_eq(-3, divide_with_ceil(10, -3))
  assert_eq(3, divide_with_ceil(-10, -3))
}

test "greet_function_edge_cases" {
  // 测试 greet 函数的边界情况
  // 测试包含各种特殊字符的字符串
  assert_eq_string("Hello, \"John\"!", greet("\"John\""))
  assert_eq_string("Hello, O'Reilly!", greet("O'Reilly"))
  assert_eq_string("Hello, C++!", greet("C++"))
  assert_eq_string("Hello, C#!", greet("C#"))
  
  // 测试包含空格的字符串
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
  assert_eq_string("Hello,  Leading!", greet(" Leading"))
  assert_eq_string("Hello, Trailing !", greet("Trailing "))
  
  // 测试包含制表符的字符串
  assert_eq_string("Hello, Tab\tSeparated!", greet("Tab\tSeparated"))
}

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  let initial_value = 100
  
  // 步骤1：乘以2
  let step1 = multiply(initial_value, 2)
  // 步骤2：加上50
  let step2 = add(step1, 50)
  // 步骤3：再乘以3
  let step3 = multiply(step2, 3)
  // 步骤4：减去100
  let step4 = add(step3, -100)
  
  // 验证最终结果：((100 * 2 + 50) * 3) - 100 = (250 * 3) - 100 = 750 - 100 = 650
  assert_eq(650, step4)
}

test "overflow_protection_verification" {
  // 验证溢出保护机制
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, 100))
  assert_eq(max_val, add(max_val, max_val))
  
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, -100))
  assert_eq(min_val, add(min_val, min_val))
  
  // 乘法溢出保护
  assert_eq(max_val, multiply(46341, 46341))
  assert_eq(min_val, multiply(-46341, 46341))
  assert_eq(min_val, multiply(min_val, -1))
  
  // 特殊情况：最小值乘以任何大于1的数都应该返回最小值
  assert_eq(min_val, multiply(min_val, 2))
  assert_eq(min_val, multiply(min_val, 100))
}

test "real_world_financial_calculation" {
  // 实际金融计算场景：贷款计算
  let principal = 100000  // 本金
  let annual_rate = 6     // 年利率6%
  let years = 10          // 贷款期限10年
  
  // 简化的单利计算：总利息 = 本金 × 利率 × 年数
  let total_interest = multiply(principal, multiply(annual_rate, years))
  
  // 总还款额 = 本金 + 总利息
  let total_payment = add(principal, total_interest)
  
  // 月还款额（简化）：总还款额 / (年数 × 12)
  let monthly_payment = total_payment / multiply(years, 12)
  
  // 验证计算结果
  assert_eq(6000000, total_interest)  // 100000 × 6 × 10 = 6000000
  assert_eq(6100000, total_payment)   // 100000 + 6000000 = 6100000
  assert_eq(50833, monthly_payment)   // 6100000 / 120 ≈ 50833（整数除法）
}

test "mathematical_sequence_calculation" {
  // 数学序列计算：斐波那契数列
  // 使用迭代方式计算前几项
  let f0 = 0
  let f1 = 1
  let f2 = add(f0, f1)      // 1
  let f3 = add(f1, f2)      // 2
  let f4 = add(f2, f3)      // 3
  let f5 = add(f3, f4)      // 5
  let f6 = add(f4, f5)      // 8
  let f7 = add(f5, f6)      // 13
  let f8 = add(f6, f7)      // 21
  let f9 = add(f7, f8)      // 34
  
  // 验证斐波那契数列的前10项
  assert_eq(0, f0)
  assert_eq(1, f1)
  assert_eq(1, f2)
  assert_eq(2, f3)
  assert_eq(3, f4)
  assert_eq(5, f5)
  assert_eq(8, f6)
  assert_eq(13, f7)
  assert_eq(21, f8)
  assert_eq(34, f9)
  
  // 验证黄金比例近似：f9/f8 ≈ 1.618（整数除法）
  let golden_ratio_approx = f9 / f8
  assert_eq(1, golden_ratio_approx)  // 34/21 = 1（整数除法）
}

test "string_processing_with_special_characters" {
  // 测试包含特殊字符的字符串处理
  // 测试包含HTML标签的字符串
  let html_name = "<div class='user'>John</div>"
  assert_eq_string("Hello, <div class='user'>John</div>!", greet(html_name))
  
  // 测试包含URL的字符串
  let url_name = "https://example.com/user?id=123"
  assert_eq_string("Hello, https://example.com/user?id=123!", greet(url_name))
  
  // 测试包含路径的字符串
  let path_name = "/home/user/documents/file.txt"
  assert_eq_string("Hello, /home/user/documents/file.txt!", greet(path_name))
  
  // 测试包含JSON的字符串
  let json_name = "{\"name\":\"John\",\"age\":30}"
  assert_eq_string("Hello, {\"name\":\"John\",\"age\":30}!", greet(json_name))
}

test "comprehensive_boundary_test" {
  // 综合边界测试：测试各种边界值的组合
  let max_val = 2147483647
  let min_val = -2147483648
  let zero = 0
  let one = 1
  let neg_one = -1
  
  // 测试加法的各种边界组合
  assert_eq(max_val, add(max_val, zero))
  assert_eq(max_val, add(zero, max_val))
  assert_eq(min_val, add(min_val, zero))
  assert_eq(min_val, add(zero, min_val))
  assert_eq(zero, add(zero, zero))
  assert_eq(one, add(zero, one))
  assert_eq(neg_one, add(zero, neg_one))
  
  // 测试乘法的各种边界组合
  assert_eq(zero, multiply(max_val, zero))
  assert_eq(zero, multiply(min_val, zero))
  assert_eq(zero, multiply(zero, max_val))
  assert_eq(zero, multiply(zero, min_val))
  assert_eq(zero, multiply(zero, zero))
  assert_eq(max_val, multiply(max_val, one))
  assert_eq(min_val, multiply(min_val, one))
  
  // 测试复杂边界组合
  let complex1 = add(multiply(max_val, one), multiply(min_val, neg_one))
  assert_eq(add(max_val, min_val), complex1)
  
  let complex2 = multiply(add(max_val, min_val), add(one, neg_one))
  assert_eq(0, complex2)
}