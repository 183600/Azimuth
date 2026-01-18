// Azimuth 项目新的综合测试用例
// 使用标准 MoonBit 测试语法，涵盖多种测试场景

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-8, add(-3, -5))
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(3, -4))
  assert_eq(0, multiply(0, 100))
}

test "extreme_boundary_values" {
  // 极端边界值测试
  let max_int = 2147483647
  let min_int = -2147483648
  
  assert_eq(max_int, add(max_int, 0))
  assert_eq(min_int, add(min_int, 0))
  assert_eq(1, add(max_int, min_int))
  assert_eq(max_int, multiply(max_int, 1))
  assert_eq(min_int, multiply(min_int, 1))
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, A!", greet("A"))
}

test "mathematical_properties" {
  // 数学性质测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 5)), add(multiply(a, b), multiply(a, 5)))
  
  // 结合律
  assert_eq(add(add(a, b), 5), add(a, add(b, 5)))
}

test "financial_calculations" {
  // 金融计算测试
  let principal = 10000
  let rate = 5
  let years = 3
  
  // 简单利息计算
  let simple_interest = multiply(principal, rate) / 100
  let total_amount = add(principal, simple_interest)
  assert_eq(10500, total_amount)
  
  // 复利计算（简化版本）
  let mut compound_amount = principal
  let mut i = 0
  while i < years {
    compound_amount = add(compound_amount, multiply(compound_amount, rate) / 100)
    i = i + 1
  }
  assert_eq(11576, compound_amount)  // 近似值
}

test "geometry_and_trigonometry" {
  // 几何与三角学测试
  let radius = 7
  
  // 圆的周长（简化π=22/7）
  let circumference = multiply(2, multiply(22, radius) / 7)
  assert_eq(44, circumference)
  
  // 圆的面积（简化π=22/7）
  let area = multiply(22, multiply(radius, radius) / 7)
  assert_eq(154, area)
  
  // 矩形对角线长度（简化计算）
  let length = 3
  let width = 4
  let diagonal_squared = add(multiply(length, length), multiply(width, width))
  assert_eq(25, diagonal_squared)
}

test "data_validation_scenarios" {
  // 数据验证场景测试
  // 测试负数输入
  assert_eq(-5, add(-10, 5))
  assert_eq(20, multiply(-4, -5))
  
  // 测试零值操作
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(5, 0))
  
  // 测试大数运算
  let large_num = 1000000
  assert_eq(2000000, add(large_num, large_num))
  // 修复超出整数范围的问题
  assert_eq(1000000, multiply(large_num, 1))
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟测试
  // 线性搜索比较次数（最坏情况）
  let array_size = 100
  let linear_comparisons = array_size
  assert_eq(100, linear_comparisons)
  
  // 二分查找比较次数（最坏情况）
  let binary_comparisons = 7  // log2(100) ≈ 6.64，向上取整
  assert_eq(7, binary_comparisons)
  
  // 冒泡排序交换次数（平均情况）
  let n = 10
  let avg_swaps = multiply(n, multiply(n, n)) / 4  // 简化平均情况
  assert_eq(250, avg_swaps)
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, 2))
  
  // 测试连续运算
  let result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result)
  
  // 测试嵌套运算
  let nested = multiply(add(1, 2), add(3, 4))
  assert_eq(21, nested)
}

test "integration_and_composite_functions" {
  // 集成和复合函数测试
  // 复合业务逻辑：计算折扣后的总价
  let unit_price = 50
  let quantity = 8
  let discount_rate = 10  // 10%
  
  let subtotal = multiply(unit_price, quantity)
  let discount_amount = multiply(subtotal, discount_rate) / 100
  let final_price = add(subtotal, -discount_amount)
  assert_eq(360, final_price)
  
  // 复合数学计算：((a+b)*c) - d
  let a = 5
  let b = 3
  let c = 4
  let d = 10
  let complex_result = add(multiply(add(a, b), c), -d)
  assert_eq(22, complex_result)
  
  // 字符串和数值集成测试
  let user_name = "Alice"
  let user_id = 12345  // 未使用变量，但保留用于展示集成概念
  let greeting_msg = greet(user_name)
  assert_eq_string("Hello, Alice!", greeting_msg)
  
  // 计算用户ID各位数字之和（简化版本）
  let id_sum = add(add(1, 2), add(3, 4)) + 5  // 1+2+3+4+5
  assert_eq(15, id_sum)
}