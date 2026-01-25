// 标准 MoonBit 增强测试套件
// 为 azimuth 库提供10个高质量的标准测试用例

test "addition_associative_law" {
  // 测试加法结合律: (a + b) + c = a + (b + c)
  let a = 15
  let b = 27
  let c = 42
  
  let left_side = add(add(a, b), c)
  let right_side = add(a, add(b, c))
  
  assert_eq(left_side, right_side)  // (15 + 27) + 42 = 15 + (27 + 42) = 84
}

test "multiplication_distributive_law" {
  // 测试乘法分配律: a * (b + c) = a * b + a * c
  let a = 8
  let b = 12
  let c = 7
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)  // 8 * (12 + 7) = 8 * 12 + 8 * 7 = 152
}

test "negative_number_boundary_operations" {
  // 测试负数边界运算
  let max_negative = -2147483647
  let min_positive = 1
  
  // 测试负数加法
  assert_eq(-2147483646, add(max_negative, min_positive))
  
  // 测试负数乘法
  assert_eq(2147483647, multiply(max_negative, -1))
  
  // 测试负数除法向上取整
  assert_eq(-2147483647, divide_with_ceil(max_negative, 1))
  assert_eq(1, divide_with_ceil(max_negative, -2147483647))
}

test "large_number_stability_operations" {
  // 测试大数运算稳定性
  let large_num1 = 1000000
  let large_num2 = 999999
  let large_num3 = 500000
  
  // 测试大数加法稳定性
  let sum_result = add(large_num1, large_num2)
  assert_eq(1999999, sum_result)
  
  // 测试大数乘法稳定性
  let product_result = multiply(large_num3, 2)
  assert_eq(1000000, product_result)
  
  // 测试大数除法稳定性
  let divide_result = divide_with_ceil(sum_result, large_num3)
  assert_eq(4, divide_result)  // ceil(1999999/500000) = 4
}

test "string_processing_security_tests" {
  // 测试字符串处理安全性
  // 测试空字符串和边界情况
  assert_eq_string("Hello, !", greet(""))
  
  // 测试包含特殊字符的字符串
  assert_eq_string("Hello, \n\t\r!", greet("\n\t\r"))
  
  // 测试包含引号的字符串
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
  
  // 测试包含路径的字符串
  assert_eq_string("Hello, /path/to/file!", greet("/path/to/file"))
}

test "division_edge_cases_comprehensive" {
  // 全面测试除法边界情况
  // 测试被除数为0的情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, -5))
  assert_eq(0, divide_with_ceil(0, 1))
  
  // 测试被除数小于除数的情况
  assert_eq(1, divide_with_ceil(3, 10))
  assert_eq(1, divide_with_ceil(-3, -10))
  assert_eq(-1, divide_with_ceil(-3, 10))
  
  // 测试除数为1的情况
  assert_eq(100, divide_with_ceil(100, 1))
  assert_eq(-100, divide_with_ceil(-100, 1))
  
  // 测试除数为-1的情况
  assert_eq(-100, divide_with_ceil(100, -1))
  assert_eq(100, divide_with_ceil(-100, -1))
}

test "complex_function_composition" {
  // 测试复合函数计算
  let base_value = 10
  let multiplier = 3
  let divisor = 4
  let addition = 15
  
  // 复合计算: ((base * multiplier) / divisor) + addition
  let step1 = multiply(base_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let final_result = add(step2, addition)
  
  assert_eq(30, step1)    // 10 * 3 = 30
  assert_eq(8, step2)     // ceil(30/4) = 8
  assert_eq(23, final_result)  // 8 + 15 = 23
}

test "real_world_shopping_cart_calculation" {
  // 测试实际购物车计算场景
  let item1_price = 29
  let item1_quantity = 2
  let item2_price = 15
  let item2_quantity = 3
  let item3_price = 8
  let item3_quantity = 4
  let shipping_cost = 12
  let discount = 25
  
  // 计算各商品小计
  let item1_total = multiply(item1_price, item1_quantity)
  let item2_total = multiply(item2_price, item2_quantity)
  let item3_total = multiply(item3_price, item3_quantity)
  
  // 计算商品总价
  let subtotal = add(add(item1_total, item2_total), item3_total)
  
  // 计算最终总价
  let total_before_discount = add(subtotal, shipping_cost)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(58, item1_total)   // 29 * 2 = 58
  assert_eq(45, item2_total)   // 15 * 3 = 45
  assert_eq(32, item3_total)   // 8 * 4 = 32
  assert_eq(135, subtotal)     // 58 + 45 + 32 = 135
  assert_eq(147, total_before_discount)  // 135 + 12 = 147
  assert_eq(122, final_total)  // 147 - 25 = 122
}

test "error_handling_robustness_validation" {
  // 测试错误处理健壮性验证
  // 测试极端除零情况
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试数值溢出保护
  let large_add = add(1073741823, 1073741824)
  assert_eq(2147483647, large_add)
  
  // 测试字符串边界处理
  assert_eq_string("Hello, x!", greet("x"))
  assert_eq_string("Hello, verylongstringwithoutspaces!", greet("verylongstringwithoutspaces"))
}

test "numerical_precision_boundary_validation" {
  // 测试数值精度和边界验证
  // 测试精度保持
  assert_eq(333334, divide_with_ceil(1000000, 3))
  assert_eq(666667, divide_with_ceil(2000000, 3))
  
  // 测试边界值运算
  let max_int = 2147483647
  let min_int = -2147483647
  
  // 测试边界值加法
  assert_eq(2147483646, add(max_int, -1))
  assert_eq(-2147483646, add(min_int, 1))
  
  // 测试边界值乘法
  assert_eq(2147483647, multiply(max_int, 1))
  assert_eq(-2147483647, multiply(min_int, 1))
  
  // 测试边界值除法
  assert_eq(2147483647, divide_with_ceil(max_int, 1))
  assert_eq(-2147483647, divide_with_ceil(min_int, 1))
  assert_eq(1, divide_with_ceil(max_int, max_int))
  assert_eq(1, divide_with_ceil(min_int, min_int))
}