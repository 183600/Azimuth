// 新增的 MoonBit 标准测试用例
// 针对 azimuth 库的补充测试，重点关注边界条件、错误处理和复杂场景

test "subtract_function_comprehensive" {
  // 全面测试减法功能
  assert_eq(5, subtract(8, 3))
  assert_eq(-5, subtract(3, 8))
  assert_eq(0, subtract(10, 10))
  assert_eq(10, subtract(0, -10))
  assert_eq(-10, subtract(-10, 0))
  assert_eq(20, subtract(-5, -25))
  assert_eq(1000, subtract(1500, 500))
  assert_eq(-1000, subtract(-500, 500))
}

test "extreme_boundary_conditions" {
  // 极端边界条件测试
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试接近边界的加法
  assert_eq(max_int - 1, add(max_int - 100, 99))
  assert_eq(min_int + 1, add(min_int + 100, -99))
  
  // 测试接近边界的乘法
  assert_eq(46340, multiply(46340, 1))  // sqrt(max_int)
  assert_eq(-46340, multiply(-46340, 1))
  
  // 测试边界减法
  assert_eq(1, subtract(max_int, max_int - 1))
  assert_eq(-1, subtract(min_int, min_int + 1))
}

test "divide_with_ceil_precision_edge_cases" {
  // 向上取整除法的精度边界测试
  // 测试大数除法
  assert_eq(33334, divide_with_ceil(100000, 3))
  assert_eq(33333, divide_with_ceil(99999, 3))
  assert_eq(1, divide_with_ceil(100000, 99999))
  
  // 测试小数精度
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 -> 2
  assert_eq(3, divide_with_ceil(5, 2))  // 2.5 -> 3
  assert_eq(4, divide_with_ceil(7, 2))  // 3.5 -> 4
  
  // 测试负数精度
  assert_eq(-1, divide_with_ceil(-3, 2))   // -1.5 -> -1
  assert_eq(-2, divide_with_ceil(-5, 2))   // -2.5 -> -2
  assert_eq(3, divide_with_ceil(-5, -2))   // 2.5 -> 3
}

test "complex_financial_calculations" {
  // 复杂金融计算场景
  let principal = 10000
  let rate1 = 5    // 第一年利率
  let rate2 = 6    // 第二年利率
  let rate3 = 7    // 第三年利率
  
  // 复利计算（简化版，使用向上取整）
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate1), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate2), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate3), 100))
  
  assert_eq(10500, year1)
  assert_eq(11130, year2)
  assert_eq(11909, year3)
  
  // 计算总收益
  let total_return = subtract(year3, principal)
  assert_eq(1909, total_return)
}

test "string_processing_edge_cases" {
  // 字符串处理的边界情况
  // 测试各种特殊字符
  assert_eq_string("Hello, \n\t\r!", greet("\n\t\r"))
  assert_eq_string("Hello, \"!'!", greet("\"!'"))
  assert_eq_string("Hello, 🔥🚀💯!", greet("🔥🚀💯"))
  
  // 测试长字符串
  let long_name = "A" + "B" + "C" + "D" + "E" + "F" + "G" + "H" + "I" + "J"
  assert_eq_string("Hello, ABCDEFGHIJ!", greet(long_name))
}

test "mathematical_sequence_calculations" {
  // 数学序列计算
  // 计算等差数列前n项和：S = n/2 * (2a + (n-1)d)
  let n = 20
  let a = 3
  let d = 2
  
  let sum = divide_with_ceil(
    multiply(n, add(multiply(2, a), multiply(subtract(n, 1), d))),
    2
  )
  
  assert_eq(460, sum)  // 20/2 * (6 + 38) = 10 * 44 = 460
  
  // 计算等比数列前n项和（简化版）
  let r = 2
  let first_term = 1
  let geo_sum = multiply(subtract(multiply(r, n), 1), first_term)  // 简化计算
  assert_eq(39, geo_sum)  // 简化的等比数列和
}

test "error_handling_robustness_comprehensive" {
  // 全面的错误处理健壮性测试
  // 测试除零的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  
  // 测试极大数值的运算稳定性
  let large_num = 1000000
  assert_eq(2000000, add(large_num, large_num))
  assert_eq(0, subtract(large_num, large_num))
  assert_eq(1000000000000, multiply(large_num, large_num))
}

test "real_world_inventory_optimization" {
  // 真实世界的库存优化场景
  let product_a_demand = 150
  let product_b_demand = 200
  let product_c_demand = 175
  
  let box_capacity = 50
  let safety_stock = 20
  
  // 计算每种产品需要的箱子数
  let boxes_a = divide_with_ceil(add(product_a_demand, safety_stock), box_capacity)
  let boxes_b = divide_with_ceil(add(product_b_demand, safety_stock), box_capacity)
  let boxes_c = divide_with_ceil(add(product_c_demand, safety_stock), box_capacity)
  
  // 计算总箱子数
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  
  // 计算剩余容量
  let total_capacity = multiply(total_boxes, box_capacity)
  let total_items_needed = add(add(add(product_a_demand, product_b_demand), product_c_demand), multiply(3, safety_stock))
  let remaining_capacity = subtract(total_capacity, total_items_needed)
  
  assert_eq(4, boxes_a)   // ceil(170/50) = 4
  assert_eq(5, boxes_b)   // ceil(220/50) = 5
  assert_eq(4, boxes_c)   // ceil(195/50) = 4
  assert_eq(13, total_boxes)
  assert_eq(650, total_capacity)
  assert_eq(585, total_items_needed)
  assert_eq(65, remaining_capacity)
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟测试
  let base_operations = 100
  let input_size = 1000
  let linear_factor = 2
  let quadratic_factor = 1
  let logarithmic_factor = 10
  
  // 模拟 O(n) 复杂度
  let linear_ops = multiply(input_size, linear_factor)
  
  // 模拟 O(n²) 复杂度（简化）
  let quadratic_ops = divide_with_ceil(multiply(input_size, input_size), quadratic_factor)
  
  // 模拟 O(log n) 复杂度（简化）
  let log_ops = multiply(divide_with_ceil(input_size, logarithmic_factor), 3)
  
  // 总操作数
  let total_ops = add(add(base_operations, linear_ops), add(quadratic_ops, log_ops))
  
  assert_eq(2000, linear_ops)
  assert_eq(1000000, quadratic_ops)
  assert_eq(300, log_ops)
  assert_eq(1002300, total_ops)
}