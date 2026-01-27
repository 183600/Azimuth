// 增强测试用例 - 专注于边界情况和复杂场景
// 使用标准 MoonBit 测试语法

test "add_function_boundary_values" {
  // 测试加法函数的边界值
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483647, -1))
  @azimuth.assert_eq(0, @azimuth.add(2147483647, -2147483647))
  @azimuth.assert_eq(1, @azimuth.add(2147483647, -2147483646))
}

test "multiply_function_precision" {
  // 测试乘法函数的精度和边界情况
  @azimuth.assert_eq(46340, @azimuth.multiply(215, 215))  // 接近√INT_MAX
  @azimuth.assert_eq(-46340, @azimuth.multiply(-215, 215))
  @azimuth.assert_eq(46340, @azimuth.multiply(-215, -215))
  @azimuth.assert_eq(0, @azimuth.multiply(2147483647, 0))
}

test "divide_with_ceil_complex_scenarios" {
  // 测试向上取整除法的复杂场景
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, 1))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(1, -1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-1, 2))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-3, 2))
}

test "greet_function_encoding_edge_cases" {
  // 测试问候函数的编码边缘情况
  @azimuth.assert_eq_string("Hello, \n\r\t!", @azimuth.greet("\n\r\t"))
  @azimuth.assert_eq_string("Hello, \"quoted\"!", @azimuth.greet("\"quoted\""))
  @azimuth.assert_eq_string("Hello, 'single'!", @azimuth.greet("'single'"))
  @azimuth.assert_eq_string("Hello, \\backslash!", @azimuth.greet("\\backslash"))
  @azimuth.assert_eq_string("Hello, 🏳️‍🌈!", @azimuth.greet("🏳️‍🌈"))
}

test "complex_financial_calculation" {
  // 复杂金融计算场景：贷款分期
  let loan_amount = 150000
  let monthly_payment = 4500
  let months_needed = @azimuth.divide_with_ceil(loan_amount, monthly_payment)
  
  @azimuth.assert_eq(34, months_needed)  // ceil(150000/4500) = 34
  
  // 验证总支付金额
  let total_payment = @azimuth.multiply(months_needed, monthly_payment)
  @azimuth.assert_eq(153000, total_payment)
  
  // 验证最后一个月的实际支付金额
  let final_payment = @azimuth.add(total_payment, 
    -@azimuth.multiply(months_needed - 1, monthly_payment))
  @azimuth.assert_eq(1500, final_payment)
}

test "resource_optimization_problem" {
  // 资源优化问题：仓库存储
  // 物品1：37个，每箱容量10
  let item1_boxes = @azimuth.divide_with_ceil(37, 10)  // 4箱
  
  // 物品2：89个，每箱容量15
  let item2_boxes = @azimuth.divide_with_ceil(89, 15)  // 6箱
  
  // 物品3：156个，每箱容量25
  let item3_boxes = @azimuth.divide_with_ceil(156, 25)  // 7箱
  
  // 计算总箱子数
  let total_boxes = @azimuth.add(@azimuth.add(item1_boxes, item2_boxes), item3_boxes)
  @azimuth.assert_eq(17, total_boxes)
  
  // 计算总存储容量
  let item1_capacity = @azimuth.multiply(item1_boxes, 10)  // 40
  let item2_capacity = @azimuth.multiply(item2_boxes, 15)  // 90
  let item3_capacity = @azimuth.multiply(item3_boxes, 25)  // 175
  
  let total_capacity = @azimuth.add(@azimuth.add(item1_capacity, item2_capacity), item3_capacity)
  @azimuth.assert_eq(305, total_capacity)
}

test "nested_function_composition" {
  // 嵌套函数组合测试
  let base = 12
  
  // 复杂嵌套计算
  let result = @azimuth.divide_with_ceil(
    @azimuth.add(
      @azimuth.multiply(base, 3),
      @azimuth.multiply(base, 2)
    ),
    @azimuth.add(base, @azimuth.divide_with_ceil(base, 5))
  )
  
  // 手动计算验证：(12*3 + 12*2) / (12 + ceil(12/5)) = 60 / 15 = 4
  @azimuth.assert_eq(4, result)
}

test "algorithm_efficiency_comparison" {
  // 算法效率比较测试
  let data_size = 1000
  
  // 模拟O(n)算法
  let linear_ops = @azimuth.multiply(data_size, 3)
  
  // 模拟O(log n)算法（简化）
  let log_ops = @azimuth.divide_with_ceil(data_size, 50)
  
  // 模拟O(n²)算法（简化）
  let quadratic_ops = @azimuth.divide_with_ceil(@azimuth.multiply(data_size, data_size), 1000)
  
  @azimuth.assert_eq(3000, linear_ops)
  @azimuth.assert_eq(20, log_ops)
  @azimuth.assert_eq(1000, quadratic_ops)
  
  // 验证效率差异
  let linear_to_log_ratio = @azimuth.divide_with_ceil(linear_ops, log_ops)
  @azimuth.assert_eq(150, linear_to_log_ratio)
}

test "error_recovery_scenarios" {
  // 错误恢复场景测试
  // 除零错误的多种处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  
  // 极值验证
  @azimuth.assert_eq(2147483647, @azimuth.divide_with_ceil(2147483647, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.divide_with_ceil(-2147483648, 1))
  
  // 极小除数情况
  @azimuth.assert_eq(2147483647, @azimuth.divide_with_ceil(2147483647, 1))
  @azimuth.assert_eq(2147483647, @azimuth.divide_with_ceil(2147483646, 1))
}