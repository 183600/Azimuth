// Azimuth 新增强测试场景
// 包含 8 个高质量 MoonBit 测试用例，覆盖特殊场景和边界情况

test "performance_large_number_calculations" {
  // 大数运算性能测试
  let large_num1 = 1000000
  let large_num2 = 999999
  
  // 大数加法
  assert_eq(1999999, add(large_num1, large_num2))
  
  // 大数乘法
  assert_eq(999999000, multiply(large_num1, 999))
  
  // 大数除法向上取整
  assert_eq(2, divide_with_ceil(large_num1, large_num2))
  assert_eq(1000000, divide_with_ceil(large_num1, 1))
}

test "unicode_string_processing" {
  // Unicode 字符串处理测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀 MoonBit!", greet("🚀 MoonBit"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, 🏳️‍🌈!", greet("🏳️‍🌈"))
}

test "complex_resource_allocation" {
  // 复杂资源分配场景测试
  // 场景：云服务器资源分配
  let total_cpu_cores = 47
  let cores_per_server = 8
  let servers_needed = divide_with_ceil(total_cpu_cores, cores_per_server)
  
  assert_eq(6, servers_needed)
  
  // 验证总CPU容量
  let total_capacity = multiply(servers_needed, cores_per_server)
  assert_eq(48, total_capacity)
  
  // 验证剩余容量
  let remaining_cores = subtract(total_capacity, total_cpu_cores)
  assert_eq(1, remaining_cores)
}

test "mathematical_function_properties" {
  // 数学函数性质测试
  let a = 12
  let b = 5
  let c = 3
  
  // 分配律测试：a * (b + c) = a * b + a * c
  let distributive_left = multiply(a, add(b, c))
  let distributive_right = add(multiply(a, b), multiply(a, c))
  assert_eq(distributive_left, distributive_right)
  
  // 结合律测试：(a + b) + c = a + (b + c)
  let associative_left = add(add(a, b), c)
  let associative_right = add(a, add(b, c))
  assert_eq(associative_left, associative_right)
}

test "financial_calculation_scenario" {
  // 金融计算场景测试
  // 场景：贷款分期计算
  let loan_amount = 10000
  let monthly_payment = 850
  let months_needed = divide_with_ceil(loan_amount, monthly_payment)
  
  assert_eq(12, months_needed)
  
  // 计算实际支付总额
  let actual_payment = multiply(months_needed, monthly_payment)
  assert_eq(10200, actual_payment)
  
  // 计算额外支付的利息
  let extra_interest = subtract(actual_payment, loan_amount)
  assert_eq(200, extra_interest)
}

test "edge_case_comprehensive" {
  // 边界情况综合测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, subtract(0, 0))
  
  // 单位值测试
  assert_eq(5, add(5, 0))
  assert_eq(5, multiply(5, 1))
  assert_eq(5, subtract(5, 0))
  assert_eq(1, divide_with_ceil(5, 5))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(-1, divide_with_ceil(1, -1))
}

test "batch_processing_workflow" {
  // 批处理工作流测试
  // 场景：数据处理批处理
  let total_records = 12345
  let batch_size = 1000
  let batches_needed = divide_with_ceil(total_records, batch_size)
  
  assert_eq(13, batches_needed)
  
  // 计算完整批次和最后一批的大小
  let full_batches = subtract(batches_needed, 1)
  let records_in_full_batches = multiply(full_batches, batch_size)
  let last_batch_size = subtract(total_records, records_in_full_batches)
  
  assert_eq(12, full_batches)
  assert_eq(12000, records_in_full_batches)
  assert_eq(345, last_batch_size)
}

test "string_formatting_edge_cases" {
  // 字符串格式化边界情况测试
  // 空字符串和特殊字符
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, 	
\r!", greet("	
\r"))
  assert_eq_string("Hello, @#$%^&*()_+-=[]{}|;':\",./<>?!", greet("@#$%^&*()_+-=[]{}|;':\",./<>?"))
  
  // 数字和字母混合
  assert_eq_string("Hello, abc123XYZ!", greet("abc123XYZ"))
  assert_eq_string("Hello, 2024-01-01!", greet("2024-01-01"))
  
  // 长字符串
  let long_name = "This is a very long name that tests the greet function with extended input"
  let expected_long = "Hello, " + long_name + "!"
  assert_eq_string(expected_long, greet(long_name))
}