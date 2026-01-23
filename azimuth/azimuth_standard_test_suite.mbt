// Azimuth 标准测试用例集
// 使用标准的 MoonBit 测试语法

test "add_function_basic_tests" {
  // 测试基本加法功能
  @azimuth.assert_eq(7, @azimuth.add(3, 4))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(1000, @azimuth.add(500, 500))
}

test "add_function_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正溢出
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  
  // 测试负溢出
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  
  // 测试边界情况
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "multiply_function_basic_tests" {
  // 测试基本乘法功能
  @azimuth.assert_eq(12, @azimuth.multiply(3, 4))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
  @azimuth.assert_eq(12, @azimuth.multiply(-3, -4))
  @azimuth.assert_eq(0, @azimuth.multiply(100, 0))
}

test "multiply_function_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试大数相乘的溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(50000, 50000))
  
  // 测试最小值的特殊处理
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "greet_function_tests" {
  // 测试问候函数
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 用户123!", @azimuth.greet("用户123"))
}

test "divide_with_ceil_function_tests" {
  // 测试向上取整除法功能
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
}

test "assertion_functions_tests" {
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_true(10 > 5)
  @azimuth.assert_false(3 > 10)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}

test "complex_arithmetic_operations" {
  // 测试复合算术运算
  let a = 15
  let b = 25
  let c = -10
  
  // 计算 (a + b) * c
  let sum = @azimuth.add(a, b)
  let result = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(-400, result)
  
  // 计算 a * b + c
  let product = @azimuth.multiply(a, b)
  let result2 = @azimuth.add(product, c)
  @azimuth.assert_eq(365, result2)
}

test "edge_cases_and_special_values" {
  // 测试边界情况和特殊值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试与0和1的运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 测试-1的乘法
  @azimuth.assert_eq(-max_val, @azimuth.multiply(max_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 123
  let y = 456
  let z = 789
  
  // 测试加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 测试乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 测试分配律 a * (b + c) = a * b + a * c
  let left = @azimuth.multiply(x, @azimuth.add(y, z))
  let right = @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z))
  @azimuth.assert_eq(left, right)
}

// 新增的8个标准MoonBit测试用例
test "performance_large_scale_operations" {
  // 大规模运算性能测试
  let iterations = 10000
  let sum_result = @azimuth.add(iterations, iterations)
  let product_result = @azimuth.multiply(100, iterations)
  let division_result = @azimuth.divide_with_ceil(product_result, 50)
  
  @azimuth.assert_eq(20000, sum_result)
  @azimuth.assert_eq(1000000, product_result)
  @azimuth.assert_eq(20000, division_result)
}

test "error_boundary_conditions" {
  // 错误边界条件测试
  // 测试各种可能导致错误的边界情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))  // 0/0
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(1, 0))  // 正数/0
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-1, 0))  // 负数/0
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(2147483647, 0))  // 最大值/0
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-2147483648, 0))  // 最小值/0
}

test "string_encoding_handling" {
  // 字符串编码处理测试
  @azimuth.assert_eq_string("Hello, 🌙⭐!", @azimuth.greet("🌙⭐"))
  @azimuth.assert_eq_string("Hello, \n\r\t!", @azimuth.greet("\n\r\t"))
  @azimuth.assert_eq_string("Hello, \"quotes\"!", @azimuth.greet("\"quotes\""))
  @azimuth.assert_eq_string("Hello, 'apostrophe'!", @azimuth.greet("'apostrophe'"))
  @azimuth.assert_eq_string("Hello, \\backslash\\!", @azimuth.greet("\\backslash\\"))
}

test "complex_financial_calculations" {
  // 复杂金融计算测试
  // 场景：计算复利和分期付款
  let principal = 10000
  let annual_rate = 5  // 5%
  let years = 3
  
  // 简化的复利计算（每年计算一次）
  let year1_end = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, annual_rate), 100))
  let year2_end = @azimuth.add(year1_end, @azimuth.divide_with_ceil(@azimuth.multiply(year1_end, annual_rate), 100))
  let year3_end = @azimuth.add(year2_end, @azimuth.divide_with_ceil(@azimuth.multiply(year2_end, annual_rate), 100))
  
  // 验证复利增长
  @azimuth.assert_eq(10500, year1_end)  // 10000 + 5%
  @azimuth.assert_eq(11025, year2_end)  // 10500 + 5%
  @azimuth.assert_eq(11576, year3_end)  // 11025 + 5%
  
  // 计算月供（简化计算）
  let total_months = @azimuth.multiply(years, 12)
  let monthly_payment = @azimuth.divide_with_ceil(year3_end, total_months)
  @azimuth.assert_eq(322, monthly_payment)  // ceil(11576/36)
}

test "algorithm_complexity_analysis" {
  // 算法复杂度分析测试
  let input_sizes = [10, 100, 1000]
  
  // 模拟O(n)算法
  let linear_10 = @azimuth.multiply(input_sizes[0], 2)
  let linear_100 = @azimuth.multiply(input_sizes[1], 2)
  let linear_1000 = @azimuth.multiply(input_sizes[2], 2)
  
  // 模拟O(log n)算法（简化为除法）
  let log_10 = @azimuth.divide_with_ceil(input_sizes[0], 3)
  let log_100 = @azimuth.divide_with_ceil(input_sizes[1], 3)
  let log_1000 = @azimuth.divide_with_ceil(input_sizes[2], 3)
  
  // 验证复杂度特征
  @azimuth.assert_eq(20, linear_10)
  @azimuth.assert_eq(200, linear_100)
  @azimuth.assert_eq(2000, linear_1000)
  
  @azimuth.assert_eq(4, log_10)
  @azimuth.assert_eq(34, log_100)
  @azimuth.assert_eq(334, log_1000)
}

test "data_structure_operations" {
  // 数据结构操作测试
  // 模拟栈操作
  let stack_size = 0
  let stack_size_after_push = @azimuth.add(stack_size, 1)
  let stack_size_after_pop = subtract(stack_size_after_push, 1)
  
  @azimuth.assert_eq(1, stack_size_after_push)
  @azimuth.assert_eq(0, stack_size_after_pop)
  
  // 模拟队列操作
  let queue_capacity = 10
  let queue_items = 7
  let remaining_capacity = subtract(queue_capacity, queue_items)
  let can_add_more = remaining_capacity > 0
  
  @azimuth.assert_eq(3, remaining_capacity)
  @azimuth.assert_true(can_add_more)
}

test "resource_utilization_optimization" {
  // 资源利用优化测试
  // 场景：服务器资源分配
  let total_memory = 16384  // GB
  let servers = 5
  let memory_per_server = @azimuth.divide_with_ceil(total_memory, servers)
  
  @azimuth.assert_eq(3277, memory_per_server)  // ceil(16384/5)
  
  // 计算实际分配的总内存
  let total_allocated = @azimuth.multiply(memory_per_server, servers)
  let memory_waste = subtract(total_allocated, total_memory)
  
  @azimuth.assert_eq(16385, total_allocated)
  @azimuth.assert_eq(1, memory_waste)
  
  // 优化分配策略
  let optimized_servers = @azimuth.add(servers, 1)
  let optimized_memory_per_server = @azimuth.divide_with_ceil(total_memory, optimized_servers)
  @azimuth.assert_eq(2341, optimized_memory_per_server)  // ceil(16384/6)
}

test "edge_case_mathematical_operations" {
  // 边缘情况数学运算测试
  // 测试连续运算的边界情况
  let initial_value = 1
  
  // 连续加法
  let result1 = @azimuth.add(@azimuth.add(@azimuth.add(initial_value, 1), 1), 1)
  @azimuth.assert_eq(4, result1)
  
  // 连续乘法
  let result2 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(initial_value, 2), 2), 2)
  @azimuth.assert_eq(8, result2)
  
  // 混合运算
  let result3 = @azimuth.divide_with_ceil(@azimuth.multiply(@azimuth.add(initial_value, 2), 3), 4)
  @azimuth.assert_eq(3, result3)  // ceil((1+2)*3/4) = ceil(9/4) = 3
  
  // 极值运算
  let max_int_test = @azimuth.add(2147483647, 0)
  let min_int_test = @azimuth.add(-2147483648, 0)
  
  @azimuth.assert_eq(2147483647, max_int_test)
  @azimuth.assert_eq(-2147483648, min_int_test)
}

// 辅助函数：减法
fn subtract(a : Int, b : Int) -> Int {
  a - b
}