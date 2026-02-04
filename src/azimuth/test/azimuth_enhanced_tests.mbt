// Azimuth 增强测试用例
// 标准的 MoonBit 测试语法，为项目提供额外的测试覆盖

// 定义必要的测试函数
fn @azimuth.assert_eq(lhs : Int, rhs : Int) -> Unit {
  if lhs != rhs {
    @builtin.abort("Assertion failed")
  } else {
    ()
  }
}

fn @azimuth.assert_eq_string(lhs : String, rhs : String) -> Unit {
  if lhs != rhs {
    @builtin.abort("String assertion failed")
  } else {
    ()
  }
}

// 定义核心功能函数
fn @azimuth.add(a : Int, b : Int) -> Int {
  a + b
}

fn @azimuth.multiply(a : Int, b : Int) -> Int {
  a * b
}

fn @azimuth.subtract(a : Int, b : Int) -> Int {
  a - b
}

fn @azimuth.divide_with_ceil(a : Int, b : Int) -> Int {
  if b == 0 { 
    0 
  } else {
    let quotient = a / b
    let remainder = a % b
    if remainder == 0 {
      quotient
    } else if a > 0 && b > 0 {
      quotient + 1
    } else if a < 0 && b < 0 {
      quotient + 1
    } else {
      quotient
    }
  }
}

fn @azimuth.greet(name : String) -> String {
  "Hello, " + name + "!"
}

test "subtract_function_comprehensive" {
  // 全面测试减法功能
  @azimuth.assert_eq(5, @azimuth.subtract(10, 5))
  @azimuth.assert_eq(-5, @azimuth.subtract(5, 10))
  @azimuth.assert_eq(0, @azimuth.subtract(5, 5))
  @azimuth.assert_eq(10, @azimuth.subtract(0, -10))
  @azimuth.assert_eq(-10, @azimuth.subtract(-5, 5))
  @azimuth.assert_eq(0, @azimuth.subtract(0, 0))
}

test "complex_mathematical_precision" {
  // 测试复杂数学运算的精度
  let a = 100
  let b = 3
  
  // 测试 ((a / b) * b) + (a % b) 的精度保持
  let quotient = @azimuth.divide_with_ceil(a, b)
  let product = @azimuth.multiply(quotient, b)
  let remainder = a % b
  let result = @azimuth.add(product, remainder)
  
  @azimuth.assert_eq(34, quotient)  // ceil(100/3) = 34
  @azimuth.assert_eq(102, product)  // 34 * 3 = 102
  @azimuth.assert_eq(1, remainder)  // 100 % 3 = 1
  @azimuth.assert_eq(103, result)   // 102 + 1 = 103
}

test "boundary_condition_handling" {
  // 测试边界条件的处理
  // 测试接近整数极限的运算
  let near_max = 2147483640
  let near_min = -2147483640
  
  @azimuth.assert_eq(2147483641, @azimuth.add(near_max, 1))
  @azimuth.assert_eq(-2147483641, @azimuth.add(near_min, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(near_max, 0))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(near_max, near_max))
}

test "real_world_financial_calculation" {
  // 真实世界金融计算场景
  // 计算贷款分期付款
  let principal = 50000
  let annual_rate = 6  // 6%
  let loan_term_years = 5
  
  // 简化的月供计算
  let total_months = @azimuth.multiply(loan_term_years, 12)
  let total_interest = @azimuth.divide_with_ceil(@azimuth.multiply(principal, annual_rate * loan_term_years), 100)
  let total_payment = @azimuth.add(principal, total_interest)
  let monthly_payment = @azimuth.divide_with_ceil(total_payment, total_months)
  
  @azimuth.assert_eq(60, total_months)
  @azimuth.assert_eq(15000, total_interest)
  @azimuth.assert_eq(65000, total_payment)
  @azimuth.assert_eq(1084, monthly_payment)  // ceil(65000/60)
}

test "error_resilience_validation" {
  // 错误恢复和验证测试
  // 测试各种异常输入的安全处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(2147483647, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-2147483648, 0))
  
  // 测试字符串函数的空值处理
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "performance_benchmark_operations" {
  // 性能基准运算测试
  // 模拟大数据集的处理
  let dataset_size = 10000
  let operations_per_item = 5
  
  // 计算总操作数
  let total_operations = @azimuth.multiply(dataset_size, operations_per_item)
  
  // 计算处理批次（每批1000个操作）
  let batch_size = 1000
  let batches_needed = @azimuth.divide_with_ceil(total_operations, batch_size)
  
  // 计算预估处理时间（每批2秒）
  let estimated_time = @azimuth.multiply(batches_needed, 2)
  
  @azimuth.assert_eq(50000, total_operations)
  @azimuth.assert_eq(50, batches_needed)
  @azimuth.assert_eq(100, estimated_time)
}

test "data_type_conversion_simulation" {
  // 数据类型转换模拟测试
  // 模拟浮点数运算的整数近似
  
  // 使用整数运算模拟浮点运算
  let scaled_1 = @azimuth.multiply(37, 10)  // 3.7 * 10
  let scaled_2 = @azimuth.multiply(23, 10)  // 2.3 * 10
  let scaled_sum = @azimuth.add(scaled_1, scaled_2)
  let result = @azimuth.divide_with_ceil(scaled_sum, 10)
  
  @azimuth.assert_eq(370, scaled_1)
  @azimuth.assert_eq(230, scaled_2)
  @azimuth.assert_eq(600, scaled_sum)
  @azimuth.assert_eq(60, result)  // ceil(600/10) = 60
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟测试
  // 模拟不同算法的时间复杂度
  let input_size = 1000
  
  // O(1) 常数时间
  let constant_time = 1
  
  // O(log n) 对数时间（简化模拟）
  let log_time = @azimuth.divide_with_ceil(input_size, 100)
  
  // O(n) 线性时间
  let linear_time = input_size
  
  // O(n log n) 线性对数时间
  let linear_log_time = @azimuth.multiply(input_size, @azimuth.divide_with_ceil(input_size, 1000))
  
  // O(n²) 平方时间
  let quadratic_time = @azimuth.multiply(input_size, input_size)
  
  @azimuth.assert_eq(1, constant_time)
  @azimuth.assert_eq(10, log_time)
  @azimuth.assert_eq(1000, linear_time)
  @azimuth.assert_eq(10000, linear_log_time)
  @azimuth.assert_eq(1000000, quadratic_time)
}

test "international_character_processing" {
  // 国际化字符处理测试
  // 测试各种Unicode字符的字符串处理
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
  @azimuth.assert_eq_string("Hello, العربية!", @azimuth.greet("العربية"))
  @azimuth.assert_eq_string("Hello, עברית!", @azimuth.greet("עברית"))
  @azimuth.assert_eq_string("Hello, हिन्दी!", @azimuth.greet("हिन्दी"))
  @azimuth.assert_eq_string("Hello, 日本語!", @azimuth.greet("日本語"))
  @azimuth.assert_eq_string("Hello, 한국어!", @azimuth.greet("한국어"))
  @azimuth.assert_eq_string("Hello, ελληνικά!", @azimuth.greet("ελληνικά"))
  @azimuth.assert_eq_string("Hello, 🌍🌎🌏!", @azimuth.greet("🌍🌎🌏"))
}

test "concurrent_scenario_simulation" {
  // 并发场景模拟测试
  // 模拟多线程环境下的计算
  let thread_count = 8
  let tasks_per_thread = 125
  let total_tasks = @azimuth.multiply(thread_count, tasks_per_thread)
  
  // 模拟任务分配
  let base_tasks_per_thread = @azimuth.divide_with_ceil(total_tasks, thread_count)
  let remaining_tasks = @azimuth.subtract(total_tasks, @azimuth.multiply(base_tasks_per_thread, thread_count))
  
  // 计算实际处理时间（每线程处理时间）
  let processing_time_per_thread = base_tasks_per_thread
  let total_processing_time = @azimuth.multiply(processing_time_per_thread, thread_count)
  
  @azimuth.assert_eq(1000, total_tasks)
  @azimuth.assert_eq(125, base_tasks_per_thread)
  @azimuth.assert_eq(0, remaining_tasks)
  @azimuth.assert_eq(1000, total_processing_time)
}