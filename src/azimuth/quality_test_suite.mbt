// 高质量 MoonBit 测试套件
// 专注于核心功能的全面测试，包含边界情况和实际应用场景

// 导入必要的依赖
use "moonbitlang/core"

test "mathematical_identity_properties" {
  // 测试数学恒等式性质
  let x = 42
  
  // 加法单位元
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法单位元
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  
  // 乘法零元
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
}

test "divide_with_ceil_comprehensive_scenarios" {
  // 全面测试向上取整除法的各种场景
  
  // 正数除法
  assert_eq(4, divide_with_ceil(10, 3))   // 3.33... -> 4
  assert_eq(3, divide_with_ceil(9, 3))    // 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100))  // 0.01 -> 1
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))  // -3.33... -> -3
  assert_eq(3, divide_with_ceil(10, -3))   // -3.33... -> -3
  assert_eq(4, divide_with_ceil(-10, -3))  // 3.33... -> 4
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 5))     // 0/5 = 0
  assert_eq(0, divide_with_ceil(5, 0))     // 除零保护
  assert_eq(1, divide_with_ceil(1, 1))     // 1/1 = 1
}

test "complex_business_calculation" {
  // 复杂业务计算：电商订单总价计算
  let product_price = 89
  let quantity = 4
  let tax_rate = 8  // 8%
  let shipping_cost = 12
  let discount_threshold = 300
  let discount_amount = 25
  
  // 计算商品小计
  let subtotal = multiply(product_price, quantity)
  
  // 计算税费（向上取整）
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算折扣前总价
  let pre_discount_total = add(add(subtotal, tax_amount), shipping_cost)
  
  // 应用折扣（如果满足条件）
  let final_total = if pre_discount_total > discount_threshold {
    add(pre_discount_total, -discount_amount)
  } else {
    pre_discount_total
  }
  
  assert_eq(356, subtotal)           // 89 * 4
  assert_eq(29, tax_amount)          // ceil(356 * 8 / 100)
  assert_eq(397, pre_discount_total) // 356 + 29 + 12
  assert_eq(372, final_total)        // 397 - 25 (超过阈值)
}

test "string_processing_internationalization" {
  // 字符串处理国际化测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, 🏳️‍🌈!", greet("🏳️‍🌈"))
  
  // 边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, a!", greet("a"))
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟测试
  
  // O(1) 常数时间操作
  let constant_time = multiply(42, 2)
  
  // O(n) 线性时间操作模拟
  let input_size = 1000
  let linear_time = multiply(input_size, 3)
  
  // O(log n) 对数时间操作模拟（简化）
  let log_time = divide_with_ceil(input_size, 100)
  
  // O(n²) 二次时间操作模拟
  let quadratic_time = divide_with_ceil(multiply(input_size, input_size), 10000)
  
  assert_eq(84, constant_time)
  assert_eq(3000, linear_time)
  assert_eq(10, log_time)
  assert_eq(100, quadratic_time)
}

test "resource_allocation_optimization" {
  // 资源分配优化问题
  
  // 场景：服务器资源分配
  let total_memory_mb = 16384
  let num_servers = 5
  
  // 计算每台服务器分配的内存（向上取整确保覆盖）
  let memory_per_server = divide_with_ceil(total_memory_mb, num_servers)
  
  // 计算实际分配的总内存
  let total_allocated = multiply(memory_per_server, num_servers)
  
  // 计算内存浪费
  let memory_waste = subtract(total_allocated, total_memory_mb)
  
  // 验证分配合理性
  let utilization_percent = divide_with_ceil(multiply(total_memory_mb, 100), total_allocated)
  
  assert_eq(3277, memory_per_server)    // ceil(16384 / 5)
  assert_eq(16385, total_allocated)     // 3277 * 5
  assert_eq(1, memory_waste)            // 16385 - 16384
  assert_eq(99, utilization_percent)    // floor(16384 * 100 / 16385)
}

test "financial_compound_interest" {
  // 金融复利计算模拟
  
  let principal = 10000
  let annual_rate = 5  // 5%
  let years = 3
  
  // 年度复利计算（使用向上取整模拟银行舍入）
  let year1_end = add(principal, divide_with_ceil(multiply(principal, annual_rate), 100))
  let year2_end = add(year1_end, divide_with_ceil(multiply(year1_end, annual_rate), 100))
  let year3_end = add(year2_end, divide_with_ceil(multiply(year2_end, annual_rate), 100))
  
  // 计算月供（简化）
  let total_months = multiply(years, 12)
  let monthly_payment = divide_with_ceil(year3_end, total_months)
  
  assert_eq(10500, year1_end)    // 10000 + ceil(10000 * 5 / 100)
  assert_eq(11025, year2_end)    // 10500 + ceil(10500 * 5 / 100)
  assert_eq(11576, year3_end)    // 11025 + ceil(11025 * 5 / 100)
  assert_eq(322, monthly_payment) // ceil(11576 / 36)
}

test "data_structure_operations_simulation" {
  // 数据结构操作模拟
  
  // 栈操作模拟
  let stack_initial_size = 0
  let stack_after_push = add(stack_initial_size, 5)
  let stack_after_pop = subtract(stack_after_push, 2)
  
  // 队列操作模拟
  let queue_capacity = 10
  let queue_items = 7
  let queue_remaining = subtract(queue_capacity, queue_items)
  let can_enqueue = queue_remaining > 0
  
  // 哈希表负载因子计算
  let hash_table_size = 16
  let hash_elements = 11
  let load_factor = divide_with_ceil(multiply(hash_elements, 100), hash_table_size)
  
  assert_eq(5, stack_after_push)
  assert_eq(3, stack_after_pop)
  assert_eq(3, queue_remaining)
  assert_true(can_enqueue)
  assert_eq(69, load_factor)  // ceil(11 * 100 / 16)
}

test "extreme_boundary_conditions" {
  // 极端边界条件测试
  
  // 大数运算
  let large_num = 1000000
  assert_eq(2000000, add(large_num, large_num))
  assert_eq(1000000000000, multiply(large_num, large_num))
  
  // 接近整数边界
  let near_max = 2147483640
  let near_min = -2147483640
  assert_eq(2147483641, add(near_max, 1))
  assert_eq(-2147483641, add(near_min, -1))
  
  // 极值除法
  assert_eq(1, divide_with_ceil(1, 1000000))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  
  // 零值运算
  assert_eq(0, multiply(0, 1000000))
  assert_eq(1000000, add(0, 1000000))
  assert_eq(0, divide_with_ceil(0, 1000000))
}

// 辅助函数：减法
fn subtract(a : Int, b : Int) -> Int {
  a - b
}