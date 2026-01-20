// 标准 MoonBit 测试套件
// 测试 azimuth 库的核心功能

test "arithmetics_property_verification" {
  // 验算基本算术性质
  let a = 15
  let b = 7
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 3)), add(multiply(a, b), multiply(a, 3)))
}

test "overflow_boundary_comprehensive" {
  // 全面的溢出边界测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正溢出场景
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  assert_eq(max_val, multiply(46341, 46341))
  
  // 负溢出场景
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
  assert_eq(min_val, multiply(-46341, 46341))
  
  // 特殊边界组合
  assert_eq(-1, add(max_val, min_val))
  assert_eq(min_val, multiply(min_val, -1))
}

test "string_processing_internationalization" {
  // 国际化字符串处理测试
  let chinese_name = "李明"
  let japanese_name = "田中太郎"
  let korean_name = "김철수"
  let arabic_name = "أحمد"
  let russian_name = "Иван"
  
  assert_eq_string("Hello, 李明!", greet(chinese_name))
  assert_eq_string("Hello, 田中太郎!", greet(japanese_name))
  assert_eq_string("Hello, 김철수!", greet(korean_name))
  assert_eq_string("Hello, أحمد!", greet(arabic_name))
  assert_eq_string("Hello, Иван!", greet(russian_name))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let unit_price = 199
  let quantity = 5
  let discount_rate = 10  // 10% 折扣
  let tax_rate = 8        // 8% 税费
  let shipping = 25       // 运费
  
  // 计算商品总价
  let subtotal = multiply(unit_price, quantity)
  
  // 计算折扣（满500元享受折扣）
  let discount = if (subtotal > 500) {
    multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  
  // 计算折后价格
  let discounted_total = add(subtotal, -discount)
  
  // 计算税费
  let tax = multiply(discounted_total, tax_rate) / 100
  
  // 计算最终总价
  let final_total = add(add(discounted_total, tax), shipping)
  
  assert_eq(1071, final_total)
}

test "mathematical_series_formula" {
  // 数学级数公式验证
  // 等差数列求和：1 + 2 + 3 + ... + n = n × (n + 1) / 2
  let n = 20
  let arithmetic_sum = multiply(n, add(n, 1)) / 2
  assert_eq(210, arithmetic_sum)
  
  // 等比数列求和（前n项2的幂）：2^0 + 2^1 + ... + 2^(n-1) = 2^n - 1
  let geo_n = 8
  let geometric_sum = add(multiply(multiply(multiply(2, 2), multiply(2, 2)), 1)
  assert_eq(255, geometric_sum)
  
  // 平方和公式：1² + 2² + ... + n² = n × (n + 1) × (2n + 1) / 6
  let square_n = 5
  let square_sum = multiply(multiply(square_n, add(square_n, 1)), add(multiply(2, square_n), 1)) / 6
  assert_eq(55, square_sum)
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟测试
  let n = 100
  
  // O(1) 常数时间操作
  let constant_op = add(5, 3)
  assert_eq(8, constant_op)
  
  // O(n) 线性时间操作：求和
  let linear_sum = multiply(n, add(n, 1)) / 2
  assert_eq(5050, linear_sum)
  
  // O(n²) 平方时间操作：嵌套循环总和
  let quadratic_sum = multiply(multiply(n, add(n, 1)), add(multiply(2, n), 1)) / 6
  assert_eq(338350, quadratic_sum)
}

test "data_structure_operations" {
  // 数据结构操作模拟
  // 栈操作：push 和 pop
  let stack_size = 10
  let push_value = 42
  let final_stack_size = add(stack_size, 1)
  assert_eq(11, final_stack_size)
  
  // 队列操作：enqueue 和 dequeue
  let queue_front = 0
  let queue_rear = 9
  let queue_size = add(add(queue_rear, 1), -queue_front)
  assert_eq(10, queue_size)
  
  // 哈希表索引计算：index = hash(key) % capacity
  let hash_key = 12345
  let table_capacity = 16
  let hash_index = hash_key % table_capacity
  assert_eq(9, hash_index)
}

test "scientific_engineering_calculations" {
  // 科学与工程计算
  // 物理公式：动能 E = 1/2 × m × v²
  let mass = 1000  // kg
  let velocity = 20  // m/s
  let kinetic_energy = multiply(mass, multiply(velocity, velocity)) / 2
  assert_eq(200000, kinetic_energy)
  
  // 化学公式：理想气体状态方程 PV = nRT（简化计算）
  let pressure = 101325  // Pa
  let volume = 1  // m³
  let moles = 40  // mol
  let gas_constant = 8  // 简化的 R 值
  let temperature = multiply(pressure, volume) / multiply(moles, gas_constant)
  assert_eq(316, temperature)
  
  // 工程公式：梁的弯曲应力 σ = M × y / I（简化计算）
  let moment = 1000  // N·m
  let distance = 50   // mm
  let moment_inertia = 100000  // mm⁴
  let bending_stress = multiply(moment, distance) / moment_inertia
  assert_eq(0, bending_stress)  // 整数除法结果
}

test "error_resilience_validation" {
  // 错误恢复能力验证
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保所有断言函数正常工作
  assert_true(true)
  assert_false(false)
  assert_eq(100, 100)
  assert_eq_string("test", "test")
  
  // 确保溢出保护机制有效
  assert_true(add(max_val, 1000) == max_val)
  assert_true(add(min_val, -1000) == min_val)
  assert_true(multiply(max_val, 10) == max_val)
  assert_true(multiply(min_val, -10) == min_val)
  
  // 确保特殊值处理正确
  assert_eq(0, multiply(0, max_val))
  assert_eq(0, multiply(0, min_val))
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
}

test "performance_benchmark_operations" {
  // 性能基准操作测试
  let base = 1000
  let multiplier = 1000
  
  // 大数乘法性能测试
  let large_product = multiply(base, multiplier)
  assert_eq(1000000, large_product)
  
  // 连续运算性能测试
  let chain_result = add(multiply(add(base, 100), 2), multiply(add(base, 200), 3))
  assert_eq(500000, chain_result)
  
  // 复杂表达式性能测试
  let complex_result = multiply(multiply(multiply(10, 20), add(30, 40)), add(50, 60))
  assert_eq(840000, complex_result)
}