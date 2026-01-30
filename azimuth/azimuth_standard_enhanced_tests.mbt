// Azimuth 标准增强测试用例
// 包含8个高质量的标准 MoonBit 测试用例

test "mathematical_commutativity_verification" {
  // 验证数学交换律
  let a = 17
  let b = 29
  
  // 加法交换律验证
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律验证
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 使用不同数值再次验证
  let x = -15
  let y = 23
  assert_eq(add(x, y), add(y, x))
  assert_eq(multiply(x, y), multiply(y, x))
}

test "divide_with_ceil_precision_testing" {
  // 向上取整除法精度测试
  // 测试小数部分刚好为0的情况
  assert_eq(5, divide_with_ceil(10, 2))
  assert_eq(8, divide_with_ceil(24, 3))
  assert_eq(12, divide_with_ceil(36, 3))
  
  // 测试小数部分接近1的情况
  assert_eq(6, divide_with_ceil(11, 2))
  assert_eq(9, divide_with_ceil(26, 3))
  assert_eq(13, divide_with_ceil(37, 3))
  
  // 测试极小余数情况
  assert_eq(4, divide_with_ceil(13, 4))
  assert_eq(7, divide_with_ceil(22, 3))
}

test "string_greeting_multilingual_support" {
  // 多语言问候支持测试
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, Français!", greet("Français"))
  assert_eq_string("Hello, Deutsch!", greet("Deutsch"))
  assert_eq_string("Hello, Português!", greet("Português"))
  assert_eq_string("Hello, Italiano!", greet("Italiano"))
  assert_eq_string("Hello, 日本語!", greet("日本語"))
  assert_eq_string("Hello, 한국어!", greet("한국어"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
}

test "complex_business_workflow_calculation" {
  // 复杂业务工作流计算测试
  // 场景：电商订单处理
  let orders_per_day = 127
  let processing_capacity = 15
  let days_needed = divide_with_ceil(orders_per_day, processing_capacity)
  assert_eq(9, days_needed)
  
  // 计算实际处理能力
  let total_capacity = multiply(days_needed, processing_capacity)
  assert_eq(135, total_capacity)
  
  // 计算剩余处理能力
  let spare_capacity = subtract(total_capacity, orders_per_day)
  assert_eq(8, spare_capacity)
  
  // 计算处理效率
  let efficiency = multiply(divide_with_ceil(orders_per_day, total_capacity), 100)
  assert_eq(94, efficiency)
}

test "mathematical_identity_properties_comprehensive" {
  // 综合数学恒等式测试
  // 加法恒等式：x + 0 = x
  assert_eq(42, add(42, 0))
  assert_eq(-17, add(-17, 0))
  assert_eq(0, add(0, 0))
  
  // 乘法恒等式：x * 1 = x, x * 0 = 0
  assert_eq(42, multiply(42, 1))
  assert_eq(-17, multiply(-17, 1))
  assert_eq(0, multiply(42, 0))
  assert_eq(0, multiply(-17, 0))
  
  // 减法恒等式：x - x = 0, x - 0 = x
  assert_eq(0, subtract(42, 42))
  assert_eq(0, subtract(-17, -17))
  assert_eq(42, subtract(42, 0))
  assert_eq(-17, subtract(-17, 0))
}

test "resource_allocation_optimization_scenario" {
  // 资源分配优化场景测试
  // 场景：云计算资源分配
  let total_requests = 1000
  let server_capacity = 60
  let servers_needed = divide_with_ceil(total_requests, server_capacity)
  assert_eq(17, servers_needed)
  
  // 计算总处理能力
  let total_processing_capacity = multiply(servers_needed, server_capacity)
  assert_eq(1020, total_processing_capacity)
  
  // 计算资源利用率
  let utilization_rate = divide_with_ceil(multiply(total_requests, 100), total_processing_capacity)
  assert_eq(98, utilization_rate)
  
  // 计算浪费的资源
  let wasted_resources = subtract(total_processing_capacity, total_requests)
  assert_eq(20, wasted_resources)
}

test "error_handling_robustness_verification" {
  // 错误处理健壮性验证测试
  // 除零错误的各种场景
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
  
  // 边界值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(-1, divide_with_ceil(1, -1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  
  // 极值运算测试
  let large_num = 1000000
  assert_eq(0, multiply(large_num, 0))
  assert_eq(large_num, add(large_num, 0))
  assert_eq(large_num, subtract(large_num, 0))
}

test "real_world_logistics_calculation" {
  // 真实世界物流计算测试
  // 场景：货物配送路线规划
  let total_packages = 347
  let truck_capacity = 24
  let trucks_needed = divide_with_ceil(total_packages, truck_capacity)
  assert_eq(15, trucks_needed)
  
  // 计算总运输能力
  let total_transport_capacity = multiply(trucks_needed, truck_capacity)
  assert_eq(360, total_transport_capacity)
  
  // 计算剩余空间
  let remaining_space = subtract(total_transport_capacity, total_packages)
  assert_eq(13, remaining_space)
  
  // 计算空间利用率
  let space_utilization = divide_with_ceil(multiply(total_packages, 100), total_transport_capacity)
  assert_eq(96, space_utilization)
  
  // 计算需要的配送员数量（每辆卡车需要一个配送员）
  let delivery_persons_needed = trucks_needed
  assert_eq(15, delivery_persons_needed)
  
  // 计算每辆卡车的平均负载
  let average_load_per_truck = divide_with_ceil(total_packages, trucks_needed)
  assert_eq(24, average_load_per_truck)
}