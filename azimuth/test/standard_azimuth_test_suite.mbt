// 标准 Azimuth 测试套件 - 10个高质量 MoonBit 测试用例
// 测试覆盖基本功能、边界条件、业务逻辑和真实场景

test "basic_arithmetic_combinations" {
  // 基本算术运算组合测试
  // 测试加法和乘法的各种组合
  
  // 基本加法测试
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  
  // 基本乘法测试
  @azimuth.assert_eq(21, @azimuth.multiply(3, 7))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
}

test "boundary_values_and_overflow_handling" {
  // 边界值和溢出处理测试
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试接近边界的加法
  @azimuth.assert_eq(max_int, @azimuth.add(max_int, 1))  // 溢出保护
  @azimuth.assert_eq(min_int, @azimuth.add(min_int, -1))  // 下溢保护
  @azimuth.assert_eq(0, @azimuth.add(max_int, min_int + 1))
  
  // 测试接近边界的乘法
  @azimuth.assert_eq(max_int, @azimuth.multiply(46340, 46341))  // 接近√max_int
  @azimuth.assert_eq(min_int, @azimuth.multiply(-46340, 46341))
  
  // 测试除法的边界情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))  // 除零保护
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律：a + b = b + a
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律：a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 加法结合律：(a + b) + c = a + (b + c)
  @azimuth.assert_eq(
    @azimuth.add(@azimuth.add(a, b), c),
    @azimuth.add(a, @azimuth.add(b, c))
  )
  
  // 乘法结合律：(a * b) * c = a * (b * c)
  @azimuth.assert_eq(
    @azimuth.multiply(@azimuth.multiply(a, b), c),
    @azimuth.multiply(a, @azimuth.multiply(b, c))
  )
  
  // 分配律：a * (b + c) = a * b + a * c
  @azimuth.assert_eq(
    @azimuth.multiply(a, @azimuth.add(b, c)),
    @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  )
}

test "string_processing_and_internationalization" {
  // 字符串处理和国际化测试
  // 基本字符串测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  
  // 空字符串和特殊字符测试
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,  !", @azimuth.greet(" "))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  
  // Unicode 和国际化字符测试
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  
  // 复杂字符串测试
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello, user@example.com!", @azimuth.greet("user@example.com"))
}

test "business_logic_inventory_management" {
  // 业务逻辑：库存管理测试
  // 场景：仓库中有多种商品，需要计算存储需求
  
  // 商品1：157件，每箱容量24件
  let product1_quantity = 157
  let product1_capacity = 24
  let product1_boxes = @azimuth.divide_with_ceil(product1_quantity, product1_capacity)
  
  // 商品2：89件，每箱容量16件
  let product2_quantity = 89
  let product2_capacity = 16
  let product2_boxes = @azimuth.divide_with_ceil(product2_quantity, product2_capacity)
  
  // 商品3：203件，每箱容量32件
  let product3_quantity = 203
  let product3_capacity = 32
  let product3_boxes = @azimuth.divide_with_ceil(product3_quantity, product3_capacity)
  
  // 计算总箱子数
  let total_boxes = @azimuth.add(@azimuth.add(product1_boxes, product2_boxes), product3_boxes)
  @azimuth.assert_eq(23, total_boxes)
  
  // 计算总存储容量
  let total_capacity = @azimuth.add(
    @azimuth.multiply(product1_boxes, product1_capacity),
    @azimuth.add(
      @azimuth.multiply(product2_boxes, product2_capacity),
      @azimuth.multiply(product3_boxes, product3_capacity)
    )
  )
  @azimuth.assert_eq(736, total_capacity)
  
  // 计算总商品数
  let total_items = @azimuth.add(@azimuth.add(product1_quantity, product2_quantity), product3_quantity)
  @azimuth.assert_eq(449, total_items)
  
  // 计算空间利用率
  let utilization = @azimuth.divide_with_ceil(
    @azimuth.multiply(total_items, 100),
    total_capacity
  )
  @azimuth.assert_eq(61, utilization)  // 约61%的空间利用率
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 除零错误处理测试
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极值运算测试
  let large_number = 100000
  @azimuth.assert_eq(200000, @azimuth.add(large_number, large_number))
  @azimuth.assert_eq(100000000, @azimuth.multiply(large_number, large_number))
  
  // 负数除法测试
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, -3))
  
  // 混合符号运算测试
  @azimuth.assert_eq(-5, @azimuth.add(10, -15))
  @azimuth.assert_eq(150, @azimuth.multiply(-10, -15))
  @azimuth.assert_eq(-150, @azimuth.multiply(10, -15))
}

test "performance_and_large_data_handling" {
  // 性能和大数据量处理测试
  // 大数运算测试
  let large_num1 = 100000
  let large_num2 = 200000
  let large_num3 = 300000
  
  // 大数加法链
  let sum_chain = @azimuth.add(@azimuth.add(large_num1, large_num2), large_num3)
  @azimuth.assert_eq(600000, sum_chain)
  
  // 大数乘法链
  let product_chain = @azimuth.multiply(@azimuth.multiply(100, 1000), @azimuth.multiply(10, 100))
  @azimuth.assert_eq(100000000, product_chain)
  
  // 批量计算测试
  let batch_size = 1000
  let batch_count = 50
  let total_items = @azimuth.multiply(batch_size, batch_count)
  let items_per_container = 75
  let containers_needed = @azimuth.divide_with_ceil(total_items, items_per_container)
  
  @azimuth.assert_eq(50000, total_items)
  @azimuth.assert_eq(667, containers_needed)
  
  // 性能压力测试：连续运算
  let mut result = 1
  result = @azimuth.add(result, 99)      // 100
  result = @azimuth.multiply(result, 10) // 1000
  result = @azimuth.add(result, 9000)    // 10000
  result = @azimuth.divide_with_ceil(result, 100) // 100
  @azimuth.assert_eq(100, result)
}

test "function_composition_and_complex_calculations" {
  // 函数组合和复杂计算测试
  // 复杂的嵌套计算
  let base_value = 15
  
  // 计算：((base * 2) + 10) / 3 + 5
  let step1 = @azimuth.multiply(base_value, 2)           // 30
  let step2 = @azimuth.add(step1, 10)                    // 40
  let step3 = @azimuth.divide_with_ceil(step2, 3)        // 14
  let final_result = @azimuth.add(step3, 5)              // 19
  @azimuth.assert_eq(19, final_result)
  
  // 复杂的业务计算：项目成本估算
  let labor_cost = 5000
  let material_cost = 3000
  let overhead_rate = 20  // 20%
  let profit_margin = 15  // 15%
  
  // 计算直接成本
  let direct_cost = @azimuth.add(labor_cost, material_cost)
  @azimuth.assert_eq(8000, direct_cost)
  
  // 计算管理费用
  let overhead_cost = @azimuth.divide_with_ceil(
    @azimuth.multiply(direct_cost, overhead_rate),
    100
  )
  @azimuth.assert_eq(1600, overhead_cost)
  
  // 计算总成本
  let total_cost = @azimuth.add(direct_cost, overhead_cost)
  @azimuth.assert_eq(9600, total_cost)
  
  // 计算最终价格
  let final_price = @azimuth.add(
    total_cost,
    @azimuth.divide_with_ceil(@azimuth.multiply(total_cost, profit_margin), 100)
  )
  @azimuth.assert_eq(11040, final_price)
}

test "resource_allocation_and_optimization" {
  // 资源分配和优化测试
  // 场景：计算资源分配优化
  let total_memory = 16384  // GB
  let total_cpu_cores = 64
  let services = 5
  
  // 内存分配（使用向上取整确保每个服务有足够内存）
  let memory_per_service = @azimuth.divide_with_ceil(total_memory, services)
  @azimuth.assert_eq(3277, memory_per_service)
  
  // CPU核心分配
  let cpu_per_service = @azimuth.divide_with_ceil(total_cpu_cores, services)
  @azimuth.assert_eq(13, cpu_per_service)
  
  // 计算实际分配的总资源
  let allocated_memory = @azimuth.multiply(memory_per_service, services)
  let allocated_cpu = @azimuth.multiply(cpu_per_service, services)
  
  @azimuth.assert_eq(16385, allocated_memory)
  @azimuth.assert_eq(65, allocated_cpu)
  
  // 计算资源浪费（使用加法代替减法）
  let memory_waste = @azimuth.add(allocated_memory, -total_memory)
  let cpu_waste = @azimuth.add(allocated_cpu, -total_cpu_cores)
  
  @azimuth.assert_eq(1, memory_waste)
  @azimuth.assert_eq(1, cpu_waste)
  
  // 优化后的分配策略（增加一个服务节点）
  let optimized_services = @azimuth.add(services, 1)
  let optimized_memory = @azimuth.divide_with_ceil(total_memory, optimized_services)
  let optimized_cpu = @azimuth.divide_with_ceil(total_cpu_cores, optimized_services)
  
  @azimuth.assert_eq(2341, optimized_memory)
  @azimuth.assert_eq(11, optimized_cpu)
}

test "real_world_application_scenarios" {
  // 真实世界应用场景测试
  // 场景：活动策划和预算管理
  
  let participants = 187
  let chairs_per_table = 10
  let catering_cost_per_person = 25
  let venue_cost_per_hour = 150
  let event_duration_hours = 4
  let staff_count = 8
  let staff_cost_per_hour = 20
  
  // 计算座位需求
  let chairs_needed = @azimuth.divide_with_ceil(participants, chairs_per_table)
  @azimuth.assert_eq(19, chairs_needed)
  
  // 计算实际可用座位
  let available_chairs = @azimuth.multiply(chairs_needed, chairs_per_table)
  @azimuth.assert_eq(190, available_chairs)
  
  // 计算餐饮成本
  let catering_cost = @azimuth.multiply(participants, catering_cost_per_person)
  @azimuth.assert_eq(4675, catering_cost)
  
  // 计算场地成本
  let venue_cost = @azimuth.multiply(venue_cost_per_hour, event_duration_hours)
  @azimuth.assert_eq(600, venue_cost)
  
  // 计算人力成本
  let total_staff_hours = @azimuth.multiply(staff_count, event_duration_hours)
  let staff_cost = @azimuth.multiply(total_staff_hours, staff_cost_per_hour)
  @azimuth.assert_eq(640, staff_cost)
  
  // 计算总预算
  let total_budget = @azimuth.add(@azimuth.add(catering_cost, venue_cost), staff_cost)
  @azimuth.assert_eq(5915, total_budget)
  
  // 计算人均成本
  let cost_per_person = @azimuth.divide_with_ceil(total_budget, participants)
  @azimuth.assert_eq(32, cost_per_person)
  
  // 计算剩余座位（使用加法代替减法）
  let empty_chairs = @azimuth.add(available_chairs, -participants)
  @azimuth.assert_eq(3, empty_chairs)
  
  // 计算预算缓冲（10%）
  let budget_buffer = @azimuth.divide_with_ceil(
    @azimuth.multiply(total_budget, 10),
    100
  )
  let total_budget_with_buffer = @azimuth.add(total_budget, budget_buffer)
  @azimuth.assert_eq(6507, total_budget_with_buffer)
}